####################### #
#
# Goldfish package
# Maintaining the support_constraint active mask over the event sequence
#
####################### #

# The support mask is realized as a derived object: its atoms are
# plain effects, so they are maintained with the SAME recipe builders the
# estimated formula uses (state container, event schedule, effect templates,
# stat cache, per-object routing) — but over the constraint sub-plan alone, in a
# self-contained pass sharing no state with the main statistics loop. Keeping
# it separate means the unconstrained statistics path is untouched, so the 1e-6
# baselines cannot move; the pass only runs when a `support_constraint` is
# present.
#
# The atoms' live values are kept at their OWN broadcast kinds here — a
# scalar, a sender vector, a receiver vector or a dense n1 x n2 matrix,
# whichever axes the atom varies on — and written in place at the entries an
# event moves. The boolean tree is evaluated at the axis-union of those kinds,
# so a separable constraint never builds a grid at all. The atoms themselves
# are re-derived from their update closures per touched entry — the same
# locality as the interaction second-hop — rather than a whole-value recompute.
#
# Atom MAINTENANCE (walking the atom event streams) is split from mask
# EVALUATION (projecting the atoms through a constraint's boolean tree).
# Maintenance happens on the merged walk: `build_constraint_atom_store()`
# seeds the atoms and `advance_constraint_atom_store()` moves them with the
# shared state, recording the atom entries each event moved
# (`constraint_replay_atoms()` exposes that record as an atom source).
# Evaluation is one maintainer per constraint, `build_mask_maintainer()`,
# whose `recompute()` takes that moved set and returns the mask entries that
# flipped: the tree is elementwise, so an atom entry can only move the mask
# entries it projects onto, and nothing is re-evaluated anywhere else.
# Several constraints over the SAME atoms share the atom source and each
# keeps its own mask maintainer and snapshot times
# (`preprocess_pooled_support_masks()`), so the atom stream is walked once
# instead of once per output. An independent atom walk over the constraint's
# own streams, `build_atom_maintainer()`, is kept in the test helpers as the
# reference these are checked against.

# Force an independent copy of an atom buffer. `write_entries()` mutates its
# buffer in place (via `set_entries()`), so a value that must survive later
# writes — a stored `initial`, or a per-replay seed — must be copied first.
# Assigning into a shared vector triggers copy-on-write, giving a new SEXP the
# in-place writer can no longer reach through the original.
copy_atom_buffer <- function(x) {
  y <- x
  if (length(y) > 0L) {
    y[[1L]] <- y[[1L]]
  }
  y
}

# One constraint-atom effect-template evaluation, reading its data off a shared
# state container by object key. A standalone twin of the closure inside
# `build_atom_maintainer()` (now the test oracle): it lets the atoms be
# maintained over the merged
# walk's ONE shared state instead of the private walk's own container, so the
# same atom advance runs whether the state is private or shared.
call_constraint_atom_template <- function(
  template,
  state,
  cache,
  shape,
  event_args,
  net_update,
  att_update,
  n1,
  n2
) {
  args <- c(
    list(
      network = if (template$n_networks == 1L) {
        state$networks[[template$net_keys]]
      } else if (template$n_networks > 1L) {
        lapply(template$net_keys, function(k) state$networks[[k]])
      } else {
        list()
      },
      attribute = if (template$n_attributes == 1L) {
        state[[template$att_components]][[template$att_keys]]
      } else if (template$n_attributes > 1L) {
        lapply(
          seq_len(template$n_attributes),
          function(j) {
            state[[template$att_components[j]]][[template$att_keys[j]]]
          }
        )
      } else {
        list()
      },
      cache = cache,
      n1 = n1,
      n2 = n2,
      net_update = net_update,
      att_update = att_update,
      event_order = 0L,
      inter_event_time = 0
    ),
    event_args
  )
  do.call(template$fun, args[template$args_by_shape[[shape]]])
}

#' Seed a constraint's atom store for maintenance over a shared walk
#'
#' Builds everything `advance_constraint_atom_store()` needs to keep a
#' constraint's atoms live — their effect templates, live caches, per-atom
#' kind-shaped values (seeded from the objects' pre-event state), and the
#' object-key → routing map — WITHOUT the private walk's own state container,
#' event fetch or schedule. The atoms are advanced by the merged walk as its
#' shared state moves, and each advance records the atom deltas it produced so a
#' mask can later be projected from them (`constraint_replay_atoms()`), the same
#' deltas the private walk would have replayed from its own container.
#'
#' @param sub_plan the compiled constraint sub-plan.
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prep_envir`.
#' @param prep_envir environment holding the realized data objects.
#' @param src data source over `prep_envir`; built when `NULL`.
#' @return a store env carrying the seed and an empty recorded-delta stream.
#' @noRd
build_constraint_atom_store <- function(
  sub_plan,
  model,
  nodes,
  nodes2,
  prep_envir = new.env(),
  src = NULL
) {
  effects <- sub_plan$effect_functions
  objects_effects_link <- sub_plan$objects_effects_link
  atom_kinds <- sub_plan$atom_kinds
  atom_labels <- sub_plan$atom_labels
  n_atoms <- length(effects)

  if (is.null(src)) {
    src <- new_data_source(envir = prep_envir)
  }
  n1 <- ds_n_nodes(src, nodes)
  n2 <- ds_n_nodes(src, nodes2)

  # A container built only to seed: it gives the templates their object-key
  # structure and `initialize_cache_stat()` the pre-event values. The advance
  # reads its data off the shared state the caller passes, never this one.
  seed_state <- build_state_container(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = prep_envir,
    src = src
  )
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    seed_state
  )
  stat_cache <- initialize_cache_stat(
    objects_effects_link = objects_effects_link,
    effects = effects,
    groups_network = NULL,
    window_parameters = vector("list", n_atoms),
    n1 = n1,
    n2 = n2,
    model = model,
    sub_model = sub_plan$atom_sub_model,
    envir = prep_envir,
    src = src
  )

  net_update_lookup <- matrix(NA_integer_, nrow(sub_plan$objects), n_atoms)
  att_update_lookup <- matrix(NA_integer_, nrow(sub_plan$objects), n_atoms)
  net_update_lookup[cbind(
    sub_plan$effect_objects$oid,
    sub_plan$effect_objects$gid
  )] <- sub_plan$effect_objects$net_update
  att_update_lookup[cbind(
    sub_plan$effect_objects$oid,
    sub_plan$effect_objects$gid
  )] <- sub_plan$effect_objects$att_update

  store <- new.env(parent = emptyenv())
  store$templates <- effects_template
  store$stat_cache <- lapply(stat_cache, "[[", "cache")
  store$atom_kinds <- atom_kinds
  store$atom_labels <- atom_labels
  store$n_atoms <- n_atoms
  store$n1 <- n1
  store$n2 <- n2
  store$routing <- sub_plan$routing
  store$net_update_lookup <- net_update_lookup
  store$att_update_lookup <- att_update_lookup
  # Which local oid an object key resolves to, and whether that object is
  # undirected, so the merged walk can route a shared covariate event by key.
  store$key_to_oid <- stats::setNames(
    sub_plan$objects$oid,
    sub_plan$objects$key
  )
  store$is_undirected <- stats::setNames(
    sub_plan$objects$is_undirected,
    sub_plan$objects$key
  )

  # Each atom stored at its own kind, seeded from the pre-event state. `initial`
  # is a SEPARATE copy kept aside so a replay starts where the private walk did:
  # `write_entries()` mutates the live buffer in place, so an aliased initial
  # would be overwritten to the final value as the walk advances.
  atom_state <- new.env(parent = emptyenv())
  initial <- vector("list", n_atoms)
  for (a in seq_len(n_atoms)) {
    seeded <- reduce_value(
      matrix(stat_cache[[a]]$stat, n1, n2),
      0L,
      atom_kinds[a]
    )
    assign(as.character(a), seeded, envir = atom_state)
    initial[[a]] <- copy_atom_buffer(seeded)
  }
  store$atom_state <- atom_state
  store$initial <- initial

  # The recorded atom-delta stream: one entry per (event, atom) the advance
  # touches, in walk order. `constraint_replay_atoms()` replays it. The
  # buffers are allocated ahead and filled up to `record_n`
  # (`record_atom_delta()`).
  clear_atom_record(store)
  store
}

# Empty a store's atom-delta record, keeping no capacity.
clear_atom_record <- function(store) {
  store$record_n <- 0L
  store$record_time <- numeric(0)
  store$record_gid <- integer(0)
  store$record_entries <- list()
  store$record_values <- list()
  invisible(store)
}

# Append one atom delta to a store's record. The buffers double when full,
# and each is taken out of the store before it is written: a vector still
# bound in the environment has a second reference, so writing into it there
# copies the whole record on every append, which made the record's cost
# quadratic in the events.
record_atom_delta <- function(store, time, gid, entries, values) {
  n <- store$record_n + 1L
  capacity <- length(store$record_gid)
  if (n > capacity) {
    capacity <- max(64L, 2L * capacity)
  }

  record_time <- store$record_time
  store$record_time <- NULL
  length(record_time) <- capacity
  record_time[n] <- time
  store$record_time <- record_time

  record_gid <- store$record_gid
  store$record_gid <- NULL
  length(record_gid) <- capacity
  record_gid[n] <- gid
  store$record_gid <- record_gid

  record_entries <- store$record_entries
  store$record_entries <- NULL
  length(record_entries) <- capacity
  record_entries[n] <- list(entries)
  store$record_entries <- record_entries

  record_values <- store$record_values
  store$record_values <- NULL
  length(record_values) <- capacity
  record_values[n] <- list(values)
  store$record_values <- record_values

  store$record_n <- n
  invisible(store)
}

# Keep only the record entries at positions `keep`, in order, at the front of
# the buffers. The buffers are taken out of the store before the write, as in
# `record_atom_delta()`, so nothing is copied.
compact_atom_record <- function(store, keep) {
  m <- length(keep)
  for (field in c(
    "record_time",
    "record_gid",
    "record_entries",
    "record_values"
  )) {
    buffer <- store[[field]]
    store[[field]] <- NULL
    buffer[seq_len(m)] <- buffer[keep]
    store[[field]] <- buffer
  }
  store$record_n <- m
  invisible(store)
}

# Write the recorded atom moves stamped strictly before `before` into the
# store's atoms and return the entries they touched, per atom, keeping the
# moves at or after `before` in the record. `touched` is the per-atom list the
# entries are added to, in record order, each atom's entries unique.
flush_atom_moves <- function(store, before, touched) {
  n <- store$record_n
  if (n == 0L) {
    return(touched)
  }
  due <- store$record_time[seq_len(n)] < before
  if (!any(due)) {
    return(touched)
  }
  for (r in which(due)) {
    entries <- store$record_entries[[r]]
    if (length(entries) == 0L) {
      next
    }
    gid <- store$record_gid[[r]]
    buffer <- get(as.character(gid), envir = store$atom_state)
    assign(
      as.character(gid),
      write_entries(buffer, entries, store$record_values[[r]]),
      envir = store$atom_state
    )
    touched[[gid]] <- unique(c(touched[[gid]], entries))
  }
  compact_atom_record(store, which(!due))
  touched
}

# Advance a constraint's atoms for one shared covariate event and record the
# deltas. `key` names the moved object; when no atom reads it the store
# is untouched. Mirrors the routing of `apply_atom_event()` inside the test
# oracle `build_atom_maintainer()`
# (undirected mirror, per-atom `collapse_operand_delta` + in-place write), but
# reads the pre-update SHARED `state` the caller passes, and appends each atom's
# `(time, gid, entries, values)` delta to the store's stream instead of only
# accumulating a touched set. `event_args` are the walk's already-resolved
# arguments (increment folded to a replace). With `write = FALSE` the delta
# is only recorded, and `flush_atom_moves()` writes it once every stored row
# stamped at `time` has read the mask, so a mask read at `time` sees the
# atoms strictly before it.
advance_constraint_atom_store <- function(
  store,
  key,
  shape,
  event_args,
  state,
  time,
  write = TRUE
) {
  oid <- store$key_to_oid[[key]]
  if (is.null(oid) || is.na(oid)) {
    return(invisible(NULL))
  }
  is_undirected <- isTRUE(store$is_undirected[[key]])
  n1 <- store$n1
  n2 <- store$n2

  for (gid in store$routing[[oid]]) {
    net_update_pos <- store$net_update_lookup[oid, gid]
    if (is.na(net_update_pos)) {
      net_update_pos <- NULL
    }
    att_update_pos <- store$att_update_lookup[oid, gid]
    if (is.na(att_update_pos)) {
      att_update_pos <- NULL
    }

    effect_update <- call_constraint_atom_template(
      store$templates[[gid]],
      state,
      store$stat_cache[[gid]],
      shape,
      event_args,
      net_update_pos,
      att_update_pos,
      n1,
      n2
    )
    if (!is.null(effect_update$cache)) {
      store$stat_cache[[gid]] <- effect_update$cache
    }
    updates <- effect_update$changes
    if (is_undirected) {
      ea2 <- event_args
      ea2$sender <- event_args$receiver
      ea2$receiver <- event_args$sender
      eu2 <- call_constraint_atom_template(
        store$templates[[gid]],
        state,
        store$stat_cache[[gid]],
        shape,
        ea2,
        net_update_pos,
        att_update_pos,
        n1,
        n2
      )
      if (!is.null(eu2$cache)) {
        store$stat_cache[[gid]] <- eu2$cache
      }
      updates <- rbind(updates, eu2$changes)
    }
    if (!is.null(updates)) {
      buffer <- get(as.character(gid), envir = store$atom_state)
      delta <- collapse_operand_delta(
        buffer,
        updates[, "node1"],
        updates[, "node2"],
        updates[, "replace"],
        store$atom_kinds[gid],
        n1,
        n2
      )
      if (write) {
        assign(
          as.character(gid),
          write_entries(buffer, delta$entries, delta$values),
          envir = store$atom_state
        )
      }
      record_atom_delta(store, time, gid, delta$entries, delta$values)
    }
  }
  invisible(NULL)
}

#' Replay a recorded atom-delta stream as a `build_mask_maintainer()` handle
#'
#' The merged walk computed each atom's deltas once, off the shared state, and
#' `build_constraint_atom_store()` recorded them. This exposes that record with
#' the exact interface `build_mask_maintainer()` / `walk_mask_streams()` expect
#' from an atom source (the one the test oracle `build_atom_maintainer()` also
#' provides) — `advance(t)`, `atom_value`, `atom_kind`,
#' `take_touched`, `n1`, `n2`, `atom_labels`, `atom_kinds` — but its `advance()`
#' only WRITES the recorded values (no state read, no template call, no second
#' container), so the atom pool is walked once and projected here cheaply.
#'
#' @param store a store from `build_constraint_atom_store()` whose stream the
#'   merged walk has filled.
#' @return an atom-source handle, with the interface the test oracle
#'   `build_atom_maintainer()` returns.
#' @noRd
constraint_replay_atoms <- function(store) {
  n_atoms <- store$n_atoms
  atom_labels <- store$atom_labels
  atom_kinds <- store$atom_kinds
  n1 <- store$n1
  n2 <- store$n2

  atom_state <- new.env(parent = emptyenv())
  for (a in seq_len(n_atoms)) {
    # A fresh copy per replay: `write_entries()` mutates in place, so replaying
    # must not scribble on the store's pristine `initial` (several fids replay
    # the same store).
    assign(
      as.character(a),
      copy_atom_buffer(store$initial[[a]]),
      envir = atom_state
    )
  }

  touched <- new.env(parent = emptyenv())
  touched$entries <- vector("list", n_atoms)

  times <- store$record_time[seq_len(store$record_n)]
  order_idx <- order(times)
  ordered_time <- times[order_idx]
  n_rec <- length(order_idx)
  cursor <- new.env(parent = emptyenv())
  cursor$next_rec <- 1L

  advance <- function(tt) {
    while (cursor$next_rec <= n_rec && ordered_time[cursor$next_rec] < tt) {
      r <- order_idx[cursor$next_rec]
      gid <- store$record_gid[[r]]
      entries <- store$record_entries[[r]]
      buffer <- get(as.character(gid), envir = atom_state)
      assign(
        as.character(gid),
        write_entries(buffer, entries, store$record_values[[r]]),
        envir = atom_state
      )
      touched$entries[[gid]] <- unique(c(touched$entries[[gid]], entries))
      cursor$next_rec <- cursor$next_rec + 1L
    }
    invisible(NULL)
  }

  atom_value <- function(label) {
    get(as.character(match(label, atom_labels)), envir = atom_state)
  }
  atom_kind <- function(label) {
    atom_kinds[[match(label, atom_labels)]]
  }
  take_touched <- function() {
    moved <- lapply(touched$entries, function(at) {
      if (is.null(at)) NULL else unique(at)
    })
    touched$entries <- vector("list", n_atoms)
    moved
  }

  list(
    n1 = n1,
    n2 = n2,
    atom_labels = atom_labels,
    atom_kinds = atom_kinds,
    advance = advance,
    atom_value = atom_value,
    atom_kind = atom_kind,
    take_touched = take_touched
  )
}

#' Maintain one constraint's mask incrementally over a shared atom pool
#'
#' The mask is a value at its own kind, maintained the way every other
#' kind-shaped value in the walk is maintained: written in place at the entries
#' that moved, and emitted as a stream of the entries that flipped.
#'
#' The recompute is local and that is exact. `assemble_support_mask()` evaluates
#' the constraint tree ELEMENTWISE, so mask entry `e` depends only on entry `e`
#' of each atom. A change to atom `k` at entries `E` can therefore move only the
#' mask entries `E` projects onto under `map_entries()`, and nothing else in the
#' mask can have moved. This is the same locality the interaction second hop
#' already relies on, applied to the same shape of problem.
#'
#' Symmetrising is the one place that reasoning needs care, because
#' `m & t(m)` is not elementwise: entry `(i, j)` of the stored mask reads the
#' raw mask at `(i, j)` AND at `(j, i)`. A symmetric constraint therefore keeps
#' two buffers, the raw mask at its own kind and the stored symmetrised mask at
#' point, and the stored entries recomputed are closed under transposition.
#'
#' @param atoms an atom-source handle: `constraint_replay_atoms()`, a stepping
#'   walk's live atoms, or in tests the oracle `build_atom_maintainer()`.
#' @param expr the evaluable mask expression over `.a{k}` placeholders.
#' @param atom_labels the constraint's atom labels, aligned with `.a{k}`.
#' @param symmetric symmetrise the dyad grid (coordination / undirected REM).
#' @param mask_kind the axis-union kind the tree is evaluated at.
#' @param stored_kind the kind the emitted mask is stored at, which equals
#'   `mask_kind` unless symmetrising forced it to point.
#' @return a handle with `initial()`, the mask before any event and never
#'   written to, `value()`, the current stored mask, and
#'   `recompute(moved)`, which takes the atom entries an advance step moved (as
#'   `take_touched()` reports them) and returns the stored-mask entries that
#'   flipped, as `list(entries, values)` in linear stored-kind addressing.
#'   The atom walk is driven by the caller, not from here, so several
#'   constraints over one atom pool each recompute from the same touched set.
#' @noRd
build_mask_maintainer <- function(
  atoms,
  expr,
  atom_labels,
  symmetric,
  mask_kind,
  stored_kind
) {
  n1 <- atoms$n1
  n2 <- atoms$n2
  slots <- match(atom_labels, atoms$atom_labels)
  kinds <- atoms$atom_kinds[slots]

  # Evaluate the constraint tree at a set of entries of the mask's own kind,
  # reading each atom through its own projection rather than densifying it.
  # The projection deliberately does NOT re-apply the zeroed diagonal a dyad
  # statistic carries: a mask entry is "is this dyad allowed", self-dyads are
  # excluded by the engines rather than by the constraint, and the stored mask
  # is read off the diagonal in any case.
  eval_at <- function(entries) {
    values <- stats::setNames(
      lapply(seq_along(atom_labels), function(k) {
        read_value_at_entries(
          atoms$atom_value(atom_labels[[k]]),
          kinds[[k]],
          entries,
          mask_kind,
          n1,
          n2
        )
      }),
      paste0(".a", seq_along(atom_labels))
    )
    as.logical(assemble_support_mask(values, expr))
  }

  raw_length <- kind_length(mask_kind, n1, n2)
  raw <- eval_at(seq_len(raw_length))
  if (mask_kind == 0L) {
    dim(raw) <- c(n1, n2)
  }

  # The transpose of a set of linear point entries, which is what closes the
  # symmetrised recompute.
  transposed <- function(entries) {
    rows <- ((entries - 1L) %% n1) + 1L
    cols <- ((entries - 1L) %/% n1) + 1L
    (rows - 1L) * n1 + cols
  }

  symmetrized <- function(value) {
    symmetrize_mask(project_value(value, mask_kind, 0L, n1, n2))
  }
  stored <- if (symmetric) symmetrized(raw) else raw

  # The initial value is evaluated a SECOND time rather than aliasing `stored`.
  # `stored` is written in place from here on, so handing the same object out as
  # the initial would let the walk rewrite the thing the stream is a diff
  # against. One extra evaluation at construction against a silently wrong
  # timeline is not a close call.
  initial <- eval_at(seq_len(raw_length))
  if (mask_kind == 0L) {
    dim(initial) <- c(n1, n2)
  }
  if (symmetric) {
    initial <- symmetrized(initial)
  }

  recompute <- function(moved) {
    at_mask <- unlist(
      lapply(seq_along(atom_labels), function(k) {
        entries <- moved[[slots[[k]]]]
        if (is.null(entries)) {
          return(NULL)
        }
        map_entries(entries, kinds[[k]], mask_kind, n1, n2)
      }),
      use.names = FALSE
    )
    if (length(at_mask) == 0L) {
      return(list(entries = integer(0), values = logical(0)))
    }
    at_mask <- unique(at_mask)

    if (!symmetric) {
      flips <- changed_entries(raw, at_mask, eval_at(at_mask))
      raw <<- write_entries(raw, flips$entries, flips$values)
      stored <<- raw
      return(flips)
    }

    # Raw first, at its own kind, then the symmetrised stored mask at the point
    # entries those raw entries reach and at their transposes.
    raw_flips <- changed_entries(raw, at_mask, eval_at(at_mask))
    raw <<- write_entries(raw, raw_flips$entries, raw_flips$values)
    if (length(raw_flips$entries) == 0L) {
      return(list(entries = integer(0), values = logical(0)))
    }
    reached <- map_entries(raw_flips$entries, mask_kind, 0L, n1, n2)
    reached <- unique(c(reached, transposed(reached)))
    grid <- project_value(raw, mask_kind, 0L, n1, n2)
    flips <- changed_entries(
      stored,
      reached,
      grid[reached] & grid[transposed(reached)]
    )
    stored <<- write_entries(stored, flips$entries, flips$values)
    flips
  }

  list(
    initial = function() initial,
    value = function() stored,
    recompute = recompute
  )
}

# Walk a shared atom pool once and encode each constraint's mask as a stream.
#
# `masks` are the DISTINCT mask maintainers over `atoms`; `request_times` gives
# one timeline per consumer and `slot` says which mask each consumer reads, so
# two consumers of one expression share its maintainer and its flips. The union
# of the timelines is walked once, the touched set is read once per union time
# and handed to every distinct mask, and each consumer's flips are then bucketed
# into its own requested intervals: a flip emitted while advancing to union time
# `u` belongs to the first requested time at or after `u`, because `advance()`
# applies the events strictly before its argument.
#
# Requested times must be ascending. The stream encodes a mask as a prefix sum,
# so out-of-order times would not describe the timeline they claim to.
walk_mask_streams <- function(atoms, masks, request_times, slot) {
  for (times in request_times) {
    if (is.unsorted(times)) {
      cli::cli_abort(
        "Support-mask snapshot times must be ascending.",
        .internal = TRUE
      )
    }
  }
  initials <- lapply(masks, function(mask) mask$initial())
  union_times <- sort(unique(unlist(request_times, use.names = FALSE)))
  # One recompute per DISTINCT mask per union time. Two fids reading the same
  # expression share a maintainer, and recomputing it twice would find the
  # buffer already written and report nothing the second time.
  n_union <- length(union_times)

  per_union <- lapply(masks, function(mask) vector("list", n_union))
  for (u in seq_len(n_union)) {
    atoms$advance(union_times[[u]])
    moved <- atoms$take_touched()
    for (m in seq_along(masks)) {
      per_union[[m]][[u]] <- masks[[m]]$recompute(moved)
    }
  }

  lapply(seq_along(request_times), function(r) {
    m <- slot[[r]]
    times <- request_times[[r]]
    n_stored <- length(times)
    flips <- vector("list", n_stored)
    if (n_stored > 0L && n_union > 0L) {
      # The requested index each union time's flips land in; a union time past
      # the last requested one is never read and is dropped.
      bucket <- findInterval(union_times, times, left.open = TRUE) + 1L
      for (u in seq_len(n_union)) {
        k <- bucket[[u]]
        if (k <= n_stored) {
          flips[[k]] <- c(flips[[k]], list(per_union[[m]][[u]]))
        }
      }
    }
    n_changes <- integer(n_stored)
    entries <- vector("list", n_stored)
    values <- vector("list", n_stored)
    for (k in seq_len(n_stored)) {
      at <- unlist(lapply(flips[[k]], "[[", "entries"), use.names = FALSE)
      if (is.null(at)) {
        # `[[<-` with NULL DELETES the element rather than emptying it, which
        # would shorten the list under the loop that is walking it.
        next
      }
      vv <- unlist(lapply(flips[[k]], "[[", "values"), use.names = FALSE)
      # One entry may flip twice between two requested times; only its last
      # value is observable, and emitting both would make the pointer lie about
      # how many entries changed.
      collapsed <- collapse_entries(at, vv)
      entries[[k]] <- collapsed$entries
      values[[k]] <- collapsed$values
      n_changes[[k]] <- length(entries[[k]])
    }
    at_vec <- unlist(entries, use.names = FALSE)
    val_vec <- unlist(values, use.names = FALSE)
    list(
      initial = initials[[m]],
      update = if (length(at_vec) > 0L) {
        rbind(as.numeric(at_vec), as.numeric(val_vec))
      } else {
        matrix(0, 2L, 0L)
      },
      update_pointer = cumsum(n_changes),
      n_stored = n_stored
    )
  })
}

#' Maintain a shared atom pool once, project a per-fid mask from it
#'
#' Several constraints over the SAME atoms (a `mutually_exclusive` layer's
#' `!tie(L)` / `tie(L)` derived constraints, or those AND-composed with a shared
#' user constraint) differ only in their boolean expression, not in the atom
#' stream they walk. Maintaining that stream once and projecting each
#' constraint's own expression from it removes the redundant walks a per-output
#' realization would incur, while evaluation correctly stays per
#' constraint — each fid keeps its own mask at its own snapshot times.
#'
#' Constraints are grouped by atom signature (their sorted labels): within a
#' group the maintenance machinery is identical, so one maintainer serves all of
#' them; across groups (disjoint atoms) separate maintainers run. Each group is
#' advanced over the UNION of its fids' snapshot times once, every distinct
#' expression is evaluated at each union time, and each fid slices its own subset
#' back out — exact because the support is piecewise-constant and the snapshot at
#' a time depends only on the atoms strictly before it (so a superset walk
#' sliced to a subset equals the subset walk).
#'
#' Partial atom overlap across DISTINCT signatures (the multivariate case, where
#' processes over different layers share some but not all atoms) is not pooled
#' here; it belongs to the merged single-clock walk. Within a single flavored
#' process all of a layer's flavors carry the same atom signature, so this pools
#' them to one walk.
#'
#' @param requests a list of `list(constraint, snapshot_times)`, one per fid, in
#'   the caller's order; `constraint = NULL` yields a `NULL` result (no mask).
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prep_envir`.
#' @param symmetric symmetrise the support (coordination / undirected REM);
#'   uniform across the family.
#' @param prep_envir environment holding the realized data objects.
#' @param src data source over `prep_envir`; built when `NULL`.
#' @param atoms_factory a function of `(sub_plan, model, nodes, nodes2,
#'   prep_envir, src)` returning the atom source for one atom signature: the
#'   merged walk's recorded store (`recorder_atoms_factory()`), or in tests
#'   the oracle `build_atom_maintainer()`.
#' @return a list aligned with `requests`; each element is a `support_mask`
#'   list (`support` or the flip stream, `initial`, `mask_kind`, `n_stored`,
#'   `symmetric`), or `NULL` for a `NULL` constraint.
#' @noRd
preprocess_pooled_support_masks <- function(
  requests,
  model,
  nodes,
  nodes2,
  symmetric = FALSE,
  prep_envir = new.env(),
  src = NULL,
  atoms_factory
) {
  result <- vector("list", length(requests))
  has_constraint <- vapply(
    requests,
    function(r) !is.null(r$constraint),
    logical(1)
  )
  if (!any(has_constraint)) {
    return(result)
  }

  # A request may carry its own `symmetric`, because a constraint belongs to the
  # process and its sub-models need not agree: a layer's rate and its
  # coordination choice share one atom pool and one evaluation, and differ only
  # in whether the stored mask is symmetrised. Absent one, the caller's applies.
  request_symmetric <- vapply(
    requests,
    function(r) isTRUE(r$symmetric %||% symmetric),
    logical(1)
  )

  con_idx <- which(has_constraint)
  signature <- vapply(
    con_idx,
    function(i) {
      paste(sort(requests[[i]]$constraint$atom_labels), collapse = "\r")
    },
    character(1)
  )

  for (group in split(con_idx, signature)) {
    constraints <- lapply(group, function(i) requests[[i]]$constraint)
    # The atom pool is built by the factory: the default private walk maintains
    # it over its own state container, but the merged walk passes a factory that
    # replays the atom deltas it already recorded off the shared state, so the
    # pool is not walked a second time.
    atoms <- atoms_factory(
      constraints[[1L]],
      model,
      nodes,
      nodes2,
      prep_envir,
      src
    )

    # Distinct MASKS within the group. Two requests read the same mask when they
    # evaluate the same expression over the same atoms and store it the same
    # way, which is what this key says. Comparing the compiled sub-plans instead
    # would answer no for two independent compilations of one constraint --
    # closures carry their environments -- and a layer's rate and choice compile
    # theirs separately, which is exactly the case worth sharing.
    keys <- vapply(
      seq_along(group),
      function(i) {
        paste(
          paste(deparse(constraints[[i]]$expr), collapse = ""),
          paste(constraints[[i]]$atom_labels, collapse = "\r"),
          constraints[[i]]$mask_kind,
          request_symmetric[[group[i]]],
          sep = "\v"
        )
      },
      character(1)
    )
    distinct_keys <- unique(keys)
    slot <- match(keys, distinct_keys)
    distinct <- constraints[match(distinct_keys, keys)]
    distinct_symmetric <- request_symmetric[group][match(distinct_keys, keys)]

    # Each distinct expression keeps its own stored kind: constraints sharing an
    # atom pool need not share an axis-union. Symmetrising destroys separability
    # -- `m & t(m)` of a row-constant mask is an outer product -- so a symmetric
    # mask is stored at point whatever its atoms' axis-union says.
    stored_kinds <- vapply(
      seq_along(distinct),
      function(di) {
        if (distinct_symmetric[[di]]) {
          0L
        } else {
          as.integer(distinct[[di]]$mask_kind)
        }
      },
      integer(1)
    )
    masks <- lapply(seq_along(distinct), function(di) {
      build_mask_maintainer(
        atoms,
        distinct[[di]]$expr,
        distinct[[di]]$atom_labels,
        distinct_symmetric[[di]],
        as.integer(distinct[[di]]$mask_kind),
        stored_kinds[[di]]
      )
    })

    # One mask maintainer per distinct expression, but one stream per fid: two
    # fids sharing an expression read it at different times, so each asks for
    # its own timeline and the walker buckets the shared flips into both.
    request_times <- lapply(group, function(i) requests[[i]]$snapshot_times)
    streams <- walk_mask_streams(atoms, masks, request_times, slot)

    for (i in seq_along(group)) {
      ri <- group[i]
      result[[ri]] <- c(
        streams[[i]],
        list(
          mask_kind = constraints[[i]]$mask_kind,
          stored_kind = stored_kinds[[slot[i]]],
          symmetric = distinct_symmetric[[slot[i]]]
        )
      )
    }
  }

  result
}

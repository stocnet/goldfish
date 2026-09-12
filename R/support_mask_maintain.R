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
# Maintenance is expressed once, by `build_atom_maintainer()`, which reports
# the atom entries each advance step moved. Evaluation is one maintainer per
# constraint, `build_mask_maintainer()`, whose `recompute()` takes that moved
# set and returns the mask entries that flipped: the tree is elementwise, so an
# atom entry can only move the mask entries it projects onto, and nothing is
# re-evaluated anywhere else. A single constraint runs one atom maintainer and
# one mask maintainer (`preprocess_support_mask()`); several constraints over
# the SAME atoms share the atom maintainer and each keeps its own mask
# maintainer and snapshot times (`preprocess_pooled_support_masks()`), so the
# atom stream is walked once instead of once per output.

#' Build the atom-maintenance state for a constraint sub-plan
#'
#' Seeds the atoms' values, each at its own broadcast kind, from the objects'
#' pre-event state and returns a handle that advances them over their own event
#' streams. The handle exposes `advance(t)` (apply every atom event strictly
#' before `t`, so the risk set is lagged), `atom_value(label)` and
#' `atom_kind(label)` (an atom's current value and the kind it is held at,
#' addressed by its deparsed label), `take_touched()` (the atom entries moved
#' since the last call), `n1`/`n2`, and the ordered `atom_labels`. Evaluation
#' of the boolean tree is a separate concern ([build_mask_maintainer()]); this
#' handle knows nothing about any particular constraint's expression, which is
#' what lets several constraints over the same atoms share one maintainer.
#'
#' @param sub_plan the compiled constraint sub-plan from
#'   `compile_support_constraint()`.
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prep_envir`.
#' @param prep_envir environment holding the realized data objects.
#' @param src optional data source; built over `prep_envir` when `NULL`.
#' @return a maintainer handle (list of closures + `n1`, `n2`, `atom_labels`).
#' @noRd
build_atom_maintainer <- function(
  sub_plan,
  model,
  nodes,
  nodes2,
  prep_envir = new.env(),
  src = NULL
) {
  effects <- sub_plan$effect_functions
  objects_effects_link <- sub_plan$objects_effects_link
  events_objects_link <- sub_plan$events_objects_link
  atom_kinds <- sub_plan$atom_kinds
  atom_labels <- sub_plan$atom_labels
  n_atoms <- length(effects)

  if (is.null(src)) {
    src <- new_data_source(envir = prep_envir)
  }
  n1 <- ds_n_nodes(src, nodes)
  n2 <- ds_n_nodes(src, nodes2)

  events <- fetch_events(sub_plan$fetch_plan, envir = prep_envir, src = src)
  schedule <- build_event_schedule(
    events,
    events_objects_link,
    sub_plan$objects
  )

  state <- build_state_container(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = prep_envir,
    src = src
  )
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    state
  )

  # Initial atom values (dense n1 x n2 each) and their live caches, seeded from
  # the objects' state before any event.
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
  # Each atom is stored AT ITS OWN KIND: a scalar for a global atom, a length-n1
  # or length-n2 vector for a sender- or receiver-axis one, and a dense matrix
  # only for a genuinely dyadic one. `atom_kinds` was already computed and until
  # now had exactly one use -- telling the expansion how to blow a kind-shaped
  # delta up into dense cells, which is the expansion this removes.
  # Which entries of which atom an event moved, accumulated between reads. The
  # mask recompute needs exactly this and nothing else: mask entry `e` depends
  # on entry `e` of each atom, so a change to atom `k` at entries `E` can move
  # only the mask entries `E` projects onto.
  touched <- new.env(parent = emptyenv())
  touched$entries <- vector("list", n_atoms)

  atom_state <- new.env(parent = emptyenv())
  for (a in seq_len(n_atoms)) {
    assign(
      as.character(a),
      reduce_value(matrix(stat_cache[[a]]$stat, n1, n2), 0L, atom_kinds[a]),
      envir = atom_state
    )
  }
  stat_cache <- lapply(stat_cache, "[[", "cache")

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

  call_atom_template <- function(
    gid,
    shape,
    event_args,
    net_update,
    att_update
  ) {
    template <- effects_template[[gid]]
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
        cache = stat_cache[[gid]],
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

  # Apply one constraint-atom object event: route the change to every atom
  # reading the object (pre-event state, mirroring the main loop's op_state),
  # update the atom's live matrix, then commit the change to the private state.
  apply_atom_event <- function(k) {
    oid <- schedule$target[k]
    component <- sub_plan$objects$component[oid]
    key <- sub_plan$objects$key[oid]
    shape <- schedule$shape[k]
    is_undirected_net <- sub_plan$objects$is_undirected[oid]

    if (shape == "global") {
      replace_value <- schedule$value[[k]]
      if (is.na(replace_value)) {
        replace_value <- 0
      }
      event_args <- list(replace = replace_value)
    } else if (shape == "node") {
      event_node <- schedule$node[k]
      if (schedule$semantics[k] == "increment") {
        increment_value <- schedule$value[[k]]
        if (is.na(increment_value)) {
          increment_value <- 0
        }
        replace_value <- state[[component]][[key]][event_node] + increment_value
      } else {
        replace_value <- schedule$value[[k]]
      }
      event_args <- list(node = event_node, replace = replace_value)
    } else {
      event_sender <- schedule$sender[k]
      event_receiver <- schedule$receiver[k]
      if (schedule$semantics[k] == "increment") {
        increment_value <- schedule$value[[k]]
        if (is.na(increment_value)) {
          increment_value <- 0
        }
        replace_value <-
          state$networks[[key]][event_sender, event_receiver] + increment_value
      } else {
        replace_value <- schedule$value[[k]]
      }
      event_args <- list(
        sender = event_sender,
        receiver = event_receiver,
        replace = replace_value
      )
    }

    for (gid in sub_plan$routing[[oid]]) {
      net_update_pos <- net_update_lookup[oid, gid]
      if (is.na(net_update_pos)) {
        net_update_pos <- NULL
      }
      att_update_pos <- att_update_lookup[oid, gid]
      if (is.na(att_update_pos)) {
        att_update_pos <- NULL
      }

      effect_update <- call_atom_template(
        gid,
        shape,
        event_args,
        net_update_pos,
        att_update_pos
      )
      if (!is.null(effect_update$cache)) {
        stat_cache[[gid]] <<- effect_update$cache
      }
      updates <- effect_update$changes
      if (is_undirected_net) {
        ea2 <- event_args
        ea2$sender <- event_args$receiver
        ea2$receiver <- event_args$sender
        eu2 <- call_atom_template(
          gid,
          shape,
          ea2,
          net_update_pos,
          att_update_pos
        )
        if (!is.null(eu2$cache)) {
          stat_cache[[gid]] <<- eu2$cache
        }
        updates <- rbind(updates, eu2$changes)
      }
      if (!is.null(updates)) {
        buffer <- get(as.character(gid), envir = atom_state)
        delta <- collapse_operand_delta(
          buffer,
          updates[, "node1"],
          updates[, "node2"],
          updates[, "replace"],
          atom_kinds[gid],
          n1,
          n2
        )
        assign(
          as.character(gid),
          write_entries(buffer, delta$entries, delta$values),
          envir = atom_state
        )
        touched$entries[[gid]] <- unique(
          c(touched$entries[[gid]], delta$entries)
        )
      }
    }

    if (shape == "global") {
      state$globals[[key]] <<- event_args$replace
    } else if (shape == "node") {
      state[[component]][[key]][event_args$node] <<- event_args$replace
    } else {
      # The one deep subassignment left in this walk, and the expensive one: the
      # atom templates receive `state$networks[[key]]` as an argument, which
      # binds it to a second name, so writing a cell duplicated the whole
      # adjacency matrix every event. `state_set_tie()` writes it in place under
      # the same aliasing precondition the main walk's state write meets -- this
      # container's matrices are materialized fresh by
      # `build_state_container()` and nothing outside it holds a reference.
      state <<- state_set_tie(
        state,
        key,
        event_args$sender,
        event_args$receiver,
        event_args$replace,
        is_undirected_net
      )
    }
  }

  # The support is piecewise-constant, changing only at the atoms' events. The
  # cursor advances the atom stream monotonically; `advance(t)` applies every
  # atom event strictly before `t` (a lagged risk set), so calling it at an
  # ascending series of times walks the stream exactly once.
  atom_ks <- which(!is.na(schedule$target))
  atom_times <- schedule$time[atom_ks]
  n_atom <- length(atom_ks)
  cursor <- new.env(parent = emptyenv())
  cursor$next_atom <- 1L
  advance <- function(tt) {
    while (cursor$next_atom <= n_atom && atom_times[cursor$next_atom] < tt) {
      apply_atom_event(atom_ks[cursor$next_atom])
      cursor$next_atom <- cursor$next_atom + 1L
    }
    invisible(NULL)
  }

  # Address an atom's current dense value by its deparsed label. Labels are
  # unique within a sub-plan (the parser deduplicates atoms by deparse), and a
  # constraint sharing this maintainer's atoms addresses them the same way, so a
  # by-label lookup decouples evaluation from any one constraint's atom order.
  atom_value <- function(label) {
    get(as.character(match(label, atom_labels)), envir = atom_state)
  }

  atom_kind <- function(label) {
    atom_kinds[[match(label, atom_labels)]]
  }

  # The entries touched since the last call, deduplicated, and cleared. Reading
  # it is what makes an advance step reportable: the caller learns which atom
  # entries moved without the maintainer knowing what a mask is.
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
#' @param atoms a handle from [build_atom_maintainer()].
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

#' Maintain the support-constraint mask across the event sequence
#'
#' Runs a stripped recipe pass over the constraint sub-plan, snapshotting the
#' support mask (the dyadic constraint before presence conjunction)
#' at each requested snapshot time. Presence (`active_1`/`active_2`) is joined
#' later by the gather / rate consumer via [assemble_model_mask()].
#'
#' The support changes only at constraint-atom object events, so it is
#' piecewise-constant in time. The pass advances the atoms over their own event
#' streams and snapshots the support at every `snapshot_times[e]` using the atom
#' state STRICTLY before that time (an event's risk set is lagged — its own
#' change is not yet applied). Passing the preprocessed object's
#' stored-event times aligns the timeline with its events by construction, even
#' though the right-censored events come from the main model's streams (not
#' carried by the constraint sub-plan).
#'
#' @param sub_plan the compiled constraint sub-plan from
#'   `compile_support_constraint()` (its `effect_functions`, registries, links,
#'   `expr`, `atom_kinds`, `mask_kind`, and `fetch_plan`).
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prep_envir`.
#' @param symmetric symmetrise the support (coordination / undirected REM).
#' @param snapshot_times numeric vector of times (the preprocessed object's
#'   stored-event times, in event order) at which to snapshot the support.
#' @param prep_envir environment holding the realized data objects.
#' @return a list with `support` (one n1 x n2 logical per snapshot time, aligned
#'   with the preprocessed object's events), `initial` (the mask before any
#'   event), `mask_kind`, `n_stored`, and `symmetric`.
#' @noRd
preprocess_support_mask <- function(
  sub_plan,
  model,
  nodes,
  nodes2,
  symmetric = FALSE,
  snapshot_times = numeric(0),
  prep_envir = new.env(),
  src = NULL
) {
  preprocess_pooled_support_masks(
    list(list(constraint = sub_plan, snapshot_times = snapshot_times)),
    model = model,
    nodes = nodes,
    nodes2 = nodes2,
    symmetric = symmetric,
    prep_envir = prep_envir,
    src = src
  )[[1L]]
}

#' Maintain a shared atom pool once, project a per-fid mask from it
#'
#' Several constraints over the SAME atoms (a `mutually_exclusive` layer's
#' `!tie(L)` / `tie(L)` derived constraints, or those AND-composed with a shared
#' user constraint) differ only in their boolean expression, not in the atom
#' stream they walk. Maintaining that stream once and projecting each
#' constraint's own expression from it removes the redundant walks the per-output
#' `preprocess_support_mask()` incurs, while evaluation correctly stays per
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
#' @param model,nodes,nodes2,symmetric,prep_envir,src as in
#'   [preprocess_support_mask()]; `symmetric` is uniform across the family.
#' @return a list aligned with `requests`; each element is the `support_mask`
#'   list `preprocess_support_mask()` returns, or `NULL` for a `NULL` constraint.
#' @noRd
preprocess_pooled_support_masks <- function(
  requests,
  model,
  nodes,
  nodes2,
  symmetric = FALSE,
  prep_envir = new.env(),
  src = NULL
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
    atoms <- build_atom_maintainer(
      constraints[[1L]],
      model,
      nodes,
      nodes2,
      prep_envir = prep_envir,
      src = src
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

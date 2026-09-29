# An independent atom walk, kept as a test oracle.
#
# `build_atom_maintainer()` advances a support constraint's atoms over their
# own event streams, with its own state container, fetch and schedule, and
# `preprocess_support_mask()` realizes one constraint's mask stream from it.
# The package no longer uses either: every object a constraint's atoms read is
# on the merged walk's shared schedule, and the atoms are recorded there by
# `build_constraint_atom_store()`. They are kept here as a second
# implementation of atom maintenance, the reference the mask-maintainer tests
# check `build_mask_maintainer()` and `preprocess_pooled_support_masks()`
# against, since a reference computed by the code under test proves nothing.

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
#' The merged walk no longer builds this private walk: every object a
#' constraint's atoms read is on the shared schedule, so the atoms are
#' recorded by `build_constraint_atom_store()` as the walk moves. A test
#' passes it to `preprocess_pooled_support_masks()` as `atoms_factory` to get
#' an independently maintained atom stream.
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
    src = src,
    atoms_factory = build_atom_maintainer
  )[[1L]]
}

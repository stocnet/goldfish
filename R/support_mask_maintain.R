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
# The atoms' live per-cell values are kept dense here (one n1 x n2 matrix per
# atom) and the boolean tree is evaluated over them. The axis-union storage kind
# (vector/scalar for separable ego/global constraints) is a memory
# optimization deferred to a later slice; correctness of the mask timeline does
# not depend on it. Likewise the atoms are re-derived from their update closures
# per touched cell — the same locality as the interaction second-hop — rather
# than a whole-matrix recompute.

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
  prep_envir = new.env()
) {
  effects <- sub_plan$effect_functions
  objects_effects_link <- sub_plan$objects_effects_link
  events_objects_link <- sub_plan$events_objects_link
  atom_kinds <- sub_plan$atom_kinds
  expr <- sub_plan$expr
  n_atoms <- length(effects)

  n1 <- nrow(get(nodes, envir = prep_envir))
  n2 <- nrow(get(nodes2, envir = prep_envir))

  events <- fetch_events(sub_plan$fetch_plan, envir = prep_envir)
  schedule <- build_event_schedule(
    events,
    events_objects_link,
    sub_plan$objects
  )

  state <- build_state_container(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = prep_envir
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
    envir = prep_envir
  )
  atom_state <- new.env(parent = emptyenv())
  for (a in seq_len(n_atoms)) {
    assign(
      as.character(a),
      matrix(stat_cache[[a]]$stat, n1, n2),
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

  eval_mask <- function() {
    atom_values <- stats::setNames(
      lapply(seq_len(n_atoms), function(a) {
        get(as.character(a), envir = atom_state)
      }),
      paste0(".a", seq_len(n_atoms))
    )
    m <- assemble_support_mask(atom_values, expr)
    m <- matrix(as.logical(m), n1, n2)
    if (symmetric) {
      m <- symmetrize_mask(m)
    }
    m
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
        eu2 <- call_atom_template(gid, shape, ea2, net_update_pos, att_update_pos)
        if (!is.null(eu2$cache)) {
          stat_cache[[gid]] <<- eu2$cache
        }
        updates <- rbind(updates, eu2$changes)
      }
      if (!is.null(updates)) {
        exp <- expand_operand_update(updates, atom_kinds[gid], n1, n2)
        am <- get(as.character(gid), envir = atom_state)
        am[exp$cells] <- exp$vals
        assign(as.character(gid), am, envir = atom_state)
      }
    }

    if (shape == "global") {
      state$globals[[key]] <<- event_args$replace
    } else if (shape == "node") {
      state[[component]][[key]][event_args$node] <<- event_args$replace
    } else {
      state$networks[[key]][event_args$sender, event_args$receiver] <<-
        event_args$replace
      if (is_undirected_net) {
        state$networks[[key]][event_args$receiver, event_args$sender] <<-
          event_args$replace
      }
    }
  }

  support_init <- eval_mask()
  # The support is piecewise-constant, changing only at the atoms' events.
  # Snapshot it at each requested time using the atoms strictly before that time
  # (a lagged risk set). Snapshots are taken in time order (mapping
  # back to the caller's event order) so the atom stream is advanced once.
  atom_ks <- which(!is.na(schedule$target))
  atom_times <- schedule$time[atom_ks]
  n_snap <- length(snapshot_times)
  support <- vector("list", n_snap)
  next_atom <- 1L
  n_atom <- length(atom_ks)
  for (idx in order(snapshot_times)) {
    tt <- snapshot_times[idx]
    while (next_atom <= n_atom && atom_times[next_atom] < tt) {
      apply_atom_event(atom_ks[next_atom])
      next_atom <- next_atom + 1L
    }
    support[[idx]] <- eval_mask()
  }

  list(
    support = support,
    initial = support_init,
    mask_kind = sub_plan$mask_kind,
    n_stored = n_snap,
    symmetric = symmetric
  )
}

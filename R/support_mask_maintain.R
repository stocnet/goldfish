####################### #
#
# Goldfish package
# Maintaining the support_constraint active mask over the event sequence
#
####################### #

# The support mask is realized as a derived object (design D7): its atoms are
# plain effects, so they are maintained with the SAME recipe builders the
# estimated formula uses (state container, event schedule, effect templates,
# stat cache, per-object routing) — but over the constraint sub-plan alone, in a
# self-contained pass that shares no state with the main statistics loop. Keeping
# it separate means the unconstrained statistics path is untouched, so the 1e-6
# baselines cannot move; the pass only runs when a `support_constraint` is
# present.
#
# The atoms' live per-cell values are kept dense here (one n1 x n2 matrix per
# atom) and the boolean tree is evaluated over them. The axis-union storage kind
# (design D2: vector/scalar for separable ego/global constraints) is a memory
# optimization deferred to a later slice; correctness of the mask timeline does
# not depend on it. Likewise the atoms are re-derived from their update closures
# per touched cell — the same locality as the interaction second-hop — rather
# than a whole-matrix recompute.

#' Maintain the support-constraint mask across the event sequence
#'
#' Runs a stripped recipe pass over the constraint sub-plan, snapshotting the
#' support mask (the dyadic constraint before presence conjunction, design D10)
#' at every dependent event. Presence (`active_1`/`active_2`) is conjoined later
#' by the gather consumer via [assemble_model_mask()].
#'
#' @param sub_plan the compiled constraint sub-plan from
#'   `compile_support_constraint()` (its `effect_functions`, registries, links,
#'   `expr`, `atom_kinds`, `mask_kind`, and `fetch_plan`).
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prepEnvir`.
#' @param symmetric symmetrise the support (coordination / undirected REM, D11).
#' @param startTime,endTime the preprocessing window bounds.
#' @param prepEnvir environment holding the realized data objects.
#' @return a list with `support` (a list of n1 x n2 logical matrices, one per
#'   dependent event), `initial` (the support mask before any event),
#'   `mask_kind`, `n_dependent`, and `symmetric`.
#' @noRd
preprocess_support_mask <- function(
  sub_plan,
  model,
  nodes,
  nodes2,
  symmetric = FALSE,
  startTime = NULL,
  endTime = NULL,
  prepEnvir = new.env()
) {
  effects <- sub_plan$effect_functions
  objects_effects_link <- sub_plan$objects_effects_link
  events_objects_link <- sub_plan$events_objects_link
  atom_kinds <- sub_plan$atom_kinds
  expr <- sub_plan$expr
  n_atoms <- length(effects)

  n1 <- nrow(get(nodes, envir = prepEnvir))
  n2 <- nrow(get(nodes2, envir = prepEnvir))

  events <- fetch_events(sub_plan$fetch_plan, envir = prepEnvir)
  schedule <- build_event_schedule(
    events,
    events_objects_link,
    sub_plan$objects
  )

  state <- build_state_container(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = prepEnvir
  )
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    state
  )

  # Initial atom values (dense n1 x n2 each) and their live caches, seeded from
  # the objects' state before any event.
  statCache <- initializeCacheStat(
    objectsEffectsLink = objects_effects_link,
    effects = effects,
    groupsNetwork = NULL,
    windowParameters = vector("list", n_atoms),
    n1 = n1,
    n2 = n2,
    model = model,
    subModel = sub_plan$atom_sub_model,
    envir = prepEnvir
  )
  atom_state <- new.env(parent = emptyenv())
  for (a in seq_len(n_atoms)) {
    assign(
      as.character(a),
      matrix(statCache[[a]]$stat, n1, n2),
      envir = atom_state
    )
  }
  statCache <- lapply(statCache, "[[", "cache")

  netUpdateLookup <- matrix(NA_integer_, nrow(sub_plan$objects), n_atoms)
  attUpdateLookup <- matrix(NA_integer_, nrow(sub_plan$objects), n_atoms)
  netUpdateLookup[cbind(
    sub_plan$effect_objects$oid,
    sub_plan$effect_objects$gid
  )] <- sub_plan$effect_objects$net_update
  attUpdateLookup[cbind(
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
        cache = statCache[[gid]],
        n1 = n1,
        n2 = n2,
        netUpdate = net_update,
        attUpdate = att_update,
        eventOrder = 0L,
        interEventTime = 0
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

  support_init <- eval_mask()
  support <- vector("list", sum(schedule$dependent))
  dep_idx <- 0L

  for (k in seq_len(schedule$n)) {
    if (schedule$dependent[k]) {
      dep_idx <- dep_idx + 1L
      support[[dep_idx]] <- eval_mask()
      next
    }

    oid <- schedule$target[k]
    if (is.na(oid)) {
      next
    }
    component <- sub_plan$objects$component[oid]
    key <- sub_plan$objects$key[oid]
    shape <- schedule$shape[k]
    isUndirectedNet <- sub_plan$objects$is_undirected[oid]

    if (shape == "global") {
      replaceValue <- schedule$value[[k]]
      if (is.na(replaceValue)) {
        replaceValue <- 0
      }
      event_args <- list(replace = replaceValue)
    } else if (shape == "node") {
      eventNode <- schedule$node[k]
      if (schedule$semantics[k] == "increment") {
        incrementValue <- schedule$value[[k]]
        if (is.na(incrementValue)) {
          incrementValue <- 0
        }
        replaceValue <- state[[component]][[key]][eventNode] + incrementValue
      } else {
        replaceValue <- schedule$value[[k]]
      }
      event_args <- list(node = eventNode, replace = replaceValue)
    } else {
      eventSender <- schedule$sender[k]
      eventReceiver <- schedule$receiver[k]
      if (schedule$semantics[k] == "increment") {
        incrementValue <- schedule$value[[k]]
        if (is.na(incrementValue)) {
          incrementValue <- 0
        }
        replaceValue <-
          state$networks[[key]][eventSender, eventReceiver] + incrementValue
      } else {
        replaceValue <- schedule$value[[k]]
      }
      event_args <- list(
        sender = eventSender,
        receiver = eventReceiver,
        replace = replaceValue
      )
    }

    # Route the change to every atom reading this object, apply its delta to the
    # atom's live matrix (pre-event state, mirroring the main loop's op_state).
    for (gid in sub_plan$routing[[oid]]) {
      netUpdatePos <- netUpdateLookup[oid, gid]
      if (is.na(netUpdatePos)) {
        netUpdatePos <- NULL
      }
      attUpdatePos <- attUpdateLookup[oid, gid]
      if (is.na(attUpdatePos)) {
        attUpdatePos <- NULL
      }

      effectUpdate <- call_atom_template(
        gid,
        shape,
        event_args,
        netUpdatePos,
        attUpdatePos
      )
      if (!is.null(effectUpdate$cache)) {
        statCache[[gid]] <- effectUpdate$cache
      }
      updates <- effectUpdate$changes
      if (isUndirectedNet) {
        ea2 <- event_args
        ea2$sender <- event_args$receiver
        ea2$receiver <- event_args$sender
        eu2 <- call_atom_template(gid, shape, ea2, netUpdatePos, attUpdatePos)
        if (!is.null(eu2$cache)) {
          statCache[[gid]] <- eu2$cache
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

    # Commit the object change to state (post-event), as the main loop does.
    if (shape == "global") {
      state$globals[[key]] <- event_args$replace
    } else if (shape == "node") {
      state[[component]][[key]][event_args$node] <- event_args$replace
    } else {
      state$networks[[key]][event_args$sender, event_args$receiver] <-
        event_args$replace
      if (isUndirectedNet) {
        state$networks[[key]][event_args$receiver, event_args$sender] <-
          event_args$replace
      }
    }
  }

  list(
    support = support,
    initial = support_init,
    mask_kind = sub_plan$mask_kind,
    n_dependent = dep_idx,
    symmetric = symmetric
  )
}

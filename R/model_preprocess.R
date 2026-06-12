PREPROCESSED_GOLDFISH_VERSION <- 2L

#' Preprocess a model given its specification
#'
#' S3 generic dispatched on the model specification class (design D1).
#' Model variants converted to the recipe architecture implement a dedicated
#' method; the remaining variants fall back to the monolithic loop through
#' `preprocess.model_spec()` until their recipe lands.
#'
#' @param spec a `model_spec` object from `new_model_spec()`.
#' @param ... arguments passed to the recipe methods, see
#'   `preprocess_monolith()` and `run_sender_recipe_loop()`.
#'
#' @return a list of class preprocessed.goldfish
#' @noRd
preprocess <- function(spec, ...) {
  UseMethod("preprocess")
}

#' @noRd
preprocess.model_spec <- function(spec, ...) {
  legacy_sub_model <- if (inherits(spec, "sender_spec")) "rate" else "choice"
  preprocess_monolith(model = spec$model, subModel = legacy_sub_model, ...)
}

#' @noRd
preprocess.dynam_rate_spec <- function(spec, ...) {
  run_sender_recipe_loop(
    spec, ...,
    right_censored = TRUE,
    intercept_scalars = TRUE
  )
}

#' @noRd
preprocess.dynam_rate_ordered_spec <- function(spec, ...) {
  run_sender_recipe_loop(
    spec, ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' @noRd
preprocess.dynam_choice_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec, ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' @noRd
preprocess.dynam_choice_coord_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec, ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' @noRd
preprocess.rem_rate_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec, ...,
    right_censored = TRUE,
    intercept_scalars = TRUE
  )
}

#' @noRd
preprocess.rem_rate_ordered_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec, ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' DyNAMi recipe wrappers
#'
#' Thin wrappers delegating to the existing monolithic DyNAMi preprocessing
#' loop with unchanged arguments (design D9). The dedicated DyNAMi recipe
#' (post-event update order, `subType` normalisation) is deferred to the
#' effects unification change. `preprocessInteraction()` keeps computing its
#' own start and end times from the event streams, as it did before the
#' dispatch wiring.
#'
#' @inheritParams run_sender_recipe_loop
#' @param groupsNetwork character, name of the groups network object.
#' @param ... absorbs the recipe arguments that the DyNAMi loop does not
#'   consume (`windowParameters`, `ignoreRepParameter`, `is_two_mode`,
#'   `startTime`, `endTime`, `opportunitiesList`).
#' @name preprocess_dynami
#' @noRd
run_dynami_monolith <- function(
  sub_model,
  events,
  effects,
  eventsObjectsLink,
  eventsEffectsLink,
  objectsEffectsLink,
  nodes,
  nodes2 = nodes,
  rightCensored = FALSE,
  progress = FALSE,
  groupsNetwork = NULL,
  prepEnvir = new.env()
) {
  prep <- preprocessInteraction(
    subModel = sub_model,
    events = events,
    effects = effects,
    eventsObjectsLink = eventsObjectsLink,
    eventsEffectsLink = eventsEffectsLink,
    objectsEffectsLink = objectsEffectsLink,
    nodes = nodes,
    nodes2 = nodes2,
    rightCensored = rightCensored,
    progress = progress,
    groupsNetwork = groupsNetwork,
    prepEnvir = prepEnvir
  )
  prep$version <- PREPROCESSED_GOLDFISH_VERSION
  prep
}

#' @rdname preprocess_dynami
#' @noRd
preprocess.dynami_rate_spec <- function(
  spec, events, effects, eventsObjectsLink, eventsEffectsLink,
  objectsEffectsLink, nodes, nodes2 = nodes, rightCensored = FALSE,
  progress = FALSE, groupsNetwork = NULL, prepEnvir = new.env(), ...
) {
  run_dynami_monolith(
    "rate", events, effects, eventsObjectsLink, eventsEffectsLink,
    objectsEffectsLink, nodes, nodes2, rightCensored, progress,
    groupsNetwork, prepEnvir
  )
}

#' @rdname preprocess_dynami
#' @noRd
preprocess.dynami_rate_ordered_spec <- function(
  spec, events, effects, eventsObjectsLink, eventsEffectsLink,
  objectsEffectsLink, nodes, nodes2 = nodes, rightCensored = FALSE,
  progress = FALSE, groupsNetwork = NULL, prepEnvir = new.env(), ...
) {
  run_dynami_monolith(
    "rate", events, effects, eventsObjectsLink, eventsEffectsLink,
    objectsEffectsLink, nodes, nodes2, rightCensored, progress,
    groupsNetwork, prepEnvir
  )
}

#' @rdname preprocess_dynami
#' @noRd
preprocess.dynami_choice_spec <- function(
  spec, events, effects, eventsObjectsLink, eventsEffectsLink,
  objectsEffectsLink, nodes, nodes2 = nodes, rightCensored = FALSE,
  progress = FALSE, groupsNetwork = NULL, prepEnvir = new.env(), ...
) {
  run_dynami_monolith(
    "choice", events, effects, eventsObjectsLink, eventsEffectsLink,
    objectsEffectsLink, nodes, nodes2, rightCensored, progress,
    groupsNetwork, prepEnvir
  )
}

#' Sender-indexed recipe kernel
#'
#' Shared event loop for the sender-indexed model variants (design D22).
#' It consumes the three recipe input structures built once before the loop
#' (design D21): the state container owned by the recipe (design D20), the
#' merged event schedule, and the compiled update plan. Statistic updates
#' are written into one flat `stat_mat_update` buffer with doubling growth
#' covering dependent and right-censored events (design D2); `initialStats`
#' is kept in the sender-native `n1 x nEffects` form. Global-attribute
#' events update the `globals` component of the state container and emit
#' right-censored statistic updates without sender/receiver recording
#' (design D16).
#'
#' @param spec a `sender_spec` model specification.
#' @inheritParams preprocess_monolith
#' @param right_censored logical, whether right-censored events are stored.
#' @param intercept_scalars logical, whether `n_dep_events`, `total_time`,
#'   and `avg_active_actors` are computed and stored.
#' @param ... absorbs arguments of `preprocess_monolith()` that the kernel
#'   does not consume (`is_two_mode`, `rightCensored`,
#'   `ignoreRepParameter`, `opportunitiesList`).
#'
#' @return a list of class preprocessed.goldfish
#' @noRd
run_sender_recipe_loop <- function(
  spec,
  events,
  effects,
  windowParameters,
  eventsObjectsLink,
  eventsEffectsLink,
  objectsEffectsLink,
  nodes,
  nodes2 = nodes,
  startTime = NULL,
  endTime = NULL,
  right_censored = FALSE,
  intercept_scalars = FALSE,
  progress = FALSE,
  prepEnvir = new.env(),
  ...
) {
  n1 <- nrow(get(nodes, envir = prepEnvir))
  n2 <- nrow(get(nodes2, envir = prepEnvir))
  nEffects <- length(effects)

  hasEndTime <- FALSE
  hasStartTime <- FALSE
  isValidEvent <- TRUE

  isWindowEffect <- !vapply(windowParameters, is.null, logical(1))
  whichEventNoWindowEffect <- eventsEffectsLink[, !isWindowEffect, drop = FALSE]
  whichEventNoWindowEffect <- rowSums(!is.na(whichEventNoWindowEffect))
  whichEventNoWindowEffect <- c(1, which(whichEventNoWindowEffect > 0))

  eventsMin <- min(vapply(
    events[whichEventNoWindowEffect],
    function(x) min(x$time),
    double(1)
  ))
  eventsMax <- max(vapply(
    events[whichEventNoWindowEffect],
    function(x) max(x$time),
    double(1)
  ))
  if (is.null(endTime)) {
    endTime <- eventsMax
    if (any(isWindowEffect)) hasEndTime <- TRUE
  } else if (endTime != eventsMax) {
    if (!is.numeric(endTime)) {
      endTime <- as.numeric(endTime)
    }
    if (eventsMin > endTime) {
      stop("End time smaller than first event time.", call. = FALSE)
    }
    hasEndTime <- TRUE
  }
  if (is.null(startTime)) {
    startTime <- eventsMin
  } else if (startTime != eventsMin) {
    if (!is.numeric(startTime)) {
      startTime <- as.numeric(startTime)
    }
    if (eventsMax < startTime) {
      stop("Start time geater than last event time.", call. = FALSE)
    }
    hasStartTime <- TRUE
    if (eventsMin < startTime) isValidEvent <- FALSE
  }

  imputed <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)

  if (progress) {
    cat("Initializing cache objects and statistical matrices.\n")
  }

  statCache <- initializeCacheStat(
    objectsEffectsLink = objectsEffectsLink,
    effects = effects,
    groupsNetwork = NULL,
    windowParameters = windowParameters,
    n1 = n1,
    n2 = n2,
    model = spec$model,
    subModel = "rate",
    envir = prepEnvir
  )
  initialStats <- do.call(cbind, lapply(statCache, "[[", "stat"))
  statCache <- lapply(statCache, "[[", "cache")

  nodes_obj <- get(nodes, envir = prepEnvir)
  nodes2_obj <- get(nodes2, envir = prepEnvir)
  active_mode1_init <- if (!is.null(nodes_obj$present)) {
    nodes_obj$present
  } else {
    rep(TRUE, n1)
  }
  active_mode2_init <- if (!is.null(nodes2_obj$present)) {
    nodes2_obj$present
  } else {
    rep(TRUE, n2)
  }
  comp_events1 <- attr(nodes_obj, "events")[
    attr(nodes_obj, "dynamic_attribute") == "present"
  ]
  comp_events2 <- attr(nodes2_obj, "events")[
    attr(nodes2_obj, "dynamic_attribute") == "present"
  ]
  active_mode1_changes <- if (
    length(comp_events1) > 0 && !is.na(comp_events1[1])
  ) {
    cc <- get(comp_events1[1], envir = prepEnvir)
    node_idx1 <- if (is.character(cc$node)) {
      match(cc$node, nodes_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx1[i], replace = cc$replace[i])
    })
  } else {
    list()
  }
  active_mode2_changes <- if (
    length(comp_events2) > 0 && !is.na(comp_events2[1])
  ) {
    cc <- get(comp_events2[1], envir = prepEnvir)
    node_idx2 <- if (is.character(cc$node)) {
      match(cc$node, nodes2_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx2[i], replace = cc$replace[i])
    })
  } else {
    list()
  }

  state <- build_state_container(
    rownames(objectsEffectsLink), nodes, nodes2,
    envir = prepEnvir
  )
  plan <- build_update_plan(
    effects, eventsObjectsLink, eventsEffectsLink, objectsEffectsLink,
    state,
    stat_kind = "sender", envir = prepEnvir
  )
  schedule <- build_event_schedule(events, eventsObjectsLink, plan$objects)

  netUpdateLookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  attUpdateLookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  netUpdateLookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$net_update
  attUpdateLookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$att_update

  call_effect_template <- function(
    template, gid, shape, event_args, net_update, att_update,
    event_order, inter_event_time
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
        cache = statCache[[gid]],
        n1 = n1,
        n2 = n2,
        netUpdate = net_update,
        attUpdate = att_update,
        eventOrder = event_order,
        interEventTime = inter_event_time
      ),
      event_args
    )
    do.call(template$fun, args[template$args_by_shape[[shape]]])
  }

  buf_capacity <- max(1000L, nEffects * nrow(events[[1L]]))
  stat_mat_buf <- matrix(0, 4L, buf_capacity)
  buf_n <- 0L

  write_pending <- function(blocks, n_cols) {
    if (n_cols > 0L) {
      while (buf_n + n_cols > buf_capacity) {
        buf_capacity <<- buf_capacity * 2L
        new_buf <- matrix(0, 4L, buf_capacity)
        if (buf_n > 0L) {
          new_buf[, seq_len(buf_n)] <- stat_mat_buf[, seq_len(buf_n)]
        }
        stat_mat_buf <<- new_buf
      }
      stat_mat_buf[, buf_n + seq_len(n_cols)] <<- do.call(cbind, blocks)
      buf_n <<- buf_n + n_cols
    }
    invisible(NULL)
  }

  max_store <- schedule$n + 1L
  stat_mat_pointer <- integer(max_store)
  intervals <- numeric(max_store)
  is_dependent <- integer(max_store)
  event_time <- numeric(max_store)
  event_sender <- integer(max_store)
  event_receiver <- integer(max_store)
  n_stored <- 0L

  pending_dep <- list()
  pending_dep_cols <- 0L
  pending_rc <- list()
  pending_rc_cols <- 0L

  iTotalEvents <- 0L
  iDependentEvents <- 0L
  time <- startTime
  interval <- 0
  finalStep <- FALSE

  if (progress) {
    cat("Preprocessing events.\n", startTime, endTime, schedule$n)
    pb <- utils::txtProgressBar(max = schedule$n, char = "*", style = 3)
    dotEvents <- ifelse(schedule$n > 50, ceiling(schedule$n / 50), 1)
  }

  for (k in seq_len(schedule$n)) {
    iTotalEvents <- iTotalEvents + 1L
    nextEventTime <- schedule$time[k]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && nextEventTime <= endTime) {
        interval <- nextEventTime - time
      } else if (isValidEvent && nextEventTime > endTime) {
        interval <- endTime - time
        nextEventTime <- endTime
        finalStep <- TRUE
      } else if (!isValidEvent && nextEventTime >= startTime) {
        interval <- nextEventTime - startTime
        isValidEvent <- TRUE
      }
    } else {
      interval <- nextEventTime - time
    }

    time <- nextEventTime

    isDependent <- schedule$dependent[k] && !finalStep

    if (progress && iTotalEvents %% dotEvents == 0) {
      utils::setTxtProgressBar(pb, iTotalEvents)
    }

    if (isValidEvent && isDependent) {
      iDependentEvents <- 1L + iDependentEvents
      n_stored <- n_stored + 1L
      write_pending(pending_dep, pending_dep_cols)
      stat_mat_pointer[n_stored] <- buf_n
      intervals[n_stored] <- interval
      is_dependent[n_stored] <- 1L
      event_time[n_stored] <- time
      if (schedule$shape[k] == "node") {
        event_sender[n_stored] <- schedule$node[k]
        event_receiver[n_stored] <- schedule$node[k]
      } else {
        event_sender[n_stored] <- schedule$sender[k]
        event_receiver[n_stored] <- schedule$receiver[k]
      }
      pending_dep <- list()
      pending_dep_cols <- 0L
      pending_rc <- list()
      pending_rc_cols <- 0L
    } else if (!isDependent) {
      if (isValidEvent && right_censored && interval > 0) {
        n_stored <- n_stored + 1L
        write_pending(pending_rc, pending_rc_cols)
        stat_mat_pointer[n_stored] <- buf_n
        intervals[n_stored] <- interval
        is_dependent[n_stored] <- 0L
        event_time[n_stored] <- time
        if (schedule$shape[k] == "global") {
          event_sender[n_stored] <- NA_integer_
          event_receiver[n_stored] <- NA_integer_
        } else if (schedule$shape[k] == "node") {
          event_sender[n_stored] <- schedule$node[k]
          event_receiver[n_stored] <- schedule$node[k]
        } else {
          event_sender[n_stored] <- schedule$sender[k]
          event_receiver[n_stored] <- schedule$receiver[k]
        }
        pending_rc <- list()
        pending_rc_cols <- 0L
      }

      if (!finalStep) {
        oid <- schedule$target[k]
        component <- plan$objects$component[oid]
        key <- plan$objects$key[oid]
        shape <- schedule$shape[k]
        isUndirectedNet <- plan$objects$is_undirected[oid]

        if (shape == "global") {
          replaceValue <- schedule$value[[k]]
          if (is.na(replaceValue)) replaceValue <- 0
          event_args <- list(replace = replaceValue)
        } else if (shape == "node") {
          eventNode <- schedule$node[k]
          if (schedule$semantics[k] == "increment") {
            incrementValue <- schedule$value[[k]]
            if (is.na(incrementValue)) incrementValue <- 0
            replaceValue <-
              state[[component]][[key]][eventNode] + incrementValue
          } else {
            replaceValue <- schedule$value[[k]]
            if (is.na(replaceValue)) {
              replaceValue <- mean(
                state[[component]][[key]][-eventNode],
                na.rm = TRUE
              )
            }
          }
          event_args <- list(node = eventNode, replace = replaceValue)
        } else {
          eventSender <- schedule$sender[k]
          eventReceiver <- schedule$receiver[k]
          if (schedule$semantics[k] == "increment") {
            incrementValue <- schedule$value[[k]]
            if (is.na(incrementValue)) incrementValue <- 0
            replaceValue <-
              state$networks[[key]][eventSender, eventReceiver] +
              incrementValue
          } else {
            replaceValue <- schedule$value[[k]]
            if (is.na(replaceValue)) replaceValue <- 0
          }
          if (replaceValue < 0) {
            warning(
              "You are dissolving a tie which doesn't exist!",
              call. = FALSE
            )
          }
          event_args <- list(
            sender = eventSender, receiver = eventReceiver,
            replace = replaceValue
          )
        }

        for (gid in plan$routing[[oid]]) {
          template <- plan$templates[[gid]]
          netUpdatePos <- netUpdateLookup[oid, gid]
          if (is.na(netUpdatePos)) netUpdatePos <- NULL
          attUpdatePos <- attUpdateLookup[oid, gid]
          if (is.na(attUpdatePos)) attUpdatePos <- NULL

          effectUpdate <- call_effect_template(
            template, gid, shape, event_args,
            netUpdatePos, attUpdatePos,
            iTotalEvents - iDependentEvents, interval
          )

          if (!is.null(attr(effectUpdate$cache, "lastUpdate"))) {
            attr(statCache[[gid]], "lastUpdate") <- attr(
              effectUpdate$cache,
              "lastUpdate"
            )
          }

          updates <- effectUpdate$changes
          if (!is.null(effectUpdate$cache) && !is.null(effectUpdate$changes)) {
            statCache[[gid]] <- effectUpdate$cache
          }

          if (isUndirectedNet) {
            event_args2 <- event_args
            event_args2$sender <- event_args$receiver
            event_args2$receiver <- event_args$sender
            effectUpdate2 <- call_effect_template(
              template, gid, shape, event_args2,
              netUpdatePos, attUpdatePos,
              iTotalEvents - iDependentEvents, interval
            )
            if (
              !is.null(effectUpdate2$cache) &&
                !is.null(effectUpdate2$changes)
            ) {
              statCache[[gid]] <- effectUpdate2$cache
            }
            updates <- rbind(updates, effectUpdate2$changes)
          }

          if (!is.null(updates)) {
            if (hasStartTime && nextEventTime < startTime) {
              initialStats[cbind(updates[, "node1"], gid)] <-
                updates[, "replace"]
            } else {
              block <- rbind(
                updates[, "node1"] - 1,
                0,
                gid - 1,
                updates[, "replace"]
              )
              pending_dep[[length(pending_dep) + 1L]] <- block
              pending_dep_cols <- pending_dep_cols + ncol(block)
              if (right_censored) {
                pending_rc[[length(pending_rc) + 1L]] <- block
                pending_rc_cols <- pending_rc_cols + ncol(block)
              }
            }
          }
        }

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
    }

    if (finalStep) break
  }

  if (progress) {
    utils::setTxtProgressBar(pb, schedule$n)
    close(pb)
  }

  stat_mat_update <- stat_mat_buf[, seq_len(buf_n), drop = FALSE]
  keep <- seq_len(n_stored)
  stat_mat_pointer <- stat_mat_pointer[keep]
  intervals <- intervals[keep]
  is_dependent <- is_dependent[keep]
  event_time <- event_time[keep]
  event_sender <- event_sender[keep]
  event_receiver <- event_receiver[keep]

  n_dep_events <- NULL
  total_time <- NULL
  avg_active_actors <- NULL
  if (intercept_scalars) {
    n_dep_events <- sum(is_dependent == 1L)
    total_time <- sum(intervals)
    nActors <- sum(active_mode1_init)
    if (length(active_mode1_changes) > 0 && n_stored > 0) {
      changesTime <- vapply(
        active_mode1_changes, `[[`, double(1), "time"
      )
      changesReplace <- vapply(
        active_mode1_changes, `[[`, logical(1), "replace"
      )
      timeAcc <- startTime
      previousTime <- -Inf
      activeAcc <- 0
      for (i in seq_len(n_stored)) {
        timeAcc <- timeAcc + intervals[i]
        changesAt <- changesTime > previousTime & changesTime <= timeAcc
        nActors <- nActors +
          sum(changesReplace[changesAt]) - sum(!changesReplace[changesAt])
        activeAcc <- activeAcc + nActors
        previousTime <- timeAcc
      }
      avg_active_actors <- activeAcc / n_stored
    } else {
      avg_active_actors <- nActors
    }
  }

  presence1_update <- NULL
  presence1_update_pointer <- NULL
  presence2_update <- NULL
  presence2_update_pointer <- NULL
  if (length(active_mode1_changes) > 0) {
    compChange1 <- data.frame(
      time = vapply(active_mode1_changes, `[[`, double(1), "time"),
      node = vapply(active_mode1_changes, `[[`, integer(1), "node"),
      replace = vapply(active_mode1_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange1, event_time)
    presence1_update <- temp$presenceUpdate
    presence1_update_pointer <- temp$presenceUpdatePointer
  }
  if (length(active_mode2_changes) > 0) {
    compChange2 <- data.frame(
      time = vapply(active_mode2_changes, `[[`, double(1), "time"),
      node = vapply(active_mode2_changes, `[[`, integer(1), "node"),
      replace = vapply(active_mode2_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange2, event_time)
    presence2_update <- temp$presenceUpdate
    presence2_update_pointer <- temp$presenceUpdatePointer
  }

  structure(
    list(
      initialStats = initialStats,
      stat_mat_update = stat_mat_update,
      stat_mat_pointer = stat_mat_pointer,
      intervals = intervals,
      is_dependent = is_dependent,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      event_pos = seq_len(n_stored),
      active_mode1_init = active_mode1_init,
      active_mode1_changes = active_mode1_changes,
      active_mode2_init = active_mode2_init,
      active_mode2_changes = active_mode2_changes,
      startTime = startTime,
      endTime = endTime,
      n_dep_events = n_dep_events,
      total_time = total_time,
      avg_active_actors = avg_active_actors,
      presence1_update = presence1_update,
      presence1_update_pointer = presence1_update_pointer,
      presence2_update = presence2_update,
      presence2_update_pointer = presence2_update_pointer,
      version = PREPROCESSED_GOLDFISH_VERSION
    ),
    class = "preprocessed.goldfish"
  )
}

#' Dyad-indexed recipe kernel
#'
#' Shared event loop for the dyad-indexed model variants (design D22).
#' It mirrors `run_sender_recipe_loop()` over the same three recipe input
#' structures (designs D20/D21) but produces dyad-shaped statistics:
#' `initialStats` is kept in the engine-native `n1 x n2 x nEffects` (3D)
#' form and the flat `stat_mat_update` buffer carries `node2` in its second
#' row. Right-censored events are stored only when the configuration sets
#' `right_censored = TRUE` (rate models with a time intercept); choice
#' configurations store dependent rows only, so the combined buffer carries
#' exclusively `is_dependent = 1` rows. Global-attribute events are handled
#' as in the sender kernel (design D16) so future choice-model interaction
#' support only touches the effects layer.
#'
#' @param spec a `dyad_spec` model specification.
#' @inheritParams run_sender_recipe_loop
#'
#' @return a list of class preprocessed.goldfish
#' @noRd
run_dyad_recipe_loop <- function(
  spec,
  events,
  effects,
  windowParameters,
  eventsObjectsLink,
  eventsEffectsLink,
  objectsEffectsLink,
  nodes,
  nodes2 = nodes,
  startTime = NULL,
  endTime = NULL,
  right_censored = FALSE,
  intercept_scalars = FALSE,
  progress = FALSE,
  prepEnvir = new.env(),
  ...
) {
  n1 <- nrow(get(nodes, envir = prepEnvir))
  n2 <- nrow(get(nodes2, envir = prepEnvir))
  nEffects <- length(effects)

  hasEndTime <- FALSE
  hasStartTime <- FALSE
  isValidEvent <- TRUE

  isWindowEffect <- !vapply(windowParameters, is.null, logical(1))
  whichEventNoWindowEffect <- eventsEffectsLink[, !isWindowEffect, drop = FALSE]
  whichEventNoWindowEffect <- rowSums(!is.na(whichEventNoWindowEffect))
  whichEventNoWindowEffect <- c(1, which(whichEventNoWindowEffect > 0))

  eventsMin <- min(vapply(
    events[whichEventNoWindowEffect],
    function(x) min(x$time),
    double(1)
  ))
  eventsMax <- max(vapply(
    events[whichEventNoWindowEffect],
    function(x) max(x$time),
    double(1)
  ))
  if (is.null(endTime)) {
    endTime <- eventsMax
    if (any(isWindowEffect)) hasEndTime <- TRUE
  } else if (endTime != eventsMax) {
    if (!is.numeric(endTime)) {
      endTime <- as.numeric(endTime)
    }
    if (eventsMin > endTime) {
      stop("End time smaller than first event time.", call. = FALSE)
    }
    hasEndTime <- TRUE
  }
  if (is.null(startTime)) {
    startTime <- eventsMin
  } else if (startTime != eventsMin) {
    if (!is.numeric(startTime)) {
      startTime <- as.numeric(startTime)
    }
    if (eventsMax < startTime) {
      stop("Start time geater than last event time.", call. = FALSE)
    }
    hasStartTime <- TRUE
    if (eventsMin < startTime) isValidEvent <- FALSE
  }

  imputed <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)

  if (progress) {
    cat("Initializing cache objects and statistical matrices.\n")
  }

  statCache <- initializeCacheStat(
    objectsEffectsLink = objectsEffectsLink,
    effects = effects,
    groupsNetwork = NULL,
    windowParameters = windowParameters,
    n1 = n1,
    n2 = n2,
    model = spec$model,
    subModel = "choice",
    envir = prepEnvir
  )
  initialStats <- array(
    unlist(lapply(statCache, "[[", "stat")),
    dim = c(n1, n2, nEffects)
  )
  statCache <- lapply(statCache, "[[", "cache")

  nodes_obj <- get(nodes, envir = prepEnvir)
  nodes2_obj <- get(nodes2, envir = prepEnvir)
  active_mode1_init <- if (!is.null(nodes_obj$present)) {
    nodes_obj$present
  } else {
    rep(TRUE, n1)
  }
  active_mode2_init <- if (!is.null(nodes2_obj$present)) {
    nodes2_obj$present
  } else {
    rep(TRUE, n2)
  }
  comp_events1 <- attr(nodes_obj, "events")[
    attr(nodes_obj, "dynamic_attribute") == "present"
  ]
  comp_events2 <- attr(nodes2_obj, "events")[
    attr(nodes2_obj, "dynamic_attribute") == "present"
  ]
  active_mode1_changes <- if (
    length(comp_events1) > 0 && !is.na(comp_events1[1])
  ) {
    cc <- get(comp_events1[1], envir = prepEnvir)
    node_idx1 <- if (is.character(cc$node)) {
      match(cc$node, nodes_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx1[i], replace = cc$replace[i])
    })
  } else {
    list()
  }
  active_mode2_changes <- if (
    length(comp_events2) > 0 && !is.na(comp_events2[1])
  ) {
    cc <- get(comp_events2[1], envir = prepEnvir)
    node_idx2 <- if (is.character(cc$node)) {
      match(cc$node, nodes2_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx2[i], replace = cc$replace[i])
    })
  } else {
    list()
  }

  state <- build_state_container(
    rownames(objectsEffectsLink), nodes, nodes2,
    envir = prepEnvir
  )
  plan <- build_update_plan(
    effects, eventsObjectsLink, eventsEffectsLink, objectsEffectsLink,
    state,
    stat_kind = "dyad", envir = prepEnvir
  )
  schedule <- build_event_schedule(events, eventsObjectsLink, plan$objects)

  netUpdateLookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  attUpdateLookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  netUpdateLookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$net_update
  attUpdateLookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$att_update

  call_effect_template <- function(
    template, gid, shape, event_args, net_update, att_update,
    event_order, inter_event_time
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
        cache = statCache[[gid]],
        n1 = n1,
        n2 = n2,
        netUpdate = net_update,
        attUpdate = att_update,
        eventOrder = event_order,
        interEventTime = inter_event_time
      ),
      event_args
    )
    do.call(template$fun, args[template$args_by_shape[[shape]]])
  }

  buf_capacity <- max(1000L, nEffects * nrow(events[[1L]]))
  stat_mat_buf <- matrix(0, 4L, buf_capacity)
  buf_n <- 0L

  write_pending <- function(blocks, n_cols) {
    if (n_cols > 0L) {
      while (buf_n + n_cols > buf_capacity) {
        buf_capacity <<- buf_capacity * 2L
        new_buf <- matrix(0, 4L, buf_capacity)
        if (buf_n > 0L) {
          new_buf[, seq_len(buf_n)] <- stat_mat_buf[, seq_len(buf_n)]
        }
        stat_mat_buf <<- new_buf
      }
      stat_mat_buf[, buf_n + seq_len(n_cols)] <<- do.call(cbind, blocks)
      buf_n <<- buf_n + n_cols
    }
    invisible(NULL)
  }

  max_store <- schedule$n + 1L
  stat_mat_pointer <- integer(max_store)
  intervals <- numeric(max_store)
  is_dependent <- integer(max_store)
  event_time <- numeric(max_store)
  event_sender <- integer(max_store)
  event_receiver <- integer(max_store)
  n_stored <- 0L

  pending_dep <- list()
  pending_dep_cols <- 0L
  pending_rc <- list()
  pending_rc_cols <- 0L

  iTotalEvents <- 0L
  iDependentEvents <- 0L
  time <- startTime
  interval <- 0
  finalStep <- FALSE

  if (progress) {
    cat("Preprocessing events.\n", startTime, endTime, schedule$n)
    pb <- utils::txtProgressBar(max = schedule$n, char = "*", style = 3)
    dotEvents <- ifelse(schedule$n > 50, ceiling(schedule$n / 50), 1)
  }

  for (k in seq_len(schedule$n)) {
    iTotalEvents <- iTotalEvents + 1L
    nextEventTime <- schedule$time[k]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && nextEventTime <= endTime) {
        interval <- nextEventTime - time
      } else if (isValidEvent && nextEventTime > endTime) {
        interval <- endTime - time
        nextEventTime <- endTime
        finalStep <- TRUE
      } else if (!isValidEvent && nextEventTime >= startTime) {
        interval <- nextEventTime - startTime
        isValidEvent <- TRUE
      }
    } else {
      interval <- nextEventTime - time
    }

    time <- nextEventTime

    isDependent <- schedule$dependent[k] && !finalStep

    if (progress && iTotalEvents %% dotEvents == 0) {
      utils::setTxtProgressBar(pb, iTotalEvents)
    }

    if (isValidEvent && isDependent) {
      iDependentEvents <- 1L + iDependentEvents
      n_stored <- n_stored + 1L
      write_pending(pending_dep, pending_dep_cols)
      stat_mat_pointer[n_stored] <- buf_n
      intervals[n_stored] <- interval
      is_dependent[n_stored] <- 1L
      event_time[n_stored] <- time
      if (schedule$shape[k] == "node") {
        event_sender[n_stored] <- schedule$node[k]
        event_receiver[n_stored] <- schedule$node[k]
      } else {
        event_sender[n_stored] <- schedule$sender[k]
        event_receiver[n_stored] <- schedule$receiver[k]
      }
      pending_dep <- list()
      pending_dep_cols <- 0L
      pending_rc <- list()
      pending_rc_cols <- 0L
    } else if (!isDependent) {
      if (isValidEvent && right_censored && interval > 0) {
        n_stored <- n_stored + 1L
        write_pending(pending_rc, pending_rc_cols)
        stat_mat_pointer[n_stored] <- buf_n
        intervals[n_stored] <- interval
        is_dependent[n_stored] <- 0L
        event_time[n_stored] <- time
        if (schedule$shape[k] == "global") {
          event_sender[n_stored] <- NA_integer_
          event_receiver[n_stored] <- NA_integer_
        } else if (schedule$shape[k] == "node") {
          event_sender[n_stored] <- schedule$node[k]
          event_receiver[n_stored] <- schedule$node[k]
        } else {
          event_sender[n_stored] <- schedule$sender[k]
          event_receiver[n_stored] <- schedule$receiver[k]
        }
        pending_rc <- list()
        pending_rc_cols <- 0L
      }

      if (!finalStep) {
        oid <- schedule$target[k]
        component <- plan$objects$component[oid]
        key <- plan$objects$key[oid]
        shape <- schedule$shape[k]
        isUndirectedNet <- plan$objects$is_undirected[oid]

        if (shape == "global") {
          replaceValue <- schedule$value[[k]]
          if (is.na(replaceValue)) replaceValue <- 0
          event_args <- list(replace = replaceValue)
        } else if (shape == "node") {
          eventNode <- schedule$node[k]
          if (schedule$semantics[k] == "increment") {
            incrementValue <- schedule$value[[k]]
            if (is.na(incrementValue)) incrementValue <- 0
            replaceValue <-
              state[[component]][[key]][eventNode] + incrementValue
          } else {
            replaceValue <- schedule$value[[k]]
            if (is.na(replaceValue)) {
              replaceValue <- mean(
                state[[component]][[key]][-eventNode],
                na.rm = TRUE
              )
            }
          }
          event_args <- list(node = eventNode, replace = replaceValue)
        } else {
          eventSender <- schedule$sender[k]
          eventReceiver <- schedule$receiver[k]
          if (schedule$semantics[k] == "increment") {
            incrementValue <- schedule$value[[k]]
            if (is.na(incrementValue)) incrementValue <- 0
            replaceValue <-
              state$networks[[key]][eventSender, eventReceiver] +
              incrementValue
          } else {
            replaceValue <- schedule$value[[k]]
            if (is.na(replaceValue)) replaceValue <- 0
          }
          if (replaceValue < 0) {
            warning(
              "You are dissolving a tie which doesn't exist!",
              call. = FALSE
            )
          }
          event_args <- list(
            sender = eventSender, receiver = eventReceiver,
            replace = replaceValue
          )
        }

        for (gid in plan$routing[[oid]]) {
          template <- plan$templates[[gid]]
          netUpdatePos <- netUpdateLookup[oid, gid]
          if (is.na(netUpdatePos)) netUpdatePos <- NULL
          attUpdatePos <- attUpdateLookup[oid, gid]
          if (is.na(attUpdatePos)) attUpdatePos <- NULL

          effectUpdate <- call_effect_template(
            template, gid, shape, event_args,
            netUpdatePos, attUpdatePos,
            iTotalEvents - iDependentEvents, interval
          )

          if (!is.null(attr(effectUpdate$cache, "lastUpdate"))) {
            attr(statCache[[gid]], "lastUpdate") <- attr(
              effectUpdate$cache,
              "lastUpdate"
            )
          }

          updates <- effectUpdate$changes
          if (!is.null(effectUpdate$cache) && !is.null(effectUpdate$changes)) {
            statCache[[gid]] <- effectUpdate$cache
          }

          if (isUndirectedNet) {
            event_args2 <- event_args
            event_args2$sender <- event_args$receiver
            event_args2$receiver <- event_args$sender
            effectUpdate2 <- call_effect_template(
              template, gid, shape, event_args2,
              netUpdatePos, attUpdatePos,
              iTotalEvents - iDependentEvents, interval
            )
            if (
              !is.null(effectUpdate2$cache) &&
                !is.null(effectUpdate2$changes)
            ) {
              statCache[[gid]] <- effectUpdate2$cache
            }
            updates <- rbind(updates, effectUpdate2$changes)
          }

          if (!is.null(updates)) {
            if (hasStartTime && nextEventTime < startTime) {
              initialStats[cbind(
                updates[, "node1"],
                updates[, "node2"],
                gid
              )] <- updates[, "replace"]
            } else {
              block <- rbind(
                updates[, "node1"] - 1,
                updates[, "node2"] - 1,
                gid - 1,
                updates[, "replace"]
              )
              pending_dep[[length(pending_dep) + 1L]] <- block
              pending_dep_cols <- pending_dep_cols + ncol(block)
              if (right_censored) {
                pending_rc[[length(pending_rc) + 1L]] <- block
                pending_rc_cols <- pending_rc_cols + ncol(block)
              }
            }
          }
        }

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
    }

    if (finalStep) break
  }

  if (progress) {
    utils::setTxtProgressBar(pb, schedule$n)
    close(pb)
  }

  stat_mat_update <- stat_mat_buf[, seq_len(buf_n), drop = FALSE]
  keep <- seq_len(n_stored)
  stat_mat_pointer <- stat_mat_pointer[keep]
  intervals <- intervals[keep]
  is_dependent <- is_dependent[keep]
  event_time <- event_time[keep]
  event_sender <- event_sender[keep]
  event_receiver <- event_receiver[keep]

  n_dep_events <- NULL
  total_time <- NULL
  avg_active_actors <- NULL
  if (intercept_scalars) {
    n_dep_events <- sum(is_dependent == 1L)
    total_time <- sum(intervals)
    nActors <- sum(active_mode1_init)
    if (length(active_mode1_changes) > 0 && n_stored > 0) {
      changesTime <- vapply(
        active_mode1_changes, `[[`, double(1), "time"
      )
      changesReplace <- vapply(
        active_mode1_changes, `[[`, logical(1), "replace"
      )
      timeAcc <- startTime
      previousTime <- -Inf
      activeAcc <- 0
      for (i in seq_len(n_stored)) {
        timeAcc <- timeAcc + intervals[i]
        changesAt <- changesTime > previousTime & changesTime <= timeAcc
        nActors <- nActors +
          sum(changesReplace[changesAt]) - sum(!changesReplace[changesAt])
        activeAcc <- activeAcc + nActors
        previousTime <- timeAcc
      }
      avg_active_actors <- activeAcc / n_stored
    } else {
      avg_active_actors <- nActors
    }
  }

  presence1_update <- NULL
  presence1_update_pointer <- NULL
  presence2_update <- NULL
  presence2_update_pointer <- NULL
  if (length(active_mode1_changes) > 0) {
    compChange1 <- data.frame(
      time = vapply(active_mode1_changes, `[[`, double(1), "time"),
      node = vapply(active_mode1_changes, `[[`, integer(1), "node"),
      replace = vapply(active_mode1_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange1, event_time)
    presence1_update <- temp$presenceUpdate
    presence1_update_pointer <- temp$presenceUpdatePointer
  }
  if (length(active_mode2_changes) > 0) {
    compChange2 <- data.frame(
      time = vapply(active_mode2_changes, `[[`, double(1), "time"),
      node = vapply(active_mode2_changes, `[[`, integer(1), "node"),
      replace = vapply(active_mode2_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange2, event_time)
    presence2_update <- temp$presenceUpdate
    presence2_update_pointer <- temp$presenceUpdatePointer
  }

  structure(
    list(
      initialStats = initialStats,
      stat_mat_update = stat_mat_update,
      stat_mat_pointer = stat_mat_pointer,
      intervals = intervals,
      is_dependent = is_dependent,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      event_pos = seq_len(n_stored),
      active_mode1_init = active_mode1_init,
      active_mode1_changes = active_mode1_changes,
      active_mode2_init = active_mode2_init,
      active_mode2_changes = active_mode2_changes,
      startTime = startTime,
      endTime = endTime,
      n_dep_events = n_dep_events,
      total_time = total_time,
      avg_active_actors = avg_active_actors,
      presence1_update = presence1_update,
      presence1_update_pointer = presence1_update_pointer,
      presence2_update = presence2_update,
      presence2_update_pointer = presence2_update_pointer,
      version = PREPROCESSED_GOLDFISH_VERSION
    ),
    class = "preprocessed.goldfish"
  )
}

#' preprocess event and related objects describe in the formula to estimate
#'
#' Create a preprocess.goldfish class object with the update statistics
#' for estimation.
#'
#' @inheritParams estimate
#' @param events list with all
#' @param effects list of effects functions return by
#'   `createEffectsFunctions()`.
#' @param eventsObjectsLink data.frame output of `getEventsAndObjectsLink()`.
#' @param eventsEffectsLink data.frame output of `getEventsEffectsLink()`.
#' @param objectsEffectsLink data.frame output of `getObjectsEffectsLink()`.
#' @param nodes character with the object that contains the nodes information
#' @param nodes2 character with the object that contains the nodes information,
#'   different from `nodes` when `is_two_mode = TRUE`.
#' @param is_two_mode logical is it a two mode network?
#' @param startTime numerical start time to preprocess the data
#' @param endTime numerical end time to preprocess the data
#' @param rightCensored logical does it consider right censored events?
#' @param progress logical should print progress
#'
#' @return a list of class preprocessed.goldfish
#'
#' @noRd
preprocess_monolith <- function(
  model,
  subModel,
  events,
  effects,
  windowParameters,
  ignoreRepParameter,
  eventsObjectsLink,
  eventsEffectsLink,
  objectsEffectsLink,
  # multipleParameter,
  nodes,
  nodes2 = nodes,
  is_two_mode,
  # add more parameters
  startTime = min(vapply(events, function(x) min(x$time), double(1))),
  endTime = max(vapply(events, function(x) max(x$time), double(1))),
  rightCensored = FALSE,
  opportunitiesList = NULL,
  progress = FALSE,
  prepEnvir = new.env()
) {
  # For debugging
  # if (identical(environment(), globalenv())) {
  #   startTime <- min(vapply(events, function(x) min(x$time), double(1)))
  #   endTime <- max(vapply(events, function(x) max(x$time), double(1)))
  #   progress <- FALSE
  # }

  # print(match.call())
  # initialize statistics functions from data objects
  # number of actors
  n1 <- nrow(get(nodes, envir = prepEnvir))
  n2 <- nrow(get(nodes2, envir = prepEnvir))
  nEffects <- length(effects)

  # check start time and end time are valid values, set flags
  hasEndTime <- FALSE
  hasStartTime <- FALSE
  isValidEvent <- TRUE

  isWindowEffect <- !vapply(windowParameters, is.null, logical(1))
  whichEventNoWindowEffect <- eventsEffectsLink[, !isWindowEffect, drop = FALSE]
  whichEventNoWindowEffect <- rowSums(!is.na(whichEventNoWindowEffect))
  whichEventNoWindowEffect <- c(1, which(whichEventNoWindowEffect > 0))

  hasIgnoreRep <- any(ignoreRepParameter)

  eventsMin <- min(vapply(
    events[whichEventNoWindowEffect],
    function(x) min(x$time),
    double(1)
  ))
  eventsMax <- max(vapply(
    events[whichEventNoWindowEffect],
    function(x) max(x$time),
    double(1)
  ))
  if (is.null(endTime)) {
    endTime <- eventsMax
    if (any(isWindowEffect)) hasEndTime <- TRUE
  } else if (endTime != eventsMax) {
    if (!is.numeric(endTime)) {
      endTime <- as.numeric(endTime)
    }
    if (eventsMin > endTime) {
      stop("End time smaller than first event time.", call. = FALSE)
    }
    # to solve: if endTime > eventsMax
    # should it produce censored events? warning?
    # add a fake event to the event list
    # endTimeEvent <- data.frame(
    #   time = endTime,
    #   sender = NA,
    #   receiver = NA,
    #   replace = NA
    # )
    # events <- c(events, endtime = list(endTimeEvent))
    hasEndTime <- TRUE
  }
  if (is.null(startTime)) {
    startTime <- eventsMin
  } else if (startTime != eventsMin) {
    if (!is.numeric(startTime)) {
      startTime <- as.numeric(startTime)
    }
    if (eventsMax < startTime) {
      stop("Start time geater than last event time.", call. = FALSE)
    }
    hasStartTime <- TRUE
    if (eventsMin < startTime) isValidEvent <- FALSE
    # if (eventsMin > startTime) isValidEvent <- TRUE
    # To solve: if startTime < eventsMin should be a warning?
  }
  ignoreEvents <- 1L # eventPos should be correct for initialization

  # impute missing data in objects: 0 for networks and mean for attributes
  imputed <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)

  if (progress) {
    cat("Initializing cache objects and statistical matrices.\n")
  }

  statCache <- initializeCacheStat(
    objectsEffectsLink = objectsEffectsLink,
    effects = effects,
    groupsNetwork = NULL,
    windowParameters = windowParameters,
    n1 = n1,
    n2 = n2,
    model = model,
    subModel = subModel,
    envir = prepEnvir
  )
  is_rate <- model == "DyNAM" && subModel == "rate"
  if (is_rate) {
    initialStats <- do.call(cbind, lapply(statCache, "[[", "stat"))
  } else {
    initialStats <- array(
      unlist(lapply(statCache, "[[", "stat")),
      dim = c(n1, n2, nEffects)
    )
  }

  statCache <- lapply(statCache, "[[", "cache")

  # UPDATED ALVARO: logical values indicating the type of information in events
  isIncrementEvent <- vapply(
    events,
    function(x) "increment" %in% names(x),
    logical(1)
  )
  isNodeEvent <- vapply(events, function(x) "node" %in% names(x), logical(1))
  isGlobalEvent <- vapply(
    events,
    function(x) !any(c("node", "sender", "receiver") %in% names(x)),
    logical(1)
  )
  isGlobalEvent[1] <- FALSE

  # initialize return objects

  # calculate total of events
  time <- unique(events[[1]]$time)
  if (rightCensored) {
    nRightCensoredEvents <- unique(unlist(lapply(events, function(x) x$time)))
    nTotalEvents <- as.integer(sum(nRightCensoredEvents <= endTime))
    nRightCensoredEvents <- setdiff(nRightCensoredEvents, time)
    if (length(nRightCensoredEvents) > 1) {
      # count right censored events in the preprocessed window
      nRightCensoredEvents <- as.integer(sum(
        nRightCensoredEvents >= startTime &
          nRightCensoredEvents <= endTime
      ))
      # -1 because the last event is the endTime event, correct if no events
      nRightCensoredEvents <- ifelse(
        nRightCensoredEvents > 1,
        nRightCensoredEvents - 1L,
        0L
      )
    } else {
      nRightCensoredEvents <- 0L
    }
  } else {
    nRightCensoredEvents <- 0L
    nTotalEvents <- as.integer(nrow(events[[1]]))
  }

  nDependentEvents <- ifelse(
    hasStartTime || hasEndTime,
    as.integer(sum(time >= startTime & time <= endTime)),
    as.integer(length(time))
  )
  nTotalChangeEvents <- nDependentEvents + nRightCensoredEvents
  stats_change <- vector("list", nTotalChangeEvents)
  intervals <- vector("numeric", nTotalChangeEvents)
  is_dependent <- vector("integer", nTotalChangeEvents)
  event_time <- vector("numeric", nTotalChangeEvents)
  event_sender <- vector("integer", nTotalChangeEvents)
  event_receiver <- vector("integer", nTotalChangeEvents)
  finalStep <- FALSE
  nodes_obj <- get(nodes, envir = prepEnvir)
  nodes2_obj <- get(nodes2, envir = prepEnvir)
  active_mode1_init <- if (!is.null(nodes_obj$present)) {
    nodes_obj$present
  } else {
    rep(TRUE, n1)
  }
  active_mode2_init <- if (!is.null(nodes2_obj$present)) {
    nodes2_obj$present
  } else {
    rep(TRUE, n2)
  }
  comp_events1 <- attr(nodes_obj, "events")[
    attr(nodes_obj, "dynamic_attribute") == "present"
  ]
  comp_events2 <- attr(nodes2_obj, "events")[
    attr(nodes2_obj, "dynamic_attribute") == "present"
  ]
  active_mode1_changes <- if (
    length(comp_events1) > 0 && !is.na(comp_events1[1])
  ) {
    cc <- get(comp_events1[1], envir = prepEnvir)
    node_idx1 <- if (is.character(cc$node)) {
      match(cc$node, nodes_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx1[i], replace = cc$replace[i])
    })
  } else {
    list()
  }
  active_mode2_changes <- if (
    length(comp_events2) > 0 && !is.na(comp_events2[1])
  ) {
    cc <- get(comp_events2[1], envir = prepEnvir)
    node_idx2 <- if (is.character(cc$node)) {
      match(cc$node, nodes2_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx2[i], replace = cc$replace[i])
    })
  } else {
    list()
  }

  # # Remove duplicates of event lists!

  # initialize loop parameters
  # pointers = [1,1,1](events have three elements:
  # callDependent(439*4), calls(439*4), friendship(766*4))
  pointers <- rep(1, length(events))
  validPointers <- rep(TRUE, length(events))
  if (hasEndTime) {
    validPointers <- vapply(events, function(x) x$time[1], double(1)) <= endTime
  }
  pointerTempRightCensored <- 1L
  time <- startTime
  interval <- 0L
  # updatesDependent/updatesIntervals: list of 6, each element if NULL
  updatesDependent <- vector("list", nEffects)
  updatesIntervals <- vector("list", nEffects)

  # initialize progressbar output, CHANGED ALVARO: add iterators

  # iRightCensored <- 0
  iDependentEvents <- 0L
  iTotalEvents <- 0L
  if (progress) {
    cat("Preprocessing events.\n", startTime, endTime, nTotalEvents)
    # # how often print, max 50 prints
    pb <- utils::txtProgressBar(max = nTotalEvents, char = "*", style = 3)
    dotEvents <- ifelse(nTotalEvents > 50, ceiling(nTotalEvents / 50), 1)
  }

  # iterate over all event lists
  while (any(validPointers)) {
    iTotalEvents <- iTotalEvents + 1L
    # times: the timepoint for next events to update in all event lists
    times <- Map(function(e, p) e[p, ]$time, events, pointers) |>
      vapply(identity, numeric(1))
    nextEvent <- which(validPointers)[head(which.min(times[validPointers]), 1)]
    nextEventTime <- times[nextEvent]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && nextEventTime <= endTime) {
        interval <- nextEventTime - time
      } else if (isValidEvent && nextEventTime > endTime) {
        interval <- endTime - time
        nextEventTime <- endTime
        finalStep <- TRUE
      } else if (!isValidEvent && nextEventTime >= startTime) {
        interval <- nextEventTime - startTime
        isValidEvent <- TRUE
      }
    } else {
      interval <- nextEventTime - time
    }

    time <- nextEventTime

    isDependent <- nextEvent == 1 && !finalStep

    if (isValidEvent) {
      eventPos <- pointers[1] + pointerTempRightCensored - ignoreEvents
    } else if (isDependent && !isValidEvent) {
      ignoreEvents <- ignoreEvents + 1L
      eventPos <- 0
    }

    # # CHANGED ALVARO: progress bar
    if (progress && iTotalEvents %% dotEvents == 0) {
      utils::setTxtProgressBar(pb, iTotalEvents)
    }

    if (progress && iTotalEvents == nTotalEvents) {
      utils::setTxtProgressBar(pb, iTotalEvents)
      close(pb)
    }

    # Distinguish three cases
    #   1. Dependent events (store stats)
    #   2. right-censored events (store stats)
    #   3. update change events (including right-censored events of 2.)
    #      calculate statistics updates
    #      update objects

    # 1. store statistic updates for DEPENDENT events
    if (isValidEvent && isDependent) {
      iDependentEvents <- 1L + iDependentEvents
      stats_change[[eventPos]] <- updatesDependent
      intervals[[eventPos]] <- interval
      is_dependent[[eventPos]] <- 1L
      event_time[[eventPos]] <- time
      updatesDependent <- vector("list", nEffects)
      updatesIntervals <- vector("list", nEffects)
      event <- events[[nextEvent]][pointers[nextEvent], ]
      if (isNodeEvent[nextEvent]) {
        event_sender[[eventPos]] <- event$node
        event_receiver[[eventPos]] <- event$node
      } else {
        event_sender[[eventPos]] <- event$sender
        event_receiver[[eventPos]] <- event$receiver
      }
    } else if (!isDependent) {
      if (isValidEvent && rightCensored && interval > 0) {
        stats_change[[eventPos]] <- updatesIntervals
        intervals[[eventPos]] <- interval
        is_dependent[[eventPos]] <- 0L
        event_time[[eventPos]] <- time
        rc_event <- events[[nextEvent]][pointers[nextEvent], ]
        if (isGlobalEvent[nextEvent]) {
          event_sender[[eventPos]] <- NA_integer_
          event_receiver[[eventPos]] <- NA_integer_
        } else if (isNodeEvent[nextEvent] && length(rc_event) == 1) {
          event_sender[[eventPos]] <- rc_event
          event_receiver[[eventPos]] <- rc_event
        } else if (isNodeEvent[nextEvent]) {
          event_sender[[eventPos]] <- rc_event$node
          event_receiver[[eventPos]] <- rc_event$node
        } else {
          event_sender[[eventPos]] <- rc_event$sender
          event_receiver[[eventPos]] <- rc_event$receiver
        }
        updatesIntervals <- vector("list", nEffects)
        pointerTempRightCensored <- pointerTempRightCensored + 1
      } # else if (isValidEvent && !finalStep && interval > 0) {
      #   timeIntervals[[iDependentEvents + 1]] <- interval +
      #     timeIntervals[[iDependentEvents + 1]]
      # }

      # 3. update stats and data objects for OBJECT CHANGE EVENTS
      # (all non-dependent events)

      # Two steps are performed for non-dependent events
      #   (0. get objects and update increment columns)
      #   a. Calculate statistic updates for each event that relates
      #     to the data update
      #   b. Update the data objects

      objectNameTable <- eventsObjectsLink[nextEvent, -1]
      objectName <- objectNameTable$name
      object <- getElementFromDataObjectTable(
        objectNameTable,
        envir = prepEnvir
      )[[1]]
      isUndirectedNet <- FALSE
      if (inherits(object, "network.goldfish")) {
        isUndirectedNet <- !attr(object, "directed")
      }

      # # CHANGED ALVARO: avoid dependence in variables position
      if (isGlobalEvent[nextEvent]) {
        event <- events[[nextEvent]][
          pointers[nextEvent],
          "replace",
          drop = FALSE
        ]
        if (is.na(event$replace)) event$replace <- 0
      } else if (isIncrementEvent[nextEvent]) {
        varsKeep <- c(
          if (isNodeEvent[nextEvent]) "node" else c("sender", "receiver"),
          "increment"
        )
        event <- events[[nextEvent]][pointers[nextEvent], varsKeep]
        # missing data imputation
        if (isNodeEvent[nextEvent]) {
          oldValue <- object[event$node]
          # if the replace is missing impute by 0 because is increment
          if (is.na(event$increment)) event$increment <- 0
        }
        if (!isNodeEvent[nextEvent]) {
          oldValue <- object[event$sender, event$receiver]
          # if the replace is missing impute by 0 (not-tie)
          if (is.na(event$increment)) event$increment <- 0
        }
        event$replace <- oldValue + event$increment
        event$increment <- NULL
      } else {
        varsKeep <- c(
          if (isNodeEvent[nextEvent]) "node" else c("sender", "receiver"),
          "replace"
        )
        event <- events[[nextEvent]][pointers[nextEvent], varsKeep]
        # missing data imputation
        if (isNodeEvent[nextEvent] && is.na(event$replace)) {
          # impute by the mean of current values for attributes
          event$replace <- mean(object[-event$node], na.rm = TRUE)
        }
        if (!isNodeEvent[nextEvent] && is.na(event$replace)) {
          # if the replace is missing impute by 0 (not-tie)
          event$replace <- 0
        }
      }

      # network update an negative replacement throws a warning
      if (
        !isNodeEvent[nextEvent] &&
          !isGlobalEvent[nextEvent] &&
          event$replace < 0
      ) {
        warning("You are dissolving a tie which doesn't exist!", call. = FALSE)
      }

      ## 3a. calculate statistics changes
      if (!finalStep) {
        for (id in which(!is.na(eventsEffectsLink[nextEvent, ]))) {
          # create the ordered list for the objects
          objectsToPass <- objectsEffectsLink[, id][
            !is.na(objectsEffectsLink[, id])
          ]
          names <- rownames(objectsEffectsLink)
          names <- names[!is.na(objectsEffectsLink[, id])]
          orderedNames <- names[order(objectsToPass)]
          orderedObjectTable <- getDataObjects(list(list("", orderedNames)))
          .objects <- getElementFromDataObjectTable(
            orderedObjectTable,
            envir = prepEnvir
          )
          # identify class to feed effects functions
          objCat <- assign_category_object(.objects)
          attIDs <- which(objCat == "attribute")
          netIDs <- which(objCat == "network")
          if (attr(objCat, "none_class")) {
            stop(
              "An object is not assigned either as network or attibute",
              paste(names[attr(objCat, "manyClasses") != 1], collapse = ", "),
              "check the class of the object.",
              call. = FALSE
            )
          }

          # call effects function with required arguments
          .argsFUN <- list(
            network = if (length(.objects[netIDs]) == 1) {
              .objects[netIDs][[1]]
            } else {
              .objects[netIDs]
            },
            attribute = if (length(.objects[attIDs]) == 1) {
              .objects[attIDs][[1]]
            } else {
              .objects[attIDs]
            },
            cache = statCache[[id]],
            n1 = n1,
            n2 = n2,
            netUpdate = if (length(.objects[netIDs]) <= 1) {
              NULL
            } else {
              which(orderedNames == objectName)
            },
            attUpdate = if (length(.objects[attIDs]) <= 1) {
              NULL
            } else {
              which(orderedNames == objectName)
            },
            # add more parameters:
            # - consecutive updates in closure effects
            # - interEventTime (since last event right-censored included):
            #   exponentially weighted decay effects
            eventOrder = iTotalEvents - iDependentEvents,
            interEventTime = interval
          )
          effectUpdate <- callFUN(
            effects,
            id,
            "effect",
            c(.argsFUN, event),
            " cannot update \n",
            colnames(objectsEffectsLink)[id]
          )

          # CHANGED - MABEL - need to update cache attributes for lastUpdate when
          # trans or cycle and history = "consecutive"
          if (!is.null(attr(effectUpdate$cache, 'lastUpdate'))) {
            attr(statCache[[id]], "lastUpdate") <- attr(
              effectUpdate$cache,
              'lastUpdate'
            )
          }

          updates <- effectUpdate$changes
          # if cache and changes are not null update cache
          if (!is.null(effectUpdate$cache) && !is.null(effectUpdate$changes)) {
            statCache[[id]] <- effectUpdate$cache
          }

          if (isUndirectedNet) {
            event2 <- event
            event2$sender <- event$receiver
            event2$receiver <- event$sender
            if (
              !is.null(effectUpdate$cache) &&
                !is.null(effectUpdate$changes)
            ) {
              # styler: off
              .argsFUN$cache <- statCache[[id]]
            }
            effectUpdate2 <- callFUN(
              effects,
              id,
              "effect",
              c(.argsFUN, event2),
              " cannot update \n",
              colnames(objectsEffectsLink)[id]
            )

            if (
              !is.null(effectUpdate2$cache) &&
                !is.null(effectUpdate2$changes)
            ) {
              # styler: off
              statCache[[id]] <- effectUpdate2$cache
            }
            updates2 <- effectUpdate2$changes
            updates <- rbind(updates, updates2)
          }

          if (!is.null(updates)) {
            if (hasStartTime && nextEventTime < startTime) {
              if (is_rate) {
                initialStats[cbind(updates[, "node1"], id)] <- updates[,
                  "replace"
                ]
              } else {
                initialStats[cbind(
                  updates[, "node1"],
                  updates[, "node2"],
                  id
                )] <-
                  updates[, "replace"]
              }
            } else {
              # CHANGED WEIGUTIAN: UPDATE THE STAT MAT
              # AND IMPUTE THE MISSING VALUES
              # if (anyNA(statCache[[id]][["stat"]])) {
              #   position_NA <- which(
              #     is.na(statCache[[id]][["stat"]]),
              #     arr.ind  = TRUE
              #   )
              #   average <- mean(statCache[[id]][["stat"]], na.rm = TRUE)
              #   updates[is.na(updates[, "replace"]), "replace"] <- average
              #   statCache[[id]][["stat"]][position_NA] <- average
              # }

              updatesDependent[[id]] <- rbind(updatesDependent[[id]], updates)
              updatesIntervals[[id]] <- rbind(updatesIntervals[[id]], updates)
            }
          }
        }
      }

      # 3b. Update the data object
      if (!finalStep) {
        if (isGlobalEvent[nextEvent]) {
          object <- event$replace
        } else if (!is.null(event$node)) {
          object[event$node] <- event$replace
        } else if (!is.null(event$sender)) {
          # [sender, receiver] value: replace value of the event
          object[event$sender, event$receiver] <- event$replace
          if (isUndirectedNet) {
            object[event$receiver, event$sender] <- event$replace
          }
        }
        # Assign object
        assign("object", object, envir = prepEnvir)
        eval(
          parse(text = paste(objectName, "<- object")),
          envir = prepEnvir,
          enclos = parent.frame()
        )
      }
    } # end 3. (!dependent)

    # update events pointers
    pointers[nextEvent] <- 1 + pointers[nextEvent]
    validPointers <- pointers <= vapply(events, nrow, integer(1)) &
      times <= endTime
  }

  if (progress && utils::getTxtProgressBar(pb) < nTotalEvents) {
    close(pb)
  }

  return(structure(
    list(
      initialStats = initialStats,
      stats_change = stats_change,
      intervals = intervals,
      is_dependent = is_dependent,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      event_pos = seq_len(length(stats_change)),
      active_mode1_init = active_mode1_init,
      active_mode1_changes = active_mode1_changes,
      active_mode2_init = active_mode2_init,
      active_mode2_changes = active_mode2_changes,
      startTime = startTime,
      endTime = endTime
    ),
    class = "preprocessed.goldfish"
  ))
}

#' initialize the cache object or the stat matrices
#'
#' @param objectsEffectsLink data.frame output of `getObjectsEffectsLink()`
#' @param effects list of effects functions return by `createEffectsFunctions()`
#' @param groupsNetwork matrix that defines groups partition in DyNAMi
#' @param windowParameters NULL or numeric value with the size of the window
#' @param n1 int `nrow(network)`
#' @param n2 int `ncol(network)`
#' @param model character
#' @param subModel character
#' @param envir environment where get the objects
#'
#' @return a list of size length(effects):
#'   list with initial cache object and stat matrices
#'
#' @noRd
initializeCacheStat <- function(
  objectsEffectsLink,
  effects,
  groupsNetwork,
  windowParameters,
  n1,
  n2,
  model,
  subModel,
  envir = environment()
) {
  objTable <- getDataObjects(
    list(rownames(objectsEffectsLink)),
    removeFirst = FALSE
  )
  .objects <- getElementFromDataObjectTable(objTable, envir = envir)
  # list of 4, call matrix, friendship matrix, actor$gradetype vector,
  #  actor$floor vector
  objCat <- assign_category_object(.objects)
  if (attr(objCat, "none_class")) {
    stop(
      "An object is not assigned either as network or attibute",
      paste(
        rownames(objectsEffectsLink)[attr(objCat, "manyClasses") != 1],
        collapse = ", "
      ),
      "check the class of the object.",
      call. = FALSE
    )
  }

  # objects: list of 6, each element is a 84*84 matrix
  objectsRet <- lapply(
    seq_along(effects),
    function(iEff) {
      o <- objectsEffectsLink[, iEff]
      attIDs <- which(!is.na(o) & objCat == "attribute")
      netIDs <- which(!is.na(o) & objCat == "network")
      attributes <- .objects[attIDs[order(o[attIDs])]]
      networks <- .objects[netIDs[order(o[netIDs])]]
      labelEffect <- colnames(objectsEffectsLink)[iEff]
      objectsNames <- paste(
        rownames(na.omit(objectsEffectsLink[, iEff, drop = FALSE])),
        collapse = ", "
      )
      messageEffect <- paste0(
        " cannot initialized with objects ",
        objectsNames,
        "\n"
      )
      # init
      .argsFUN <- list(
        effectFun = effects[[iEff]][["effect"]],
        network = if (length(networks) == 1) networks[[1]] else networks,
        attribute = if (length(attributes) == 1) {
          attributes[[1]]
        } else {
          attributes
        },
        groupsNetwork = groupsNetwork,
        window = windowParameters[[iEff]],
        n1 = n1,
        n2 = n2
      )
      callFUN(
        effects,
        iEff,
        "initEffect",
        .argsFUN,
        messageEffect,
        labelEffect
      )
    }
  )
}


#' call fun
#' Call a function with a variable set of arguments (effects functions)
#' @param effects list of effects functions return by `createEffectsFunctions()`
#' @param effectPos int indicates the effect to be used.
#' @param effectType character indicates which effect function use.
#'   Available values
#'   `c("initCache", "updateCache", "initStat", "effect")`
#' @param .argsFUN named list with the arguments to feed FUN.
#' @param error function that returns the error mesage if any.
#' @param warning function that returns the warning mesage if any.
#' @param effectLabel character use by the error or warning function to give
#'   an additional information to the user.
#'
#' @return the output of call `effects[[effectPos]][[effectType]]`
#'   with arguments `.argsFUN` in the case if not errors.
#' @noRd
#' @examples
#' \donttest{
#' .argsFUN <- list(
#'   network = m,
#'   n1 = 5, n2 = 5,
#'   sender = 1, receiver = 5, replace = 0
#' )
#' effects <- list(list(effect = out))
#'
#' ver2 <- callFUN(
#'   effects = effects, effectPos = effectPos, effectType = "effect",
#'   .argsFUN = .argsFUN, textMss = " ver ",
#'   effectLabel = "out"
#' )
#'
#'
#' .argsFUN <- list(network = m, n1 = 5, n2 = 5, sender = 1, receiver = 5)
#' effects <- list(list(effect = out))
#'
#' ver2 <- callFUN(
#'   effects = effects, effectPos = effectPos, effectType = "effect",
#'   .argsFUN = .argsFUN, textMss = " ver ",
#'   effectLabel = "out"
#' )
#' }
callFUN <- function(
  effects,
  effectPos,
  effectType,
  .argsFUN,
  textMss,
  effectLabel
) {
  err <- NULL
  warn <- NULL
  .argsNames <- formals(effects[[effectPos]][[effectType]])
  .argsKeep <- pmatch(names(.argsNames), names(.argsFUN))
  # check for more than one net
  errorHandler <- function(e) {
    erro <- simpleError(
      paste0(
        "Effect ",
        dQuote(effectLabel),
        " (",
        effectPos,
        ") ",
        textMss,
        e$message
      )
    )
    stop(erro)
  }
  tryCatch(
    {
      withCallingHandlers(
        {
          callRes <- do.call(
            effects[[effectPos]][[effectType]],
            .argsFUN[na.omit(.argsKeep)]
          )
        },
        error = identity,
        warning = function(w) {
          warn <<- w
          invokeRestart("muffleWarning")
        }
      )
    },
    error = errorHandler
  )
  if (!is.null(warn)) {
    warning(warn)
  }
  return(callRes)
}


#' Impute missing values
#' If network missing values are replace by zero,
#' if attributes missing values are impute by the mean.
#' @param objectsEffectsLink matrix. Rows objects, columns effects, 1 or NA.
#'  `getObjectsEffectsLink(rhsNames)` output.
#' @param envir evaluation enviroment to get and assign impute objects.
#'
#' @return a vector of `nrow(objectsEffectsLink)` length with
#'   logical value signaling imputation of missing values.
#' @noRd
#'
#' @examples
#' \donttest{
#' actorsEx <- data.frame(
#'   label = sprintf("Actor %d", 1:5),
#'   present = rep(TRUE, 5),
#'   attr1 = c(9.9, NA, 0.5, 0.45, 0.25),
#'   stringsAsFactors = FALSE
#' )
#'
#' networkAlgo <- matrix(
#'   c(
#'     0, 3, 0, 0, 0,
#'     1, 0, 1, 1, 0,
#'     0, 0, NA, 1, 0,
#'     0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE,
#'   dimnames = list(
#'     sprintf("Actor %d", 1:5),
#'     sprintf("Actor %d", 1:5)
#'   )
#' )
#'
#' objectsEffectsLink <- matrix(
#'   c(1, NA, NA, 1),
#'   nrow = 2, ncol = 2,
#'   dimnames = list(
#'     c("networkAlgo", "actorsEx$attr1"),
#'     c("inertia", "alter")
#'   )
#' )
#' prepEnvir <- environment()
#'
#' check <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)
#' }
imputeMissingData <- function(objectsEffectsLink, envir = new.env()) {
  # get data object table, row objects columns class (matrix, attribute)
  objTable <- getDataObjects(
    list(rownames(objectsEffectsLink)),
    removeFirst = FALSE
  )
  # print(objTable)
  done <- structure(vector("logical", nrow(objTable)), names = objTable$name)
  for (iEff in seq_len(nrow(objTable))) {
    objectNameTable <- objTable[iEff, ]
    object <- getElementFromDataObjectTable(objectNameTable, envir = envir)[[1]]
    objectName <- objectNameTable$name
    # print(table(is.na(object)))
    # cat(objectName, "\n")
    if (is.matrix(object) && any(is.na(object))) {
      object[is.na(object)] <- 0
      done[iEff] <- TRUE
      # cat("matrix\n")
      # Assign object
      assign(objectName, object, envir = envir)
    } else if (is.vector(object) && any(is.na(object))) {
      if (is.numeric(object)) {
        cli::cli_warn(c(
          "i" = "Missing data has been detected. Mean is used to impute for numerical values"
        ))
        object[is.na(object)] <- mean(object, na.rm = TRUE)
      } else {
        cli::cli_warn(c(
          "i" = "Missing data has been detected. Mode is used to impute for categorical values"
        ))
        object[is.na(object)] <- names(which.max(table(object)))
      }
      done[iEff] <- TRUE
      # cat("vector\n")
      # Assign object
      assign("object", object, envir = envir)
      eval(
        parse(text = paste(objectName, "<- object")),
        envir = envir,
        enclos = parent.frame()
      )
    }
  }
  return(done)
}

###################### ##
#
# Goldfish package
# Preprocessing for DyNAM-i
#
###################### ##

#' Create a preprocess.goldfish class object with the update statistics
#' for estimation of a DyNAM-i model.
#'
#' @inheritParams preprocess
#' @param groups_network a character with the object that contains the
#' groups network information
#'
#' @return a list of class preprocessed.goldfish
#'
#' @noRd
preprocess_interaction <- function(
  sub_model,
  events,
  effects,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  # multiple_parameter,
  nodes,
  nodes2 = nodes,
  # add more parameters
  startTime = min(vapply(events, function(x) min(x$time), double(1))),
  endTime = max(vapply(events, function(x) max(x$time), double(1))),
  right_censored = FALSE,
  progress = FALSE,
  groups_network = groups_network,
  prep_envir = environment()
) {
  # For debugging
  # if (identical(environment(), globalenv())) {
  #   startTime <- min(vapply(events, function(x) min(x$time), double(1)))
  #   endTime <- max(vapply(events, function(x) max(x$time), double(1)))
  #   progress <- FALSE
  # }

  # prep_envir <- environment()
  # initialize statistics functions from data objects
  # number of actors
  n1 <- nrow(get(nodes, envir = prep_envir))
  n2 <- nrow(get(nodes2, envir = prep_envir))
  nEffects <- length(effects)
  # changed Marion
  groupsNetworkObject <- get(groups_network, envir = prep_envir)
  # impute missing data in objects: 0 for networks and mean for attributes
  imputed <- impute_missing_data(objects_effects_link, envir = prep_envir)

  if (progress) {
    cat("Initializing cache objects and statistical matrices.\n")
  }
  model <- "DyNAMi"
  stats <- initialize_cache_stat(
    objects_effects_link = objects_effects_link,
    effects = effects,
    groups_network = groupsNetworkObject,
    window_parameters = NULL,
    n1 = n1,
    n2 = n2,
    model = model,
    sub_model = sub_model,
    envir = prep_envir
  )

  # We put the initial stats to the previous format of 3 dimensional array
  initial_stats <- array(unlist(stats), dim = c(n1, n2, nEffects))

  # stat_cache <- lapply(stat_cache, "[[", "cache")

  # initialize return objects
  # CHANGED MARION: for choice model, only joining events
  if (right_censored) {
    n_dependent_events <-
      length(unique(unlist(lapply(events, function(x) x$time))))
  } else {
    n_dependent_events <- sum(events[[1]]$increment == -1)
  }
  dependentStatistics <- list()
  rightCensoredStatistics <- list()
  time_intervals <- list()
  timeIntervalsRightCensored <- list()
  # CHANGED MARION: added a list that tracks the chronological(ordered by time)
  #  order of events
  # between dependent and right-censored events
  # 1 is for dependent and 2 if for right-censored
  order_events <- list()
  event_time <- list()
  event_sender <- list()
  event_receiver <- list()

  # check start time and end time are valid values, set flags
  hasEndTime <- FALSE
  events_min <- min(vapply(events, function(x) min(x$time), double(1)))
  events_max <- max(vapply(events, function(x) max(x$time), double(1)))
  if (!is.null(endTime) && endTime != events_max) {
    stop(
      dQuote("DyNAMi"),
      " doesn't support setting the ",
      dQuote("endTime"),
      "parameter",
      call. = FALSE
    )
  }

  if (!is.null(startTime) && startTime != events_min) {
    stop(
      dQuote("DyNAMi"),
      " doesn't support setting the ",
      dQuote("StartTime"),
      "parameter",
      call. = FALSE
    )
  }

  # initialize loop parameters
  events[[1]] <- NULL
  pointers <- rep(1, length(events))
  valid_pointers <- rep(TRUE, length(events))
  pointerDependent <- 1
  pointer_temp_right_censored <- 1
  time <- startTime
  interval <- 0
  # updates_dependent/updates_intervals: list of 6, each element if NULL
  updates_dependent <- vector("list", nEffects)
  updates_intervals <- vector("list", nEffects)

  # added Marion: find index of the dependent, exogenous events on the groups
  # and of the past interaction updates
  dname <- events_objects_link[1, 1]
  # PATCH Marion: the depdendent.depevents_DyNAMi is not sanitized yet
  dnameObject <- sanitizeEvents(
    get(dname, envir = prep_envir),
    nodes,
    nodes2,
    envir = prep_envir
  )
  assign(dname, dnameObject, envir = prep_envir)

  depindex <- 0
  deporder <- NULL
  exoindex <- 0
  exoorder <- NULL
  pastindexes <- numeric()
  pastorders <- list()
  numpast <- 0
  if (length(events) > 0) {
    for (e in seq.int(length(events))) {
      ev <- events[[e]]
      if (
        inherits(ev, "goldfishInterGrp") &&
          all(get(dname, envir = prep_envir) == ev)
      ) {
        depindex <- e
        deporder <- attr(ev, "order")
      } else if (
        inherits(ev, "goldfishInterGrp") &&
          !all(get(dname, envir = prep_envir) == ev)
      ) {
        exoindex <- e
        exoorder <- attr(ev, "order")
      } else if (
        inherits(ev, "goldfishInterNet") &&
          !is.null(attr(ev, "order"))
      ) {
        numpast <- numpast + 1
        pastindexes[numpast] <- e
        pastorders[[numpast]] <- attr(ev, "order")
      }
    }
  }

  # If depindex and exoindex not there
  # (because there was no effect with the default network)
  # we need to find them anyway!
  if (depindex == 0) {
    # find groups udates and add them to events
    groupsupdates <- attr(groupsNetworkObject, "events")

    # PATCH Marion: the groups update events were not sanitized
    groupsupdates1Object <- sanitizeEvents(
      get(groupsupdates[1], envir = prep_envir),
      nodes,
      nodes2,
      envir = prep_envir
    )
    assign(groupsupdates[1], groupsupdates1Object, envir = prep_envir)
    groupsupdates2Object <- sanitizeEvents(
      get(groupsupdates[2], envir = prep_envir),
      nodes,
      nodes2,
      envir = prep_envir
    )
    assign(groupsupdates[2], groupsupdates2Object, envir = prep_envir)

    if (
      all(
        get(dname, envir = prep_envir) ==
          get(groupsupdates[1], envir = prep_envir)
      )
    ) {
      depn <- groupsupdates[1]
      exon <- groupsupdates[2]
    } else {
      depn <- groupsupdates[2]
      exon <- groupsupdates[1]
    }
    depindex <- length(events) + 1
    exoindex <- length(events) + 2
    events[[depindex]] <- get(depn, envir = prep_envir)
    events[[exoindex]] <- get(exon, envir = prep_envir)

    # find orders
    deporder <- attr(events[[depindex]], "order")
    exoorder <- attr(events[[exoindex]], "order")

    # sanitize events
    nodesObject <- attr(groupsNetworkObject, "nodes")

    if (length(nodesObject) > 1) {
      nodes <- nodesObject[1]
      nodes2 <- nodesObject[2]
    } else {
      nodes <- nodes2 <- nodesObject
    }
    events[[depindex]] <- sanitizeEvents(
      events[[depindex]],
      nodes,
      nodes2,
      envir = prep_envir
    )
    events[[exoindex]] <- sanitizeEvents(
      events[[exoindex]],
      nodes,
      nodes2,
      envir = prep_envir
    )

    # augment the link objects
    events_objects_link <- rbind(
      events_objects_link,
      c(depn, groups_network, groups_network, NA, NA),
      c(exon, groups_network, groups_network, NA, NA)
    )

    events_effects_link <- rbind(
      events_effects_link,
      rep(NA, dim(events_effects_link)[2]),
      rep(NA, dim(events_effects_link)[2])
    )
    rownames(events_effects_link)[dim(events_effects_link)[1] - 1] <- depn
    rownames(events_effects_link)[dim(events_effects_link)[1]] <- exon

    objects_effects_link <- rbind(
      objects_effects_link,
      rep(NA, dim(objects_effects_link)[2])
    )
    rownames(objects_effects_link)[dim(objects_effects_link)[
      1
    ]] <- groups_network

    # reset the pointers for ALL events
    pointers <- rep(1, length(events))
    valid_pointers <- rep(TRUE, length(events))
  }

  # Set the counter for the ordered events
  cptorder <- 0

  # added Marion: updates of statistics
  updFun <- function(stat, change) {
    if (!is.null(change)) {
      if ("node2" %in% colnames(change)) {
        stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      } else {
        for (k in seq_len(nrow(change))) {
          stat[change[k, "node1"], ] <- change[k, "replace"]
        }
      }
    }
    return(stat)
  }

  # initialize progressbar output, CHANGED ALVARO: add iterators

  # i_right_censored <- 0
  i_dependent_events <- 0
  if (progress) {
    cat("Preprocessing events.\n")
    pb <- utils::txtProgressBar(max = n_dependent_events, char = "*", style = 3)
    dot_events <- ifelse(
      n_dependent_events > 50,
      ceiling(n_dependent_events / 50),
      1
    ) # # how often print, max 50 prints
  }

  # UPDATED ALVARO: logical values indicating the type of information in events
  is_increment_event <- vapply(
    events,
    function(x) "increment" %in% names(x),
    logical(1)
  )
  is_node_event <- vapply(events, function(x) "node" %in% names(x), logical(1))

  # iterate over all event lists
  while (any(valid_pointers)) {
    # times: the timepoint for next events to update in all event lists
    times <- Map(function(e, p) e[p, ]$time, events, pointers) |>
      vapply(identity, numeric(1))

    # added Marion: we set priority to dependent,
    # exogenous and past updates before anything else
    # and between those 3 (or the 2 first if the 3rd is not needed),
    # the order is decided
    # note: when it's a windowed past update, the value of the cpt is 0
    mintime <- min(times, na.rm = TRUE)
    currentpointers <- which(valid_pointers & times == mintime)
    prioritypointers <- intersect(
      currentpointers,
      c(depindex, exoindex, pastindexes)
    )
    if (length(prioritypointers) > 0) {
      cpts <- Map(
        \(p) {
          if (p == depindex) {
            return(deporder[pointers[p]])
          }
          if (p == exoindex) {
            return(exoorder[pointers[p]])
          }
          if (p %in% pastindexes) {
            return(pastorders[[which(pastindexes == p)]][pointers[p]])
          }
        },
        prioritypointers
      ) |>
        vapply(identity, numeric(1))

      if (max(cpts) == 0) {
        next_event <- prioritypointers[1]
      } else {
        nextcpt <- min(cpts[cpts > cptorder])
        cptorder <- nextcpt
        if (cptorder %in% deporder) {
          next_event <- depindex
        }
        if (cptorder %in% exoorder) {
          next_event <- exoindex
        }
        if (length(pastorders) > 0 && cptorder %in% pastorders[[1]]) {
          cptindexes <- prioritypointers[cpts == nextcpt]
          next_event <- cptindexes[1]
          if (length(cptindexes) > 1) cptorder <- cptorder - 1
        }
      }
    } else {
      # otherwise we take the first next event
      next_event <- currentpointers[1]
    }
    interval <- times[next_event] - time
    time <- min(times[valid_pointers])

    # changed Marion: for choice, only joining events are dependent events
    isDependent <- (sub_model == "rate" && next_event == depindex) ||
      (sub_model == "choice" &&
        next_event == depindex &&
        events[[depindex]][pointers[next_event], "increment"] > 0)

    # # CHANGED ALVARO: progress bar
    if (progress && i_dependent_events %% dot_events == 0) {
      utils::setTxtProgressBar(pb, i_dependent_events)
    }

    if (progress && i_dependent_events == n_dependent_events) {
      utils::setTxtProgressBar(pb, i_dependent_events)
      close(pb)
    }

    # Distinguish three cases
    #   1. Dependent events (store stats)
    #   2. right-censored events (store stats)
    #   3. update change events (including right-censored events of 2.)
    #      calculate statistics updates
    #      update objects

    # 1. store statistic updates for DEPENDENT events
    if (isDependent) {
      # first store statistics
      i_dependent_events <- 1 + i_dependent_events
      dependentStatistics[[i_dependent_events]] <- updates_dependent
      time_intervals[[i_dependent_events]] <- interval
      updates_dependent <- vector("list", nEffects)
      updates_intervals <- vector("list", nEffects)
      # CHANGED MARION: added order_events
      order_events[[(pointerDependent + pointer_temp_right_censored - 1)]] <- 1
      # CHANGDE SIWEI: added time point of each event
      # (dependent & right-censorde)
      event_time[[(pointerDependent + pointer_temp_right_censored - 1)]] <- time
      # CHANGED MARION: added sender and receiver
      vars_keep <- c(
        if (is_node_event[next_event]) "node" else c("sender", "receiver"),
        "increment"
      )
      event <- events[[next_event]][pointers[next_event], vars_keep]
      event_sender[[(pointerDependent + pointer_temp_right_censored - 1)]] <-
        event$sender
      event_receiver[[(pointerDependent + pointer_temp_right_censored - 1)]] <-
        event$receiver

      # second update the network (no need to calculate the stats there
      #  because they will be updated
      # with the following exogenous event of leaving the previous group or
      #  joining an isolate)

      ## FOR TESTING: SEE AVAILABLE GROUPS>2 FOR EACH EVENT (TO COUNT THE
      ##  PROPORTION OF GROUPS IN THE EVENTS)
      # if (max(colSums(groups.network.object)) > 2) {
      #   print(paste("event", i_dependent_events))
      #   grinds <- which(colSums(groups.network.object) > 2)
      #   for (grind in 1:length(grinds)) {
      #     print(
      #       paste("group present with actors: ",
      #         which(groups.network.object[, grinds[grind]]==1)))
      #   }
      #   if (event$increment == -1 && event$receiver %in% grinds) {
      #     print("this is a group leaving event!")
      #   }
      # }

      groupsNetworkObject[event$sender, event$receiver] <-
        groupsNetworkObject[event$sender, event$receiver] + event$increment
      assign(groups_network, groupsNetworkObject, envir = prep_envir)

      pointerDependent <- pointerDependent + 1
    }

    if (!isDependent) {
      # 2. store statistic updates for RIGHT-CENSORED
      # (non-dependent, positive) intervals
      if (right_censored && interval > 0) {
        # CHANGED MARION: the incremented index was incorrect
        # rightCensoredStatistics[[ pointers[next_event] ]] <- updates_intervals
        # timeIntervalsRightCensored[[length(rightCensoredStatistics)]] <-
        #  interval
        rightCensoredStatistics <- append(
          rightCensoredStatistics,
          list(updates_intervals)
        )
        timeIntervalsRightCensored <- append(
          timeIntervalsRightCensored,
          interval
        )
        updates_intervals <- vector("list", nEffects)
        # CHANGED MARION: added order_events
        nextPointer <- (pointers[depindex] + pointer_temp_right_censored - 1)
        order_events[[nextPointer]] <- 2
        event_time[[nextPointer]] <- time
        # CHANGED MARION: added sender and receiver
        # CHANGED WEIGUTIAN: removed "increment" which results a bug
        event <- events[[next_event]][pointers[next_event], ]
        if (is_node_event[next_event] && length(event) == 1) {
          event_sender[[nextPointer]] <- event
          event_receiver[[nextPointer]] <- event
        } else if (is_node_event[next_event] && length(event) > 1) {
          event_sender[[nextPointer]] <- event$node
          event_receiver[[nextPointer]] <- event$node
        } else {
          event_sender[[nextPointer]] <- event$sender
          event_receiver[[nextPointer]] <- event$receiver
        }
        pointer_temp_right_censored <- pointer_temp_right_censored + 1
      }

      # 3. update stats and data objects for OBJECT CHANGE EVENTS
      # (all non-dependent events)

      # Two steps are performed for non-dependent events
      #   (0. get objects and update increment columns)
      #   a. Calculate statistic updates for each event that relates
      #    to the data update
      #   b. Update the data objects

      object_name_table <- events_objects_link[next_event + 1, -1]
      object_name <- object_name_table$name
      object <- get_element_from_data_object_table(
        object_name_table,
        envir = prep_envir
      )[[1]]
      is_undirected_net <- FALSE
      if (inherits(object, "network.goldfish")) {
        is_undirected_net <- !attr(object, "directed")
      }

      # # CHANGED ALVARO: avoid dependence in variables position
      if (is_increment_event[next_event]) {
        vars_keep <- c(
          if (is_node_event[next_event]) "node" else c("sender", "receiver"),
          "increment"
        )
        event <- events[[next_event]][pointers[next_event], vars_keep]

        if (is_node_event[next_event]) {
          old_value <- object[event$node]
        }
        if (!is_node_event[next_event]) {
          old_value <- object[event$sender, event$receiver]
        }
        event$replace <- old_value + event$increment
        event$increment <- NULL
      } else {
        vars_keep <- c(
          if (is_node_event[next_event]) "node" else c("sender", "receiver"),
          "replace"
        )
        event <- events[[next_event]][pointers[next_event], vars_keep]
      }

      # if (!is_node_event[next_event] && event$replace < 0) {
      #  warning("You are dissolving a tie which doesn't exist!", call. = FALSE)
      # }

      # b. Update the data object
      if (is_node_event[next_event]) {
        object[event$node] <- event$replace
      }
      if (!is_node_event[next_event]) {
        # [sender, receiver] value: replace value of the event
        object[event$sender, event$receiver] <- event$replace
        if (is_undirected_net) {
          object[event$receiver, event$sender] <- event$replace
        }
      }

      # Assign object
      assign("object", object, envir = prep_envir)
      eval(
        parse(text = paste(object_name, "<- object")),
        envir = prep_envir,
        enclos = parent.frame()
      )

      # added Marion: for interaction model, check whether this is
      # an exogenous event or past update
      isinteractionupdate <- inherits(
        events[[next_event]],
        "goldfishInterNet"
      )
      isgroupupdate <- inherits(
        events[[next_event]],
        "goldfishInterGrp"
      )

      # a. calculate statistics changes:
      # if EXOGENOUS JOINING OR LEAVING, everything is recalculated
      if (isgroupupdate) {
        effIds <- seq.int(dim(events_effects_link)[2])
      } else {
        # OTHERWISE (PAST UPDATE or ATTRIBUTE UPDATE),
        # only statistics related to the object
        effIds <- which(!is.na(events_effects_link[next_event + 1, ]))
      }
      groupsNetworkObject <- get(groups_network, envir = prep_envir)

      for (id in effIds) {
        # create the ordered list for the objects
        objects_to_pass <-
          objects_effects_link[, id][!is.na(objects_effects_link[, id])]
        names <- rownames(objects_effects_link)[
          !is.na(objects_effects_link[, id])
        ]
        ordered_names <- names[order(objects_to_pass)]
        ordered_object_table <- get_data_objects(list(list("", ordered_names)))
        unnamedOrderedParameters <- get_element_from_data_object_table(
          ordered_object_table,
          envir = prep_envir
        )

        # # CHANGED ALVARO: check if statistics is an argument of
        # the effects function
        isStatPar <- "statistics" %in% names(formals(effects[[id]]$effect))
        updates <- do.call(
          effects[[id]]$effect,
          c(
            unnamedOrderedParameters,
            statistics = stats[id],
            event,
            list(n1 = n1, n2 = n2, groups_network = groupsNetworkObject)
          )
        )

        # added Marion: update stats
        stats[[id]] <- updFun(stats[[id]], updates)

        # if (is_undirected_net && !isinteractionupdate) {
        #   event2 <- event
        #   event2$sender <- event$receiver
        #   event2$receiver <- event$sender
        #   updates2 <- do.call(
        #     effects[[id]]$effect,
        #     c(unnamedOrderedParameters,
        #       switch(isStatPar, list(statistics = stats[[id]]), NULL),
        #       event2,
        #       list(n1 = n1, n2 = n2, groups.network = groups.network.object)
        #     )
        #   )
        #   updates <- rbind(updates, updates2)
        # }

        if (!is.null(updates)) {
          updates_dependent[[id]] <- rbind(updates_dependent[[id]], updates)
          updates_intervals[[id]] <- rbind(updates_intervals[[id]], updates)
        }
      }
    } # end 3. (!dependent)

    pointers[next_event] <- 1 + pointers[next_event]
    valid_pointers <- pointers <= vapply(events, nrow, numeric(1))
  }

  if (progress && utils::getTxtProgressBar(pb) < n_dependent_events) {
    close(pb)
  }

  return(structure(
    list(
      initial_stats = initial_stats,
      dependent_stats_change = dependentStatistics,
      right_censored_stats_change = rightCensoredStatistics,
      intervals = time_intervals,
      # CHANGED MARION
      right_censored_intervals = timeIntervalsRightCensored,
      order_events = order_events,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      start_time = startTime,
      end_time = endTime
    ),
    class = "preprocessed.goldfish"
  ))
}

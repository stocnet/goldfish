###################### ##
#
# Goldfish package
# Preprocessing for DyNAM-i
#
###################### ##

#' preprocess event and related objects describe in the formula estimate for DyNAM-i
#'
#' Create a preprocess.goldfish class object for estimation
#'
#' @inheritParams preprocess
#' @param groupsNetwork a character with the object that contains the groups network information
#'
#' @return a list of class preprocessed.goldfish
#'
#' @importFrom methods is
#' @importFrom utils setTxtProgressBar getTxtProgressBar object.size txtProgressBar
#' @importFrom stats time
#' @noRd
preprocessInteraction <- function(
  subModel,
  events,
  effects,
  eventsObjectsLink,
  eventsEffectsLink,
  objectsEffectsLink,
  # multipleParameter,
  nodes,
  nodes2 = nodes,
  # add more parameters
  startTime = min(vapply(events, function(x) min(x$time), double(1))),
  endTime = max(vapply(events, function(x) max(x$time), double(1))),
  rightCensored = FALSE,
  verbose = TRUE,
  silent = FALSE,
  groupsNetwork = groupsNetwork) {

# For debugging
  if (identical(environment(), globalenv())) {
    startTime <- min(vapply(events, function(x) min(x$time), double(1)))
    endTime <- max(vapply(events, function(x) max(x$time), double(1)))
    verbose <- TRUE
    silent <- TRUE
  }

  prepEnvir <- environment()
  # initialize statistics functions from data objects
  # number of actors
  n1 <- nrow(get(nodes))
  n2 <- nrow(get(nodes2))
  nEffects <- length(effects)
  # changed Marion
  groupsNetworkObject <- get(groupsNetwork)
  # impute missing data in objects: 0 for networks and mean for attributes
  imputed <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)

  if (!silent) cat("Initializing cache objects and statistical matrices.\n")
  model <- "DyNAMi"
  stats <- initializeCacheStat(
    objectsEffectsLink = objectsEffectsLink, effects = effects,
    groupsNetwork = groupsNetworkObject, windowParameters = NULL,
    n1 = n1, n2 = n2, model = model, subModel = subModel, envir = prepEnvir)

  # We put the initial stats to the previous format of 3 dimensional array
  initialStats <- array(unlist(stats),
                        dim = c(n1, n2, nEffects)
  )

  # statCache <- lapply(statCache, "[[", "cache")

  # initialize return objects
  # CHANGED MARION: for choice model, only joining events
  if (rightCensored) {
    nDependentEvents <- length(unique(unlist(lapply(events, function(x) x$time))))
  } else {
    nDependentEvents <- sum(events[[1]]$increment == -1)
  }
  dependentStatistics <- list()
  rightCensoredStatistics <- list()
  timeIntervals <- list()
  timeIntervalsRightCensored <- list()
  # CHANGED MARION: added a list that tracks the chronological(ordered by time) order of events
  # between dependent and right-censored events
  # 1 is for dependent and 2 if for right-censored
  orderEvents <- list()
  event_time <- list()
  event_sender <- list()
  event_receiver <- list()

  # check start time and end time are valid values, set flags
  hasEndTime <- FALSE
  eventsMin <- min(vapply(events, function(x) min(x$time), double(1)))
  eventsMax <- max(vapply(events, function(x) max(x$time), double(1)))
  if (!is.null(endTime) && endTime != eventsMax) {
    stop(dQuote("DyNAMi"), " doesn't support setting the ", dQuote("endTime"), "parameter", call. = FALSE)
  }
  if (!is.null(startTime) && startTime != eventsMin) {
    stop(dQuote("DyNAMi"), " doesn't support setting the ", dQuote("StartTime"), "parameter", call. = FALSE)
  }

  # initialize loop parameters
  events[[1]] <- NULL
  pointers <- rep(1, length(events))
  validPointers <- rep(T, length(events))
  pointerDependent <- 1
  pointerTempRightCensored <- 1
  time <- startTime
  interval <- 0
  # updatesDependent/updatesIntervals: list of 6, each element if NULL
  updatesDependent <- vector("list", nEffects)
  updatesIntervals <- vector("list", nEffects)

  # added Marion: find index of the dependent, exogenous events on the groups
  # and of the past interaction updates
  dname <- eventsObjectsLink[1, 1]
  # PATCH Marion: the depdendent.depevents_DyNAMi is not sanitized yet
  dnameObject <- sanitizeEvents(get(dname),nodes,nodes2)
  assign(dname, dnameObject)

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
      if (inherits(ev, "interaction.groups.updates") && all(get(dname) == ev)) {
        depindex <- e
        deporder <- attr(ev, "order")
      } else if (inherits(ev, "interaction.groups.updates") && !all(get(dname) == ev)) {
        exoindex <- e
        exoorder <- attr(ev, "order")
      } else if (inherits(ev, "interaction.network.updates") && !is.null(attr(ev, "order"))) {
        numpast <- numpast + 1
        pastindexes[numpast] <- e
        pastorders[[numpast]] <- attr(ev, "order")
      }
    }
  }

  # If depindex and exoindex not there (because there was no effect with the default network)
  # we need to find them anyway!
  if (depindex == 0) {

    # find groups udates and add them to events
    groupsupdates <- attr(groupsNetworkObject, "events")

    # PATCH Marion: the groups update events were not sanitized
    groupsupdates1Object <- sanitizeEvents(get(groupsupdates[1]),nodes,nodes2)
    assign(groupsupdates[1], groupsupdates1Object)
    groupsupdates2Object <- sanitizeEvents(get(groupsupdates[2]),nodes,nodes2)
    assign(groupsupdates[2], groupsupdates2Object)

    if (all(get(dname) == get(groupsupdates[1]))) {
      depn <- groupsupdates[1]
      exon <- groupsupdates[2]
    } else {
      depn <- groupsupdates[2]
      exon <- groupsupdates[1]
    }
    depindex <- length(events) + 1
    exoindex <- length(events) + 2
    events[[depindex]] <- get(depn)
    events[[exoindex]] <- get(exon)

    # find orders
    deporder <- attr(events[[depindex]], "order")
    exoorder <- attr(events[[exoindex]], "order")

    # sanitize events
    nodesObject <- attr(groupsNetworkObject, "nodes")

    if (length(nodesObject) > 1) {
      nodes <- nodesObject[1]
      nodes2 <- nodesObject[2]
    } else nodes <- nodes2 <- nodesObject
    events[[depindex]] <- sanitizeEvents(events[[depindex]], nodes, nodes2)
    events[[exoindex]] <- sanitizeEvents(events[[exoindex]], nodes, nodes2)

    # augment the link objects
    eventsObjectsLink <- rbind(
      eventsObjectsLink,
      c(depn, groupsNetwork, groupsNetwork, NA, NA),
      c(exon, groupsNetwork, groupsNetwork, NA, NA)
    )

    eventsEffectsLink <- rbind(
      eventsEffectsLink,
      rep(NA, dim(eventsEffectsLink)[2]),
      rep(NA, dim(eventsEffectsLink)[2])
    )
    rownames(eventsEffectsLink)[dim(eventsEffectsLink)[1] - 1] <- depn
    rownames(eventsEffectsLink)[dim(eventsEffectsLink)[1]] <- exon

    objectsEffectsLink <- rbind(
      objectsEffectsLink,
      rep(NA, dim(objectsEffectsLink)[2])
    )
    rownames(objectsEffectsLink)[dim(objectsEffectsLink)[1]] <- groupsNetwork

    # reset the pointers for ALL events
    pointers <- rep(1, length(events))
    validPointers <- rep(TRUE, length(events))
  }


  # Set the counter for the ordered events
  cptorder <- 0

  # added Marion: updates of statistics
  updFun <- function(stat, change) {
    if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    return(stat)
  }

  # initialize progressbar output, CHANGED ALVARO: add iterators
  showProgressBar <- FALSE
  progressEndReached <- FALSE

  # iRightCensored <- 0
  iDependentEvents <- 0
  if (!silent) {
    cat("Preprocessing events.\n")
    showProgressBar <- TRUE
    pb <- utils::txtProgressBar(max = nDependentEvents, char = "*", style = 3)
    dotEvents <- ifelse(nDependentEvents > 50, ceiling(nDependentEvents / 50), 1) # # how often print, max 50 prints
  }

  # UPDATED ALVARO: logical values indicating the type of information in events
  isIncrementEvent <- vapply(events, function(x) "increment" %in% names(x), logical(1))
  isNodeEvent <- vapply(events, function(x) "node" %in% names(x), logical(1))

  # iterate over all event lists
  while (any(validPointers)) {

    # times: the timepoint for next events to update in all event lists
    times <- mapply(function(e, p) e[p, ]$time, events, pointers)
    increments <- mapply(function(e, p) {
      if ("increment" %in% names(e)) {
        e[p, ]$increment
      } else {
        0
      }
    }, events, pointers)

    # added Marion: we set priority to dependent, exogenous and past updates before anything else
    # and between those 3 (or the 2 first if the 3rd is not needed), the order is decided
    # note: when it's a windowed past update, the value of the cpt is 0
    mintime <- min(times, na.rm = TRUE)
    currentpointers <- which(validPointers & times == mintime)
    prioritypointers <- intersect(currentpointers, c(depindex, exoindex, pastindexes))
    if (length(prioritypointers) > 0) {
      cpts <- mapply(function(p) {
        if (p == depindex) {
          return(deporder[pointers[p]])
        }
        if (p == exoindex) {
          return(exoorder[pointers[p]])
        }
        if (p %in% pastindexes) {
          return(pastorders[[which(pastindexes == p)]][pointers[p]])
        }
      }, prioritypointers)
      if (max(cpts) == 0) {
        nextEvent <- prioritypointers[1]
      } else {
        nextcpt <- min(cpts[cpts > cptorder])
        cptorder <- nextcpt
        if (cptorder %in% deporder) nextEvent <- depindex
        if (cptorder %in% exoorder) nextEvent <- exoindex
        if (length(pastorders) > 0 && cptorder %in% pastorders[[1]]) {
          cptindexes <- prioritypointers[cpts == nextcpt]
          nextEvent <- cptindexes[1]
          if (length(cptindexes) > 1) cptorder <- cptorder - 1
        }
      }
    } else {# otherwise we take the first next event
      nextEvent <- currentpointers[1]
    }
    interval <- times[nextEvent] - time
    time <- min(times[validPointers])


    # changed Marion: for choice, only joining events are dependent events
    isDependent <- (subModel == "rate" && nextEvent == depindex) ||
      (subModel == "choice" && nextEvent == depindex && events[[depindex]][pointers[nextEvent], "increment"] > 0)

    # # CHANGED ALVARO: progress bar
    if (showProgressBar && iDependentEvents %% dotEvents == 0) {
      utils::setTxtProgressBar(pb, iDependentEvents)
    }

    if (showProgressBar && iDependentEvents == nDependentEvents) {
      utils::setTxtProgressBar(pb, iDependentEvents)
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
      iDependentEvents <- 1 + iDependentEvents
      dependentStatistics[[iDependentEvents]] <- updatesDependent
      timeIntervals[[iDependentEvents]] <- interval
      updatesDependent <- vector("list", nEffects)
      updatesIntervals <- vector("list", nEffects)
      # CHANGED MARION: added orderEvents
      orderEvents[[(pointerDependent + pointerTempRightCensored - 1)]] <- 1
      # CHANGDE SIWEI: added time point of each event (dependent & right-censorde)
      event_time[[(pointerDependent + pointerTempRightCensored - 1)]] <- time
      # CHANGED MARION: added sender and receiver
      varsKeep <- c(if (isNodeEvent[nextEvent]) "node" else c("sender", "receiver"), "increment")
      event <- events[[nextEvent]][pointers[nextEvent], varsKeep]
      event_sender[[(pointerDependent + pointerTempRightCensored - 1)]] <- event$sender
      event_receiver[[(pointerDependent + pointerTempRightCensored - 1)]] <- event$receiver

      # second update the network (no need to calculate the stats there because they will be updated
      # with the following exogenous event of leaving the previous group or joining an isolate)

      ## FOR TESTING: SEE AVAILABLE GROUPS>2 FOR EACH EVENT (TO COUNT THE PROPORTION OF GROUPS IN THE EVENTS)
      # if (max(colSums(groups.network.object)) > 2) {
      #   print(paste("event", iDependentEvents))
      #   grinds <- which(colSums(groups.network.object) > 2)
      #   for (grind in 1:length(grinds)) {
      #     print(paste("group present with actors: ",which(groups.network.object[, grinds[grind]]==1)))
      #   }
      #   if (event$increment == -1 && event$receiver %in% grinds) {
      #     print("this is a group leaving event!")
      #   }
      # }

      groupsNetworkObject[event$sender, event$receiver] <-
        groupsNetworkObject[event$sender, event$receiver] + event$increment
      assign(groupsNetwork, groupsNetworkObject)


      pointerDependent <- pointerDependent + 1
    }

    if (!isDependent) {
      # 2. store statistic updates for RIGHT-CENSORED (non-dependent, positive) intervals
      if (rightCensored && interval > 0) {
        # CHANGED MARION: the incremented index was incorrect
        # rightCensoredStatistics[[ pointers[nextEvent] ]] <- updatesIntervals
        # timeIntervalsRightCensored[[length(rightCensoredStatistics)]] <- interval
        rightCensoredStatistics <- append(rightCensoredStatistics, list(updatesIntervals))
        timeIntervalsRightCensored <- append(timeIntervalsRightCensored, interval)
        updatesIntervals <- vector("list", nEffects)
        # CHANGED MARION: added orderEvents
        orderEvents[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- 2
        event_time[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- time
        # CHANGED MARION: added sender and receiver
        # CHANGED WEIGUTIAN: removed "increment" which results a bug
        event <- events[[nextEvent]][pointers[nextEvent], ]
        if (isNodeEvent[nextEvent] & length(event) == 1) {
          event_sender[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- event
          event_receiver[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- event
        } else if (isNodeEvent[nextEvent] & length(event) > 1) {
          event_sender[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- event$node
          event_receiver[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- event$node
        } else {
          event_sender[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- event$sender
          event_receiver[[(pointers[depindex] + pointerTempRightCensored - 1)]] <- event$receiver
        }
        pointerTempRightCensored <- pointerTempRightCensored + 1
      }

    # 3. update stats and data objects for OBJECT CHANGE EVENTS (all non-dependent events)

      # Two steps are performed for non-dependent events
      #   (0. get objects and update increment columns)
      #   a. Calculate statistic updates for each event that relates to the data update
      #   b. Update the data objects

      objectNameTable <- eventsObjectsLink[nextEvent + 1, -1]
      objectName <- objectNameTable$name
      object <- getElementFromDataObjectTable(objectNameTable, envir = prepEnvir)[[1]]
      isUndirectedNet <- FALSE
      if (inherits(object, "network.goldfish")) {
        isUndirectedNet <- !attr(object, "directed")
      }

      # # CHANGED ALVARO: avoid dependence in variables position
      if (isIncrementEvent[nextEvent]) {
        varsKeep <- c(if (isNodeEvent[nextEvent]) "node" else c("sender", "receiver"), "increment")
        event <- events[[nextEvent]][pointers[nextEvent], varsKeep]

        if (isNodeEvent[nextEvent]) oldValue <- object[event$node]
        if (!isNodeEvent[nextEvent]) oldValue <- object[event$sender, event$receiver]
        event$replace <- oldValue + event$increment
        event$increment <- NULL
      } else {
        varsKeep <- c(if (isNodeEvent[nextEvent]) "node" else c("sender", "receiver"), "replace")
        event <- events[[nextEvent]][pointers[nextEvent], varsKeep]
      }

      #if (!isNodeEvent[nextEvent] && event$replace < 0) {
      #  warning("You are dissolving a tie which doesn't exist!", call. = FALSE)
      #}


      # b. Update the data object
      if (isNodeEvent[nextEvent]) object[event$node] <- event$replace
      if (!isNodeEvent[nextEvent]) {
        # [sender, receiver] value: replace value of the event
        object[event$sender, event$receiver] <- event$replace
        if (isUndirectedNet) {
          object[event$receiver, event$sender] <- event$replace
        }
      }

      # Assign object
      eval(parse(text = paste(objectName, "<- object")), envir = prepEnvir)

      # added Marion: for interaction model, check whether this is an exogenous event or past update
      isinteractionupdate <- inherits(events[[nextEvent]], "interaction.network.updates")
      isgroupupdate <- inherits(events[[nextEvent]], "interaction.groups.updates")

      # a. calculate statistics changes: if EXOGENOUS JOINING OR LEAVING, everything is recalculated
      if (isgroupupdate) {
        effIds <- seq.int(dim(eventsEffectsLink)[2])
      } else {# OTHERWISE (PAST UPDATE or ATTRIBUTE UPDATE), only statistics related to the object
        effIds <- which(!is.na(eventsEffectsLink[nextEvent + 1, ]))
      }
      groupsNetworkObject <- get(groupsNetwork)


      for (id in effIds) {
        # create the ordered list for the objects
        objectsToPass <- objectsEffectsLink[, id][!is.na(objectsEffectsLink[, id])]
        names <- rownames(objectsEffectsLink)[!is.na(objectsEffectsLink[, id])]
        orderedNames <- names[order(objectsToPass)]
        orderedObjectTable <- getDataObjects(list(list("", orderedNames)))
        unnamedOrderedParameters <- getElementFromDataObjectTable(orderedObjectTable, envir = prepEnvir)

        # # CHANGED ALVARO: check if statistics is an argument of the effects function
        isStatPar <- "statistics" %in% names(formals(effects[[id]]$effect))
        updates <- do.call(
          effects[[id]]$effect,
          c(
            unnamedOrderedParameters,
            statistics = stats[id],
            event, list(n1 = n1, n2 = n2, groupsNetwork = groupsNetworkObject)
          )
        )

        # added Marion: update stats
        stats[[id]] <- updFun(stats[[id]], updates)

        # if (isUndirectedNet && !isinteractionupdate) {
        #   event2 <- event
        #   event2$sender <- event$receiver
        #   event2$receiver <- event$sender
        #   updates2 <- do.call(effects[[id]]$effect,
        #                        c(unnamedOrderedParameters,
        #                          switch(isStatPar, list(statistics = stats[[id]]), NULL),
        #                          event2, list(n1 = n1, n2 = n2, groups.network = groups.network.object))
        #   )
        #   updates <- rbind(updates, updates2)
        # }

        if (!is.null(updates)) {
          updatesDependent[[id]] <- rbind(updatesDependent[[id]], updates)
          updatesIntervals[[id]] <- rbind(updatesIntervals[[id]], updates)
        }
      }
    } # end 3. (!dependent)


    pointers[nextEvent] <- 1 + pointers[nextEvent]
    validPointers <- pointers <= sapply(events, nrow)
  }

  if (showProgressBar && utils::getTxtProgressBar(pb) < nDependentEvents) {
    close(pb)
  }

  return(structure(list(
    initialStats = initialStats,
    dependentStatsChange = dependentStatistics,
    rightCensoredStatsChange = rightCensoredStatistics,
    intervals = timeIntervals,
    # CHANGED MARION
    rightCensoredIntervals = timeIntervalsRightCensored,
    orderEvents = orderEvents,
    eventTime = event_time,
    eventSender = event_sender,
    eventReceiver = event_receiver,
    startTime = startTime,
    endTime = endTime
  ),
  class = "preprocessed.goldfish"
  ))
}


getStatisticsFromObjects_interaction <- function(FUN,
                                                 network = NULL, network2 = NULL,
                                                 attribute = NULL, attribute2 = NULL,
                                                 n1, n2, groups.network.object = groups.network.object) {
  objectList <- list()
  if (!is.null(network)) objectList <- append(objectList, list(network = network))
  if (!is.null(network2)) objectList <- append(objectList, list(network2 = network2))
  if (!is.null(attribute)) objectList <- append(objectList, list(attribute = attribute))
  if (!is.null(attribute2)) objectList <- append(objectList, list(attribute2 = attribute2))

  # # CHANGED ALVARO: preallocate for real values
  # emptyObjects <- lapply(objectList, function(obj) {
  #  if(is.matrix(obj)) obj[1:nrow(obj), 1:ncol(obj)] <- NA_real_
  #  if(is.vector(obj)) obj[1:length(obj)] <- NA_real_
  #  obj
  # })
  # changed Marion: there are some stats that should be filled in from the beginning
  emptyObjects <- objectList

  isStatPar <- "statistics" %in% names(formals(FUN))

  stats <- matrix(0, n1, n2)

  for (iObj in seq.int(length(objectList))) {
    if (is.matrix(objectList[[iObj]])) {

      # # CHANGED ALVARO: not updates to make, skip the iterations
      # if (all(objectList[[iObj]][!is.na(objectList[[iObj]])] == 0)) {
      #  next
      # }

      # # CHANGED ALVARO: not iterate over all the entries, O(m)
      # Marion: problem when there is a network for an effect that is not the same size as the dependent network
      rowsIter <- (1:n1)[rowSums(objectList[[iObj]] != 0, na.rm = TRUE) > 0]
      for (i in rowsIter) {
        # hack Marion: remove this because dimensions are not equal
        # colsIter <- (1:n2)[!is.na(objectList[[iObj]][i, ]) & objectList[[iObj]][i, ] != 0]
        # for(j in colsIter) { #TO check: issue for two-mode networks
        for (j in 1:n2) {
          # hack Marion: add groups.network
          additionalParams <- list(
            sender = i,
            receiver = j,
            replace = 0,
            n1 = n1, n2 = n2, groups.network = groups.network.object
          )
          # construct network objects step by step from empty objects
          res <- do.call(
            FUN,
            c(
              switch(isStatPar, list(statistics = stats), NULL),
              emptyObjects, additionalParams
            )
          )
          if (!is.null(res) && nrow(res) > 0) {
            stats[cbind(res[, 1], res[, 2])] <- res[, 3]
          }
          # update networks
          # hack: if it's not the same dimension, the network shouldn't be updated
          if (dim(objectList[[iObj]])[1] == n1 && dim(objectList[[iObj]])[2] == n2) {
            emptyObjects[[iObj]][i, j] <- objectList[[iObj]][i, j]
          }
        }
      }
    } # matrix

    if (is.vector(objectList[[iObj]])) {
      for (i in seq.int(length(objectList[[iObj]]))) {
        # hack Marion: add groups.network, put everything to sender, receiver (=1, doesn't mattter for now)
        additionalParams <- list(
          sender = i,
          receiver = 1,
          replace = objectList[[iObj]][i],
          n1 = n1, n2 = n2, groups.network = groups.network.object
        )
        # use empty objects for attribute updates
        res <- do.call(
          FUN,
          c(
            switch(isStatPar, list(statistics = stats), NULL),
            emptyObjects, additionalParams
          )
        )
        if (!is.null(res) && nrow(res) > 0) {
          stats[cbind(res[, 1], res[, 2])] <- res[, 3]
        }
        emptyObjects[[iObj]][i] <- objectList[[iObj]][i]
      }
    }
  } # iterate over all objects

  return(stats)
}


initializeStatsMatrices_interaction <- function(objectsEffectsLink, effects, n1, n2, groups.network.object) {
  objTable <- getDataObjects(list(rownames(objectsEffectsLink)), removeFirst = F)
  isAttribute <- sapply(
    getElementFromDataObjectTable(objTable),
    function(x) {
      if (is.matrix(x)) {
        return(F)
      }
      if (is.vector(x)) {
        return(T)
      }
      return(NULL)
    }
  )
  objects <- getElementFromDataObjectTable(objTable)
  # list of 4, call matrix, friendship matrix, actor$gradetype vector, actor$floor vector
  namedObjectList <- list()
  for (i in seq.int(ncol(objectsEffectsLink))) {
    o <- objectsEffectsLink[, i]
    attIDs <- which(!is.na(o) & isAttribute)
    netIDs <- which(!is.na(o) & !isAttribute)
    attributes <- objects[attIDs[order(o[attIDs])]]
    networks <- objects[netIDs[order(o[netIDs])]]
    # namedObjectList: list of 6, each element is a list of 4: network, network2, attribute, attribute2
    # in this case, the element in the list of 4 is
    # 1: network: 84*84 matrix, 2: network: 84*84 matrix
    # 3: network: 84*84 matrix, 4: network: 84*84 matrix, 5: attribute: 84 vector, 6: attribute: 84 vector
    namedObjectList[[i]] <- list(
      network = if (length(networks) > 0) networks[[1]] else NULL,
      network2 = if (length(networks) > 1) networks[[2]] else NULL,
      attribute = if (length(attributes) > 0) attributes[[1]] else NULL,
      attribute2 = if (length(attributes) > 1) attributes[[2]] else NULL
    )
  }

  stats <- list()
  for (iEff in seq.int(length(effects))) {
    args <- c(list(FUN = effects[[iEff]]$effect), namedObjectList[[iEff]],
              list(n1 = n1, n2 = n2, groups.network = groups.network.object))
    tryCatch({
        stats[[iEff]] <- do.call(getStatisticsFromObjects_interaction, args)
      },
      error = function(e) {
        stop(paste(
          "Effect",
          paste(colnames(objectsEffectsLink)[iEff]),
          "cannot be initialized with objects",
          paste(rownames(objectsEffectsLink)[iEff], collapse = ", ")
        ))
      }
    )
  }
  # stats: list of 6, each element is a 84*84 matrix
  return(stats)
}

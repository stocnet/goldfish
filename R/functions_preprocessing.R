#' preprocess event and related objects describe in the formula to estimate
#'
#' Create a preprocess.goldfish class object with the neccesary information for estimation.
#'
#' @inheritParams estimate
#' @param events list with all
#' @param effects list of effects functions return by \code(createEffectsFunctions).
#' @param eventsObjectsLink data.frame output of \code(getEventsAndObjectsLink)
#' @param eventsEffectsLink data.frame output of \code(getEventsEffectsLink)
#' @param objectsEffectsLink data.frame output of \code(getObjectsEffectsLink)
#' @param nodes character with the object that contains the nodes information
#' @param nodes2 character with the object that contains the nodes information,
#'   different from \codes(nodes) when \code(isTwoMode = TRUE).
#' @param isTwoMode logical is it a two mode network?
#' @param startTime numerical start time to preprocess the data
#' @param endTime numerical end time to preprocess the data
#' @param rightCensored logical does it consider right censored events?
#' @param verbose logical verbose
#' @param silent logical silent
#'
#' @return a list of class preprocessed.goldfish
#'
#' @importFrom methods is
#' @importFrom utils setTxtProgressBar getTxtProgressBar object.size txtProgressBar
#' @importFrom stats time
#' @noRd
preprocess <- function(
  model,
  subModel,
  events,
  effects,
  windowParameters,
  eventsObjectsLink,
  eventsEffectsLink,
  objectsEffectsLink,
  # multipleParameter,
  nodes,
  nodes2 = nodes,
  isTwoMode,
  # add more parameters
  startTime = min(vapply(events, function(x) min(x$time), double(1))),
  endTime = max(vapply(events, function(x) max(x$time), double(1))),
  rightCensored = FALSE,
  verbose = TRUE,
  silent = FALSE) {

  # For debugging
  if (identical(environment(), globalenv())) {
    startTime <- min(vapply(events, function(x) min(x$time), double(1)))
    endTime <- max(vapply(events, function(x) max(x$time), double(1)))
    verbose <- TRUE
    silent <- TRUE
  }

  prepEnvir <- environment()
  # print(match.call())
  # initialize statistics functions from data objects
  # number of actors
  n1 <- nrow(get(nodes))
  n2 <- nrow(get(nodes2))
  nEffects <- length(effects)

  # check start time and end time are valid values, set flags
  hasEndTime <- FALSE
  hasStartTime <- FALSE
  isValidEvent <- TRUE

  eventsMin <- min(vapply(events, function(x) min(x$time), double(1)))
  eventsMax <- max(vapply(events, function(x) max(x$time), double(1)))
  if (is.null(endTime)) {
    endTime <- eventsMax
  } else if (endTime != eventsMax) {
    if (!is.numeric(endTime)) endTime <- as.numeric(endTime)
    if (eventsMin > endTime) stop("End time smaller than first event time.", call. = FALSE)
      # to solve: if endTime > eventsMax
      # should it produce censored events? warning?
    # add a fake event to the event list
    endTimeEvent <- data.frame(time = endTime, sender = NA, receiver = NA, replace = NA)
    events <- c(events, endtime = list(endTimeEvent))
    hasEndTime <- TRUE
  }
  if (is.null(startTime)) {
    startTime <- eventsMin
  } else if (startTime != eventsMin) {
    if (!is.numeric(startTime)) startTime <- as.numeric(startTime)
    if (eventsMax < startTime)
      stop("Start time geater than last event time.", call. = FALSE)
    hasStartTime <- TRUE
    if (eventsMin < startTime) isValidEvent <- FALSE
    # To solve: if startTime < eventsMin should be a warning?
  }

  # impute missing data in objects: 0 for networks and mean for attributes
  imputed <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)

  if (!silent) cat("Initializing cache objects and statistical matrices.\n")

  statCache <- initializeCacheStat(
    objectsEffectsLink = objectsEffectsLink, effects = effects,
    groupsNetwork = NULL, windowParameters = windowParameters,
    n1 = n1, n2 = n2, model = model, subModel = subModel, envir = prepEnvir)
  # We put the initial stats to the previous format of 3 dimensional array
  initialStats <- array(unlist(lapply(statCache, "[[", "stat")),
                        dim = c(n1, n2, nEffects)
  )

  statCache <- lapply(statCache, "[[", "cache")

  # UPDATED ALVARO: logical values indicating the type of information in events
  isIncrementEvent <- vapply(events, function(x) "increment" %in% names(x), logical(1))
  isNodeEvent <- vapply(events, function(x) "node" %in% names(x), logical(1))

  # initialize return objects

  # calculate total of events
  time <- unique(events[[1]]$time)
  nRightCensoredEvents <- unique(unlist(lapply(events, function(x) x$time)))
  nTotalEvents  <- as.integer(sum(nRightCensoredEvents <= endTime))
  nRightCensoredEvents <- setdiff(nRightCensoredEvents, time)
  if (length(nRightCensoredEvents) > 1) {
    nRightCensoredEvents <- as.integer(sum(
      nRightCensoredEvents >= startTime &
      nRightCensoredEvents <= endTime) - 1)
  } else nRightCensoredEvents <- 0
  nDependentEvents <- as.integer(sum(time >= startTime & time <= endTime))
  if (!rightCensored) nRightCensoredEvents <- 0L
  # CHANGED ALVARO: preallocate objects sizes
  dependentStatistics <- vector("list", nDependentEvents)
  timeIntervals <- vector("numeric", nDependentEvents)
  rightCensoredStatistics <- vector("list", nRightCensoredEvents)
  timeIntervalsRightCensored <- vector("numeric", nRightCensoredEvents)
  # CHANGED MARION: added a list that tracks the chronological(ordered by time)
  #                 order of events between dependent and right-censored events
  # 1 is for dependent and 2 if for right-censored
  # Also a list of the senders and receivers to allow the preprocessingInit routine
  orderEvents <- vector("integer", nDependentEvents + nRightCensoredEvents)
  event_time <- vector("numeric", nDependentEvents + nRightCensoredEvents)
  event_sender <- vector("integer", nDependentEvents + nRightCensoredEvents)
  event_receiver <- vector("integer", nDependentEvents + nRightCensoredEvents)
  finalStep <- FALSE

  # # Remove duplicates of event lists!

  # initialize loop parameters
  # pointers = [1,1,1](events have three elements:
  # callDependent(439*4), calls(439*4), friendship(766*4))
  pointers <- rep(1, length(events))
  validPointers <- rep(TRUE, length(events))
  if (hasEndTime) validPointers <- vapply(events, function(x) x$time[1], double(1)) <= endTime
  pointerTempRightCensored <- 1L
  time <- startTime
  interval <- 0L
  # updatesDependent/updatesIntervals: list of 6, each element if NULL
  updatesDependent <- vector("list", nEffects)
  updatesIntervals <- vector("list", nEffects)

  # initialize progressbar output, CHANGED ALVARO: add iterators
  showProgressBar <- FALSE
  progressEndReached <- FALSE

  # iRightCensored <- 0
  iDependentEvents <- 0L
  iTotalEvents <- 0L
  if (!silent) {
    cat("Preprocessing events.\n", startTime, endTime, nTotalEvents)
    showProgressBar <- TRUE
    # # how often print, max 50 prints
    pb <- utils::txtProgressBar(max = nTotalEvents, char = "*", style = 3)
    dotEvents <- ifelse(nTotalEvents > 50, ceiling(nTotalEvents / 50), 1)
  }

  # iterate over all event lists
  while (any(validPointers)) {
    iTotalEvents <- iTotalEvents + 1L
    # times: the timepoint for next events to update in all event lists
    times <- mapply(function(e, p) e[p, ]$time, events, pointers)
    nextEvent <- which(validPointers)[head(which.min(times[validPointers]), 1)]
    nextEventTime <- times[nextEvent]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && nextEventTime < endTime) {
         interval <- nextEventTime - time
      } else if (isValidEvent && nextEventTime >= endTime) {
        interval <- nextEventTime - endTime
      } else if (!isValidEvent && nextEventTime > startTime) {
        interval <- startTime - nextEventTime
        isValidEvent <- TRUE
        finalStep <- TRUE
      }
    } else interval <- nextEventTime - time

    time <- nextEventTime

    isDependent <- nextEvent == 1

    eventPos <- pointers[1] + pointerTempRightCensored - 1
    # # CHANGED ALVARO: progress bar
    if (showProgressBar && iTotalEvents %% dotEvents == 0) {
      utils::setTxtProgressBar(pb, iTotalEvents)
    }

    if (showProgressBar && iTotalEvents == nTotalEvents) {
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
      dependentStatistics[[iDependentEvents]] <- updatesDependent
      timeIntervals[[iDependentEvents]] <- interval
      updatesDependent <- vector("list", nEffects)
      updatesIntervals <- vector("list", nEffects)
      # CHANGED MARION: added orderEvents
      orderEvents[[eventPos]] <- 1L
      # CHANGED SIWEI: added time point of each event (dependent & right-censored)
      event_time[[eventPos]] <- time
      # CHANGED MARION: added sender and receiver
      event <- events[[nextEvent]][pointers[nextEvent], ]
      if (isNodeEvent[nextEvent]) {
        event_sender[[eventPos]] <- event$node
        event_receiver[[eventPos]] <- event$node
      } else {
        event_sender[[eventPos]] <- event$sender
        event_receiver[[eventPos]] <- event$receiver
      }

    } else {
      # 2. store statistic updates for RIGHT-CENSORED (non-dependent, positive) intervals
      if (isValidEvent && rightCensored && interval > 0) {
        # CHANGED MARION: the incremented index was incorrect
        #rightCensoredStatistics[[ pointers[nextEvent] ]] <- updatesIntervals
        #timeIntervalsRightCensored[[length(rightCensoredStatistics)]] <- interval
        rightCensoredStatistics[[pointerTempRightCensored]] <- updatesIntervals
        timeIntervalsRightCensored[[pointerTempRightCensored]] <- interval
        updatesIntervals <- vector("list", nEffects)

        # CHANGED MARION: added orderEvents
        orderEvents[[eventPos]] <- 2L
        event_time[[eventPos]] <- time
        # CHANGED MARION: added sender and receiver
        # CHANGED WEIGUTIAN: removed "increment" which results a bug
        # Check (WEIGUTIAN): check whether the following block is necessary for
        #   right censored event,
        #   Because in the right-censored events there's no sender and receiver.
        event <- events[[nextEvent]][pointers[nextEvent], ]
        if (isNodeEvent[nextEvent] & length(event) == 1) {
          event_sender[[eventPos]] <- event
          event_receiver[[eventPos]] <- event
        } else if (isNodeEvent[nextEvent] & length(event) > 1) {
          event_sender[[eventPos]] <- event$node
          event_receiver[[eventPos]] <- event$node
        } else {
          event_sender[[eventPos]] <- event$sender
          event_receiver[[eventPos]] <- event$receiver
        }
        pointerTempRightCensored <- pointerTempRightCensored + 1
      }

      # 3. update stats and data objects for OBJECT CHANGE EVENTS (all non-dependent events)

      # Two steps are performed for non-dependent events
      #   (0. get objects and update increment columns)
      #   a. Calculate statistic updates for each event that relates to the data update
      #   b. Update the data objects

      objectNameTable <- eventsObjectsLink[nextEvent, -1]
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
        # missing data imputation
        if (isNodeEvent[nextEvent]) {
          oldValue <- object[event$node]
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
        varsKeep <- c(if (isNodeEvent[nextEvent]) "node" else c("sender", "receiver"), "replace")
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

      if (!isNodeEvent[nextEvent] && event$replace < 0) {
        warning("You are dissolving a tie which doesn't exist!", call. = FALSE)
      }


      ## 3a. calculate statistics changes
      if (!finalStep) for (id in which(!is.na(eventsEffectsLink[nextEvent, ]))) {
        # create the ordered list for the objects
        objectsToPass <- objectsEffectsLink[, id][!is.na(objectsEffectsLink[, id])]
        names <- rownames(objectsEffectsLink)[!is.na(objectsEffectsLink[, id])]
        orderedNames <- names[order(objectsToPass)]
        orderedObjectTable <- getDataObjects(list(list("", orderedNames)))
        .objects <- getElementFromDataObjectTable(orderedObjectTable, envir = prepEnvir)
        # identify class to feed effects functions
        objCat <- assignCatToObject(.objects)
        attIDs <- which(objCat == "attribute")
        netIDs <- which(objCat == "network")
        if (attr(objCat, "noneClass"))
          stop("An object is not assigned either as network or attibute",
               paste(names[attr(objCat, "manyClasses") != 1], collapse = ", "),
               "check the class of the object.", call. = FALSE)

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
          netUpdate = if (length(.objects[netIDs]) == 1) {
            NULL
          } else {
            which(orderedNames == objectName)
          }
        )

        effectUpdate <- callFUN(
          effects, id, "effect", c(.argsFUN, event), " cannot update \n",
          colnames(objectsEffectsLink)[id]
        )

        updates <- effectUpdate$changes
        # if cache and changes are not null update cache
        if (!is.null(effectUpdate$cache) & !is.null(effectUpdate$changes)) {
          statCache[[id]] <- effectUpdate$cache
        }

        if (isUndirectedNet) {
          event2 <- event
          event2$sender <- event$receiver
          event2$receiver <- event$sender
          if (!is.null(effectUpdate$cache) & !is.null(effectUpdate$changes))
            .argsFUN$cache <- statCache[[id]]
          effectUpdate2 <- callFUN(
            effects, id, "effect", c(.argsFUN, event2), " cannot update \n",
            colnames(objectsEffectsLink)[id]
          )

          if (!is.null(effectUpdate2$cache) & !is.null(effectUpdate2$changes))
            statCache[[id]] <- effectUpdate2$cache
          updates2 <- effectUpdate2$changes
          updates <- rbind(updates, updates2)
        }

        if (!is.null(updates)) {
          # CHANGED WEIGUTIAN: UPDATE THE STAT MAT AND IMPUTE THE MISSING VALUES
          # statCache[[id]][["stat"]][cbind(updates[, "node1"], updates[, "node2"])] <- updates[, "replace"]
          # if (anyNA(statCache[[id]][["stat"]])) {
          #   position_NA <- which(is.na(statCache[[id]][["stat"]]), arr.ind  = TRUE)
          #   average <- mean(statCache[[id]][["stat"]], na.rm = TRUE)
          #   updates[is.na(updates[, "replace"]), "replace"] <- average
          #   statCache[[id]][["stat"]][position_NA] <- average
          # }
          updatesDependent[[id]] <- rbind(updatesDependent[[id]], updates)
          updatesIntervals[[id]] <- rbind(updatesIntervals[[id]], updates)
        }
      }

      # 3b. Update the data object
      if (!finalStep) {
        if (!is.null(event$node)) object[event$node] <- event$replace
        if (!is.null(event$sender)) {
          # [sender, receiver] value: replace value of the event
          object[event$sender, event$receiver] <- event$replace
          if (isUndirectedNet) {
            object[event$receiver, event$sender] <- event$replace
          }
        }
        # Assign object
        eval(parse(text = paste(objectName, "<- object")), envir = prepEnvir)
      }
    } # end 3. (!dependent)

    # update events pointers
    pointers[nextEvent] <- 1 + pointers[nextEvent]
    validPointers <- pointers <= vapply(events, nrow, integer(1)) & times <= endTime
  }

  if (showProgressBar && utils::getTxtProgressBar(pb) < nTotalEvents) {
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

#' initialize the cache object or the stat matrices
#'
#' @param objectsEffectsLink data.frame output of \code(getObjectsEffectsLink)
#' @param effects list of effects functions return by \code(createEffectsFunctions).
#' @param groupsNetwork matrix that defines groups partition in DyNAMi
#' @param windowParameters NULL or numeric value with the size of the window
#' @param n1 int \code(nrow(network))
#' @param n2 int \code(ncol(network))
#' @param model character
#' @param subModel character
#' @param envir environment where get the objects
#'
#' @return a list of size length(effects): list with initial cache object and stat matrices
#'
#' @noRd
initializeCacheStat <- function(
  objectsEffectsLink, effects,
  groupsNetwork, windowParameters,
  n1, n2,
  model, subModel, envir = environment()) {
  objTable <- getDataObjects(list(rownames(objectsEffectsLink)),
                             removeFirst = FALSE
  )
  .objects <- getElementFromDataObjectTable(objTable, envir = envir)
  # list of 4, call matrix, friendship matrix, actor$gradetype vector, actor$floor vector
  objCat <- assignCatToObject(.objects)
  if (attr(objCat, "noneClass"))
    stop("An object is not assigned either as network or attibute",
         paste(rownames(objectsEffectsLink)[attr(objCat, "manyClasses") != 1], collapse = ", "),
         "check the class of the object.", call. = FALSE)

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
      messageEffect <- paste0(" cannot initialized with objects ", objectsNames, "\n")
      # init
      .argsFUN <- list(
        effectFun = effects[[iEff]][["effect"]],
        network = if (length(networks) == 1) networks[[1]] else networks,
        attribute = if (length(attributes) == 1) attributes[[1]] else attributes,
        groupsNetwork = groupsNetwork,
        window = windowParameters[[iEff]],
        n1 = n1,
        n2 = n2
      )
      callFUN(
        effects, iEff, "initEffect", .argsFUN, messageEffect,
        labelEffect
      )
    })
}


#' call fun
#' Call a function with a variable set of arguments (effects functions)
#' @param effects list of effects functions return by \code(createEffectsFunctions).
#' @param effectPos int indicates the effect to be used.
#' @param effectType character indicates which effect function use. Available values
#'        \code(c("initCache", "updateCache", "initStat", "effect"))
#' @param .argsFUN named list with the arguments to feed FUN.
#' @param error function that returns the error mesage if any.
#' @param warning function that returns the warning mesage if any.
#' @param effectLabel character use by the error or warning function to give an
#'        additional information to the user.
#'
#' @return the output of call \code(effects[[effectPos]][[effectType]]) with arguments
#'         \code(.argsFUN) in the case if not errors.
#' @noRd
#' @examples
#' \dontrun{
#' .argsFUN <- list(network = m, n1 = 5, n2 = 5, sender = 1, receiver = 5, replace = 0)
#' effects <- list(list(effect = out))
#'
#' ver2 <- callFUN(effects = effects, effectPos = effectPos, effectType = "effect",
#'                 .argsFUN = .argsFUN, textMss = " ver ",
#'                 effectLabel = "out")
#'
#'
#' .argsFUN <- list(network = m, n1 = 5, n2 = 5, sender = 1, receiver = 5)
#' effects <- list(list(effect = out))
#'
#' ver2 <- callFUN(effects = effects, effectPos = effectPos, effectType = "effect",
#'                 .argsFUN = .argsFUN, textMss = " ver ",
#'                 effectLabel = "out")
#' }
callFUN <- function(effects, effectPos, effectType, .argsFUN, textMss,
                    effectLabel) {
  err <- NULL
  warn <- NULL
  .argsNames <- formals(effects[[effectPos]][[effectType]])
  .argsKeep <- pmatch(names(.argsNames), names(.argsFUN)) # check for more than one net
  errorHandler <- function(e) {
    erro <- simpleError(paste0("Effect ", sQuote(effectLabel), " (", effectPos, ") ", textMss, e$message))
    stop(erro)
  }
  tryCatch({
    withCallingHandlers({
      callRes <- do.call(
        effects[[effectPos]][[effectType]],
        .argsFUN[na.omit(.argsKeep)]
      )},
      error = identity,
      warning = function(w) {
        warn <<- w
        invokeRestart("muffleWarning")
      }
    )
  },
  error = errorHandler
  )
  if (!is.null(warn)) warning(warn)
  return(callRes)
}


#' Impute missing values
#' If network missing values are replace by zero, if attributes missing values are impute by the mean.
#' @param objectsEffectsLink matrix. Rows objects, columns effects, 1 or NA. getObjectsEffectsLink(rhsNames) output.
#' @param envir evaluation enviroment to get and assign impute objects.
#'
#' @return a vector of nrow(objectsEffectsLink) length with logical value signaling imputation of missing values.
#' @noRd
#'
#' @examples
#' \dontrun{
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
#' objectsEffectsLink <- matrix(c(1, NA, NA, 1), nrow = 2, ncol = 2,
#'                              dimnames = list(c("networkAlgo", "actorsEx$attr1"),
#'                                              c("inertia", "alter")))
#' prepEnvir <- environment()
#'
#' revisar <- imputeMissingData(objectsEffectsLink, envir = prepEnvir)
#' }
imputeMissingData <- function(objectsEffectsLink, envir = .GlobalEnv) {
  # get data object table, row objects columns class (matrix, attribute)
  objTable <- getDataObjects(list(rownames(objectsEffectsLink)),
                             removeFirst = FALSE)
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
      object[is.na(object)] <- mean(object, na.rm = TRUE)
      done[iEff] <- TRUE
      # cat("vector\n")
      # Assign object
      assign("object", object, envir = envir)
      eval(parse(text = paste(objectName, "<- object")), envir = envir)
    }
  }
  return(done)
}

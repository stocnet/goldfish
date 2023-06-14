

#' Get compositional events
#'
#' 
#' @param nodesInfo an object output from [setNodesInfo()]
#' @param envir environment where to look objects up
#'
#' @return a list of length two. The events data frames with the compositional
#' changes 
#' @noRd
#'
#' @examples
#' getCompositionChange(list(isTwoMode = FALSE, nodes = "actorsEx"))
#' getCompositionChange(list(
#'   isTwoMode = TRUE, nodes = "actorsEx", nodes2 = "clubsEx"
#' ))
getCompositionChange <- function(nodesInfo, envir = new.env()) {
  attributesNodes <- attributes(get(nodesInfo[["nodes"]], envir = envir))
  if ("present" %in% attributesNodes[["dynamicAttributes"]]) {
    compChangeName1 <-  attributesNodes[["events"]][
      "present" == attributesNodes[["dynamicAttributes"]]
    ]
    compChange1 <- get(compChangeName1, envir = envir)
  } else {
    compChange1 <- NULL  
  }
  
  if (nodesInfo[["isTwoMode"]]) {
    attributesNodes <- attributes(get(nodesInfo[["nodes2"]], envir = envir))
    if ("present" %in% attributesNodes[["dynamicAttributes"]]) {
      compChangeName2 <-  attributesNodes[["events"]][
        "present" == attributesNodes[["dynamicAttributes"]]
      ]
      compChange2 <- get(compChangeName2, envir = envir)
    } else {
      compChange2 <- NULL  
    }  
  } else {
    compChange2 <- NULL
  }
  
  return(list(compChange1 = compChange1, compChange2 = compChange2))
}


listCompositionChange <- function(nodesInfo, envir = new.env()) {
  attributesNodes <- attributes(get(nodesInfo[["nodes"]], envir = envir))
  listRet <- NULL
  if ("present" %in% attributesNodes[["dynamicAttributes"]])
    listRet <- paste0(nodesInfo[["nodes"]], "$present")

  if (nodesInfo[["isTwoMode"]]) {
    attributesNodes <- attributes(get(nodesInfo[["nodes2"]], envir = envir))
    if ("present" %in% attributesNodes[["dynamicAttributes"]])
      listRet <- c(listRet, paste0(nodesInfo[["nodes2"]], "$present"))
  }
  
  # if (is.null(listRet)) return(listRet)
  # 
  # objRet <- as.list(rep.int("", length(listRet)))
  # names(objRet) <- listRet

  return(listRet)
}

#' Define nodes info from dependent event object
#' 
#' @param depName character with the name of the dependent event object
#' @param envir environment where the object is located
#'
#' @return
#' a list with three components: `isTwoMode` logical value indicating whether
#' the dependent event uses a default network a two-mode network,
#' `nodes` a character with the name of the object defining the first mode,
#' and `nodes2` a character with the name of the object defining the second mode
#' of the dependent event's default-network. `nodes2` is set to `nodes` value
#' when `isTwoMode` is `FALSE`.
#' @noRd
#'
#' @examples
#' setNodesInfo("depNetwork")
setNodesInfo <- function(depName, envir = new.env()) {
  # get node sets of dependent variable
  nodes <- attr(get(depName, envir = envir), "nodes")
  isTwoMode <- FALSE
  
  # two-mode networks(2 kinds of nodes)
  if (length(nodes) == 2) {
    nodes2 <- nodes[2]
    nodes <- nodes[1]
    isTwoMode <- TRUE
  } else {
    nodes2 <- nodes
  }
  
  return(list(isTwoMode = isTwoMode, nodes = nodes, nodes2 = nodes2))
}


#' Joining parsed formulas
#'
#' @param parsedRate parsed rate formula
#' @param parsedChoice parsed choice formula
#'
#' @return a list with the two parsed objects joined
#' @noRd
#'
#' @examples
#' joinParsedFormulas(
#'   parseFormula(depNetwork ~ 1 + indeg),
#'   parseFormula(depNetwork ~ inertia + recip)
#' )
joinParsedFormulas <- function(parsedRate, parsedChoice = NULL) {
  if (is.null(parsedChoice)) return(parsedRate)
  
  parsedFormula <- parsedRate
  parsedFormula$rhsNames <- lapply(
    parsedFormula$rhsNames, append, list(type = deparse("ego"))
  )
  parsedFormula$typeParameter <- replace(
    parsedFormula$typeParameter, seq_along(parsedFormula$typeParameter), "ego"
  )

  # keep intercept from rate and depName and default network are the same in both
  elementsToAppend <- names(parsedFormula)[
    !names(parsedFormula) %in% c("depName", "hasIntercept", "defaultNetworkName")
  ]
  for (element in elementsToAppend) {
    parsedFormula[[element]] <- append(
      parsedFormula[[element]],
      parsedChoice[[element]]
    )
  }
  
  return(parsedFormula)
}


adjustPrepInit <- function(
    preprocessingInit, x, parsedformula,
    model, subModel, progress,
    envir = new.env()
) {
  ## PARSE for preprocessingInit: check the formula consistency
  # find the old and new effects indexes, do basic consistency checks
  oldparsedformula <- parseFormula(preprocessingInit$formula, envir = envir)
  effectsindexes <- compareFormulas(
    oldparsedformula = oldparsedformula,
    newparsedformula = parsedformula,
    model = model, subModel = subModel
  )
  # recover the nodesets
  nodesInfo  <- preprocessingInit$nodesInfo
  
  ##  INITIALIZE OBJECTS for preprocessingInit: remove old effects,
  ## add new ones
  # find new effects
  if (min(effectsindexes) == 0) {
    if (progress) cat("Calculating newly added effects.\n")
    newrhsNames <- rhsNames[which(effectsindexes == 0)]
    newWindowParameters <- windowParameters[which(effectsindexes == 0)]
    neweffects <- createEffectsFunctions(
      newrhsNames, model, subModel, envir = PreprocessEnvir)
    
    # test the objects
    # for now it's easier to just reject formulas that have new objects
    # (and therefore possibly new events)
    # otherwise we need to go in the details of orderEvents,
    # eventTime, eventSender, eventReceiver
    # objectspresent <-
    #   rownames(newobjectsEffectsLink) %in% rownames(objectsEffectsLink)
    # if(FALSE %in% objectspresent)
    #  stop("The formula contains new objects or windows that were not taken
    #         into account in the preprocessing used in preprocessingInit.\n
    #       This is likely to affect statistics calculation.
    #       Please recalculate the preprocessed object.")
    
    # Preprocess the new effects
    if (progress) cat("Pre-processing additional effects.\n")
    newprep <- preprocess(
      model,
      subModel,
      newparsedformula,
      # multipleParameter = multipleParameter,
      nodesInfo = nodesInfo,
      opportunitiesList = opportunitiesList,
      startTime = preprocessingInit[["startTime"]],
      endTime = preprocessingInit[["endTime"]],
      rightCensored = rightCensored,
      progress = progress,
      prepEnvir = PreprocessEnvir
    )
    
    # test the length of the dependent and RC updates (in case the events
    #   objects was changed in the environment)
    if (length(preprocessingInit$intervals) != length(newprep$intervals))
      stop(
        "The numbers of dependent events in the formula and in the ",
        "preprocessed object are not consistent.\n",
        "\tPlease check whether these events have changed.",
        call. = FALSE
      )
    
    if (length(preprocessingInit$rightCensoredIntervals) !=
        length(newprep$rightCensoredIntervals))
      stop(
        "The numbers of right-censored events in the formula and in the ",
        "preprocessed object are not consistent.\n",
        "\tPlease check whether some windows have been changed.",
        call. = FALSE  
      )
    
  }
  
  # combine old and new preprocessed objects
  if (progress) cat("Removing no longer required effects.\n")
  allprep <- preprocessingInit
  allprep$initialStats <- array(
    0,
    dim = c(
      nrow(get(nodesInfo[["nodes"]], envir = PreprocessEnvir)),
      nrow(get(nodesInfo[["nodes2"]], envir = PreprocessEnvir)),
      length(effectsindexes)
    )
  )
  allprep$dependentStatsChange <- list()
  allprep$rightCensoredStatsChange <- list()
  cptnew <- 1
  
  # initial stats
  for (e in seq_along(effectsindexes)) {
    if (effectsindexes[e] == 0) {
      allprep$initialStats[, , e] <- newprep$initialStats[, , cptnew]
      cptnew <- cptnew + 1
    }
    if (effectsindexes[e] > 0) {
      allprep$initialStats[, , e] <-
        preprocessingInit$initialStats[, , effectsindexes[e]]
    }
  }
  
  # dependent stats updates
  for (t in seq_along(allprep$intervals)) {
    cptnew <- 1
    allprep$dependentStatsChange[[t]] <-
      lapply(seq_along(effectsindexes), function(x) NULL)
    for (e in seq_along(effectsindexes)) {
      if (effectsindexes[e] == 0) {
        if (!is.null(newprep$dependentStatsChange[[t]][[cptnew]])) {
          allprep$dependentStatsChange[[t]][[e]] <-
            newprep$dependentStatsChange[[t]][[cptnew]]
        }
        cptnew <- cptnew + 1
      }
      if (effectsindexes[e] > 0) {
        if (
          !is.null(
            preprocessingInit$dependentStatsChange[[t]][[effectsindexes[e]]])) {
          allprep$dependentStatsChange[[t]][[e]] <-
            preprocessingInit$dependentStatsChange[[t]][[effectsindexes[e]]]
        }
      }
    }
  }
  
  # right censored stats updates
  if (length(allprep$rightCensoredIntervals) > 0) {
    for (t in seq_along(allprep$rightCensoredIntervals)) {
      cptnew <- 1
      allprep$rightCensoredStatsChange[[t]] <-
        lapply(seq_along(effectsindexes), function(x) NULL)
      for (e in seq_along(effectsindexes)) {
        if (effectsindexes[e] == 0) {
          if (!is.null(newprep$rightCensoredStatsChange[[t]][[cptnew]])) {
            allprep$rightCensoredStatsChange[[t]][[e]] <-
              newprep$rightCensoredStatsChange[[t]][[cptnew]]
          }
          cptnew <- cptnew + 1
        }
        if (effectsindexes[e] > 0) {
          if (
            !is.null(
              preprocessingInit$rightCensoredStatsChange[[t]][[effectsindexes[e]]])) {
            allprep$rightCensoredStatsChange[[t]][[e]] <-
              preprocessingInit$rightCensoredStatsChange[[t]][[effectsindexes[e]]]
          }
        }
      }
    }
  }
  
  prep           <- allprep
  prep$formula   <- x
  prep$model     <- model
  prep$subModel  <- subModel
  prep$nodesInfo <- nodesInfo
  
  return(prep)
}

#' setting preprocessing objects
#'
#' @param parsedFormula an output from [parseFormula()]
#' @param model a character string 
#' @param subModel a character string
#' @param envir the envirronment where the object will be created
#'
#' @return logical `TRUE` is everything is set
#' @noRd
#'
#' @examples
#' initializePreprocessing(parseFormula(depNetwork ~ inertia),
#'                         "DyNAM", "choice")
initializePreprocessing <- function(
    parsedFormula, model, subModel, rightCensored, progress,
    startTime = NULL, endTime = NULL,
    opportunitiesList = NULL,
    groupsNetwork = NULL, envir = parent.frame()
) {
  
  assign("parsedFormula", parsedFormula, envir = envir)
  assign("model", model, envir = envir)
  assign("subModel", subModel, envir = envir)
  assign("startTime", startTime, envir = envir)
  assign("endTime", endTime, envir = envir)
  assign("rightCensored", rightCensored, envir = envir)
  assign("progress", progress, envir = envir)
  assign("opportunitiesList", opportunitiesList, envir = envir)
  assign("groupsNetwork", groupsNetwork, envir = envir)
  # initialize statistics functions from data objects
  # number of actors
  nodesInfo <- setNodesInfo(parsedFormula$depName, envir = envir)
  
  assign("nodesInfo", nodesInfo, envir = envir)
  assign(
    "n1",
    nrow(get(nodesInfo[["nodes"]], envir = envir)),
    envir = envir
  )
  assign(
    "n2",
    nrow(get(nodesInfo[["nodes2"]], envir = envir)),
    envir = envir
  )
  ## INITIALIZE OBJECTS
  # effects info
  effects <- createEffectsFunctions(
    parsedFormula[["rhsNames"]], model, subModel,
    envir = envir
  )
  assign("effects", effects, envir = envir)
  assign("nEffects", length(effects), envir = envir)
  
  assign(
    "objectsEffectsLink",
    getObjectsEffectsLink(parsedFormula[["rhsNames"]]),
    envir = envir
  )
  events <- getEventsAndObjectsLink(
    parsedFormula[["depName"]], parsedFormula[["rhsNames"]],
    envir = envir
  )
  assign("eventsObjectsLink", events[["eventsObjectsLink"]], envir = envir)
  assign("hasCompChange", events[["hasCompChange"]], envir = envir)
  assign("events", events[["events"]], envir = envir)
  
  
  assign(
    "eventsEffectsLink",
    getEventsEffectsLink(
      events[["events"]], parsedFormula[["rhsNames"]],
      events[["eventsObjectsLink"]]
    ),
    envir = envir
  )
  
  # DyNAMi
  assign(
    "groupsNetworkObject",
    if (!is.null(groupsNetwork)) get0(groupsNetwork, envir = envir) else NULL,
    envir = envir
  )
  
  return(TRUE)
}

#' set or check start and end time values
#'
#'   
#' @param envir environment where to eval the expressions and set new variables
#'
#' @return set values in envir
#' @noRd
#'
#' @examples
#' # see preprocessing
setStartEndTime <- function(envir = parent.frame()) {
  local({
    # check start time and end time are valid values, set flags
    hasEndTime <- FALSE
    hasStartTime <- FALSE
    isValidEvent <- TRUE
    
    isWindowEffect <- !vapply(parsedFormula$windowParameters, is.null, logical(1))
    whichEventNoWindowEffect <- eventsEffectsLink[, !isWindowEffect, drop = FALSE]
    whichEventNoWindowEffect <- rowSums(!is.na(whichEventNoWindowEffect))
    # include always dependent events for start and end time
    whichEventNoWindowEffect <- c(1, which(whichEventNoWindowEffect > 0))
    
    # windowed effects modified the events object,
    # therefore not useful for defining start and end time
    eventsRange <- range(vapply(
      events[whichEventNoWindowEffect],
      function(x) range(x$time),
      double(2)
    ))
    
    if (is.null(endTime)) {
      endTime <- eventsRange[2]
      # avoid preprocess events created by window decreasing events
      if (any(isWindowEffect)) hasEndTime <- TRUE
    } else if (endTime != eventsRange[2]) {
      if (!is.numeric(endTime)) endTime <- as.numeric(endTime)
      if (eventsRange[1] > endTime)
        stop("End time smaller than first event time.", call. = FALSE)
      # to solve: if endTime > eventsMax
      # should it produce censored events? warning?
      # add a fake event to the event list
      # endTimeEvent <- data.frame(
      #   time = endTime,
      #   sender = NA,
      #   receiver = NA,
      #   replace = NA
      # )
      # events <- append(events, list(endtime = endTimeEvent))
      hasEndTime <- TRUE
    }
    
    if (is.null(startTime)) {
      startTime <- eventsRange[1]
    } else if (startTime != eventsRange[1]) {
      if (!is.numeric(startTime)) startTime <- as.numeric(startTime)
      if (eventsRange[2] < startTime)
        stop("Start time geater than last event time.", call. = FALSE)
      hasStartTime <- TRUE
      if (eventsRange[1] < startTime) isValidEvent <- FALSE
      # if (eventsMin > startTime) isValidEvent <- TRUE
      # To solve: if startTime < eventsMin should be a warning?
    }
    
    ignoreEvents <- 1L # eventPos should be correct for initialization
  },
  envir = envir
  )
  return(TRUE)
}

#' initialize cache and stat objects
#'
#' @param envir where to eval the code and assign objects
#'
#' @return assign new objects to the environment
#' @noRd
#'
#' @examples
#' # see preprocessing
initStatPrep <- function(envir) {
  local({
    statCache <- initializeCacheStat(
      objectsEffectsLink = objectsEffectsLink,
      effects = effects,
      groupsNetwork = groupsNetworkObject,
      windowParameters = parsedFormula$windowParameters, # Q: DyNAMi
      n1 = n1, n2 = n2,
      model = model, subModel = subModel,
      envir = environment()
    )
    # We put the initial stats to the previous format of 3 dimensional array
    initialStats <- array(
      unlist(lapply(statCache, "[[", "stat")),
      dim = c(n1, n2, nEffects)
    )
    
    statCache <- lapply(statCache, "[[", "cache")
  },
   envir = envir
  )
  return(TRUE)
}

initStoreObj <- function(envir) {
  local({
    # logical values indicating the type of information in events
    isIncrementEvent <- vapply(
      events,
      function(x) "increment" %in% names(x),
      logical(1)
    )
    isNodeEvent <- vapply(events, function(x) "node" %in% names(x), logical(1))
    isCompChange <- isNodeEvent & 
      "present" == eventsObjectsLink$attribute 
    isNodeEvent <- isNodeEvent & !isCompChange
    
    # calculate total of events
    time <- unique(events[[1]]$time)
    if (rightCensored) {
      nRightCensoredEvents <- unique(unlist(
        lapply(events[!isCompChange], function(x) x$time)
      ))
      nTotalEvents  <- as.integer(sum(nRightCensoredEvents <= endTime))
      nRightCensoredEvents <- setdiff(nRightCensoredEvents, time)
      if (length(nRightCensoredEvents) > 1) {
        nRightCensoredEvents <- as.integer(sum(
          nRightCensoredEvents >= startTime &
            nRightCensoredEvents <= endTime) - 1)
      } else nRightCensoredEvents <- 0L
    } else {
      nRightCensoredEvents <- 0L
      nTotalEvents <- as.integer(nrow(events[[1]]))
    }
    
    nDependentEvents <- ifelse(
      hasStartTime || hasEndTime,
      as.integer(sum(time >= startTime & time <= endTime)),
      as.integer(length(time))
    )
    # CHANGED ALVARO: preallocate objects sizes
    dependentStatistics <- vector("list", nDependentEvents)
    timeIntervals <- vector("numeric",
                            ifelse(rightCensored, nDependentEvents, 0))
    rightCensoredStatistics <- vector("list", nRightCensoredEvents)
    timeIntervalsRightCensored <- vector("numeric", nRightCensoredEvents)
    # added a list that tracks the chronological(ordered by time)
    #   order of events between dependent and right-censored events
    # 1 is for dependent and 2 if for right-censored
    # Also a list of the senders and receivers to allow
    # the preprocessingInit routine
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
    if (hasEndTime)
      validPointers <- vapply(
        events, function(x) x$time[1], double(1)
      ) <= endTime
    pointerTempRightCensored <- 1L
    time <- startTime
    interval <- 0L
    # updatesDependent/updatesIntervals: list of 6, each element if NULL
    updatesDependent <- vector("list", nEffects)
    updatesIntervals <- vector("list", nEffects)
    
    # iRightCensored <- 0
    iDependentEvents <- 0L
    iTotalEvents <- 0L
    
  },
  envir = envir)
}

initProgressBar <- function(envir) {
  local({
    if (progress) {
      cat("Preprocessing events.\n", startTime, endTime, nTotalEvents)
      # # how often print, max 50 prints
      pb <- utils::txtProgressBar(max = nTotalEvents, char = "*", style = 3)
      dotEvents <- ifelse(nTotalEvents > 50, ceiling(nTotalEvents / 50), 1)
    }
  },
  envir = envir)
}

getNextEvent <- function(envir) {
  local({
    
  },
  envir = envir)
}

#' update a network from a change array
#'
#' @param stat a matrix with the effect statistics from a network
#' @param change an array with three columns: node1, node2 and replace.
#' Indicating the changes to stat.
#'
#' @return stat after update
#' @noRd
#'
#' @examples
#' updFun(
#'  matrix(0, 5, 5),
#'  cbind(node1 = c(1, 3, 4), node2 = c(2, 1, 5), replace = rep(1, 3))
#' )
updFun <- function(stat, change) {
  if (!is.null(change))
    stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
  return(stat)
}

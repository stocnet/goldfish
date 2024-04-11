# #
# # Author(s): AU
# #
# #
# # Description: Helper functions to gather the preprocess data from a model


#' Gather preprocess data from a formula
#'
#' Gather the preprocess data from a formula and a model,
#' where the output corresponds to the data structure used by the engine
#' `gather_compute`; see [estimate()].
#'
#' It differs from the `estimate()` output when the argument `preprocessingOnly`
#' is set to `TRUE` regarding the memory space requirement.
#' The `gatherPreprocessing()` produces a list where the first element
#' is a matrix that could have up to the number of events times
#' the number of actors rows and the number of effects columns.
#' For medium to large datasets with thousands of events and
#' thousands of actors, the memory RAM requirements are large and,
#' therefore, errors are produced due to a lack of space.
#' The advantage of the data structure is that it can be adapted
#' to estimate the models (or extensions of them) using standard packages
#' for generalized linear models (or any other model)
#' that use tabular data as input.
#'
#' @inheritParams estimate
#'
#' @param formula a formula object that defines at the
#' left-hand side the dependent
#' network (see [defineDependentEvents()]) and at the right-hand side the
#' effects and the variables for which the effects are expected to occur
#' (see `vignette("goldfishEffects")`).
#' @param preprocessArgs a list containing additional parameters
#' for preprocessing. It may contain:
#' \describe{
#'  \item{startTime}{a numerical value or a date-time character with the same
#'  time-zone formatting as the times in event that indicates the starting time
#'  to be considered during estimation.
#'  \emph{Note:} it is only use during preprocessing}
#'  \item{endTime}{a numerical value or a date-time character with the same
#'  time-zone formatting as the times in event that indicates the end time
#'  to be considered during estimation.
#'  \emph{Note:} it is only use during preprocessing}
#'  \item{opportunitiesList}{a list containing for each dependent event
#'   the list of available nodes for the choice model, this list should be
#'   the same length as the dependent events list (ONLY for choice models).}
#' }
#'
#' @return a list object including:
#'  \describe{
#'   \item{stat_all_events}{a matrix. The number of rows can be up to the number
#'    of events times the number of actors
#'    (square number of actors for the REM).
#'    Rigth-censored events are included when the model has an intercept.
#'    The number of columns is the number of effects in the model.
#'    Every row is the effect statistics at the time of the event for each actor
#'    in the choice set or the sender set.}
#'   \item{n_candidates}{
#'    a numeric vector with the number of rows related with an event.
#'    The length correspond to the number of events
#'    plus right censored events if any.}
#'   \item{selected}{a numeric vector with the position of the
#'    selected actor (choice model), sender actor (rate model), or
#'    active dyad (choice-coordination model, REM model).
#'    Indexing start at 1 for each event.}
#'   \item{sender, receiver}{
#'    a character vector with the label of the sender/receiver actor.
#'    For right-censored events the receiver values is not meaningful.}
#'   \item{hasIntercept}{
#'    a logical value indicating if the model has an intercept.}
#'   \item{namesEffects}{a character vector with a short name of the effect.
#'   It includes the name of the object used to calculate the effects and
#'   modifiers of the effect, e.g., the type of effect, weighted effect.}
#'   \item{effectDescription}{
#'    a character matrix with the description of the effects.
#'    It includes the name of the object used to calculate the effects and
#'    additional information of the effect, e.g., the type of effect,
#'    weighted effect, transformation function, window length.}
#'  }
#'  If the model has an intercept and the subModel is `rate` or model is `REM`,
#'  additional elements are included:
#'  \describe{
#'   \item{timespan}{
#'    a numeric vector with the time span between events,
#'    including right-censored events.}
#'   \item{isDependent}{
#'    a logical vector indicating if the event is dependent or right-censored.}
#'  }
#'
#' @export
#'
#' @examples
#' data("Fisheries_Treaties_6070")
#' states <- defineNodes(states)
#' states <- linkEvents(states, sovchanges, attribute = "present")
#' states <- linkEvents(states, regchanges, attribute = "regime")
#' states <- linkEvents(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- linkEvents(bilatnet, bilatchanges, nodes = states)
#'
#' createBilat <- defineDependentEvents(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, defaultNetwork = bilatnet
#' )
#'
#' contignet <- defineNetwork(contignet, nodes = states, directed = FALSE)
#' contignet <- linkEvents(contignet, contigchanges, nodes = states)
#'
#' gatheredData <- GatherPreprocessing(
#'   createBilat ~ inertia(bilatnet) + trans(bilatnet) + tie(contignet)
#' )
#'
GatherPreprocessing <- function(
    formula,
    model = c("DyNAM", "REM"),
    subModel = c("choice", "choice_coordination", "rate"),
    preprocessArgs = NULL,
    progress = getOption("progress"),
    envir = new.env()) {
  model <- match.arg(
    arg = if (length(model) > 1) model[1] else model,
    choices = c("DyNAM", "REM", "DyNAMRE")
  )
  subModel <- match.arg(subModel)

  checkModelPar(
    model = model, subModel = subModel,
    modelList = c("DyNAM", "REM", "DyNAMRE"),
    subModelList = list(
      DyNAM = c("choice", "rate", "choice_coordination"),
      REM = "choice",
      DyNAMRE = c("choice", "choice_coordination")
    )
  )

  if (!is.null(preprocessArgs)) {
    parInit <- names(preprocessArgs) %in%
      c(
        "startTime", "endTime", "opportunitiesList"
      )

    if (any(!parInit)) {
      warning(
        "The parameter: ",
        paste(names(preprocessArgs)[!parInit], collapse = ", "),
        " is not recognized for the preprocessing. ",
        "See the documentation for the list of available parameters",
        call. = FALSE, immediate. = TRUE
      )
    }

    if (!is.null(preprocessArgs[["opportunitiesList"]])) {
      warning(
        dQuote("GatherPreprocessing"), " doesn't implement yet the ",
        dQuote("opportunitiesList"), " functionality"
      )
    }
  }

  if (is.null(progress)) progress <- FALSE

  ### 1. PARSE the formula----
  parsedformula <- parseFormula(formula, envir = envir)
  rhsNames <- parsedformula$rhsNames
  depName <- parsedformula$depName
  hasIntercept <- parsedformula$hasIntercept
  windowParameters <- parsedformula$windowParameters
  ignoreRepParameter <- unlist(parsedformula$ignoreRepParameter)

  # # C implementation doesn't have ignoreRep option issue #105
  if (any(unlist(parsedformula$ignoreRepParameter))) {
    stop("gatherPreprocessing ",
      " doesn't support ignoreRep effects (GH issue #105)!",
      call. = FALSE, immediate. = TRUE
    )
  }

  # Model-specific preprocessing initialization
  if (model == "DyNAMRE") {
    if (subModel == "choice") model <- "REM" else model <- "DyNAM"
    altModel <- "DyNAMRE"
  } else {
    altModel <- NULL
  }

  if (model %in% c("DyNAM", "DyNAMi") &&
    subModel %in% c("choice", "choice_coordination") &&
    parsedformula$hasIntercept) {
    warning("Model ", dQuote(model), " subModel ", dQuote(subModel),
      " ignores the time intercept.",
      call. = FALSE, immediate. = TRUE
    )
    parsedformula$hasIntercept <- FALSE
  }
  rightCensored <- parsedformula$hasIntercept

  # if (progress && !all(vapply(windowParameters, is.null, logical(1)))) {
  #   cat("Creating window objects in global environment.\n")
  # }

  ### 2. INITIALIZE OBJECTS: effects, nodes, and link objects----

  if (progress) cat("Initializing objects.\n")

  ## 2.0 Set isTwoMode to define effects functions
  # get node sets of dependent variable
  .nodes <- attr(get(parsedformula$depName, envir = envir), "nodes")

  # two-mode networks(2 kinds of nodes)
  if (length(.nodes) == 2) {
    .nodes2 <- .nodes[2]
    .nodes <- .nodes[1]
    isTwoMode <- TRUE
  } else {
    .nodes2 <- .nodes
    isTwoMode <- FALSE
  }

  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessingInit or not
  # enviroment from which get the objects
  effects <- createEffectsFunctions(
    parsedformula$rhsNames, model, subModel,
    envir = envir
  )
  # Get links between objects and effects for printing results
  objectsEffectsLink <- getObjectsEffectsLink(parsedformula$rhsNames)

  ## 2.2 INITIALIZE OBJECTS for preprocessingInit == NULL

  # Initialize events list and link to objects
  events <- getEventsAndObjectsLink(
    parsedformula$depName, parsedformula$rhsNames,
    .nodes, .nodes2,
    envir = envir
  )[[1]]
  # moved cleanInteractionEvents in getEventsAndObjectsLink
  eventsObjectsLink <- getEventsAndObjectsLink(
    parsedformula$depName, parsedformula$rhsNames,
    .nodes, .nodes2,
    envir = envir
  )[[2]]
  eventsEffectsLink <- getEventsEffectsLink(
    events, parsedformula$rhsNames, eventsObjectsLink
  )

  ## 3.2 PREPROCESS when preprocessingInit == NULL
  preprocessingStat <- preprocess(
    model = model,
    subModel = subModel,
    events = events,
    effects = effects,
    windowParameters = parsedformula$windowParameters,
    ignoreRepParameter = ignoreRepParameter,
    eventsObjectsLink = eventsObjectsLink, # for data update
    eventsEffectsLink = eventsEffectsLink,
    objectsEffectsLink = objectsEffectsLink, # for parameterization
    # multipleParameter = multipleParameter,
    nodes = .nodes,
    nodes2 = .nodes2,
    isTwoMode = isTwoMode,
    startTime = preprocessArgs[["startTime"]],
    endTime = preprocessArgs[["endTime"]],
    rightCensored = rightCensored,
    progress = progress,
    prepEnvir = envir
  )

  # # 3.3 additional processing to flat array objects
  allowReflexive <- isTwoMode

  reduceMatrixToVector <- FALSE
  reduceArrayToMatrix <- FALSE

  if (!is.null(altModel) && subModel == "choice") model <- "DyNAM"

  if (model == "REM") {
    if (!parsedformula$hasIntercept) {
      modelTypeCall <- "REM-ordered"
    } else {
      modelTypeCall <- "REM"
    }
  } else if (model == "DyNAM") {
    if (subModel == "rate" && !parsedformula$hasIntercept) {
      modelTypeCall <- "DyNAM-M-Rate-ordered"
      reduceMatrixToVector <- TRUE
    } else if (subModel == "rate") {
      modelTypeCall <- "DyNAM-M-Rate"
      reduceMatrixToVector <- TRUE
    } else if (subModel == "choice_coordination") {
      modelTypeCall <- "DyNAM-MM"
    } else {
      modelTypeCall <- "DyNAM-M"
      reduceArrayToMatrix <- TRUE
    }
  }

  # from estimate_c_init
  preprocessingStat <- modifyStatisticsList(
    preprocessingStat, modelTypeCall,
    reduceMatrixToVector = reduceMatrixToVector,
    reduceArrayToMatrix = reduceArrayToMatrix,
    excludeParameters = NULL,
    addInterceptEffect = parsedformula$hasIntercept
  )

  # nEvents <- length(preprocessingStat$orderEvents)# number of events
  nodes <- get(.nodes, envir = envir)
  nodes2 <- get(.nodes2, envir = envir)

  ## SET VARIABLES BASED ON STATSLIST
  twomode_or_reflexive <- (allowReflexive || isTwoMode)
  # n_events <- length(preprocessingStat$orderEvents)
  dimensions <- dim(preprocessingStat$initialStats)
  n_parameters <- dimensions[3]
  n_actors1 <- dimensions[1]
  n_actors2 <- dimensions[2]

  ## CONVERT UPDATES INTO THE FORMAT ACCEPTED BY C FUNCTIONS
  temp <- convert_change(preprocessingStat$dependentStatsChange)
  stat_mat_update <- temp$statMatUpdate
  stat_mat_update_pointer <- temp$statMatUpdatePointer
  if (parsedformula$hasIntercept) {
    stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
  }
  # Convert the right-censored events
  # which will be a zero matrice and a zero vector
  # if there's no right-censored event
  if (length(preprocessingStat$rightCensoredIntervals) == 0) {
    stat_mat_rightcensored_update <- matrix(0, 4, 1)
    stat_mat_rightcensored_update_pointer <- numeric(1)
  } else {
    temp <- convert_change(preprocessingStat$rightCensoredStatsChange)
    stat_mat_rightcensored_update <- temp$statMatUpdate
    stat_mat_rightcensored_update_pointer <- temp$statMatUpdatePointer
    if (parsedformula$hasIntercept) {
      stat_mat_rightcensored_update[3, ] <-
        stat_mat_rightcensored_update[3, ] + 1
    }
  }

  ## CONVERT COMPOSITION CHANGES INTO THE FORMAT ACCEPTED BY C FUNCTIONS
  compChangeName1 <- attr(nodes, "events")[
    "present" == attr(nodes, "dynamicAttribute")
  ]
  hasCompChange1 <- !is.null(compChangeName1) && length(compChangeName1) > 0

  compChangeName2 <- attr(nodes2, "events")[
    "present" == attr(nodes2, "dynamicAttribute")
  ]
  hasCompChange2 <- !is.null(compChangeName2) && length(compChangeName2) > 0

  if (hasCompChange1) {
    temp <- get(compChangeName1, envir = envir)
    temp <- sanitizeEvents(temp, nodes, envir = envir)
    temp <- C_convert_composition_change(temp, preprocessingStat$eventTime)
    presence1_update <- temp$presenceUpdate
    presence1_update_pointer <- temp$presenceUpdatePointer
  } else {
    presence1_update <- matrix(0, 0, 0)
    presence1_update_pointer <- numeric(1)
  }

  if (hasCompChange2) {
    temp <- get(compChangeName2, envir = envir)
    temp <- sanitizeEvents(temp, nodes2, envir = envir)
    temp <- C_convert_composition_change(temp, preprocessingStat$eventTime)
    presence2_update <- temp$presenceUpdate
    presence2_update_pointer <- temp$presenceUpdatePointer
  } else {
    presence2_update <- matrix(0, 0, 0)
    presence2_update_pointer <- numeric(1)
  }

  if (!is.null(nodes$present)) {
    presence1_init <- nodes$present
  } else {
    presence1_init <- rep(TRUE, nrow(nodes))
  }

  if (!is.null(nodes2$present)) {
    presence2_init <- nodes2$present
  } else {
    presence2_init <- rep(TRUE, nrow(nodes2))
  }

  ## CONVERT TYPES OF EVENTS AND TIMESPANS INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  if (modelTypeCall %in% c("DyNAM-M-Rate", "REM", "DyNAM-MM")) {
    is_dependent <- preprocessingStat$orderEvents == 1
    timespan <- numeric(length(is_dependent))
    if (modelTypeCall != "DyNAM-MM") {
      timespan[is_dependent] <- preprocessingStat$intervals
      timespan[(!is_dependent)] <- preprocessingStat$rightCensoredIntervals
    }
  } else {
    timespan <- NA
  }

  ## CONVERT INFOS OF SENDERS AND RECEIVERS INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  event_mat <- rbind(
    preprocessingStat$eventSender, preprocessingStat$eventReceiver
  )

  ## CONVERT THE INITIALIZATION OF DATA MATRIX INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  stat_mat_init <- matrix(0, n_actors1 * n_actors2, n_parameters)
  for (i in 1:n_parameters) {
    stat_mat_init[, i] <- t(preprocessingStat$initialStats[, , i])
  }

  gatheredData <- gather_(
    modelTypeCall = modelTypeCall,
    event_mat = event_mat,
    timespan = timespan,
    is_dependent = is_dependent,
    stat_mat_init = stat_mat_init,
    stat_mat_update = stat_mat_update,
    stat_mat_update_pointer = stat_mat_update_pointer,
    stat_mat_rightcensored_update = stat_mat_rightcensored_update,
    stat_mat_rightcensored_update_pointer =
      stat_mat_rightcensored_update_pointer,
    presence1_init = presence1_init,
    presence1_update = presence1_update,
    presence1_update_pointer = presence1_update_pointer,
    presence2_init = presence2_init,
    presence2_update = presence2_update,
    presence2_update_pointer = presence2_update_pointer,
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    twomode_or_reflexive = twomode_or_reflexive,
    verbose = progress, # If not silent, output the progress of data gathering
    impute = FALSE
  )

  ## Add additional information
  gatheredData$sender <- nodes$label[preprocessingStat$eventSender]
  if (model == "REM" || (model == "DyNAM" && subModel != "rate")) {
    gatheredData$receiver <-
      nodes2$label[preprocessingStat$eventReceiver]
  } else if (model == "DyNAM" && subModel == "rate" &&
    parsedformula$hasIntercept) {
    gatheredData$timespan <- timespan
    gatheredData$isDependent <- is_dependent
  }
  gatheredData$hasIntercept <- parsedformula$hasIntercept

  gatheredData$selected <- gatheredData$selected +
    if (parsedformula$hasIntercept) (1 * is_dependent) else 1

  ### 4. PREPARE PRINTING----
  # functions_utility.R
  effectDescription <-
    GetDetailPrint(objectsEffectsLink, parsedformula)
  hasWindows <- attr(effectDescription, "hasWindows")

  namesEffects <- CreateNames(effectDescription, sep = "_", joiner = "_")

  gatheredData$namesEffects <- namesEffects
  colnames(gatheredData$stat_all_events) <- namesEffects
  gatheredData$effectDescription <- effectDescription

  return(gatheredData)
}

#' Generate names for statistics effects
#'
#' Using the names data frame from `goldfish` generate compact names to the
#' columns for data frame or matrix
#'
#' @param names data frame from `goldfish`
#' @param sep string. Separator between different arguments and objects
#' @param joiner string. Separator to join multiple object names
#'
#' @return a string vector with the names.
#' @noRd
#'
#' @examples
#' names <- cbind(
#'   Object = c("bilatnet", "bilatnet", "contignet"),
#'   Weighted = c("W", "", "W")
#' )
#' rownames(names) <- c("inertia", "trans", "tie")
#' CreateNames(names, sep = "|")
CreateNames <- function(
    names, sep = " ", joiner = ", ") {
  isObjectD <- grepl("Object \\d+", colnames(names))
  if (any(isObjectD)) {
    object <- apply(
      names[, isObjectD], 1,
      function(z) {
        ret <- Filter(function(w) !is.na(w) & w != "", z)
        ret <- paste(ret, collapse = joiner)
        return(ret)
      }
    )
    newNames <- c("Object", colnames(names)[!isObjectD])
    names <- cbind(object, names[, !isObjectD])
    colnames(names) <- newNames
  }

  if ("fixed" %in% colnames(names)) {
    names[, "fixed"] <- ifelse(names[, "fixed"] == "TRUE", "Fx", "")
  }

  names <- cbind(effect = rownames(names), names)
  nombres <- apply(
    names, 1,
    function(z) {
      ret <- Filter(function(w) !is.na(w) & w != "", z)
      ret <- paste(ret, collapse = sep)
      return(ret)
    }
  )
  names(nombres) <- NULL

  return(nombres)
}

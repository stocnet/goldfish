

getSenderWaitingTime_DyNAM_rate <- function(
                                            formula, coeff, modelType = "DyNAM-M-Rate", # sender,

                                            processState, seed, silent = FALSE, verbose = FALSE) {
  # # steps
  # 0. check parameters classes
  # 1. parsing formula: add intercept if not?
  # 2. calculate statistics from effect function given the state: is the state the network
  # ToDo: consider parameter to set initial time and preprocess until that point as initial state
  #       create auxiliar function to calculate statistics from effect function in preprocessing

  modelType == "DyNAM-M-Rate"

  # # 0. check
  stopifnot(
    inherits(formula, "formula"),
    inherits(processState, "preprocessed.goldfish"),
    inherits(coeff, "numeric"),
    modelType != "DyNAM-M-Rate"
  ) # # ToDo: adjust, more informative

  # # 1. PARSE the formula
  if (!silent) cat("Parsing formula.\n")

  # Parse and check the validity of the formula
  # TODO: maybe some steps are not necessary when we skip the preprocessing (preprocessingInit)
  parsedformula <- parseFormula(formula)
  rhsNames <- parsedformula$rhsNames
  depName <- parsedformula$depName
  hasIntercept <- parsedformula$hasIntercept
  defaultNetworkName <- parsedformula$defaultNetworkName
  windowParameters <- parsedformula$windowParameters
  ignoreRepParameter <- parsedformula$ignoreRepParameter
  weightedParameter <- parsedformula$weightedParameter
  userSetParameter <- parsedformula$userSetParameter

  if (verbose) cat(ifelse(hasIntercept, "T", "No time intercept added.\n"))
  if (!silent && !all(sapply(windowParameters, is.null))) {
    cat("Creating window objects in global environment.\n")
  }

  # 1.2. INITIALIZE effects, nodes, and link objects
  if (!silent) cat("Initializing objects.\n")

  effects <- createEffectsFunctions(rhsNames)

  # get node sets of dependent variable
  nodes <- attr(get(depName), "nodes")
  isTwoMode <- F
  # two-mode networks(2 kinds of nodes)
  if (length(nodes) == 2) {
    nodes2 <- nodes[2]
    nodes <- nodes[1]
    isTwoMode <- T
  } else {
    nodes2 <- nodes
  }

  # Initialize events list and link to objects
  events <- getEventsAndObjectsLink(depName, rhsNames, nodes, nodes2,
    envir = environment()
  )[[1]]
  eventsObjectsLink <- getEventsAndObjectsLink(depName, rhsNames, nodes, nodes2,
    envir = environment()
  )[[2]]
  eventsEffectsLink <- getEventsEffectsLink(events, rhsNames, eventsObjectsLink)
  objectsEffectsLink <- getObjectsEffectsLink(rhsNames, effects)

  # Model-specific preprocessing initialization
  # if(modelType %in% c("DyNAM-M", "DyNAM-MM") && hasIntercept){
  #   warning(paste("Model type", modelType, "ignores the time intercept."), call. = F)
  #   hasIntercept <- F
  # }
  rightCensored <- hasIntercept

  rowOnly <- colOnly <- F
  if (modelType == "DyNAM-M-Rate") colOnly <- T
  # if (modelType == "DyNAM-M") rowOnly <- T

  # # 2. calculate statistics from effect function given the state
}


getReceiverProbability_DyNAM_choice <- function(
                                                formula, coeff, modelType = "DyNAM-M", # sender,

                                                processState, seed, silent = FALSE, verbose = FALSE) {
  # # steps
  # 0. check parameters classes
  # 1. parsing formula: add intercept if not?

  # # 0. check
  stopifnot(
    inherits(formula, "formula"),
    inherits(processState, "preprocessed.goldfish"),
    inherits(coeff, "numeric"),
    modelType != "DyNAM-M"
  ) # # ToDo: adjust, more informative

  # # 1. PARSE the formula
  if (!silent) cat("Parsing formula.\n")

  # Parse and check the validity of the formula
  # TODO: maybe some steps are not necessary when we skip the preprocessing (preprocessingInit)
  parsedformula <- parseFormula(model)
  rhsNames <- parsedformula$rhsNames
  depName <- parsedformula$depName
  hasIntercept <- parsedformula$hasIntercept
  defaultNetworkName <- parsedformula$defaultNetworkName
  windowParameters <- parsedformula$windowParameters
  ignoreRepParameter <- parsedformula$ignoreRepParameter
  weightedParameter <- parsedformula$weightedParameter
  userSetParameter <- parsedformula$userSetParameter

  if (verbose) cat(ifelse(hasIntercept, "T", "No time intercept added.\n"))
  if (!silent && !all(sapply(windowParameters, is.null))) {
    cat("Creating window objects in global environment.\n")
  }

  # 1.2. INITIALIZE effects, nodes, and link objects
  if (!silent) cat("Initializing objects.\n")

  effects <- createEffectsFunctions(rhsNames)

  # get node sets of dependent variable
  nodes <- attr(get(depName), "nodes")
  isTwoMode <- F
  # two-mode networks(2 kinds of nodes)
  if (length(nodes) == 2) {
    nodes2 <- nodes[2]
    nodes <- nodes[1]
    isTwoMode <- T
  } else {
    nodes2 <- nodes
  }

  # Initialize events list and link to objects
  events <- getEventsAndObjectsLink(depName, rhsNames, nodes, nodes2,
    envir = environment()
  )[[1]]
  eventsObjectsLink <- getEventsAndObjectsLink(depName, rhsNames, nodes, nodes2,
    envir = environment()
  )[[2]]
  eventsEffectsLink <- getEventsEffectsLink(events, rhsNames, eventsObjectsLink)
  objectsEffectsLink <- getObjectsEffectsLink(rhsNames, effects)

  # Model-specific preprocessing initialization
  if (modelType %in% c("DyNAM-M", "DyNAM-MM") && hasIntercept) {
    warning("Model type ", modelType, " ignores the time intercept.", call. = FALSE, immediate. = TRUE)
    hasIntercept <- F
  }
  rightCensored <- hasIntercept

  rowOnly <- colOnly <- F
  # if (modelType == "DyNAM-M-Rate") colOnly <- T
  if (modelType == "DyNAM-M") rowOnly <- T

  # # 2. calculate statistics from effect function given the state
}

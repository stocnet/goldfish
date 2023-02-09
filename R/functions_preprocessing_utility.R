

#' Get compositional events
#'
#' 
#' @param isTwoMode 
#' @param nodes 
#' @param nodes2 
#' @param envir 
#'
#' @return a list of length two. The events data frames with the compositional
#' changes 
#' @noRd
#'
#' @examples
#' GetCompositionChange(FALSE, "actorsEx")
#' GetCompositionChange(TRUE, "actorsEx", "clubsEx")
GetCompositionChange <- function(isTwoMode, nodes, nodes2 = NULL, envir = new.env()) {
  attributesNodes <- attributes(get(nodes, envir = envir))
  if ("present" %in% attributesNodes[["dynamicAttributes"]]) {
    compChangeName1 <-  attributesNodes[["events"]][
      "present" == attributesNodes[["dynamicAttributes"]]
    ]
    compChange1 <- get(compChangeName1, envir = envir)
  } else {
    compChange1 <- NULL  
  }
  
  if (isTwoMode) {
    attributesNodes <- attributes(get(nodes2, envir = envir))
    if ("present" %in% attributesNodes[["dynamicAttributes"]]) {
      compChangeName2 <-  attributesNodes[["events"]][
        "present" == attributesNodes[["dynamicAttributes"]]
      ]
      compChange2 <- get(compChangeName2, envir = envir)
    } else {
      compChange1 <- NULL  
    }  
  } else {
    compChange2 <- NULL
  }
  
  return(list(compChange1 = compChange1, compChange2 = compChange2))
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
#' JoinParsedFormulas(
#'   parseFormula(depNetwork ~ 1 + indeg),
#'   parseFormula(depNetwork ~ inertia + recip)
#' )
JoinParsedFormulas <- function(parsedRate, parsedChoice = NULL) {
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


AdjustPrepInit <- function(
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

#' Simulate a sequence of events
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Experimental version of the simulate functionality.
#' Current version **only** simulate endogenous events for a DyNAM model
#' with rate and choice submodel specifications.
#' It's restricted to simulate a fix length sequence,
#' oppose to the general case of simulate events until end time is reached.
#'
#' @inheritParams estimate
#' @param formulaRate a formula as define in \code{\link{estimate}} with the
#' effects for the rate sub-model \code{subModel = "rate"}.
#' @param parameterRate a numeric vector with the numerical values that
#' effects parameters on \code{formulaRate} should take during simulation.
#' @param formulaChoice a formula as define in \code{\link{estimate}} with the
#' effects for the choice sub-model \code{subModel = "choice"}.
#' When \code{model = "REM"} this formula is not required.
#' @param parameterChoice a numeric vector with the numerical values that
#' effects parameters on \code{formulaChoice} should take during simulation.
#' @param nEvents integer with the number of events to simulate from
#' the given formulas and parameter vectors. Default to \code{100}.
#' 
#' @export
#' 
#' @examples
#' 
#' 
#' 
simulate <- function(formulaRate,
                     parameterRate,
                     formulaChoice = NULL,
                     parameterChoice = NULL,
                     model = c("DyNAM", "REM"),
                     subModel = c("choice", "choice_coordination"),
                     # estimationInit = NULL,
                     # preprocessingInit = NULL,
                     # preprocessingOnly = FALSE,
                     verbose = FALSE,
                     silent = FALSE,
                     debug = FALSE,
                     nEvents = 100) {
  UseMethod("simulate", formulaRate)
}


# First estimation from a formula: can return either 
# a preprocessed object or a result object
#' @export
simulate.formula <- function(formulaRate,
                             parameterRate,
                             formulaChoice = NULL,
                             parameterChoice = NULL,
                             model,
                             subModel,
                             # estimationInit = NULL,
                             # preprocessingInit = NULL,
                             # preprocessingOnly = FALSE,
                             verbose = FALSE,
                             silent = FALSE,
                             debug = FALSE,
                             nEvents = 100) {

  # CHECK THE INPUT
  if (subModel == "choice_coordination")
    stop(
      "It doesn't support simulating a DyNAM choice coordination model.\n",
      "Since the generating process for the waiting time is not specified",
      call. = FALSE)

  # PARSE THE FORMULA

  ## 1.1 PARSE for all cases: preprocessingInit or not
  parsedformulaRate <- parseFormula(formulaRate, model, subModel)
  rhsNamesRate <- parsedformulaRate$rhsNames
  depNameRate <- parsedformulaRate$depName
  hasInterceptRate <- parsedformulaRate$hasIntercept
  defaultNetworkNameRate <- parsedformulaRate$defaultNetworkName
  # The number of the independent variables should be the length 
  # of the input parameter vector
  if (length(rhsNamesRate) + hasInterceptRate != length(parameterRate))
    stop(
      "The number of independent effects should be the same",
      " as the length of the input parameter vector:",
      format(formulaRate), " with parameter ",
      paste(parameterRate, collapse = ",", sep = ""),
      call. = FALSE
    )

  if (!is.null(formulaChoice)) {
    if (!(model == "DyNAM" && subModel == "choice"))
      stop(
        "The model you specified doesn't require a formula",
        "for the choice subModel",
        call. = FALSE)

    ## 1.1 PARSE for all cases: preprocessingInit or not
    parsedformulaChoice <- parseFormula(formulaChoice, model, subModel)
    rhsNamesChoice <- parsedformulaChoice$rhsNames
    # depNameChoice <- parsedformulaChoice$depName
    # defaultNetworkNameChoice <- parsedformulaChoice$defaultNetworkName
    if (parsedformulaChoice$hasIntercept)
      # In the DyNAM choice model,
      # the intercept will be cancelled and hence useless.
      stop("Intercept in the choice subModel model will be ignored.",
           " Please remove the intercep and run again.", call. = FALSE)

    if (length(rhsNamesChoice) != length(parameterChoice))
      stop(
        "The number of the independent effects should be the same",
        " as the length of the input parameter:",
        format(formulaChoice), " with parameter ",
        paste(parameterChoice, collapse = ","),
        call. = FALSE
      )
  }

  # CHECK THE INPUT FORMULA
  # If there's no intercept in the formula for the waiting-time generating process (For example, DyNAM choice, or REM),
  # we take the coefficient of the intercept as 0.
  if (!hasInterceptRate) {
    cat("\n", "You didn't specify an intercept in the first formula so we take the intercept as 0.", "\n")
    parameterRate <- c(0, parameterRate)
  }



  # get node sets of dependent variable
  nodes <- attr(get(depNameRate), "nodes")
  isTwoMode <- FALSE

  # two-mode networks(2 kinds of nodes)
  if (length(nodes) == 2) {
    nodes2 <- nodes[2]
    nodes <- nodes[1]
    isTwoMode <- TRUE
  } else {
    nodes2 <- nodes
  }



  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessingInit or not

  # enviroment from which get the objects
  envir <- environment()

  # effect and objectsEffectsLink for sender-deciding process
  effectsRate <- createEffectsFunctions(rhsNamesRate,
                                        model, subModel, envir = envir)
  objectsEffectsLinkRate <- getObjectsEffectsLink(rhsNamesRate)

  # effect and objectsEffectsLink for receiver-deciding process
  effectsChoice <- NULL
  objectsEffectsLinkChoice <- NULL
  if (!is.null(formulaChoice)) {
    effectsChoice <- createEffectsFunctions(rhsNamesChoice,
                                            model, subModel, envir = envir)
    objectsEffectsLinkChoice <- getObjectsEffectsLink(rhsNamesChoice)
  }





  # Simulating!
  if (!silent) cat("Starting simulation\n")
  events <- simulate_engine(
    model,
    subModel,
    parameter = parameterRate,
    effects = effectsRate,
    objectsEffectsLink = objectsEffectsLinkRate, # for parameterization
    parameterChoice = parameterChoice,
    effectsChoice = effectsChoice,
    objectsEffectsLinkChoice = objectsEffectsLinkChoice,
    nodes = nodes,
    nodes2 = nodes2,
    isTwoMode = isTwoMode,
    rightCensored = FALSE, # ToDo: check
    verbose = verbose,
    silent = silent,
    nEvents = nEvents
  )

  # Styling the result
  events <- data.frame(time = events[, 1], sender = as.character(actors$label[events[, 2]]), receiver = as.character(actors$label[events[, 3]]), increment = events[, 4], stringsAsFactors = FALSE)

  return(events)
}

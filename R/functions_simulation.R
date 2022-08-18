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
#' @param parametersRate a numeric vector with the numerical values that
#' effects parameters on \code{formulaRate} should take during simulation.
#' @param formulaChoice a formula as define in \code{\link{estimate}} with the
#' effects for the choice sub-model \code{subModel = "choice"}.
#' When \code{model = "REM"} this formula is not required.
#' @param parametersChoice a numeric vector with the numerical values that
#' effects parameters on \code{formulaChoice} should take during simulation.
#' @param nEvents integer with the number of events to simulate from
#' the given formulas and parameter vectors. Default to \code{100}.
#' 
#' @export
#' @importFrom lifecycle badge
#' 
#' @examples
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls,
#'                           nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors,
#'                                         defaultNetwork = callNetwork)
#' 
#' simulateEvents <- simulate(
#'   formulaRate = callsDependent ~ 1 + indeg + outdeg,
#'   parametersRate = c(-14, 0.76, 0.25),
#'   formulaChoice = callsDependent ~ inertia + trans + recip + indeg,
#'   parametersChoice = c(5.3, -0.05, 1.4, -0.16),
#'   model = "DyNAM", subModel = "choice",
#'   nEvents = 100
#' )
#' 
simulate <- function(formulaRate,
                     parametersRate,
                     formulaChoice = NULL,
                     parametersChoice = NULL,
                     model = c("DyNAM", "REM"),
                     subModel = c("choice", "choice_coordination"),
                     progress = getOption("progress"),
                     nEvents = 100) {
  UseMethod("simulate", formulaRate)
}


# First estimation from a formula: can return either 
# a preprocessed object or a result object
#' @export
simulate.formula <- function(formulaRate,
                             parametersRate,
                             formulaChoice = NULL,
                             parametersChoice = NULL,
                             model = c("DyNAM", "REM"),
                             subModel = c("choice", "rate"),
                             progress = getOption("progress"),
                             nEvents = 100) {

  # CHECK INPUT
  model <- match.arg(model)
  subModel <- match.arg(subModel)

  ### check model and subModel
  checkModelPar(model, subModel,
                modelList = c("DyNAM", "REM", "DyNAMi", "TriNAM"),
                subModelList = list(
                  DyNAM = c("choice", "rate", "choice_coordination"),
                  REM = "choice",
                  DyNAMi = c("choice", "rate"),
                  TriNAM = c("choice", "rate")
                )
  )

  if (subModel == "choice_coordination")
    stop(
      "It doesn't support simulating a DyNAM choice coordination model.\n",
      "Since the generating process for the waiting time is not specified",
      call. = FALSE)
  
  stopifnot(
    inherits(formulaRate, "formula"),
    is.null(formulaChoice) || inherits(formulaChoice, "formula"),
    inherits(parametersRate, "numeric"),
    is.null(parametersChoice) || inherits(parametersChoice, "numeric"),
    is.null(progress) || inherits(progress, "logical"),
    inherits(nEvents, "numeric") && nEvents > 0
  )
  
  if (is.null(progress)) progress <- FALSE

  ## 1.1 Preparing
  parsedformulaRate <- parseFormula(formulaRate)
  
  # The number of the independent variables should be the length 
  # of the input parameter vector
  if (length(parsedformulaRate$rhsNames) +
      parsedformulaRate$hasIntercept !=
      length(parametersRate))
    stop(
      "The number of independent effects should be the same",
      " as the length of the input parameter vector:",
      format(formulaRate), " with parameter ",
      paste(parametersRate, collapse = ",", sep = ""),
      call. = FALSE
    )

  if (!is.null(formulaChoice)) {
    if (!(model == "DyNAM" && subModel == "choice"))
      stop(
        "The model you specified doesn't require a formula",
        "for the choice subModel",
        call. = FALSE)

    ## 1.1 PARSE for all cases: preprocessingInit or not
    parsedformulaChoice <- parseFormula(formulaChoice)
    if (parsedformulaChoice$hasIntercept)
      # In the DyNAM choice model,
      # the intercept will be cancelled and hence useless.
      stop("Intercept in the choice subModel model will be ignored.",
           " Please remove the intercep and run again.", call. = FALSE)

    if (length(parsedformulaChoice$rhsNames) !=
        length(parametersChoice))
      stop(
        "The number of the independent effects should be the same",
        " as the length of the input parameter:",
        format(formulaChoice), " with parameter ",
        paste(parametersChoice, collapse = ","),
        call. = FALSE
      )
    
    if (parsedformulaRate$depName != parsedformulaChoice$depName)
      stop("formula for rate and choice submodels",
           " must be defined over the same dependent event object",
           call. = FALSE)
  } else {
    parsedformulaChoice <- NULL
  }

  # CHECK THE INPUT FORMULA
  # There must exist the intercept in the formula for the waiting-time
  # generating process (For example, DyNAM rate, or REM),
  if (!parsedformulaRate$hasIntercept)
    stop("You didn't specify an intercept in the rate formula.",
         "\n\tCurrent implementation requires intercept and",
         " a positive parameter value for it.",
         call. = FALSE)

  # get node sets of dependent variable
  nodes <- attr(get(parsedformulaRate$depName), "nodes")
  isTwoMode <- FALSE
  
  # two-mode networks(2 kinds of nodes)
  if (length(nodes) == 2) {
    nodes2 <- nodes[2]
    nodes <- nodes[1]
    isTwoMode <- TRUE
  } else {
    nodes2 <- nodes
  }
  # Simulating!
  if (progress) cat("Starting simulation\n")
  events <- simulate_engine(
    model = model,
    subModel = subModel,
    parametersRate = parametersRate,
    parsedformulaRate = parsedformulaRate,
    parametersChoice = parametersChoice,
    parsedformulaChoice = parsedformulaChoice,
    nEvents = nEvents,
    nodes = nodes,
    nodes2 = nodes2,
    isTwoMode = isTwoMode,
    startTime = 0,
    endTime = NULL,
    rightCensored = FALSE, # ToDo: check
    progress = progress
  )

  nodes <- get(nodes, envir = environment())
  nodes2 <- get(nodes2, envir = environment())
  # Styling the result
  events <- data.frame(
    time = events[, 1],
    sender = as.character(nodes$label[events[, 2]]),
    receiver = as.character(nodes$label[events[, 3]]),
    increment = events[, 4],
    stringsAsFactors = FALSE
  )

  return(events)
}

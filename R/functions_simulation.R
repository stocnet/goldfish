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
#' @param nEventsMax integer with the maximum number of events to simulate from
#' the given formulas and parameter vectors. Default to \code{1e3}.
#' It ensures the simulation call finishes even in the case when the model
#' specification and parameter values leads to a process explosion in finite
#' interval.
#' @param startTime a numerical value or a date-time character.
#' It indicates the starting time for the simulation of the relational events.
#' The default value is `NULL`.
#' @param endTime a numerical value or a date-time character.
#' It indicates the ending time for the simulation of the relational events.
#' The default value is `NULL`.
#' @param keepPreprocess a logical value indicating whether should
#' preprocessed statistics be returned.
#'
#' @details
#' If arguments `startTime` and `endTime` use the default `NULL` value,
#' they would be set to the time of the first and last event respectively,
#' found on the dependent events and linked events objects that define
#' the formula(s) (`formulaRate` and `formulaChoice`).
#' If a date-time character is used, it is transformed to a numeric value
#' using `as.numeric()`.
#'
#' @export
#' @importFrom lifecycle badge
#'
#' @return when `keepPreprocess = FALSE` a data frame with the simulated
#' relational events that contains the following variables.
#' \describe{
#' \item{time}{`numeric` variable
#' containing the time-stamps when the event happen.}
#' \item{sender}{`character` variable indicating the label of the sender
#' of the event.}
#' \item{receiver}{`character` variable indicating the label of the receiver
#' of the event.}
#' }
#' when `keepPreprocess = TRUE`, and depending on the values of the arguments
#' `model` and `subModel`, is an object of class `preprocessed.goldfish`
#' or a list of objects of that class.
#'
#' @examples
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(
#'   x = callNetwork, changeEvent = calls,
#'   nodes = actors
#' )
#' callsDependent <- defineDependentEvents(
#'   events = calls, nodes = actors,
#'   defaultNetwork = callNetwork
#' )
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
simulate <- function(
    formulaRate,
    parametersRate,
    formulaChoice = NULL,
    parametersChoice = NULL,
    model = c("DyNAM", "REM"),
    subModel = c("choice", "choice_coordination"),
    nEventsMax = 1e3,
    startTime = NULL,
    endTime = NULL,
    keepPreprocess = FALSE,
    progress = getOption("progress")) {
  # CHECK INPUT
  model <- match.arg(model)
  subModel <- match.arg(subModel)

  ### check model and subModel
  checkModelPar(model, subModel,
    modelList = c("DyNAM", "REM", "DyNAMi"),
    subModelList = list(
      DyNAM = c("choice", "rate", "choice_coordination"),
      REM = "choice",
      DyNAMi = c("choice", "rate")
    )
  )

  if (subModel == "choice_coordination" || model == "DyNAMi") {
    stop(
      "It doesn't support yet simulating a DyNAM choice coordination or",
      "DyNAMi model.\n",
      call. = FALSE
    )
  }

  stopifnot(
    inherits(formulaRate, "formula"),
    is.null(formulaChoice) || inherits(formulaChoice, "formula"),
    inherits(parametersRate, "numeric"),
    is.null(parametersChoice) || inherits(parametersChoice, "numeric"),
    is.null(progress) || inherits(progress, "logical"),
    inherits(nEventsMax, "numeric") && nEventsMax > 0
  )

  if (is.null(progress)) progress <- FALSE

  envir <- environment()

  ## 1.1 Preparing
  parsedformulaRate <- parseFormula(formulaRate, envir = envir)

  # The number of the independent variables should be the length
  # of the input parameter vector
  if (length(parsedformulaRate$rhsNames) +
    parsedformulaRate$hasIntercept !=
    length(parametersRate)) {
    stop(
      "The number of effects in the rate sub-model should be the same",
      " as the length of the input parameter vector:",
      format(formulaRate), " with parameter ",
      paste(parametersRate, collapse = ",", sep = ""),
      call. = FALSE
    )
  }

  if (!is.null(formulaChoice)) {
    if (!(model == "DyNAM" && subModel %in% c("rate", "choice"))) {
      stop(
        "The model you specified doesn't require a formula",
        "for the choice subModel",
        call. = FALSE
      )
    }

    ## 1.1 PARSE for all cases: preprocessingInit or not
    parsedformulaChoice <- parseFormula(formulaChoice, envir = envir)
    if (parsedformulaChoice$hasIntercept) {
      # In the DyNAM choice model,
      # the intercept will be cancelled and hence useless.
      stop("Intercept in the choice subModel model will be ignored.",
        " Please remove the intercep and run again.",
        call. = FALSE
      )
    }

    if (length(parsedformulaChoice$rhsNames) !=
      length(parametersChoice)) {
      stop(
        "The number of the effects in the choice sub-model should be the same",
        " as the length of the input parameter:",
        format(formulaChoice), " with parameter ",
        paste(parametersChoice, collapse = ","),
        call. = FALSE
      )
    }

    if (parsedformulaRate$depName != parsedformulaChoice$depName) {
      stop("formula for rate and choice sub-models",
        " must be defined over the same dependent event object",
        call. = FALSE
      )
    }
  } else {
    parsedformulaChoice <- NULL
  }

  # CHECK THE INPUT FORMULA
  # There must exist the intercept in the formula for the waiting-time
  # generating process (For example, DyNAM rate, or REM),
  if (!parsedformulaRate$hasIntercept) {
    stop("You didn't specify an intercept in the rate formula.",
      "\n\tCurrent implementation requires intercept.",
      call. = FALSE
    )
  }

  # get node sets of dependent variable
  nodesInfo <- setNodesInfo(parsedformulaRate$depName, envir = envir)

  # Simulating!
  if (progress) cat("Starting simulation\n")
  events <- simulateEngine(
    model = model,
    subModel = subModel,
    parametersRate = parametersRate,
    parsedformulaRate = parsedformulaRate,
    parametersChoice = parametersChoice,
    parsedformulaChoice = parsedformulaChoice,
    nEvents = nEventsMax,
    nodesInfo = nodesInfo,
    startTime = startTime,
    endTime = endTime,
    keepPreprocess = keepPreprocess,
    progress = progress,
    envir = envir
  )

  if (keepPreprocess) {
    return(events)
  }

  nodes <- get(nodesInfo[["nodes"]], envir = envir)$label
  nodes2 <- if (!nodesInfo[["isTwoMode"]]) {
    nodes
  } else {
    get(nodesInfo[["nodes2"]], envir = envir)$label
  }
  # Styling the result
  events <- data.frame(
    time = events[, 1],
    sender = as.character(nodes[events[, 2]]),
    receiver = as.character(nodes[events[, 3]]),
    increment = events[, 4],
    stringsAsFactors = FALSE
  )

  return(events)
}

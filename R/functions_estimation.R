#' Estimate a model
#'
#' Estimates parameters for a dynamic network model via maximum likelihood
#'  implementing the iterative Newton-Raphson procedure as describe in
#'  Stadtfeld and Block (2017).
#'
#' Missing data is imputed during the preprocessing stage.
#' For network data missing values are replaced by a zero value,
#' it means that is assuming a not tie/event explicitly.
#' For attributes missing values are replaced by the mean value,
#' if missing values are presented during events updates they are replace by
#' the mean of the attribute in that moment of time.
#'
#' @section DyNAM:
#'
#' The actor-oriented models that the goldfish package implements have been
#' called Dynamic Network Actor Models (DyNAMs).
#' The model is a two-step process. In the first step, the waiting time until
#' an actor \eqn{i} initiates the next relational event is modeled
#' (`model = "DyNAM"` and `subModel = "rate"`) by an exponential
#' distribution depending on the actor activity rate.
#' In the second step, the conditional probability of \eqn{i} choosing
#'  \eqn{j} as the event receiver is modeled (`model = "DyNAM"` and
#'  `subModel = "choice"`) by a multinomial probability distribution
#'  with a linear predictor.
#' These two-steps are assumed to be conditionally independent given
#' the process state (Stadtfeld, 2012),
#' due to this assumption is possible to estimate these components by
#' different calls of the `estimate` function.
#'
#' @section Waiting times:
#'
#' When DyNAM-rate (`model = "DyNAM"` and `subModel = "rate"`) model
#' is used to estimate the first step component of the process, or the REM
#' `model = "REM"` model is used.
#' It is important to add a time intercept to model the waiting times between
#' events, in this way the algorithm considers the right-censored intervals
#' in the estimation process.
#'
#' In the case that the intercept is not included in the formula.
#' The model reflects the likelihood of an event being the next
#' in the sequence.
#' This specification is useful for scenarios where the researcher doesn't have
#' access to the exact interevent times.
#' For this ordinal case the likelihood of an event is merely a
#' multinomial probability (Butts, 2008).
#'
#' @param model a character string defining the model type.
#' Current options include `"DyNAM"`, `"DyNAMi"` or `"REM"`
#' \describe{
#'  \item{DyNAM}{Dynamic Network Actor Models
#'  (Stadtfeld, Hollway and Block, 2017 and Stadtfeld and Block, 2017)}
#'  \item{DyNAMi}{Dynamic Network Actor Models for interactions
#'  (Hoffman et al., 2020)}
#'  \item{REM}{Relational Event Model (Butts, 2008)}
#' }
#' @param subModel a character string defining the submodel type.
#'  Current options include `"choice"`, `"rate"` or
#'  `"choice_coordination"`
#' \describe{
#'  \item{choice}{a multinomial receiver choice model `model = "DyNAM"`
#'  (Stadtfeld and Block, 2017), or the general Relational event model
#'  `model = "REM"` (Butts, 2008).
#'  A multinomial group choice model `model = "DyNAMi"` (Hoffman et al., 2020)}
#'  \item{choice_coordination}{a multinomial-multinomial model for coordination
#'  ties `model = "DyNAM"` (Stadtfeld, Hollway and Block, 2017)}
#'  \item{rate}{A individual activity rates model `model = "DyNAM"`
#'  (Stadtfeld and Block, 2017).
#'  Two rate models, one for individuals joining groups and one for individuals
#'  leaving groups, jointly estimated `model = "DyNAMi"`(Hoffman et al., 2020)}
#' }
#' @param estimationInit a list containing lower level technical parameters
#' for estimation. It may contain:
#' \describe{
#'  \item{initialParameters}{a numeric vector.
#'  It includes initial parameters of the estimation.
#'  Default is set to NULL.}
#'	\item{fixedParameters}{a numeric vector. It specifies which component of
#'	the coefficient parameters (intercept included) is fixed and the value
#'	it takes during estimation, e.g., if the vector is `c(2, NA)` then
#'	the first component of the parameter is fixed to 2 during the
#'	estimation process. Default is set to `NULL`, i.e. all parameters are
#'	estimated. Note that it must be consistent with `initialParameters`.}
#'  \item{maxIterations}{maximum number of iterations of the Gauss/Fisher
#'  scoring method for the estimation. Default is set to 20.}
#'  \item{maxScoreStopCriterion}{maximum absolute score criteria for successful
#'  convergence. Default value is 0.001}
#'  \item{initialDamping}{a numeric vector used to declare the initial damping
#'  factor for each parameter.
#'  It controls the size of the update step during the iterative estimation
#'  process. The default is set to 30 when the formula has windowed effects or
#'  10 in another case, see `vignette("goldfishEffects")`.}
#'  \item{dampingIncreaseFactor}{a numeric value. It controls the factor that
#'  increases the damping of the parameters when improvements in the estimation
#'  are found.}
#'  \item{dampingDecreaseFactor}{a numeric value. Controls the factor that
#'  decreases the damping of the parameters when no improvements in the
#'  estimation are found.}
#'  \item{returnIntervalLogL}{a logical value. Whether to keep the
#'  log-likelihood of each event from the final iteration of the Gauss/Fisher
#'  estimation method.}
#'  \item{engine}{a string indicating the estimation engine to be used.
#'  Current options include `"default"`, `"default_c"`, and `"gather_compute"`.
#'  The default value is `"default"`, it is an estimation routine
#'  implemented in pure `R` code.
#'  `"default_c"` uses a `C` implementation of the `"default"` routine.
#'  `"gather_compute"` uses a `C` implementation with a different data
#'  structure that reduces the time but it can increase the memory usage.}
#' }
#' @param preprocessingOnly logical indicating whether only preprocessed
#' statistics should be returned rather than a `result.goldfish` object
#' with the estimated coefficients.
#' @param preprocessingInit a `preprocessed.goldfish` object computed for
#' the current formula, allows skipping the preprocessing step.
#' @param preprocessingArgs a list or arguments to be passed to the 
#' pre-processing function. It may contain:
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
#'  }
#' @param verbose logical indicating whether should print
#' very detailed intermediate results of the iterative Newton-Raphson
#' procedure; slows down the routine significantly.
#' @param progress logical indicating whether should print a minimal output
#' to the console of the progress of the preprocessing and
#' estimation processes.
#' @param x a formula that defines at the left-hand side the dependent
#' network (see [defineDependentEvents()]) and at the right-hand side the
#' effects and the variables for which the effects are expected to occur
#' (see `vignette("goldfishEffects")`).
#'
#' @return returns an object of [class()] `"result.goldfish"`
#' when `preprocessingOnly = FALSE` or
#' a preprocessed statistics object of class `"preprocessed.goldfish"`
#' when `preprocessingOnly = TRUE`.
#'
#' An object of class `"result.goldfish"` is a list including:
#'   \item{parameters}{a numeric vector with the coefficients estimates.}
#'   \item{standardErrors}{
#'    a numeric vector with the standard errors of the coefficients estimates.}
#'   \item{logLikelihood}{the log-likelihood of the estimated model}
#'   \item{finalScore}{
#'    a vector with the final score reach by the parameters during estimation.}
#'   \item{finalInformationMatrix}{
#'    a matrix with the final values of the negative Fisher information matrix.
#'    The inverse of this matrix gives the variance-covariance matrix for the
#'    parameters estimates.}
#'   \item{convergence}{a list with two elements.
#'    The first element (\code{isConverged}) is a logical value that indicates
#'    the convergence of the model.
#'    The second element (\code{maxAbsScore}) reports the final maximum
#'    absolute score in the final iteration.}
#'   \item{nIterations}{
#'    an integer with the total number of iterations performed during the
#'    estimation process.}
#'   \item{nEvents}{
#'    an integer reporting the number of events considered in the model.}
#'   \item{names}{
#'    a matrix with a description of the effects used for model fitting.
#'    It includes the name of the object used to calculate the effects and
#'    additional parameter description.}
#'   \item{formula}{a formula with the information of the model fitted.}
#'   \item{model}{a character value of the model type.}
#'   \item{subModel}{a character value of the subModel type.}
#'   \item{rightCensored}{
#'   a logical value indicating if the estimation process considered
#'   right-censored events.
#'   Only it is considered for DyNAM-rate (`model = "DyNAM"` and
#'   `subModel = "rate"`) or REM (`model = "REM"`) models,
#'   and when the model includes the intercept.}
#'
#' @importFrom stats formula na.omit end filter
#' @export
#' @seealso [defineDependentEvents()], [defineGlobalAttribute()],
#'  [defineNetwork()], [defineNodes()], [linkEvents()]
#'
#' @references Butts C. (2008). A Relational Event Framework for Social Action.
#' \emph{Sociological Methodology 38 (1)}.
#' \doi{10.1111/j.1467-9531.2008.00203.x}
#'
#' Hoffman, M., Block P., Elmer T., and Stadtfeld C. (2020).
#' A model for the dynamics of face-to-face interactions in social groups.
#' \emph{Network Science}, 8(S1), S4-S25. \doi{10.1017/nws.2020.3}
#'
#' Stadtfeld, C. (2012). Events in Social Networks: A Stochastic Actor-oriented
#' Framework for Dynamic Event Processes in Social Networks.
#' \emph{KIT Scientific Publishing}. \doi{10.5445/KSP/1000025407}
#'
#' Stadtfeld, C., and Block, P. (2017). Interactions, Actors, and Time:
#' Dynamic Network Actor Models for Relational Events.
#' \emph{Sociological Science 4 (1)}, 318-52. \doi{10.15195/v4.a14}
#'
#' Stadtfeld, C., Hollway, J., and Block, P. (2017).
#' Dynamic Network Actor Models: Investigating Coordination Ties Through Time.
#' \emph{Sociological Methodology 47 (1)}. \doi{10.1177/0081175017709295}
#'
#' @examples
#' # A multinomial receiver choice model
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls,
#'                           nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors,
#'                                         defaultNetwork = callNetwork)
#'
#' \dontshow{
#' callsDependent <- callsDependent[1:50, ]
#' }
#' 
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'                   model = "DyNAM", subModel = "choice",
#'                   estimationInit = list(engine = "default_c"))
#' summary(mod01)
#'
#' # A individual activity rates model
#' mod02 <- estimate(callsDependent ~ 1 + nodeTrans + indeg + outdeg,
#'                   model = "DyNAM", subModel = "rate",
#'                   estimationInit = list(engine = "default_c"))
#' summary(mod02)
#'
#' \donttest{
#' # A multinomial-multinomial choice model for coordination ties
#' data("Fisheries_Treaties_6070")
#' states <- defineNodes(states)
#' states <- linkEvents(states, sovchanges, attribute = "present")
#' states <- linkEvents(states, regchanges, attribute = "regime")
#' states <- linkEvents(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- linkEvents(bilatnet, bilatchanges, nodes = states)
#'
#' contignet <- defineNetwork(contignet, nodes = states, directed = FALSE)
#' contignet <- linkEvents(contignet, contigchanges, nodes = states)
#'
#' createBilat <- defineDependentEvents(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, defaultNetwork = bilatnet
#' )
#'  
#' partnerModel <- estimate(
#'   createBilat ~
#'     inertia(bilatnet) +
#'     indeg(bilatnet, ignoreRep = TRUE) +
#'     trans(bilatnet, ignoreRep = TRUE) +
#'     tie(contignet) +
#'     alter(states$regime) +
#'     diff(states$regime) +
#'     alter(states$gdp) +
#'     diff(states$gdp),
#'   model = "DyNAM", subModel = "choice_coordination",
#'   estimationInit = list(initialDamping = 40, maxIterations = 30)
#' )
#' summary(partnerModel)
#' }
#' 
estimate <- function(
  x,
  model = c("DyNAM", "REM", "DyNAMi"),
  subModel = c("choice", "rate", "choice_coordination"),
  estimationInit = NULL,
  preprocessingOnly = FALSE,
  preprocessingArgs = NULL,
  preprocessingInit = NULL,
  progress = getOption("progress"),
  verbose = getOption("verbose"))
  UseMethod("estimate", x)


# First estimation from a formula: can return either a preprocessed object or a
# result object
#' @importFrom stats as.formula
#' @export
estimate.formula <- function(
  x,
  model = c("DyNAM", "REM", "DyNAMi"),
  subModel = c("choice", "rate", "choice_coordination"),
  estimationInit = NULL,
  preprocessingOnly = FALSE,
  preprocessingArgs = NULL,
  preprocessingInit = NULL,
  progress = getOption("progress"),
  verbose = getOption("verbose")) {
  # Steps:
  # 1. Parse the formula
  # 2. Initialize additional objects
  # 3. Preprocess
  # 4. Estimate model (old estimation still available as an option)

  ### 0. check parameters----
  model <- match.arg(model)
  subModel <- match.arg(subModel)

  # enviroment from which get the objects
  # envir <- environment()

  ### check model and subModel
  checkModelPar(
    model, subModel,
    modelList = c("DyNAM", "REM", "DyNAMi"),
    subModelList = list(
      DyNAM = c("choice", "rate", "choice_coordination"),
      REM = "choice",
      DyNAMi = c("choice", "rate")
    )
  )

  stopifnot(
    inherits(preprocessingOnly, "logical"),
    inherits(verbose, "logical"),
    is.null(progress) || inherits(progress, "logical"),
    is.null(preprocessingInit) ||
      inherits(preprocessingInit, "preprocessed.goldfish"),
    is.null(estimationInit) ||
      inherits(estimationInit, "list"),
    is.null(preprocessingArgs) ||
      inherits(preprocessingArgs, "list")
  )

  if (is.null(progress)) progress <- FALSE

  checkArgs(
    estimationInit,
    c("maxIterations", "maxScoreStopCriterion", "initialDamping",
      "dampingIncreaseFactor", "dampingDecreaseFactor", "initialParameters",
      "fixedParameters", "returnEventProbabilities", "returnIntervalLogL",
      "impute", "engine")
  )
  checkArgs(
    preprocessingArgs, c("startTime", "endTime", "opportunitiesList")
  )

  # Decide the type of engine
  engine <- match.arg(estimationInit[["engine"]],
                      c("default", "default_c", "gather_compute"))
  if (!is.null(estimationInit[["engine"]])) estimationInit[["engine"]] <- NULL

  # gather_compute and default_c don't support returnEventProbabilities
  if (!is.null(estimationInit) &&
      "returnEventProbabilities" %in% names(estimationInit) &&
      engine != "default") {
    warning("engine = ", dQuote(engine), " doesn't support",
            dQuote("returnEventProbabilities"),
            ". engine =", dQuote("default"), " is used instead.",
            call. = FALSE, immediate. = TRUE)
    engine <- "default"
  }
  
  # gather_compute and default_c don't support restrictions of opportunity sets
  if (!is.null(preprocessingArgs) &&
      "opportunitiesList" %in% names(preprocessingArgs) &&
      engine != "default") {
    warning("engine = ", dQuote(engine), " doesn't support",
            dQuote("opportunitiesList"),
            ". engine =", dQuote("default"), " is used instead.",
            call. = FALSE, immediate. = TRUE)
    engine <- "default"
  }



  ### 1. PARSE the formula----
  prepEnvir <- new.env()
  
  if (progress) cat("Parsing formula.\n")
  ## 1.1 PARSE for all cases: preprocessingInit or not
  parsedformula <- parseFormula(x, envir = prepEnvir)

  # DyNAM-i ONLY: creates extra parameter to differentiate joining and
  # leaving rates, and effect subtypes. Added directly to GetDetailPrint

  # # C implementation doesn't have ignoreRep option issue #105
  if (any(unlist(parsedformula$ignoreRepParameter)) &&
      engine %in% c("default_c", "gather_compute")) {
    warning("engine = ", dQuote(engine),
            " doesn't support ignoreRep effects. engine =",
            dQuote("default"), " is used instead.",
            call. = FALSE, immediate. = TRUE)
    engine <- "default"
  }
  # Model-specific preprocessing initialization
  if (model %in% c("DyNAM", "DyNAMi") &&
      subModel %in% c("choice", "choice_coordination") &&
      parsedformula$hasIntercept) {
    warning("Model ", dQuote(model), " subModel ", dQuote(subModel),
            " ignores the time intercept.",
            call. = FALSE, immediate. = TRUE)
    parsedformula$hasIntercept <- FALSE
  }
  rightCensored <- parsedformula$hasIntercept

  if (progress && 
      !(model %in% c("DyNAM", "DyNAMi") &&
      subModel %in% c("choice", "choice_coordination")))
    cat(
      ifelse(parsedformula$hasIntercept, "T", "No t"),
      "ime intercept added.\n", sep = ""
    )

  # if (progress) cat("Initializing objects.\n")

  ## 2.0 get nodes info: isTwoMode to define effects functions
  # get node sets of dependent variable
  nodesInfo <- setNodesInfo(parsedformula$depName, envir = prepEnvir)

  ### 3. PREPROCESS statistics----
  ## 3.2 PREPROCESS when preprocessingInit == NULL
  if (is.null(preprocessingInit)) {
    if (progress) cat("Starting preprocessing.\n")
    if (model == "DyNAMi") {
      prep <- preprocessInteraction(
        subModel = subModel,
        # multipleParameter,
        rightCensored = rightCensored,
        progress = progress,
        groupsNetwork = parsedformula$defaultNetworkName,
        prepEnvir = prepEnvir)
    } else {
      prep <- preprocess(
        model = model,
        subModel = subModel,
        parsedformula = parsedformula,
        # multipleParameter = multipleParameter,
        opportunitiesList = opportunitiesList,
        startTime = estimationInit[["startTime"]],
        endTime = estimationInit[["endTime"]],
        rightCensored = rightCensored,
        progress = progress,
        prepEnvir = prepEnvir
      )
  }
    # The formula, nodes, nodes2 are added to the preprocessed object so that
    # we can call the estimation with preprocessingInit later
    # (for parsing AND composition changes)
    prep$formula   <- x
    prep$model     <- model
    prep$subModel  <- subModel
    prep$nodesInfo <- nodesInfo
  } else if (!is.null(preprocessingInit) &&
       !isTRUE(all.equal(x, preprocessingInit$formula))) {
    prep <- adjustPrepInit(
      preprocessingInit = preprocessingInit, x = x, 
      parsedformula = parsedformula,
      model = model, subModel = subModel, progress = progress,
      envir = prepEnvir
    )
  }

  ## 3.3 Stop here if preprocessingOnly
  if (preprocessingOnly) {
    return(prep)
  }


  ### 4. PREPARE PRINTING----
  effectDescription <- GetDetailPrint(
    parsedformula, estimationInit[["fixedParameters"]]
  )
  hasWindows <- attr(effectDescription, "hasWindows")
  if (is.null(hasWindows)) {
    hasWindows <- !all(vapply(
      parsedformula$windowParameters, is.null, logical(1)
    ))
  }
  attr(effectDescription, "hasWindows") <- NULL
  ### 5. ESTIMATE----
  # CHANGED Alvaro: to match model and subModel new parameters
  if (model == "REM") {
    if (!parsedformula$hasIntercept) {
      modelTypeCall <- "REM-ordered"
    } else {
      modelTypeCall <- "REM"
    }
  } else if (model %in% c("DyNAM", "DyNAMi")) {
    if (subModel == "rate" && !parsedformula$hasIntercept) {
      modelTypeCall <- "DyNAM-M-Rate-ordered"
    } else if (subModel == "rate") {
      modelTypeCall <- "DyNAM-M-Rate"
    } else if (subModel == "choice_coordination") {
      modelTypeCall <- "DyNAM-MM"
    } else {
      modelTypeCall <- "DyNAM-M"
    }
  }
  if (progress)
    cat(
      "Estimating a model: ", dQuote(model), ", subModel: ",
      dQuote(subModel), ".\n", sep = "")

  EstimateEnvir <- new.env()
  # Default estimation
  additionalArgs <- list(
    statsList = prep,
    nodes = get(nodesInfo[["nodes"]], envir = prepEnvir),
    nodes2 = get(nodesInfo[["nodes2"]], envir = prepEnvir),
    defaultNetworkName = parsedformula$defaultNetworkName,
    addInterceptEffect = parsedformula$hasIntercept,
    modelType = modelTypeCall,
    initialDamping = ifelse(hasWindows, 30, 10),
    parallelize = FALSE,
    cpus = 1,
    verbose = verbose,
    progress = progress,
    ignoreRepParameter = parsedformula$ignoreRepParameter,
    isTwoMode = nodesInfo[["isTwoMode"]],
    prepEnvir = EstimateEnvir
  )
  # prefer user-defined arguments
  argsEstimation <- append(
    estimationInit[!(names(estimationInit) %in%
                       c("startTime", "endTime"))],
    additionalArgs[!(names(additionalArgs) %in% names(estimationInit))]
  )

  # use different engine depends on the variable "engine"
  if (engine %in% c("default_c", "gather_compute")) {
    tryCatch(
      result <- do.call("estimate_c_int", args = argsEstimation),
      error = function(e) stop("Error in ", model, " ", subModel,
                               " estimation: ", e, call. = FALSE)
    )
  } else {
    tryCatch(
      result <- do.call("estimate_int", args = argsEstimation),
      error = function(e) stop("Error in ", model, " ", subModel,
                               " estimation: ", e, call. = FALSE)
    )
  }

  ### 6. RESULTS----
  result$names <- effectDescription
  formulaKeep <- as.formula(Reduce(paste, deparse(x)),
                            env = new.env(parent = emptyenv()))
  result$formula <- formulaKeep
  result$model <- model
  result$subModel <- subModel
  result$rightCensored <- rightCensored
  result$nParams <- sum(!GetFixed(result))
  result$call <- match.call(call = sys.call(-1L), expand.dots = TRUE)
  result$call[[2]] <- formulaKeep
  
  return(result)
}

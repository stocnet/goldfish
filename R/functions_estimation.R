#' Estimate a model
#'
#' Estimates parameters for a dynamic network model via maximum likelihood.
#'
#' Missing data is imputed during the preprocessing stage. For network data missing values
#' are replaced by a zero value, it means that is assuming a not tie/event explicitly.
#' For attributes missing values are replaced by the mean value, if missing values are
#' presented during events updates they are replace by the mean of the attribute in that moment of time.
#'
#' @section DyNAM:
#'
#' The actor-oriented models that the goldfish package implements have been called
#' Dynamic Network Actor Models (DyNAMs).
#' The model is a two-step process. In the first step, the waiting time until an actor \eqn{i}
#' becomes active to send an event is modeled (\code{model = "DyNAM"} and \code{subModel = "rate"}). In the second step,
#' the conditional probability of \eqn{i} choosing \eqn{j} as the event receiver is modeled
#' (\code{model = "DyNAM"} and \code{subModel = "choice"}).
#' These two-steps are assumed to be conditionally independent given the process state (Stadtfeld, 2012),
#' due to this assumption is possible to estimate these components by different calls of the \code{estimate} function.
#'
#' When \code{model = "DyNAM"} and \code{subModel = "rate"} is used to estimate the first step component of the process.
#' It is important to add a time intercept to model the waiting times between events, in this way the model
#' considers the right-censored intervals in the estimation process.
#'
#' @param model a character string defining the model type. Current options include \code{"DyNAM"} or \code{"REM"}
#' \describe{
#'  \item{DyNAM}{Dynamic Network Actor Models (Stadtfeld, Hollway and Block, 2017 and Stadtfeld and Block, 2017)}
#'  \item{REM}{Relational Event Model (Butts, 2008)}
#' }
#' @param subModel a character string defining the submodel type. Current options include "choice", "rate" or
#'  "choice_coordination"
#' \describe{
#'  \item{choice}{a multinomial receiver choice model if \code{model = "DyNAM"} (Stadtfeld and Block, 2017),
#'  or the general Relational event model if \code{model = "REM"} (Butts, 2008).}
#'  \item{choice_coordination}{a multinomial-multinomial model for coordination ties
#'  (Stadtfeld, Hollway and Block, 2017)}
#'  \item{rate}{A individual activity rates model if \code{model = "DyNAM"} (Stadtfeld and Block, 2017)}
#' }
#' @param estimationInit a list containing lower level technical parameters for estimation. May be:
#' \describe{
#'  \item{initialParameters}{a numeric vector. It includes initial parameters of the estimation.
#'  Default is set to NULL.}
#'	\item{fixedParameters}{a numeric vector. It specify which component of the parameter (intercept included) is fixed
#'	and what's the fixed values during the estimation. E.g. if the vector is c(2,NA). Then the first component of
#'	the parameter is fixed to 2 during the whole esitmation process. Default is set to NULL, i.e. we don't fixed
#'	any parameter. Note that it must be consistent with initialParameters. }
#'  \item{maxIterations}{maximum number of iterations of the Gauss/Fisher scoring method for the estimation.
#'  Default is set to 20.}
#'  \item{maxScoreStopCriterion}{maximum absolute score criteria for successful convergence. Default value is 0.001}
#'  \item{initialDamping}{a numeric vector used to declare the initial damping factor for each parameter.
#'  It controls the size of the update step during the iterative estimation process. The default is set
#'  to 30 when the formula has windowed effects or 10 in another case, see \code{\link{goldfishEffects}}.}
#'  \item{dampingIncreaseFactor}{a numeric value. It controls the factor that increases the damping of the parameters
#'  when improvements in the estimation are found.}
#'  \item{dampingDecreaseFactor}{a numeric value. Controls the factor that decreases the damping of the parameters
#'  when no improvements in the estimation are found.}
#'  \item{engine}{a string indicating the estimation engine to be used.
#'  Current options include "old", "default", "default_c",and "gather_compute".
#'  The default value is "default", it is an estimation routine implemented in pure \code{R} code.
#'  "old" uses an \code{R} estimation routine that uses an inefficient version of data structures.
#'  "default_c" uses a \code{C} implementation of the "default" routine.
#'  "gather_compute" uses a \code{C} implementation with a different data structure
#'  that reduces the time but it can increase the memory usage.}
#'  \item{startTime}{a numerical value or a date-time character with the same time-zone formatting as the times in
#'  event that indicates the starting time to be considered during estimation.}
#'  \item{endTime}{a numerical value or a date-time character with the same time-zone formatting as the times in
#'  event that indicates the end time to be considered during estimation.}
#' }
#' @param preprocessingOnly logical. Indicates whether only preprocessed statistics should be returned
#' rather than a results object.
#' @param preprocessingInit a preprocessed.goldfish object computed for the current formula,
#' allows skipping the preprocessing step.
#' @param silent logical indicating whether a mimimal output should be given.
#' @param debug logical indicating whether very detailed intermediate results should be given;
#' slows down the routine significantly.
#' @param verbose logical indicating whether details of the estimation routine should be provided.
#' @param x a formula that defines at the left-hand side the dependent network
#' (see \code{\link{defineDependentEvents}}) and at the right-hand side the effects and the variables for
#' which the effects are expected to occur (see \code{\link{goldfishEffects}}).
# or a preprocessed statistics object.
#'
#' @return returns an object of \code{\link{class}} \code{"result.goldfish"} when \code{preprocessingOnly = FALSE} or
#' a preprocessed statistics object of class \code{"preprocessed.goldfish"} when \code{preprocessingOnly = TRUE}.
#'
#' An object of class \code{"result.goldfish"} is a list including:
#'   \item{parameters}{a matrix with the coefficients estimates.}
#'   \item{standardErrors}{a vector with the standard errors of the coefficients.}
#'   \item{logLikelihood}{the log likelihood of the estimate model}
#'   \item{finalScore}{a vector with the final score reach by the parameters during estimation.}
#'   \item{finalInformationMatrix}{a matrix with the final values of the negative Fisher information matrix.
#'   The inverse of this matrix gives the variance-covariance matrix for the parameters estimates.}
#'   \item{convergence}{a list with two elements.
#'   The first element (\code{isConverged}) is a logical value that indicates the convergence of the model.
#'   The second element (\code{maxAbsScore}) reports the final maximum absolute score in the final iteration.}
#'   \item{nIterations}{an integer with the total number of iterations performed during the estimation process.}
#'   \item{nEvents}{an integer reporting the number of events considered in the model.}
#'   \item{names}{a matrix with a description of the effects used for model fitting.
#'   It includes the name of the object used to calculate the effects and additional parameter description.}
#'   \item{formula}{a formula with the information of the model fitted.}
#'   \item{model}{a character vector of the model type.}
#'   \item{rightCensored}{a logical value indicating if the estimation process considered right censored events.
#'   Only it is considered when \code{model = "DyNAM"}, \code{subModel = "rate"} and the model includes intercept.}
#'
#' @importFrom stats formula na.omit end filter
#' @export
#' @seealso \code{\link{defineDependentEvents}}, \code{\link{goldfishEffects}}, \code{\link{defineGlobalAttribute}},
#'  \code{\link{defineNetwork}}, \code{\link{defineNodes}}, \code{\link{linkEvents}}
#' @references Butts C. (2008). A Relational Event Framework for Social Action.
#' \emph{Sociological Methodology 38 (1)}. \doi{10.1111/j.1467-9531.2008.00203.x}
#'
#' Stadtfeld, C. (2012). Events in Social Networks: A Stochastic Actor-oriented Framework for Dynamic Event Processes
#' in Social Networks.
#' \emph{KIT Scientific Publishing}. \doi{10.5445/KSP/1000025407}
#'
#' Stadtfeld, C., and Block, P. (2017). Interactions, Actors, and Time: Dynamic Network Actor Models for
#' Relational Events.
#' \emph{Sociological Science 4 (1)}, 318-52. \doi{10.15195/v4.a14}
#'
#' Stadtfeld, C., Hollway, J., and Block, P. (2017). Dynamic Network Actor Models: Investigating Coordination Ties
#' Through Time.
#' \emph{Sociological Methodology 47 (1)}. \doi{10.1177/0081175017709295}
#'
#'
#' @usage
#' estimate(x, model = c("DyNAM", "REM"),
#'          subModel = c("choice", "rate", "choice_coordination"),
#'          estimationInit = NULL, preprocessingInit = NULL,
#'          preprocessingOnly = FALSE,
#'          verbose = FALSE, silent = FALSE, debug = FALSE)
#'
#' @examples
#' # A multinomial receiver choice model
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors,
#'                                         defaultNetwork = callNetwork)
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'                   model = "DyNAM", subModel = "choice")
#' summary(mod01)
#'
#' # A individual activity rates model
#' mod02 <- estimate(callsDependent ~ 1 + node_trans + indeg + outdeg,
#'                   model = "DyNAM", subModel = "rate")
#' summary(mod02)
#'
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
#' createBilat <- defineDependentEvents(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, defaultNetwork = bilatnet
#' )
#'
#' contignet <- defineNetwork(contignet, nodes = states, directed = FALSE)
#' contignet <- linkEvents(contignet, contigchanges, nodes = states)
#' partner.model <- estimate(createBilat ~
#'   inertia(bilatnet) +
#'   indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
#'   tie(contignet, ignoreRep = TRUE) +
#'   alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE) +
#'   alter(states$gdp, ignoreRep = TRUE) + diff(states$gdp, ignoreRep = TRUE),
#' model = "DyNAM", subModel = "choice_coordination",
#' estimationInit = list(initialDamping = 40, maxIterations = 30)
#' )
#' summary(partner.model)
# \item{impute}{a boolean indicating whether it should impute the statistics for missing values. }
estimate <- function(x,
                     model = c("DyNAM", "REM"),
                     subModel = c("choice", "rate", "choice_coordination"),
                     estimationInit = NULL,
                     preprocessingInit = NULL,
                     preprocessingOnly = FALSE,
                     verbose = FALSE,
                     silent = FALSE,
                     debug = FALSE)
  UseMethod("estimate", x)


# First estimation from a formula: can return either a preprocessed object or a result object
#' @export
estimate.formula <- function(x,
                             model = c("DyNAM", "REM"),
                             subModel = c("choice", "rate", "choice_coordination"),
                             estimationInit = NULL,
                             preprocessingInit = NULL,
                             preprocessingOnly = FALSE,
                             verbose = FALSE,
                             silent = FALSE,
                             debug = FALSE) {
  # Steps:
  # 1. Parse the formula
  # 2. Initialize additional objects
  # 3. Preprocess
  # 4. Estimate model (old estimation still available as an option)

  # TODO new:
  # - go through old TODO list and see what is still important
  # - understand how right censored stats are calculated and used afterwards
  # - check if we can get rid of modelType and modelTypeCall every where
  #
  # TODO:
  # - Check if the dependent variable is associated with a default network
  #   - If yes, use this for "multiple" or if no objects are provided, e.g. "events ~ trans"
  #   - "multiple" mostly makes sense with a default network, it seems
  # - add the parallelization parameters
  # - add interaction effects
  # - Does the estimation from prior results work with intercepts?
  # - add the rowOnly colOnly option to prep
  # - allow specifications of effects without naming the actor set; through a lookup
  # - egoX only works with two parameters or more
  # - update 'multiple' for tie deletion
  # - time window name output: update object name
  # - add n.right.censored to results object

  # For debugging
  if (identical(environment(), globalenv())) {
    verbose <- FALSE
    silent <- FALSE
    model <- "DyNAM"
    subModel <- "rate"
    estimationInit <- NULL
    preprocessingOnly <- FALSE
    preprocessingInit <- NULL
    source("R/functions_effects.R")
    source("R/functions_effects_DyNAM_rate.R")
    source("R/functions_effects_DyNAM_choice.R")
    source("R/functions_estimation_engine.R")
    source("R/functions_preprocessing.R")
    source("R/functions_estimation_old.R")
    source("R/functions_parsing.R")
    source("R/functions_utility.R")
    # source("R/functions_utility.R")
    # x <- formula
  }

  ### 0. check parameters----

  # .calls_args_ <- names(lapply(match.call(), deparse))[-1]
  # if (any("modelType" %in% .calls_args_)) {
  #   stop("use 'model' and 'subModel' instead of 'modelType'")
  # }
  # rm(.calls_args_)

  #
  model <- match.arg(model)
  subModel <- match.arg(subModel)

  ### check model and subModel
  checkModelPar(model, subModel,
    modelList = c("DyNAM", "REM", "TriNAM"),
    subModelList = list(
      DyNAM = c("choice", "rate", "choice_coordination"),
      REM = c("choice"),
      TriNAM = c("choice", "rate")
    )
  )

  # ToDo: more informative message for R>4.0.0
  stopifnot(
    inherits(preprocessingOnly, "logical"),
    inherits(verbose, "logical"),
    inherits(silent, "logical"),
    inherits(debug, "logical"),
    is.null(preprocessingInit) || inherits(preprocessingInit, "preprocessed.goldfish"),
    is.null(estimationInit) || inherits(estimationInit, "list")
  )

  if (!is.null(estimationInit)) {
    parInit <- names(estimationInit) %in%
      c(
        "maxIterations", "maxScoreStopCriterion", "initialDamping",
        "dampingIncreaseFactor", "dampingDecreaseFactor", "initialParameters", "fixedParameters",
        "returnEventProbabilities", "returnIntervalLogL", "impute", "engine",
        "startTime", "endTime"
      )


    if (any(!parInit)) {
      warning("The parameter: ", paste(names(estimationInit)[!parInit], collapse = ", "),
        " is not recognized. See the documentation for the list of available parameters",
        call. = FALSE, immediate. = TRUE
      )
    }
  }

  # Decide the type of engine
  engine <- match.arg(estimationInit[["engine"]], c("default", "old", "default_c", "gather_compute"))
  if (!is.null(estimationInit[["engine"]])) estimationInit[["engine"]] <- NULL

  # gather_compute and default_c doesn't support returnEventProbabilities
  if (!is.null(estimationInit) && "returnEventProbabilities" %in% names(estimationInit)) {
    if (estimationInit["returnEventProbabilities"] == TRUE && engine != "default") {
      warning("Current estimation engine doesn't support returnEventProbabilities", call. = FALSE, immediate. = TRUE)
    }
  }

  ### 1. PARSE the formula----

  if (!silent) cat("Parsing formula.\n")
  formula <- x

  ## 1.1 PARSE for all cases: preprocessingInit or not
  parsedformula <- parseFormula(formula)
  rhsNames <- parsedformula$rhsNames
  depName <- parsedformula$depName
  hasIntercept <- parsedformula$hasIntercept
  windowParameters <- parsedformula$windowParameters
  # defaultNetworkName <- parsedformula$defaultNetworkName
  # ignoreRepParameter <- parsedformula$ignoreRepParameter
  # weightedParameter <- parsedformula$weightedParameter
  # userSetParameter <- parsedformula$userSetParameter

  # # C implementation doesn't have ignoreRep option issue #105
  if (any(unlist(parsedformula$ignoreRepParameter)) && engine %in% c("default_c", "gather_compute")) {
    warning("engine: ", engine, " doesn't support ignoreRep effects. engine = 'default' is used instead.",
            call. = FALSE, immediate. = TRUE)
    engine <- "default"
  }
  # Model-specific preprocessing initialization
  if (model %in% c("DyNAM", "TriNAM") && subModel %in% c("choice", "choice_coordination") && hasIntercept) {
    warning("Model ", model, " subModel ", subModel, " ignores the time intercept.", call. = FALSE, immediate. = TRUE)
    hasIntercept <- FALSE
  }
  rightCensored <- hasIntercept

  if (verbose) cat(ifelse(hasIntercept, "T", "No t"), "ime intercept added.\n", sep = "")
  if (!silent && !all(vapply(windowParameters, is.null, logical(1)))) {
    cat("Creating window objects in global environment.\n")
  }

  ## 1.2 PARSE for preprocessingInit: check the formula consistency
  if (!is.null(preprocessingInit)) {
    # find the old and new effects indexes, do basic consistency checks
    oldparsedformula <- parseFormula(preprocessingInit$formula)
    effectsindexes <- compareFormulas(
      oldparsedformula = oldparsedformula,
      newparsedformula = parsedformula,
      model = model, subModel = subModel
    )
  }

  ### 2. INITIALIZE OBJECTS: effects, nodes, and link objects----

  if (!silent) cat("Initializing objects.\n")

  ## 2.0 Set idTwoMode to define effects functions
  # get node sets of dependent variable
  .nodes <- attr(get(depName), "nodes")
  isTwoMode <- FALSE
  # two-mode networks(2 kinds of nodes)
  if (length(.nodes) == 2) {
    .nodes2 <- .nodes[2]
    .nodes <- .nodes[1]
    isTwoMode <- TRUE
  } else {
    .nodes2 <- .nodes
  }


  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessingInit or not
  # enviroment from which get the objects
  envir <- environment()

  effects <- createEffectsFunctions(rhsNames, model, subModel, envir = envir)
  # Get links between objects and effects for printing results
  objectsEffectsLink <- getObjectsEffectsLink(rhsNames)

  ## 2.2 INITIALIZE OBJECTS for preprocessingInit == NULL
  if (is.null(preprocessingInit)) {


    # Initialize events list and link to objects
    events <- getEventsAndObjectsLink(depName, rhsNames, .nodes, .nodes2, envir = envir)[[1]]
    eventsObjectsLink <- getEventsAndObjectsLink(depName, rhsNames, .nodes, .nodes2, envir = envir)[[2]]
    eventsEffectsLink <- getEventsEffectsLink(events, rhsNames, eventsObjectsLink)
  }

  ### 3. PREPROCESS statistics----
  ## 3.1 INITIALIZE OBJECTS for preprocessingInit: remove old effects, add new ones
  if (!is.null(preprocessingInit)) {

    # recover the nodesets
    .nodes  <- preprocessingInit$nodes
    .nodes2 <- preprocessingInit$nodes2
    isTwoMode <- FALSE
    if (!identical(.nodes, .nodes2)) isTwoMode <- TRUE

    # find new effects
    if (min(effectsindexes) == 0) {
      if (!silent) cat("Calculating newly added effects.\n")
      newrhsNames <- rhsNames[which(effectsindexes == 0)]
      newWindowParameters <- windowParameters[which(effectsindexes == 0)]
      neweffects <- createEffectsFunctions(newrhsNames, model, subModel, envir = environment())
      # Get links between objects and effects for printing results
      newobjectsEffectsLink <- getObjectsEffectsLink(newrhsNames)

      # test the objects
      # for now it's easier to just reject formulas that have new objects (and therefore possibly new events)
      # otherwise we need to go in the details of orderEvents, eventTime, eventSender, eventReceiver
      # objectspresent <- rownames(newobjectsEffectsLink) %in% rownames(objectsEffectsLink)
      # if(FALSE %in% objectspresent)
      #  stop("The formula contains new objects or windows that were not taken into account in the
      #         preprocessing used in preprocessingInit.\n
      #       This is likely to affect statistics calculation. Please recalculate the preprocessed object.")

      # Retrieve again the events to calculate new statistics
      newevents <- getEventsAndObjectsLink(depName, newrhsNames, .nodes, .nodes2, envir = envir)[[1]]
      neweventsObjectsLink <- getEventsAndObjectsLink(depName, newrhsNames, .nodes, .nodes2, envir = envir)[[2]]
      neweventsEffectsLink <- getEventsEffectsLink(newevents, newrhsNames, neweventsObjectsLink)

      # Preprocess the new effects
      if (!silent) cat("Pre-processing additional effects.\n")
      newprep <- preprocess(
        model,
        subModel,
        events = newevents,
        effects = neweffects,
        windowParameters = newWindowParameters,
        eventsObjectsLink = neweventsObjectsLink, # for data update
        eventsEffectsLink = neweventsEffectsLink,
        objectsEffectsLink = newobjectsEffectsLink, # for parameterization
        # multipleParameter = multipleParameter,
        nodes = .nodes,
        nodes2 = .nodes2,
        isTwoMode = isTwoMode,
        startTime = preprocessingInit[["startTime"]],
        endTime = preprocessingInit[["endTime"]],
        rightCensored = rightCensored,
        verbose = verbose,
        silent = silent
      )

      # test the length of the dependent and RC updates (in case the events objects was changed in the environment)
      if (length(preprocessingInit$intervals) != length(newprep$intervals)) {
        stop("The numbers of dependent events in the formula and in the preprocessed object are not consistent.\n
           Please check whether these events have changed.")
      }
      if (length(preprocessingInit$rightCensoredIntervals) != length(newprep$rightCensoredIntervals)) {
        stop("The numbers of right-censored events in the formula and in the preprocessed object are not consistent.\n
           Please check whether some windows have been changed.")
      }
    }

    # combine old and new preprocessed objects
    if (!silent) cat("Removing no longer required effects.\n")
    allprep <- preprocessingInit
    allprep$initialStats <- array(0,
      dim = c(
        nrow(get(.nodes)), nrow(get(.nodes2)),
        length(effectsindexes)
      )
    )
    allprep$dependentStatsChange <- list()
    allprep$rightCensoredStatsChange <- list()
    cptold <- 1
    cptnew <- 1

    # initial stats
    for (e in seq_along(effectsindexes)) {
      if (effectsindexes[e] == 0) {
        allprep$initialStats[, , e] <- newprep$initialStats[, , cptnew]
        cptnew <- cptnew + 1
      }
      if (effectsindexes[e] > 0) {
        allprep$initialStats[, , e] <- preprocessingInit$initialStats[, , cptold]
        cptold <- cptold + 1
      }
    }

    # dependent stats updates
    for (t in seq_along(allprep$intervals)) {
      cptold <- 1
      cptnew <- 1
      allprep$dependentStatsChange[[t]] <- lapply(seq_along(effectsindexes), function(x) NULL)
      for (e in seq_along(effectsindexes)) {
        if (effectsindexes[e] == 0) {
          if (!is.null(newprep$dependentStatsChange[[t]][[cptnew]])) {
            allprep$dependentStatsChange[[t]][[e]] <- newprep$dependentStatsChange[[t]][[cptnew]]
          }
          cptnew <- cptnew + 1
        }
        if (effectsindexes[e] > 0) {
          if (!is.null(preprocessingInit$dependentStatsChange[[t]][[cptold]])) {
            allprep$dependentStatsChange[[t]][[e]] <- preprocessingInit$dependentStatsChange[[t]][[cptold]]
          }
          cptold <- cptold + 1
        }
      }
    }

    # right censored stats updates
    if (length(allprep$rightCensoredIntervals) > 0) {
      for (t in seq_along(allprep$rightCensoredIntervals)) {
        cptold <- 1
        cptnew <- 1
        allprep$rightCensoredStatsChange[[t]] <- lapply(seq_along(effectsindexes), function(x) NULL)
        for (e in seq_along(effectsindexes)) {
          if (effectsindexes[e] == 0) {
            if (!is.null(newprep$rightCensoredStatsChange[[t]][[cptnew]])) {
              allprep$rightCensoredStatsChange[[t]][[e]] <- newprep$rightCensoredStatsChange[[t]][[cptnew]]
            }
            cptnew <- cptnew + 1
          }
          if (effectsindexes[e] > 0) {
            if (!is.null(preprocessingInit$rightCensoredStatsChange[[t]][[cptold]])) {
              allprep$rightCensoredStatsChange[[t]][[e]] <- preprocessingInit$rightCensoredStatsChange[[t]][[cptold]]
            }
            cptold <- cptold + 1
          }
        }
      }
    }

    prep <- allprep
    prep$formula <- formula
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
  }

  ## 3.2 PREPROCESS when preprocessingInit == NULL
  if (is.null(preprocessingInit)) {
    if (!silent) cat("Starting preprocessing.\n")
    prep <- preprocess(
      model,
      subModel,
      events = events,
      effects = effects,
      windowParameters = windowParameters,
      eventsObjectsLink = eventsObjectsLink, # for data update
      eventsEffectsLink = eventsEffectsLink,
      objectsEffectsLink = objectsEffectsLink, # for parameterization
      # multipleParameter = multipleParameter,
      nodes = .nodes,
      nodes2 = .nodes2,
      isTwoMode = isTwoMode,
      startTime = estimationInit[["startTime"]],
      endTime = estimationInit[["endTime"]],
      rightCensored = rightCensored,
      verbose = verbose,
      silent = silent
    )
    # The formula, nodes, nodes2 are added to the preprocessed object so that we can call
    # the estimation with preprocessingInit later (for parsing AND composition changes)
    prep$formula <- formula
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
  }

  ## 3.3 Stop here if preprocessingOnly == TRUE
  if (preprocessingOnly) {
    return(prep)
  }


  ### 4. PREPARE PRINTING----
  # functions_utility.R
  effectDescription <- GetDetailPrint(objectsEffectsLink, parsedformula, estimationInit[["fixedParameters"]])
  hasWindows <- attr(effectDescription, "hasWindows")
  if (is.null(hasWindows)) {
    hasWindows <- !all(vapply(windowParameters, is.null, logical(1)))
  }
  attr(effectDescription, "hasWindows") <- NULL
  ### 5. ESTIMATE----
  # CHANGED Alvaro: to match model and subModel new parameters
  if (model == "REM") {
    if (!hasIntercept) {
      modelTypeCall <- "REM-ordered"
    } else {
      modelTypeCall <- "REM"
    }
  } else if (model %in% c("DyNAM", "TriNAM")) {
    if (subModel == "rate" && !hasIntercept) {
      modelTypeCall <- "DyNAM-M-Rate-ordered"
    } else if (subModel == "rate") {
      modelTypeCall <- "DyNAM-M-Rate"
    } else if (subModel == "choice_coordination") {
      modelTypeCall <- "DyNAM-MM"
    } else {
      modelTypeCall <- "DyNAM-M"
    }
  }
  if (!silent) cat("Estimating a model: ", dQuote(model), ", subModel: ", dQuote(subModel), ".\n", sep = "")

  # Old estimation
  if (engine == "old") {
    if (!is.null(estimationInit[["fixedParameters"]])) {
      stop("engine = ", dQuote("old"), " does not support ",
           dQuote("fixedParameters"), " argument", call. = FALSE)
    }

    # use of Marion's fast estimation routine by default
    rowOnly <- colOnly <- F
    if (model %in% c("DyNAM") && subModel %in% c("rate")) colOnly <- T
    if (model %in% c("DyNAM") && subModel %in% c("choice")) rowOnly <- T
    resold <- estimate_old(prep,
      events,
      .nodes,
      .nodes2,
      isTwoMode,
      rightCensored,
      hasIntercept,
      modelType,
      hasWindows,
      parsedformula$ignoreRepParameter,
      parsedformula$defaultNetworkName,
      colOnly,
      estimationInit,
      initialDamping = ifelse(hasWindows, 30, 10),
      parallelize = FALSE,
      cpus = 1,
      effectDescription,
      verbose,
      silent
    )
    resold$call <- callT
    return(resold)
  }

  # Normal estimation
  additionalArgs <- list(
    statsList = prep,
    nodes = get(.nodes),
    nodes2 = get(.nodes2),
    defaultNetworkName = parsedformula$defaultNetworkName,
    addInterceptEffect = hasIntercept,
    modelType = modelTypeCall,
    initialDamping = ifelse(hasWindows, 30, 10),
    parallelize = FALSE,
    cpus = 1,
    verbose = verbose,
    silent = silent,
    ignoreRepParameter = parsedformula$ignoreRepParameter,
    isTwoMode = isTwoMode
  )
  # prefer user-defined arguments
  argsEstimation <- append(
    estimationInit[!(names(estimationInit) %in% c("startTime", "endTime"))],
    additionalArgs[!(names(additionalArgs) %in% names(estimationInit))]
  )

  # use different engine depends on the variable "engine"
  if (engine %in% c("default_c", "gather_compute")) {
    tryCatch(
      result <- do.call("estimate_c_int", args = argsEstimation),
      error = function(e) stop("Error in ", model, " ", subModel, " estimation: ", e, call. = FALSE)
    )
  } else {
    tryCatch(
      result <- do.call("estimate_int", args = argsEstimation),
      error = function(e) stop("Error in ", model, " ", subModel, " estimation: ", e, call. = FALSE)
    )
  }

  ### 6. RESULTS----
  result$names <- effectDescription
  result$formula <- formula
  result$model <- model
  result$subModel <- subModel
  result$rightCensored <- hasIntercept
  result$nParams <- if ("fixed" %in% colnames(effectDescription)) {
    sum(!vapply(effectDescription[, "fixed"], function(x) eval(parse(text = x)), logical(1)))
  } else  length(result$parameters)

  # if (!silent) if (requireNamespace("beepr", quietly = TRUE) & result$convergence[[1]]) beepr::beep(3)

  result$call <- match.call(call = sys.call(-1L),
                            expand.dots = TRUE)
  return(result)
}

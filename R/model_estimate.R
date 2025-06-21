#' Estimate a model
#'
#' Estimates parameters for a dynamic network model via maximum likelihood
#'  implementing the iterative Newton-Raphson procedure as describe in
#'  Stadtfeld and Block (2017).
#'
#' Missing data is handled during the preprocessing stage of the data.
#' The specific imputation strategy depends on the type of data:
#'
#' \itemize{
#'   \item{\strong{Network Data:}}
#'   Missing values in the initial network structure or in linked events
#'   that update network ties are imputed with a value of zero (0).
#'   This explicitly assumes the absence of a tie or event.
#'
#'   \item{\strong{Attribute Covariates:}}
#'   \itemize{
#'     \item{Initial Values:} Missing values for the initial state of an
#'     attribute covariate are replaced by the mean value of that attribute
#'     across all actors.
#'     \item{During Event Updates (via linked events):}
#'     \itemize{
#'       \item{Using `replace`:} If a linked event uses the `replace` variable
#'       to specify a new attribute value and that value is missing, the missing
#'       value is replaced by the mean of the attribute,
#'       excluding the node being updated, at the moment of the event.
#'       \item{Using `increment`:} If a linked event uses the `increment`
#'       variable to specify a change in attribute value and that increment is
#'       missing, the missing value is imputed with a value of zero (0).
#'       This assumes no change occurred.
#'     }
#'   }
#' }
#'
#' @section DyNAM:
#'
#' The actor-oriented models that the goldfish package implements have been
#' called Dynamic Network Actor Models (DyNAMs).
#' The model is a two-step process. In the first step, the waiting time until
#' an actor \eqn{i} initiates the next relational event is modeled
#' (`model = "DyNAM"` and `sub_model = "rate"`) by an exponential
#' distribution depending on the actor activity rate.
#' In the second step, the conditional probability of \eqn{i} choosing
#'  \eqn{j} as the event receiver is modeled (`model = "DyNAM"` and
#'  `sub_model = "choice"`) by a multinomial probability distribution
#'  with a linear predictor.
#' These two-steps are assumed to be conditionally independent given
#' the process state (Stadtfeld, 2012),
#' due to this assumption is possible to estimate these components by
#' different calls of the `estimate` function.
#'
#' @section Waiting times:
#'
#' When DyNAM-rate (`model = "DyNAM"` and `sub_model = "rate"`) model
#' is used to estimate the first step component of the process, or the REM
#' `model = "REM"` model is used.
#' It is important to add a time intercept to model the waiting times between
#' events, in this way the algorithm considers the right-censored intervals
#' in the estimation process.
#'
#' In the case that the intercept is not included in the formula.
#' The model reflects the likelihood of an event being the next in the sequence.
#' This specification is useful for scenarios where the researcher doesn't have
#' access to the exact interevent times.
#' For this ordinal case the likelihood of an event is merely a
#' multinomial probability (Butts, 2008).
#'
#' @param model a character string defining the model type.
#' Current options include `"DyNAM"`, `"DyNAMi"` or `"REM"`.
#' \describe{
#'  \item{DyNAM}{Dynamic Network Actor Models
#'  (Stadtfeld, Hollway and Block, 2017 and Stadtfeld and Block, 2017)}
#'  \item{DyNAMi}{Dynamic Network Actor Models for interactions
#'  (Hoffman et al., 2020)}
#'  \item{REM}{Relational Event Model (Butts, 2008)}
#' }
#' @param sub_model A character string specifying the sub-model to be estimated.
#'  It can be `"rate"` to model the waiting times between events,
#'  `"choice"` to model the choice of the receiver, or `"choice_coordination"`
#'  to model coordination ties. See details.
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
#' @param control_estimation An object of class `control_estimation.goldfish`
#'   (typically created by [control_estimation()]),
#'   specifying parameters for the estimation algorithm.
#' @param control_preprocessing An object of class
#'   `control_preprocessing.goldfish` (typically created by
#'   [control_preprocessing()]),
#'   specifying parameters for data preprocessing. This is only used
#'   if `preprocessing_init` is not a `preprocessed.goldfish` object or NULL.
#' @param preprocessing_init an optional preprocessed object of class
#'  `preprocessed.goldfish` from a previous estimation. When it is provided,
#'  the function will skip the preprocessing of the effects that are already
#'  present in the object and only preprocess the new effects. Default to
#'  `NULL`.
#' @param preprocessing_only logical. If `TRUE`, the function will only run
#'  the preprocessing stage and return an object of class
#'  `preprocessed.goldfish`. Default to `FALSE`.
#' @param verbose logical indicating whether should print
#'   very detailed intermediate results of the iterative Newton-Raphson
#'   procedure; slows down the routine significantly.
#' @param progress logical indicating whether should print a minimal output
#'   to the console of the progress of the preprocessing and
#'   estimation processes.
#' @param x a formula that defines at the left-hand side the dependent
#'   network (see [make_dependent_events()]) and at the right-hand side the
#'   effects and the variables for which the effects are expected to occur
#'   (see `vignette("goldfishEffects")`).
#' @param data a `data.goldfish` object created with [make_data()].
#' It is an environment that contains the nodesets, networks,
#' attributes and dependent events objects. Default to `NULL`.
#'
#' @return returns an object of [class()] `"result.goldfish"`
#' when `preprocessing_only = FALSE` or
#' a preprocessed statistics object of class `"preprocessed.goldfish"`
#' when `preprocessing_only = TRUE`.
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
#'    The second element (\code{maxAbsScore}) reports the final maximum absolute
#'    score in the final iteration.}
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
#'   \item{sub_model}{a character value of the sub_model type.}
#'   \item{rightCensored}{
#'   a logical value indicating if the estimation process considered
#'   right-censored events.
#'   Only it is considered for DyNAM-rate (`model = "DyNAM"` and
#'   `sub_model = "rate"`) or REM (`model = "REM"`) models,
#'   and when the model includes the intercept.}
#'
#' @importFrom stats formula na.omit
#' @export
#' @seealso [make_dependent_events()], [make_global_attribute()],
#'  [make_network()], [make_nodes()], [link_events()]
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
#' callNetwork <- make_network(nodes = actors, directed = TRUE)
#' callNetwork <- link_events(
#'   x = callNetwork, change_event = calls,
#'   nodes = actors
#' )
#' callsDependent <- make_dependent_events(
#'   events = calls, nodes = actors,
#'   default_network = callNetwork
#' )
#'
#' \dontshow{
#' callsDependent <- callsDependent[1:50, ]
#' }
#'
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'   model = "DyNAM", sub_model = "choice",
#'   control_estimation = control_estimation(engine = "default_c")
#' )
#' summary(mod01)
#'
#' # A individual activity rates model
#' mod02 <- estimate(callsDependent ~ 1 + nodeTrans + indeg + outdeg,
#'   model = "DyNAM", sub_model = "rate",
#'   control_estimation = control_estimation(engine = "default_c")
#' )
#' summary(mod02)
#'
#' \donttest{
#' # A multinomial-multinomial choice model for coordination ties
#' data("Fisheries_Treaties_6070")
#' states <- make_nodes(states)
#' states <- link_events(states, sovchanges, attribute = "present")
#' states <- link_events(states, regchanges, attribute = "regime")
#' states <- link_events(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
#'
#' contignet <- make_network(contignet, nodes = states, directed = FALSE)
#' contignet <- link_events(contignet, contigchanges, nodes = states)
#'
#' createBilat <- make_dependent_events(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, default_network = bilatnet
#' )
#'
#' partnerModel <- estimate(
#'   createBilat ~
#'     inertia(bilatnet) +
#'     indeg(bilatnet, ignore_repetitions = TRUE) +
#'     trans(bilatnet, ignore_repetitions = TRUE) +
#'     tie(contignet) +
#'     alter(states$regime) +
#'     diff(states$regime) +
#'     alter(states$gdp) +
#'     diff(states$gdp),
#'   model = "DyNAM", sub_model = "choice_coordination",
#'   control_estimation =
#'     set_estimation_opt(initial_damping = 40, max_iterations = 30)
#' )
#' summary(partnerModel)
#' }
#'
estimate <- function(
    x,
    model = c("DyNAM", "REM", "DyNAMi"),
    sub_model = c("choice", "rate", "choice_coordination"),
    control_estimation = set_estimation_opt(),
    control_preprocessing = set_preprocessing_opt(),
    preprocessing_init = NULL,
    preprocessing_only = FALSE,
    data = NULL, # Expects a data.goldfish object
    progress = getOption("progress"),
    verbose = getOption("verbose")) {
  UseMethod("estimate", x)
}

# First estimation from a formula: can return either a preprocessed object or a
# result object
#' @importFrom stats as.formula
#' @export
estimate.formula <- function(x,
    model = c("DyNAM", "REM", "DyNAMi"),
    sub_model = c("choice", "rate", "choice_coordination"),
    control_estimation = set_estimation_opt(),
    control_preprocessing = set_preprocessing_opt(),
    preprocessing_init = NULL,
    preprocessing_only = FALSE,
    data = NULL, # Expects a data.goldfish object
    progress = getOption("progress"),
    verbose = getOption("verbose")) {

  # Steps:
  # 1. Parse the formula
  # 2. Initialize additional objects
  # 3. Preprocess
  # 4. Estimate model

  ### 0. check parameters----
  model <- match.arg(model)
  sub_model <- match.arg(sub_model)

  ### check model and subModel
  check_model_par(
    model, sub_model,
    model_list = c("DyNAM", "REM", "DyNAMi"),
    sub_model_list = list(
      DyNAM = c("choice", "rate", "choice_coordination"),
      REM = "choice",
      DyNAMi = c("choice", "rate")
    )
  )

  stopifnot(
    inherits(data, "data.goldfish"),
    inherits(preprocessing_only, "logical"),
    inherits(verbose, "logical"),
    is.null(progress) || inherits(progress, "logical"),
    is.null(preprocessing_init) ||
      inherits(preprocessing_init, "preprocessed.goldfish"),
    inherits(control_estimation, "estimation_opt.goldfish"),
    inherits(control_preprocessing, "preprocessing_opt.goldfish")
  )

  if (is.null(progress)) progress <- FALSE

  # gather_compute and default_c don't support returnEventProbabilities
  if (control_estimation$return_probabilities &&
      control_estimation$engine != "default") {
    warning("engine = ", dQuote(control_estimation$engine), " doesn't support",
      dQuote("return_probabilities"),
      ". engine =", dQuote("default"), " is used instead.",
      call. = FALSE, immediate. = TRUE
      )
    control_estimation$engine <- "default"
  }

  # gather_compute and default_c don't support restrictions of opportunity sets
  if (!is.null(control_preprocessing$opportunities_list) &&
      control_estimation$engine != "default") {
    warning("engine = ", dQuote(control_estimation$engine), " doesn't support",
      dQuote("opportunities_list"),
      ". engine =", dQuote("default"), " is used instead.",
      call. = FALSE, immediate. = TRUE
    )
    control_estimation$engine <- "default"
  }


  ### 1. PARSE the formula----
  if (progress) cat("Parsing formula.\n")
  formula <- x

  # Create a working copy of the data environment to avoid side-effects
  work_env <- rlang::env_clone(data)

  ## 1.1 PARSE for all cases: preprocessingInit or not
  parsed_formula <- parse_formula(formula, envir = work_env)
  rhs_names <- parsed_formula$rhs_names
  dep_name <- parsed_formula$dep_name
  has_intercept <- parsed_formula$has_intercept
  window_parameters <- parsed_formula$window_parameters
  ignore_rep_parameter <- unlist(parsed_formula$ignore_rep_parameter)

  # DyNAM-i ONLY: creates extra parameter to differentiate joining and
  # leaving rates, and effect subtypes. Added directly to GetDetailPrint

  # # C implementation doesn't have ignore_repetitions option issue #105
  if (any(unlist(parsed_formula$ignore_rep_parameter)) &&
      control_estimation$engine %in% c("default_c", "gather_compute")) {
    warning("engine = ", dQuote(control_estimation$engine),
            " doesn't support ignore_repetitions effects. engine =",
            dQuote("default"), " is used instead.",
      call. = FALSE, immediate. = TRUE
    )
    control_estimation$engine <- "default"
  }
  # Model-specific preprocessing initialization
  if (has_intercept && model %in% c("DyNAM", "DyNAMi") &&
    sub_model %in% c("choice", "choice_coordination")) {
    warning("Model ", dQuote(model), " sub_model ", dQuote(sub_model),
            " ignores the time intercept.",
      call. = FALSE, immediate. = TRUE
    )
    parsed_formula$has_intercept <- has_intercept <- FALSE
  }
  rightCensored <- has_intercept

  if (progress &&
    !(model %in% c("DyNAM", "DyNAMi") &&
      sub_model %in% c("choice", "choice_coordination"))) {
    cat(
      ifelse(has_intercept, "T", "No t"), "ime intercept added.\n",
      sep = ""
    )
  }
  # if (progress && !all(vapply(windowParameters, is.null, logical(1))))
  #   cat("Creating window objects in global environment.")

  ## 1.2 PARSE for preprocessingInit: check the formula consistency
  if (!is.null(preprocessing_init)) {
    # find the old and new effects indexes, do basic consistency checks
    old_parsed_formula <- parse_formula(
      preprocessing_init$formula, envir = work_env
    )
    effects_indexes <- compare_formulas(
      old_parsed_formula = old_parsed_formula,
      new_parsed_formula = parsed_formula,
      model = model, sub_model = sub_model
    )
  }

  ### 2. INITIALIZE OBJECTS: effects, nodes, and link objects----

  if (progress) cat("Initializing objects.\n")

  ## 2.0 Set is_two_mode to define effects functions
  # get node sets of dependent variable
  .nodes <- attr(get(dep_name, envir = work_env), "nodes")
  is_two_mode <- FALSE
  if (length(.nodes) == 2) {
    .nodes2 <- .nodes[2]
    .nodes <- .nodes[1]
    is_two_mode <- TRUE
  } else {
    .nodes2 <- .nodes
  }

  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessingInit or not
  # enviroment from which get the objects

  effects <- create_effects_functions(
    rhs_names, model, sub_model,
    envir = work_env
  )
  objects_effects_link <- get_objects_effects_link(rhs_names)

  ## 2.2 INITIALIZE OBJECTS for preprocessing_init == NULL
  if (is.null(preprocessing_init)) {
    # Initialize events list and link to objects
    events <- get_events_and_objects_link(
      dep_name, rhs_names, .nodes, .nodes2,
      envir = work_env
    )[[1]]
    events_objects_link <- get_events_and_objects_link(
      dep_name, rhs_names, .nodes, .nodes2,
      envir = work_env
    )[[2]]
    events_effects_link <- get_events_effects_link(
      events, rhs_names, events_objects_link
    )
  }

  # DyNAM-i ONLY: extra cleaning step
  # we assign an extra class to the windowed events,
  # and remove leaving events for the choice estimation
  if (model == "DyNAMi") {
    events <- cleanInteractionEvents(
      events, events_effects_link, window_parameters, sub_model, dep_name,
      events_objects_link,
      envir = work_env
    )
  }

  ### 3. PREPROCESS statistics----
  ## 3.1 INITIALIZE OBJECTS for preprocessingInit: remove old effects,
  ## add new ones
  if (!is.null(preprocessing_init)) {
    # recover the nodesets
    .nodes <- preprocessing_init$nodes
    .nodes2 <- preprocessing_init$nodes2
    is_two_mode <- FALSE
    if (!identical(.nodes, .nodes2)) is_two_mode <- TRUE

    # find new effects
    if (min(effects_indexes) == 0) {
      if (progress) cat("Calculating newly added effects.\n")
      new_rhs_names <- rhs_names[which(effects_indexes == 0)]
      new_window_parameters <- window_parameters[which(effects_indexes == 0)]
      new_effects <- create_effects_functions(
        new_rhs_names, model, sub_model,
        envir = work_env
      )
      new_objects_effects_link <- get_objects_effects_link(new_rhs_names)
      new_events <- get_events_and_objects_link(
        dep_name, new_rhs_names, .nodes, .nodes2,
        envir = work_env
      )[[1]]
      new_events_objects_link <- get_events_and_objects_link(
        dep_name, new_rhs_names, .nodes, .nodes2,
        envir = work_env
      )[[2]]
      new_events_effects_link <- get_events_effects_link(
        new_events, new_rhs_names, new_events_objects_link
      )

      # Preprocess the new effects
      if (progress) cat("Pre-processing additional effects.\n")
      newprep <- preprocess(
        model,
        sub_model,
        events = new_events,
        effects = new_effects,
        windowParameters = new_window_parameters,
        ignoreRepParameter = ignore_rep_parameter,
        eventsObjectsLink = new_events_objects_link, # for data update
        eventsEffectsLink = new_events_effects_link,
        objectsEffectsLink = new_objects_effects_link, # for parameterization
        # multipleParameter = multipleParameter,
        nodes = .nodes,
        nodes2 = .nodes2,
        is_two_mode = is_two_mode,
        startTime = control_preprocessing$start_time,
        endTime = control_preprocessing$end_time,
        rightCensored = rightCensored,
        progress = progress,
        prepEnvir = work_env
      )

      # test the length of the dependent and RC updates (in case the events
      #   objects was changed in the environment)
      if (length(preprocessing_init$intervals) != length(newprep$intervals)) {
        stop(
          "The numbers of dependent events in the formula and in the ",
          "preprocessed object are not consistent.\n",
          "\tPlease check whether these events have changed.",
          call. = FALSE
        )
      }

      if (length(preprocessing_init$rightCensoredIntervals) !=
        length(newprep$rightCensoredIntervals)) {
        stop(
          "The numbers of right-censored events in the formula and in the ",
          "preprocessed object are not consistent.\n",
          "\tPlease check whether some windows have been changed.",
          call. = FALSE
        )
      }
    }

    # combine old and new preprocessed objects
    if (progress) cat("Removing no longer required effects.\n")
    allprep <- preprocessing_init
    allprep$initialStats <- array(0,
      dim = c(
        nrow(get(.nodes, envir = work_env)),
        nrow(get(.nodes2, envir = work_env)),
        length(effects_indexes)
      )
    )
    allprep$dependentStatsChange <- list()
    allprep$rightCensoredStatsChange <- list()
    cptnew <- 1

    # initial stats
    for (e in seq_along(effects_indexes)) {
      if (effects_indexes[e] == 0) {
        allprep$initialStats[, , e] <- newprep$initialStats[, , cptnew]
        cptnew <- cptnew + 1
      }
      if (effects_indexes[e] > 0) {
        allprep$initialStats[, , e] <-
          preprocessing_init$initialStats[, , effects_indexes[e]]
      }
    }

    # dependent stats updates
    for (t in seq_along(preprocessing_init$dependentStatsChange)) {
      cptnew <- 1
      allprep$dependentStatsChange[[t]] <-
        lapply(seq_along(effects_indexes), function(x) NULL)
      for (e in seq_along(effects_indexes)) {
        if (effects_indexes[e] == 0) {
          if (!is.null(newprep$dependentStatsChange[[t]][[cptnew]])) {
            allprep$dependentStatsChange[[t]][[e]] <-
              newprep$dependentStatsChange[[t]][[cptnew]]
          }
          cptnew <- cptnew + 1
        }
        if (effects_indexes[e] > 0) {
          if (
            !is.null(
              preprocessing_init$dependentStatsChange[[t]][[effects_indexes[e]]]
            )
          ) {
            allprep$dependentStatsChange[[t]][[e]] <-
              preprocessing_init$dependentStatsChange[[t]][[effects_indexes[e]]]
          }
        }
      }
    }

    # right censored stats updates
    if (length(preprocessing_init$rightCensoredIntervals) > 0) {
      for (t in seq_along(preprocessing_init$rightCensoredIntervals)) {
        cptnew <- 1
        allprep$rightCensoredStatsChange[[t]] <-
          lapply(seq_along(effects_indexes), function(x) NULL)
        for (e in seq_along(effects_indexes)) {
          if (effects_indexes[e] == 0) {
            if (!is.null(newprep$rightCensoredStatsChange[[t]][[cptnew]])) {
              allprep$rightCensoredStatsChange[[t]][[e]] <-
                newprep$rightCensoredStatsChange[[t]][[cptnew]]
            }
            cptnew <- cptnew + 1
          }
          if (effects_indexes[e] > 0) {
            if (
              !is.null(
                preprocessing_init$rightCensoredStatsChange[[t]][[
                  effects_indexes[e]
                ]]
      )
            ) {
              allprep$rightCensoredStatsChange[[t]][[e]] <-
                preprocessing_init$rightCensoredStatsChange[[t]][[
                  effects_indexes[e]
                ]]
            }
          }
        }
      }
    }

    prep <- allprep
    prep$formula <- formula
    prep$model <- model
    prep$subModel <- sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
  }

  ## 3.2 PREPROCESS when preprocessingInit == NULL
  if (is.null(preprocessing_init)) {
    if (progress) cat("Starting preprocessing.\n")
    if (model == "DyNAMi") {
      prep <- preprocessInteraction(
        subModel = sub_model,
        events = events,
        effects = effects,
        eventsObjectsLink = events_objects_link,
        eventsEffectsLink = events_effects_link,
        objectsEffectsLink = objects_effects_link,
        # multipleParameter,
        nodes = .nodes,
        nodes2 = .nodes2,
        rightCensored = rightCensored,
        progress = progress,
        groupsNetwork = parsed_formula$default_network_name,
        prepEnvir = work_env
      )
  } else {
    prep <- preprocess(
        model = model,
        subModel = sub_model,
        events = events,
        effects = effects,
        windowParameters = window_parameters,
        ignoreRepParameter = ignore_rep_parameter,
        eventsObjectsLink = events_objects_link, # for data update
        eventsEffectsLink = events_effects_link,
        objectsEffectsLink = objects_effects_link, # for parameterization
        # multipleParameter = multipleParameter,
        nodes = .nodes,
        nodes2 = .nodes2,
        is_two_mode = is_two_mode,
        startTime = control_preprocessing$start_time,
        endTime = control_preprocessing$end_time,
        rightCensored = rightCensored,
        progress = progress,
        prepEnvir = work_env
      )
    }
    # The formula, nodes, nodes2 are added to the preprocessed object so that
    # we can call the estimation with preprocessingInit later
    # (for parsing AND composition changes)
    prep$formula <- formula
    prep$model <- model
    prep$subModel <- sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
  }

  ## 3.3 Stop here if preprocessingOnly == TRUE
  if (preprocessing_only) {
    return(prep)
  }

  ### 4. PREPARE PRINTING----
  # functions_utility.R
  effectDescription <-
    GetDetailPrint(
      objects_effects_link, parsed_formula,
      control_estimation$fixed_parameters 
    )
  hasWindows <- attr(effectDescription, "hasWindows")
  if (is.null(hasWindows)) {
    hasWindows <- !all(vapply(window_parameters, is.null, logical(1)))
  }
  attr(effectDescription, "hasWindows") <- NULL
  ### 5. ESTIMATE----
  # CHANGED Alvaro: to match model and subModel new parameters
  if (model == "REM") {
    if (!has_intercept) {
      modelTypeCall <- "REM-ordered"
    } else {
      modelTypeCall <- "REM"
    }
  } else if (model %in% c("DyNAM", "DyNAMi")) {
    if (sub_model == "rate" && !has_intercept) {
      modelTypeCall <- "DyNAM-M-Rate-ordered"
    } else if (sub_model == "rate") {
      modelTypeCall <- "DyNAM-M-Rate"
    } else if (sub_model == "choice_coordination") {
      modelTypeCall <- "DyNAM-MM"
    } else {
      modelTypeCall <- "DyNAM-M"
    }
  }
  if (progress) {
    cat(
      "Estimating a model: ", dQuote(model), ", subModel: ",
      dQuote(sub_model), ".\n",
      sep = ""
    )
  }

  EstimateEnvir <- rlang::env_clone(data)

  argsEstimation <- list(
    initialParameters = control_estimation$initial_parameters,
    fixedParameters = control_estimation$fixed_parameters,
    maxIterations = as.integer(control_estimation$max_iterations),
    maxScoreStopCriterion = control_estimation$convergence_criterion,
    dampingIncreaseFactor = control_estimation$damping_increase_factor,
    dampingDecreaseFactor = control_estimation$damping_decrease_factor,
    returnEventProbabilities = control_estimation$return_probabilities,
    returnIntervalLogL = control_estimation$return_interval_loglik,
    statsList = prep,
    nodes = get(.nodes, envir = data),
    nodes2 = get(.nodes2, envir = data),
    defaultNetworkName = parsed_formula$default_network_name,
    hasIntercept = has_intercept,
    modelType = modelTypeCall,
    # overridden damping
    initialDamping = if(!is.null(control_estimation$initial_damping)) {
      control_estimation$initial_damping
    } else {
      ifelse(hasWindows, 30, 10)
    },
    parallelize = FALSE,
    cpus = 1,
    verbose = verbose,
    progress = progress,
    ignoreRepParameter = ignore_rep_parameter,
    prepEnvir = EstimateEnvir
  )

  # Call the appropriate estimation engine
  if (control_estimation$engine %in% c("default_c", "gather_compute")) {
    tryCatch(
      result <- do.call(
        "estimate_c_int",
        args = c(argsEstimation, list(engine = control_estimation$engine))
      ),
      error = \(e) {
        stop("For ", model, " ", subModel,
          " estimation:\n\t", e$message,
          call. = FALSE
        )
      }
    )
  } else {
    tryCatch(
      result <- do.call("estimate_int", args = argsEstimation),
      error = \(e) {
        stop("For ", model, " ", subModel,
          " estimation:\n\t", e$message,
          call. = FALSE
        )
      }
    )
  }

  ### 6. RESULTS----
  result$names <- effectDescription
  formulaKeep <- as.formula(Reduce(paste, deparse(formula)),
    env = new.env(parent = emptyenv())
  )
  result$formula <- formulaKeep
  result$model <- model
  result$subModel <- sub_model
  result$rightCensored <- has_intercept
  result$nParams <- sum(!GetFixed(result))
  result$call <- match.call(
    call = sys.call(-1L),
    expand.dots = TRUE
  )
  result$call[[2]] <- formulaKeep

  return(result)
}

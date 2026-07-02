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
#'     \item{Initial Values:} Missing numeric values for the initial state of
#'     an attribute covariate are replaced by the mean value of that attribute
#'     across all actors, and categorical values are replaced by the mode value.
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
#' @section Models:
#' Currently there are implemented the following models:
#' \describe{
#'  \item{DyNAM}{Dynamic Network Actor Models, \code{estimate_dynam()},
#'  modeling a sequence of relational events as an actor-oriented process
#'  (Stadtfeld, Hollway and Block, 2017 and Stadtfeld and Block, 2017)}
#'  \item{DyNAMi}{Dynamic Network Actor Models for interactions,
#'  \code{estimate_dynami()}, modeling face-to-face interactions as an
#'  actor oriented process
#'  (Hoffman et al., 2020)}
#'  \item{REM}{Relational Event Model, \code{estimate_rem()},
#'  modeling a sequence of relational events as a tie-oriented process
#'  (Butts, 2008).}
#' }
#'
#' @section DyNAM:
#'
#' The actor-oriented models that the goldfish package implements,
#' `estimate_dynam()`, have been
#' called Dynamic Network Actor Models (DyNAMs).
#' The model is a two-step process. In the first step, the waiting time until
#' an actor \eqn{i} initiates the next relational event is modeled
#' (`sub_model = "rate"`) by an exponential distribution depending on
#' the actor activity rate.
#' In the second step, the conditional probability of \eqn{i} choosing
#'  \eqn{j} as the event receiver is modeled (`sub_model = "choice"`)
#'  by a multinomial probability distribution with a linear predictor.
#' These two-steps are assumed to be conditionally independent given
#' the process state (Stadtfeld, 2012),
#' due to this assumption is possible to estimate these components by
#' different calls of the `estimate_dynam()` function.
#'
#' @section Waiting times:
#'
#' When DyNAM-rate (`estimate_dynam(x, sub_model = "rate")`) model
#' is used to estimate the first step component of the process, or the REM
#' `estimate_rem(x, model = "REM")` model is used.
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
#' @param sub_model A character string specifying the sub-model to be estimated.
#'  It can be `"rate"` to model the waiting times between events,
#'  `"rate_ordered"` to model only the order of the events,
#'  `"choice"` to model the choice of the receiver, or `"choice_coordination"`
#'  to model coordination ties. See details.
#' \describe{
#'  \item{choice}{a multinomial receiver choice model `estimate_dynam()`
#'  (Stadtfeld and Block, 2017).
#'  A multinomial group choice model `estimate_dynami()` (Hoffman et al., 2020)}
#'  \item{choice_coordination}{a multinomial-multinomial model for coordination
#'  ties `estimate_dynam()` (Stadtfeld, Hollway and Block, 2017)}
#'  \item{rate}{A individual activity rates model `estimate_dynam()`
#'  (Stadtfeld and Block, 2017).
#'  Two rate models, one for individuals joining groups and one for individuals
#'  leaving groups, jointly estimated `estimate_dynami()`(Hoffman et al., 2020)}
#'  \item{rate_ordered}{An individual activity rates model where only the
#'  order of the events is modeled (ordinal case, partial likelihood as in
#'  the CoxPH model). It replaces the previous specification of this model:
#'  `sub_model = "rate"` with a formula without the time intercept.}
#' }
#' For `estimate_rem()` the valid values are `"rate"` (full dyadic hazard
#' model, the default) and `"rate_ordered"` (only the order of the events is
#' modeled); `"choice"` is kept as a deprecated alias of `"rate"`.
#' @param control_estimation An object of class `control_estimation.goldfish`
#'   (typically created by [set_estimation_opt()]),
#'   specifying parameters for the estimation algorithm.
#' @param control_preprocessing An object of class
#'   `control_preprocessing.goldfish` (typically created by
#'   [set_preprocessing_opt()]),
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
#'   \item{convergence}{a list reporting the convergence of the
#'    Newton-Raphson procedure: \code{isConverged} (logical),
#'    \code{returnCode} (\code{1} gradient close to zero, \code{2} step size
#'    close to zero, \code{0} not converged), \code{maxAbsScore} (final maximum
#'    absolute score), \code{maxAbsUpdate} (final step, the maximum absolute
#'    parameter update), and \code{score_rel_norm} (the likelihood-scaled
#'    relative gradient norm compared against \code{score_tol}).}
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
#'   Only it is considered for `estimate_dynam(x, sub_model = "rate")` or
#'   REM (`estimate_rem()`), when the model includes the intercept.}
#'   \item{rightCensoredEvents}{a logical vector indicating whether or not an
#'   event is a right censored event}
#'   \item{eventTimes}{
#'   a numerical vector of times of events (including right censored events)
#'   }
#'
#' @importFrom stats formula na.omit
#' @name estimate
#' @seealso [make_dependent_events()], [make_global_attributes()],
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
#' # A DyNAM modeling rate and choice steps
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
#' socialEvData <- make_data(callsDependent, callNetwork, call, actors)
#'
#' mod01 <- estimate_dynam(callsDependent ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = socialEvData,
#'   control_estimation = set_estimation_opt(engine = "gather_compute")
#' )
#' summary(mod01)
#'
#' # A individual activity rates model
#' mod02 <- estimate_dynam(callsDependent ~ 1 + node_trans + indeg + outdeg,
#'   sub_model = "rate",
#'   data = socialEvData,
#'   control_estimation = set_estimation_opt(engine = "gather_compute")
#' )
#' summary(mod02)
#'
#' # A REM
#'
#' mod03 <- estimate_rem(
#'   callsDependent ~ 1 + node_trans(callNetwork, type = "ego") +
#'     indeg(callNetwork, type = "ego") + outdeg(callNetwork, type = "ego") +
#'     inertia + recip + trans,
#'     data = socialEvData,
#'     control_estimation = set_estimation_opt(engine = "gather_compute")
#' )
#' summary(mod03)
#'
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
#' fisheriesData <- make_data(
#'   createBilat, contignet, bilatnet, contigchanges, bilatchanges,
#'   states, sovchanges, regchanges, gdpchanges
#'  )
#' partnerModel <- estimate_dynam(
#'   createBilat ~
#'     inertia(bilatnet) +
#'     indeg(bilatnet) +
#'     trans(bilatnet) +
#'     tie(contignet) +
#'     alter(states$regime) +
#'     diff(states$regime) +
#'     alter(states$gdp) +
#'     diff(states$gdp),
#'   sub_model = "choice_coordination",
#'   data = fisheriesData,
#'   control_estimation =
#'     set_estimation_opt(
#'       initial_damping = 40, max_iterations = 30,
#'       engine = "default"
#'     )
#' )
#' summary(partnerModel)
#' }
#'
NULL

#' @rdname estimate
#' @export
estimate_dynam <- function(
  x,
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  data = NULL,
  control_estimation = set_estimation_opt(),
  control_preprocessing = set_preprocessing_opt(),
  preprocessing_init = NULL,
  preprocessing_only = FALSE,
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE)
) {
  sub_model <- match.arg(sub_model)
  if (inherits(x, "specification.goldfish")) {
    return(estimate_from_specification(
      spec = x,
      model = "DyNAM",
      sub_model = sub_model,
      data = data,
      control_estimation = control_estimation,
      control_preprocessing = control_preprocessing,
      preprocessing_init = preprocessing_init,
      preprocessing_only = preprocessing_only,
      progress = progress,
      verbose = verbose
    ))
  }
  estimate_wrapper(
    x = x,
    model = "DyNAM",
    sub_model = sub_model,
    data = data,
    control_estimation = control_estimation,
    control_preprocessing = control_preprocessing,
    preprocessing_init = preprocessing_init,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose
  )
}

#' @rdname estimate
#' @export
estimate_dynami <- function(
  x,
  sub_model = c("choice", "rate"),
  data = NULL,
  control_estimation = set_estimation_opt(),
  control_preprocessing = set_preprocessing_opt(),
  preprocessing_init = NULL,
  preprocessing_only = FALSE,
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE)
) {
  sub_model <- match.arg(sub_model)
  estimate_wrapper(
    x = x,
    model = "DyNAMi",
    sub_model = sub_model,
    data = data,
    control_estimation = control_estimation,
    control_preprocessing = control_preprocessing,
    preprocessing_init = preprocessing_init,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose
  )
}

#' @rdname estimate
#' @export
estimate_rem <- function(
  x,
  sub_model = c("rate", "rate_ordered", "choice"),
  data = NULL,
  control_estimation = set_estimation_opt(),
  control_preprocessing = set_preprocessing_opt(),
  preprocessing_init = NULL,
  preprocessing_only = FALSE,
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE)
) {
  sub_model <- match.arg(sub_model)
  if (inherits(x, "specification.goldfish")) {
    return(estimate_from_specification(
      spec = x,
      model = "REM",
      sub_model = sub_model,
      data = data,
      control_estimation = control_estimation,
      control_preprocessing = control_preprocessing,
      preprocessing_init = preprocessing_init,
      preprocessing_only = preprocessing_only,
      progress = progress,
      verbose = verbose
    ))
  }
  estimate_wrapper(
    x = x,
    model = "REM",
    sub_model = sub_model,
    data = data,
    control_estimation = control_estimation,
    control_preprocessing = control_preprocessing,
    preprocessing_init = preprocessing_init,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose
  )
}

# Estimate from a specification.goldfish object (task 4.4). Selects the submodel
# bundle matching the requested sub_model's family (rate vs choice), reuses its
# parsed formula bundle so estimation does not re-parse, and forwards to the
# shared estimator with the bundle's own sub_model. Results are identical to
# estimating the equivalent `layer ~ rhs` formula.
estimate_from_specification <- function(
  spec,
  model,
  sub_model,
  data = NULL,
  control_estimation,
  control_preprocessing,
  preprocessing_init,
  preprocessing_only,
  progress,
  verbose
) {
  if (!identical(spec$model, model)) {
    cli::cli_abort(c(
      "This specification is for model {.val {spec$model}}.",
      "x" = "It cannot be estimated with {.fn {paste0('estimate_',
             tolower(model))}}."
    ))
  }
  family <- if (sub_model %in% c("rate", "rate_ordered")) "rate" else "choice"
  bundle <- spec$submodels[[family]]
  if (is.null(bundle)) {
    cli::cli_abort(c(
      "This specification has no {.field {family}} sub-model.",
      "i" = "Available sub-model{?s}: {.field {names(spec$submodels)}}."
    ))
  }

  est_data <- if (is.null(data)) spec$data else data
  # Reuse the parsed bundle only when it matches the data it was parsed against
  # and no incremental preprocessing_init re-parse is involved; otherwise let the
  # wrapper parse afresh against the supplied data.
  reuse_parsed <- is.null(preprocessing_init) &&
    (is.null(data) || identical(data, spec$data))

  estimate_wrapper(
    x = bundle$formula,
    model = model,
    sub_model = bundle$sub_model,
    data = est_data,
    control_estimation = control_estimation,
    control_preprocessing = control_preprocessing,
    preprocessing_init = preprocessing_init,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose,
    parsed_formula = if (reuse_parsed) bundle$parsed else NULL
  )
}

#' Compute preprocessed statistics for a model
#'
#' Runs the preprocessing stage of a model and returns the change statistics
#' of the effects for the event sequence, without estimating the model.
#' The returned object can be passed to the estimation functions
#' ([estimate_dynam()], [estimate_rem()], [estimate_dynami()]) through their
#' `preprocessing_init` argument, or used directly by users who want to
#' work with the sufficient statistics of a model.
#'
#' @param formula a formula that defines at the left-hand side the dependent
#'   network (see [make_dependent_events()]) and at the right-hand side the
#'   effects and the variables for which the effects are expected to occur
#'   (see `vignette("goldfishEffects")`).
#' @param data a `data.goldfish` object created with [make_data()].
#' @param model a character string specifying the model. Current options are
#'   `"DyNAM"`, `"REM"` or `"DyNAMi"`, see [estimate_dynam()],
#'   [estimate_rem()] and [estimate_dynami()].
#' @param sub_model a character string specifying the sub-model, see
#'   [estimate_dynam()]. The default value `NULL` resolves to `"rate"` for
#'   `model = "REM"` and `"choice"` otherwise.
#' @param output a character string specifying the output format of the
#'   preprocessed statistics. `"default"` returns the estimation-ready
#'   `preprocessed.goldfish` object; `"gather"` returns the gather stack (one
#'   row per event x alternative, as in [gather_model_data()]); `"db"` streams
#'   the gather rows to the database table configured via
#'   [set_preprocessing_opt()] (`db` / `db_table`) and returns a descriptor.
#' @param ... additional arguments passed to the preprocessing stage, e.g.,
#'   `control_preprocessing` (see [set_preprocessing_opt()]) and `progress`.
#'
#' @return an object of class `"preprocessed.goldfish"` with the change
#'   statistics of the effects for the event sequence and the information
#'   of the model variant computed. See the `Value` section of
#'   [estimate_dynam()] for the `preprocessing_only = TRUE` case.
#'
#' @seealso [estimate_dynam()], [estimate_rem()], [estimate_dynami()],
#'   [set_preprocessing_opt()]
#' @export
#' @examples
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors, directed = TRUE)
#' callNetwork <- link_events(
#'   x = callNetwork, change_event = calls, nodes = actors
#' )
#' callsDependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = callNetwork
#' )
#' \dontshow{
#' callsDependent <- callsDependent[1:50, ]
#' }
#' socialEvData <- make_data(callsDependent, callNetwork, calls, actors)
#'
#' prep <- compute_stats(
#'   callsDependent ~ inertia + recip + trans,
#'   data = socialEvData,
#'   model = "DyNAM", sub_model = "choice"
#' )
#' prep
compute_stats <- function(
  formula,
  data,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = NULL,
  output = c("default", "gather", "db"),
  ...
) {
  model <- match.arg(model)
  if (is.null(sub_model)) {
    sub_model <- if (model == "REM") "rate" else "choice"
  }
  output <- match.arg(output)
  estimate_wrapper(
    x = formula,
    model = model,
    sub_model = sub_model,
    data = data,
    output = output,
    preprocessing_only = TRUE,
    ...
  )
}

#' Recipe (DyNAM/REM) preprocessing front-end
#'
#' Compiles the `spec_map` upfront (design D8) and runs the shared recipe loop
#' via `preprocess(spec_map, …)`. Returns both `prep` and the `spec_map` (the
#' latter carries the single-source-of-truth `effect_description` consumed by the
#' printing step). Isolated from the DyNAMi front-end (task 2.3e) so the shared
#' estimate path carries no model conditionals in its preprocessing.
#'
#' @return a list with `prep` (preprocessed.goldfish) and `spec_map`.
#' @noRd
preprocess_recipe <- function(
  parsed_formula,
  model_spec,
  effects,
  window_parameters,
  objects_effects_link,
  events_objects_link,
  events_effects_link,
  fetch_plan,
  control_preprocessing,
  progress,
  work_env,
  writer = writer_default()
) {
  spec_map <- build_spec_map(
    parsed_formula,
    model_spec,
    effects,
    window_parameters,
    objects_effects_link,
    events_objects_link,
    events_effects_link,
    fetch_plan,
    envir = work_env
  )
  # The recipe loop realizes derived inputs (from plan$derivations) and fetches
  # events (from spec$fetch_plan) inside state creation (design D8, task 2.3f),
  # so no pre-fetched events list is threaded here.
  prep <- preprocess(
    spec_map,
    startTime = control_preprocessing$start_time,
    endTime = control_preprocessing$end_time,
    progress = progress,
    prepEnvir = work_env,
    writer = writer
  )
  list(prep = prep, spec_map = spec_map)
}

#' DyNAMi preprocessing front-end (isolated)
#'
#' The DyNAMi-only path: the extra `cleanInteractionEvents` cleaning step (window
#' class tagging + leaving-event removal for the choice estimation) followed by
#' the monolithic `preprocessInteraction` loop via `preprocess.dynami_*`. It keeps
#' the bridge-argument signature (no `spec_map`); the recipe/`spec_map` unification
#' is deferred to `refactor-dynami-engine`. Fenced here (task 2.3e) so the shared
#' recipe path (`preprocess_recipe()`) is DyNAMi-free.
#'
#' @return a preprocessed.goldfish object.
#' @noRd
preprocess_dynami <- function(
  model_spec,
  events,
  effects,
  window_parameters,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  sub_model,
  dep_name,
  nodes,
  nodes2,
  is_two_mode,
  ignore_rep_parameter,
  rightCensored,
  control_preprocessing,
  parsed_formula,
  progress,
  work_env,
  writer = writer_default()
) {
  # DyNAM-i ONLY: assign an extra class to the windowed events and remove
  # leaving events for the choice estimation.
  events <- cleanInteractionEvents(
    events,
    events_effects_link,
    window_parameters,
    sub_model,
    dep_name,
    events_objects_link,
    envir = work_env
  )
  preprocess(
    model_spec,
    events = events,
    effects = effects,
    windowParameters = window_parameters,
    ignoreRepParameter = ignore_rep_parameter,
    eventsObjectsLink = events_objects_link, # for data update
    eventsEffectsLink = events_effects_link,
    objectsEffectsLink = objects_effects_link, # for parameterization
    nodes = nodes,
    nodes2 = nodes2,
    is_two_mode = is_two_mode,
    startTime = control_preprocessing$start_time,
    endTime = control_preprocessing$end_time,
    rightCensored = rightCensored,
    opportunitiesList = control_preprocessing$opportunities_list,
    progress = progress,
    groupsNetwork = parsed_formula$default_network_name,
    prepEnvir = work_env,
    writer = writer
  )
}

# First estimation from a formula: can return either a preprocessed object or a
# result object
#' @importFrom stats as.formula
#' @noRd
estimate_wrapper <- function(
  x,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  data = NULL,
  control_estimation = set_estimation_opt(),
  control_preprocessing = set_preprocessing_opt(),
  preprocessing_init = NULL,
  preprocessing_only = FALSE,
  output = c("default", "gather", "db"),
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE),
  max_length = 63L,
  parsed_formula = NULL
) {
  output <- match.arg(output)

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
    model,
    sub_model,
    model_list = c("DyNAM", "REM", "DyNAMi"),
    sub_model_list = list(
      DyNAM = c("rate", "rate_ordered", "choice", "choice_coordination"),
      REM = c("rate", "rate_ordered", "choice"),
      DyNAMi = c("choice", "rate")
    )
  )

  if (model == "REM" && sub_model == "choice") {
    cli::cli_warn(c(
      "!" = "{.code sub_model = \"choice\"} is deprecated for REM models.",
      "i" = "Use {.code sub_model = \"rate\"} instead; REM models the rate
             of dyadic events."
    ))
    sub_model <- "rate"
  }

  stopifnot(
    inherits(data, "data.goldfish"),
    rlang::is_scalar_logical(preprocessing_only),
    rlang::is_scalar_logical(verbose),
    is.null(progress) || rlang::is_scalar_logical(progress),
    is.null(preprocessing_init) ||
      inherits(preprocessing_init, "preprocessed.goldfish"),
    inherits(control_estimation, "estimation_opt.goldfish"),
    inherits(control_preprocessing, "preprocessing_opt.goldfish")
  )

  if (is.null(progress)) {
    progress <- FALSE
  }

  if (
    !is.null(preprocessing_init) &&
      !identical(preprocessing_init$version, PREPROCESSED_GOLDFISH_VERSION)
  ) {
    cli::cli_abort(c(
      "The {.arg preprocessing_init} object uses an outdated preprocessing
       format.",
      "x" = "Objects preprocessed with a previous goldfish version cannot be
             reused for estimation.",
      "i" = "Recompute the preprocessing object with {.fn compute_stats}."
    ))
  }

  # gather_compute and default_c don't support returnEventProbabilities
  if (
    control_estimation$return_probabilities &&
      control_estimation$engine != "default"
  ) {
    warning(
      "engine = ",
      dQuote(control_estimation$engine),
      " doesn't support",
      dQuote("return_probabilities"),
      ". engine =",
      dQuote("default"),
      " is used instead.",
      call. = FALSE,
      immediate. = TRUE
    )
    control_estimation$engine <- "default"
  }

  # gather_compute and default_c don't support restrictions of opportunity sets
  if (
    !is.null(control_preprocessing$opportunities_list) &&
      control_estimation$engine != "default"
  ) {
    warning(
      "engine = ",
      dQuote(control_estimation$engine),
      " doesn't support",
      dQuote("opportunities_list"),
      ". engine =",
      dQuote("default"),
      " is used instead.",
      call. = FALSE,
      immediate. = TRUE
    )
    control_estimation$engine <- "default"
  }

  ### 1. PARSE the formula----
  if (progress) {
    cat("Parsing formula.\n")
  }
  formula <- x

  # Create a working copy of the data environment to avoid side-effects
  work_env <- rlang::env_clone(data)

  ## 1.1 PARSE for all cases: preprocessingInit or not
  # On the fresh recipe (DyNAM/REM) path the shared parser stays free of
  # environment mutations (design D8): parse_formula() records the window
  # derivation recipe but does not realize it, and the recipe state container
  # realizes it from `plan$derivations` (task 2.3f). DyNAMi and the
  # preprocessing_init path keep the eager parse-time realization
  # (byte-identical) via realize_windows = TRUE.
  recipe_deferred_windows <- model %in%
    c("DyNAM", "REM") &&
    is.null(preprocessing_init)
  # A specification.goldfish object supplies its parsed bundle so estimation
  # reuses it rather than re-parsing (task 4.4); it was parsed with the same
  # recipe-deferred window semantics. Otherwise parse the formula here.
  if (is.null(parsed_formula)) {
    parsed_formula <- parse_formula(
      formula,
      envir = work_env,
      realize_windows = !recipe_deferred_windows
    )
  }
  rhs_names <- parsed_formula$rhs_names
  dep_name <- parsed_formula$dep_name
  has_intercept <- parsed_formula$has_intercept
  window_parameters <- parsed_formula$window_parameters
  ignore_rep_parameter <- unlist(parsed_formula$ignore_rep_parameter)

  # DyNAM-i ONLY: creates extra parameter to differentiate joining and
  # leaving rates, and effect subtypes. Added directly to GetDetailPrint

  if (any(unlist(parsed_formula$ignore_rep_parameter))) {
    cli::cli_abort(c(
      "Effects with {.code ignore_repetitions = TRUE} are disabled.",
      "x" = "The previous implementation computed incorrect statistics:
             it always masked repetitions using the dependent network instead
             of the network the effect is applied to.",
      "i" = "Follow the reimplementation progress in
             {.url https://github.com/stocnet/goldfish/issues/105}."
    ))
  }

  # Model-specific preprocessing initialization
  if (
    has_intercept &&
      ((model %in%
        c("DyNAM", "DyNAMi") &&
        sub_model %in% c("choice", "choice_coordination")) ||
        sub_model == "rate_ordered")
  ) {
    warning(
      "Model ",
      dQuote(model),
      " sub_model ",
      dQuote(sub_model),
      " ignores the time intercept.",
      call. = FALSE,
      immediate. = TRUE
    )
    parsed_formula$has_intercept <- has_intercept <- FALSE
  }
  rightCensored <- has_intercept

  # Per-(model, sub_model) main-effect validity (design D3). Unavailable effects
  # (no bare implementation, e.g. global in choice) abort in every phase;
  # computable-but-unidentified effects stay producible via preprocessing
  # (compute_stats, as design columns for interactions / random effects) and are
  # rejected only when estimating. Uses the effective sub_model (a rate formula
  # without the time intercept is the ordinal case) and runs after `*` expansion.
  # All effects are main until interaction terms land (task 2.5).
  validity_sub_model <- sub_model
  if (sub_model == "rate" && !has_intercept) {
    validity_sub_model <- "rate_ordered"
  }
  # Offset (fixed-coefficient) terms are not estimated main effects, so the D3
  # identification matrix does not reject them (design D7): a constant-across-
  # alternatives offset warns rather than aborts (handled in the fixedParameters
  # assembly below). Exclude them from the main-effect validity check.
  is_offset <- unlist(parsed_formula$offset_parameter)
  if (is.null(is_offset)) {
    is_offset <- logical(length(rhs_names))
  }
  main_effect <- !is_offset
  validate_effects(
    model,
    validity_sub_model,
    vapply(rhs_names[main_effect], "[[", character(1), 1),
    vapply(
      parsed_formula$type_parameter[main_effect],
      as.character,
      character(1)
    ),
    estimating = !preprocessing_only
  )

  if (model == "DyNAM" && sub_model == "rate" && !has_intercept) {
    cli::cli_warn(c(
      "!" = "{.code sub_model = \"rate\"} with a formula without the time
             intercept is deprecated.",
      "i" = "Use {.code sub_model = \"rate_ordered\"} to model only the order
             of the events."
    ))
  }

  legacy_sub_model <- sub_model
  if (sub_model == "rate_ordered") {
    legacy_sub_model <- "rate"
  }
  if (model == "REM") {
    legacy_sub_model <- "choice"
  }

  if (
    progress &&
      !(model %in%
        c("DyNAM", "DyNAMi") &&
        sub_model %in% c("choice", "choice_coordination"))
  ) {
    cat(
      ifelse(has_intercept, "T", "No t"),
      "ime intercept added.\n",
      sep = ""
    )
  }
  # if (progress && !all(vapply(windowParameters, is.null, logical(1))))
  #   cat("Creating window objects in global environment.")

  ## 1.2 PARSE for preprocessingInit: check the formula consistency
  if (!is.null(preprocessing_init)) {
    # find the old and new effects indexes, do basic consistency checks
    old_parsed_formula <- parse_formula(
      preprocessing_init$formula,
      envir = work_env
    )
    effects_indexes <- compare_formulas(
      old_parsed_formula = old_parsed_formula,
      new_parsed_formula = parsed_formula,
      model = model,
      sub_model = legacy_sub_model
    )
    if (sum(duplicated(effects_indexes)) > 0) {
      stop(
        "The comparison of the new formula and old formula is not able\n",
        "to identify the effects properly.\n",
        "It's not possible to use the preprocessing_init object in this case.",
        call. = FALSE
      )
    }
  }

  ### 2. INITIALIZE OBJECTS: effects, nodes, and link objects----

  if (progress) {
    cat("Initializing objects.\n")
  }

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
    rhs_names,
    model,
    legacy_sub_model,
    envir = work_env,
    derivations = parsed_formula$window_derivations
  )
  objects_effects_link <- get_objects_effects_link(rhs_names)

  ## 2.2 INITIALIZE OBJECTS for preprocessing_init == NULL
  if (is.null(preprocessing_init)) {
    # Build the event-stream link metadata + fetch plan (no tables). The recipe
    # (DyNAM/REM) path defers fetching to state creation (design D8, task 2.3f);
    # DyNAMi realizes windows eagerly at parse time and fetches here (its
    # front-end cleans the fetched events before its monolith loop).
    link <- build_events_objects_link(
      dep_name,
      rhs_names,
      .nodes,
      .nodes2,
      envir = work_env,
      derivations = parsed_formula$window_derivations
    )
    events_objects_link <- link$events_objects_link
    fetch_plan <- link$fetch_plan
    events_effects_link <- get_events_effects_link(
      rhs_names,
      events_objects_link
    )
    events <- if (model == "DyNAMi") {
      fetch_events(fetch_plan, envir = work_env)
    } else {
      NULL
    }
  }

  ### 3. PREPROCESS statistics----
  if (!is.null(preprocessing_init)) {
    # recover the nodesets
    .nodes <- preprocessing_init$nodes
    .nodes2 <- preprocessing_init$nodes2
    is_two_mode <- FALSE
    if (!identical(.nodes, .nodes2)) is_two_mode <- TRUE
  }

  spec_sub_model <- sub_model
  if (sub_model == "rate" && !has_intercept) {
    spec_sub_model <- "rate_ordered"
  }
  model_spec <- new_model_spec(
    model = model,
    sub_model = spec_sub_model,
    is_two_mode = is_two_mode,
    nodes = .nodes,
    nodes2 = .nodes2,
    has_intercept = has_intercept
  )

  # Recipe (DyNAM/REM) models compile the spec_map upfront (design D8) and
  # dispatch preprocess() on it (`preprocess_recipe()`); DyNAMi runs its own
  # isolated front-end (`preprocess_dynami()`, task 2.3e). `spec_map` stays NULL
  # for DyNAMi and for the preprocessing_init path (the printing step falls back
  # to `GetDetailPrint`).
  spec_map <- NULL

  ## 3.1 INITIALIZE OBJECTS for preprocessingInit: remove old effects,
  ## add new ones
  if (!is.null(preprocessing_init)) {
    # find new effects
    if (min(effects_indexes) == 0) {
      if (progress) {
        cat("Calculating newly added effects.\n")
      }
      new_rhs_names <- rhs_names[which(effects_indexes == 0)]
      new_window_parameters <- window_parameters[which(effects_indexes == 0)]
      new_effects <- create_effects_functions(
        new_rhs_names,
        model,
        legacy_sub_model,
        envir = work_env,
        derivations = parsed_formula$window_derivations
      )
      new_objects_effects_link <- get_objects_effects_link(new_rhs_names)
      new_link <- build_events_objects_link(
        dep_name,
        new_rhs_names,
        .nodes,
        .nodes2,
        envir = work_env,
        derivations = parsed_formula$window_derivations
      )
      new_events_objects_link <- new_link$events_objects_link
      new_fetch_plan <- new_link$fetch_plan
      new_events_effects_link <- get_events_effects_link(
        new_rhs_names,
        new_events_objects_link
      )

      # Preprocess the new effects through the model's front-end (task 2.3e):
      # recipe (DyNAM/REM) compiles the spec_map; DyNAMi runs its isolated path.
      if (progress) {
        cat("Pre-processing additional effects.\n")
      }
      newprep <- if (model == "DyNAMi") {
        preprocess_dynami(
          model_spec,
          fetch_events(new_fetch_plan, envir = work_env),
          new_effects,
          new_window_parameters,
          new_events_objects_link,
          new_events_effects_link,
          new_objects_effects_link,
          sub_model,
          dep_name,
          .nodes,
          .nodes2,
          is_two_mode,
          ignore_rep_parameter,
          rightCensored,
          control_preprocessing,
          parsed_formula,
          progress,
          work_env
        )
      } else {
        preprocess_recipe(
          parsed_formula,
          model_spec,
          new_effects,
          new_window_parameters,
          new_objects_effects_link,
          new_events_objects_link,
          new_events_effects_link,
          new_fetch_plan,
          control_preprocessing,
          progress,
          work_env
        )$prep
      }

      if (
        sum(preprocessing_init$is_dependent == 1L) !=
          sum(newprep$is_dependent == 1L)
      ) {
        stop(
          "The numbers of dependent events in the formula and in the ",
          "preprocessed object are not consistent.\n",
          "\tPlease check whether these events have changed.",
          call. = FALSE
        )
      }

      if (
        sum(preprocessing_init$is_dependent == 0L) !=
          sum(newprep$is_dependent == 0L)
      ) {
        stop(
          "The numbers of right-censored events in the formula and in the ",
          "preprocessed object are not consistent.\n",
          "\tPlease check whether some windows have been changed.",
          call. = FALSE
        )
      }
    }

    # combine old and new preprocessed objects
    if (progress) {
      cat("Removing no longer required effects.\n")
    }
    allprep <- preprocessing_init
    is_rate_model <- preprocessing_init$model == "DyNAM" &&
      preprocessing_init$subModel == "rate"
    init_is_flat <- is.null(preprocessing_init$stats_change)
    has_new_effects <- min(effects_indexes) == 0
    if (has_new_effects && init_is_flat != is.null(newprep$stats_change)) {
      cli::cli_abort(c(
        "The {.arg preprocessing_init} object format does not match the
         format produced by the current preprocessing.",
        "i" = "Recompute the preprocessing object with {.fn compute_stats}."
      ))
    }
    n1_val <- nrow(get(.nodes, envir = work_env))
    n2_val <- nrow(get(.nodes2, envir = work_env))
    nEffectsNew <- length(effects_indexes)
    if (is_rate_model) {
      allprep$initialStats <- matrix(0, nrow = n1_val, ncol = nEffectsNew)
    } else {
      allprep$initialStats <- array(0, dim = c(n1_val, n2_val, nEffectsNew))
    }
    cptnew <- 1

    # initial stats
    for (e in seq_along(effects_indexes)) {
      if (effects_indexes[e] == 0) {
        if (is_rate_model) {
          allprep$initialStats[, e] <- newprep$initialStats[, cptnew]
        } else {
          allprep$initialStats[,, e] <- newprep$initialStats[,, cptnew]
        }
        cptnew <- cptnew + 1
      }
      if (effects_indexes[e] > 0) {
        if (is_rate_model) {
          allprep$initialStats[, e] <-
            preprocessing_init$initialStats[, effects_indexes[e]]
        } else {
          allprep$initialStats[,, e] <-
            preprocessing_init$initialStats[,, effects_indexes[e]]
        }
      }
    }

    # stats updates (unified dependent + right-censored)
    if (init_is_flat) {
      merged <- merge_flat_updates(
        preprocessing_init,
        if (has_new_effects) newprep else NULL,
        effects_indexes
      )
      allprep$stat_mat_update <- merged$stat_mat_update
      allprep$stat_mat_pointer <- merged$stat_mat_pointer
    } else {
      allprep$stats_change <- list()
      for (t in seq_along(preprocessing_init$stats_change)) {
        cptnew <- 1
        allprep$stats_change[[t]] <-
          lapply(seq_along(effects_indexes), function(x) NULL)
        for (e in seq_along(effects_indexes)) {
          if (effects_indexes[e] == 0) {
            if (!is.null(newprep$stats_change[[t]][[cptnew]])) {
              allprep$stats_change[[t]][[e]] <-
                newprep$stats_change[[t]][[cptnew]]
            }
            cptnew <- cptnew + 1
          }
          if (effects_indexes[e] > 0) {
            if (
              !is.null(
                preprocessing_init$stats_change[[t]][[effects_indexes[e]]]
              )
            ) {
              allprep$stats_change[[t]][[e]] <-
                preprocessing_init$stats_change[[t]][[effects_indexes[e]]]
            }
          }
        }
      }
    }

    prep <- allprep
    prep$formula <- formula
    prep$model <- model
    prep$subModel <- legacy_sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
  }

  ## 3.2 PREPROCESS when preprocessingInit == NULL
  if (is.null(preprocessing_init)) {
    if (progress) {
      cat("Starting preprocessing.\n")
    }
    writer <- switch(
      output,
      default = writer_default(),
      gather = writer_gather(),
      db = writer_db(
        control_preprocessing$db,
        control_preprocessing$db_table
      )
    )
    # Preprocess through the model's front-end (task 2.3e): recipe (DyNAM/REM)
    # compiles + dispatches on the spec_map; DyNAMi runs its isolated path and
    # leaves `spec_map` NULL (the printing step falls back to `GetDetailPrint`).
    if (model == "DyNAMi") {
      prep <- preprocess_dynami(
        model_spec,
        events,
        effects,
        window_parameters,
        events_objects_link,
        events_effects_link,
        objects_effects_link,
        sub_model,
        dep_name,
        .nodes,
        .nodes2,
        is_two_mode,
        ignore_rep_parameter,
        rightCensored,
        control_preprocessing,
        parsed_formula,
        progress,
        work_env,
        writer
      )
    } else {
      recipe_out <- preprocess_recipe(
        parsed_formula,
        model_spec,
        effects,
        window_parameters,
        objects_effects_link,
        events_objects_link,
        events_effects_link,
        fetch_plan,
        control_preprocessing,
        progress,
        work_env,
        writer
      )
      prep <- recipe_out$prep
      spec_map <- recipe_out$spec_map
    }
    if (output %in% c("gather", "db")) {
      gathered <- finalize_gather_output(
        prep,
        model,
        sub_model,
        has_intercept,
        get(.nodes, envir = data),
        get(.nodes2, envir = data),
        objects_effects_link,
        parsed_formula,
        max_length = max_length,
        effect_description = spec_map$effect_description
      )
      if (output == "db") {
        return(write_gather_to_db(
          gathered,
          control_preprocessing$db,
          control_preprocessing$db_table
        ))
      }
      return(gathered)
    }
    # The formula, nodes, nodes2 are added to the preprocessed object so that
    # we can call the estimation with preprocessingInit later
    # (for parsing AND composition changes)
    prep$formula <- formula
    prep$model <- model
    prep$subModel <- legacy_sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
  }

  prep$model_spec <- model_spec

  ## 3.3 Stop here if preprocessingOnly == TRUE
  if (preprocessing_only) {
    return(prep)
  }

  ### 3.4 Assemble the fixed-coefficient (offset) vector----
  # offset() terms fix their coefficient rather than estimate it (design D7).
  # The parameter vector is [intercept?, effects...], so an offset at rhs
  # position j fixes parameter j (+1 when the intercept is prepended); the
  # existing positional `fixedParameters` (Newton-Raphson) is reused unchanged.
  # Constant-across-alternatives offsets in choice cancel in the softmax, so
  # they warn rather than abort (design D3 axis).
  effective_fixed_parameters <- assemble_fixed_parameters(
    parsed_formula,
    rhs_names,
    has_intercept,
    model,
    sub_model,
    control_estimation$fixed_parameters,
    control_estimation$offset_coef
  )

  ### 4. PREPARE PRINTING----
  # functions_utility.R
  # Reuse the spec_map's single-source-of-truth description when available
  # (recipe models, no fixed-coefficient marking); otherwise compute it (the
  # fixed-coefficient case adds a column, and the DyNAMi / preprocessing_init
  # paths have no spec_map). Behaviour is identical to the unconditional call.
  effectDescription <-
    if (
      !is.null(spec_map$effect_description) &&
        is.null(effective_fixed_parameters)
    ) {
      spec_map$effect_description
    } else {
      GetDetailPrint(
        objects_effects_link,
        parsed_formula,
        effective_fixed_parameters
      )
    }
  hasWindows <- attr(effectDescription, "hasWindows")
  if (is.null(hasWindows)) {
    hasWindows <- !all(vapply(window_parameters, is.null, logical(1)))
  }
  attr(effectDescription, "hasWindows") <- NULL
  ### 5. ESTIMATE----
  if (progress) {
    cat(
      "Estimating a model: ",
      dQuote(model),
      ", subModel: ",
      dQuote(sub_model),
      ".\n",
      sep = ""
    )
  }

  argsEstimation <- list(
    initialParameters = control_estimation$initial_parameters,
    fixedParameters = effective_fixed_parameters,
    maxIterations = as.integer(control_estimation$max_iterations),
    score_tol = control_estimation$score_tol,
    step_tol = control_estimation$step_tol,
    dampingIncreaseFactor = control_estimation$damping_increase_factor,
    dampingDecreaseFactor = control_estimation$damping_decrease_factor,
    returnEventProbabilities = control_estimation$return_probabilities,
    returnIntervalLogL = control_estimation$return_interval_loglik,
    statsList = prep,
    nodes = get(.nodes, envir = data),
    nodes2 = get(.nodes2, envir = data),
    hasIntercept = has_intercept,
    is_two_mode = is_two_mode,
    modelType = legacy_model_type(model_spec),
    # overridden damping
    initialDamping = if (!is.null(control_estimation$initial_damping)) {
      control_estimation$initial_damping
    } else {
      ifelse(hasWindows, 30, 10)
    },
    parallelize = FALSE,
    cpus = 1,
    verbose = verbose,
    progress = progress,
    opportunitiesList = control_preprocessing$opportunities_list
  )

  # Call the appropriate estimation engine
  if (control_estimation$engine %in% c("default_c", "gather_compute")) {
    tryCatch(
      result <- do.call(
        "estimate_c_int",
        args = c(argsEstimation, list(engine = control_estimation$engine))
      ),
      error = \(e) {
        stop(
          "For ",
          model,
          " ",
          sub_model,
          " estimation:\n\t",
          e$message,
          call. = FALSE
        )
      }
    )
  } else {
    tryCatch(
      result <- do.call(
        "estimate_int",
        args = c(list(spec = model_spec), argsEstimation)
      ),
      error = \(e) {
        stop(
          "For ",
          model,
          " ",
          sub_model,
          " estimation:\n\t",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  ### 6. RESULTS----
  result$names <- effectDescription
  result$model_spec <- model_spec
  formulaKeep <- as.formula(
    Reduce(paste, deparse(formula)),
    env = new.env(parent = emptyenv())
  )
  result$formula <- formulaKeep
  result$model <- model
  result$subModel <- sub_model
  result$rightCensored <- has_intercept
  result$nParams <- sum(!GetFixed(result))
  # The specification path adds an extra `estimate_from_specification` hop whose
  # signature differs from the estimator's, so match.call() against this
  # definition would fail; fall back to the raw caller expression in that case.
  result$call <- tryCatch(
    match.call(
      call = sys.call(-1L),
      expand.dots = TRUE
    ),
    error = function(e) sys.call(-1L)
  )
  result$call[[2]] <- formulaKeep
  ## added to allow printing/plotting of rate models with rightCnesoredEvents
  result$eventTime <- prep$event_time
  result$rightCensoredEvents <- prep$is_dependent == 0L

  return(result)
}

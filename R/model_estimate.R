#' Estimate a model
#'
#' Estimates parameters for a dynamic network model via maximum likelihood
#'  implementing the iterative Newton-Raphson procedure as describe in
#'  Stadtfeld and Block (2017).
#'
#' @inheritSection goldfish_data Missing data
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
#' @param control_algo An object of class `algorithm.goldfish`
#'   (typically created by [set_algorithm_newton()]),
#'   specifying the algorithm and its parameters for the estimation.
#' @param control_prep An object of class
#'   `preprocessing.goldfish` (typically created by
#'   [set_preprocessing()]),
#'   specifying parameters for data preprocessing. This is only used
#'   if `preprocessed` is not a `preprocessed.goldfish` object or NULL.
#' @param preprocessed an optional preprocessed object of class
#'  `preprocessed.goldfish` from a previous estimation. When it is provided,
#'  the function will skip the preprocessing of the effects that are already
#'  present in the object and only preprocess the new effects. Default to
#'  `NULL`.
#' @param preprocessing_only `r lifecycle::badge("deprecated")` logical. If
#'  `TRUE`, the function will only run the preprocessing stage and return an
#'  object of class `preprocessed.goldfish`. Default to `FALSE`. Superseded in
#'  goldfish 2.0.0 by [compute_statistics()] with `output = "preprocessed"`,
#'  which returns the same object from a function that says what it does; it
#'  keeps working through 2.x.
#' @param control_estimation `r lifecycle::badge("deprecated")` Renamed to
#'  `control_algo` in goldfish 2.0.0.
#' @param control_preprocessing `r lifecycle::badge("deprecated")` Renamed to
#'  `control_prep` in goldfish 2.0.0.
#' @param preprocessing_init `r lifecycle::badge("deprecated")` Renamed to
#'  `preprocessed` in goldfish 2.0.0, the name every diagnostic consumer of a
#'  `preprocessed.goldfish` object uses.
#' @param support_constraint a one-sided formula restricting the per-event risk
#'   set, written in the restricted boolean-tree grammar: effect atoms
#'   (`tie(net)`, `indeg(net)`, ...) combined with `& | !`, comparisons
#'   `> < >= <= == !=` (effect-vs-constant or effect-vs-effect), and elementwise
#'   arithmetic `+ - * /`; a bare effect means `effect != 0`. Inside a constraint
#'   `*` is elementwise arithmetic, never the effects formula's interaction
#'   expansion; for 0/1 indicators prefer `&` over `*` (`tie(a) & tie(b)`).
#'   Ignored when `x` is a `specification.goldfish` object (which carries its own
#'   constraint). See [make_specification()] for the full grammar. `NULL` by
#'   default.
#' @param verbose logical indicating whether should print
#'   very detailed intermediate results of the iterative Newton-Raphson
#'   procedure; slows down the routine significantly.
#' @param progress logical indicating whether should print a minimal output
#'   to the console of the progress of the preprocessing and
#'   estimation processes.
#' @param x a formula that defines at the left-hand side the dependent
#'   network (see [make_dependent_events()]) and at the right-hand side the
#'   effects and the variables for which the effects are expected to occur
#'   (see `vignette("goldfish_effects")`).
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
#'   \item{right_censored}{
#'   a logical value indicating if the estimation process considered
#'   right-censored events.
#'   Only it is considered for `estimate_dynam(x, sub_model = "rate")` or
#'   REM (`estimate_rem()`), when the model includes the intercept.}
#'   \item{right_censored_events}{a logical vector indicating whether or not an
#'   event is a right censored event}
#'   \item{event_times}{
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
#' # The prebuilt data objects are single stocnet objects (see
#' # `?social_evolution` / `?fisheries_treaties` for how they are constructed).
#'
#' # A DyNAM modeling the choice step; the focal `calls` layer names the process
#' data("social_evolution")
#' mod01 <- estimate_dynam(calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(engine = "gather_compute")
#' )
#' summary(mod01)
#'
#' # An individual activity rates model
#' mod02 <- estimate_dynam(calls ~ 1 + node_trans + indeg + outdeg,
#'   sub_model = "rate",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(engine = "gather_compute")
#' )
#' summary(mod02)
#'
#' \donttest{
#' # A REM
#' mod03 <- estimate_rem(
#'   calls ~ 1 + node_trans(calls, type = "ego") +
#'     indeg(calls, type = "ego") + outdeg(calls, type = "ego") +
#'     inertia + recip + trans,
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(engine = "gather_compute")
#' )
#' summary(mod03)
#'
#' # A multinomial-multinomial choice model for coordination ties. Naming the
#' # treaty layer's two processes lets a keyed specification model signings
#' # while every treaty change still updates the network state.
#' data("fisheries_treaties")
#' fish <- add_flavor(
#'   fisheries_treaties,
#'   layer = "treaties",
#'   values_equivalence = c(signing = 1, ending = -1),
#'   flavor_style = "redundant"
#' )
#' partner_spec <- make_specification(
#'   choice = list(
#'     signing ~ inertia(treaties) + indeg(treaties) + trans(treaties) +
#'       tie(contiguity) + alter(regime) + diff(regime) +
#'       alter(gdp) + diff(gdp)
#'   ),
#'   model = "DyNAM",
#'   choice_sub_model = "choice_coordination",
#'   data = fish
#' )
#' partner_model <- estimate_dynam(
#'   partner_spec,
#'   sub_model = "choice_coordination",
#'   data = fish,
#'   control_algo =
#'     set_algorithm_newton(
#'       initial_damping = 40, max_iterations = 30,
#'       engine = "default"
#'     )
#' )
#' summary(partner_model)
#' }
#'
NULL

# Reject a legacy `data.goldfish` environment at the public surface. Every model
# family now assembles to a stocnet (`make_data()` / `make_groups_interaction()`
# return one, or abort), so no public entrypoint needs an environment; the only
# environments left are objects saved before the 2.0.0 flip. The internal
# stocnet -> environment bridge for DyNAM-i is built inside `estimate_wrapper()`,
# after this guard, so it is unaffected.
abort_legacy_environment <- function(data, call = rlang::caller_env()) {
  if (is.environment(data)) {
    cli::cli_abort(
      c(
        "{.arg data} must be a {.cls stocnet} object, not a legacy
         {.cls data.goldfish} environment.",
        "i" = "Rebuild the object with {.fn make_data} /
               {.fn make_groups_interaction} (both now return a {.cls stocnet})."
      ),
      call = call
    )
  }
  invisible(data)
}

#' @rdname estimate
#' @export
estimate_dynam <- function(
  x,
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  data = NULL,
  control_algo = set_algorithm_newton(),
  control_prep = set_preprocessing(),
  preprocessed = NULL,
  preprocessing_only = FALSE,
  support_constraint = NULL,
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE),
  control_estimation = deprecated(),
  control_preprocessing = deprecated(),
  preprocessing_init = deprecated()
) {
  if (!missing(preprocessing_only)) {
    warn_preprocessing_only("estimate_dynam")
  }
  control_algo <- fold_renamed_arg(
    control_algo,
    !missing(control_algo),
    control_estimation,
    "estimate_dynam",
    "control_estimation",
    "control_algo"
  )
  control_prep <- fold_renamed_arg(
    control_prep,
    !missing(control_prep),
    control_preprocessing,
    "estimate_dynam",
    "control_preprocessing",
    "control_prep"
  )
  preprocessed <- fold_renamed_arg(
    preprocessed,
    !missing(preprocessed),
    preprocessing_init,
    "estimate_dynam",
    "preprocessing_init",
    "preprocessed"
  )
  sub_model <- match.arg(sub_model)
  abort_legacy_environment(data)
  if (inherits(x, "specification.goldfish")) {
    return(estimate_from_specification(
      spec = x,
      model = "DyNAM",
      sub_model = sub_model,
      data = data,
      control_algo = control_algo,
      control_prep = control_prep,
      preprocessed = preprocessed,
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
    control_algo = control_algo,
    control_prep = control_prep,
    preprocessed = preprocessed,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose,
    support_constraint = support_constraint
  )
}

#' @rdname estimate
#' @export
estimate_dynami <- function(
  x,
  sub_model = c("choice", "rate"),
  data = NULL,
  control_algo = set_algorithm_newton(),
  control_prep = set_preprocessing(),
  preprocessed = NULL,
  preprocessing_only = FALSE,
  support_constraint = NULL,
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE),
  control_estimation = deprecated(),
  control_preprocessing = deprecated(),
  preprocessing_init = deprecated()
) {
  if (!missing(preprocessing_only)) {
    warn_preprocessing_only("estimate_dynami")
  }
  control_algo <- fold_renamed_arg(
    control_algo,
    !missing(control_algo),
    control_estimation,
    "estimate_dynami",
    "control_estimation",
    "control_algo"
  )
  control_prep <- fold_renamed_arg(
    control_prep,
    !missing(control_prep),
    control_preprocessing,
    "estimate_dynami",
    "control_preprocessing",
    "control_prep"
  )
  preprocessed <- fold_renamed_arg(
    preprocessed,
    !missing(preprocessed),
    preprocessing_init,
    "estimate_dynami",
    "preprocessing_init",
    "preprocessed"
  )
  sub_model <- match.arg(sub_model)
  abort_legacy_environment(data)
  if (inherits(x, "specification.goldfish")) {
    return(estimate_from_specification(
      spec = x,
      model = "DyNAMi",
      sub_model = sub_model,
      data = data,
      control_algo = control_algo,
      control_prep = control_prep,
      preprocessed = preprocessed,
      preprocessing_only = preprocessing_only,
      progress = progress,
      verbose = verbose
    ))
  }
  estimate_wrapper(
    x = x,
    model = "DyNAMi",
    sub_model = sub_model,
    data = data,
    control_algo = control_algo,
    control_prep = control_prep,
    preprocessed = preprocessed,
    preprocessing_only = preprocessing_only,
    support_constraint = support_constraint,
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
  control_algo = set_algorithm_newton(),
  control_prep = set_preprocessing(),
  preprocessed = NULL,
  preprocessing_only = FALSE,
  support_constraint = NULL,
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE),
  control_estimation = deprecated(),
  control_preprocessing = deprecated(),
  preprocessing_init = deprecated()
) {
  if (!missing(preprocessing_only)) {
    warn_preprocessing_only("estimate_rem")
  }
  control_algo <- fold_renamed_arg(
    control_algo,
    !missing(control_algo),
    control_estimation,
    "estimate_rem",
    "control_estimation",
    "control_algo"
  )
  control_prep <- fold_renamed_arg(
    control_prep,
    !missing(control_prep),
    control_preprocessing,
    "estimate_rem",
    "control_preprocessing",
    "control_prep"
  )
  preprocessed <- fold_renamed_arg(
    preprocessed,
    !missing(preprocessed),
    preprocessing_init,
    "estimate_rem",
    "preprocessing_init",
    "preprocessed"
  )
  sub_model <- match.arg(sub_model)
  abort_legacy_environment(data)
  if (inherits(x, "specification.goldfish")) {
    return(estimate_from_specification(
      spec = x,
      model = "REM",
      sub_model = sub_model,
      data = data,
      control_algo = control_algo,
      control_prep = control_prep,
      preprocessed = preprocessed,
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
    control_algo = control_algo,
    control_prep = control_prep,
    preprocessed = preprocessed,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose,
    support_constraint = support_constraint
  )
}

# Estimate from a specification.goldfish object. Selects the submodel
# bundle matching the requested sub_model's family (rate vs choice), reuses its
# parsed formula bundle so estimation does not re-parse, and forwards to the
# shared estimator with the bundle's own sub_model. Results are identical to
# estimating the equivalent `layer ~ rhs` formula.
estimate_from_specification <- function(
  spec,
  model,
  sub_model,
  data = NULL,
  control_algo,
  control_prep,
  preprocessed,
  preprocessing_only,
  progress,
  verbose,
  output = "default",
  max_length = 63L
) {
  if (!identical(spec$model, model)) {
    cli::cli_abort(c(
      "This specification is for model {.val {spec$model}}.",
      "x" = "It cannot be estimated with {.fn {paste0('estimate_',
             tolower(model))}}."
    ))
  }
  # A multi-flavor specification is K parallel processes over one layer: it
  # preprocesses in one pass and estimates per process, returning a container
  # rather than a single fit.
  if (!is.null(spec$processes)) {
    return(estimate_flavored(
      spec = spec,
      model = model,
      data = data,
      control_algo = control_algo,
      control_prep = control_prep,
      preprocessed = preprocessed,
      preprocessing_only = preprocessing_only,
      progress = progress,
      verbose = verbose
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
  # and no incremental preprocessed re-parse is involved; otherwise let the
  # wrapper parse afresh against the supplied data.
  reuse_parsed <- is.null(preprocessed) &&
    (is.null(data) || identical(data, spec$data))

  estimate_wrapper(
    x = bundle$formula,
    model = model,
    sub_model = bundle$sub_model,
    data = est_data,
    control_algo = control_algo,
    control_prep = control_prep,
    preprocessed = preprocessed,
    preprocessing_only = preprocessing_only,
    progress = progress,
    verbose = verbose,
    parsed_formula = if (reuse_parsed) bundle$parsed else NULL,
    support_constraint = spec$constraint,
    modeled_flavor = spec$modeled_flavor,
    output = output,
    max_length = max_length
  )
}

#' Compute preprocessed statistics for a model
#'
#' Runs the preprocessing stage of a model and returns the change statistics
#' of the effects for the event sequence, without estimating the model.
#' The returned object can be passed to the estimation functions
#' ([estimate_dynam()], [estimate_rem()], [estimate_dynami()]) through their
#' `preprocessed` argument, or used directly by users who want to
#' work with the sufficient statistics of a model.
#'
#' @param x a formula that defines at the left-hand side the dependent
#'   network (see [make_dependent_events()]) and at the right-hand side the
#'   effects and the variables for which the effects are expected to occur
#'   (see `vignette("goldfish_effects")`), or a `specification.goldfish` object
#'   from [make_specification()] — the same first argument the `estimate_*()`
#'   functions take.
#' @param model a character string specifying the model. Current options are
#'   `"DyNAM"`, `"REM"` or `"DyNAMi"`, see [estimate_dynam()],
#'   [estimate_rem()] and [estimate_dynami()].
#' @param sub_model a character string specifying the sub-model, see
#'   [estimate_dynam()]. The default value `NULL` resolves to `"rate"` for
#'   `model = "REM"` and `"choice"` otherwise. Mind the consequence of that
#'   default: `"rate"` is the *exact-time* sub-model, so it force-adds the time
#'   intercept and stores right-censored rows. Use `"rate_ordered"` to model
#'   only the order of the events, which has neither.
#' @param data a `data.goldfish` object created with [make_data()].
#' @param output a character string specifying the output format of the
#'   preprocessed statistics. `"preprocessed"` returns the estimation-ready
#'   `preprocessed.goldfish` object; `"gather"` returns the gather stack (one
#'   row per event x alternative, as in [gather_model_data()]); `"db"` streams
#'   the gather rows to the database table configured via
#'   [set_preprocessing()] (`db` / `db_table`) and returns a descriptor.
#' @param control_prep an object of class `preprocessing.goldfish` created
#'   with [set_preprocessing()].
#' @param progress logical. Whether to print a progress bar during
#'   preprocessing.
#' @param max_length integer. Maximum number of characters for each produced
#'   effect/column name in the `"gather"` and `"db"` outputs (default `63`, a
#'   database-safe value). Names are made valid and unique; the uniqueness
#'   suffix is applied after truncation so uniqueness is preserved.
#' @param ... additional arguments passed to the preprocessing stage.
#'
#' @return an object of class `"preprocessed.goldfish"` with the change
#'   statistics of the effects for the event sequence and the information
#'   of the model variant computed. See the `Value` section of
#'   [estimate_dynam()] for the `preprocessing_only = TRUE` case.
#'
#'   Every output form reports two logical fields describing the likelihood
#'   shape it was produced under, so a consumer never has to infer it from the
#'   columns: `has_intercept` (the exact-time time intercept is present) and
#'   `right_censored` (right-censored rows are stored, carrying `timespan` and
#'   `isDependent`). Both are `TRUE` for `sub_model = "rate"` and `FALSE` for
#'   `"rate_ordered"` and the choice sub-models.
#'
#' @section Indexing statistic columns:
#' Index statistic columns **by name**, never by position. The exact-time
#' sub-models prepend an `Intercept` column, so the same formula yields
#' statistics at different positions under `sub_model = "rate"` and
#' `"rate_ordered"`. The names are in `namesEffects` and on the columns of
#' `stat_all_events`.
#'
#' @seealso [estimate_dynam()], [estimate_rem()], [estimate_dynami()],
#'   [set_preprocessing()]
#' @export
#' @examples
#' data("social_evolution")
#' prep <- compute_statistics(
#'   calls ~ inertia + recip + trans,
#'   model = "DyNAM", sub_model = "choice",
#'   data = social_evolution
#' )
#' prep
compute_statistics <- function(
  x,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = NULL,
  data = NULL,
  output = c("preprocessed", "gather", "db"),
  control_prep = set_preprocessing(),
  progress = getOption("progress", default = FALSE),
  max_length = 63L,
  ...
) {
  # No local match.arg on model/sub_model: the vocabulary is validated once,
  # downstream in estimate_wrapper(), so this surface cannot drift from what
  # estimation accepts (the drift that made gather_model_data() reject
  # sub_model = "rate_ordered" while estimation ran it). Only the default is
  # resolved here, since the sub_model default depends on the model.
  model <- if (length(model) > 1) model[[1]] else model
  if (is.null(sub_model)) {
    sub_model <- if (identical(model, "REM")) "rate" else "choice"
  }
  output <- match.arg(output)
  # A specification takes the same front door here as at estimation, so the two
  # entry points cannot disagree about what a specification means.
  if (inherits(x, "specification.goldfish")) {
    return(estimate_from_specification(
      spec = x,
      model = model,
      sub_model = sub_model,
      data = data,
      control_algo = set_algorithm_newton(),
      control_prep = control_prep,
      preprocessed = NULL,
      preprocessing_only = TRUE,
      progress = progress,
      verbose = FALSE,
      output = if (output == "preprocessed") "default" else output,
      max_length = max_length
    ))
  }
  estimate_wrapper(
    x = x,
    model = model,
    sub_model = sub_model,
    data = data,
    # the wrapper and the writers keep the legacy "default" token for the
    # estimation-ready object; only the user-facing vocabulary says
    # "preprocessed"
    output = if (output == "preprocessed") "default" else output,
    control_prep = control_prep,
    progress = progress,
    preprocessing_only = TRUE,
    max_length = max_length,
    ...
  )
}

# Preprocessing-time set-size validation for a support_constraint.
# Fails fast — before the C++ likelihood — with event context. Cases:
#   A observed dyad excluded (choice) / observed sender gated out (rate) -> error
#   B dependent event's own sender has 0 live receivers                  -> error
#   D the whole risk set is empty at a dependent event                   -> error
#   C the risk set has exactly 1 candidate (forced choice)               -> warn
#   E a node is never live across the whole sequence                     -> warn
# Rate nuance: a NON-observed sender with 0 live receivers is gated out (normal,
# not an error) — only the dependent event's own sender triggers A/B.
# `process_label` names the process the failing object belongs to when several
# are preprocessed together, so the message identifies which one is empty. It is
# rendered for display only, never parsed back.
validate_support_constraint <- function(
  support_mask,
  event_sender,
  event_receiver,
  is_dependent,
  active_1,
  active_2,
  family,
  process_label = NULL
) {
  support <- support_mask$support
  in_process <- if (is.null(process_label)) {
    NULL
  } else {
    c("i" = "Process {.field {process_label}}.")
  }
  dep <- which(is_dependent == 1L)
  n1 <- length(active_1)
  n2 <- length(active_2)
  forced <- integer(0)

  if (family == "choice") {
    ever_candidate <- logical(n2)
    for (e in dep) {
      allowed <- which(support[[e]][event_sender[[e]], ] & active_2)
      ever_candidate[allowed] <- TRUE
      if (length(allowed) == 0L) {
        cli::cli_abort(c(
          "{.arg support_constraint}: empty risk set at event {e}.",
          "x" = "Sender {event_sender[[e]]} has no allowed, present receiver.",
          in_process
        ))
      }
      if (!(event_receiver[[e]] %in% allowed)) {
        cli::cli_abort(c(
          "{.arg support_constraint}: the observed dyad is excluded.",
          "x" = "Event {e}: receiver {event_receiver[[e]]} is not allowed for
                 sender {event_sender[[e]]}.",
          in_process
        ))
      }
      if (length(allowed) == 1L) {
        forced <- c(forced, e)
      }
    }
    never <- which(active_2 & !ever_candidate)
    if (length(never) > 0) {
      cli::cli_warn(c(
        "!" = "{.arg support_constraint}: {length(never)} present receiver{?s}
               never an allowed candidate.",
        "i" = "{cli::qty(length(never))}Node{?s}: {.val {never}}."
      ))
    }
  } else {
    ever_active <- logical(n1)
    for (e in dep) {
      gate <- rowSums(support[[e]] & rep(active_2, each = n1)) > 0
      ever_active <- ever_active | (active_1 & gate)
      if (!gate[event_sender[[e]]]) {
        cli::cli_abort(c(
          "{.arg support_constraint}: the observed sender is gated out.",
          "x" = "Event {e}: sender {event_sender[[e]]} has no allowed, present
                 receiver.",
          in_process
        ))
      }
      if (sum(active_1 & gate) == 1L) {
        forced <- c(forced, e)
      }
    }
    never <- which(active_1 & !ever_active)
    if (length(never) > 0) {
      cli::cli_warn(c(
        "!" = "{.arg support_constraint}: {length(never)} present sender{?s}
               never at risk (always gated out).",
        "i" = "{cli::qty(length(never))}Node{?s}: {.val {never}}."
      ))
    }
  }

  if (length(forced) > 0) {
    cli::cli_warn(c(
      "!" = "{.arg support_constraint}: {length(forced)} event{?s} with a
             single candidate (forced choice; contributes 0 to the
             log-likelihood).",
      "i" = "{cli::qty(length(forced))}Event{?s}: {.val {forced}}."
    ))
  }
  invisible(NULL)
}

# Run the set-size validation on a preprocessed object, picking the presence
# vectors the check needs. Rate reads the raw sender presence stashed by the
# fold (the object's own `active_sender_init` is already the folded
# availability), and the dyad fold likewise overwrites `active_dyad_init`, so
# both read the unfolded vectors the mask stashed. A constraint-free object has
# no mask and nothing to validate.
validate_prep_support <- function(prep, is_rate_family, process_label = NULL) {
  if (is.null(prep$support_mask)) {
    return(invisible(NULL))
  }
  validate_support_constraint(
    prep$support_mask,
    prep$event_sender,
    prep$event_receiver,
    prep$is_dependent,
    if (is_rate_family && !is.null(prep$support_mask$sender_presence_init)) {
      prep$support_mask$sender_presence_init
    } else {
      prep$active_sender_init
    },
    if (!is.null(prep$support_mask$receiver_presence_init)) {
      prep$support_mask$receiver_presence_init
    } else {
      prep$active_dyad_init
    },
    family = if (is_rate_family) "rate" else "choice",
    process_label = process_label
  )
}

#' Recipe (DyNAM/REM) preprocessing front-end
#'
#' Compiles the `spec_map` upfront and runs the shared recipe loop
#' via `preprocess(spec_map, …)`. Returns both `prep` and the `spec_map` (the
#' latter carries the single-source-of-truth `effect_description` consumed by the
#' printing step). Isolated from the DyNAMi front-end so the shared
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
  control_prep,
  progress,
  work_env,
  support_constraint = NULL,
  writer = writer_default(),
  work_data = NULL,
  modeled_flavor = NULL,
  flavor_plan = NULL
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
    support_constraint = support_constraint,
    envir = work_env,
    data = work_data,
    modeled_flavor = modeled_flavor
  )
  # The per-attribute imputation policy rides on the compiled spec so the recipe
  # context reaches it without threading through every loop's `...`.
  spec_map$impute_policy <- control_prep$impute
  # A multi-flavor walk drives K consumers instead of the single writer. The
  # consumer specs can only be assembled here, after the spec_map has compiled
  # each `(layer, flavor)` constraint into `plan$support_constraints`: a
  # consumer carries the COMPILED sub-plan its mask is realized from, not the
  # parsed one.
  consumer_specs <- if (is.null(flavor_plan)) {
    NULL
  } else {
    build_consumer_specs(
      flavor_plan$consumers,
      spec_map$plan$support_constraints
    )
  }
  # The recipe loop realizes derived inputs (from plan$derivations) and fetches
  # events (from spec$fetch_plan) inside state creation,
  # so no pre-fetched events list is threaded here.
  prep <- preprocess(
    spec_map,
    startTime = control_prep$start_time,
    endTime = control_prep$end_time,
    opportunitiesList = control_prep$opportunities_list,
    progress = progress,
    prep_envir = work_env,
    writer = writer,
    consumer_specs = consumer_specs
  )
  list(prep = prep, spec_map = spec_map)
}

#' DyNAMi preprocessing front-end (isolated)
#'
#' The DyNAMi-only path: the extra `clean_interaction_events` cleaning step (window
#' class tagging + leaving-event removal for the choice estimation) followed by
#' the monolithic `preprocess_interaction` loop via `preprocess.dynami_*`. It keeps
#' the bridge-argument signature (no `spec_map`); the recipe/`spec_map` unification
#' is deferred to a future DyNAMi engine refactor. Fenced here so the shared
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
  right_censored,
  control_prep,
  parsed_formula,
  progress,
  work_env,
  writer = writer_default()
) {
  # DyNAM-i ONLY: assign an extra class to the windowed events and remove
  # leaving events for the choice estimation.
  events <- clean_interaction_events(
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
    window_parameters = window_parameters,
    ignore_rep_parameter = ignore_rep_parameter,
    events_objects_link = events_objects_link, # for data update
    events_effects_link = events_effects_link,
    objects_effects_link = objects_effects_link, # for parameterization
    nodes = nodes,
    nodes2 = nodes2,
    is_two_mode = is_two_mode,
    startTime = control_prep$start_time,
    endTime = control_prep$end_time,
    right_censored = right_censored,
    opportunitiesList = control_prep$opportunities_list,
    progress = progress,
    groups_network = parsed_formula$default_network_name,
    prep_envir = work_env,
    writer = writer
  )
}

# The data input is either a stocnet (a list of components) or, for the
# deprecation cycle, a legacy environment of goldfish objects.
check_estimation_data <- function(data, call = rlang::caller_env()) {
  if (is.environment(data) || (is.list(data) && !is.data.frame(data))) {
    return(invisible(data))
  }
  cli::cli_abort(
    c(
      "{.arg data} must be a {.cls stocnet} object.",
      "i" = "Build it with {.fn manynet::make_stocnet}, or gate it early with
             {.fn as_goldfish}."
    ),
    call = call
  )
}

# Pre-run guardrail: when per-event probabilities are requested, warn once with
# the estimated storage footprint (n_events x |riskset| x 8 bytes) and point to
# the scalable `ranks`/`margins` primitives. Best-effort on the dimensions: if
# the preprocessed object lacks the presence vectors (e.g. some DyNAMi shapes)
# the warning is skipped rather than dropping the request.
warn_probabilities_footprint <- function(
  prep,
  spec,
  call = rlang::caller_env()
) {
  n_senders <- length(prep$active_sender_init)
  n_receivers <- length(prep$active_dyad_init)
  if (n_senders == 0 && n_receivers == 0) {
    return(invisible())
  }
  riskset <- if (isTRUE(risk_set_is_dyadic(spec))) {
    as.numeric(n_senders) * as.numeric(n_receivers)
  } else if (identical(risk_set_axis(spec), "sender")) {
    as.numeric(n_senders)
  } else {
    as.numeric(n_receivers)
  }
  n_events <- sum(prep$is_dependent == 1L)
  bytes <- n_events * riskset * 8
  size <- format(structure(bytes, class = "object_size"), units = "auto")
  cli::cli_warn(
    c(
      "!" = "Storing per-event probabilities for {n_events} event{?s} over a
             risk set of size {riskset} will use about {size}.",
      "i" = "For scalable diagnostics request {.val ranks} or {.val margins}
             instead of {.val probabilities}."
    ),
    call = call
  )
  invisible()
}

# Pre-run note: with a large event count the per-event diagnostic vectors
# (`intervalLogL`, `total_rate`, and the `event_scores` columns) occupy
# noticeable memory. Emit a one-time cli message with the estimated footprint and
# the `diagnostics = FALSE` opt-out. Fires only above `threshold` events so
# ordinary fits stay quiet; `total_rate` is stored only for exact-time submodels.
note_diagnostic_storage_footprint <- function(
  n_events,
  diagnostics,
  n_params,
  is_exact_time,
  threshold = 1e5,
  call = rlang::caller_env()
) {
  if (n_events < threshold) {
    return(invisible())
  }
  doubles_per_event <- 0
  if ("loglik" %in% diagnostics) {
    doubles_per_event <- doubles_per_event + 1 + as.integer(is_exact_time)
  }
  if ("scores" %in% diagnostics) {
    doubles_per_event <- doubles_per_event + n_params
  }
  if (doubles_per_event == 0) {
    return(invisible())
  }
  bytes <- as.numeric(n_events) * doubles_per_event * 8
  size <- format(structure(bytes, class = "object_size"), units = "auto")
  cli::cli_inform(
    c(
      "i" = "Storing per-event diagnostics for {n_events} event{?s} will use
             about {size}.",
      "i" = "Set {.code diagnostics = FALSE} to skip per-event storage."
    ),
    call = call
  )
  invisible()
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
  control_algo = set_algorithm_newton(),
  control_prep = set_preprocessing(),
  preprocessed = NULL,
  preprocessing_only = FALSE,
  output = c("default", "gather", "db"),
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE),
  max_length = 63L,
  parsed_formula = NULL,
  support_constraint = NULL,
  modeled_flavor = NULL,
  flavor_plan = NULL
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

  ### check model and sub_model
  check_model_par(
    model,
    sub_model,
    model_list = c("DyNAM", "REM", "DyNAMi"),
    sub_model_list = list(
      DyNAM = c("rate", "rate_ordered", "choice", "choice_coordination"),
      REM = c("rate", "rate_ordered", "choice"),
      DyNAMi = c("choice", "rate")
    ),
    deprecated_sub_models = list(REM = "choice")
  )

  if (model == "REM" && sub_model == "choice") {
    cli::cli_warn(c(
      "!" = "{.code sub_model = \"choice\"} is deprecated for REM models.",
      "i" = "Use {.code sub_model = \"rate\"} to model the timing of the
             dyadic events (exact-time), or {.code sub_model =
             \"rate_ordered\"} to model only their order (ordinal).",
      "i" = "Continuing with {.code sub_model = \"rate\"}."
    ))
    sub_model <- "rate"
  }

  check_estimation_data(data)

  # DyNAMi consumes the legacy environment, which make_data() no longer mints
  # (it assembles every bundle into a stocnet). At the boundary a stocnet is
  # rewritten onto the environment names and reversed into the environment shape
  # the interaction monolith reads; the bridge is internal and never returned.
  dynami_availability <- NULL
  if (model == "DyNAMi" && inherits(data, "stocnet")) {
    focal <- data$info$focal
    if (
      identical(
        unname(data$info$sender[[focal]]),
        unname(data$info$receiver[[focal]])
      )
    ) {
      cli::cli_abort(c(
        "{.fn estimate_dynami} needs a two-mode actors x groups data object.",
        "x" = "The focal layer {.val {focal}} runs within one mode, so it is \\
               not an interaction-groups object.",
        "i" = "Build the object with {.fn make_groups_interaction}."
      ))
    }
    if (inherits(x, "formula")) {
      x <- rewrite_dynami_formula(x, data)
    }
    # The joining choice set is the groups occupied at the decision point
    # (Hoffman et al. Eq. 8's denominator over the present second-mode nodes,
    # which includes the joiner's own singleton -- an isolate may choose to
    # remain isolated, and the construction records such observed choices). It is
    # derived from the focal layer's occupancy here, while the stocnet is still
    # intact, and folded into the dense `active_dyad` after preprocessing -- the
    # standard maintained-availability path the estimation kernel reads, not the
    # deprecated opportunities channel.
    if (sub_model %in% c("choice", "choice_coordination")) {
      dynami_availability <- dynami_choice_availability(data)
    }
    data <- stocnet_to_dynami_env(data, parent_env = environment(x))
  }

  stopifnot(
    rlang::is_scalar_logical(preprocessing_only),
    rlang::is_scalar_logical(verbose),
    is.null(progress) || rlang::is_scalar_logical(progress),
    is.null(preprocessed) ||
      inherits(preprocessed, "preprocessed.goldfish"),
    inherits(control_algo, "algorithm.goldfish"),
    inherits(control_prep, "preprocessing.goldfish")
  )

  if (is.null(progress)) {
    progress <- FALSE
  }

  # The multi-flavor walk emits one object per consumer, which only the
  # preprocessing return shape can carry: the gather/db writers and the
  # incremental `preprocessed` re-parse are single-output by construction,
  # and per-flavor estimation drives this walk through its own loop.
  if (!is.null(flavor_plan)) {
    stopifnot(
      preprocessing_only,
      identical(output, "default"),
      is.null(preprocessed)
    )
  }

  if (
    !is.null(preprocessed) &&
      !identical(preprocessed$version, PREPROCESSED_GOLDFISH_VERSION)
  ) {
    cli::cli_abort(c(
      "The {.arg preprocessed} object uses an outdated preprocessing
       format.",
      "x" = "Objects preprocessed with a previous goldfish version cannot be
             reused for estimation.",
      "i" = "Recompute the preprocessing object with {.fn compute_statistics}."
    ))
  }

  # gather_compute and default_c don't support returnEventProbabilities
  if (
    control_algo$return_probabilities &&
      control_algo$engine != "default"
  ) {
    warning(
      "engine = ",
      dQuote(control_algo$engine),
      " doesn't support",
      dQuote("return_probabilities"),
      ". engine =",
      dQuote("default"),
      " is used instead.",
      call. = FALSE,
      immediate. = TRUE
    )
    control_algo$engine <- "default"
  }

  # The per-event score matrix is produced by the two per-event engines
  # (default_c via the C++ evaluator flag, default in its contribution loop);
  # gather_compute has no per-event decomposition to expose. Because "scores" is
  # in the default diagnostics, aborting whenever it is on would break every
  # gather_compute fit; instead abort only when scores were requested explicitly
  # and silently drop the default-sourced request.
  if (
    isTRUE(control_algo$return_event_scores) &&
      control_algo$engine == "gather_compute"
  ) {
    if (isTRUE(control_algo$scores_explicit)) {
      cli::cli_abort(c(
        "The {.val scores} diagnostic (per-event score matrix) is not supported
         with {.code engine = \"gather_compute\"}.",
        "i" = "Use {.code engine = \"default_c\"} or {.code engine = \"default\"}
               to store the per-event score matrix."
      ))
    }
    control_algo$return_event_scores <- FALSE
  }

  # Optimizers other than the built-in Newton-Raphson are maxLik-backed:
  # they run only on the default_c evaluator and require the
  # Suggests-only maxLik package. Both are resolved before any preprocessing so
  # the abort is free of side effects.
  optimizer <- control_algo$optimizer
  if (is.null(optimizer)) {
    optimizer <- "newton_raphson"
  }
  if (!identical(optimizer, "newton_raphson")) {
    if (control_algo$engine != "default_c") {
      cli::cli_abort(c(
        "{.arg optimizer} {.val {optimizer}} requires
         {.code engine = \"default_c\"}.",
        "x" = "It is not available with
               {.code engine = {.val {control_algo$engine}}}.",
        "i" = "maxLik-backed optimizers run only on the default_c evaluator."
      ))
    }
    if (!requireNamespace("maxLik", quietly = TRUE)) {
      cli::cli_abort(c(
        "{.arg optimizer} {.val {optimizer}} requires the {.pkg maxLik} package.",
        "i" = "Install it with {.run install.packages(\"maxLik\")}."
      ))
    }
  }

  # gather_compute and default_c don't support restrictions of opportunity sets
  if (
    !is.null(control_prep$opportunities_list) &&
      control_algo$engine != "default"
  ) {
    warning(
      "engine = ",
      dQuote(control_algo$engine),
      " doesn't support",
      dQuote("opportunities_list"),
      ". engine =",
      dQuote("default"),
      " is used instead.",
      call. = FALSE,
      immediate. = TRUE
    )
    control_algo$engine <- "default"
  }

  ### 1. PARSE the formula----
  if (progress) {
    cat("Parsing formula.\n")
  }
  formula <- x

  # Create a working copy of the data environment to avoid side-effects. The
  # stocnet path resolves names from components instead, so it keeps an empty
  # environment and carries the data object alongside.
  is_legacy <- is.environment(data)
  work_env <- if (is_legacy) rlang::env_clone(data) else new.env()
  work_data <- if (is_legacy) NULL else data

  # A dependent-events object name on the LHS (the legacy call surface, e.g.
  # `create_bilat ~ ...`) resolves to its focal layer and stamped flavor before
  # parsing, so the rest of the pipeline sees an ordinary focal layer.
  if (!is_legacy) {
    aliased <- resolve_dependent_alias(formula, work_data, modeled_flavor)
    formula <- aliased$formula
    modeled_flavor <- aliased$modeled_flavor
  }

  # Resolve the support_constraint to a parsed sub-plan the recipe consumes. A
  # specification supplies an already-parsed `support_constraint_plan`; the
  # formula surface supplies a one-sided formula parsed here against the working
  # data. Dyadic atoms are legal only when the spec has a dyad-indexed part (a
  # choice submodel, or REM); a rate-only spec rejects them.
  constraint_plan <- NULL
  if (!is.null(support_constraint)) {
    if (inherits(support_constraint, "support_constraint_plan")) {
      constraint_plan <- support_constraint
    } else if (!is.null(flavor_plan)) {
      # A multi-flavor walk supplies the already-parsed per-`(layer, flavor)`
      # plans, keyed by constraint id; the spec_map compiles each into its own
      # sibling sub-plan.
      constraint_plan <- support_constraint
    } else {
      has_dyad_part <- model == "REM" ||
        sub_model %in% c("choice", "choice_coordination")
      constraint_plan <- parse_and_validate_constraint(
        support_constraint,
        has_dyad_part = has_dyad_part,
        envir = work_env
      )
    }
  }

  ## 1.1 PARSE for all cases: preprocessed or not
  # On the fresh recipe (DyNAM/REM) path the shared parser stays free of
  # environment mutations: parse_formula() records the window
  # derivation recipe but does not realize it, and the recipe state container
  # realizes it from `plan$derivations`. DyNAMi and the
  # preprocessed path keep the eager parse-time realization
  # (byte-identical) via realize_windows = TRUE.
  recipe_deferred_windows <- model %in%
    c("DyNAM", "REM") &&
    is.null(preprocessed)
  # A specification.goldfish object supplies its parsed bundle so estimation
  # reuses it rather than re-parsing; it was parsed with the same
  # recipe-deferred window semantics. Otherwise parse the formula here.
  if (is.null(parsed_formula)) {
    parsed_formula <- parse_formula(
      formula,
      envir = work_env,
      realize_windows = !recipe_deferred_windows,
      data = work_data
    )
  }
  rhs_names <- parsed_formula$rhs_names
  dep_name <- parsed_formula$dep_name
  has_intercept <- parsed_formula$has_intercept
  window_parameters <- parsed_formula$window_parameters
  ignore_rep_parameter <- unlist(parsed_formula$ignore_rep_parameter)

  # The layer being modeled is the focal layer of this estimation. Stamp it onto
  # the working copy so every downstream new_data_source(data = work_data)
  # resolves focal/side/mode lookups against the modeled layer through the
  # existing `%||% info$focal` fallback -- info$focal is only the default for
  # *which* layer to model, never the source of truth once a layer is chosen. A
  # focal-less object then estimates, and an info$focal naming a different layer
  # never wins over the modeled one. work_data is a local copy (copy-on-modify),
  # so this never mutates the caller's object.
  if (!is.null(work_data)) {
    work_data$info$focal <- dep_name
  }

  # Interaction terms compute their product in the dyad recipe loop;
  # guard the not-yet-supported model families (sender / DyNAMi).
  abort_if_interactions_unsupported(parsed_formula, model, sub_model)

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
  # `sub_model = "rate"` models the waiting times between events; the time
  # intercept is the baseline hazard that likelihood needs, so a rate formula
  # without an explicit `1` gets the intercept added rather than collapsing to
  # the order-only (ordinal) partial likelihood. Ordinal modeling is requested
  # explicitly with `sub_model = "rate_ordered"`.
  if (sub_model == "rate" && !has_intercept) {
    cli::cli_inform(c(
      "i" = "{.code sub_model = \"rate\"} models the waiting times between
             events; a time intercept has been added.",
      "i" = "Use {.code sub_model = \"rate_ordered\"} to model only the order
             of the events (ordinal likelihood)."
    ))
    parsed_formula$has_intercept <- has_intercept <- TRUE
  }
  right_censored <- has_intercept

  # Per-(model, sub_model) main-effect validity. Unavailable effects
  # (no bare implementation, e.g. global in choice) abort in every phase;
  # computable-but-unidentified effects stay producible via preprocessing
  # (compute_statistics, as design columns for interactions / random effects) and are
  # rejected only when estimating. Runs after `*` expansion. All effects are
  # main until interaction terms land.
  validity_sub_model <- sub_model
  # Validity is role-aware. Offset (fixed-coefficient) terms and
  # interaction operand-only terms are NOT bare main effects, so they are held
  # out of the main-effect identification check: an offset warns rather than
  # aborts (handled in the fixedParameters assembly below), and an operand
  # (e.g. `global`/`ego` feeding an interaction that restores variation) follows
  # the separate operand rule. A term that is both a requested main effect and an
  # operand (the `a*b` case, is_main) stays in the main-effect check.
  is_offset <- unlist(parsed_formula$offset_parameter)
  if (is.null(is_offset)) {
    is_offset <- logical(length(rhs_names))
  }
  is_main <- unlist(parsed_formula$is_main_parameter)
  if (is.null(is_main)) {
    is_main <- rep(TRUE, length(rhs_names))
  }
  is_operand <- unlist(parsed_formula$is_operand_parameter)
  if (is.null(is_operand)) {
    is_operand <- logical(length(rhs_names))
  }
  operand_only <- is_operand & !is_main
  main_effect <- !is_offset & !operand_only
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
  validate_operands(
    model,
    validity_sub_model,
    vapply(rhs_names[operand_only], "[[", character(1), 1),
    vapply(
      parsed_formula$type_parameter[operand_only],
      as.character,
      character(1)
    )
  )

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
  # if (progress && !all(vapply(window_parameters, is.null, logical(1))))
  #   cat("Creating window objects in global environment.")

  ## 1.2 PARSE for preprocessed: check the formula consistency
  if (!is.null(preprocessed)) {
    # find the old and new effects indexes, do basic consistency checks
    old_parsed_formula <- parse_formula(
      preprocessed$formula,
      envir = work_env,
      data = work_data
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
        "It's not possible to use the preprocessed object in this case.",
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
  work_src <- new_data_source(
    data = work_data,
    envir = work_env,
    focal = dep_name,
    modeled_flavor = modeled_flavor
  )
  # Estimation reports on the node sets as supplied, not on the working copies
  # imputation may have filled in; on the stocnet path nothing shadows `nodes`,
  # so the working source already is the original.
  orig_src <- if (is_legacy) new_data_source(envir = data) else work_src

  # A length-2 answer means the dependent process spans two modes: legacy reads
  # the dependent object's node sets, stocnet the focal layer's side pair.
  .nodes <- ds_layer_sides(work_src, dep_name)
  is_two_mode <- FALSE
  if (length(.nodes) == 2) {
    .nodes2 <- .nodes[2]
    .nodes <- .nodes[1]
    is_two_mode <- TRUE
  } else {
    .nodes2 <- .nodes
  }

  # Coordination (DyNAM-MM) models the symmetric joint creation of a tie by both
  # endpoints, reading both directed dyads (a, b) and (b, a) over one node set.
  # That is undefined on a two-mode layer -- an event does not reciprocally
  # coordinate with an actor -- and the C++ step indexes out of bounds. Reject it
  # here, before any preprocessing or the estimation engine.
  if (is_two_mode && identical(sub_model, "choice_coordination")) {
    cli::cli_abort(c(
      "{.val choice_coordination} cannot run on a two-mode layer.",
      "x" = "The focal layer {.val {dep_name}} is two-mode.",
      "i" = "Coordination (DyNAM-MM) models symmetric ties within one node set.",
      "i" = "Use {.code sub_model = \"choice\"} for a two-mode choice model."
    ))
  }

  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessed or not
  # enviroment from which get the objects

  effects <- create_effects_functions(
    rhs_names,
    model,
    legacy_sub_model,
    envir = work_env,
    derivations = parsed_formula$window_derivations,
    data = work_data
  )
  objects_effects_link <- get_objects_effects_link(rhs_names)

  ## 2.2 INITIALIZE OBJECTS for preprocessed == NULL
  if (is.null(preprocessed)) {
    # Build the event-stream link metadata + fetch plan (no tables). The recipe
    # (DyNAM/REM) path defers fetching to state creation;
    # DyNAMi realizes windows eagerly at parse time and fetches here (its
    # front-end cleans the fetched events before its monolith loop).
    link <- build_events_objects_link(
      dep_name,
      rhs_names,
      .nodes,
      .nodes2,
      envir = work_env,
      derivations = parsed_formula$window_derivations,
      data = work_data
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
  if (!is.null(preprocessed)) {
    # recover the nodesets
    .nodes <- preprocessed$nodes
    .nodes2 <- preprocessed$nodes2
    # The focal layer's mode map decides two-modeness (stocnet); the legacy
    # source, which has no map, answers from the recovered side names.
    is_two_mode <- ds_model_is_two_mode(work_src, .nodes, .nodes2)
  }

  model_spec <- new_model_spec(
    model = model,
    sub_model = sub_model,
    is_two_mode = is_two_mode,
    nodes = .nodes,
    nodes2 = .nodes2,
    has_intercept = has_intercept
  )

  # Recipe (DyNAM/REM) models compile the spec_map upfront and
  # dispatch preprocess() on it (`preprocess_recipe()`); DyNAMi runs its own
  # isolated front-end (`preprocess_dynami()`). `spec_map` stays NULL
  # for DyNAMi and for the preprocessed path (the printing step falls back
  # to `GetDetailPrint`).
  spec_map <- NULL

  ## 3.1 INITIALIZE OBJECTS for preprocessed: remove old effects,
  ## add new ones
  if (!is.null(preprocessed)) {
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

      # Preprocess the new effects through the model's front-end:
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
          right_censored,
          control_prep,
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
          control_prep,
          progress,
          work_env
        )$prep
      }

      if (
        sum(preprocessed$is_dependent == 1L) != sum(newprep$is_dependent == 1L)
      ) {
        stop(
          "The numbers of dependent events in the formula and in the ",
          "preprocessed object are not consistent.\n",
          "\t_please check whether these events have changed.",
          call. = FALSE
        )
      }

      if (
        sum(preprocessed$is_dependent == 0L) != sum(newprep$is_dependent == 0L)
      ) {
        stop(
          "The numbers of right-censored events in the formula and in the ",
          "preprocessed object are not consistent.\n",
          "\t_please check whether some windows have been changed.",
          call. = FALSE
        )
      }
    }

    # combine old and new preprocessed objects
    if (progress) {
      cat("Removing no longer required effects.\n")
    }
    allprep <- preprocessed
    is_rate_model <- preprocessed$model == "DyNAM" &&
      preprocessed$sub_model == "rate"
    init_is_flat <- is.null(preprocessed$stats_change)
    has_new_effects <- min(effects_indexes) == 0
    if (has_new_effects && init_is_flat != is.null(newprep$stats_change)) {
      cli::cli_abort(c(
        "The {.arg preprocessed} object format does not match the
         format produced by the current preprocessing.",
        "i" = "Recompute the preprocessing object with {.fn compute_statistics}."
      ))
    }
    n1_val <- ds_n_nodes(work_src, .nodes)
    n2_val <- ds_n_nodes(work_src, .nodes2)
    n_effects_new <- length(effects_indexes)
    if (is_rate_model) {
      allprep$initialStats <- matrix(0, nrow = n1_val, ncol = n_effects_new)
    } else {
      allprep$initialStats <- array(0, dim = c(n1_val, n2_val, n_effects_new))
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
            preprocessed$initialStats[, effects_indexes[e]]
        } else {
          allprep$initialStats[,, e] <-
            preprocessed$initialStats[,, effects_indexes[e]]
        }
      }
    }

    # stats updates (unified dependent + right-censored)
    if (init_is_flat) {
      merged <- merge_flat_updates(
        preprocessed,
        if (has_new_effects) newprep else NULL,
        effects_indexes
      )
      allprep$stat_mat_update <- merged$stat_mat_update
      allprep$stat_mat_pointer <- merged$stat_mat_pointer
    } else {
      allprep$stats_change <- list()
      for (t in seq_along(preprocessed$stats_change)) {
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
                preprocessed$stats_change[[t]][[effects_indexes[e]]]
              )
            ) {
              allprep$stats_change[[t]][[e]] <-
                preprocessed$stats_change[[t]][[effects_indexes[e]]]
            }
          }
        }
      }
    }

    prep <- allprep
    prep$formula <- formula
    prep$model <- model
    prep$sub_model <- legacy_sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
    prep$node_lookup <- ds_node_lookup(orig_src)
  }

  ## 3.2 PREPROCESS when preprocessed == NULL
  if (is.null(preprocessed)) {
    if (progress) {
      cat("Starting preprocessing.\n")
    }
    writer <- switch(
      output,
      default = writer_default(),
      gather = writer_gather(),
      db = writer_db(
        control_prep$db,
        control_prep$db_table
      )
    )
    # Preprocess through the model's front-end: recipe (DyNAM/REM)
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
        right_censored,
        control_prep,
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
        control_prep,
        progress,
        work_env,
        support_constraint = constraint_plan,
        writer = writer,
        work_data = work_data,
        modeled_flavor = modeled_flavor,
        flavor_plan = flavor_plan
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
        ds_nodes_frame(orig_src, .nodes),
        ds_nodes_frame(orig_src, .nodes2),
        objects_effects_link,
        parsed_formula,
        max_length = max_length,
        effect_description = spec_map$effect_description
      )
      # The node lookup resolves index_i/index_j back to original node identity.
      gathered$node_lookup <- ds_node_lookup(orig_src)
      if (output == "db") {
        return(write_gather_to_db(
          gathered,
          control_prep$db,
          control_prep$db_table
        ))
      }
      return(gathered)
    }
    # The formula, nodes, nodes2 are added to the preprocessed object so that
    # we can call the estimation with preprocessed later
    # (for parsing AND composition changes)
    decorate <- function(p, own_formula = formula) {
      p$formula <- own_formula
      p$model <- model
      p$sub_model <- legacy_sub_model
      p$nodes <- .nodes
      p$nodes2 <- .nodes2
      p$node_lookup <- ds_node_lookup(orig_src)
      p
    }
    # A multi-flavor walk emits one object per consumer, so the decoration that
    # makes an object estimable on its own is applied to each of them. Each
    # carries its OWN formula, not the union that drove the walk: its statistics
    # columns are the projection onto that formula's effects, so stamping the
    # union here would describe columns the object does not hold and misalign
    # any later `preprocessed` re-parse.
    prep <- if (is.null(flavor_plan)) {
      decorate(prep)
    } else {
      lapply(names(prep), function(key) {
        decorate(prep[[key]], flavor_plan$consumers[[key]]$formula)
      }) |>
        stats::setNames(names(prep))
    }
  }

  prep <- if (is.null(flavor_plan)) {
    prep$model_spec <- model_spec
    prep
  } else {
    lapply(prep, function(p) {
      p$model_spec <- model_spec
      p
    })
  }

  ## 3.3 Stop here if preprocessing_only == TRUE
  if (preprocessing_only) {
    return(prep)
  }

  # The interaction monolith emits the pre-recipe preprocessing shape; map it to
  # the recipe statsList the shared estimation kernel reads. Done after the
  # preprocessing_only return so the raw monolith object (which the DyNAM-i
  # preprocessing tests inspect) is preserved. Temporary seam retired with the
  # monolith by the DyNAM-i engine conversion.
  if (model == "DyNAMi") {
    prep <- dynami_recipe_statslist(prep, sub_model, is_two_mode)
    if (!is.null(dynami_availability)) {
      prep <- dynami_fold_availability(prep, dynami_availability)
    }
  }

  prob_requested <- isTRUE(control_algo$return_probabilities) ||
    "probabilities" %in% control_algo$diagnostics
  if (prob_requested) {
    warn_probabilities_footprint(prep, model_spec)
  }
  note_diagnostic_storage_footprint(
    n_events = length(prep$is_dependent),
    diagnostics = control_algo$diagnostics,
    n_params = length(rhs_names) + as.integer(isTRUE(has_intercept)),
    is_exact_time = identical(sub_model, "rate")
  )

  ### 3.4 Assemble the fixed-coefficient (offset) vector----
  # offset() terms fix their coefficient rather than estimate it.
  # The parameter vector is [intercept?, effects...], so an offset at rhs
  # position j fixes parameter j (+1 when the intercept is prepended); the
  # existing positional `fixedParameters` (Newton-Raphson) is reused unchanged.
  # Constant-across-alternatives offsets in choice cancel in the softmax, so
  # they warn rather than abort.
  effective_fixed_parameters <- assemble_fixed_parameters(
    parsed_formula,
    rhs_names,
    has_intercept,
    model,
    sub_model,
    control_algo$fixed_parameters,
    control_algo$offset_coef
  )

  ### 4. PREPARE PRINTING----
  # functions_utility.R
  # Reuse the spec_map's single-source-of-truth description when available
  # (recipe models, no fixed-coefficient marking); otherwise compute it (the
  # fixed-coefficient case adds a column, and the DyNAMi / preprocessed
  # paths have no spec_map). Behaviour is identical to the unconditional call.
  effect_description <-
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
  has_windows <- attr(effect_description, "has_windows")
  if (is.null(has_windows)) {
    has_windows <- !all(vapply(window_parameters, is.null, logical(1)))
  }
  attr(effect_description, "has_windows") <- NULL
  ### 5. ESTIMATE----
  if (progress) {
    cat(
      "Estimating a model: ",
      dQuote(model),
      ", sub_model: ",
      dQuote(sub_model),
      ".\n",
      sep = ""
    )
  }

  # A support_constraint is folded into the maintained availability during
  # preprocessing — a rate constraint into `active_sender`, a choice / REM /
  # coordination constraint into `active_dyad` — so every engine reads the
  # folded buffers and no standalone mask is assembled here. The capability map
  # aborts an unwired family below.
  opportunities_effective <- control_prep$opportunities_list
  # A constraint-free opportunity list is folded into `active_dyad` at the point
  # encoding during preprocessing: the default engine reads it
  # through the point accessor, so it is not also passed as a per-iteration
  # opportunity recompute. (A support_constraint folds the opportunity list in
  # too, so it is nulled below once the constraint is folded.)
  if (
    is.null(constraint_plan) &&
      !is.null(opportunities_effective) &&
      isTRUE(prep$active_dyad_folded)
  ) {
    opportunities_effective <- NULL
  }
  if (!is.null(constraint_plan) && !is.null(prep$support_mask)) {
    # Only families wired to consume a folded constraint reach the likelihood;
    # the engine-capability table (keyed on the spec class) is the single guard.
    if (!constrained_estimation_supported(model_spec)) {
      abort_constraint_unsupported(model_spec)
    }
    # Fail fast before the likelihood: excluded observed dyads / empty risk sets
    # error; forced choices and never-active nodes warn. The rate (sender-axis)
    # family uses the sender-gate policy; choice and REM check the observed dyad
    # directly. A multi-flavor object was validated at preprocessing time, so
    # re-running here would only duplicate every warning it emitted.
    if (!isTRUE(prep$support_validated)) {
      validate_prep_support(
        prep,
        identical(risk_set_axis(model_spec), "sender")
      )
    }
    # Every wired family folds its availability during preprocessing — a rate
    # constraint into `active_sender`, a choice / REM / coordination constraint
    # into `active_dyad` — and the engines read the folded buffers directly, so
    # no standalone mask or sender gate is assembled here. A folded one-sided
    # choice constraint already carries any user opportunity list, so the
    # per-iteration opportunity recompute is skipped.
    if (
      identical(risk_set_axis(model_spec), "receiver_given_sender") &&
        isTRUE(prep$active_dyad_folded)
    ) {
      opportunities_effective <- NULL
    }
  }

  args_estimation <- list(
    initialParameters = control_algo$initial_parameters,
    fixedParameters = effective_fixed_parameters,
    maxIterations = as.integer(control_algo$max_iterations),
    score_tol = control_algo$score_tol,
    step_tol = control_algo$step_tol,
    dampingIncreaseFactor = control_algo$damping_increase_factor,
    dampingDecreaseFactor = control_algo$damping_decrease_factor,
    returnEventProbabilities = control_algo$return_probabilities,
    returnIntervalLogL = control_algo$return_interval_loglik,
    return_event_scores = isTRUE(control_algo$return_event_scores),
    statsList = prep,
    nodes = ds_nodes_frame(orig_src, .nodes),
    nodes2 = ds_nodes_frame(orig_src, .nodes2),
    hasIntercept = has_intercept,
    is_two_mode = is_two_mode,
    # overridden damping
    initialDamping = if (!is.null(control_algo$initial_damping)) {
      control_algo$initial_damping
    } else {
      ifelse(has_windows, 30, 10)
    },
    parallelize = FALSE,
    cpus = 1,
    verbose = verbose,
    progress = progress,
    opportunitiesList = opportunities_effective
  )

  # Call the appropriate estimation engine
  if (control_algo$engine %in% c("default_c", "gather_compute")) {
    tryCatch(
      result <- do.call(
        "estimate_c_int",
        args = c(
          args_estimation,
          list(
            spec = model_spec,
            engine = control_algo$engine,
            optimizer = optimizer,
            return_ranks = "ranks" %in% control_algo$diagnostics,
            return_margins = "margins" %in% control_algo$diagnostics,
            return_total_rate = "loglik" %in% control_algo$diagnostics
          )
        )
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
        args = c(list(spec = model_spec), args_estimation)
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
  result$names <- effect_description
  # Name the per-event score columns by effect (rows of effect_description are
  # the coefficients, in the same order as the score vector).
  if (
    !is.null(result$event_scores) &&
      ncol(result$event_scores) == nrow(effect_description)
  ) {
    colnames(result$event_scores) <- rownames(effect_description)
  }
  result$model_spec <- model_spec
  formula_keep <- as.formula(
    Reduce(paste, deparse(formula)),
    env = new.env(parent = emptyenv())
  )
  result$formula <- formula_keep
  # The self-contained data has no caller-scope dependent object for the broom /
  # diagnostic surface to `get()`, so carry the modeled dependent events on the
  # result (labels resolved from the mode map), in event-schedule order.
  if (!is.environment(data)) {
    result$dependent_events <- stocnet_dependent_events(
      data,
      dep_name,
      modeled_flavor
    )
  }
  # The node lookup (side, local index, global id, label) travels with the
  # result so residuals / event-scores / export consumers resolve index_i /
  # index_j to the original node identity without re-deriving the mode map.
  result$node_lookup <- ds_node_lookup(orig_src)
  result$model <- model
  result$sub_model <- sub_model
  result$right_censored <- has_intercept
  result$nParams <- sum(!GetFixed(result))
  # Reconstruct the call for printing. On the direct path `sys.call(-1L)` is the
  # user's estimate_*() call. On the specification path the estimator is reached
  # through an internal hop, so `match.call()` surfaces a wrapper frame -- and
  # its error handler, evaluating `sys.call()` afresh, surfaces the `tryCatch`
  # machinery itself. Capture the caller once here, keep it only when it names a
  # public estimator, and otherwise fall back to a clean estimator call built
  # from the kept formula.
  estimator_name <- switch(
    model,
    REM = "estimate_rem",
    DyNAMi = "estimate_dynami",
    "estimate_dynam"
  )
  caller_call <- sys.call(-1L)
  clean_call <- as.call(list(as.name(estimator_name), formula_keep))
  result$call <- tryCatch(
    {
      matched <- match.call(call = caller_call, expand.dots = TRUE)
      is_public <- is.call(matched) &&
        is.name(matched[[1L]]) &&
        as.character(matched[[1L]]) %in%
          c("estimate_dynam", "estimate_rem", "estimate_dynami")
      if (is_public) matched else clean_call
    },
    error = function(e) clean_call
  )
  result$call[[2]] <- formula_keep
  ## added to allow printing/plotting of rate models with rightCnesoredEvents
  result$event_time <- prep$event_time
  result$right_censored_events <- prep$is_dependent == 0L

  return(result)
}

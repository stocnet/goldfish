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
#' # A DyNAM modeling rate and choice steps
#' data("Social_Evolution")
#' call_network <- make_network(nodes = actors, directed = TRUE)
#' call_network <- link_events(
#'   x = call_network, change_event = calls,
#'   nodes = actors
#' )
#' calls_dependent <- make_dependent_events(
#'   events = calls, nodes = actors,
#'   default_network = call_network
#' )
#'
#' \dontshow{
#' calls_dependent <- calls_dependent[1:50, ]
#' }
#'
#' social_ev_data <- make_data(calls_dependent, call_network, call, actors)
#'
#' mod01 <- estimate_dynam(calls_dependent ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_ev_data,
#'   control_estimation = set_estimation_opt(engine = "gather_compute")
#' )
#' summary(mod01)
#'
#' # A individual activity rates model
#' mod02 <- estimate_dynam(calls_dependent ~ 1 + node_trans + indeg + outdeg,
#'   sub_model = "rate",
#'   data = social_ev_data,
#'   control_estimation = set_estimation_opt(engine = "gather_compute")
#' )
#' summary(mod02)
#'
#' # A REM
#'
#' mod03 <- estimate_rem(
#'   calls_dependent ~ 1 + node_trans(call_network, type = "ego") +
#'     indeg(call_network, type = "ego") + outdeg(call_network, type = "ego") +
#'     inertia + recip + trans,
#'     data = social_ev_data,
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
#' create_bilat <- make_dependent_events(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, default_network = bilatnet
#' )
#'
#' fisheries_data <- make_data(
#'   create_bilat, contignet, bilatnet, contigchanges, bilatchanges,
#'   states, sovchanges, regchanges, gdpchanges
#'  )
#' partner_model <- estimate_dynam(
#'   create_bilat ~
#'     inertia(bilatnet) +
#'     indeg(bilatnet) +
#'     trans(bilatnet) +
#'     tie(contignet) +
#'     alter(states$regime) +
#'     diff(states$regime) +
#'     alter(states$gdp) +
#'     diff(states$gdp),
#'   sub_model = "choice_coordination",
#'   data = fisheries_data,
#'   control_estimation =
#'     set_estimation_opt(
#'       initial_damping = 40, max_iterations = 30,
#'       engine = "default"
#'     )
#' )
#' summary(partner_model)
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
  support_constraint = NULL,
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
  support_constraint = NULL,
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
    parsed_formula = if (reuse_parsed) bundle$parsed else NULL,
    support_constraint = spec$constraint,
    modeled_flavor = spec$modeled_flavor
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
#'   (see `vignette("goldfish_effects")`).
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
#' call_network <- make_network(nodes = actors, directed = TRUE)
#' call_network <- link_events(
#'   x = call_network, change_event = calls, nodes = actors
#' )
#' calls_dependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = call_network
#' )
#' \dontshow{
#' calls_dependent <- calls_dependent[1:50, ]
#' }
#' social_ev_data <- make_data(calls_dependent, call_network, calls, actors)
#'
#' prep <- compute_stats(
#'   calls_dependent ~ inertia + recip + trans,
#'   data = social_ev_data,
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

# Reduce the support mask to a per-event receiver filter for the DyNAM-choice
# default engine. A choice event has a single sender, so its
# allowed receivers are the sender's row of the support mask conjoined with
# receiver presence downstream; the resulting per-event id list is consumed by
# the existing opportunities machinery in `compute_iteration_step()`, which
# shrinks `n_candidates` and reindexes `selected` within the constrained set. A
# user-supplied `opportunities_list` is intersected in (both restrict the set).
mask_to_opportunities <- function(support_mask, statsList, user_opp = NULL) {
  support <- support_mask$support
  senders <- statsList$event_sender
  if (length(support) != length(senders)) {
    cli::cli_abort(
      "The support mask ({length(support)}) and event count
       ({length(senders)}) are misaligned."
    )
  }
  lapply(seq_along(support), function(e) {
    allowed <- which(support[[e]][senders[[e]], ])
    if (
      !is.null(user_opp) &&
        length(user_opp) >= e &&
        !is.null(user_opp[[e]])
    ) {
      allowed <- intersect(allowed, user_opp[[e]])
    }
    allowed
  })
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
validate_support_constraint <- function(
  support_mask,
  event_sender,
  event_receiver,
  is_dependent,
  active_1,
  active_2,
  family
) {
  support <- support_mask$support
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
          "x" = "Sender {event_sender[[e]]} has no allowed, present receiver."
        ))
      }
      if (!(event_receiver[[e]] %in% allowed)) {
        cli::cli_abort(c(
          "{.arg support_constraint}: the observed dyad is excluded.",
          "x" = "Event {e}: receiver {event_receiver[[e]]} is not allowed for
                 sender {event_sender[[e]]}."
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
                 receiver."
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
  control_preprocessing,
  progress,
  work_env,
  support_constraint = NULL,
  writer = writer_default(),
  work_data = NULL,
  modeled_flavor = NULL
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
  # The recipe loop realizes derived inputs (from plan$derivations) and fetches
  # events (from spec$fetch_plan) inside state creation,
  # so no pre-fetched events list is threaded here.
  prep <- preprocess(
    spec_map,
    startTime = control_preprocessing$start_time,
    endTime = control_preprocessing$end_time,
    opportunitiesList = control_preprocessing$opportunities_list,
    progress = progress,
    prep_envir = work_env,
    writer = writer
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
  control_preprocessing,
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
    startTime = control_preprocessing$start_time,
    endTime = control_preprocessing$end_time,
    right_censored = right_censored,
    opportunitiesList = control_preprocessing$opportunities_list,
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
  parsed_formula = NULL,
  support_constraint = NULL,
  modeled_flavor = NULL
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

  check_estimation_data(data)

  stopifnot(
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

  # The per-event score matrix is produced by the two per-event engines
  # (default_c via the C++ evaluator flag, default in its contribution loop);
  # gather_compute has no per-event decomposition to expose.
  if (
    isTRUE(control_estimation$return_event_scores) &&
      control_estimation$engine == "gather_compute"
  ) {
    cli::cli_abort(c(
      "{.arg return_event_scores} is not supported with
       {.code engine = \"gather_compute\"}.",
      "i" = "Use {.code engine = \"default_c\"} or {.code engine = \"default\"}
             to return the per-event score matrix."
    ))
  }

  # Optimizers other than the built-in Newton-Raphson are maxLik-backed:
  # they run only on the default_c evaluator and require the
  # Suggests-only maxLik package. Both are resolved before any preprocessing so
  # the abort is free of side effects.
  optimizer <- control_estimation$optimizer
  if (is.null(optimizer)) {
    optimizer <- "newton_raphson"
  }
  if (!identical(optimizer, "newton_raphson")) {
    if (control_estimation$engine != "default_c") {
      cli::cli_abort(c(
        "{.arg optimizer} {.val {optimizer}} requires
         {.code engine = \"default_c\"}.",
        "x" = "It is not available with
               {.code engine = {.val {control_estimation$engine}}}.",
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

  ## 1.1 PARSE for all cases: preprocessing_init or not
  # On the fresh recipe (DyNAM/REM) path the shared parser stays free of
  # environment mutations: parse_formula() records the window
  # derivation recipe but does not realize it, and the recipe state container
  # realizes it from `plan$derivations`. DyNAMi and the
  # preprocessing_init path keep the eager parse-time realization
  # (byte-identical) via realize_windows = TRUE.
  recipe_deferred_windows <- model %in%
    c("DyNAM", "REM") &&
    is.null(preprocessing_init)
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
  right_censored <- has_intercept

  # Per-(model, sub_model) main-effect validity. Unavailable effects
  # (no bare implementation, e.g. global in choice) abort in every phase;
  # computable-but-unidentified effects stay producible via preprocessing
  # (compute_stats, as design columns for interactions / random effects) and are
  # rejected only when estimating. Uses the effective sub_model (a rate formula
  # without the time intercept is the ordinal case) and runs after `*` expansion.
  # All effects are main until interaction terms land.
  validity_sub_model <- sub_model
  if (sub_model == "rate" && !has_intercept) {
    validity_sub_model <- "rate_ordered"
  }
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
  # if (progress && !all(vapply(window_parameters, is.null, logical(1))))
  #   cat("Creating window objects in global environment.")

  ## 1.2 PARSE for preprocessing_init: check the formula consistency
  if (!is.null(preprocessing_init)) {
    # find the old and new effects indexes, do basic consistency checks
    old_parsed_formula <- parse_formula(
      preprocessing_init$formula,
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

  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessing_init or not
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

  ## 2.2 INITIALIZE OBJECTS for preprocessing_init == NULL
  if (is.null(preprocessing_init)) {
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

  # Recipe (DyNAM/REM) models compile the spec_map upfront and
  # dispatch preprocess() on it (`preprocess_recipe()`); DyNAMi runs its own
  # isolated front-end (`preprocess_dynami()`). `spec_map` stays NULL
  # for DyNAMi and for the preprocessing_init path (the printing step falls back
  # to `GetDetailPrint`).
  spec_map <- NULL

  ## 3.1 INITIALIZE OBJECTS for preprocessing_init: remove old effects,
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
          "\t_please check whether these events have changed.",
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
          "\t_please check whether some windows have been changed.",
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
      preprocessing_init$sub_model == "rate"
    init_is_flat <- is.null(preprocessing_init$stats_change)
    has_new_effects <- min(effects_indexes) == 0
    if (has_new_effects && init_is_flat != is.null(newprep$stats_change)) {
      cli::cli_abort(c(
        "The {.arg preprocessing_init} object format does not match the
         format produced by the current preprocessing.",
        "i" = "Recompute the preprocessing object with {.fn compute_stats}."
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
    prep$sub_model <- legacy_sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
    prep$node_lookup <- ds_node_lookup(orig_src)
  }

  ## 3.2 PREPROCESS when preprocessing_init == NULL
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
        support_constraint = constraint_plan,
        writer = writer,
        work_data = work_data,
        modeled_flavor = modeled_flavor
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
          control_preprocessing$db,
          control_preprocessing$db_table
        ))
      }
      return(gathered)
    }
    # The formula, nodes, nodes2 are added to the preprocessed object so that
    # we can call the estimation with preprocessing_init later
    # (for parsing AND composition changes)
    prep$formula <- formula
    prep$model <- model
    prep$sub_model <- legacy_sub_model
    prep$nodes <- .nodes
    prep$nodes2 <- .nodes2
    prep$node_lookup <- ds_node_lookup(orig_src)
  }

  prep$model_spec <- model_spec

  ## 3.3 Stop here if preprocessing_only == TRUE
  if (preprocessing_only) {
    return(prep)
  }

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
    control_estimation$fixed_parameters,
    control_estimation$offset_coef
  )

  ### 4. PREPARE PRINTING----
  # functions_utility.R
  # Reuse the spec_map's single-source-of-truth description when available
  # (recipe models, no fixed-coefficient marking); otherwise compute it (the
  # fixed-coefficient case adds a column, and the DyNAMi / preprocessing_init
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

  # Consume a support_constraint on the R (default) engine. DyNAM-choice reduces
  # the mask to a per-event receiver filter routed through the existing
  # opportunities machinery (shrinks n_candidates, reindexes selected);
  # DyNAM-rate reduces it to a per-event sender gate (a sender is at risk only
  # with >= 1 allowed present receiver) and recomputes the constrained
  # intercept denominator; REM keeps the full per-event dyad mask (the risk set is
  # 2D). DyNAM rate_ordered and the compiled engines land with the C++ gather
  # rewrite, so they abort rather than silently ignore the constraint.
  opportunities_effective <- control_preprocessing$opportunities_list
  # A constraint-free opportunity list is folded into `active_dyad` at the point
  # encoding during preprocessing: the default engine reads it
  # through the point accessor, so it is not also passed as a per-iteration
  # opportunity recompute. (With a support_constraint the mask path below
  # carries the intersection, so the list still rides there.)
  if (
    is.null(constraint_plan) &&
      !is.null(opportunities_effective) &&
      isTRUE(prep$active_dyad_folded)
  ) {
    opportunities_effective <- NULL
  }
  sender_gate <- NULL
  rem_mask <- NULL
  support_gather <- NULL
  if (!is.null(constraint_plan) && !is.null(prep$support_mask)) {
    is_choice_family <- model == "DyNAM" &&
      sub_model %in% c("choice", "choice_coordination")
    # Coordination (`choice_coordination` / `DyNAM-MM`) is a dyad-part choice
    # family for the guard + validation, but its two-sided likelihood consumes a
    # symmetrised FULL mask like REM, not the one-sided-choice row —
    # so folding/consumption below splits it from one-sided choice.
    is_coord_family <- model == "DyNAM" && sub_model == "choice_coordination"
    is_one_sided_choice <- is_choice_family && !is_coord_family
    is_rate_family <- model == "DyNAM" && sub_model == "rate"
    # Standard REM (`rem_rate_spec`, time intercept) zeroes disallowed dyads in the
    # Poisson contribution; ordinal REM (`rem_rate_ordered_spec`, no intercept)
    # zeroes their utility before the multinomial normalizer. Both fold both
    # presences ∩ support into a dense point `active_dyad` and consume
    # it as the maintained risk mask.
    is_rem_family <- model == "REM" && sub_model == "rate" && has_intercept
    is_rem_ordered_family <- model == "REM" && sub_model == "rate_ordered"
    if (
      !is_choice_family &&
        !is_rate_family &&
        !is_rem_family &&
        !is_rem_ordered_family
    ) {
      cli::cli_abort(c(
        "{.arg support_constraint} is not yet consumed for {.val {model}}
         {.val {sub_model}} estimation.",
        "i" = "Risk-set restriction is currently wired for the DyNAM
               {.val choice} / {.val choice_coordination} / {.val rate} and
               {.val REM} sub-models on the default engine; the preprocessed mask
               is available in {.code prep$support_mask}."
      ))
    }
    # Fail fast before the likelihood: excluded observed dyads / empty
    # risk sets error; forced choices and never-active nodes warn. Rate uses the
    # sender-gate policy; choice and REM both check the observed dyad directly.
    # Rate uses the raw sender presence stashed by the fold; the
    # object's own `active_sender_init` is already the folded availability.
    validate_active_1 <- if (
      is_rate_family && !is.null(prep$support_mask$sender_presence_init)
    ) {
      prep$support_mask$sender_presence_init
    } else {
      prep$active_sender_init
    }
    validate_support_constraint(
      prep$support_mask,
      prep$event_sender,
      prep$event_receiver,
      prep$is_dependent,
      validate_active_1,
      # Raw receiver presence: the dyad fold overwrites `active_dyad_init` with
      # the folded object, so validation reads the stashed unfolded vector.
      if (!is.null(prep$support_mask$receiver_presence_init)) {
        prep$support_mask$receiver_presence_init
      } else {
        prep$active_dyad_init
      },
      family = if (is_rate_family) "rate" else "choice"
    )
    # `avg_active_entity` (the rate intercept init) is now computed during
    # preprocessing from the folded `active_sender`; no
    # estimation-time recombination.
    # The compiled engines consume the mask natively where wired: gather_compute
    # for DyNAM choice / rate (the R gather filters candidates), and default_c for
    # DyNAM choice (the C++ estimator filters receivers). Other engine/model
    # combinations fall back to the default (R) engine, which consumes the mask via
    # the sender/receiver filters or the REM contribution.
    # A DyNAM-choice constraint (alter or point) folds into `active_dyad`, and a
    # DyNAM-rate constraint folds its sender gate into `active_sender`, during
    # preprocessing: every engine reads the folded object
    # directly, so neither the standalone mask nor the per-event opportunity
    # reduction is passed. An ego-kind (outer) choice constraint is not yet folded
    # and still rides the standalone mask path.
    choice_folded <- is_one_sided_choice && isTRUE(prep$active_dyad_folded)
    rate_folded <- is_rate_family && isTRUE(prep$active_sender_folded)
    # A folded standard- or ordinal-REM constraint rides the dense point
    # `active_dyad`, which `estimate_REM` / `estimate_REM_ordered`
    # consume cell-wise, so `default_c` runs it natively; the gather likewise
    # builds masked candidates. A folded rate constraint rides `active_sender`,
    # which `estimate_DyNAM_rate` already consumes as its sender
    # filter. A folded coordination constraint rides the symmetrised dense point
    # `active_dyad`, consumed as the full mask by `estimate_DyNAM_MM`
    # and the encoding-aware gather.
    rem_folded <- is_rem_family && isTRUE(prep$active_dyad_folded)
    rem_ordered_folded <- is_rem_ordered_family &&
      isTRUE(prep$active_dyad_folded)
    coord_folded <- is_coord_family && isTRUE(prep$active_dyad_folded)
    # A constrained coordination model now runs natively on `gather_compute`:
    # the gather emits the symmetrically-folded off-diagonal dyad list (only
    # mask-allowed rows) plus the per-sender groups and (i,j)<->(j,i) pairing,
    # and the dyad-triangle kernel reads that ragged list directly — no square
    # n x n candidate matrix is required. The former redirect to
    # `default_c` is retired.
    native_compiled <-
      (control_estimation$engine == "gather_compute" &&
        (is_one_sided_choice ||
          is_rate_family ||
          (is_rem_family && rem_folded) ||
          (is_rem_ordered_family && rem_ordered_folded) ||
          (is_coord_family && coord_folded))) ||
      (control_estimation$engine == "default_c" &&
        (is_one_sided_choice ||
          (is_rate_family && rate_folded) ||
          (is_rem_family && rem_folded) ||
          (is_rem_ordered_family && rem_ordered_folded) ||
          (is_coord_family && coord_folded)))
    if (native_compiled) {
      if (
        !choice_folded &&
          !rate_folded &&
          !rem_folded &&
          !rem_ordered_folded &&
          !coord_folded
      ) {
        support_gather <- prep$support_mask$support
      }
    } else {
      # Non-native path = the default (R) engine only: every reachable constrained
      # model (DyNAM choice / rate / coordination, standard + ordinal REM) folds
      # its availability and runs natively on gather_compute / default_c above, and
      # the other families abort at the guard before this branch — so the former
      # "engine does not yet consume … using default" downgrade is dead and has
      # been lifted.
      if (is_one_sided_choice) {
        if (!choice_folded) {
          # An unfolded (ego-kind / outer) choice constraint still reduces to the
          # per-event opportunity list for the default engine.
          opportunities_effective <- mask_to_opportunities(
            prep$support_mask,
            prep,
            opportunities_effective
          )
        }
      } else if (is_rate_family) {
        # The default engine consumes the folded `active_sender` directly as its
        # sender filter; no separate sender gate is passed.
      } else if (is_coord_family) {
        # Coordination consumes the folded symmetrised dense `active_dyad` as its
        # full risk mask on the default engine (the `folded_full` path); nothing
        # separate is passed.
      } else if (!isTRUE(prep$active_dyad_folded)) {
        # REM (standard or ordinal): the contribution zeroes disallowed dyads from
        # the risk set. A REM constraint is folded into `active_dyad` during
        # preprocessing and consumed as the maintained risk mask; the
        # standalone mask remains only as a fallback when the fold did not apply.
        rem_mask <- prep$support_mask$support
      }
    }
    if (choice_folded) {
      # The folded `active_dyad` already carries any user opportunity list, so
      # the per-iteration opportunity recompute is skipped.
      opportunities_effective <- NULL
    }
  }

  args_estimation <- list(
    initialParameters = control_estimation$initial_parameters,
    fixedParameters = effective_fixed_parameters,
    maxIterations = as.integer(control_estimation$max_iterations),
    score_tol = control_estimation$score_tol,
    step_tol = control_estimation$step_tol,
    dampingIncreaseFactor = control_estimation$damping_increase_factor,
    dampingDecreaseFactor = control_estimation$damping_decrease_factor,
    returnEventProbabilities = control_estimation$return_probabilities,
    returnIntervalLogL = control_estimation$return_interval_loglik,
    return_event_scores = isTRUE(control_estimation$return_event_scores),
    statsList = prep,
    nodes = ds_nodes_frame(orig_src, .nodes),
    nodes2 = ds_nodes_frame(orig_src, .nodes2),
    hasIntercept = has_intercept,
    is_two_mode = is_two_mode,
    modelType = legacy_model_type(model_spec),
    # overridden damping
    initialDamping = if (!is.null(control_estimation$initial_damping)) {
      control_estimation$initial_damping
    } else {
      ifelse(has_windows, 30, 10)
    },
    parallelize = FALSE,
    cpus = 1,
    verbose = verbose,
    progress = progress,
    opportunitiesList = opportunities_effective,
    senderGate = sender_gate,
    remMask = rem_mask,
    supportMask = support_gather
  )

  # Call the appropriate estimation engine
  if (control_estimation$engine %in% c("default_c", "gather_compute")) {
    tryCatch(
      result <- do.call(
        "estimate_c_int",
        args = c(
          args_estimation,
          list(engine = control_estimation$engine, optimizer = optimizer)
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
  result$call[[2]] <- formula_keep
  ## added to allow printing/plotting of rate models with rightCnesoredEvents
  result$event_time <- prep$event_time
  result$right_censored_events <- prep$is_dependent == 0L

  return(result)
}

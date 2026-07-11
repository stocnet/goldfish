#' Control Parameters for Estimation
#'
#' Specifies control parameters for the model estimation process in
#' `[estimate]`.
#'
#' The damping factors arguments control the step size at each iteration of
#' the Newton-Raphson algorithm. They have a bigger impact in the first
#' iterations of the algorithm and will decrease by half after each iteration.
#' In particular, the increase factor is the one that is expected to play
#' a role during the first iterations where it's easier to improve
#' the log-likelihood.
#' In scenarios where the model is fit in a large dataset, for example,
#' when the number of actors in the system is large,
#' the `damping_increase_factor` and the `damping_decrease_factor` arguments
#' can be increased from the default values (e.g., 4 or 6) to speed up the
#' estimation process with large changes in the coefficients.
#' However, this should have the opposite effect in small datasets producing
#' large changes in the coefficients that would create more iterations
#' (in a similar vein to the step size parameter in gradient descendent).

#'
#' @param initial_parameters A numeric vector. It includes initial parameter
#'   values used to initialize the estimation process.
#'   Default is `NULL`, which means parameters are initialized at zero,
#'   except for the rate intercept when present.
#' @param fixed_parameters `r lifecycle::badge("superseded")` A numeric vector
#'   of the same length as the number of parameters to be estimated in the model.
#'   `NA` values indicate parameters to be estimated,
#'   while numeric values indicate parameters to be fixed at the given value.
#'   For example, if the vector is `c(2, NA)` then the first component of the
#'   parameter is fixed to 2 during the estimation process.
#'   Default is `NULL` (all parameters are estimated).
#'   Superseded by wrapping the term in `offset()` in the model formula and
#'   supplying its value through `offset_coef`, which aligns values to terms by
#'   name instead of by counting coefficient positions.
#' @param offset_coef A numeric vector giving the fixed coefficient value(s) for
#'   the `offset()` term(s) in the model formula, aligned to the offset terms in
#'   formula order. For example, `~ inertia + offset(ego(sex)) + recip` with
#'   `offset_coef = 2` holds the `ego(sex)` coefficient at 2 while estimating the
#'   rest. Default is `NULL` (no offsets).
#' @param max_iterations An integer non-negative.
#'   The maximum number of iterations in the Gauss-Fisher scoring algorithm.
#'   Default is `20`.
#' @param score_tol A positive numeric value.
#'   Scale-invariant gradient convergence criterion.
#'   The algorithm stops if
#'   `max(abs(score)) / max(1, abs(logLik)) <= score_tol`.
#'   Default is `1e-6`.
#' @param step_tol A positive numeric value.
#'   Damped Newton step size convergence criterion.
#'   The algorithm stops if `max(abs(update)) <= step_tol`.
#'   Default is `1e-8`.
#' @param convergence_criterion `r lifecycle::badge("deprecated")`.
#'   Use `score_tol` instead. This argument is ignored.
#' @param initial_damping A positive numeric value.
#'   The initial damping factor for the Gauss-Fisher scoring algorithm.
#'   Default is `NULL`, which allows `estimate_dynam()`, `estimate_rem()` and
#'   `estimate_dynami()` to set
#'   a context-dependent default
#'   (e.g., 30 or 10 based on wheter the model has windows effects).
#'   If set, this value is used directly.
#' @param damping_increase_factor A positive numeric value.
#'   Factor by which damping is increased when improvements
#'   in the estimation are found. Must be >= 1. Default is `2`.
#' @param damping_decrease_factor A positive numeric value.
#'   Factor by which damping is decreased when no improvements
#'   in the estimation are found. Must be >= 1. Default is `3`.
#' @param return_interval_loglik A logical value.
#'   Whether to keep and return the
#'   log-likelihood for each event. Default is `FALSE`.
#' @param return_probabilities A logical value.
#'   Whether to keep and return the
#'   probabilities for all alternatives for each event.
#'   * When `subModel = "choice"` the probabilities correspond to all actors in
#'     the choice set present at the time of the event.
#'   * When `model = "REM"` the probabilities correspond to all dyads present at
#'     the time of the event.
#'   Default is `FALSE`.
#' @param return_event_scores A logical value.
#'   Whether to keep and return the per-event score matrix (one row per
#'   dependent event, one column per effect) evaluated at the returned
#'   parameter estimates, stored as the `event_scores` component of the result.
#'   Each row is the observation-level gradient contribution whose column sums
#'   equal the aggregate score. The matrix supports downstream diagnostics
#'   (implemented by other tools, not here): robust sandwich and clustered
#'   standard errors from the outer product of gradients `crossprod(event_scores)`,
#'   per-effect score-process diagnostics that localize where individual effects
#'   drift over the event sequence, and event-influence measures. Only the
#'   `"default_c"` and `"default"` engines support it; `"gather_compute"` aborts.
#'   Default is `FALSE`.
#' @param optimizer `r lifecycle::badge("experimental")` A character string
#'   naming the optimization algorithm. Options are:
#'   \describe{
#'      \item{newton_raphson}{The built-in damped Newton-Raphson / Fisher
#'       scoring loop (default).}
#'      \item{bfgs}{Quasi-Newton BFGS, via `maxLik::maxLik()`.}
#'      \item{bhhh}{Berndt-Hall-Hall-Hausman, using the per-event score matrix
#'       as observation-level gradients, via `maxLik::maxLik()`.}
#'      \item{nelder_mead}{Derivative-free Nelder-Mead, via `maxLik::maxLik()`.}
#'    }
#'   Any value other than `"newton_raphson"` requires the \pkg{maxLik} package
#'   (in `Suggests`) and runs only on the `"default_c"` engine. Default is
#'   `"newton_raphson"`.
#' @param engine A character string specifying the estimation engine.
#'   Options are:
#'   \describe{
#'      \item{default_c}{`C++` based implementation using RcppEigen
#'       and RcppParallel.}
#'      \item{default}{R-based implementation.}
#'      \item{gather_compute}{`C++` based implementation with a different data
#'       structure that reduces the time but it can increase the memory usage.}
#'    }
#'   Default is `"default_c"`.
#'
#' @return An object of class `estimation_opt.goldfish` (a list object),
#'  where the components values are the default values or the values provided
#'  to the function. The list object has the following components:
#'   \item{initial_parameters}{Initial parameter values used during
#'      the estimation process.}
#'   \item{fixed_parameters}{Values for parameters fixed during
#'      the estimation process.}
#'   \item{max_iterations}{Maximum number of iterations in the
#'      estimation process.}
#'   \item{score_tol}{Scale-invariant gradient convergence criterion for
#'      the estimation process.}
#'   \item{step_tol}{Damped Newton step size convergence criterion for
#'      the estimation process.}
#'   \item{initial_damping}{Initial damping factor for the
#'      estimation process.}
#'   \item{damping_increase_factor}{Factor by which damping is
#'      increased when improvements in the estimation are found.}
#'   \item{damping_decrease_factor}{Factor by which damping is
#'      decreased when no improvements in the estimation are found.}
#'   \item{return_interval_loglik}{Logical value indicating whether to
#'      return the log-likelihood for each event.}
#'   \item{return_event_scores}{Logical value indicating whether to
#'      return the per-event score matrix.}
#'   \item{optimizer}{Optimization algorithm used in the estimation process.}
#'   \item{engine}{Estimation engine used in the estimation process.}
#' @export
#' @examples
#' est_ctrl <- set_estimation_opt(
#'   max_iterations = 50,
#'   score_tol = 1e-7,
#'   step_tol = 1e-9
#' )
set_estimation_opt <- function(
  initial_parameters = NULL,
  fixed_parameters = NULL,
  offset_coef = NULL,
  max_iterations = 20,
  convergence_criterion = deprecated(),
  score_tol = 1e-6,
  step_tol = 1e-8,
  initial_damping = NULL,
  damping_increase_factor = 2,
  damping_decrease_factor = 3,
  return_interval_loglik = TRUE,
  return_probabilities = FALSE,
  return_event_scores = FALSE,
  optimizer = c("newton_raphson", "bfgs", "bhhh", "nelder_mead"),
  engine = c("default_c", "default", "gather_compute")
) {
  engine <- match.arg(engine)
  optimizer <- match.arg(optimizer)

  if (lifecycle::is_present(convergence_criterion)) {
    lifecycle::deprecate_warn(
      when = "1.7.2",
      what = "set_estimation_opt(convergence_criterion)",
      details = paste0(
        "`convergence_criterion` is ignored; ",
        "please use `score_tol` instead (default: 1e-6)."
      )
    )
  }

  # Argument checks
  if (!is.null(initial_parameters) && !is.numeric(initial_parameters)) {
    stop(
      "'initial_parameters' must be a numeric vector or NULL.",
      call. = FALSE
    )
  }
  if (!is.null(fixed_parameters)) {
    if (!is.numeric(fixed_parameters)) {
      stop(
        "'fixed_parameters' must be a numeric vector or NULL.",
        call. = FALSE
      )
    }
    lifecycle::deprecate_soft(
      when = "1.8.4",
      what = "set_estimation_opt(fixed_parameters)",
      details = c(
        "!" = "Wrap the term in `offset()` in the model formula and supply its
               value through `offset_coef` instead.",
        "i" = "`offset()` aligns fixed values to terms by name rather than by
               counting coefficient positions."
      )
    )
  }
  if (!is.null(offset_coef) && !is.numeric(offset_coef)) {
    stop("'offset_coef' must be a numeric vector or NULL.", call. = FALSE)
  }
  if (!is.null(fixed_parameters) && !is.null(offset_coef)) {
    cli::cli_abort(c(
      "{.arg fixed_parameters} and {.arg offset_coef} cannot both be supplied.",
      "i" = "Use {.arg offset_coef} with {.fn offset} terms in the formula
             ({.arg fixed_parameters} is superseded)."
    ))
  }
  if (
    !rlang::is_scalar_integerish(max_iterations, finite = TRUE) ||
      max_iterations < 0
  ) {
    stop(
      "'max_iterations' must be a single non-negative integer.",
      call. = FALSE
    )
  }
  if (!rlang::is_scalar_double(score_tol) || score_tol <= 0) {
    stop("'score_tol' must be a single positive numeric value.", call. = FALSE)
  }
  if (!rlang::is_scalar_double(step_tol) || step_tol <= 0) {
    stop("'step_tol' must be a single positive numeric value.", call. = FALSE)
  }
  if (
    !is.null(initial_damping) &&
      (!is.numeric(initial_damping) ||
        length(initial_damping) != 1 ||
        initial_damping <= 0)
  ) {
    stop(
      "'initial_damping' must be a single positive numeric value or NULL.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(damping_increase_factor) ||
      length(damping_increase_factor) != 1 ||
      damping_increase_factor < 1
  ) {
    stop(
      "'damping_increase_factor' must be a single numeric value >= 1.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(damping_decrease_factor) ||
      length(damping_decrease_factor) != 1 ||
      damping_decrease_factor < 1
  ) {
    stop(
      "'damping_decrease_factor' must be a single numeric value >= 1.",
      call. = FALSE
    )
  }
  if (!rlang::is_scalar_logical(return_interval_loglik)) {
    stop(
      "'return_interval_loglik' must be a single logical value.",
      call. = FALSE
    )
  }
  if (!rlang::is_scalar_logical(return_probabilities)) {
    stop(
      "'return_probabilities' must be a single logical value.",
      call. = FALSE
    )
  }
  if (!rlang::is_scalar_logical(return_event_scores)) {
    stop(
      "'return_event_scores' must be a single logical value.",
      call. = FALSE
    )
  }

  control_list <- list(
    initial_parameters = initial_parameters,
    fixed_parameters = fixed_parameters,
    offset_coef = offset_coef,
    max_iterations = max_iterations,
    score_tol = score_tol,
    step_tol = step_tol,
    initial_damping = initial_damping,
    damping_increase_factor = damping_increase_factor,
    damping_decrease_factor = damping_decrease_factor,
    return_interval_loglik = return_interval_loglik,
    return_probabilities = return_probabilities,
    return_event_scores = return_event_scores,
    optimizer = optimizer,
    engine = engine
  )

  class(control_list) <- c("estimation_opt.goldfish", "list")
  return(control_list)
}

#' Control Parameters for Preprocessing
#'
#' Specifies control parameters for the data preprocessing stage,
#' used by `estimate_dynam()`, `estimate_rem()` and `estimate_dynami()`
#' (when `preprocessingInit` is not a
#' `preprocessed.goldfish` object) and `gather_model_data()`.
#'
#' @param start_time A numerical value or a date-time character string
#'   (parsable by `as.POSIXct`) indicating the starting time when the events
#'   are considered for likelihood computation.
#'   All the events that happen before the `start_time` are used to compute
#'   the initial values of the effects statistics in the model.
#'   It's useful to set this parameter when the model has windowed effects or
#'   effects that depends of previous order of events (e.g., `trans()` and
#'   `cycle()` when `history` argument is set to sequential or consecutive,
#'   as they are initialized with empty values.
#'   Default is `NULL` (start from the first event).
#' @param end_time A numerical value or a date-time character string
#'   (parsable by `as.POSIXct`) indicating the end time when the events
#'   are not to be considered for likelihood computation.
#'   The preprocessing stage won't stop at this time and will continue
#'   processing events after this time.
#'   Default is `NULL` (end with the last event).
#' @param opportunities_list `r lifecycle::badge("deprecated")` A list object.
#'   For choice models, this list specifies, for each dependent event,
#'   the set of available nodes in the choice set.
#'   The list should have the same length as the number of events in the
#'   dependent events objects created with `make_dependent_events()`.
#'   Default is `NULL`, so the choice set is the set of all nodes present at the
#'   time of the event. Superseded by the `support_constraint` argument of
#'   [estimate_dynam()] / [make_specification()], which generalizes it to a
#'   per-`(sender, receiver)` risk-set restriction and works on every engine; an
#'   equivalent constraint over an allowed-dyad network reproduces the
#'   opportunity-list coefficients.
#' @param db A `DBIConnection` object or `NULL` (default). When supplied
#'   together with `compute_stats(..., output = "db")`, the gather statistics
#'   are streamed to the database table named by `db_table` instead of being
#'   held in memory.
#' @param db_table A single character string naming the database table to
#'   write to when a `db` connection is configured. Default is `"stats"`.
# @param keep_sender_index A logical value. If `TRUE`, the sender index,
#   the index in the nodeset, of the potential senders of the events is
#   kept in the preprocessed data.
# @param keep_receiver_index A logical value. If `TRUE`, the receiver index,
#  the index in the nodeset, of the potential receivers of the events is
#  kept in the preprocessed data.
#' @return An object of class `preprocessing_opt.goldfish` (a list object), with
#'  where the components values are the default values or the values provided
#'  to the function. The list object has the following components:
#'   \item{start_time}{Value from `start_time` argument.}
#'   \item{end_time}{Value from `end_time` argument.}
#'   \item{opportunities_list}{Value from `opportunities_list` argument.}
#' @export
#' @examples
#' prep_ctrl <- set_preprocessing_opt(
#'   start_time = "2000-01-01 00:00:00",
#'   end_time = "2000-12-31 23:59:59"
#' )
set_preprocessing_opt <- function(
  start_time = NULL,
  end_time = NULL,
  opportunities_list = NULL,
  db = NULL,
  db_table = "stats"
) {
  # Argument checks
  classesAllowed <- c("numeric", "character", "POSIXlt", "POSIXct", "POSIXt")
  if (!is.null(start_time)) {
    if (length(start_time) != 1) {
      stop("'start_time' must be NULL or a single value.", call. = FALSE)
    }
    if (!any(check_classes(start_time, classesAllowed))) {
      stop(
        "'start_time' must be NULL or an object of class: ",
        paste(classesAllowed, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(end_time)) {
    if (length(end_time) != 1) {
      stop("'end_time' must be NULL or a single value.", call. = FALSE)
    }
    if (!any(check_classes(end_time, classesAllowed))) {
      stop(
        "'end_time' must be NULL or an object of class: ",
        paste(classesAllowed, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(opportunities_list)) {
    lifecycle::deprecate_warn(
      when = "1.8.6",
      what = "set_preprocessing_opt(opportunities_list)",
      details = c(
        i = paste(
          "Use the `support_constraint` argument of `estimate_dynam()` /",
          "`make_specification()` instead."
        )
      )
    )
    if (!is.list(opportunities_list)) {
      stop(
        "'opportunities_list' must be a list or NULL.",
        call. = FALSE
      )
    }
    # check every element is a integer or character vector
    is_vector <- vapply(
      opportunities_list,
      \(x) any(check_classes(x, c("integer", "character"))),
      logical(1)
    )
    if (!all(is_vector)) {
      stop(
        "'opportunities_list' must be a list of integer or character vectors.",
        call. = FALSE
      )
    }
  }

  # if (!rlang::is_scalar_logical(keep_sender_index)) {
  #   stop(
  #     "'keep_sender_index' must be a single logical value.",
  #     call. = FALSE
  #   )
  # }
  #
  # if (!rlang::is_scalar_logical(keep_receiver_index)) {
  #   stop(
  #     "'keep_receiver_index' must be a single logical value.",
  #     call. = FALSE
  #   )
  # }

  if (!is.null(db) && !inherits(db, "DBIConnection")) {
    stop("'db' must be NULL or a DBI connection object.", call. = FALSE)
  }
  if (!is.character(db_table) || length(db_table) != 1L) {
    stop("'db_table' must be a single character string.", call. = FALSE)
  }

  control_list <- list(
    start_time = start_time,
    end_time = end_time,
    opportunities_list = opportunities_list,
    db = db,
    db_table = db_table
  )

  class(control_list) <- c("preprocessing_opt.goldfish", "list")
  return(control_list)
}

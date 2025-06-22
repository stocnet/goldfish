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
#' @param fixed_parameters A numeric vector of the same length
#'   as the number of parameters to be estimated in the model.
#'   `NA` values indicate parameters to be estimated,
#'   while numeric values indicate parameters to be fixed at the given value.
#'   For example, if the vector is `c(2, NA)` then the first component of the
#'   parameter is fixed to 2 during the estimation process.
#'   Default is `NULL` (all parameters are estimated).
#' @param max_iterations An integer.
#'   The maximum number of iterations in the Gauss-Fisher scoring algorithm.
#'   Default is `20`.
#' @param convergence_criterion A numeric value.
#'   The convergence criterion for the estimation.
#'   The algorithm stops if the sum of absolute scores is smaller than
#'   this value.
#'   Default is `0.001`.
#' @param initial_damping A numeric value.
#'   The initial damping factor for the Gauss-Fisher scoring algorithm.
#'   Default is `NULL`, which allows `estimate_dynam()`, `estimate_rem()` and
#'   `estimate_dynami()` to set
#'   a context-dependent default
#'   (e.g., 30 or 10 based on wheter the model has windows effects).
#'   If set, this value is used directly.
#' @param damping_increase_factor A numeric value.
#'   Factor by which damping is increased when improvements
#'   in the estimation are found. Must be >= 1. Default is `2`.
#' @param damping_decrease_factor A numeric value.
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
#'   \item{convergence_criterion}{Convergence criterion for the
#'      estimation process.}
#'   \item{initial_damping}{Initial damping factor for the
#'      estimation process.}
#'   \item{damping_increase_factor}{Factor by which damping is
#'      increased when improvements in the estimation are found.}
#'   \item{damping_decrease_factor}{Factor by which damping is
#'      decreased when no improvements in the estimation are found.}
#'   \item{return_interval_loglik}{Logical value indicating whether to
#'      return the log-likelihood for each event.}
#'   \item{engine}{Estimation engine used in the estimation process.}
#' @export
#' @examples
#' est_ctrl <- set_estimation_opt(
#'   max_iterations = 50,
#'   convergence_criterion = 1e-4
#' )
set_estimation_opt <- function(
  initial_parameters = NULL,
  fixed_parameters = NULL,
  max_iterations = 20,
  convergence_criterion = 0.001,
  initial_damping = NULL,
  damping_increase_factor = 2,
  damping_decrease_factor = 3,
  return_interval_loglik = FALSE,
  return_probabilities = FALSE,
  engine = c("default_c", "default", "gather_compute")
) {
  engine <- match.arg(engine)

  # Argument checks
  if (!is.null(initial_parameters) && !is.numeric(initial_parameters)) {
    stop(
      "'initial_parameters' must be a numeric vector or NULL.",
      call. = FALSE
    )
  }
  if (!is.null(fixed_parameters) && !is.numeric(fixed_parameters)) {
    stop("'fixed_parameters' must be a numeric vector or NULL.", call. = FALSE)
  }
  if (!is.numeric(max_iterations) || length(max_iterations) != 1 ||
      max_iterations <= 0 || floor(max_iterations) != max_iterations) {
    stop("'max_iterations' must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(convergence_criterion) ||
      length(convergence_criterion) != 1 || convergence_criterion <= 0) {
    stop(
      "'convergence_criterion' must be a single positive numeric value.",
      call. = FALSE
    )
  }
  if (!is.null(initial_damping) && (!is.numeric(initial_damping) ||
      length(initial_damping) != 1 || initial_damping <= 0)) {
    stop(
      "'initial_damping' must be a single positive numeric value or NULL.",
      call. = FALSE
    )
  }
  if (!is.numeric(damping_increase_factor) ||
      length(damping_increase_factor) != 1 || damping_increase_factor < 1) {
    stop(
      "'damping_increase_factor' must be a single numeric value >= 1.",
      call. = FALSE
    )
  }
  if (!is.numeric(damping_decrease_factor) ||
      length(damping_decrease_factor) != 1 || damping_decrease_factor < 1) {
    stop(
      "'damping_decrease_factor' must be a single numeric value >= 1.",
      call. = FALSE
    )
  }
  if (!is.logical(return_interval_loglik) ||
      length(return_interval_loglik) != 1) {
    stop(
      "'return_interval_loglik' must be a single logical value.",
      call. = FALSE
    )
  }
  if (!is.logical(return_probabilities) ||
      length(return_probabilities) != 1) {
    stop(
      "'return_probabilities' must be a single logical value.",
      call. = FALSE
    )
  }

  control_list <- list(
    initial_parameters = initial_parameters,
    fixed_parameters = fixed_parameters,
    max_iterations = max_iterations,
    convergence_criterion = convergence_criterion,
    initial_damping = initial_damping,
    damping_increase_factor = damping_increase_factor,
    damping_decrease_factor = damping_decrease_factor,
    return_interval_loglik = return_interval_loglik,
    return_probabilities = return_probabilities,
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
#' @param opportunities_list A list object. For choice models,
#'   this list specifies, for each dependent event,
#'   the set of available nodes in the choice set.
#'   The list should have the same length as the number of events in the
#'   dependent events objects created with `make_dependent_events()`.
#'   Default is `NULL`, so the choice set is the set of all nodes present at the
#'   time of the event.
#'
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
  opportunities_list = NULL
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
        paste(classesAllowed, collapse = ", "), ".",
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
        paste(classesAllowed, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(opportunities_list)) {
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
      logical(1))
    if (!all(is_vector)) {
      stop(
        "'opportunities_list' must be a list of integer or character vectors.",
        call. = FALSE
      )
    }
  }

  control_list <- list(
    start_time = start_time,
    end_time = end_time,
    opportunities_list = opportunities_list
  )

  class(control_list) <- c("preprocessing_opt.goldfish", "list")
  return(control_list)
}

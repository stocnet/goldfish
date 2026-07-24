#' Functions renamed in goldfish 2.0.0
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' goldfish 2.0.0 aligned the estimation control surface with the naming
#' conventions shared across the \href{https://github.com/stocnet}{stocnet}
#' ecosystem: control constructors are `set_algorithm_*()` / `set_*()`, and
#' the estimators take them as `control_algo` / `control_prep`.
#'
#' * `set_estimation_opt()` -> [set_algorithm_newton()]
#' * `set_preprocessing_opt()` -> [set_preprocessing()]
#'
#' The old names keep working as thin wrappers that forward every argument
#' unchanged, so results are identical; calling one directly emits a
#' soft-deprecation warning naming its replacement. They are scheduled for
#' removal no earlier than goldfish 3.0.0, together with the 1.7.0 layer
#' documented in [defunct].
#'
#' @param ... Arguments passed on to the replacement function.
#' @keywords internal
#' @name goldfish-deprecated
#' @aliases NULL
NULL

#' @rdname goldfish-deprecated
#' @export
set_estimation_opt <- function(...) {
  # Emitted from this frame so lifecycle attributes the warning to the user's
  # call: a wrapper reached from inside goldfish stays silent, as intended.
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = "set_estimation_opt()",
    with = "set_algorithm_newton()"
  )
  set_algorithm_newton(...)
}

#' @rdname goldfish-deprecated
#' @export
set_preprocessing_opt <- function(...) {
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = "set_preprocessing_opt()",
    with = "set_preprocessing()"
  )
  set_preprocessing(...)
}

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
#' * `examine_outliers()` -> [diagnose_outliers()]
#' * `examine_changepoints()` -> [diagnose_changepoints()]
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

#' @rdname goldfish-deprecated
#' @export
examine_outliers <- function(...) {
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = "examine_outliers()",
    with = "diagnose_outliers()"
  )
  diagnose_outliers(...)
}

#' @rdname goldfish-deprecated
#' @export
examine_changepoints <- function(...) {
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = "examine_changepoints()",
    with = "diagnose_changepoints()"
  )
  diagnose_changepoints(...)
}

# Warn for the estimators' `preprocessing_only`, whose replacement is a
# function rather than another argument: compute_statistics(). Called from each
# estimator with its own name so the message names the call the user wrote;
# `user_env` reaches past this helper and that estimator.
warn_preprocessing_only <- function(fn, user_env = rlang::caller_env(2)) {
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = paste0(fn, "(preprocessing_only)"),
    with = I('compute_statistics(output = "preprocessed")'),
    user_env = user_env
  )
}

# Fold a pre-2.0.0 estimator argument into the argument that replaced it.
# `new_supplied` decides precedence: an explicitly supplied new argument wins
# over the deprecated one, which is otherwise honored unchanged. The warning is
# emitted whenever the old name is used, since that is what the user has to
# change. `details` adds bullets for a rename that also changes what the
# argument accepts, keeping it to a single warning. `user_env` reaches past this
# helper and its caller so lifecycle attributes the warning to the user's own
# call.
fold_renamed_arg <- function(
  new,
  new_supplied,
  old,
  fn,
  old_name,
  new_name,
  details = NULL,
  user_env = rlang::caller_env(2)
) {
  if (!lifecycle::is_present(old)) {
    return(new)
  }
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = paste0(fn, "(", old_name, ")"),
    with = paste0(fn, "(", new_name, ")"),
    details = c(
      if (new_supplied) {
        c(
          "!" = paste0(
            "Both were supplied; the value of `",
            new_name,
            "` is used."
          )
        )
      },
      details
    ),
    user_env = user_env
  )
  if (new_supplied) new else old
}

##################### ###
#
# Goldfish package
# Fitted values and in-sample prediction for a fitted model
#
##################### ###

#' Fitted values of a goldfish model
#'
#' @description
#' The model's own fitted quantities at the observed decision points:
#' `"outcome"` (the default) is the fitted probability — for the exact-time
#' sub-models, the density contribution — of each interval's realized outcome,
#' and `"probabilities"` is the full fitted vector over the alternatives at
#' each of them.
#'
#' @details
#' `"outcome"` is `exp()` of the stored per-interval log-likelihood, so it
#' costs nothing: a fit that stored the `"loglik"` primitive (the default) can
#' always produce it. `"probabilities"` is the per-event vector over the whole
#' node set, read from the fit when it stored the `"probabilities"` primitive
#' and otherwise recomputed through one evaluation pass. That primitive is the
#' one estimation warns about the size of, so recomputing rather than storing
#' is the ordinary case.
#'
#' @param object a fitted model of class `"result.goldfish"`.
#' @param type the quantity to return, `"outcome"` (default) or
#'   `"probabilities"`.
#' @inheritParams residuals.result.goldfish
#'
#' @return For `"outcome"`, a numeric vector with one value per interval. For
#'   `"probabilities"`, a list with one per-event vector or dyad grid over the
#'   whole node set, zero off that interval's risk set.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = social_evolution
#' )
#' # How probable was what actually happened, event by event?
#' summary(fitted(fit))
#'
#' @seealso [predict.result.goldfish()] for the same quantities under the
#'   prediction vocabulary, [residuals.result.goldfish()] for what the model
#'   got wrong.
#' @method fitted result.goldfish
#' @export
fitted.result.goldfish <- function(
  object,
  type = c("outcome", "probabilities"),
  preprocessed = NULL,
  ...
) {
  abort_if_stale_result(object, "fitted values")
  type <- match.arg(type)
  if (identical(type, "outcome")) {
    return(exp(residual_stored(
      object,
      "interval_log_lik",
      "loglik",
      type,
      noun = "Fitted values"
    )))
  }
  fitted_probabilities(object, preprocessed)
}

# The per-event probability vectors, stored if the fit has them and recomputed
# if not. Both `fitted()` and `predict()` want exactly this, and asking twice
# in two spellings is how the two would come to disagree.
fitted_probabilities <- function(
  object,
  preprocessed,
  at = NULL,
  call = rlang::caller_env()
) {
  stored <- object$event_probabilities
  if (is.null(at) && !is.null(stored)) {
    return(stored)
  }
  prep <- resolve_preprocessed(preprocessed, object, call = call)
  arguments <- list(
    object,
    return = "probabilities",
    preprocessed = prep
  )
  if (!is.null(at)) {
    arguments$at <- at
  }
  do.call(evaluate_model, arguments)$probabilities
}

#' In-sample prediction from a fitted goldfish model
#'
#' @description
#' What the fitted model says at the observed decision points: the probability
#' it assigns to each alternative, or the rank it gives the alternative that
#' was actually realized.
#'
#' @details
#' This is **in-sample prediction given the observed history**, not
#' forecasting. Every quantity is evaluated at a decision point the data
#' contains, with the endogenous statistics as the observed sequence made
#' them — so `type = "ranks"` answers "how well would the model have called
#' this event, knowing everything up to it", not "what happens next". Rolling
#' the process forward requires simulating the events that would have
#' followed, which is a different computation and not this method. These are
#' also not marginal effects: a probability here is conditional on that
#' interval's realized risk set.
#'
#' At the fitted estimate both quantities come off the fit when it stored the
#' primitive they need — `"ranks"` from `observed_rank`, `"probabilities"`
#' from `event_probabilities` — and are recomputed through one evaluation pass
#' otherwise. Supplying `at` always evaluates, since the stored values belong
#' to the fitted coefficients.
#'
#' @param object a fitted model of class `"result.goldfish"`.
#' @param type the quantity to predict, `"probabilities"` (default) or
#'   `"ranks"`.
#' @param events an optional index vector selecting the intervals to report,
#'   either integer positions or a logical mask over all intervals. Defaults to
#'   all of them.
#' @param at an optional parameter vector to predict at, in the form
#'   [evaluate_model()] accepts. Defaults to the fitted coefficients.
#' @inheritParams residuals.result.goldfish
#'
#' @return For `"ranks"`, an integer vector with the realized alternative's
#'   rank in each interval, `1` being the most likely one and `NA` on a
#'   right-censored interval, which realizes none. For `"probabilities"`, a
#'   list with one per-event vector or dyad grid over the whole node set.
#'   Either is subset to `events` when that is given.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(diagnostics = c("loglik", "ranks"))
#' )
#' # How often was the actor who was actually called the model's first guess?
#' mean(predict(fit, type = "ranks") == 1)
#'
#' @inheritSection diagnostic-requirements What a diagnostic needs
#'
#' @section What these need:
#'
#' The model's **statistics** and **one evaluation pass**: a fitted value is a
#' per-alternative quantity over the realized risk set, which no fit stores
#' unless `"probabilities"` was requested, and which is in any case recomputed
#' at whatever parameter vector is asked for.
#'
#' @seealso [fitted.result.goldfish()] for the same quantities under the
#'   fitted-value vocabulary, [evaluate_model()] for evaluation at any
#'   parameter vector.
#' @method predict result.goldfish
#' @export
predict.result.goldfish <- function(
  object,
  type = c("probabilities", "ranks"),
  events = NULL,
  at = NULL,
  preprocessed = NULL,
  ...
) {
  abort_if_stale_result(object, "predictions")
  type <- match.arg(type)
  predicted <- if (identical(type, "ranks")) {
    predicted_ranks(object, preprocessed, at)
  } else {
    fitted_probabilities(object, preprocessed, at)
  }
  subset_intervals(predicted, events, length(object$right_censored_events))
}

# The realized alternative's rank. At the fitted estimate a fit that stored the
# ranks already holds the answer, and running a pass to reproduce it would be a
# pass nobody needs -- the evaluator route is for `at` elsewhere, or for a fit
# that stored none.
predicted_ranks <- function(
  object,
  preprocessed,
  at = NULL,
  call = rlang::caller_env()
) {
  stored <- object$observed_rank
  if (is.null(at) && !is.null(stored)) {
    return(stored)
  }
  prep <- resolve_preprocessed(preprocessed, object, call = call)
  arguments <- list(object, return = "ranks", preprocessed = prep)
  if (!is.null(at)) {
    arguments$at <- at
  }
  do.call(evaluate_model, arguments)$ranks
}

# Select intervals by position or by mask. Validated against the interval count
# rather than silently recycling or padding: an out-of-range index means the
# caller is indexing something other than this fit's intervals.
subset_intervals <- function(
  x,
  events,
  n_intervals,
  call = rlang::caller_env()
) {
  if (is.null(events)) {
    return(x)
  }
  if (is.logical(events)) {
    if (length(events) != n_intervals) {
      cli::cli_abort(
        c(
          "A logical {.arg events} must have one value per interval.",
          "x" = "It has {length(events)}; the fit has {n_intervals}."
        ),
        call = call
      )
    }
    events <- which(events)
  }
  if (!is.numeric(events) || any(events < 1) || any(events > n_intervals)) {
    cli::cli_abort(
      c(
        "{.arg events} must index this fit's intervals.",
        "x" = "It must lie in {.val {1}}:{.val {n_intervals}}."
      ),
      call = call
    )
  }
  x[events]
}

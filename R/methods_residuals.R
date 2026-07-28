##################### ###
#
# Goldfish package
# Residuals of a fitted model
#
##################### ###

# The residual types this method serves, and which stored primitive each needs.
# Types are grouped by what they read, not by what they mean, because that is
# what decides whether a fit can produce them at all.
RESIDUAL_TYPES_LOGLIK <- c("deviance")
RESIDUAL_TYPES_SCORES <- c(
  "schoenfeld",
  "score",
  "dfbeta",
  "dfbetas",
  "cooks"
)

#' Residuals of a fitted goldfish model
#'
#' @description
#' Per-event residuals and influence measures, following the
#' [survival::coxph()] type vocabulary. Every type on this page is read
#' straight off the primitives estimation stored — none of them re-evaluates
#' the model — so they cost nothing beyond the arithmetic, and a fit that did
#' not store the primitive a type needs says so rather than recomputing it
#' silently.
#'
#' @details
#' The types, and what each one is:
#' \describe{
#'   \item{`deviance`}{`-2 *` the interval's log-likelihood contribution: how
#'     surprising that interval was under the fitted model. For the exact-time
#'     sub-models the contribution is a log **density**, not a log
#'     probability, so it can be positive and the deviance correspondingly
#'     negative; only differences between intervals are interpretable there.}
#'   \item{`score`}{the per-event score increments — one row per interval, one
#'     column per coefficient — that estimation summed into the gradient. For
#'     an exact-time sub-model the row includes the exposure term, so the rows
#'     of a converged fit sum to (numerically) zero.}
#'   \item{`schoenfeld`}{the observed-minus-expected statistic rows. On the
#'     multinomial sub-models (choice, the ordinal rate and REM sub-models,
#'     coordination) this **is** the score row — the expected statistic is the
#'     risk-set probability-weighted mean — so the two types coincide and the
#'     free-parameter columns sum to zero at the maximum.}
#'   \item{`dfbeta`}{the one-step approximate change in the coefficient vector
#'     from deleting an interval's likelihood term, `I^-1 s_k`: one row per
#'     interval, on the coefficient scale.}
#'   \item{`dfbetas`}{`dfbeta` divided by the coefficients' standard errors, so
#'     the columns are comparable across coefficients of different scales.}
#'   \item{`cooks`}{`s_k' I^-1 s_k`, the scalar self-influence of an interval
#'     (a Cook's-distance analog): one number per interval, large where a
#'     single interval moves the estimate a lot relative to its precision.}
#' }
#'
#' Coefficients held fixed through `offset()` carry no influence: their
#' `dfbeta` / `dfbetas` columns are zero, and only the estimated block enters
#' `cooks`, because a fixed coefficient does not move when an interval is
#' deleted.
#'
#' On a DyNAM fit the residuals are **conditional on the sub-model**: rate
#' residuals live over the sender risk set, choice residuals over the receiver
#' risk set given the observed sender. The two are separate fits and their
#' residuals are not commensurable.
#'
#' @section Caveats:
#' **Likelihood deletion is not history deletion.** `dfbeta`, `dfbetas` and
#' `cooks` remove an interval's *likelihood term* while that event remains
#' inside every subsequent endogenous statistic — the tie it created still
#' feeds inertia, reciprocity and every triadic count afterwards. They
#' therefore answer "how much did this term's contribution pull the estimate",
#' not "what would the estimate be had this event never happened". The
#' counterfactual question needs a statistics replay per deleted event, which
#' is a different and far more expensive computation, and none of these
#' measures estimates it.
#'
#' **Early events at the null benchmark are uninformative, not surprising.**
#' At the start of a sequence the history is left-censored: endogenous
#' statistics are still at their initial values, so every alternative looks
#' alike and each event's log-likelihood sits at the per-event null benchmark
#' (`-log` of the risk-set size). A deviance trace therefore opens flat and
#' high. That is the model having nothing to say yet, not the model fitting
#' badly, and reading it as misfit is the most common misreading of the trace.
#'
#' **Score-based diagnostics are blind at cold start.** When an endogenous
#' statistic is constant across the risk set, the observed alternative's
#' statistic equals the risk-set mean, so the score contribution is *exactly*
#' zero — not small. Early events therefore show no influence at all on the
#' endogenous coefficients, while the intercept and exogenous blocks can still
#' absorb them. An influence measure that looks reassuringly quiet over the
#' opening events is reporting this structural zero.
#'
#' The remedies are to warm-start the history (begin the observation window
#' after enough events have accumulated) or to exclude the opening segment
#' from estimation, and to compare the estimate with and without it.
#'
#' @param object a fitted model of class `"result.goldfish"`.
#' @param type the residual type, one of `"deviance"` (default), `"score"`,
#'   `"schoenfeld"`, `"dfbeta"`, `"dfbetas"` and `"cooks"`.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return For `"deviance"` and `"cooks"`, a numeric vector with one value per
#'   interval. For the others, a numeric matrix with one row per interval and
#'   one column per coefficient, carrying the column names of the stored
#'   per-event score matrix (the effect names). Right-censored intervals are
#'   included: they contribute a likelihood term and a score.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution
#' )
#' summary(residuals(fit))
#' # One row per event, one column per coefficient.
#' head(residuals(fit, type = "score"))
#' # Which events moved the estimate most?
#' head(order(residuals(fit, type = "cooks"), decreasing = TRUE))
#'
#' @seealso [estimate_dynam()] for the primitives these read,
#'   [set_algorithm_newton()] for requesting them, and [margin_table()] for
#'   the per-actor calibration counterpart.
#' @method residuals result.goldfish
#' @export
residuals.result.goldfish <- function(
  object,
  type = c("deviance", "score", "schoenfeld", "dfbeta", "dfbetas", "cooks"),
  ...
) {
  abort_if_stale_result(object, "residuals")
  type <- match.arg(type)
  if (type %in% RESIDUAL_TYPES_LOGLIK) {
    return(-2 * residual_stored(object, "interval_log_lik", "loglik", type))
  }
  scores <- residual_stored(object, "event_scores", "scores", type)
  if (identical(type, "schoenfeld")) {
    return(schoenfeld_rows(object, scores))
  }
  if (identical(type, "score")) {
    return(scores)
  }
  influence_rows(object, scores, type)
}

# The stored primitive a type reads, or the error that names how to store it.
# Stated as "this type needs that primitive" rather than "something is
# missing", so the fix is in the message.
residual_stored <- function(
  object,
  component,
  primitive,
  type,
  call = rlang::caller_env()
) {
  stored <- object[[component]]
  if (is.null(stored)) {
    cli::cli_abort(
      c(
        "Residuals of type {.val {type}} need the {.val {primitive}}
         primitive, which this fit did not store.",
        "i" = "Re-estimate with {.arg diagnostics} including
               {.val {primitive}} in {.fn set_algorithm_newton}."
      ),
      call = call
    )
  }
  stored
}

# A component of the fitted object that a diagnostic needs and an older fit may
# lack. The format epoch deliberately does not move when a component is added
# during a development line (see format_version.R), so within that line an
# object can be current in stamp and still predate a component -- and this is
# the guard that covers it. Same shape as `residual_stored()`: name the
# component, and name the remedy.
fit_component <- function(
  object,
  component,
  what,
  call = rlang::caller_env()
) {
  stored <- object[[component]]
  if (is.null(stored)) {
    cli::cli_abort(
      c(
        "{what} needs the {.field {component}} component, which this fit does
         not carry.",
        "i" = "It was added after this model was fitted; re-estimate to obtain
               it."
      ),
      call = call
    )
  }
  stored
}

# Schoenfeld rows are the score rows without the exposure term. On a
# multinomial sub-model there is no exposure term to remove -- the scale is 1 --
# so the score rows ARE the Schoenfeld rows. On an exact-time sub-model they
# are not: the score carries `Dt * total_rate` on the weighted mean, and
# removing it needs the observed alternative's own statistic row, which no
# stored primitive carries.
schoenfeld_rows <- function(object, scores, call = rlang::caller_env()) {
  if (identical(risk_set_normalizer(object$model_spec), "poisson")) {
    cli::cli_abort(
      c(
        "Schoenfeld residuals of an exact-time sub-model are not available
         from stored primitives.",
        "i" = "Their rows drop the exposure term the stored score rows carry,
               which needs the observed alternative's statistic row.",
        "i" = "Use {.code type = \"score\"} for the score rows this fit
               stores."
      ),
      call = call
    )
  }
  scores
}

# One-step influence: dfbeta = I^-1 s_k per event, dfbetas the same scaled by
# the standard errors, cooks the quadratic form s_k' I^-1 s_k. Fixed
# coefficients are excluded from the inverse and carry zero influence -- they
# do not move when an interval is deleted -- which is also what keeps the
# information matrix invertible on a fit with offsets.
influence_rows <- function(object, scores, type) {
  is_fixed <- GetFixed(object)
  information <- object$final_information_matrix[!is_fixed, !is_fixed]
  inverse <- tryCatch(solve(information), error = function(e) {
    cli::cli_abort(c(
      "The information matrix of this fit cannot be inverted.",
      "x" = "Influence measures need it, and collinear effects make it
             singular.",
      "i" = "Check the model for redundant effects."
    ))
  })
  free_scores <- scores[, !is_fixed, drop = FALSE]
  if (identical(type, "cooks")) {
    return(rowSums((free_scores %*% inverse) * free_scores))
  }
  influence <- matrix(
    0,
    nrow = nrow(scores),
    ncol = ncol(scores),
    dimnames = dimnames(scores)
  )
  influence[, !is_fixed] <- free_scores %*% inverse
  if (identical(type, "dfbetas")) {
    standard_errors <- object$standard_errors
    influence[, !is_fixed] <- sweep(
      influence[, !is_fixed, drop = FALSE],
      2,
      standard_errors[!is_fixed],
      "/"
    )
  }
  influence
}

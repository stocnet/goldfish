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
#' [survival::coxph()] type vocabulary. Most types are read straight off the
#' primitives estimation stored — `deviance`, `score`, `cox_snell`, the
#' influence measures, and `schoenfeld` and `martingale` where the fit carries
#' what they need — so they cost nothing beyond the arithmetic. The rest run
#' **one** evaluation pass over the statistics: `response`, `martingale` at
#' the dyad level, and `schoenfeld` on an exact-time fit that did not store the
#' conditional score rows. A type that can be neither read nor recomputed says
#' which primitive to store and how to supply the statistics, rather than
#' returning a near-enough number under the name that was asked for.
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
#'     free-parameter columns sum to zero at the maximum. On the exact-time
#'     sub-models they are a *different* object: the score row carries an
#'     exposure term on its weighted mean, and these rows drop it, which is why
#'     they do **not** sum to zero at the maximum even for free parameters.
#'     They come from the `"conditional_scores"` primitive if the fit stored
#'     it, and otherwise from one evaluation pass over the statistics; the row
#'     of a right-censored interval is `NA`, there being no realized
#'     alternative to compare against.}
#'   \item{`scaled_schoenfeld`}{the Grambsch-Therneau scaling
#'     `coef(object) + n * I^-1 s_k` of those rows, on the coefficient scale:
#'     each row reads as the estimate a single interval "votes" for, so a trend
#'     against time is evidence of a time-varying effect. `n` is the
#'     **dependent-event** count of the sub-model being diagnosed, never shared
#'     with another sub-model.}
#'   \item{`cox_snell`}{each interval's compensator, elapsed time times the
#'     total fitted rate. Under a correct exact-time model these are unit
#'     exponential, so their Q-Q plot is a goodness-of-fit check. Exact-time
#'     sub-models only; requesting them elsewhere aborts, a multinomial
#'     likelihood having no compensator.}
#'   \item{`response`}{observed indicator minus fitted probability, per
#'     alternative and per event: one vector (or dyad grid) per interval over
#'     the whole node set, zero off the risk set. Always recomputed — the
#'     per-event probabilities are the one primitive whose storage the size
#'     guardrail warns about.}
#'   \item{`martingale`}{per-actor observed minus expected counts, the
#'     difference the stored `margins` are the two halves of, with both sides
#'     reported on a tie-oriented (REM) fit. At `level = "dyad"` the per-dyad
#'     map those margins are the row and column sums of, which is never stored
#'     and always costs one pass.}
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
#' @param object a fitted model of class `"goldfishFit"`.
#' @param type the residual type, one of `"deviance"` (default), `"score"`,
#'   `"schoenfeld"`, `"scaled_schoenfeld"`, `"cox_snell"`, `"response"`,
#'   `"martingale"`, `"dfbeta"`, `"dfbetas"` and `"cooks"`.
#' @param level which stratification to return, for the two types that have
#'   more than one. Defaults per type rather than globally, since they disagree
#'   about what an unstratified answer is.
#'
#'   For `"martingale"`: `"actor"` (the default, the margins' own difference) or
#'   `"dyad"`. The dyad level is never stored and always costs one evaluation
#'   pass.
#'
#'   For `"cox_snell"`: `"event"` (the default, one compensator per dependent
#'   event) or `"actor"`, each actor's compensators over **its own** consecutive
#'   events. The actor level answers whether one actor's spacing is what the
#'   model expected, where the event level answers that of the sequence as a
#'   whole, so it reads shape where [margin_table()] reads level. It returns a
#'   list per actor — for a tie-oriented fit, a `sender` and a `receiver`
#'   list, as `"martingale"` does — each carrying a `right_censored` attribute
#'   marking the final span, which runs to the end of the observation window
#'   and closes no event. Summing an actor's spans returns its expected margin,
#'   and the uncensored spans number its observed one.
#'
#'   Supplying `level` for any other type is an error: they have one reading.
#' @param preprocessed a `goldfishStat` object to recompute from, as
#'   returned by [compute_statistics()]. Only the recomputing types read it, and
#'   only when the fit did not store what they need; it defaults to the object
#'   attached by `estimate_*(return_preprocessed = TRUE)`.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return For `"deviance"`, `"cooks"` and `"cox_snell"`, a numeric vector with
#'   one value per interval. For `"score"`, `"schoenfeld"`,
#'   `"scaled_schoenfeld"`, `"dfbeta"` and `"dfbetas"`, a numeric matrix with
#'   one row per interval and one column per coefficient, carrying the column
#'   names of the stored per-event score matrix (the effect names). Right-
#'   censored intervals are included: they contribute a likelihood term and a
#'   score — but they realize no alternative, so the Schoenfeld rows (and the
#'   scaled ones) are `NA` there. For `"response"`, a list with one per-event
#'   vector or grid over the node set. For `"martingale"`, per-actor
#'   differences shaped like the fit's `margins`, or — at `level = "dyad"` —
#'   one observed-minus-expected value per dyad.
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
#' @inheritSection diagnostic-requirements What a diagnostic needs
#'
#' @section What these need:
#'
#' It depends on `type`, and the range is the whole family's. `"cox_snell"`
#' needs the interval clock and the `"loglik"` primitive, and **no pass**.
#' `"schoenfeld"`, `"scaled_schoenfeld"` and `"score"` read the `"scores"`
#' primitive when the fit carries it and otherwise take **one pass**.
#' `"deviance"`, `"response"`, `"martingale"`, `"dfbeta"` and `"dfbetas"` need
#' the model's **statistics** and **one pass**.
#'
#' @seealso [estimate_dynam()] for the primitives these read,
#'   [set_algorithm_newton()] for requesting them, and [margin_table()] for
#'   the per-actor calibration counterpart.
#' @method residuals goldfishFit
#' @export
residuals.goldfishFit <- function(
  object,
  type = c(
    "deviance",
    "score",
    "schoenfeld",
    "scaled_schoenfeld",
    "cox_snell",
    "response",
    "martingale",
    "dfbeta",
    "dfbetas",
    "cooks"
  ),
  level = NULL,
  preprocessed = NULL,
  ...
) {
  abort_if_stale_result(object, "residuals")
  type <- match.arg(type)
  level <- resolve_residual_level(level, type)
  switch(
    type,
    deviance = accumulate_over_events(
      -2 * residual_stored(object, "interval_log_lik", "loglik", type),
      object
    ),
    cox_snell = if (identical(level, "actor")) {
      actor_cox_snell_residuals(object, preprocessed)
    } else {
      accumulate_over_events(cox_snell_residuals(object), object)
    },
    schoenfeld = event_aligned_rows(
      schoenfeld_rows(object, preprocessed),
      object
    ),
    scaled_schoenfeld = event_aligned_rows(
      scaled_schoenfeld_rows(object, preprocessed),
      object
    ),
    response = response_residuals(object, preprocessed),
    martingale = martingale_residuals(object, level, preprocessed),
    score = accumulate_over_events(
      residual_stored(object, "event_scores", "scores", type),
      object
    ),
    # dfbeta and dfbetas are linear in the score row, so accumulating first
    # commutes with the transform. cooks is a quadratic form and does not: it is
    # the influence of the whole event, so it is evaluated ON the accumulated
    # row rather than summed over the intervals inside it.
    influence_rows(
      object,
      accumulate_over_events(
        residual_stored(object, "event_scores", "scores", type),
        object
      ),
      type
    )
  )
}

# Realign a per-interval matrix whose censored rows are NA onto the per-event
# axis every other residual type now uses.
#
# The Schoenfeld forms are not accumulated -- they are already one meaningful
# row per event, with `NA` where a right-censored interval realized no
# alternative to compare against. Dropping those rows is what makes them per
# event; a censored remainder, closing no waiting time, is then padded back as
# the final NA row so the series still lines up with the accumulating types and
# with `augment()`.
event_aligned_rows <- function(rows, object) {
  censored <- object$right_censored_events
  if (is.null(censored) || !any(censored) || is.null(rows)) {
    return(rows)
  }
  kept <- rows[!censored, , drop = FALSE]
  n_returned <- length(accumulate_over_events(object$interval_log_lik, object))
  if (n_returned > nrow(kept)) {
    kept <- rbind(kept, rows[NA_integer_, , drop = FALSE])
  }
  rownames(kept) <- NULL
  kept
}

# Which dependent event's waiting time each interval belongs to.
#
# A Cox-Snell residual is the compensator over the span from one event to the
# next, so an interval closes the waiting time of the event that FOLLOWS it and
# accumulates there. That direction is forced by what the quantity means, not
# chosen: grouping an interval with the preceding event would make its residual
# the exposure *after* that event.
#
# Intervals following the last dependent event close no waiting time. They form
# a censored remainder, reported as a final observation rather than folded into
# the last event -- which would inflate it with exposure that came after it --
# or dropped, which would lose it from totals the compensator identity depends
# on. Windowed effects never produce such a tail, their dissolve pseudo-events
# being bounded by the observation window; only exogenous streams and an
# `end_time` past the last event do.
accumulation_index <- function(object) {
  censored <- object$right_censored_events
  if (is.null(censored)) {
    return(NULL)
  }
  dependent <- !censored
  cumsum(c(0L, dependent[-length(dependent)])) + 1L
}

# Sum a per-interval vector or matrix over each event's span. On a family with
# no censored intervals every group holds exactly one interval, so this is the
# identity and those fits are untouched.
accumulate_over_events <- function(x, object) {
  index <- accumulation_index(object)
  if (is.null(index)) {
    return(x)
  }
  if (is.matrix(x)) {
    accumulated <- rowsum(x, index, reorder = TRUE)
    rownames(accumulated) <- NULL
    return(flag_censored_tail(accumulated, object))
  }
  flag_censored_tail(
    as.numeric(rowsum(as.numeric(x), index, reorder = TRUE)),
    object
  )
}

# Mark the censored remainder, and only where there is one. A caller reading the
# series as one value per event has to be able to tell that a final entry closed
# no waiting time; without the flag it reads as an ordinary, and unusually large,
# last observation.
#
# The attribute is absent rather than all-FALSE on the fits that have no tail --
# which is every multinomial fit and every windowed-only one. A flag that is
# always present but almost never informative is noise: it would break every
# comparison against the stored components while saying nothing. Absent means
# "every value closes a waiting time", and `any(attr(x, "right_censored"))`
# reads FALSE on NULL, so the idiom is the same either way.
flag_censored_tail <- function(accumulated, object) {
  n <- if (is.matrix(accumulated)) nrow(accumulated) else length(accumulated)
  if (n <= object$n_events) {
    return(accumulated)
  }
  censored <- logical(n)
  censored[n] <- TRUE
  attr(accumulated, "right_censored") <- censored
  accumulated
}

# The stored primitive a type reads, or the error that names how to store it.
# Stated as "this type needs that primitive" rather than "something is
# missing", so the fix is in the message.
residual_stored <- function(
  object,
  component,
  primitive,
  type,
  noun = "Residuals",
  call = rlang::caller_env()
) {
  stored <- object[[component]]
  if (is.null(stored)) {
    cli::cli_abort(
      c(
        "{noun} of type {.val {type}} need the {.val {primitive}}
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

# Is this an exact-time (Poisson) sub-model? The one predicate the type-level
# branches read, so "which families have a compensator" is stated once.
is_exact_time_fit <- function(object) {
  identical(behavior_likelihood(object$model_spec), "poisson")
}

# Schoenfeld rows are the score rows without the exposure term. On a
# multinomial sub-model there is no exposure term to remove -- the scale is 1 --
# so the score rows ARE the Schoenfeld rows, and this type coincides with
# `"score"`. On an exact-time sub-model they are a different object: the score
# carries `Dt * total_rate` on the weighted mean, and removing it would need the
# observed alternative's own statistic row. That is one vector equation in two
# unknown vectors, so these rows are NOT derivable from a stored score row --
# they are the `"conditional_scores"` primitive, read here if the fit stored it
# and recomputed through one evaluation pass if it did not.
schoenfeld_rows <- function(object, preprocessed, call = rlang::caller_env()) {
  if (!is_exact_time_fit(object)) {
    return(residual_stored(object, "event_scores", "scores", "schoenfeld"))
  }
  stored <- object$conditional_scores
  if (!is.null(stored)) {
    return(stored)
  }
  prep <- tryCatch(
    resolve_preprocessed(preprocessed, object, call = call),
    error = function(e) NULL
  )
  if (is.null(prep)) {
    # Neither route available, so the error names BOTH: this is the first
    # residual type that spans the stored and the recomputed tier, and a
    # message naming only one of them sends half the readers the wrong way.
    cli::cli_abort(
      c(
        "Schoenfeld residuals of an exact-time sub-model need the
         {.val conditional_scores} primitive, which this fit did not store,
         or the statistics to recompute it from, which it does not carry
         either.",
        "i" = "Re-estimate with {.arg diagnostics} including
               {.val conditional_scores} in {.fn set_algorithm_newton}, or",
        "i" = "re-estimate with {.code return_preprocessed = TRUE}, or pass
               {.code preprocessed = compute_statistics(..., output =
               \"preprocessed\")}."
      ),
      call = call
    )
  }
  evaluate_model(
    object,
    return = "conditional_scores",
    preprocessed = prep
  )$conditional_scores
}

# Grambsch-Therneau scaling: `theta_hat + Vbar^-1 s_k` with `Vbar = I / n` the
# AVERAGE per-event observed information, equivalently `theta_hat + n I^-1 s_k`.
# `n` is the event count of the sub-model being diagnosed -- the dependent
# events, not the intervals, since a right-censored interval realizes no
# alternative and contributes no Schoenfeld row. The constant is never shared
# across sub-models: a DyNAM rate and its choice counterpart have different n.
#
# A coefficient held fixed does not move, so its column is the estimate itself
# and it is excluded from the inverse -- the same rule the influence measures
# apply, and what keeps the information invertible on a fit with offsets.
scaled_schoenfeld_rows <- function(
  object,
  preprocessed,
  call = rlang::caller_env()
) {
  rows <- schoenfeld_rows(object, preprocessed, call = call)
  is_fixed <- GetFixed(object)
  n_events <- object$n_events
  inverse <- invert_free_information(object)
  estimate <- stats::coef(object, complete = TRUE)
  scaled <- matrix(
    estimate,
    nrow = nrow(rows),
    ncol = length(estimate),
    byrow = TRUE,
    dimnames = dimnames(rows)
  )
  scaled[, !is_fixed] <- scaled[, !is_fixed] +
    n_events * (rows[, !is_fixed, drop = FALSE] %*% inverse)
  scaled
}

# The compensator of each interval: elapsed time times the total fitted rate.
# Under the model these are unit-exponential, which is what makes their Q-Q
# plot a goodness-of-fit check. Both factors are stored -- the interval clock
# rides on every fit and `total_rate` on the `"loglik"` primitive -- so at the
# fitted estimate this costs no pass; only an evaluation at another parameter
# vector goes through `evaluate_model()`.
cox_snell_residuals <- function(object, call = rlang::caller_env()) {
  if (!is_exact_time_fit(object)) {
    # Every family without a compensator gets refused here, but they are not
    # the same family and the reason has to name the one being asked. A
    # coordination likelihood is a softmax over unordered dyads, not over a
    # sender's alternatives, so calling it multinomial is simply wrong.
    likelihood <- behavior_likelihood(object$model_spec)
    sub_model <- object$model_spec$sub_model
    models <- switch(
      likelihood,
      coordination = "which unordered dyad formed",
      multinomial = "which alternative was realized",
      "which outcome was realized"
    )
    cli::cli_abort(
      c(
        "Cox-Snell residuals are defined for the exact-time sub-models only.",
        "x" = "{.val {sub_model}} has a {likelihood} likelihood, which carries
               no compensator: it models {models}, not when.",
        "i" = "Use {.code type = \"deviance\"} for a per-interval
               goodness-of-fit measure on this sub-model."
      ),
      call = call
    )
  }
  total_rate <- residual_stored(object, "total_rate", "loglik", "cox_snell")
  if (!is.null(object$intervals)) {
    return(object$intervals * total_rate)
  }
  # A fit predating the interval clock: recover the elapsed time from the
  # stored components rather than demanding a replay object. On a censored
  # interval the whole log-likelihood contribution IS the compensator; on a
  # dependent one the observed alternative's term has to be added back, which
  # `conditional_logl` supplies as `x_obs - log T`.
  interval_log_lik <- residual_stored(
    object,
    "interval_log_lik",
    "loglik",
    "cox_snell"
  )
  conditional <- fit_component(
    object,
    "conditional_logl",
    "Cox-Snell residuals of a fit made before the interval clock"
  )
  compensator <- -interval_log_lik
  dependent <- !object$right_censored_events
  compensator[dependent] <- conditional[dependent] +
    log(total_rate[dependent]) -
    interval_log_lik[dependent]
  compensator
}

# Observed indicator minus fitted probability, per alternative and per event.
# Always a recompute: the per-event probability vectors are the one primitive
# whose storage the guardrail warns about, so this type reads them from a pass
# rather than expecting them on the fit. On an exact-time sub-model the
# probabilities are the conditional (next-event) ones, `lambda / sum lambda`,
# which is what makes the residual comparable with a multinomial fit's.
response_residuals <- function(
  object,
  preprocessed,
  call = rlang::caller_env()
) {
  prep <- resolve_preprocessed(preprocessed, object, call = call)
  probabilities <- evaluate_model(
    object,
    return = "probabilities",
    preprocessed = prep
  )$probabilities
  axis <- risk_set_axis(object)
  dependent <- !object$right_censored_events
  lapply(seq_along(probabilities), function(i) {
    fitted <- probabilities[[i]]
    if (!dependent[i]) {
      # A right-censored interval realizes no alternative, so the observed
      # indicator is zero everywhere and the residual is minus the fitted mass.
      return(-fitted)
    }
    sender <- prep$event_sender[i]
    receiver <- prep$event_receiver[i]
    if (identical(axis, "sender")) {
      fitted[sender] <- fitted[sender] - 1
    } else if (identical(axis, "receiver_given_sender")) {
      fitted[receiver] <- fitted[receiver] - 1
    } else {
      fitted[sender, receiver] <- fitted[sender, receiver] - 1
    }
    -fitted
  })
}

# Per-actor observed minus expected: the margins' own difference, which is why
# it is not recomputed when the fit stored them. On an exact-time fit `expected`
# is the compensator, so the difference is the aggregated counting-process
# martingale residual; on a multinomial one it is a calibration difference on
# the probability scale, and the margins' `scale` attribute says which.
#
# `level = "dyad"` is never a stored primitive -- the margins ARE its row and
# column sums -- so it always costs one pass.
martingale_residuals <- function(
  object,
  level,
  preprocessed,
  call = rlang::caller_env()
) {
  if (identical(level, "dyad")) {
    return(dyad_martingale_map(object, preprocessed, call = call))
  }
  margins <- residual_stored(object, "margins", "margins", "martingale")
  sides <- sub("^observed", "", grep("^observed", names(margins), value = TRUE))
  out <- lapply(sides, function(side) {
    difference <- margins[[paste0("observed", side)]] -
      margins[[paste0("expected", side)]]
    # The expected vector's `scale` marker would ride along through the
    # subtraction, and a difference is on neither scale: it is a residual, not
    # a fitted mass. Dropping it keeps the marker meaning what it says.
    attr(difference, "scale") <- NULL
    difference
  })
  if (length(out) == 1L) {
    return(out[[1]])
  }
  stats::setNames(out, sub("^_", "", sides))
}

# The dyad-level map the margins are the marginals of: expected mass summed per
# dyad over the sequence, subtracted from the observed dyad counts.
#
# The dyad families carry a grid per event already. A choice sub-model does not
# -- its alternatives are receivers given the observed sender -- but a dyad map
# is still what it means: that event's probability vector belongs to the
# observed sender's ROW, which is exactly what makes the map readable as
# "which sender-receiver pairs is the model over-predicting". A rate sub-model
# has no such reading: its alternatives ARE actors, and there is no second axis
# to scatter onto, so it says so rather than returning the actor-level answer
# under the dyad name.
dyad_martingale_map <- function(
  object,
  preprocessed,
  call = rlang::caller_env()
) {
  axis <- risk_set_axis(object)
  if (identical(axis, "sender")) {
    cli::cli_abort(
      c(
        "{.code level = \"dyad\"} is not defined for a sender-axis sub-model.",
        "x" = "Its alternatives are actors, not dyads: there is no second axis
               for a per-dyad map to range over.",
        "i" = "Use {.code level = \"actor\"}, which is the per-alternative
               answer for this sub-model."
      ),
      call = call
    )
  }
  prep <- resolve_preprocessed(preprocessed, object, call = call)
  probabilities <- evaluate_model(
    object,
    return = "probabilities",
    preprocessed = prep
  )$probabilities
  dependent <- which(!object$right_censored_events)
  n_actors_1 <- nrow(prep$initial_stats)
  n_actors_2 <- ncol(prep$initial_stats)
  # The probability scale, over dependent events only, for the reason the
  # probability-scale margins use it: the map is a calibration comparison, so
  # it must total the same set the observed counts do -- events, not intervals.
  expected <- matrix(0, n_actors_1, n_actors_2)
  observed <- matrix(0, n_actors_1, n_actors_2)
  for (i in dependent) {
    sender <- prep$event_sender[i]
    fitted <- probabilities[[i]]
    if (is.matrix(fitted)) {
      expected <- expected + fitted
    } else {
      expected[sender, ] <- expected[sender, ] + fitted
    }
    observed[sender, prep$event_receiver[i]] <-
      observed[sender, prep$event_receiver[i]] + 1
  }
  labels <- prep$node_lookup
  if (!is.null(labels)) {
    dimnames(observed) <- list(
      labels$label[labels$side == 1L],
      labels$label[labels$side == max(labels$side)]
    )
  }
  dimnames(expected) <- dimnames(observed)
  observed - expected
}

# The inverse of the information over the ESTIMATED coefficients. Fixed ones
# are excluded rather than zeroed: they do not move, and including them is what
# makes the matrix singular on a fit with offsets. Shared by the influence
# measures and by the Grambsch-Therneau scaling, so both fail the same way and
# with the same diagnosis.
invert_free_information <- function(object, call = rlang::caller_env()) {
  is_fixed <- GetFixed(object)
  information <- object$final_information_matrix[!is_fixed, !is_fixed]
  tryCatch(
    solve(information),
    error = function(e) {
      cli::cli_abort(
        c(
          "The information matrix of this fit cannot be inverted.",
          "x" = "Influence measures and scaled residuals need it, and collinear
                 effects make it singular.",
          "i" = "Check the model for redundant effects."
        ),
        call = call
      )
    }
  )
}

# One-step influence: dfbeta = I^-1 s_k per event, dfbetas the same scaled by
# the standard errors, cooks the quadratic form s_k' I^-1 s_k. Fixed
# coefficients are excluded from the inverse and carry zero influence -- they
# do not move when an interval is deleted -- which is also what keeps the
# information matrix invertible on a fit with offsets.
influence_rows <- function(object, scores, type) {
  is_fixed <- GetFixed(object)
  inverse <- invert_free_information(object)
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

#' @export
#' @method residuals goldfishFlavFit
#' @noRd
residuals.goldfishFlavFit <- function(object, ..., flavor = NULL) {
  # A residual series is a vector, a matrix or a list of them, so nothing in the
  # return can say which process it came from -- hence a list keyed by process
  # label rather than the row-bound table `augment()` gives.
  flavored_component_apply(
    object,
    flavor,
    function(fit) stats::residuals(fit, ...),
    "residuals"
  )
}

# Which stratification a residual type is being asked for.
#
# `level` cannot take one default across types, because the types disagree about
# what an unstratified answer is: a martingale residual is a per-actor map and
# has been since it existed, while a Cox-Snell residual is a per-event series
# and stratifying it is the new reading. Resolving per type keeps both defaults
# where they were rather than moving one to give the other a shorter call.
resolve_residual_level <- function(level, type, call = rlang::caller_env()) {
  defaults <- c(martingale = "actor", cox_snell = "event")
  allowed <- list(
    martingale = c("actor", "dyad"),
    cox_snell = c("event", "actor")
  )
  if (is.null(level)) {
    return(if (type %in% names(defaults)) unname(defaults[[type]]) else "event")
  }
  level <- rlang::arg_match0(
    level,
    c("event", "actor", "dyad"),
    arg_nm = "level"
  )
  if (!type %in% names(allowed)) {
    cli::cli_abort(
      c(
        "{.arg level} does not apply to {.code type = {.val {type}}}.",
        "x" = "That type has one reading, and it is per event.",
        "i" = "Only {.val cox_snell} and {.val martingale} stratify."
      ),
      call = call
    )
  }
  if (!level %in% allowed[[type]]) {
    cli::cli_abort(
      c(
        "{.code type = {.val {type}}} has no {.val {level}} level.",
        "i" = "Available: {.val {allowed[[type]]}}."
      ),
      call = call
    )
  }
  level
}

# Each actor's compensators over ITS OWN consecutive events.
#
# The unstratified series answers "was the waiting time to the next event, by
# anyone, what the model expected"; this answers the same question of one
# actor's own spacing, which is what makes it a shape diagnostic rather than a
# second level reading. An actor's rate is the total rate times that actor's
# share of it, so the per-interval contribution is the same compensator the
# unstratified type accumulates, split across the risk set -- and summing an
# actor's over the whole sequence returns its stored expected margin exactly.
#
# The trailing span carries no event of that actor and is marked censored, as
# the unstratified series marks its own remainder: an actor that stopped early
# has been waiting since, and dropping that exposure would lose it from the
# totals.
actor_cox_snell_residuals <- function(
  object,
  preprocessed,
  call = rlang::caller_env()
) {
  cox_snell_residuals(object, call = call)
  prep <- resolve_preprocessed(preprocessed, object, call = call)
  compensator <- object$intervals * object$total_rate
  probabilities <- evaluate_model(
    object,
    return = "probabilities",
    preprocessed = prep
  )$probabilities
  dependent <- !object$right_censored_events
  labels <- prep$node_lookup

  if (identical(risk_set_axis(object), "dyad")) {
    # A tie-oriented fit gives each interval a dyad matrix, and an actor's own
    # rate is that matrix marginalized over the side it is not on. Both sides
    # are reported, under the names the margins already use.
    return(list(
      sender = stratified_compensators(
        compensator,
        t(vapply(probabilities, rowSums, numeric(nrow(probabilities[[1]])))),
        prep$event_sender,
        dependent,
        side_labels(labels, 1L)
      ),
      receiver = stratified_compensators(
        compensator,
        t(vapply(probabilities, colSums, numeric(ncol(probabilities[[1]])))),
        prep$event_receiver,
        dependent,
        side_labels(labels, max(labels$side %||% 1L))
      )
    ))
  }
  axis <- risk_set_axis(object)
  actor <- if (identical(axis, "receiver_given_sender")) {
    prep$event_receiver
  } else {
    prep$event_sender
  }
  side <- if (identical(axis, "receiver_given_sender")) {
    max(labels$side %||% 1L)
  } else {
    1L
  }
  stratified_compensators(
    compensator,
    do.call(rbind, probabilities),
    actor,
    dependent,
    side_labels(labels, side)
  )
}

side_labels <- function(node_lookup, side) {
  if (is.null(node_lookup)) {
    return(NULL)
  }
  node_lookup$label[node_lookup$side == side]
}

# One actor's spans, for every actor. `rates` is intervals-by-actors, holding
# each actor's share of the risk set at each interval.
stratified_compensators <- function(
  compensator,
  rates,
  actor,
  dependent,
  labels
) {
  out <- lapply(seq_len(ncol(rates)), function(a) {
    contribution <- compensator * rates[, a]
    closes <- dependent & !is.na(actor) & actor == a
    # The same grouping the unstratified accumulation uses, on this actor's own
    # events: an interval closes the waiting time of the event that FOLLOWS it,
    # so the group index advances AFTER an event rather than at it.
    group <- cumsum(c(0L, closes[-length(closes)])) + 1L
    spans <- as.numeric(rowsum(contribution, group, reorder = TRUE))
    n_events <- sum(closes)
    # Always attached here, unlike the unstratified series where the
    # attribute's presence IS the signal that a remainder exists. There is one
    # series; here there are as many as there are actors, and a caller reading
    # all of them should not have to branch on whether this particular actor
    # happened to send the final event.
    censored <- logical(length(spans))
    if (length(spans) > n_events) {
      censored[length(spans)] <- TRUE
    }
    attr(spans, "right_censored") <- censored
    spans
  })
  if (!is.null(labels) && length(labels) == length(out)) {
    names(out) <- labels
  }
  out
}

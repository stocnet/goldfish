# The goldfishDynesFit result class returned by estimate_dynes().
#
# The class carries the fit parent `goldfishBaseFit`, so a generic added to the
# parent later reaches it, but no parent method it has today is correct for it:
# each reads the single-process fields (`names`, `n_params`, `log_likelihood`,
# `final_information_matrix`, `standard_errors`) that a fit over a weighted
# pool of augmented sequences does not have. Every generic dispatching on fit
# classes is therefore either overridden here or refused with a reason; the
# verdicts are recorded in the fit-class-hierarchy capability.

# Result object ----

# The fit from the loop's result `res` (parameters already named by the
# composite labels). One dedicated full-pool evaluation at the returned
# estimate supplies the per-sequence scores and information matrices both
# covariances need, on every returning run -- converged or not -- so a fit
# never carries a NULL covariance. SEs are never harvested from the last
# minibatch.
new_dynes_fit <- function(res, authored_spec, control, fit_call, call, start) {
  # Read before the store closes below. A loop stopped on a pool with no
  # finite likelihood already has its own warning, and its pooled information
  # is empty.
  pool_dropped <- isTRUE(res$pool$store$tally$pool_dropped)
  if (identical(control$weights$use, "resampling")) {
    cli::cli_warn(
      c(
        "!" = "The standard errors use the importance weights over the whole
               pool, not the resampled pool.",
        "i" = "Resampling is an E-step device and is not part of the
               estimand."
      ),
      class = "goldfish_dynes_se_importance",
      call = call
    )
  }
  labels <- names(res$parameters)
  covariances <- dynes_covariances(
    res$pool,
    unname(res$parameters),
    res$weights
  )
  dims <- list(labels, labels)
  dimnames(covariances$covariance) <- dims
  dimnames(covariances$mc_covariance) <- dims
  if (!is.null(covariances$information)) {
    covariances$information <- lapply(
      covariances$information,
      function(part) `dimnames<-`(part, dims)
    )
  }
  # The first cause that applies, each excluding the next: a dropped pool
  # leaves I_c empty, and a singular I_c has no observed information to test.
  covariance_status <- if (pool_dropped) {
    "pool_dropped"
  } else if (covariances$singular) {
    "singular"
  } else if (covariances$not_pd) {
    "not_pd"
  } else {
    "ok"
  }
  if (covariance_status == "singular") {
    warn_singular_information(
      singular_attribution(covariances$information$complete),
      labels,
      start = start,
      value = unname(res$parameters),
      call = call
    )
  } else if (covariance_status == "not_pd") {
    warn_observed_information_not_pd(res$converged, call = call)
  }

  # The store is closed once estimation returns; the fit keeps the sequences.
  res$pool$store <- NULL
  res$covariance <- covariances$covariance
  res$mc_covariance <- covariances$mc_covariance
  res$covariance_status <- covariance_status
  res$newton_se <- if (covariance_status == "ok") {
    dynes_newton_se(covariances$covariance, covariances$mean_score)
  } else {
    NA_real_
  }
  res$at_optimum <- res$newton_se < dynes_optimum_threshold
  # Kept on a singular or not-PD fit too, where only the inverses failed and
  # the matrices are what a user inspects; a dropped pool evaluated nothing.
  res$information <- if (!pool_dropped) covariances$information
  res$fraction_missing <- if (pool_dropped) {
    NA_real_
  } else {
    covariances$fraction_missing
  }
  # Under uniform weighting 1 / sum(w^2) is the number of kept sequences
  # whatever the chain's mixing, so it is not reported; the chain's own
  # diagnostics in the em_trace carry that failure mode.
  res$covariance_ess <- if (
    pool_dropped || identical(control$weights$weighting, "uniform")
  ) {
    NA_real_
  } else {
    effective_sample_size(res$weights)
  }
  res$authored_spec <- authored_spec
  res$call <- fit_call
  class(res) <- c("goldfishDynesFit", "goldfishBaseFit")
  res
}

# The fit's optimum gate: the largest coordinate of the exact Newton step from
# the estimate to the optimum of the final pool's importance-weighted
# observed-data log-likelihood, max_j |(I_obs^-1 S)_j| / SE_j. Its Hessian
# there is -I_obs (see louis_covariances()), so the step is the covariance
# times the weighted mean score S. Under resampling it still reads the
# importance-weighted pool, which is the estimand, not the multiset the M-step
# optimized.
dynes_newton_se <- function(covariance, mean_score) {
  max(abs(drop(covariance %*% mean_score)) / sqrt(diag(covariance)))
}

# Below this many standard errors the estimate is at the final pool's optimum.
# Fixed, and independent of `sgd_tolerance`: the M-step's per-pass test is an
# approximation of this one (its information is held at the M-step's start),
# so it is no measure of where the returned estimate sits.
dynes_optimum_threshold <- 0.1

# The two covariance estimators at `theta` over the whole pool, with the
# run's own importance weights (see louis_covariances()).
dynes_covariances <- function(pool, theta, weights) {
  res <- evaluate_sequence_pool(pool, theta, what = c("score", "fisher"))
  louis_covariances(res$score, res$fisher, res$degenerate, weights)
}

# The observed-data information by Louis's (1982) identity and the two
# covariances it gives, from per-sequence complete-data scores s_i (K x p),
# information matrices J_i (a list of K) and degeneracy flags already
# evaluated at one theta, with normalized weights w_i and S = sum_i w_i s_i:
#   complete        I_c   = sum_i w_i J_i
#   observed        I_obs = I_c - (sum_i w_i s_i s_i' - S S')
#   standard error  I_obs^-1
#   Monte Carlo     I_obs^-1 (sum_i w_i^2 (s_i - S)(s_i - S)') I_obs^-1
# The pooled paths are imputations of the same data, so the spread of their
# scores is uncertainty about the latent path: the missing information, which
# the observed information subtracts from the complete one. It is not sampling
# variability. As the middle of a sandwich between two I_c^-1 it would measure
# the Monte-Carlo variability within an iteration, and with nothing missing it
# would collapse to S S', bounded by the M-step's gradient tolerance. The
# identity needs J_i to be the negative Hessian of the path's log-likelihood,
# not only its expectation, which holds for a log-linear rate and a
# canonical-link choice. It is exact for untransformed self-normalized
# weights; the derivative of a clipped or smoothed weight differs, and the
# estimate is then an approximation.
#
# The Monte-Carlo middle term is the self-normalized importance-sampling
# variance of the weighted score; aggregating the scores before the outer
# product (S S') would give a rank-1 matrix bounded by the M-step's gradient
# tolerance, not a variance. The bread is the delta method's: the estimate
# solves sum_i w_i(theta) s_i(theta) = 0, whose derivative in theta is -I_obs
# once the weights' dependence on theta is differentiated too. Uniform weights
# do not depend on theta, and that derivative alone would give I_c, but the
# bread is I_obs^-1 there as well: the estimate is a fixed point of the
# Monte-Carlo EM map M, one M-step carries the E-step's score error through
# I_c^-1, and at the fixed point that error is amplified by (I - dM)^-1, with
# the EM rate matrix dM = I_c^-1 (I_c - I_obs), so the product is I_obs^-1
# (Dempster, Laird and Rubin 1977, section 3; Booth and Hobert 1999).
#
# A degenerate or zero-weight sequence contributes nothing. A singular I_c
# yields all-NA matrices flagged `singular`, and an I_obs that is not positive
# definite all-NA ones flagged `not_pd`, rather than an error or negative
# variances: with few effective draws the estimated missing information can
# reach the complete information in some direction. Positive definiteness is a
# Cholesky factorization of the symmetrized I_obs with no tolerance, the same
# exactness rule singular_attribution() applies, so the two cannot disagree
# about a borderline matrix. The three matrices come back as `information`,
# list(complete = I_c, missing = Var_w(s), observed = I_obs), whatever the
# flags, since only their inverses can fail; it is NULL when no sequence is
# kept. `missing` holds the centered variance, S S' included, so `observed`
# is a plain subtraction.
#
# `fraction_missing` is the largest eigenvalue of I_c^-1 Var_w(s), the share of
# the complete information the paths miss in the worst direction, with
# Var_w(s) = sum_i w_i s_i s_i' - S S'. It is taken from the symmetric
# L^-1 Var_w(s) L^-T, L the Cholesky factor of I_c, which has the same
# eigenvalues and keeps eigen() on a symmetric matrix. It is below one exactly
# when I_obs is positive definite, and NA when I_c has no inverse.
#
# `mean_score` is S itself, returned so that a caller reads the Newton step at
# theta (see dynes_newton_se()) without evaluating the pool again.
louis_covariances <- function(score, fisher, degenerate, weights) {
  p <- ncol(score)
  keep <- weights > 0 & !degenerate
  scores <- score[keep, , drop = FALSE]
  w <- weights[keep]
  complete <- Reduce(`+`, Map(`*`, fisher[keep], w))
  mean_score <- colSums(scores * w)
  missing <- crossprod(scores * sqrt(w)) - tcrossprod(mean_score)
  observed <- if (!is.null(complete)) complete - missing
  information <- if (!is.null(complete)) {
    list(complete = complete, missing = missing, observed = observed)
  }
  centered <- sweep(scores, 2L, mean_score)
  mc_meat <- crossprod(centered * w)
  unavailable <- list(
    covariance = matrix(NA_real_, p, p),
    mc_covariance = matrix(NA_real_, p, p),
    information = information,
    fraction_missing = NA_real_,
    mean_score = mean_score
  )
  if (is.null(tryCatch(solve(complete), error = function(e) NULL))) {
    return(c(unavailable, singular = TRUE, not_pd = FALSE))
  }
  unavailable$fraction_missing <- largest_missing_fraction(complete, missing)
  factor <- tryCatch(
    chol((observed + t(observed)) / 2),
    error = function(e) NULL
  )
  if (is.null(factor)) {
    return(c(unavailable, singular = FALSE, not_pd = TRUE))
  }
  bread <- chol2inv(factor)
  list(
    covariance = bread,
    mc_covariance = bread %*% mc_meat %*% bread,
    information = information,
    fraction_missing = unavailable$fraction_missing,
    mean_score = mean_score,
    singular = FALSE,
    not_pd = FALSE
  )
}

# The largest eigenvalue of I_c^-1 Var_w(s) (see louis_covariances()); NA
# when the Cholesky factor of I_c does not exist.
largest_missing_fraction <- function(information, missing) {
  factor <- tryCatch(chol(information), error = function(e) NULL)
  if (is.null(factor)) {
    return(NA_real_)
  }
  half <- backsolve(factor, missing, transpose = TRUE)
  scaled <- backsolve(factor, t(half), transpose = TRUE)
  max(
    eigen((scaled + t(scaled)) / 2, symmetric = TRUE, only.values = TRUE)$values
  )
}

# One warning for an observed information that is not positive definite at
# the returned estimate while the complete information is invertible. With a
# converged run that is a pool-size symptom. Away from a maximum the observed
# information can be indefinite whatever the pool size, so a run that did not
# converge says so too.
warn_observed_information_not_pd <- function(
  converged,
  call = rlang::caller_env()
) {
  cli::cli_warn(
    c(
      "!" = "The Monte-Carlo estimate of the observed information at the
             returned estimate is not positive definite; the covariances are
             {.code NA}.",
      "x" = "The pool's estimate of the missing information is as large as the
             complete information in some direction.",
      if (!isTRUE(converged)) {
        c(
          "i" = "The run did not converge, so the estimate may not be a
                 maximum, and the observed information need not be positive
                 definite there."
        )
      },
      "i" = "Consider a larger {.arg n_sequences} or a model-driven
             {.arg routine} ({.val sim} or {.val mcmc})."
    ),
    class = "goldfish_dynes_observed_information_not_pd",
    call = call
  )
}

# Which free coefficients a singular pooled information leaves without any
# information: those whose diagonal is not positive (`<=` absorbs a slightly
# negative rounding). The test is exact, not relative to the largest
# diagonal, because the diagonals carry different statistics' units. The
# pooled information is a non-negatively weighted sum of positive
# semi-definite matrices, so a zero diagonal zeroes its row and column and
# the coefficient spans an exact null direction on its own. Collinear groups,
# which need a null-space analysis, are left empty.
singular_attribution <- function(information) {
  list(
    coefficients = unname(which(diag(information) <= 0)),
    groups = list()
  )
}

# One warning for a singular pooled information at the returned estimate,
# from singular_attribution() over the free coefficients. A coefficient with
# no information has a zero score on every path, so it keeps its starting
# value; but the pool changes across passes, so one that moved on an earlier
# pool and lost its information on the final one is reported as such.
warn_singular_information <- function(
  attribution,
  labels,
  start,
  value,
  call = rlang::caller_env()
) {
  named <- attribution$coefficients
  stays <- value[named] == start[named]
  at_start <- labels[named[stays]]
  moved <- labels[named[!stays]]
  moved_value <- signif(value[named[!stays]], 4)
  # Pinned: cli would read a single numeric value as the quantity itself.
  n_moved <- length(moved)
  n_named <- length(named)
  cli::cli_warn(
    c(
      "!" = "The pooled information matrix at the returned estimate is
             singular; the covariances are {.code NA}.",
      if (length(at_start) > 0L) {
        c(
          "x" = "{.field {at_start}} {?has/have} no information in the pool;
                 {?it was/they were} not estimated and {?stays/stay} at
                 {?its/their} starting value{?s}."
        )
      },
      if (n_moved > 0L) {
        c(
          "x" = "{.field {moved}} {?has/have} no information in the final
                 pool; the returned value{?s} {.val {moved_value}}
                 {cli::qty(n_moved)}{?is not an estimate/are not estimates}."
        )
      },
      if (n_named > 0L) {
        c(
          "i" = "{cli::qty(n_named)}Consider removing {?its/their} effect{?s}
                 from the specification."
        )
      } else {
        c("i" = "No single coefficient is without information.")
      }
    ),
    class = "goldfish_dynes_singular_information",
    call = call
  )
}

# The fit's estimates as a goldfishParams over the specification it was
# estimated from: the estimate at each free slot, NA at each fixed slot so the
# specification's own value prevails.
dynes_fit_parameters <- function(fit) {
  spec <- fit$authored_spec
  layout <- coef_layout(spec)
  labels <- paste0(layout$process, ": ", layout$name)
  values <- unname(fit$parameters[labels])
  values[layout$fixed] <- NA_real_
  by_fid <- split(seq_len(nrow(layout)), layout$fid)
  by_fid <- by_fid[as.character(unique(layout$fid))]
  vectors <- lapply(by_fid, function(idx) values[idx])
  names(vectors) <- vapply(by_fid, function(idx) layout$process[idx][1L], "")
  do.call(set_parameters, c(list(spec), vectors))
}

# Overridden generics ----

#' @export
#' @method coef goldfishDynesFit
#' @noRd
coef.goldfishDynesFit <- function(object, ...) {
  object$parameters
}

#' @param object A `goldfishDynesFit` from `estimate_dynes()`.
#' @param information Which information `vcov()` inverts. `"observed"`, the
#'   default, is the covariance the fit reports, the inverse of Louis's
#'   observed-data information. `"complete_data"` is the inverse of the
#'   pooled complete-data information `information$complete`. It ignores the
#'   information the latent paths miss, so its variances are a lower bound on
#'   the observed ones, and it is returned even when the observed information
#'   is not positive definite. Both are `NA` when their matrix has no inverse
#'   or the pool was dropped.
#' @param ... Unused.
#' @rdname estimate_dynes
#' @export
#' @method vcov goldfishDynesFit
vcov.goldfishDynesFit <- function(
  object,
  information = c("observed", "complete_data"),
  ...
) {
  information <- rlang::arg_match(information)
  if (information == "observed") {
    return(object$covariance)
  }
  complete <- object$information$complete
  inverse <- if (!is.null(complete)) {
    tryCatch(solve(complete), error = function(e) NULL)
  }
  if (is.null(inverse)) {
    inverse <- object$covariance
    inverse[] <- NA_real_
  }
  inverse
}

#' @export
#' @method coef_layout goldfishDynesFit
#' @noRd
coef_layout.goldfishDynesFit <- function(x, ...) {
  layout <- coef_layout(x$spec)
  labels <- paste0(layout$process, ": ", layout$name)
  free <- !layout$fixed
  at <- match(labels[free], names(x$parameters))
  layout$value[free] <- unname(x$parameters[at])
  layout$se <- NA_real_
  layout$se[free] <- sqrt(diag(x$covariance))[at]
  layout$mc_se <- NA_real_
  layout$mc_se[free] <- sqrt(diag(x$mc_covariance))[at]
  layout
}

#' @method simulate goldfishDynesFit
#' @export
#' @noRd
simulate.goldfishDynesFit <- function(
  object,
  nsim = 1,
  seed = NULL,
  coef = NULL,
  ...
) {
  # The fit holds the joint specification it was estimated from, so unlike a
  # single-process fit it needs no data and rebuilds no specification. It
  # simulates the completed one: completion keeps its pins on the wave grid
  # estimation used, where the authored one would be pinned on a single
  # default window.
  if (is.null(coef)) {
    coef <- dynes_fit_parameters(object)
  }
  simulate(object$spec, nsim = nsim, seed = seed, coef = coef, ...)
}

#' @method times_of goldfishDynesFit
#' @export
#' @noRd
times_of.goldfishDynesFit <- function(object) {
  times_of(object$authored_spec)
}

#' @export
#' @method print goldfishDynesFit
#' @noRd
print.goldfishDynesFit <- function(
  x,
  ...,
  digits = max(3, getOption("digits") - 2)
) {
  cli::cli_rule(left = "{.cls goldfishDynesFit}")
  dynes_fit_status(x)
  layout <- coef_layout(x)
  for (process in unique(layout$process)) {
    rows <- layout[layout$process == process, ]
    cli::cli_text("")
    cli::cli_text("{.strong {process}}")
    estimates <- stats::setNames(rows$value, rows$name)
    # Numeric tables stay on print.default: cli formats prose, not columns.
    print.default(
      format(estimates, digits = digits),
      print.gap = 2,
      quote = FALSE,
      ...
    )
  }
  invisible(x)
}

#' @export
#' @method summary goldfishDynesFit
#' @noRd
summary.goldfishDynesFit <- function(object, ...) {
  layout <- coef_layout(object)
  z <- layout$value / layout$se
  layout$z <- z
  layout$p <- 2 * stats::pnorm(-abs(z))
  structure(
    list(
      coefficients = layout,
      converged = object$converged,
      iterations = object$iterations,
      n_sequences = length(object$pool$sequences),
      covariance_status = object$covariance_status,
      fraction_missing = object$fraction_missing,
      covariance_ess = object$covariance_ess
    ),
    class = "goldfishSummDynesFit"
  )
}

#' @export
#' @method print goldfishSummDynesFit
#' @noRd
print.goldfishSummDynesFit <- function(
  x,
  ...,
  digits = max(3, getOption("digits") - 2)
) {
  cli::cli_rule(left = "{.cls goldfishSummDynesFit}")
  dynes_fit_status(x)
  table <- x$coefficients
  for (process in unique(table$process)) {
    rows <- table[table$process == process, ]
    cli::cli_text("")
    cli::cli_text("{.strong {process}}")
    coefmat <- cbind(
      Estimate = rows$value,
      `Std. Error` = rows$se,
      `MC Error` = rows$mc_se,
      `z-value` = rows$z,
      `Pr(>|z|)` = rows$p
    )
    rownames(coefmat) <- rows$name
    # Estimate, standard error and MC error share one scale; the z-value is
    # the test statistic column.
    stats::printCoefmat(
      coefmat,
      digits = digits,
      signif.stars = FALSE,
      cs.ind = 1:3,
      tst.ind = 4L,
      na.print = "",
      ...
    )
  }
  cli::cli_text("")
  cli::cli_text(
    "{.field MC Error} is the Monte-Carlo error of the estimate from the
     final pool of {x$n_sequences} augmented sequence{?s}."
  )
  dynes_covariance_diagnostics(x)
  invisible(x)
}

# The covariance diagnostics under a summary's table: the fraction of missing
# information and the covariance ESS where they exist, and on a fit whose
# standard-error columns are blank, the reason and the lever the warning named.
dynes_covariance_diagnostics <- function(x) {
  status <- x$covariance_status
  if (status == "ok") {
    cli::cli_text(
      "Largest fraction of missing information:
       {.val {signif(x$fraction_missing, 3)}}."
    )
  } else if (status == "pool_dropped") {
    cli::cli_alert_warning(
      "Standard errors are blank: the pool stopped with no finite likelihood."
    )
  } else if (status == "singular") {
    cli::cli_alert_warning(
      "Standard errors are blank: the complete-data information is singular."
    )
    cli::cli_text(
      "An effect with no variation across the augmented sequences carries no
       information; see the warning for the coefficients."
    )
  } else {
    cli::cli_alert_warning(
      "Standard errors are blank: the observed information is not positive
       definite."
    )
    cli::cli_text(
      "The pool's estimate of the missing information is as large as the
       complete information in some direction. Consider a larger
       {.arg n_sequences} or a model-driven {.arg routine}."
    )
  }
  if (!is.na(x$covariance_ess)) {
    cli::cli_text(
      "Covariance effective sample size: {.val {signif(x$covariance_ess, 3)}}."
    )
  }
}

# The run's status line, shared by the fit and its summary: a fit that
# stopped on max_iterations is flagged before any number is read.
dynes_fit_status <- function(x) {
  if (isTRUE(x$converged)) {
    cli::cli_alert_success(
      "Converged after {x$iterations} accepted EM iteration{?s}."
    )
  } else {
    # An alert keeps its source line breaks, so the text is one line here and
    # the explanation goes to a wrapped paragraph.
    cli::cli_alert_warning("Not converged.")
    cli::cli_text(
      "{.arg max_iterations} ended the run after {x$iterations} accepted EM
       iteration{?s}; the estimates are the last accepted ones."
    )
  }
}

#' @export
#' @method tidy goldfishDynesFit
#' @noRd
tidy.goldfishDynesFit <- function(x, ...) {
  layout <- coef_layout(x)
  free <- layout[!layout$fixed, ]
  statistic <- free$value / free$se
  tibble::tibble(
    process = free$process,
    term = paste0(free$process, ": ", free$name),
    estimate = free$value,
    std.error = free$se,
    mc.error = free$mc_se,
    statistic = statistic,
    p.value = 2 * stats::pnorm(-abs(statistic))
  )
}

#' @export
#' @method glance goldfishDynesFit
#' @noRd
glance.goldfishDynesFit <- function(x, ...) {
  # No log-likelihood and no information criterion: the objective is a
  # Monte-Carlo estimate (see logLik.goldfishDynesFit()).
  tibble::tibble(
    converged = x$converged,
    iterations = x$iterations,
    n_sequences = length(x$pool$sequences),
    df = length(x$parameters),
    covariance_status = x$covariance_status,
    fraction_missing = x$fraction_missing,
    covariance_ess = x$covariance_ess,
    newton_se = x$newton_se,
    at_optimum = x$at_optimum
  )
}

# Refused generics ----

refuse_dynes_generic <- function(generic, reason, instead = NULL) {
  cli::cli_abort(
    c(
      "{.fn {generic}} is not defined for a {.cls goldfishDynesFit} object.",
      "x" = reason,
      "i" = instead
    ),
    class = "goldfish_generic_refused",
    call = rlang::caller_env()
  )
}

dynes_event_level_reason <- paste(
  "It reads the event-level statistics of one observed sequence, and a DyNES",
  "fit has a weighted pool of augmented sequences instead."
)

#' @export
#' @method logLik goldfishDynesFit
#' @noRd
logLik.goldfishDynesFit <- function(object, ...) {
  # Refused so that AIC() and BIC(), which reach a fit only through logLik(),
  # refuse too. Any total reported in future is a Monte-Carlo estimate and
  # includes the fixed-only processes' parameter-free per-path term, which the
  # pinned-rate contract allows in a reported total but never in the
  # objective.
  refuse_dynes_generic(
    "logLik",
    "The estimator ascends a Monte-Carlo estimate of the observed-data
     likelihood, so there is no exact log-likelihood to return, and
     {.fn AIC} and {.fn BIC} cannot be computed for this fit.",
    "The ascent diagnostics are in the fit's {.field em_trace}."
  )
}

#' @export
#' @method model_terms goldfishDynesFit
#' @noRd
model_terms.goldfishDynesFit <- function(x, pattern = NULL, ...) {
  refuse_dynes_generic(
    "model_terms",
    "Its term table is keyed by the effect names of a single process.",
    "Use {.fn coef_layout} for the joint fit's coefficients by process."
  )
}

#' @export
#' @method augment goldfishDynesFit
#' @noRd
augment.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("augment", dynes_event_level_reason)
}

#' @export
#' @method fitted goldfishDynesFit
#' @noRd
fitted.goldfishDynesFit <- function(object, ...) {
  refuse_dynes_generic("fitted", dynes_event_level_reason)
}

#' @export
#' @method predict goldfishDynesFit
#' @noRd
predict.goldfishDynesFit <- function(object, ...) {
  refuse_dynes_generic("predict", dynes_event_level_reason)
}

#' @export
#' @method residuals goldfishDynesFit
#' @noRd
residuals.goldfishDynesFit <- function(object, ...) {
  refuse_dynes_generic("residuals", dynes_event_level_reason)
}

#' @export
#' @method evaluate_model goldfishDynesFit
#' @noRd
evaluate_model.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("evaluate_model", dynes_event_level_reason)
}

#' @export
#' @method test_gof goldfishDynesFit
#' @noRd
test_gof.goldfishDynesFit <- function(object, ...) {
  refuse_dynes_generic("test_gof", dynes_event_level_reason)
}

#' @export
#' @method test_parameter goldfishDynesFit
#' @noRd
test_parameter.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("test_parameter", dynes_event_level_reason)
}

#' @export
#' @method test_time goldfishDynesFit
#' @noRd
test_time.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("test_time", dynes_event_level_reason)
}

#' @export
#' @method diagnose_outliers goldfishDynesFit
#' @noRd
diagnose_outliers.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("diagnose_outliers", dynes_event_level_reason)
}

#' @export
#' @method diagnose_changepoints goldfishDynesFit
#' @noRd
diagnose_changepoints.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("diagnose_changepoints", dynes_event_level_reason)
}

#' @export
#' @method diagnose_onset goldfishDynesFit
#' @noRd
diagnose_onset.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("diagnose_onset", dynes_event_level_reason)
}

#' @export
#' @method margin_table goldfishDynesFit
#' @noRd
margin_table.goldfishDynesFit <- function(x, ...) {
  refuse_dynes_generic("margin_table", dynes_event_level_reason)
}

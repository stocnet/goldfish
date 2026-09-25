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
new_dynes_fit <- function(res, authored_spec, control, fit_call, call) {
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

  res$covariance <- covariances$covariance
  res$mc_covariance <- covariances$mc_covariance
  res$authored_spec <- authored_spec
  res$call <- fit_call
  class(res) <- c("goldfishDynesFit", "goldfishBaseFit")
  res
}

# The two covariance estimators at `theta` over the whole pool, with
# importance weights w, I = sum_i w_i I_i and S = sum_i w_i s_i:
#   standard error  I^-1 (sum_i w_i s_i s_i') I^-1          (Ruth 2024, 2.2)
#   Monte Carlo     I^-1 (sum_i w_i^2 (s_i - S)(s_i - S)') I^-1
# The middle term of the second is the Monte-Carlo variance of the weighted
# score; aggregating the scores before the outer product (S S') would give a
# rank-1 matrix bounded by the M-step's gradient tolerance, not a variance.
# A singular information yields all-NA matrices rather than an error.
dynes_covariances <- function(pool, theta, weights) {
  p <- length(theta)
  res <- evaluate_sequence_pool(pool, theta, what = c("score", "fisher"))
  keep <- weights > 0 & !res$degenerate
  scores <- res$score[keep, , drop = FALSE]
  w <- weights[keep]
  information <- Reduce(`+`, Map(`*`, res$fisher[keep], w))
  meat <- crossprod(scores * sqrt(w))
  centered <- sweep(scores, 2L, colSums(scores * w))
  mc_meat <- crossprod(centered * w)
  unavailable <- matrix(NA_real_, p, p)
  bread <- tryCatch(solve(information), error = function(e) NULL)
  if (is.null(bread)) {
    return(list(covariance = unavailable, mc_covariance = unavailable))
  }
  list(
    covariance = bread %*% meat %*% bread,
    mc_covariance = bread %*% mc_meat %*% bread
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

#' @export
#' @method vcov goldfishDynesFit
#' @noRd
vcov.goldfishDynesFit <- function(object, ...) {
  object$covariance
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
  # single-process fit it needs no data and rebuilds no specification.
  if (is.null(coef)) {
    coef <- dynes_fit_parameters(object)
  }
  simulate(object$authored_spec, nsim = nsim, seed = seed, coef = coef, ...)
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
      n_sequences = length(object$pool$sequences)
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
  invisible(x)
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
    df = length(x$parameters)
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

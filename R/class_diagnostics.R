#' Diagnostic functions
#'
#' Provide diagnostic functions for an object of class \code{result.goldfish}.
#' \code{outliers} helps to identify outliers events.
#' \code{changepoints} helps to identify where a change point
#' in the events sequence is presented using the log-likelihood.
#' @param x an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call.
#' @param include_censored logical, whether the right-censored intervals take
#'   part in the statistic. Defaults to `FALSE` — see the
#'   \emph{Which intervals are analyzed} section, which explains why the
#'   pooled series misleads. Either way the returned table keeps one row per
#'   interval; the setting decides which rows can be flagged, not which rows
#'   exist.
#'
#' @section Which intervals are analyzed:
#' A fitted rate or REM model has two kinds of interval, and they contribute
#' structurally different quantities to the per-interval log-likelihood these
#' functions read:
#' \describe{
#'   \item{a **dependent** interval}{ends in an observed event, and
#'     contributes a log density that includes the observed alternative's own
#'     term.}
#'   \item{a **right-censored** interval}{ends in an exogenous change — a
#'     window closing, a covariate updating — and contributes only the timing
#'     term, with no observed alternative.}
#' }
#'
#' Pooling them makes the median, the interquartile range, the Hampel window
#' and the changepoint segmentation describe the *censoring pattern* rather
#' than the fit. The effect is largest exactly where windowed effects are
#' used: a window opens at each event and closes a fixed time later, so the
#' two kinds of interval alternate almost one for one, and the series becomes
#' a square wave whose transitions a changepoint detector dutifully reports.
#' On a windowed rate model of the `social_evolution` calls, segmenting the
#' pooled series finds a changepoint at nearly every window closure, while
#' the dependent intervals alone yield an order of magnitude fewer.
#'
#' The default therefore analyzes the dependent intervals only. Set
#' `include_censored = TRUE` to pool them, knowing what the pooled series
#' mixes. On the multinomial sub-models — choice, the ordinal rate and REM
#' sub-models, coordination — there are no right-censored intervals and the
#' two settings agree.
#'
#' This restriction is deliberately *not* applied to the score-based
#' diagnostics ([diagnose_onset()]): a right-censored interval's score row is
#' a genuine contribution to the gradient, which sums to zero over all
#' intervals, so a score series restricted to a subset has no null to be read
#' against.
#'
#' @name diagnose
#' @examples
#' # A multinomial receiver choice model on the prebuilt `social_evolution` data
#' data("social_evolution")
#' mod01 <- estimate_dynam(
#'   calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(
#'     return_interval_loglik = TRUE,
#'     backend = "cpp"
#'   )
#' )
#'
#' diagnose_outliers(mod01)
#'
#' diagnose_changepoints(mod01)
NULL

# Examine outlier cases
#' @param method A method for identifying outliers.
#'   The current options are "Hampel" for a Hampel filter/identifier,
#'   "IQR" for identifying outliers on the basis of lying outside
#'   the interquartile range, and "Top" which returns the
#'   `threshold` number of outliers.
#' @param threshold An integer that represents the number of absolute outliers
#'   to identify, the threshold for the Hampel filter, i.e. `threshold * MAD`,
#'   or the threshold beyond the interquartile range halved, i.e.
#'   `threshold/2 * IQR`.
#' @param parameter `r lifecycle::badge("deprecated")` Renamed to `threshold`
#'   in goldfish 2.0.0.
#' @param window The window half-width for the Hampel filter.
#'   By default it is half the width of the event sequence.
#' @section Outliers:
#' \code{diagnose_outliers} creates a plot with the log-likelihood of the events
#' in the y-axis and the event index in the x-axis, identifying observations
#' with labels indicating the sender and recipient.
#' The function call creates an object identifying the outliers
#' identified by the method
#' @importFrom stats IQR median na.exclude
#' @export
#' @rdname diagnose
diagnose_outliers <- function(
  x,
  method = c("Hampel", "IQR", "Top"),
  threshold = 3,
  window = NULL,
  include_censored = FALSE,
  parameter = deprecated()
) {
  threshold <- fold_renamed_arg(
    threshold,
    !missing(threshold),
    parameter,
    "diagnose_outliers",
    "parameter",
    "threshold"
  )
  abort_if_not_diagnosable(x, "Outlier identification")
  method <- match.arg(method)

  # Through the generic, like the other broom surfaces: the method is no longer
  # an exported name of its own.
  data <- augment(x)
  candidate <- diagnosable_intervals(data, include_censored)
  series <- data$interval_log_lik[candidate]
  positions <- which(candidate)

  data <- transform(data, label = "")
  data <- transform(data, outlier = FALSE)

  # Every branch below indexes the CANDIDATE series, so `flagged` holds
  # positions within it; `positions[flagged]` carries them back to interval
  # indices, which is what the returned one-row-per-interval table needs.
  if (method == "Top") {
    flagged <- order(series)[seq_len(min(threshold, length(series)))]
  } else if (method == "IQR") {
    flagged <- which(
      series < median(series) - (threshold / 2) * IQR(series)
    )
  } else if (method == "Hampel") {
    if (is.null(window)) {
      window <- (length(series) / 2) - 1
    }
    n <- length(series)
    L <- 1.4826
    flagged <- numeric(0)
    for (i in (window + 1):(n - window)) {
      x0 <- median(series[(i - window):(i + window)])
      S0 <- L * median(abs(series[(i - window):(i + window)] - x0))
      if (abs(series[i] - x0) > threshold * S0) {
        flagged <- c(flagged, i)
      }
    }
  }
  outlierIndexes <- positions[flagged]

  if (length(outlierIndexes) > 0) {
    data$outlier[outlierIndexes] <- TRUE
    data$label[outlierIndexes] <- paste(
      data$sender,
      data$receiver,
      sep = "-"
    )[outlierIndexes]
  }
  # otherwise if no outliers, no change required

  new_diagnostic_table(
    data,
    "diagnose_outliers",
    context = diagnose_context(x, candidate),
    params = list(
      method = method,
      threshold = threshold,
      window = window,
      include_censored = include_censored
    )
  )
}


# Examine change point
#' @param moment character argument to choose between "mean" or "variance".
#' See section \emph{Change point} for details.
#' @param method Choice of \code{"AMOC"}, \code{"PELT"} or \code{"BinSeg"}.
#' For a detail description see \code{\link[changepoint]{cpt.mean}} or
#' \code{\link[changepoint]{cpt.var}}. The default value is \code{"PELT"}.
#' @param ... additional arguments to be passed to the functions in the
#' \pkg{changepoint} package.
#' @section Change point:
#' The parameter \code{moment} controls which method from the package
#' \pkg{changepoint} is used:
#' \describe{
#'   \item{\code{"mean"}}{It uses the \code{\link[changepoint]{cpt.mean}}
#'   function to investigate optimal positioning and (potentially) number
#'   of change points for the log-likelihood of the events in mean.}
#'   \item{\code{"variance"}}{It uses the
#'   \code{\link[changepoint]{cpt.var}}
#'   function to investigate optimal positioning and (potentially) number
#'   of change points for the log-likelihood of the events in variance}
#' }
#' The function call creates an object identifying the change
#' point sections identified by the method.
#' @export
#' @rdname diagnose
diagnose_changepoints <- function(
  x,
  moment = c("mean", "variance"),
  method = c("PELT", "AMOC", "BinSeg"),
  window = NULL,
  include_censored = FALSE,
  ...
) {
  abort_if_not_diagnosable(x, "Changepoint identification")

  moment <- match.arg(moment)
  method <- match.arg(method)

  # Through the generic, like the other broom surfaces: the method is no longer
  # an exported name of its own.
  data <- augment(x)
  candidate <- diagnosable_intervals(data, include_censored)
  series <- data$interval_log_lik[candidate]
  positions <- which(candidate)

  if (is.null(window)) {
    window <- max(table(data$time[candidate]))
  }

  if (moment == "mean") {
    cpt <- changepoint::cpt.mean(
      series,
      method = method,
      minseglen = window,
      ...
    )
  }
  if (moment == "variance") {
    cpt <- changepoint::cpt.var(
      series,
      method = method,
      minseglen = window,
      ...
    )
  }

  # The detector returns positions in the analyzed series; carry them back to
  # interval indices before anything reads them as rows of the table.
  cpt.pts <- positions[attributes(cpt)$cpts]
  # cpt.mean <- attributes(cpt)$param.est$mean

  if (anyDuplicated(data$time[cpt.pts])) {
    cpt.pts <- cpt.pts[!duplicated(data$time[cpt.pts], fromLast = TRUE)]
  }

  data <- transform(data, cpt = FALSE)
  data$cpt[cpt.pts] <- TRUE

  new_diagnostic_table(
    data,
    "diagnose_changepoints",
    context = diagnose_context(x, candidate),
    params = list(
      moment = moment,
      method = method,
      window = window,
      include_censored = include_censored
    )
  )
}

# Which intervals take part in the statistic. Read off the `NA` pattern
# `augment()` already carries on `.resid`, which marks exactly the intervals
# realizing no outcome -- a second definition of "is this a dependent event"
# is what would let the two drift apart.
diagnosable_intervals <- function(data, include_censored) {
  if (isTRUE(include_censored)) {
    return(rep(TRUE, nrow(data)))
  }
  !is.na(data$.resid)
}

# The shared guard: both functions need a fitted model that stored the
# per-interval log-likelihood, and say which primitive is missing rather than
# failing later on a NULL.
abort_if_not_diagnosable <- function(x, what, call = rlang::caller_env()) {
  if (!inherits(x, "result.goldfish")) {
    cli::cli_abort(
      c(
        "{what} needs a fitted goldfish model.",
        "x" = "{.arg x} is {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  if (is.null(x$interval_log_lik)) {
    cli::cli_abort(
      c(
        "{what} needs the {.val loglik} primitive, which this fit did not
         store.",
        "i" = "Re-estimate with {.arg diagnostics} including {.val loglik} in
               {.fn set_algorithm_newton}."
      ),
      call = call
    )
  }
  invisible(NULL)
}

# The D18 context: what the table was computed from, and how much of the
# sequence took part. `n_analyzed` is what separates the two settings of
# `include_censored`, so a reader of a saved object can tell which it was.
diagnose_context <- function(x, candidate) {
  list(
    model = x$model,
    sub_model = x$sub_model,
    backend = x$backend,
    n_intervals = length(candidate),
    n_analyzed = sum(candidate)
  )
}

#' Diagnostic functions
#'
#' Provide diagnostic functions for an object of class \code{result.goldfish}.
#' \code{outliers} helps to identify outliers events.
#' \code{changepoints} helps to identify where a change point
#' in the events sequence is presented using the log-likelihood.
#' @param x an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call; for the print methods, the
#' \code{diagnose_outliers} or \code{diagnose_changepoints} table they render.
#' @param ... additional arguments passed to or from other methods.
#' \code{diagnose_changepoints} passes them on to the \pkg{changepoint}
#' function its \code{moment} selects; the other methods have none of their
#' own.
#' @param include_censored `r lifecycle::badge("deprecated")` It selected
#'   whether the right-censored intervals joined the statistic. Each row is now
#'   one dependent event whose span already accumulates the censored intervals
#'   of its own waiting time, so there is no pooled alternative left to choose
#'   and the argument is ignored.
#' @param effect an optional single model term, naming the series to diagnose
#'   instead of the per-interval log-likelihood — see the \emph{Diagnosing one
#'   term} section. Accepts any name the term answers to (the compact string
#'   the summary prints, the export form, the `coef()` label) or its position;
#'   [model_terms()] lists them.
#' @param preprocessed a `preprocessed.goldfish` object, needed only when
#'   `effect` is given on an exact-time fit that did not store the
#'   `"conditional_scores"` primitive. See [residuals.result.goldfish()].
#'
#' @section Diagnosing one term:
#' Without `effect` both functions read the per-interval log-likelihood, and
#' answer a question about the *model*: which intervals it fit badly, and where
#' its fit shifted. With `effect` they read that term's own series instead, and
#' answer a question about the *coefficient*:
#' \describe{
#'   \item{`diagnose_changepoints(effect =)`}{segments the term's scaled
#'     Schoenfeld residuals, whose level is the coefficient an interval
#'     "votes" for — so a changepoint there is a regime shift in the effect
#'     itself rather than in overall fit.}
#'   \item{`diagnose_outliers(effect =)`}{ranks intervals by the absolute
#'     `dfbeta` for that term — how far each one moved that coefficient. This
#'     localizes influence rather than surprise: an interval can be perfectly
#'     ordinary in likelihood and still be the one carrying an estimate.}
#' }
#'
#' Whichever series was analyzed comes back in the `.series` column of the
#' returned table, `NA` on the intervals that took no part, and `params` names
#' it. A plot method therefore draws the series the flags were computed from,
#' rather than the log-likelihood beside flags that came from something else.
#'
#' **Post-selection caveat.** A changepoint found on a term's score series is
#' *exploratory*. It was chosen by looking at the data, so re-testing the same
#' split on the same data with `test_time(method = "periods")` is not
#' confirmatory evidence — the split was selected to look extreme. Treat it as
#' a hypothesis to check on other data, or against a pre-specified period
#' structure.
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
#' A series mixing the two described the *censoring pattern* rather than the
#' fit. The effect was largest exactly where windowed effects are used: a window
#' opens at each event and closes a fixed time later, so the two kinds of
#' interval alternated almost one for one and the series became a square wave
#' whose transitions a changepoint detector dutifully reported.
#'
#' **That alternation no longer exists.** These functions read one row per
#' **dependent event**, and each row's span has already accumulated the
#' right-censored intervals of its own waiting time — so the censored
#' contribution is present, attributed to the event it belongs to, rather than
#' interleaved as rows of its own. The `include_censored` argument that used to
#' choose between the two readings is deprecated and ignored: there is no longer
#' a second reading to select.
#'
#' A fit whose observation window outlives its last event carries one final span
#' that closes no waiting time. It appears as a censored row, is not a
#' candidate for flagging, and is the only row for which `.resid` is `NA`.
#'
#' [diagnose_onset()] never had this problem and still does not: a
#' right-censored interval's score row is a genuine contribution to the
#' gradient, which sums to zero over all intervals, so a score series
#' restricted to a subset has no null to be read against.
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
diagnose_outliers <- function(x, ...) {
  UseMethod("diagnose_outliers")
}

#' @export
diagnose_outliers.default <- function(x, ...) {
  abort_not_diagnosable_class("diagnose_outliers", x)
}

#' @export
#' @rdname diagnose
diagnose_outliers.result.goldfish <- function(
  x,
  method = c("Hampel", "IQR", "Top"),
  threshold = 3,
  window = NULL,
  effect = NULL,
  include_censored = deprecated(),
  preprocessed = NULL,
  parameter = deprecated(),
  ...
) {
  threshold <- fold_renamed_arg(
    threshold,
    !missing(threshold),
    parameter,
    "diagnose_outliers",
    "parameter",
    "threshold"
  )
  warn_include_censored(include_censored, "diagnose_outliers")
  abort_if_not_diagnosable(x, "Outlier identification")
  method <- match.arg(method)

  # Through the generic, like the other broom surfaces: the method is no longer
  # an exported name of its own.
  data <- augment(x)
  candidate <- diagnosable_intervals(data)
  positions <- which(candidate)
  # Without `effect` the series is the per-interval log-likelihood: how
  # surprising each interval was. With it, the term's own influence series --
  # a large value means that interval moved THAT coefficient, which is a
  # different question and localizes rather than ranks.
  selected <- selected_term(x, effect, "diagnose_outliers")
  reported <- if (is.null(selected)) {
    data$event_log_lik[candidate]
  } else {
    abs(residuals(
      x,
      type = "dfbeta",
      preprocessed = preprocessed
    )[candidate, selected$index])
  }
  # Every branch below takes the SMALLEST values as the flagged ones, while
  # influence is extreme in either direction and large rather than small, so
  # the term-wise ranking is negated to reuse one code path.
  series <- if (is.null(selected)) reported else -reported
  data$.series <- NA_real_
  data$.series[positions] <- reported

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
      effect = selected$term,
      series = if (is.null(selected)) {
        "Interval log likelihood"
      } else {
        "Absolute dfbeta"
      }
    ),
    defining = c("outlier", ".series")
  )
}


# Examine change point
#' @param moment character argument to choose between "mean" or "variance".
#' See section \emph{Change point} for details.
#' @param method Choice of \code{"AMOC"}, \code{"PELT"} or \code{"BinSeg"}.
#' For a detail description see \code{\link[changepoint]{cpt.mean}} or
#' \code{\link[changepoint]{cpt.var}}. The default value is \code{"PELT"}.
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
diagnose_changepoints <- function(x, ...) {
  UseMethod("diagnose_changepoints")
}

#' @export
diagnose_changepoints.default <- function(x, ...) {
  abort_not_diagnosable_class("diagnose_changepoints", x)
}

#' @export
#' @rdname diagnose
diagnose_changepoints.result.goldfish <- function(
  x,
  moment = c("mean", "variance"),
  method = c("PELT", "AMOC", "BinSeg"),
  window = NULL,
  effect = NULL,
  include_censored = deprecated(),
  preprocessed = NULL,
  ...
) {
  warn_include_censored(include_censored, "diagnose_changepoints")
  abort_if_not_diagnosable(x, "Changepoint identification")

  moment <- match.arg(moment)
  method <- match.arg(method)

  # Through the generic, like the other broom surfaces: the method is no longer
  # an exported name of its own.
  data <- augment(x)
  candidate <- diagnosable_intervals(data)
  positions <- which(candidate)
  # Without `effect` the series is the per-interval log-likelihood, and a
  # changepoint is a shift in how well the model fits. With it, the term's
  # scaled Schoenfeld series, whose level IS the coefficient -- so a
  # changepoint there is a regime shift in the effect itself.
  selected <- selected_term(x, effect, "diagnose_changepoints")
  series <- if (is.null(selected)) {
    data$event_log_lik[candidate]
  } else {
    residuals(
      x,
      type = "scaled_schoenfeld",
      preprocessed = preprocessed
    )[candidate, selected$index]
  }
  # An exact-time fit has no Schoenfeld row on a right-censored interval, so
  # the term-wise series is NA there whatever `include_censored` says. Drop
  # those rather than handing NAs to the detector.
  if (anyNA(series)) {
    keep <- !is.na(series)
    series <- series[keep]
    positions <- positions[keep]
  }
  data$.series <- NA_real_
  data$.series[positions] <- series

  if (is.null(window)) {
    window <- max(table(data$time[positions]))
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
      effect = selected$term,
      series = if (is.null(selected)) {
        "Interval log likelihood"
      } else {
        "Scaled Schoenfeld residual"
      }
    ),
    defining = c("cpt", ".series")
  )
}

# Which intervals take part in the statistic. Read off the `NA` pattern
# `augment()` already carries on `.resid`, which marks exactly the intervals
# realizing no outcome -- a second definition of "is this a dependent event"
# is what would let the two drift apart.
diagnosable_intervals <- function(data) {
  !is.na(data$.resid)
}

# `include_censored` existed to suppress the alternation of dependent and
# right-censored intervals, which made a segmented series describe the
# censoring pattern rather than the fit. Under the per-event contract there is
# no alternation left to suppress: `augment()` returns one row per dependent
# event, each already carrying the censored intervals of its own waiting time.
#
# The argument is therefore inert rather than merely discouraged, and it is
# deprecated rather than removed because it ships in 1.9.23. Ignoring it
# silently would be the failure mode this change spent its guard work removing.
warn_include_censored <- function(
  include_censored,
  fn,
  user_env = rlang::caller_env(2)
) {
  if (!lifecycle::is_present(include_censored)) {
    return(invisible())
  }
  lifecycle::deprecate_warn(
    when = "2.0.0",
    what = paste0(fn, "(include_censored)"),
    details = c(
      "i" = cli::format_inline(
        "Each row is now one dependent event, whose span already accumulates
         the right-censored intervals of its own waiting time, so there is no
         pooled alternative to select."
      )
    ),
    user_env = user_env
  )
  invisible()
}

# Which term `effect =` selected, or NULL for the default log-likelihood
# series. Resolved through the one matcher every term argument uses, so the
# string that selects a term here selects it in `initial_parameters` and in the
# `test_*` family too. A single term only: these series are one column each,
# and a changepoint or an influence rank over several of them at once would be
# a statistic nobody asked for.
selected_term <- function(x, effect, arg, call = rlang::caller_env()) {
  if (is.null(effect)) {
    return(NULL)
  }
  if (length(effect) != 1L) {
    cli::cli_abort(
      c(
        "{.arg effect} must select a single term.",
        "x" = "It selects {length(effect)}.",
        "i" = "Call {.fn {arg}} once per term."
      ),
      call = call
    )
  }
  index <- resolve_term_index(effect, x$names, "effect", call = call)
  list(
    index = index,
    term = unname(compact_term_strings(x$names, "console", width = Inf))[index]
  )
}

# What a `diagnose_*` or `test_*` default method says about what it received.
# Both families dispatch on the fitted object, so the class is settled before
# any body runs and this is the only place that reports a wrong one. `arg`
# names the first argument, which is `object` on the one generic whose
# signature is set by a sibling package rather than by goldfish.
abort_not_diagnosable_class <- function(
  fn,
  x,
  arg = "x",
  call = rlang::caller_env()
) {
  cli::cli_abort(
    c(
      "{.fn {fn}} needs a fitted goldfish model.",
      "x" = "{.arg {arg}} is {.obj_type_friendly {x}}.",
      "i" = "Fit one with {.fn estimate_dynam} or {.fn estimate_rem}."
    ),
    call = call
  )
}

# The shared guard of the diagnose family: a fitted model that stored the
# primitive the function reads, saying which one is missing rather than failing
# later on a NULL. The default pair is the per-interval log-likelihood, which
# is what the two interval diagnostics read; `diagnose_onset()` names the score
# rows instead.
abort_if_not_diagnosable <- function(
  x,
  what,
  component = "interval_log_lik",
  primitive = "loglik",
  call = rlang::caller_env()
) {
  if (is.null(x[[component]])) {
    cli::cli_abort(
      c(
        "{what} needs the {.val {primitive}} primitive, which this fit did not
         store.",
        "i" = "Re-estimate with {.arg diagnostics} including
               {.val {primitive}} in {.fn set_algorithm_newton}."
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

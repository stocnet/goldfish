#' Diagnostic functions
#'
#' Provide diagnostic functions for an object of class \code{result.goldfish}.
#' \code{outliers} helps to identify outliers events.
#' \code{changepoints} helps to identify where a change point
#' in the events sequence is presented using the log-likelihood.
#' @param x an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call.
#' @return \code{NULL} if neither outliers nor change points are identified.
# A subset of the dependent event data frame
# (see [make_dependent_events()]) with the events identified as
# outliers or change point inflections.
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
  if (!"result.goldfish" %in% attr(x, "class")) {
    stop("Not a goldfish results object.")
  }
  if (is.null(x$interval_log_lik)) {
    stop(
      "Outlier identification only available when interval log likelihood
      returned in results object."
    )
  }
  method <- match.arg(method)

  data <- augment.result.goldfish(x)

  data <- transform(data, label = "")
  data <- transform(data, outlier = FALSE)

  if (method == "Top") {
    outlierIndexes <- order(data$interval_log_lik)[1:threshold]
  } else if (method == "IQR") {
    outlierIndexes <- which(
      data$interval_log_lik <
        median(data$interval_log_lik) -
          (threshold / 2) * IQR(data$interval_log_lik)
    )
  } else if (method == "Hampel") {
    if (is.null(window)) {
      window <- (nrow(data) / 2) - 1
    }
    n <- length(data$interval_log_lik)
    L <- 1.4826
    # which(vapply((window + 1):(n - window), function(i) {
    #   x0 <- median(data$interval_log_lik[(i - window):(i + window)])
    #   S0 <- L *
    #     median(abs(data$interval_log_lik[(i - window):(i + window)] - x0))
    #   if (abs(data$interval_log_lik[i] - x0) > threshold * S0) TRUE else FALSE
    # }, FUN.VALUE = logical(1)))
    outlierIndexes <- numeric(0)
    for (i in (window + 1):(n - window)) {
      x0 <- median(data$interval_log_lik[(i - window):(i + window)])
      S0 <- L *
        median(abs(data$interval_log_lik[(i - window):(i + window)] - x0))
      if (abs(data$interval_log_lik[i] - x0) > threshold * S0) {
        outlierIndexes <- c(outlierIndexes, i)
      }
    }
  }

  if (length(outlierIndexes > 0)) {
    data$outlier[outlierIndexes] <- TRUE
    data$label[outlierIndexes] <- paste(
      data$sender,
      data$receiver,
      sep = "-"
    )[outlierIndexes]
  }
  # otherwise if no outliers, no change required

  class(data) <- c("diagnostic.goldfish", class(data))

  return(data)
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
  ...
) {
  if (!methods::is(x, "result.goldfish")) {
    stop("Not a goldfish results object.", call. = FALSE)
  }
  if (is.null(x$interval_log_lik)) {
    stop(
      "Changepoint identification only available when interval log likelihood
      returned in results object."
    )
  }

  moment <- match.arg(moment)
  method <- match.arg(method)

  data <- augment.result.goldfish(x)

  if (is.null(window)) {
    window <- max(table(data$time))
  }

  if (moment == "mean") {
    cpt <- changepoint::cpt.mean(
      data$interval_log_lik,
      method = method,
      minseglen = window,
      ...
    )
  }
  if (moment == "variance") {
    cpt <- changepoint::cpt.var(
      data$interval_log_lik,
      method = method,
      minseglen = window,
      ...
    )
  }

  cpt.pts <- attributes(cpt)$cpts
  # cpt.mean <- attributes(cpt)$param.est$mean

  if (anyDuplicated(data$time[cpt.pts])) {
    cpt.pts <- cpt.pts[!duplicated(data$time[cpt.pts], fromLast = TRUE)]
  }

  data <- transform(data, cpt = FALSE)
  data$cpt[cpt.pts] <- TRUE

  class(data) <- c("diagnostic.goldfish", class(data))

  return(data)
}

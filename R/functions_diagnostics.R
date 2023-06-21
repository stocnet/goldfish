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
# (see \code{\link{defineDependentEvents}}) with the events identified as
# outliers or change point inflections.
#' An object of class `ggplot` object from a call of [ggplot2::ggplot()].
#' It can be modified using the `ggplot2` syntax.
#' @name examine
#' @examples
#' # A multinomial receiver choice model
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(
#'   x = callNetwork, changeEvent = calls,
#'   nodes = actors
#' )
#' callsDependent <- defineDependentEvents(
#'   events = calls, nodes = actors,
#'   defaultNetwork = callNetwork
#' )
#' \dontshow{
#' callsDependent <- callsDependent[1:50, ]
#' }
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'   model = "DyNAM", subModel = "choice",
#'   estimationInit = list(
#'     returnIntervalLogL = TRUE,
#'     engine = "default_c"
#'   )
#' )
#'
#' examineOutliers(mod01)
#'
#' examineChangepoints(mod01)
NULL

# Examine outlier cases
#' @param method A method for identifying outliers.
#'   The current options are "Hampel" for a Hampel filter/identifier,
#'   "IQR" for identifying outliers on the basis of lying outside
#'   the interquartile range, and "Top" which returns the
#'   `parameter` number of outliers.
#' @param parameter An integer that represents the number of absolute outliers
#'   to identify, the threshold for the Hampel filter, i.e. `parameter * MAD`,
#'   or the threshold beyond the interquartile range halved, i.e.
#'   `parameter/2 * IQR`.
#' @param window The window half-width for the Hampel filter.
#'   By default it is half the width of the event sequence.
#' @section Outliers:
#' \code{examineOutliers} creates a plot with the log-likelihood of the events
#' in the y-axis and the event index in the x-axis, identifying observations
#' with labels indicating the sender and recipient.
#' @importFrom stats IQR median na.exclude
#' @export
#' @rdname examine
examineOutliers <- function(x,
                            method = c("Hampel", "IQR", "Top"),
                            parameter = 3,
                            window = NULL) {
  if (!"result.goldfish" %in% attr(x, "class")) {
    stop("Not a goldfish results object.")
  }
  if (is.null(x$intervalLogL)) {
    stop(
      "Outlier identification only available when interval log likelihood",
      " returned in results object."
    )
  }
  method <- match.arg(method)

  data <- get(as.character(x$formula[2]))
  if (length(data$time) != length(x$intervalLogL)) {
    calls <- as.list(x$call)
    calls[[1]] <- NULL
    calls$preprocessingOnly <- TRUE
    calls$preprocessingInit <- NULL
    calls$progress <- FALSE
    calls$verbose <- FALSE
    prep <- suppressWarnings(do.call(estimate, calls))
    data$intervalLogL <- x$intervalLogL[prep$orderEvents == 1]
  } else {
    data$intervalLogL <- x$intervalLogL
  }

  if (!is.numeric(data$time)) {
    data$time <- as.POSIXct(data$time)
  }

  data$label <- ""
  data$outlier <- "NO"
  if (method == "Top") {
    outlierIndexes <- order(data$intervalLogL)[1:parameter]
  } else if (method == "IQR") {
    outlierIndexes <- which(
      data$intervalLogL < median(data$intervalLogL) -
        (parameter / 2) * IQR(data$intervalLogL)
    )
  } else if (method == "Hampel") {
    if (is.null(window)) window <- (nrow(data) / 2) - 1
    n <- length(data$intervalLogL)
    L <- 1.4826
    # which(vapply((window + 1):(n - window), function(i) {
    #   x0 <- median(data$intervalLogL[(i - window):(i + window)])
    #   S0 <- L * median(abs(data$intervalLogL[(i - window):(i + window)] - x0))
    #   if (abs(data$intervalLogL[i] - x0) > parameter * S0) TRUE else FALSE
    # }, FUN.VALUE = logical(1)))
    outlierIndexes <- numeric(0)
    for (i in (window + 1):(n - window)) {
      x0 <- median(data$intervalLogL[(i - window):(i + window)])
      S0 <- L * median(abs(data$intervalLogL[(i - window):(i + window)] - x0))
      if (abs(data$intervalLogL[i] - x0) > parameter * S0) {
        outlierIndexes <- c(outlierIndexes, i)
      }
    }
  }

  if (length(outlierIndexes > 0)) {
    data$outlier[outlierIndexes] <- "YES"
    data$label[outlierIndexes] <- paste(
      data$sender,
      data$receiver,
      sep = "-"
    )[outlierIndexes]
  } else {
    return(cat("No outliers found."))
  }

  ggplot2::ggplot(data, ggplot2::aes(x = .data$time, y = .data$intervalLogL)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color = .data$outlier)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label),
      angle = 270, size = 2,
      hjust = "outward", color = "red"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_manual(
      values = c("black", "red"),
      guide = "none"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Interval log likelihood")
}

# Examine change point
#' @param moment character argument to choose between "mean" or "variance".
#' See section \emph{Change point} for details.
#' @param method Choice of \code{"AMOC"}, \code{"PELT"} or \code{"BinSeg"}.
#' For a detail description see \code{\link[changepoint]{cpt.mean}} or
#' \code{\link[changepoint]{cpt.var}}. The default value is \code{"PELT"}.
#' @param ... additional arguments to be passed to the functions in the
#' \code{\link{changepoint}} package.
#' @section Change point:
#' The parameter \code{moment} controls which method from the package
#' \code{\link{changepoint}} is used:
#' \describe{
#'   \item{\code{"mean"}}{It uses the \code{\link[changepoint]{cpt.mean}}
#'   function to investigate optimal positioning and (potentially) number
#'   of change points for the log-likelihood of the events in mean.}
#'   \item{\code{"variance"}}{It uses the
#'   \code{\link[changepoint]{cpt.var}}
#'   function to investigate optimal positioning and (potentially) number
#'   of change points for the log-likelihood of the events in variance}
#' }
#' The function call creates a plot with the log-likelihood of the events
#' in the y-axis and the event index in the x-axis, highlighting the change
#' point sections identified by the method.
# Also it prints a table of the change points events that are returned by the
# method.
#' @export
#' @rdname examine
examineChangepoints <- function(x, moment = c("mean", "variance"),
                                method = c("PELT", "AMOC", "BinSeg"),
                                window = NULL,
                                ...) {
  if (!methods::is(x, "result.goldfish")) {
    stop("Not a goldfish results object.", call. = FALSE)
  }
  if (is.null(x$intervalLogL)) {
    stop(
      "Outlier identification only available when interval log likelihood",
      " returned in results object."
    )
  }

  moment <- match.arg(moment)
  method <- match.arg(method)

  data <- get(as.character(x$formula[2]))
  if (length(data$time) != length(x$intervalLogL)) {
    calls <- as.list(x$call)
    calls[[1]] <- NULL
    calls$preprocessingOnly <- TRUE
    calls$preprocessingInit <- NULL
    calls$progress <- FALSE
    calls$verbose <- FALSE
    prep <- suppressWarnings(do.call(goldfish::estimate, calls))
    data$intervalLogL <- x$intervalLogL[prep$orderEvents == 1]
  } else {
    data$intervalLogL <- x$intervalLogL
  }

  if (!is.numeric(data$time)) {
    data$time <- as.POSIXct(data$time)
  }

  if (is.null(window)) window <- max(table(data$time))

  if (moment == "mean") {
    cpt <- changepoint::cpt.mean(data$intervalLogL,
      method = method, minseglen = window, ...
    )
  }
  if (moment == "variance") {
    cpt <- changepoint::cpt.var(data$intervalLogL,
      method = method, minseglen = window, ...
    )
  }

  cpt.pts <- attributes(cpt)$cpts
  # cpt.mean <- attributes(cpt)$param.est$mean

  if (anyDuplicated(data$time[cpt.pts])) {
    cpt.pts <- cpt.pts[!duplicated(data$time[cpt.pts],
      fromLast = TRUE
    )]
  }
  if (length(cpt.pts) == 1 && data$time[cpt.pts] == max(data$time)) {
    return(cat("No regime changes found."))
  }

  ggplot2::ggplot(data, ggplot2::aes(x = .data$time, y = .data$intervalLogL)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(
      xintercept = na.exclude(data$time[cpt.pts]),
      color = "red"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") +
    ggplot2::ylab("Interval log likelihood") +
    ggplot2::scale_x_continuous(
      breaks = data$time[cpt.pts],
      labels = data$time[cpt.pts]
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

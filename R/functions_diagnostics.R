#' Diagnostic functions
#'
#' Provide diagnostic functions for an object of class \code{result.goldfish}.
#' \code{outliers} helps to identify outliers events.
#' \code{changepoints} helps to identify where a change point
#' in the events sequence is presented using the log-likelihood.
#' @param x an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call.
#' @return \code{NULL} if neither outliers nor change points are identified.
#' A subset of the dependent event data frame
#' (see \code{\link{defineDependentEvents}}) with the events identified as
#' outliers or change point inflections.
#' @name examine
#' @examples
#' \donttest{
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
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'   model = "DyNAM", subModel = "choice",
#'   estimationInit = list(returnIntervalLogL = TRUE)
#' )
#'
#' examine.outliers(mod01)
#'
#' examine.changepoints(mod01)
#' }
NULL

# Examine outlier cases
#' @param outliers either an integer for the number of outliers to report,
#' or "IQR" if instead those events with log likelihoods greater than 1.5*IQR
#' in absolute value should be identified.
#' @section Outliers:
#' \code{examineOutliers} creates a plot with the log-likelihood of the events
#' in the y-axis and the event index in the x-axis, identifying observations
#' with labels indicating the sender and recipient.
#' @importFrom graphics points
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text theme_minimal xlab ylab
#' @export
#' @rdname examine
examineOutliers <- function(x, outliers = 3) {
  
  if (!"result.goldfish" %in% attr(x, "class")) {
    stop("Not a goldfish results object.")
  }
  if (is.null(x$intervalLogL)) {
    stop(
      "Outlier identification only available when interval log likelihood",
      " returned in results object."
    )
  }

  data <- get(as.character(x$formula[2]))
  if(length(data$time != x$intervalLogL)){
    calls <- as.list(x$call)
    calls[[1]] <- NULL
    calls$preprocessingOnly <- TRUE
    calls$preprocessingInit <- NULL
    calls$silent <- TRUE
    calls$debug <- FALSE
    calls$verbose <- FALSE
    prep <- suppressWarnings(do.call(estimate, calls))
    data$intervalLogL <- x$intervalLogL[prep$orderEvents==1]
  } else data$intervalLogL <- x$intervalLogL

  data$label <- ""
  if (is.integer(outliers)) {
    outlierIndexes <- order(data$intervalLogL)[1:outliers]
    data$label[outlierIndexes] <- paste(data$sender, 
                                        data$receiver, sep = "-")[outlierIndexes]
  } else if (outliers == "IQR") {
    outlierIndexes <- which(data$intervalLogL < median(data$intervalLogL) -
                              1.5 * IQR(data$intervalLogL))

    if (length(outlierIndexes > 0))
      data$label[outlierIndexes] <- paste(data$sender, 
                                          data$receiver, sep = "-")[outlierIndexes]
  }
  
  data$time <- as.POSIXct(data$time)

  ggplot2::ggplot(data, ggplot2::aes(x = time, y = intervalLogL)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = label)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") +
    ggplot2::ylab("Interval log likelihood")
}

# Examine change point
#' @param moment character argument to choose between "mean" or "variance".
#' See section \emph{Change point} for details.
#' @param method Choice of \code{"AMOC"}, \code{"PELT"} or \code{"BinSeg"}.
#' For a detail description see \code{\link[changepoint]{cpt.mean}} or
#' \code{\link[changepoint]{cpt.var}}. The default value is \code{"PELT"}.
#' @param minseglen Positive integer giving the minimum segment length
#' (no. of observations between changes), default is 3.
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
#' Also it prints a table of the change points events that are returned by the
#' method.
#' @importFrom changepoint cpt.mean cpt.var
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal xlab ylab
#'  geom_vline scale_x_continuous theme element_text
#' @export
#' @rdname examine
examineChangepoints <- function(x, moment = c("mean", "variance"),
                                 method = c("PELT", "AMOC", "BinSeg"),
                                 minseglen = 3,
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
  if(length(data$time != x$intervalLogL)){
    calls <- as.list(x$call)
    calls[[1]] <- NULL
    calls$preprocessingOnly <- TRUE
    calls$preprocessingInit <- NULL
    calls$silent <- TRUE
    calls$debug <- FALSE
    calls$verbose <- FALSE
    prep <- suppressWarnings(do.call(estimate, calls))
    data$intervalLogL <- x$intervalLogL[prep$orderEvents==1]
  } else data$intervalLogL <- x$intervalLogL

  if (moment == "mean") {
    cpt <- changepoint::cpt.mean(x$intervalLogL, 
                                 method = method, minseglen = minseglen, ...)
  }
  if (moment == "variance") {
    cpt <- changepoint::cpt.var(x$intervalLogL, 
                                method = method, minseglen = minseglen, ...)
  }

  cpt.pts <- attributes(cpt)$cpts
  cpt.mean <- attributes(cpt)$param.est$mean

  data$time <- as.POSIXct(data$time)

  ggplot2::ggplot(data, ggplot2::aes(x = time, y = intervalLogL)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = data$time[cpt.pts], color = "red") +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") +
    ggplot2::ylab("Interval log likelihood") +
    ggplot2::scale_x_continuous(breaks = data$time[cpt.pts], labels = data$time[cpt.pts]) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

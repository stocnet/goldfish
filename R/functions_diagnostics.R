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
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls,
#'                           nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors,
#'                                         defaultNetwork = callNetwork)
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'                   model = "DyNAM", subModel = "choice",
#'                   estimationInit = list(returnIntervalLogL = TRUE))
#'
#' examine.outliers(mod01)
#'
#' examine.changepoints(mod01)
#' }
NULL

# Examine outlier cases
#' @param outliers the number of outliers to report
#' @section Outliers:
#' \code{examine.outliers} creates a plot with the log-likelihood of the events
#' in the y-axis and the event index in the x-axis, identifying observations
#' not well-fitted by the model with a small red circle,
#' and prints a table of the top `outliers` events.
#' @importFrom graphics points
#' @export
#' @rdname examine
examine.outliers <- function(x, outliers = 10) {
  if (!"result.goldfish" %in% attr(x, "class"))
    stop("Not a goldfish results object.")
  if (is.null(x$intervalLogL))
    stop("Outlier identification only available when interval log likelihood",
         " returned in results object.")

  plot(x$intervalLogL, type = "l", lwd = 2, xlab = "Event index",
       ylab = "Interval log likelihood")
  outlierIndexes <- order(x$intervalLogL)[1:outliers]
  points(x = outlierIndexes, y = x$intervalLogL[outlierIndexes], col = "red",
         cex = 3, lwd = 3)

  dv <- get(strsplit(as.character(x$formula), " ~ ")[[2]][1])
  dv[outlierIndexes, ]
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
#' Also it prints a table of the change points events that are returned by the
#' method.
#' @importFrom changepoint cpt.mean cpt.var
#' @export
#' @rdname examine
examine.changepoints <- function(x, moment = c("mean", "variance"),
                                 method = c("PELT", "AMOC", "BinSeg"),
                                 ...) {

  if (!requireNamespace("changepoint", quietly = TRUE))
    stop("Package \"changepoint\" needed for this function to work. ",
         "Please install it.",
         call. = FALSE)

  if (!methods::is(x, "result.goldfish"))
    stop("Not a goldfish results object.", call. = FALSE)
  if (is.null(x$intervalLogL))
    stop("Outlier identification only available when interval log likelihood",
         " returned in results object.")

  moment <- match.arg(moment)
  method <- match.arg(method)

  if (moment == "mean")
    cpt <- changepoint::cpt.mean(x$intervalLogL, method = method, ...)
  if (moment == "variance")
    cpt <- changepoint::cpt.var(x$intervalLogL, method = method, ...)

    changepoint::plot(
      cpt, type = "l", lwd = 2,
      xlab = "Event index", ylab = "Interval log likelihood",
      cpt.width = 3)

  if (length(cpt@cpts) > 1) {
    dv <- get(strsplit(as.character(x$formula), " ~ ")[[2]][1])
    dv[cpt@cpts[-length(cpt@cpts)], ]
  }
}

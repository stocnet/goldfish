#' Examine outlier cases
#' @param x a goldfish results object
#' @param outliers the number of outliers to report
#' @return Plots the log-likelihood, identifying observations not well-fitted by the model with a small red circle,
#' and prints a table of the top `outliers`
#' @export
examine.outliers <- function(x, outliers = 10) {
  if (!"result.goldfish" %in% attr(x, "class")) stop("Not a goldfish results object.")
  if (is.null(x$interval.logL))
    stop("Outlier identification only available when interval log likelihood returned in results object.")

  plot(x$interval.logL, type = "l", lwd = 2, xlab = "Event index", ylab = "Interval log likelihood")
  outlierIndexes <- order(x$interval.logL)[1:outliers]
  points(x = outlierIndexes, y = x$interval.logL[outlierIndexes], col = "red", cex = 3, lwd = 3)

  dv <- get(strsplit(as.character(x$formula), " ~ ")[[2]][1])
  dv[outlierIndexes, ]
}

#' Examine
#' @param x a goldfish results object
#' @param moment argument passed on to `changepoint` package, either "mean" or "variance"
#' @param method additional arguments to be passed to or from other functions
#' @return Log likelihood of the model or the average log likelihood per event
#' @importFrom changepoint cpt.mean cpt.var
#' @export
examine.changepoints <- function(x, moment = "mean", method = "PELT") {

  if (!requireNamespace("changepoint", quietly = TRUE))
    stop("Package \"changepoint\" needed for this function to work. Please install it.",
         call. = FALSE)

  if (!methods::is(x, "result.goldfish")) stop("Not a goldfish results object.", call. = FALSE)
  if (is.null(x$interval.logL))
    stop("Outlier identification only available when interval log likelihood returned in results object.")
  if (method == "SegNeigh") stop("Does not currently work with 'SegNeigh' method")

  if (moment == "mean") {
    cpt <- changepoint::cpt.mean(x$interval.logL, method = method)
    plot(cpt, type = "l", lwd = 2, xlab = "Event index", ylab = "Interval log likelihood", cpt.width = 3)
  }
  if (moment == "variance") {
    cpt <- changepoint::cpt.var(x$interval.logL, method = method)
    plot(cpt, type = "l", lwd = 2, xlab = "Event index", ylab = "Interval log likelihood", cpt.width = 3)
  }

  if (length(cpt@cpts) > 1) {
    dv <- get(strsplit(as.character(x$formula), " ~ ")[[2]][1])
    dv[cpt@cpts[-length(cpt@cpts)], ]
  }
}

######################### ###
#
# Print, summary and xtable functions
# for classes in Goldfish
#
######################### ###


#' Extract coefs from result goldfish object
#'
#' Return a named vector with the estimated coefficients.
#' The names just correspond to the effect name. For a comprehensive output use
#' \code{\link{summary.result.goldfish}}.
#' @param object an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call.
#' @param fixed logical. Indicates whether the parameter coefficients of effects
#' fixed during estimation using `fixedParameters` should be printed.
#' @param ... additional arguments to be passed.
#' @method coef result.goldfish
#' @export
#' @return A named vector.
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
#'                   model = "DyNAM", subModel = "choice")
#' coef(mod01)
#' }
coef.result.goldfish <- function(object, ..., fixed = TRUE) {
  result <- object$parameters
  names(result) <- rownames(object$names)
  if (!fixed && "fixed" %in% colnames(object$names)) {
    fixed <- vapply(object$names[, "fixed"],
                    function(x) eval(parse(text = x)), logical(1))
    result <- result[!fixed]
  }
  result
}


#' Information criteria statistics
#' @param object an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call.
#' @param ... additional arguments to be passed
#' @return AIC, BIC, log likelihood of the model or the average
#'   log likelihood per event.
#' @name model-selection
NULL

# Calculate AIC of Goldfish results
# @param object a goldfish results object
# @param ... additional arguments to be passed to or from other functions
#' @param k Penalty factor
#' @importFrom stats AIC BIC qnorm
# @return AIC of a model
#' @export
#' @rdname model-selection
#' @method AIC result.goldfish
AIC.result.goldfish <- function(object, ..., k = 2) {
  if (k != 2) warning("implemented only for k = 2")

  aic <- -2 * object$logLikelihood + 2 * object$nParams
  return(aic)
  # aicc <- aic + 2*nParams*(nParams + 1)/(result$n.events - nParams - 1)
}

# Calculate BIC of Goldfish results
# @param object a goldfish results object
# @param ... additional arguments to be passed to or from other functions
# @return AIC of a model
#' @export
#' @rdname model-selection
#' @method BIC result.goldfish
BIC.result.goldfish <- function(object, ...) {
  bic <- -2 * object$logLikelihood + object$nParams * log(object$nEvents)
  return(bic)
}

# Calculate log likelihood of Goldfish results
# @param object a goldfish results object
#' @param avgPerEvent a boolean indicating whether the average
#' likelihood per event should be calculated
# @param ... additional arguments to be passed to or from other functions
# @return Log likelihood of the model or the average log likelihood per event
#' @export
#' @rdname model-selection
#' @method logLik result.goldfish
logLik.result.goldfish <- function(object, ..., avgPerEvent = FALSE) {
  if (avgPerEvent) {
    return(object$logLikelihood / object$nEvents)
  }
  return(object$logLikelihood)
}

#' Printing functions for `goldfish` objects.
#' @param x an object of class \code{result.goldfish},
#' \code{summary.result.goldfish}, \code{nodes.goldfish},
#' \code{network.goldfish}, \code{dependent.goldfish}, or
#' \code{preprocessed.goldfish}.
#' @param object an object of class \code{result.goldfish}.
#' @param digits minimal number of significant digits,
#'   see \code{\link{print.default}}.
#' @param width only used when \code{max.levels} is \code{NULL},
#'   see  \code{\link{print}}.
#' @param fixed logical. Indicates whether the parameter coefficients of effects
#' fixed during estimation using `fixedParameters` should be printed.
#' The default value is \code{TRUE}. \emph{Note:} applies for objects of class
#' \code{result.goldfish} and \code{summary.result.goldfish}.
#' @param full logical. Indicates whether the complete
#' \code{matrix}/\code{data.frame} should be printed.
#' The default value \code{FALSE}.
#' @param ... further arguments to be passed to the respective \code{default}
#' method.
#' @name print-method
NULL

# Print Goldfish results
# @return prints just the coefficients of the estimated model.
#   See \code{\link{print.summary.result.goldfish}} for a more
#   comprehensible output.
#' @importFrom stats coef
#' @export
#' @rdname print-method
#' @method print result.goldfish
print.result.goldfish <- function(
  x, ..., digits = max(3, getOption("digits") - 2),
  width = getOption("width")) {
  cat("\nCall:\n")
  print(x$call)
  cat("\n\n")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits),
                  print.gap = 2, quote = FALSE, ...)
  } else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}

#' @method summary result.goldfish
#' @export
#' @rdname print-method
summary.result.goldfish <- function(object, ...) {
  nParams <- object$nParams

  if (is.null(object$names)) object$names <- seq_len(nParams)
  names <- object$names
  est <- object$parameters
  std.err <- object$standardErrors
  z <- est / std.err
  p <- 2 * (1 - pnorm(abs(z)))
  # sig <- rep("", nparams)
  # sig[abs(z) > qnorm(1 - 0.05 / 2)] <- "*"
  # sig[abs(z) > qnorm(1 - 0.01 / 2)] <- "**"
  # sig[abs(z) > qnorm(1 - 0.001 / 2)] <- "***"

  # signif <- symnum(pv, corr = false, na = false,
  #                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  #                  symbols = c("***", "**", "*",
  #                              ".", " "))
  #
  coefmat <- cbind(est, std.err, z, p)
  dimnames(coefmat) <- list(
    rownames(object$names),
    c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  )

  object$coefMat <- coefmat
  object$AIC <- AIC.result.goldfish(object)
  object$BIC <- BIC.result.goldfish(object)
  class(object) <- "summary.result.goldfish"
  return(object)
  # printCoefmat()

  # aic <- -2 * object$log.likelihood + 2 * nParams
  # bic <- -2 * object$log.likelihood + nParams * log(object$n.events)
  #
  # format.pval()
  #
  # cat(" ", paste("Log likelihood", round(object$log.likelihood, 4), "\n"))

}

#' @export
#' @importFrom stats printCoefmat pnorm qnorm
#' @rdname print-method
print.summary.result.goldfish <- function(
  x, ...,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"), fixed = FALSE) {

  nParams <- x$nParams
  aicc <- x$AIC + 2 * nParams * (nParams + 1) / (x$nEvents - nParams - 1)
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  # cat("Frequencies of alternatives:")
  # print(prop.table(x$freq), digits = digits)
  # cat("\n")
  # print(x$est.stat)


  isFixed <- GetFixed(x)

  if (!fixed && any(isFixed)) {
    names <- x$names[!isFixed, ]
    coefMat <- x$coefMat[!isFixed, ]
    isDetPrint <- !((ncol(names) == 2) &&
                      (length(unique(names[, "Object"])) == 1))
  } else {
    names <- x$names
    coefMat <- x$coefMat
    isDetPrint <- !((ncol(names) == 1) &&
                      (length(unique(names[, "Object"])) == 1))
  }

  if (isDetPrint) {
    cat("\nEffects details :\n")
    print.default(names, quote = FALSE)
  }

  cat("\nCoefficients :\n")
  stats::printCoefmat(coefMat, digits = digits)
  cat("\n")
  cat(" ", paste(
    ifelse(x$convergence$isConverged, "Converged", "Not converged"),
    "with max abs. score of",
    round(x$convergence$maxAbsScore, digits)
  ), "\n")
  cat(" ",
      paste("Log-Likelihood: ", signif(x$logLikelihood, digits),
            "\n", sep = ""))
  cat(" ",
    "AIC: ", signif(x$AIC, digits),
    "\n  AICc:", signif(aicc, digits),
    "\n  BIC: ", signif(x$BIC, digits), "\n")
  cat("  model:", dQuote(x$model), "subModel:", dQuote(x$subModel), "\n")
  invisible(x)
}

# print nodes.goldfish object
# @param x a nodes.goldfish object
#' @export
#' @method print nodes.goldfish
#' @rdname print-method
# @examples print(structure(data.frame(label = 1:5),
#                 class = c("nodes.goldfish", "data.frame")))
print.nodes.goldfish <- function(x, ..., full = FALSE, n = 6) {
  events <- attr(x, "events")
  dynamicAttr <- attr(x, "dynamicAttributes")
  cat("Number of nodes:", nrow(x), "\n")
  if ("present" %in% names(x))
    cat("Number of present nodes:", sum(x$present), "\n")
  if (!is.null(events) && any(events != "")) {
    title <- c("Dynamic attribute(s):", "Linked events")
    mxName <- max(nchar(dynamicAttr), nchar(title[1])) + 4
    cat(title[1], strrep(" ", mxName - nchar(title[1])), title[2],
        "\n", sep = "")
    lapply(
      seq(length(events)),
      function(x) {
        cat(strrep(" ", 2), dynamicAttr[x],
            strrep(" ", mxName - nchar(dynamicAttr[x]) - 2), events[x], "\n")
      })
  }

  cat("\n")
  attributes(x)[c("events", "dynamicAttributes")] <- NULL
  class(x) <- "data.frame"
  # x <- as.data.frame(x)
  if (full) {
    print((x), ...)
  } else {
    cat("First", min(nrow(x), n), "rows\n")
    print(head(x, min(nrow(x), n)), ...)
  }
  invisible(NULL)
}

#' @export
#' @importFrom utils head
#' @rdname print-method
head.nodes.goldfish <- function(x, ..., n = 6L) {
  attributes(x)[c("events", "dynamicAttributes")] <- NULL
  class(x) <- "data.frame"
  print(head(x, min(n, nrow(x)), ...))
  invisible(NULL)
}

#' @inheritParams utils::tail
#' @export
#' @importFrom utils tail
#' @rdname print-method
tail.nodes.goldfish <- function(x, ..., n = 6L,
                                keepnums = FALSE, addrownums = FALSE) {
  attributes(x)[c("events", "dynamicAttributes")] <- NULL
  class(x) <- "data.frame"
  if (R.version$major >= "4") {
    print(tail(x, min(n, nrow(x)), keepnums = keepnums))
  } else {
    print(tail(x, min(n, nrow(x)), addrownums = addrownums))
  }
  invisible(NULL)
}


# stylize print network.goldfish object
#
# @param x a network.goldfish object to print
#' @param n number of rows for \code{data.frame}, and rows and columns for
#' \code{matrix} to be printed.
#' @export
#' @rdname print-method
# @examples print(structure(rep(0, 100), dim = c(10, 10),
#                 class = "network.goldfish"))
print.network.goldfish <- function(x, ..., full = FALSE, n = 6L) {
  nodes <- attr(x, "nodes")
  directed <- attr(x, "directed")
  ties <- if (directed) sum(x > 0) else sum(x > 0) / 2
  events <- attr(x, "events")
  cat("Dimensions:", paste(dim(x), collapse = " "),
      "\nNumber of ties (no weighted):", ties,
       "\nNodes set(s):", paste(nodes, collapse = " "),
       "\nIt is a", ifelse(length(nodes) == 2, "two-mode", "one-mode"),
       "and", ifelse(directed, "directed", "undirected"), "network\n")

  if (!is.null(events) && any(events != ""))
    cat("Linked events:", paste(events, collapse = ", "), "\n")

  cat("\n")
  attributes(x)[c("class", "events", "nodes", "directed")] <- NULL
  if (full) {
    print(x)
  } else {
    cat("First", min(c(dim(x), n)), "rows and columns\n")
    if (R.version$major >= "4") {
      print(head(x, c(min(c(nrow(x), n)), min(c(ncol(x), n)))))
    } else {
      print(head(x[, seq(min(c(nrow(x), n)))], min(c(ncol(x), n))))
    }
  }
  invisible(NULL)
}

#' @export
#' @importFrom utils head
#' @rdname print-method
head.network.goldfish <- function(x, ..., n = 6L) {
  attributes(x)[c("class", "events", "nodes", "directed")] <- NULL
  if (R.version$major >= "4") {
    print(head(x, c(min(c(nrow(x), n)), min(c(ncol(x), n)))))
  } else {
    print(head(x[, seq(min(c(nrow(x), n)))], min(c(ncol(x), n))))
  }
  invisible(NULL)
}

#' @export
#' @importFrom utils tail
#' @rdname print-method
tail.network.goldfish <- function(x, ..., n = 6L,
                                  keepnums = TRUE, addrownums = TRUE) {
  attributes(x)[c("class", "events", "nodes", "directed")] <- NULL
  if (R.version$major >= "4") {
    print(tail(x, c(min(c(nrow(x), n)), min(c(ncol(x), n))),
               keepnums = keepnums))
  } else {
    print(tail(x[, seq(ncol(x) - min(c(nrow(x), n)), ncol(x))],
               min(c(ncol(x), n)), addrownums = addrownums))
  }
  invisible(NULL)
}

# print dependent.goldfish object
#
# @param x a dependent.goldfish object
#' @export
#' @rdname print-method
#
# @examples
# print(
#  structure(
#    data.frame(sender = 1:5, receiver = 2:6, time = 1:5, replace = rep(1, 5)),
#    class = c("nodes.goldfish", "data.frame"), nodes = "nodes", defaultNetwork = "network"
#  )
# )
print.dependent.goldfish <- function(x, ..., full = FALSE, n = 6) {
  nodes <- attr(x, "nodes")
  defaultNetwork <- attr(x, "defaultNetwork")
  cat("Number of events:", nrow(x),
      "\nNodes set(s):", paste(nodes, collapse = " "), "\n")
  if (!is.null(defaultNetwork) && defaultNetwork != "")
    cat("Default network:", defaultNetwork, "\n")

  cat("\n")
  attributes(x)[c("nodes", "defaultNetwork", "type")] <- NULL
  class(x) <- "data.frame"
  # x <- as.data.frame(x)
  if (full) {
    print((x))
  } else {
    cat("First", min(nrow(x), n), "rows\n")
    print(head(x, min(nrow(x), n)))
  }
  invisible(NULL)
}

#' @export
#' @importFrom utils head
#' @rdname print-method
head.dependent.goldfish <- function(x, ..., n = 6L) {
  attributes(x)[c("nodes", "defaultNetwork", "type")] <- NULL
  class(x) <- "data.frame"
  print(head(x, min(nrow(x), n)))
  invisible(NULL)
}

#' @export
#' @importFrom utils tail
tail.dependent.goldfish <- function(x, ..., n = 6L,
                                    keepnums = FALSE, addrownums = FALSE) {
  attributes(x)[c("nodes", "defaultNetwork", "type")] <- NULL
  class(x) <- "data.frame"
  if (R.version$major >= "4") {
    print(tail(x, min(nrow(x), n), keepnums = keepnums))
  } else {
    print(tail(x, min(nrow(x), n), addrownums = addrownums))
  }
  invisible(NULL)
}


# print preprocessed.goldfish
#
# @param x a preprocessed.goldfish object
#' @export
#' @rdname print-method
#
# @examples print(structure(list(formula = dep ~ inertia, dependentStatistics = numeric(20)),
# class = "preprocessed.goldfish"))
print.preprocessed.goldfish <- function(x, ..., width = getOption("width")) {
  cat("**Preprocess object for the model:**\n")
  print(x$formula)
  cat(" dependent events processed: ", length(x$dependentStatsChange), "\n")
  # cat(" Model type:", result$model.type, "\n")
  cat("*The results are available in the following objects:*\n\n")

  description <- data.frame(
    name = c("initialStats", "dependentStatsChange", "rightCensoredStatsChange",
             "intervals", "rightCensoredIntervals", "orderEvents", "eventTime",
             "eventSender", "eventReceiver", "startTime", "endTime", "formula",
             "nodes", "nodes2"),
    description =
      c("Initial statistical matrices for the effects given previous history.",
        "List: For each dependent event, a list with the change statistics for the given\n state of the process.",
        "List: dependent change statistics for a given right-censored event.",
        "Elapsed time between events.",
        "List: updates statistics during the elapsed time between events.",
        "Order of events.",
        "Time of the event.",
        "Event sender.",
        "Event receiver.",
        "Numeric time value of the initial time considered during estimation.",
        "Numeric time value of the final time considered during estimation.",
        "Formula of the model to estimate.",
        "A character with the name of the object of class nodes.goldfish (rows/first-mode)",
        "A character with the name of the object of class nodes.goldfish (cols/second-mode)"),
    stringsAsFactors = FALSE
  )

  description$name <- paste0("$", description$name)
  mxName <- max(nchar(description$name)) + 3

  lapply(
    seq_len(nrow(description)),
    function(x) {
      cat(description[x, 1])
      wrap <- strwrap(description[x, 2], width = width)
      wrap[1] <- paste(strrep(" ", mxName - nchar(description[x, 1])), wrap[1])
      wrap <- paste(wrap, collapse = paste0("\n", strrep(" ", mxName + 1)))
      cat(wrap)
      cat("\n")
    })

  invisible(NULL)
}

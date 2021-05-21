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
  # TODO check events / parameter ratio ad adjust k
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

# plot.nodes.goldfish_ <- function(x) {
#   if (is.null(goldfish:::findPresence(x))) stop("No composition change")
#   chan <- get(goldfish:::findPresence(x))
#   chan$time <- as.Date.character(chan$time)
#   plotbeg <- min(chan$time)
#   plotend <- max(chan$time)
#   chan <- chan[!duplicated(chan$node), ]
#
#   require(ggplot2)
#   require(scales)
#   plotit <- x[order(x$present, decreasing = TRUE), ]
#   plotit$beg[plotit$present] <- as.character(plotbeg)
#   plotit[match(chan$node, plotit$label), "beg"] <- as.character(chan$time)
#   plotit$end <- as.Date.character(plotend)
#   plotit <- plotit[order(plotit$beg), ]
#
#   ggplot(plotit, aes(x = label, ymin = as.Date(beg), ymax = as.Date(end))) +
#     geom_linerange() + theme_classic() + coord_flip() +
#     scale_y_date(
#       limits = c(as.Date(plotbeg), as.Date(plotend)),
#       breaks = date_breaks("1 years")
#     ) +
#     scale_x_discrete(limits = plotit$label[!duplicated(plotit$label)]) +
#     theme(
#       axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
#       axis.text.y = element_blank(), axis.title.y = element_blank(),
#       axis.ticks.y = element_blank()
#     )
# }

# plot.dependent.goldfish_ <- function(x) {
#   if (is.null(attr(x, "defaultNetwork"))) stop("No default network")
#   if (is.null(attr(x, "nodes"))) stop("No nodes")
#   # nodes <- get(attr(x, "nodes")[1])
#   # if(is.null(goldfish:::findPresence(nodes))) stop("No composition change")
#   # chan <- get(goldfish:::findPresence(nodes))
#
#   require(reReg)
#   x <- x[order(x$sender), ]
#   # y <- attr(x, "nodes")
#   plotEvents(reSurv(
#     time1 = as.numeric(x$time) - min(as.numeric(x$time)),
#     time2 = max(as.numeric(x$time)) - min(as.numeric(x$time)),
#     id = x$sender,
#     event = (duplicated(x$sender, fromLast = TRUE)) * 1,
#     status = rep(0, nrow(x))
#   ))
# }


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

## TODO adapt to new simplified network objects
# print.attribute.goldfish_ <- function(x) {
#   cat("Type of attribute:", paste(x$type, collapse = " "), "\n")
#   cat("Dimensions:", paste(length(x$data), collapse = " "), "\n")
# }
#
# print.elements.goldfish <- function(x) {
#   # Check the number of networks and attributes
#   n.networks <- 0
#   n.attributes <- 0
#   if (!is.null(x)) {
#     for (n in seq_along(x)) {
#       if (inherits(x[[n]], "network.goldfish")) n.networks <- n.networks + 1
#       if (inherits(x[[n]], "attribute.goldfish")) n.attributes <- n.attributes + 1
#     }
#   }
#   # Print networks
#   cat("\nNetwork elements: \n")
#   if (n.networks == 0) {
#     cat("No network added yet.")
#   } else {
#     for (n in seq_along(x)) {
#       if (inherits(x[[n]], "network.goldfish") && !x[[n]]$isWindow) {
#         cat("\n\t", names(x)[n], "\n", sep = "")
#         print(x[[n]])
#         indexes <- which(grepl(paste(names(x)[n], ".", sep = ""), names(x)))
#         if (length(indexes) > 0)
#           cat("Windows:",
#               paste(substr(names(x)[indexes], nchar(names(x)[n]) + 2, nchar(names(x)[indexes])), "s", sep = ""),
#               "\n")
#       }
#     }
#   }
#   # Print attributes
#   cat("\nAttribute elements: \n")
#   if (n.attributes == 0) {
#     cat("No attribute added yet.")
#   } else {
#     for (n in seq_along(x)) {
#       if (inherits(x[[n]], "attribute.goldfish") && !x[[n]]$isWindow) {
#         cat("\n\t", names(x)[n], "\n", sep = "")
#         print(x[[n]])
#         indexes <- which(grepl(paste(names(x)[n], ".", sep = ""), names(x)))
#         if (length(indexes) > 0)
#           cat("Windows:",
#               paste(substr(names(x)[indexes], nchar(names(x)[n]) + 2, nchar(names(x)[indexes])), "s", sep = ""),
#               "\n")
#       }
#     }
#   }
# }

# print.eventList.goldfish_ <- function(x, head = TRUE, n = 3) {
#   cat("Dimensions:", paste(dim(x$data), collapse = " "), "\nProcess state element:", x$processStateElement, "\n")
#   if (head) print(head(x$data, n = n))
# }

# print.events.goldfish <- function(x) {
#   # if(is.data.frame(x)) return()
#   if (length(x) == 0) {
#     cat("No events added yet.")
#   } else {
#     for (n in seq_along(x)) {
#       if (!x[[n]]$isWindow) {
#         cat(paste("\n\t", names(x)[n], "\n"))
#         print(x[[n]])
#         indexes <- which(grepl(paste(names(x)[n], ".", sep = ""), names(x)))
#         if (length(indexes) > 0) {
#           cat("Windows: ")
#           cat(paste(substr(names(x)[indexes], nchar(names(x)[n]) + 2, nchar(names(x)[indexes])), "s", sep = ""))
#           cat(" \n")
#         }
#       }
#     }
#   }
# }

# print.data.goldfish <- function(x) {
#
#   # Print nodesets
#   cat("Nodesets: \n\n")
#   print(x$nodes)
#
#   # Print networks and attributes
#   print(x$elements)
#
#   # Print events
#   cat("\nEvents: \n")
#   print(x$events)
# }

# print.effects.goldfish <- function(x) {
#
#   # Print data object associated
#   cat(paste("Dependent event list: ", x$depv))
#
#   # Print data object associated
#   cat("\nEffects:\n")
#   if (length(x$effects) == 0) {
#     cat("No effect added yet")
#   } else {
#     tab <- data.frame(x$effects, row.names = NULL)
#     colnames(tab) <- c("effect", "weighted", "window")
#     print(tab)
#   }
# }

# Function to prettily print list of goldfish results
# with some defaults (needs work/extension)
# @importFrom knitr kable
# print.list <- function(x, substitute = NULL, dependents = NULL) {
#   stopifnot(inherits(x, "list"))
#   if (all(lapply(x, inherits, what = "result.goldfish"))) {
#     # Get types
#     depVars <- unlist(lapply(x, function(y) as.character(y$formula)[2]))
#     depVarTitles <- as.character(c(1, rep(3, length(unique(depVars)))))
#     if (is.null(dependents)) {
#       names(depVarTitles) <- c(" ", paste0(unique(depVars), "[note]"))
#     } else {
#       names(depVarTitles) <- c(" ", paste0(dependents, "[note]"))
#     }
#     depMods <- if_else(grepl(
#       "Rate",
#       unlist(lapply(x, function(y) as.character(y$model.type)))
#     ),
#     "Rate", "Choice"
#     )
#
#     # Get lengths
#     nParams <- unlist(lapply(x, function(y) length(y$parameters)))
#     maxRate <- max(nParams[depMods == "Rate"])
#     maxChoi <- max(nParams[depMods == "Choice"])
#
#     # Diagnostics
#     nevs <- unlist(lapply(x, function(y) y$n.events))
#     logl <- unlist(lapply(x, function(y) round(y$log.likelihood, 2)))
#     aics <- unlist(lapply(x, function(y) round(-2 * y$log.likelihood + 2 * length(y$parameters), 2)))
#     #   aicc <- aic + 2*nParams*(nParams + 1)/(result$n.events - nParams - 1)
#     bics <- unlist(lapply(x, function(y) round(-2 * y$log.likelihood + length(y$parameters) * log(y$n.events), 2)))
#     diags <- unlist(lapply(unique(depVars), function(y) {
#       paste0(
#         "N = ", paste(nevs[depVars == y], collapse = " and "),
#         ". Log Likelihood = ", paste(logl[depVars == y], collapse = " and "),
#         ". AIC = ", paste(aics[depVars == y], collapse = " and "),
#         ". BIC = ", paste(bics[depVars == y], collapse = " and "),
#         "."
#       )
#     }))
#
#     # Extract and format key results
#     cleanRes <- lapply(x, function(y) {
#       data.frame(
#         Effect = apply(as.data.frame(y$names), 1, paste, collapse = " "),
#         Est. = round(y$parameters, 3),
#         S.E. = format(round(y$standard.errors, 3), digits = 3),
#         t = y$parameters / y$standard.errors,
#         stringsAsFactors = F
#       ) %>%
#         mutate(S.E. = paste0("(", if_else(S.E. == "0.000", "< 0.001", as.character(S.E.)), ")")) %>%
#         mutate(t = if_else(abs(t) > qnorm(1 - 0.001 / 2), "***",
#           if_else(abs(t) > qnorm(1 - 0.01 / 2), "**",
#             if_else(abs(t) > qnorm(1 - 0.05 / 2), "*", "")
#           )
#         ))
#     })
#
#     # Renaming
#     if (!is.null(substitute)) {
#       cleanRes <- lapply(cleanRes, function(y) {
#         y$Effect <- y$Effect %>% str_replace_all(fixed(substitute))
#         y
#       })
#     }
#     cleanRes <- lapply(cleanRes, function(y) {
#       y <- mutate(y,
#         Effect = if_else(str_detect(Effect, "inertia"), "Inertia", Effect),
#         Effect = str_replace_all(
#           Effect,
#           fixed(c(
#             "ego " = "Ego's ",
#             "alter " = "Alter's ",
#             "diff " = "Different "
#           ))
#         ),
#         Effect = if_else(str_detect(Effect, "out"), paste(str_remove(Effect, "out "), "Outdegree"), Effect),
#         Effect = if_else(str_detect(Effect, "indeg"), paste(str_remove(Effect, "indeg "), "Indegree"), Effect),
#         Effect = if_else(str_detect(Effect, "recip"), paste(str_remove(Effect, "recip "), "Reciprocity"), Effect),
#         Effect = if_else(str_detect(Effect, "trans"), paste(str_remove(Effect, "trans "), "Transitivity"), Effect),
#         Effect = str_remove(Effect, "tie ")
#       )
#     })
#
#     # Combine
#     cleanRes <- lapply(unique(depVars), function(d) bind_rows(cleanRes[depVars == d])) %>%
#       reduce(full_join, by = "Effect")
#     cleanRes[is.na(cleanRes)] <- ""
#
#
#     kableligns <- c("l", rep(c("r", "r", "l"), length(depVarTitles) - 1))
#     kablenames <- c("Effect", rep(c("Est.", "S.E.", ""), length(depVarTitles) - 1))
#
#     kable(cleanRes,
#       booktabs = TRUE, caption = "Results",
#       align = kableligns, col.names = kablenames
#     ) %>%
#       add_header_above(header = depVarTitles) %>%
#       add_footnote(diags, notation = "symbol", threeparttable = TRUE) %>%
#       kable_styling(latex_options = "hold_position") %>%
#       pack_rows("Rate", 1, maxRate) %>%
#       pack_rows("Choice", maxRate + 1, maxRate + maxChoi + 1)
#   }
# }


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

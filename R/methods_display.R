######################### ###
#
# Print and summary functions
# for classes in Goldfish
#
######################### ###



#' Methods for `goldfish` objects.
#'
#' Printing functions for `goldfish` objects.
#'
#' @param x an object of class `result.goldfish`, `summary.result.goldfish`,
#' `nodes.goldfish`, `network.goldfish`, `dependent.goldfish`, or
#' `preprocessed.goldfish`.
#' @param digits minimal number of significant digits, see [print.default()].
#' @param width controls the maximum number of columns on a line used in
#' printing `summary.result.goldfish` and `preprocessed.goldfish`,
#' see  [print.default()].
#' @param complete logical. Indicates whether the parameter coefficients
#' of effects fixed during estimation using `fixedParameters` should be printed.
#' The default value is `FALSE`. _Note:_ applies for objects of class
#' `result.goldfish` and `summary.result.goldfish`.
#' @param full logical. Indicates whether the complete `matrix`/`data.frame`
#' should be printed. The default value `FALSE`.
#' @param ... further arguments to be passed to the respective `default`
#' method.
#' @name print-method
#' @return Not value, called for printing side effect.
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
    width = getOption("width"), complete = FALSE) {
  cat("\nCall:\n")
  print(x$call)
  cat("\n\n")
  if (length(coef(x, complete = complete))) {
    cat("Coefficients:\n")
    print.default(format(coef(x, complete = complete), digits = digits),
      print.gap = 2, quote = FALSE, width = width, ...
    )
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}

#' @method summary result.goldfish
#' @export
#' @noRd
summary.result.goldfish <- function(object, ...) {
  nParams <- object$nParams

  if (is.null(object$names)) object$names <- seq_len(nParams)
  # names <- object$names

  est <- object$parameters
  std.err <- object$standardErrors
  z <- est / std.err
  p <- 2 * (1 - stats::pnorm(abs(z)))

  isFixed <- GetFixed(object)

  if (any(isFixed)) {
    std.err[isFixed] <- NA_real_
    z[isFixed] <- NA_real_
    p[isFixed] <- NA_real_
  }
  # sig <- rep("", nparams)
  # sig[abs(z) > stats::qnorm(1 - 0.05 / 2)] <- "*"
  # sig[abs(z) > stats::qnorm(1 - 0.01 / 2)] <- "**"
  # sig[abs(z) > stats::qnorm(1 - 0.001 / 2)] <- "***"

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
  object$AIC <- stats::AIC(object)
  object$BIC <- stats::BIC(object)
  class(object) <- "summary.result.goldfish"
  return(object)
  # format.pval()
}

#' @export
#' @rdname print-method
#' @return For objects of class `result.goldfish` and `summary.result.goldfish`
#'  print the estimated coefficients when `complete = FALSE`, otherwise it
#'  includes also the fixed coefficients.
#' For `summary.result.goldfish` print:
#' \item{Effect details:}{a table with additional information of the effects.
#' The information corresponds to the  values of the effects arguments when
#' they are modified and if they where fixed during estimation, see
#' `vignette("goldfishEffects")` for the complete list of arguments, and
#' [estimate()] on how to fix coefficients during estimation.}
#' \item{Coefficients:}{a table with the estimated coefficients, their
#'   approximate standard error obtain from the inverse of the negative Fisher
#'   information matrix, z-value and the p-value of the univariate two-tailed
#'   Wald test to test the hypothesis that the parameter is 0.}
#' \item{Convergence and Information Criteria:}{Information about the
#'   convergence of the iterative Newton-Raphson procedure and the score value
#'   in the last iteration. Information criteria as the AIC, BIC and the AIC
#'   corrected for small sample size AICc are reported.}
#' \item{Model and subModel:}{the values set during estimation.}
print.summary.result.goldfish <- function(
    x, ...,
    digits = max(3, getOption("digits") - 2),
    width = getOption("width"), complete = FALSE) {
  nParams <- x$nParams
  aicc <- x$AIC + 2 * nParams * (nParams + 1) / (x$nEvents - nParams - 1)
  cat("\nCall:\n")
  print(x$call, width = width, ...)
  cat("\n")
  # cat("Frequencies of alternatives:")
  # print(prop.table(x$freq), digits = digits)
  # cat("\n")
  # print(x$est.stat)


  isFixed <- GetFixed(x)

  if (!complete && any(isFixed)) {
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
    cat("\nEffects details:\n")
    print.default(names, quote = FALSE, width = width, ...)
  }

  cat("\nCoefficients:\n")
  stats::printCoefmat(coefMat, digits = digits, width = width, ...)
  cat("\n")
  cat(" ", paste(
    ifelse(x$convergence$isConverged, "Converged", "Not converged"),
    "with max abs. score of",
    round(x$convergence$maxAbsScore, digits)
  ), "\n")
  cat(
    " ",
    paste("Log-Likelihood: ", signif(x$logLikelihood, digits),
      "\n",
      sep = ""
    )
  )
  cat(
    " ",
    "AIC: ", signif(x$AIC, digits),
    "\n  AICc:", signif(aicc, digits),
    "\n  BIC: ", signif(x$BIC, digits), "\n"
  )
  cat("  model:", dQuote(x$model), "subModel:", dQuote(x$subModel), "\n")
  invisible(x)
}

# print nodes.goldfish object
# @param x a nodes.goldfish object
#' @export
#' @method print nodes.goldfish
#' @importFrom utils head
#' @rdname print-method
#' @return For objects of class `nodes.goldfish` print information of the total
#' number of nodes in the object, the number of nodes present at the beginning
#' of preprocessing, a table with the linked attributes with their respective
#' events data frame and a printing of the first rows in the nodes data frame.
#' See [make_nodes()].
# @examples print(structure(data.frame(label = 1:5),
#                 class = c("nodes.goldfish", "data.frame")))
print.nodes.goldfish <- function(x, ..., full = FALSE, n = 6) {
  events <- attr(x, "events")
  dynamicAttr <- attr(x, "dynamic_attributes")
  cat("Number of nodes:", nrow(x), "\n")
  if ("present" %in% names(x)) {
    cat("Number of present nodes:", sum(x$present), "\n")
  }
  if (!is.null(events) && any(events != "")) {
    title <- c("Dynamic attribute(s):", "Linked events")
    mxName <- max(nchar(dynamicAttr), nchar(title[1])) + 4
    cat(title[1], strrep(" ", mxName - nchar(title[1])), title[2],
      "\n",
      sep = ""
    )
    lapply(
      seq_along(events),
      function(x) {
        cat(
          strrep(" ", 2), dynamicAttr[x],
          strrep(" ", mxName - nchar(dynamicAttr[x]) - 2), events[x], "\n"
        )
      }
    )
  }

  cat("\n")
  attributes(x)[c("events", "dynamic_attributes")] <- NULL
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



# stylize print network.goldfish object
#
# @param x a network.goldfish object to print
#' @param n number of rows for \code{data.frame}, and rows and columns for
#' \code{matrix} to be printed.
#' @export
#' @rdname print-method
#' @return For objects of class `network.goldfish` print information of the
#' dimensions of the network, number of ties presented at the beginning of the
#' preprocessing, the nodes data frames linked to it, information about their
#' definition as a one-mode and directed network, linked events data frame to it
#' and a printing of the first rows and columns in the array.
#' See [make_network()].
# @examples print(structure(rep(0, 100), dim = c(10, 10),
#                 class = "network.goldfish"))
print.network.goldfish <- function(x, ..., full = FALSE, n = 6L) {
  nodes <- attr(x, "nodes")
  directed <- attr(x, "directed")
  is_two_mode <- attr(x, "is_two_mode")
  ties <- sum(x > 0, na.rm = TRUE) / ifelse(directed, 1, 2)
  events <- attr(x, "events")
  cat(
    "Dimensions:", paste(dim(x), collapse = " "),
    "\nNumber of ties (no weighted):", ties,
    "\nNodes set(s):", paste(nodes, collapse = " "),
    "\nIt is a", ifelse(is_two_mode, "two-mode", "one-mode"),
    "and", ifelse(directed, "directed", "undirected"), "network\n"
  )

  if (!is.null(events) && any(events != "")) {
    cat("Linked events:", paste(events, collapse = ", "), "\n")
  }

  cat("\n")
  attributes(x)[c("class", "events", "nodes", "directed")] <- NULL
  if (full) {
    print(x)
  } else {
    cat("First", min(c(dim(x), n)), "rows and columns\n")
    if (R.version$major >= "4") {
      print(head(x, c(min(c(nrow(x), n)), min(c(ncol(x), n)))), ...)
    } else {
      print(head(x[, seq(min(c(nrow(x), n)))], min(c(ncol(x), n))), ...)
    }
  }
  invisible(NULL)
}


#' @export
#' @rdname print-method
#' @return For objects of class `dependent.goldfish` print information of the
#'  total number of events in the object, linked nodes set(s),
#'  linked default network
#' and a printing of the first rows in the events data frame.
#' See [make_dependent_events()].
#
# @examples
# print(
#  structure(
#    data.frame(sender = 1:5, receiver = 2:6, time = 1:5, replace = rep(1, 5)),
#    class = c("nodes.goldfish", "data.frame"),
#    nodes = "nodes", default_network = "network"
#  )
# )
print.dependent.goldfish <- function(x, ..., full = FALSE, n = 6) {
  nodes <- attr(x, "nodes")
  default_network <- attr(x, "default_network")
  cat(
    "Number of events:", nrow(x),
    "\nNodes set(s):", paste(nodes, collapse = " "), "\n"
  )
  if (!is.null(default_network) && default_network != "") {
    cat("Default network:", default_network, "\n")
  }

  cat("\n")
  attributes(x)[c("nodes", "default_network", "type")] <- NULL
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
#' @rdname print-method
print.data.goldfish <- function(x, ...) {
  cat("Goldfish Data Environment\n")
  cat("=========================\n\n")

  all_obj_names <- ls(envir = x, all.names = TRUE)
  obj_names <- all_obj_names[!grepl("^\\.", all_obj_names)]

  if (length(obj_names) == 0) {
    cat("Environment is empty.\n")
    invisible(x)
    return()
  }

  nodeset_names <- mget(
    ".nodeset_names",
    envir = x,
    ifnotfound = list(character(0))
  )$.nodeset_names
  events_names <- mget(
    ".events_names",
    envir = x,
    ifnotfound = list(character(0))
  )$.events_names

  nets_obj <- character(0)
  deps_obj <- character(0)
  global_obj <- character(0)

  categorized <- c(events_names)

  for (name in setdiff(obj_names, categorized)) {
    obj <- get(name, envir = x)
    if (inherits(obj, "nodes.goldfish")) {
      nodeset_names <- c(nodeset_names, name)
    } else if (inherits(obj, "network.goldfish")) {
      nets_obj <- c(nets_obj, name)
    } else if (inherits(obj, "dependent.goldfish")) {
      deps_obj <- c(deps_obj, name)
    } else if (inherits(obj, "global.goldfish")) {
      global_obj <- c(global_obj, name)
    }
  }
  
  nodeset_names <- unique(nodeset_names)
  categorized <- c(categorized, nodeset_names, nets_obj, deps_obj, global_obj)
  other_obj <- setdiff(obj_names, categorized)

  if (length(nodeset_names) > 0) {
    cat("--- Nodesets ---\n")
    nodes_summary <- data.frame(
      n = vapply(nodeset_names, \(n) nrow(get(n, envir = x)), integer(1)),
      attributes = vapply(nodeset_names, \(n) {
        obj <- get(n, envir = x)
        ncol(obj) - 1L
      }, integer(1)),
      linked_events = vapply(nodeset_names, \(n) {
        obj <- get(n, envir = x)
        ev <- if (inherits(obj, "nodes.goldfish")) attr(obj, "events") else NULL
        if (is.null(ev) || length(ev) == 0) "" else paste(ev, collapse = ", ")
      }, character(1)),
      row.names = nodeset_names,
      check.names = FALSE
    )
    print(nodes_summary)
    cat("\n")
  }

  if (length(nets_obj) > 0) {
    cat("--- Networks ---\n")
    nets_summary <- data.frame(
      dimensions = vapply(
        nets_obj,
        \(n) paste(dim(get(n, envir = x)), collapse = "x"),
        character(1)
      ),
      nodeset = vapply(
        nets_obj,
        \(n) paste(attr(get(n, envir = x), "nodes"), collapse = ", "),
        character(1)
      ),
      linked_events = vapply(
        nets_obj,
        function(n) {
          ev <- attr(get(n, envir = x), "events")
          if (is.null(ev) || length(ev) == 0) "" else paste(ev, collapse = ", ")
        },
        character(1)
      ),
      directed = vapply(
        nets_obj,
        \(n) attr(get(n, envir = x), "directed"),
        logical(1)
      ),
      two_mode = vapply(
        nets_obj,
        function(n) {
          is_two_mode <- attr(get(n, envir = x), "is_two_mode")
          if (is.null(is_two_mode)) FALSE else is_two_mode
        },
        logical(1)
      ),
      row.names = nets_obj,
      check.names = FALSE
    )
    print(nets_summary)
    cat("\n")
  }

  if (length(deps_obj) > 0) {
    cat("--- Dependent Events ---\n")
    deps_summary <- data.frame(
      n_events = vapply(deps_obj, \(n) nrow(get(n, envir = x)), integer(1)),
      default_network = vapply(
        deps_obj,
        function(n) {
          net <- attr(get(n, envir = x), "default_network")
          if (is.null(net)) "" else net
        },
        character(1)
      ),
      nodeset = vapply(
        deps_obj,
        \(n) paste(attr(get(n, envir = x), "nodes"), collapse = ", "),
        character(1)
      ),
      row.names = deps_obj,
      check.names = FALSE
    )
    print(deps_summary)
    cat("\n")
  }

  if (length(events_names) > 0) {
    cat("--- Events Data Frames ---\n")
    events_summary <- data.frame(
      n_events = vapply(events_names, \(n) nrow(get(n, envir = x)), integer(1)),
      object_type = vapply(
        events_names,
        function(n) {
          cols <- colnames(get(n, envir = x))
          if (all(c("sender", "receiver") %in% cols)) return("dyadic")
          if ("node" %in% cols) return("nodal")
          return("unknown")
        },
        character(1)
      ),
      update_mode = vapply(
        events_names,
        function(n) {
          cols <- colnames(get(n, envir = x))
          if ("increment" %in% cols) return("increment")
          if ("replace" %in% cols) return("replace")
          return("unknown")
      }, character(1)),
      row.names = events_names,
      check.names = FALSE
    )
    print(events_summary)
    cat("\n")
  }

  if (length(global_obj) > 0) {
    cat("--- Global Attributes ---\n")
    global_summary <- data.frame(
      n_events = vapply(global_obj, \(n) nrow(get(n, envir = x)), integer(1)),
      row.names = global_obj,
      check.names = FALSE
    )
    print(global_summary)
    cat("\n")
  }

  if (length(other_obj) > 0) {
    cat("--- Other Objects ---\n")
    other_summary <- data.frame(
      class = vapply(other_obj, \(n) class(get(n, envir = x))[1], character(1)),
      length = vapply(other_obj, \(n) length(get(n, envir = x)), integer(1)),
      row.names = other_obj,
      check.names = FALSE
    )
    print(other_summary)
    cat("\n")
  }

  invisible(x)
}

# print preprocessed.goldfish
#
# @param x a preprocessed.goldfish object
#' @export
#' @rdname print-method
#
# @examples print(
#   structure(
#     list(formula = dep ~ inertia, dependentStatistics = numeric(20)),
#     class = "preprocessed.goldfish"
#   )
# )
print.preprocessed.goldfish <- function(x, ..., width = getOption("width")) {
  cat("**Preprocess object for the model:**\n")
  print(x$formula)
  cat(" dependent events processed: ", length(x$dependentStatsChange), "\n")
  # cat(" Model type:", result$model.type, "\n")
  cat("*The results are available in the following objects:*\n\n")

  textNodes <- "A character with the name of the object of class nodes.goldfish"

  description <- data.frame(
    name = c(
      "initialStats", "dependentStatsChange", "rightCensoredStatsChange",
      "intervals", "rightCensoredIntervals", "orderEvents", "eventTime",
      "eventSender", "eventReceiver", "startTime", "endTime", "formula",
      "nodes", "nodes2"
    ),
    description = c(
      "Initial statistical matrices for the effects given previous history.",
      paste(
        "List: For each dependent event,",
        "a list with the change statistics for the given",
        "\n state of the process."
      ),
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
      paste(textNodes, "(rows/first-mode)"),
      paste(textNodes, "(cols/second-mode)")
    ),
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
    }
  )

  invisible(NULL)
}

# Print estimation_opt.goldfish object
#' @export
#' @rdname print-method
#' @return For objects of class `estimation_opt.goldfish`, print a summary of the estimation control options.
print.estimation_opt.goldfish <- function(x, ...) {
  cat("Estimation Control Options (estimation_opt.goldfish):\n")
  for (name in names(x)) {
    value <- x[[name]]
    if (is.null(value)) {
      cat(sprintf("  %-25s: NULL\n", name))
    } else if (is.atomic(value) && length(value) == 1) {
      cat(sprintf("  %-25s: %s\n", name, as.character(value)))
    } else if (is.atomic(value) && length(value) > 1 && length(value) <= 5) {
      cat(sprintf("  %-25s: %s\n", name, paste(as.character(value), collapse = ", ")))
    } else if (is.list(value) || (is.atomic(value) && length(value) > 5)) {
      cat(sprintf("  %-25s: <%s of length %d>\n", name, class(value)[1], length(value)))
    } else {
      cat(sprintf("  %-25s: <%s>\n", name, class(value)[1]))
    }
  }
  invisible(x)
}

# Print preprocessing_opt.goldfish object
#' @export
#' @rdname print-method
#' @return For objects of class `preprocessing_opt.goldfish`, print a summary of the preprocessing control options.
print.preprocessing_opt.goldfish <- function(x, ...) {
  cat("Preprocessing Control Options (preprocessing_opt.goldfish):\n")
  for (name in names(x)) {
    value <- x[[name]]
    if (is.null(value)) {
      cat(sprintf("  %-20s: NULL\n", name))
    } else if (name == "opportunities_list" && is.list(value)) {
      cat(sprintf("  %-20s: List of %d elements\n", name, length(value)))
    } else if (is.atomic(value) && length(value) == 1) {
      cat(sprintf("  %-20s: %s\n", name, as.character(value)))
    } else if (is.atomic(value) && length(value) > 1 && length(value) <= 5) {
      cat(sprintf("  %-20s: %s\n", name, paste(as.character(value), collapse = ", ")))
    } else {
      cat(sprintf("  %-20s: <%s>\n", name, class(value)[1]))
    }
  }
  invisible(x)
}

#' @importFrom generics tidy
#' @export
generics::tidy
# tidy <- function(x) UseMethod("tidy")
# # just for testing, don't use because overwrites use in other packages

#' @method tidy result.goldfish
#' @export
tidy.result.goldfish <- function(
    x, conf.int = FALSE, conf.level = 0.95,
    compact = TRUE, complete = FALSE, ...) {
  isFixed <- GetFixed(x)
  coefMat <- summary.result.goldfish(x)$coefMat
  colnames(coefMat) <- c("estimate", "std.error", "statistic", "p.value")

  if (conf.int) {
    confInterval <- stats::confint(x, level = conf.level)
    colnames(confInterval) <- c("conf.low", "conf.high")
  }

  if (compact) {
    terms <- paste(
      x$names[, 1],
      rownames(x$names),
      if (ncol(x$names) > 2) {
        apply(x$names[, -1], 1, paste, collapse = " ")
      } else {
        x$names[, -1]
      }
    )
    terms <- trimws(terms)
    terms <- gsub("\\$", " ", terms)

    if (!complete) terms <- terms[!isFixed]

    terms <- cbind(term = terms)
  } else {
    terms <- cbind(term = rownames(x$names), x$names)
    terms[, "Object"] <- gsub("\\$", " ", terms[, "Object"])

    if (!complete) terms <- terms[!isFixed, ]
  }

  if (complete) {
    result <- cbind(tibble::as_tibble(terms), tibble::as_tibble(coefMat))

    if (conf.int) {
      confIntervalComplete <- matrix(
        NA_real_,
        nrow = length(isFixed),
        ncol = ncol(confInterval),
        dimnames = list(NULL, colnames(confInterval))
      )
      confIntervalComplete[!isFixed, ] <- confInterval

      result <- cbind(result, tibble::as_tibble(confIntervalComplete))
    }
  } else {
    coefMat <- coefMat[!isFixed, ]
    result <- cbind(tibble::as_tibble(terms), tibble::as_tibble(coefMat))

    if (conf.int) result <- cbind(result, tibble::as_tibble(confInterval))
  }

  return(tibble::as_tibble(result))
}

#' @importFrom generics glance
#' @export
generics::glance
# glance <- function(x) UseMethod("glance")
# just for testing, don't use because overwrites use in other packages

#' @method glance result.goldfish
#' @export
glance.result.goldfish <- function(x, ...) {
  with(
    summary(x),
    tibble::tibble(
      # r.squared = r.squared,
      # adj.r.squared = adj.r.squared,
      # sigma = sigma,
      # statistic = fstatistic["value"],
      # p.value = pf(
      #   fstatistic["value"],
      #   fstatistic["numdf"],
      #   fstatistic["dendf"],
      #   lower.tail = FALSE
      # ),
      logLik = as.numeric(logLik(x)),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
      # deviance = stats::deviance(x),
      # df.residual = df.residual(x),
      df = x$nParams,
      nobs = x$nEvents
    )
  )
}

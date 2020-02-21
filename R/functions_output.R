######################### ###
#
# Print, summary and xtable functions
# for classes in Goldfish
#
######################### ###

#' Information criteria statistics
#' @param object a goldfish results object
#' @param digits number of decimal places to be printed
#' @param ... additional arguments to be passed
#' @return AIC, BIC, log likelihood of the model or the average log likelihood per event.
#' @name model-selection
NULL


#' Print Goldfish results
#' @param x a goldfish results object
#' @param digits number of decimal places to be printed
#' @param ... additional arguments to be passed
#' @return prints a results overview table including names, estimates, standard errors, significance levels, AIC and likelihood.
#' @noRd
#' @export
print.result.goldfish <- function(x, digits = 2, ...) {
  result <- x
  nParams <- length(result$parameters)

  if (is.null(result$names)) result$names <- seq_len(nParams)
  names <- result$names
  est <- result$parameters
  std.err <- result$standard.errors
  t <- est / std.err
  sig <- rep("", nParams)
  sig[abs(t) > qnorm(1 - 0.05 / 2)] <- "*"
  sig[abs(t) > qnorm(1 - 0.01 / 2)] <- "**"
  sig[abs(t) > qnorm(1 - 0.001 / 2)] <- "***"

  aic <- -2 * result$log.likelihood + 2 * nParams
  aicc <- aic + 2 * nParams * (nParams + 1) / (result$n.events - nParams - 1)
  bic <- -2 * result$log.likelihood + nParams * log(result$n.events)

  print(data.frame(names, est, std.err, sig, t))
  cat(" ", paste("Log likelihood", round(result$log.likelihood, 4), "\n"))
  cat(" ", paste(
    ifelse(result$convergence[[1]], "Converged", "Not converged"), "with max abs. score of",
    round(result$convergence$max.abs.score, 5)
  ), "\n")
  cat(" ", paste(
    "AIC ", round(AIC(result), 5),
    # "\n  AICc", round(aicc, 5),
    "\n  BIC ", round(BIC(result), 5)
  ), "\n")
  cat(" ", paste("Model type:", result$model.type), "\n")
}

# Calculate AIC of Goldfish results
# @param object a goldfish results object
# @param ... additional arguments to be passed to or from other functions
#' @param k Penalty factor
# @importFrom stats AIC BIC qnorm
# @return AIC of a model
# @export
#' @rdname model-selection
AIC.result.goldfish <- function(object, ..., k = 2) {
  if (k != 2) warning("implemented only for k = 2")
  # TODO check events / parameter ratio ad adjust k
  result <- object
  nParams <- length(result$parameters)
  aic <- -2 * result$log.likelihood + 2 * nParams
  # aicc <- aic + 2*nParams*(nParams + 1)/(result$n.events - nParams - 1)
}

# Calculate BIC of Goldfish results
# @param object a goldfish results object
# @param ... additional arguments to be passed to or from other functions
# @return AIC of a model
# @export
#' @rdname model-selection
BIC.result.goldfish <- function(object, ...) {
  result <- object
  nParams <- length(result$parameters)
  bic <- -2 * result$log.likelihood + nParams * log(result$n.events)
}

# Calculate log likelihood of Goldfish results
# @param object a goldfish results object
#' @param avgPerEvent a boolean indicating whether the everage likelihood per event should be calculated
# @param ... additional arguments to be passed to or from other functions
# @return Log likelihood of the model or the average log likelihood per event
# @export
#' @rdname model-selection
logLik.result.goldfish <- function(object, avgPerEvent = FALSE, ...) {
  if (avgPerEvent) {
    return(object$log.likelihood / object$n.events)
  }
  return(object$log.likelihood)
}


plot.nodes.goldfish <- function(x) {
  if (is.null(goldfish:::findPresence(x))) stop("No composition change")
  chan <- get(goldfish:::findPresence(x))
  chan$time <- as.Date.character(chan$time)
  plotbeg <- min(chan$time)
  plotend <- max(chan$time)
  chan <- chan[!duplicated(chan$node), ]

  require(ggplot2)
  require(scales)
  plotit <- x[order(x$present, decreasing = TRUE), ]
  plotit$beg[plotit$present] <- as.character(plotbeg)
  plotit[match(chan$node, plotit$label), "beg"] <- as.character(chan$time)
  plotit$end <- as.Date.character(plotend)
  plotit <- plotit[order(plotit$beg), ]

  ggplot(plotit, aes(x = label, ymin = as.Date(beg), ymax = as.Date(end))) +
    geom_linerange() + theme_classic() + coord_flip() +
    scale_y_date(
      limits = c(as.Date(plotbeg), as.Date(plotend)),
      breaks = date_breaks("1 years")
    ) +
    scale_x_discrete(limits = plotit$label[!duplicated(plotit$label)]) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = element_blank(), axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

plot.dependent.goldfish <- function(x) {
  if (is.null(attr(x, "defaultNetwork"))) stop("No default network")
  if (is.null(attr(x, "nodes"))) stop("No nodes")
  # nodes <- get(attr(x, "nodes")[1])
  # if(is.null(goldfish:::findPresence(nodes))) stop("No composition change")
  # chan <- get(goldfish:::findPresence(nodes))

  require(reReg)
  x <- x[order(x$sender), ]
  # y <- attr(x, "nodes")
  plotEvents(reSurv(
    time1 = as.numeric(x$time) - min(as.numeric(x$time)),
    time2 = max(as.numeric(x$time)) - min(as.numeric(x$time)),
    id = x$sender,
    event = (duplicated(x$sender, fromLast = TRUE)) * 1,
    status = rep(0, nrow(x))
  ))
}

print.nodeset.goldfish <- function(x) {
  cat(paste("Dimensions:", paste(x$n, collapse = " "), "\n"))
  cat(paste("Number of present actors:", sum(x$isPresent), "\n"))
}

# DEPRECATED
# print.nodes.goldfish <- function(x){
#   if(is.null(x)) {
#     cat("No nodeset added yet.")
#   } else {
#     for( n in 1:length(x) ) {
#       cat("\t",names(x)[n],"\n")
#       print(x[[n]])
#     }
#   }
# }

## TODO adapt to new simplified network objects
print.network.goldfish_ <- function(x) {
  cat(paste("Dimensions:", paste(x$size, collapse = " "), "\n"))
  cat(paste("Number of ties:", sum(x$data), "\n"))
  if (x$isBipartite) {
    cat(paste("Actor sets:", paste(x$nodeSet, collapse = " "), "\n"))
  } else {
    cat(paste("Actor sets:", x$nodeSet[1], "\n"))
  }
  cat(ifelse(x$isBipartite, "bipartite\n", "not bipartite\n"))
  cat(ifelse(x$isSymmetric, "symmetric\n", "asymmetric\n"))
}

## TODO adapt to new simplified network objects
print.attribute.goldfish_ <- function(x) {
  cat(paste("Type of attribute:", paste(x$type, collapse = " "), "\n"))
  cat(paste("Dimensions:", paste(length(x$data), collapse = " "), "\n"))
}

print.elements.goldfish <- function(x) {
  # Check the number of networks and attributes
  n.networks <- 0
  n.attributes <- 0
  if (!is.null(x)) {
    for (n in seq_along(x)) {
      if (class(x[[n]]) == "network.goldfish") n.networks <- n.networks + 1
      if (class(x[[n]]) == "attribute.goldfish") n.attributes <- n.attributes + 1
    }
  }
  # Print networks
  cat("\nNetwork elements: \n")
  if (n.networks == 0) {
    cat("No network added yet.")
  } else {
    for (n in seq_along(x)) {
      if (class(x[[n]]) == "network.goldfish" && !x[[n]]$isWindow) {
        cat(paste("\n\t", names(x)[n], "\n"))
        print(x[[n]])
        indexes <- which(grepl(paste(names(x)[n], ".", sep = ""), names(x)))
        if (length(indexes) > 0) {
          cat("Windows: ")
          cat(paste(substr(names(x)[indexes], nchar(names(x)[n]) + 2, nchar(names(x)[indexes])), "s", sep = ""))
          cat(" \n")
        }
      }
    }
  }
  # Print attributes
  cat("\nAttribute elements: \n")
  if (n.attributes == 0) {
    cat("No attribute added yet.")
  } else {
    for (n in seq_along(x)) {
      if (class(x[[n]]) == "attribute.goldfish" && !x[[n]]$isWindow) {
        cat(paste("\n\t", names(x)[n], "\n"))
        print(x[[n]])
        indexes <- which(grepl(paste(names(x)[n], ".", sep = ""), names(x)))
        if (length(indexes) > 0) {
          cat("Windows: ")
          cat(paste(substr(names(x)[indexes], nchar(names(x)[n]) + 2, nchar(names(x)[indexes])), "s", sep = ""))
          cat(" \n")
        }
      }
    }
  }
}

print.eventList.goldfish <- function(x, head = TRUE, n = 3) {
  cat(paste("Dimensions:", paste(dim(x$data), collapse = " "), "\n"))
  cat(paste("Process state element:", x$processStateElement, "\n"))
  if (head) print(head(x$data, n = n))
}

print.events.goldfish <- function(x) {
  # if(class(x)=="data.frame") return()
  if (length(x) == 0) {
    cat("No events added yet.")
  } else {
    for (n in seq_along(x)) {
      if (!x[[n]]$isWindow) {
        cat(paste("\n\t", names(x)[n], "\n"))
        print(x[[n]])
        indexes <- which(grepl(paste(names(x)[n], ".", sep = ""), names(x)))
        if (length(indexes) > 0) {
          cat("Windows: ")
          cat(paste(substr(names(x)[indexes], nchar(names(x)[n]) + 2, nchar(names(x)[indexes])), "s", sep = ""))
          cat(" \n")
        }
      }
    }
  }
}

print.data.goldfish <- function(x) {

  # Print nodesets
  cat("Nodesets: \n\n")
  print(x$nodes)

  # Print networks and attributes
  print(x$elements)

  # Print events
  cat("\nEvents: \n")
  print(x$events)
}

print.effects.goldfish <- function(x) {

  # Print data object associated
  cat(paste("Dependent event list: ", x$depv))

  # Print data object associated
  cat("\nEffects:\n")
  if (length(x$effects) == 0) {
    cat("No effect added yet")
  } else {
    tab <- data.frame(x$effects, row.names = NULL)
    colnames(tab) <- c("effect", "weighted", "window")
    print(tab)
  }
}

# Function to prettily print list of goldfish results
# with some defaults (needs work/extension)
print.list <- function(x, substitute = NULL, dependents = NULL) {
  if (all(lapply(x, class) == "result.goldfish")) {

    # Get types
    depVars <- unlist(lapply(x, function(y) as.character(y$formula)[2]))
    depVarTitles <- as.character(c(1, rep(3, length(unique(depVars)))))
    if (is.null(dependents)) {
      names(depVarTitles) <- c(" ", paste0(unique(depVars), "[note]"))
    } else {
      names(depVarTitles) <- c(" ", paste0(dependents, "[note]"))
    }
    depMods <- if_else(grepl(
      "Rate",
      unlist(lapply(x, function(y) as.character(y$model.type)))
    ),
    "Rate", "Choice"
    )

    # Get lengths
    nParams <- unlist(lapply(x, function(y) length(y$parameters)))
    maxRate <- max(nParams[depMods == "Rate"])
    maxChoi <- max(nParams[depMods == "Choice"])

    # Diagnostics
    nevs <- unlist(lapply(x, function(y) y$n.events))
    logl <- unlist(lapply(x, function(y) round(y$log.likelihood, 2)))
    aics <- unlist(lapply(x, function(y) round(-2 * y$log.likelihood + 2 * length(y$parameters), 2)))
    #   aicc <- aic + 2*nParams*(nParams + 1)/(result$n.events - nParams - 1)
    bics <- unlist(lapply(x, function(y) round(-2 * y$log.likelihood + length(y$parameters) * log(y$n.events), 2)))
    diags <- unlist(lapply(unique(depVars), function(y) {
      paste0(
        "N = ", paste(nevs[depVars == y], collapse = " and "),
        ". Log Likelihood = ", paste(logl[depVars == y], collapse = " and "),
        ". AIC = ", paste(aics[depVars == y], collapse = " and "),
        ". BIC = ", paste(bics[depVars == y], collapse = " and "),
        "."
      )
    }))

    # Extract and format key results
    cleanRes <- lapply(x, function(y) {
      data.frame(
        Effect = apply(as.data.frame(y$names), 1, paste, collapse = " "),
        Est. = round(y$parameters, 3),
        S.E. = format(round(y$standard.errors, 3), digits = 3),
        t = y$parameters / y$standard.errors,
        stringsAsFactors = F
      ) %>%
        mutate(S.E. = paste0("(", if_else(S.E. == "0.000", "< 0.001", as.character(S.E.)), ")")) %>%
        mutate(t = if_else(abs(t) > qnorm(1 - 0.001 / 2), "***",
          if_else(abs(t) > qnorm(1 - 0.01 / 2), "**",
            if_else(abs(t) > qnorm(1 - 0.05 / 2), "*", "")
          )
        ))
    })

    # Renaming
    if (!is.null(substitute)) {
      cleanRes <- lapply(cleanRes, function(y) {
        y$Effect <- y$Effect %>% str_replace_all(fixed(substitute))
        y
      })
    }
    cleanRes <- lapply(cleanRes, function(y) {
      y <- y %>% mutate(
        Effect = if_else(str_detect(Effect, "inertia"), "Inertia", Effect),
        Effect = str_replace_all(
          Effect,
          fixed(c(
            "ego " = "Ego's ",
            "alter " = "Alter's ",
            "diff " = "Different "
          ))
        ),
        Effect = if_else(str_detect(Effect, "out"), paste(str_remove(Effect, "out "), "Outdegree"), Effect),
        Effect = if_else(str_detect(Effect, "indeg"), paste(str_remove(Effect, "indeg "), "Indegree"), Effect),
        Effect = if_else(str_detect(Effect, "recip"), paste(str_remove(Effect, "recip "), "Reciprocity"), Effect),
        Effect = if_else(str_detect(Effect, "trans"), paste(str_remove(Effect, "trans "), "Transitivity"), Effect),
        Effect = str_remove(Effect, "tie ")
      )
    })

    # Combine
    cleanRes <- lapply(unique(depVars), function(d) bind_rows(cleanRes[depVars == d])) %>%
      reduce(full_join, by = "Effect")
    cleanRes[is.na(cleanRes)] <- ""


    kableligns <- c("l", rep(c("r", "r", "l"), length(depVarTitles) - 1))
    kablenames <- c("Effect", rep(c("Est.", "S.E.", ""), length(depVarTitles) - 1))

    kable(cleanRes,
      booktabs = TRUE, caption = "Results",
      align = kableligns, col.names = kablenames
    ) %>%
      add_header_above(header = depVarTitles) %>%
      add_footnote(diags, notation = "symbol", threeparttable = TRUE) %>%
      kable_styling(latex_options = "hold_position") %>%
      pack_rows("Rate", 1, maxRate) %>%
      pack_rows("Choice", maxRate + 1, maxRate + maxChoi + 1)
  }
}


print.preprocessed.goldfish <- function(x, digits = 2) {
  cat("**Preprocess object for the model:**\n")
  print(x$formula)
  cat(" dependent events processed: ", length(x$dependetStatsChange), "\n")
  # cat(" Model type:", result$model.type, "\n")
  cat("*The results are available in the following objects:*\n\n")
  
  description <- data.frame(
    name = c("initialStats", "dependentStatsChange", "rightCensoredStatsChange", "intervals", "rightCensoredIntervals",
             "orderEvents", "eventTime", "eventSender", "eventReceiver", "startTime", "endTime", "formula",
             "nodes", "nodes2" ),
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
      wrap <- strwrap(description[x, 2], width = 80)
      wrap[1] <- paste(strrep(" ", mxName - nchar(description[x, 1])), wrap[1])
      wrap <- paste(wrap, collapse = paste0("\n", strrep(" ", mxName + 1)))
      cat(wrap)
      cat("\n")
    } )

  invisible(NULL)
}

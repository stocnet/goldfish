#' @importFrom generics tidy
#' @export
generics::tidy
# tidy <- function(x) UseMethod("tidy") # just for testing, don't use because overwrites use in other packages

#' @method tidy result.goldfish
#' @export
tidy.result.goldfish <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  
  if (x$model == "DyNAM" & x$subModel == "rate") {
    terms <- paste(rownames(x$names), x$names[,1])
  } else {
    terms <- paste(x$names[,1], rownames(x$names), x$names[,2])
  }
  terms <- trimws(terms)
  terms <- gsub("\\$"," ",terms)
  
  result <- tibble::tibble(term = terms,
                           estimate = x$parameters,
                           std.error = x$standardErrors,
                           statistic = x$parameters/x$standardErrors,
                           p.value = 2 * (1 - pnorm(abs(x$parameters/x$standardErrors))))
  
  # if (conf.int) {
  #   ci <- confint(x, level = conf.level)
  #   result <- dplyr::left_join(result, ci, by = "term")
  # }
  
  result
}

#' @importFrom generics glance
#' @export
generics::glance
# glance <- function(x) UseMethod("glance") # just for testing, don't use because overwrites use in other packages

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
      # df = fstatistic["numdf"],
      logLik = as.numeric(stats::logLik(x)),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
      # deviance = stats::deviance(x),
      # df.residual = df.residual(x),
      nobs = x$nEvents
    )
  )
}

# to be implemented...
# #' @importFrom generics augment
# #' @export
# generics::augment

#' Extract model coefficients from estimate output
#'
#' Return a named vector with the estimated coefficients returned by `estimate`.
#' The names just correspond to the short effect name. For a comprehensive output use
#' `summary()`.
#' Note that while the output to the console is rounded, the returned vector
#' is not.
#' @param object an object of class `result.goldfish` output from an
#' [estimate()] call.
#' @param complete logical. Indicates whether the parameter coefficients of
#' effects fixed during estimation using `fixedParameters` should be printed.
#' @param ... additional arguments to be passed.
#' @method coef result.goldfish
#' @export
#' @noRd
#' @return A named numeric vector with the extracted coefficients from the
#' output of `estimate`.
#' The naming correspond to the short name of the effect use in the formula.
#' Coefficients with the same name are produced when the same effect is used
#' more than one time with different arguments, e.g., 
#' `dependentEvents ~ indeg + indeg(exogenousNetwork)`.
#' Duplicates names could produce erroneous results when subsetting the vector
#' by names.
#' A more comprehensive output can be obtain using [generics::tidy()], see
#' `vignette("teaching2")`.
#' 
#' @examples
#' # A multinomial receiver choice model
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls,
#'                           nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors,
#'                                         defaultNetwork = callNetwork)
#' \dontshow{
#' callsDependent <- callsDependent[1:50, ]
#' }
#' mod01 <- estimate(callsDependent ~ inertia + recip + trans,
#'                   model = "DyNAM", subModel = "choice")
#' coef(mod01)
coef.result.goldfish <- function(object, ..., complete = FALSE) {
  result <- object$parameters
  names(result) <- rownames(object$names)
  isFixed <- GetFixed(object)
  if (!complete && any(isFixed)) {
    result <- result[!isFixed]
  }
  result
}

#' Extract log-likelihood from a fitted model object
#' 
#' This function extract the log-likelihood from the output of a
#' `estimate` call.
#' The extracted log-likelihood correspond to the value in the last
#' iteration of the `estimate` call, users should check convergence of 
#' the Gauss/Fisher scoring method before using the log-likelihood statistic
#' to compare models.
#' 
#' Users might use [stats::AIC()] and [stats::BIC()] to compute the Information
#' Criteria from one or several fitted model objects.
#' An information criterion could be used to compare models
#' with respect to their predictive power.
#' 
#' Alternatively, [lmtest::lrtest()] can be used to compare models via
#' asymptotic likelihood ratio tests. The test is designed to compare nested
#' models. i.e., models where the model specification of one contains a subset
#' of the predictor variables that define the other. 
#' 
#' @param object an object of class \code{result.goldfish} output from an
#' \code{\link{estimate}} call with a fitted model.
#' @param avgPerEvent a logical value indicating whether the average
#' likelihood per event should be calculated.
#' @param ... additional arguments to be passed.
#' @return Returns an object of class `logLik` when `avgPerEvent = FALSE`.
#' This is a number with the extracted log-likelihood from the fitted model,
#' and with the following attributes:
#'   \item{df}{degrees of freedom with the number of estimated parameters in
#'     the model}
#'   \item{nobs}{the number of observations used in estimation.
#'     In general, it corresponds to the number of dependent events used in
#'     estimation. For a `subModel = "rate"` or `model = "REM"` with intercept,
#'     it corresponds to the number of dependent events plus right-censored
#'     events due to exogenous or endogenous changes.}
#' 
#' When `avgPerEvent = TRUE`, the function returns a number with the average
#' log-likelihood per event. The total number of events depends on the presence
#' of right-censored events in a similar way that the attribute `nobs`
#' is computed when `avgPerEvent = FALSE`. 
#' @export
#' @method logLik result.goldfish
logLik.result.goldfish <- function(object, ..., avgPerEvent = FALSE) {
  if (avgPerEvent) {
    return(object$logLikelihood / object$nEvents)
  }
  
  val <- object$logLikelihood
  # attr(val, "nall") <- object$nEvents
  attr(val, "nobs") <- object$nEvents
  attr(val, "df") <- object$nParams
  class(val) <- "logLik"
  return(val)
}

#' @export
#' @importFrom stats .vcov.aliased
#' @method vcov result.goldfish
vcov.result.goldfish <- function(object, complete = FALSE, ...) {
  isFixed <- GetFixed(object)
  namesCoef <- rownames(object$names)
  
  vc <- solve(object$finalInformationMatrix[!isFixed, !isFixed])
  vc <- .vcov.aliased(isFixed, vc, complete = complete)  
  if (!complete) {
    namesCoef <- namesCoef[!isFixed]
  }
  
  return(structure(vc, dimnames = list(namesCoef, namesCoef)))
}

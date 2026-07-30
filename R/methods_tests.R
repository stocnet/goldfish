##################### ###
#
# Goldfish package
# The diagnostic test family: the generics and what they dispatch on
#
##################### ###

#' Diagnostic tests for a fitted model
#'
#' @description
#' Three questions a fitted relational event model can be asked, each an S3
#' generic dispatching on the fit:
#' \describe{
#'   \item{[test_gof()]}{does the model fit the sequence it was estimated on?
#'     Read from the cumulative score processes, which are bridges at the
#'     maximum: a process that wanders is an effect whose contribution is not
#'     spread over the sequence the way the model assumes.}
#'   \item{[test_parameter()]}{is a coefficient held at an imposed value
#'     consistent with the data? The score test of the terms a fit carries as
#'     `offset()`, evaluated at the vector those terms were fixed to.}
#'   \item{[test_time()]}{is a coefficient constant over the sequence? Read
#'     either as a trend in the scaled Schoenfeld residuals against a time
#'     transform, or as a comparison of period-wise partial sums.}
#' }
#'
#' @details
#' The three names are shared with \pkg{RSiena}, which publishes generics of
#' the same names for the same three questions asked of a different model
#' class. That is a convergence rather than a collision — but R finds a method
#' through the generic in scope, so with both packages attached the one
#' attached later masks the other's generic and the masked package's methods
#' become invisible to it. Call `goldfish::test_gof()` and its siblings
#' explicitly if both are attached; that always resolves.
#'
#' @param object a fitted model of class `"result.goldfish"`, or a
#'   specification (multi-process) fit. Named `object` rather than `x` on
#'   [test_gof()] alone, matching the signature the sibling generic already
#'   publishes.
#' @param x a fitted model of class `"result.goldfish"`, or a specification
#'   (multi-process) fit.
#' @param ... additional arguments passed to or from other methods.
#'
#' @return An object of the test's own class, carrying the metadata described
#'   in [diagnostic-tables].
#'
#' @seealso [diagnose_outliers()] and [diagnose_changepoints()] for the
#'   descriptive counterparts, [residuals.result.goldfish()] for the residuals
#'   these read, and [diagnostic-tables] for the metadata a result carries.
#' @name diagnostic-tests
NULL

#' @rdname diagnostic-tests
#' @export
test_gof <- function(object, ...) {
  UseMethod("test_gof")
}

#' @export
test_gof.default <- function(object, ...) {
  abort_not_diagnosable_class("test_gof", object, arg = "object")
}

#' @rdname diagnostic-tests
#' @export
test_parameter <- function(x, ...) {
  UseMethod("test_parameter")
}

#' @export
test_parameter.default <- function(x, ...) {
  abort_not_diagnosable_class("test_parameter", x)
}

#' @rdname diagnostic-tests
#' @export
test_time <- function(x, ...) {
  UseMethod("test_time")
}

#' @export
test_time.default <- function(x, ...) {
  abort_not_diagnosable_class("test_time", x)
}

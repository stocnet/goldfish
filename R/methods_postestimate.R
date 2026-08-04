#' Extract model coefficients from estimate output
#'
#' Return a named vector with the estimated coefficients returned by `estimate`.
#' The names just correspond to the short effect name.
#' For a comprehensive output use `summary()`.
#' Note that while the output to the console is rounded, the returned vector
#' is not.
#' @param object an object of class `result.goldfish` output from an
#' [estimate] call.
#' @param complete logical. Indicates whether the parameter coefficients of
#' effects held fixed during estimation (via `offset()`) should be printed.
#' @param ... additional arguments to be passed.
#' @method coef result.goldfish
#' @export
#' @noRd
#' @return A named numeric vector with the extracted coefficients from the
#' output of `estimate`.
#' The naming uses a minimal-unique short form: the curated short effect name,
#' with the smallest disambiguating suffix (object prefix, then argument codes)
#' appended to every member of a colliding group, so names stay unique even when
#' the same effect is used more than once with different arguments, e.g.,
#' `dependentEvents ~ indeg + indeg(exogenousNetwork)`. This makes name-based
#' subsetting of the returned vector reliable.
#' A more comprehensive output can be obtain using [generics::tidy()], see
#' `vignette("teaching2")`.
#'
#' @examples
#' # A multinomial receiver choice model on the prebuilt `social_evolution` data
#' data("social_evolution")
#' mod01 <- estimate_dynam(calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution
#' )
#' coef(mod01)
coef.result.goldfish <- function(object, ..., complete = FALSE) {
  # Deliberately unguarded: `parameters` was never renamed, so an old object's
  # coefficients are still the right numbers, and `print()` -- which calls this
  # twice -- already carries the diagnosis for the interactive case.
  result <- object$parameters
  names(result) <- term_label(object$names, ".coef_name", "coef")
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
#'     estimation. For a `sub_model = "rate"` or `model = "REM"` with intercept,
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
  # Guards the AIC() / BIC() path too: the default methods reach the fit only
  # through logLik(), and a NULL `df` is exactly what let them misreport.
  abort_if_stale_result(object, "a log-likelihood")
  if (avgPerEvent) {
    return(object$log_likelihood / object$n_events)
  }

  val <- object$log_likelihood
  # attr(val, "nall") <- object$n_events
  attr(val, "nobs") <- object$n_events
  attr(val, "df") <- object$n_params
  class(val) <- "logLik"
  return(val)
}

#' @export
#' @method vcov result.goldfish
vcov.result.goldfish <- function(object, complete = FALSE, ...) {
  abort_if_stale_result(object, "a variance-covariance matrix")
  isFixed <- GetFixed(object)
  namesCoef <- term_label(object$names, ".coef_name", "coef")

  # A covariance is a statement about estimated coefficients, and a fit whose
  # coefficients are all held has none. The information over the free
  # parameters is then zero-dimensional and `solve()` reports that in its own
  # vocabulary ("'a' is 0-diml"), which says nothing about the model.
  if (!any(!isFixed)) {
    cli::cli_abort(c(
      "A variance-covariance matrix needs at least one estimated coefficient.",
      "x" = "All {length(isFixed)} coefficient{?s} of this fit
             {?is/are} held at a fixed value.",
      "i" = "The log-likelihood and the information criteria are defined here;
             the covariance is not."
    ))
  }

  vc <- solve(object$final_information_matrix[!isFixed, !isFixed])
  vc <- stats::.vcov.aliased(isFixed, vc, complete = complete)
  if (!complete) {
    namesCoef <- namesCoef[!isFixed]
  }

  return(structure(vc, dimnames = list(namesCoef, namesCoef)))
}

# =========================================================================== #
# Multi-process (flavored) fits.
#
# The competing-flavor likelihood factorizes, so a container is K independent
# fits rather than one joint fit with a block-diagonal parameter vector. The
# extractors therefore return one COMPONENT per process instead of concatenating
# into a single vector: stacking would imply a joint covariance the model never
# estimated, and `vcov()` in particular has no meaningful stacked form -- the
# cross-process blocks are structurally zero, not estimated to be zero.
#
# Component names are rendered from the process_map. They are labels for
# reading, never identity: fids key the container, and a layer or flavor name
# containing a dot or colliding with another must not be able to masquerade as
# structure in a pasted key.
# =========================================================================== #

# process_map rows grouped by flavor, families in their map order within each.
# The map itself stays in fid order -- that is the canonical identity order and
# nothing reorders it -- but every user-facing view presents flavor-major,
# because the flavor is the process a reader reasons about and its rate and
# choice parts are two halves of one decision. Print and the extractors share
# this so they cannot describe the same object in two different orders.
flavored_row_order <- function(object) {
  map <- object$process_map
  unlist(
    lapply(object$flavors, function(fl) which(map$flavor == fl)),
    use.names = FALSE
  )
}

# Rendered labels for the container's components, flavor-major.
flavored_component_labels <- function(object) {
  map <- object$process_map
  vapply(
    map$fid[flavored_row_order(object)],
    function(f) render_process_label(map, f),
    character(1)
  )
}

#' @export
#' @method coef flavored_result.goldfish
#' @noRd
coef.flavored_result.goldfish <- function(object, ..., complete = FALSE) {
  fids <- object$process_map$fid[flavored_row_order(object)]
  out <- lapply(fids, function(f) {
    stats::coef(object$results[[as.character(f)]], complete = complete, ...)
  })
  stats::setNames(out, flavored_component_labels(object))
}

#' @export
#' @method vcov flavored_result.goldfish
#' @noRd
vcov.flavored_result.goldfish <- function(object, complete = FALSE, ...) {
  fids <- object$process_map$fid[flavored_row_order(object)]
  out <- lapply(fids, function(f) {
    stats::vcov(object$results[[as.character(f)]], complete = complete, ...)
  })
  stats::setNames(out, flavored_component_labels(object))
}

#' @export
#' @method logLik flavored_result.goldfish
#' @noRd
logLik.flavored_result.goldfish <- function(object, ..., avgPerEvent = FALSE) {
  parts <- lapply(object$process_map$fid, function(f) {
    stats::logLik(object$results[[as.character(f)]])
  })
  # The processes factorize, so the joint log-likelihood is the sum and the
  # degrees of freedom add up. `nobs` sums each process's own event count, which
  # differs between them: a timed rate process counts its right-censored rows,
  # a choice process does not.
  total <- sum(vapply(parts, as.numeric, numeric(1)))
  n_obs <- sum(vapply(parts, function(p) attr(p, "nobs"), numeric(1)))
  n_par <- sum(vapply(parts, function(p) attr(p, "df"), numeric(1)))

  if (avgPerEvent) {
    return(total / n_obs)
  }
  structure(total, nobs = n_obs, df = n_par, class = "logLik")
}

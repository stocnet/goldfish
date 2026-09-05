#' Extract model coefficients from estimate output
#'
#' Return a named vector with the estimated coefficients returned by `estimate`.
#' The names just correspond to the short effect name.
#' For a comprehensive output use `summary()`.
#' Note that while the output to the console is rounded, the returned vector
#' is not.
#' @param object an object of class `goldfishFit` output from an
#' [estimate] call.
#' @param complete logical. Indicates whether the parameter coefficients of
#' effects held fixed during estimation (via `offset()`) should be printed.
#' @param ... additional arguments to be passed.
#' @method coef goldfishFit
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
coef.goldfishFit <- function(object, ..., complete = FALSE) {
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
#' @param object an object of class \code{goldfishFit} output from an
#' \code{\link{estimate}} call with a fitted model.
#' @param avgPerEvent a logical value indicating whether the average
#' likelihood per event should be calculated.
#' @param ... additional arguments to be passed.
#' @return Returns an object of class `logLik` when `avgPerEvent = FALSE`.
#' This is a number with the extracted log-likelihood from the fitted model,
#' and with the following attributes:
#'   \item{df}{degrees of freedom with the number of estimated parameters in
#'     the model}
#'   \item{nobs}{the number of dependent events the model was estimated on,
#'     which is what the information criteria built on it should use. On a
#'     censoring sub-model (`sub_model = "rate"`, or `model = "REM"` with an
#'     intercept) the number of likelihood *intervals* is larger, since a
#'     right-censored interval is opened by every non-dependent event inside
#'     the observation window; that count is `n_intervals` on the fit and is
#'     deliberately not what `nobs` reports.}
#'
#' When `avgPerEvent = TRUE`, the function returns the average log-likelihood
#' per dependent event, dividing by the same count `nobs` reports.
#' @export
#' @method logLik goldfishFit
logLik.goldfishFit <- function(object, ..., avgPerEvent = FALSE) {
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
#' @method vcov goldfishFit
vcov.goldfishFit <- function(object, complete = FALSE, ...) {
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

# The container's processes, flavor-major, each with the identity its rows are
# tagged by. Every flavored method walks this rather than the map's own fid
# order, which is what stops two diagnostics of one fit disagreeing about row
# order -- `model_terms()` and `margin_table()` used to iterate the map
# directly and so ordered their rows differently from the `test_*` family.
flavored_processes <- function(object) {
  map <- object$process_map
  lapply(flavored_row_order(object), function(i) {
    list(
      fit = object$results[[as.character(map$fid[i])]],
      fid = map$fid[i],
      flavor = map$flavor[i],
      family = map$family[i]
    )
  })
}

# Tag a per-process table with the process it came from.
#
# Appended, never prepended: the term columns stay positionally stable between
# a single-process fit and a multi-process one, so a consumer indexing by
# position does not break when a second flavor appears. These are identity
# columns and NOT defining ones -- dropping them leaves the table's class, and
# therefore its plot dispatch, intact.
append_process_identity <- function(table, process) {
  table$flavor <- process$flavor
  table$family <- process$family
  table
}

# The processes a `flavor =` selection names, with their rendered labels.
#
# `flavor` NARROWS; it does not disambiguate. A flavor spanning several families
# leaves several processes selected, and the caller gets the list restricted to
# them rather than an error asking which family was meant. A flavor with one
# family -- the ordinary shape, and the one the Fisheries Treaties fits have --
# leaves exactly one, which is what lets the single-fit shape be one argument
# away.
flavored_selection <- function(object, flavor, call = rlang::caller_env()) {
  processes <- flavored_processes(object)
  labels <- flavored_component_labels(object)
  if (is.null(flavor)) {
    return(list(processes = processes, labels = labels))
  }
  available <- object$flavors
  unknown <- setdiff(flavor, available)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg flavor} names {length(unknown)} flavor{?s} this fit does not
         carry.",
        "x" = "Unknown: {.val {unknown}}.",
        "i" = "This fit carries {.val {available}}."
      ),
      call = call
    )
  }
  keep <- vapply(processes, function(p) p$flavor %in% flavor, logical(1))
  list(processes = processes[keep], labels = labels[keep])
}

# Apply a per-process method across a selection, unwrapping a lone process.
#
# The return shape therefore depends on how many processes the selection leaves,
# which is deliberate and is the whole point of `flavor =`: a container answers
# with a list because no vector can carry the process identity, and naming one
# process gets the ordinary single-fit shape back.
flavored_component_apply <- function(
  object,
  flavor,
  fn,
  what,
  call = rlang::caller_env()
) {
  selected <- flavored_selection(object, flavor, call = call)
  # Named on failure, as the `test_*` and `diagnose_*` families are. A process
  # can refuse for reasons the others do not share -- it carries no preprocessed
  # statistics, or its formula lacks a requested term -- and a message that does
  # not say which of four processes refused is not actionable.
  out <- Map(
    function(process, label) {
      tryCatch(
        fn(process$fit),
        error = function(e) {
          cli::cli_abort(
            "{.fn {what}} could not read process {.val {label}}.",
            parent = e,
            call = call
          )
        }
      )
    },
    selected$processes,
    selected$labels
  )
  if (length(out) == 1L) {
    return(out[[1L]])
  }
  stats::setNames(out, selected$labels)
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
#' @method coef goldfishFlavFit
#' @noRd
coef.goldfishFlavFit <- function(object, ..., complete = FALSE) {
  fids <- object$process_map$fid[flavored_row_order(object)]
  out <- lapply(fids, function(f) {
    stats::coef(object$results[[as.character(f)]], complete = complete, ...)
  })
  stats::setNames(out, flavored_component_labels(object))
}

#' @export
#' @method vcov goldfishFlavFit
#' @noRd
vcov.goldfishFlavFit <- function(object, complete = FALSE, ...) {
  fids <- object$process_map$fid[flavored_row_order(object)]
  out <- lapply(fids, function(f) {
    stats::vcov(object$results[[as.character(f)]], complete = complete, ...)
  })
  stats::setNames(out, flavored_component_labels(object))
}

#' @export
#' @method logLik goldfishFlavFit
#' @noRd
logLik.goldfishFlavFit <- function(object, ..., avgPerEvent = FALSE) {
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

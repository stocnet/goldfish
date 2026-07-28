##################### ###
#
# Goldfish package
# One evaluation pass of a fitted model's engine at a supplied parameter vector
#
##################### ###

# The quantities `evaluate_model()` can return, in the order a result reports
# them. `loglik` / `score` / `information` come out of every pass; the rest are
# opt-in components the engines compute in the event loop when asked.
EVALUATE_QUANTITIES <- c(
  "loglik",
  "score",
  "information",
  "interval_loglik",
  "total_rate",
  "conditional_logl",
  "event_scores",
  "ranks",
  "recall",
  "margins",
  "probabilities"
)

# Which stored-primitive capability each quantity needs, for the backend
# support check. The three always-computed ones and the derived recall map to
# nothing of their own.
EVALUATE_PRIMITIVE_OF <- c(
  interval_loglik = "loglik",
  total_rate = "loglik",
  conditional_logl = "loglik",
  event_scores = "scores",
  ranks = "ranks",
  recall = "ranks",
  margins = "margins",
  probabilities = "probabilities"
)

#' Evaluate a fitted model at a parameter vector
#'
#' @description
#' Runs the fitted model's likelihood machinery **once** at the parameter
#' vector `at` — no Newton-Raphson iterations, no re-estimation — and returns
#' the quantities asked for. It is the recompute half of the diagnostics
#' surface: what a fit did not store can be produced at the maximum likelihood
#' estimate, at a constrained vector, or anywhere else, without refitting.
#'
#' @details
#' The evaluation reuses the statistics the model was estimated from, so it
#' never re-runs preprocessing: they come from the object attached by
#' `estimate_*(return_preprocessed = TRUE)`, or from an equivalent one supplied
#' through `preprocessed`. Without either, the evaluation aborts naming both
#' routes rather than silently recomputing statistics that might not match.
#'
#' Evaluation is **unconditional on fixedness**: at a vector holding some
#' coefficients at fixed values, the returned score and information are those
#' of the full model at that vector, with the dimensions of the full effect
#' set. Nothing is zeroed out — that is what makes the output usable as the
#' constrained-model input of a score test.
#'
#' @param x a fitted model of class `"result.goldfish"`.
#' @param at the parameter vector to evaluate at. Either a full-length numeric
#'   vector in coefficient order, or a named one matched against the
#'   coefficient labels the model reports, in which case unnamed coefficients
#'   keep their fitted value. Defaults to the fitted coefficients.
#' @param return a character vector naming the quantities to compute, any
#'   subset of `"loglik"`, `"score"`, `"information"`, `"interval_loglik"`,
#'   `"total_rate"`, `"conditional_logl"`, `"event_scores"`, `"ranks"`,
#'   `"recall"`, `"margins"` and `"probabilities"`. The returned list carries
#'   exactly these, in that order. The components mean what the same-named
#'   components of a fitted object mean; see [estimate_dynam()].
#' @param preprocessed a `preprocessed.goldfish` object to evaluate over, as
#'   returned by [compute_statistics()]. Defaults to the one attached to the
#'   fit.
#' @param backend the computational implementation to evaluate on, defaulting
#'   to the one that produced the fit. Requesting a quantity a backend cannot
#'   produce aborts naming the backends that can.
#' @param recall_at an integer vector of thresholds for `"recall"`: the
#'   proportion of dependent events whose realized alternative ranked within
#'   the top `k`.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return A named list carrying the requested quantities, plus `backend`, the
#'   implementation that produced them, and `at`, the vector they were
#'   evaluated at. `"recall"` is a named numeric vector, one proportion per
#'   threshold; `"margins"` is the per-actor list documented under
#'   [estimate_dynam()], labeled and scale-marked as a fit's own margins are.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   return_preprocessed = TRUE
#' )
#' # At the maximum the score is (near) zero by construction.
#' evaluate_model(fit, return = c("loglik", "score"))
#'
#' # Anywhere else it is not: the full-model score at a vector holding the
#' # third coefficient at zero is what a score test reads.
#' constrained <- coef(fit)
#' constrained[3] <- 0
#' evaluate_model(fit, at = constrained, return = c("score", "information"))
#'
#' @seealso [estimate_dynam()] for the stored primitives an evaluation
#'   complements, [compute_statistics()] for the statistics it reads.
#' @export
evaluate_model <- function(x, ...) {
  UseMethod("evaluate_model")
}

#' @export
evaluate_model.default <- function(x, ...) {
  cli::cli_abort(c(
    "{.fn evaluate_model} needs a fitted goldfish model.",
    "x" = "{.arg x} is {.obj_type_friendly {x}}.",
    "i" = "Fit one with {.fn estimate_dynam} or {.fn estimate_rem}."
  ))
}

#' @rdname evaluate_model
#' @export
evaluate_model.result.goldfish <- function(
  x,
  at = stats::coef(x),
  return = c("loglik", "score"),
  preprocessed = NULL,
  backend = NULL,
  recall_at = c(1L, 5L, 10L),
  ...
) {
  abort_if_stale_result(x, "a model evaluation")
  quantities <- resolve_evaluate_return(return)
  prep <- resolve_preprocessed(preprocessed, x)
  backend <- resolve_evaluate_backend(backend, x, quantities)
  pars <- resolve_evaluate_at(at, x)

  spec <- prep$model_spec %||% x$model_spec
  needs <- evaluate_needs(quantities)
  res <- evaluate_engine_once(
    spec = spec,
    prep = prep,
    pars = pars,
    backend = backend,
    needs = needs
  )
  assemble_evaluation(res, quantities, x, spec, prep, pars, backend, recall_at)
}

# The vocabulary check. Unknown names abort naming the valid ones rather than
# being dropped, so a typo cannot read as "that quantity was not available".
resolve_evaluate_return <- function(return, call = rlang::caller_env()) {
  if (!is.character(return) || length(return) == 0) {
    cli::cli_abort(
      "{.arg return} must name at least one quantity to compute.",
      call = call
    )
  }
  unknown <- setdiff(return, EVALUATE_QUANTITIES)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown {.arg return} quantit{?y/ies} {.val {unknown}}.",
        "i" = "Valid quantities are {.val {EVALUATE_QUANTITIES}}."
      ),
      call = call
    )
  }
  intersect(EVALUATE_QUANTITIES, return)
}

# The backend to evaluate on: the one that produced the fit unless the caller
# names another, checked against the same capability table estimation uses.
resolve_evaluate_backend <- function(
  backend,
  x,
  quantities,
  call = rlang::caller_env()
) {
  backend <- backend %||% x$backend
  if (is.null(backend)) {
    cli::cli_abort(
      c(
        "This fit does not record which backend produced it.",
        "i" = "Name one with {.arg backend}."
      ),
      call = call
    )
  }
  backend <- resolve_backend(backend)
  primitives <- unique(unname(EVALUATE_PRIMITIVE_OF[quantities]))
  primitives <- primitives[!is.na(primitives)]
  check_diagnostic_support(primitives, backend, call = call)
  backend
}

# The vector to evaluate at. A named vector seeds the coefficients it names on
# top of the fitted ones -- the same convention `initial_parameters` uses --
# so evaluating one constrained coefficient does not mean respelling the rest.
resolve_evaluate_at <- function(at, x, call = rlang::caller_env()) {
  fitted <- stats::coef(x, complete = TRUE)
  if (!is.numeric(at)) {
    cli::cli_abort("{.arg at} must be a numeric vector.", call = call)
  }
  if (is.null(names(at))) {
    if (length(at) != length(fitted)) {
      cli::cli_abort(
        c(
          "{.arg at} must have one value per coefficient.",
          "x" = "It has {length(at)}; the model has {length(fitted)}."
        ),
        call = call
      )
    }
    return(unname(as.numeric(at)))
  }
  unknown <- setdiff(names(at), names(fitted))
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg at} names coefficient{?s} the model does not have:
         {.val {unknown}}.",
        "i" = "Its coefficients are {.val {names(fitted)}}."
      ),
      call = call
    )
  }
  fitted[names(at)] <- at
  unname(as.numeric(fitted))
}

# Which engine flags the requested quantities imply. Kept separate from the
# assembly so the cost of a request is readable in one place: nothing beyond
# the log-likelihood, score and information is computed unless asked for.
evaluate_needs <- function(quantities) {
  list(
    scores = "event_scores" %in% quantities,
    ranks = any(c("ranks", "recall") %in% quantities),
    margins = "margins" %in% quantities,
    total_rate = any(c("total_rate", "conditional_logl") %in% quantities),
    probabilities = "probabilities" %in% quantities
  )
}

# One pass through the same engine estimation iterates, built from the
# preprocessed statistics rather than from a formula. `seed_intercept = FALSE`
# throughout: an evaluation uses the supplied vector verbatim, where estimation
# would replace an unseeded time intercept with its data-derived start.
evaluate_engine_once <- function(spec, prep, pars, backend, needs) {
  has_intercept <- isTRUE(prep$has_intercept %||% spec$has_intercept)
  is_rate_model <- identical(risk_set_axis(spec), "sender")
  is_two_mode <- isTRUE(spec$is_two_mode)
  if (identical(backend, "r")) {
    engine <- make_r_engine_evaluator(
      spec = spec,
      stats_list = prep,
      parameters = pars,
      nodes = evaluate_nodes_frame(prep, side = 1L),
      nodes2 = evaluate_nodes_frame(prep, side = 2L),
      has_intercept = has_intercept,
      is_rate_model = is_rate_model,
      is_two_mode = is_two_mode,
      seed_intercept = FALSE
    )
  } else {
    engine <- make_engine_evaluator(
      spec = spec,
      stats_list = prep,
      parameters = pars,
      backend = backend,
      has_intercept = has_intercept,
      is_rate_model = is_rate_model,
      is_two_mode = is_two_mode,
      seed_intercept = FALSE
    )
  }
  engine$evaluate(
    engine$parameters,
    needs$scores,
    needs$ranks,
    needs$margins,
    needs$total_rate,
    needs$probabilities
  )
}

# The node frame a side's actor count and labels come from. Only the row count
# and the labels are ever read, and `node_lookup` is the fit's own index
# resolver, so it is the one source both sides are taken from. A one-mode model
# carries side 1 only, and both of its axes are that side.
evaluate_nodes_frame <- function(prep, side) {
  lookup <- prep$node_lookup
  if (is.null(lookup)) {
    cli::cli_abort(c(
      "The preprocessed statistics carry no node lookup.",
      "i" = "Re-run {.fn compute_statistics} on a {.cls stocnet} data object."
    ))
  }
  rows <- lookup[lookup$side == side, , drop = FALSE]
  if (nrow(rows) == 0) {
    rows <- lookup[lookup$side == 1L, , drop = FALSE]
  }
  data.frame(label = rows$label, stringsAsFactors = FALSE)
}

# The two engines spell their pass differently -- the compiled one returns
# `derivative` / `fisher` / `intervalLogL` / `event_probabilities`, the r one
# `score` / `informationMatrix` / `eventLogL` / `pMatrix` -- so the one place
# that difference is reconciled is here, before any quantity is assembled.
normalize_engine_pass <- function(res, backend) {
  if (identical(backend, "r")) {
    return(list(
      loglik = res$logLikelihood,
      score = res$score,
      information = res$informationMatrix,
      interval_loglik = res$eventLogL,
      probabilities = res$pMatrix
    ))
  }
  list(
    loglik = res$logLikelihood,
    score = res$derivative,
    information = res$fisher,
    interval_loglik = res$intervalLogL,
    probabilities = res$event_probabilities
  )
}

# Map the engine's raw pass into the requested quantities, in the vocabulary's
# order. Per-event components are named and labeled exactly as the same
# components are when estimation stores them, so a recomputed quantity and a
# stored one are the same object in different places.
assemble_evaluation <- function(
  res,
  quantities,
  x,
  spec,
  prep,
  pars,
  backend,
  recall_at
) {
  common <- normalize_engine_pass(res, backend)
  coefficient_names <- names(stats::coef(x, complete = TRUE))
  out <- list()
  for (quantity in quantities) {
    out[[quantity]] <- switch(
      quantity,
      loglik = as.numeric(common$loglik),
      score = stats::setNames(as.numeric(common$score), coefficient_names),
      information = common$information,
      interval_loglik = as.numeric(common$interval_loglik),
      total_rate = evaluate_optional_numeric(res$total_rate),
      conditional_logl = evaluate_optional_numeric(res$conditional_logl),
      event_scores = evaluate_event_scores(res$event_scores, x),
      ranks = res$observed_rank,
      recall = evaluate_recall(res$observed_rank, recall_at),
      margins = evaluate_margins(res, spec, prep, backend),
      probabilities = common$probabilities
    )
  }
  out$backend <- backend
  out$at <- pars
  out
}

evaluate_optional_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) NULL else as.numeric(x)
}

evaluate_event_scores <- function(scores, x) {
  if (is.null(scores)) {
    return(NULL)
  }
  if (ncol(scores) == nrow(x$names)) {
    colnames(scores) <- rownames(x$names)
  }
  scores
}

# Recall at k: the share of dependent events whose realized alternative ranked
# within the top k, under the same tie rule the ranks themselves use -- they
# ARE the ranks, so the two cannot disagree about which alternatives are tied.
# Right-censored intervals carry no realized alternative and are excluded.
evaluate_recall <- function(ranks, recall_at) {
  if (is.null(ranks)) {
    return(NULL)
  }
  dependent <- ranks[!is.na(ranks)]
  stats::setNames(
    vapply(recall_at, function(k) mean(dependent <= k), numeric(1)),
    paste0("recall_at_", recall_at)
  )
}

# The margins of this pass, through the same labeling every stored margin goes
# through, so an evaluated margin and a stored one differ only in provenance.
# The r backend's step already returns them shaped; the compiled kernels return
# the raw accumulators, which the shared assembly shapes.
evaluate_margins <- function(res, spec, prep, backend) {
  margins <- if (identical(backend, "r")) {
    res$margins
  } else {
    assemble_engine_margins(res)
  }
  label_margins(
    margins,
    axis = risk_set_axis(spec),
    nodes = evaluate_nodes_frame(prep, side = 1L),
    nodes2 = evaluate_nodes_frame(prep, side = 2L),
    is_exact_time = identical(risk_set_normalizer(spec), "poisson")
  )
}

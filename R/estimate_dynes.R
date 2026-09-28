#' Estimate a DyNES model by ascent-based Monte Carlo EM
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `estimate_dynes()` estimates the free parameters of a joint specification
#' whose modeled panel layers are only partially observed, by the
#' ascent-based Monte Carlo EM (ABMCEM) algorithm. Each EM iteration augments a
#' pool of endpoint-hitting event sequences, weights the pool at the current
#' parameters (the sample-reuse importance weights of Levine and Casella,
#' 2001), takes a stochastic-gradient M-step, and accepts the step, grows the
#' pool, or stops according to the estimated ascent Q and its standard error.
#'
#' @details
#' The specification is completed once, on entry: a modeled process missing
#' its rate or its choice receives a default with no free parameter (a pinned
#' intercept-only rate, or a uniform choice). Completed and all-`offset()`
#' processes are never estimated, but their log-likelihood still enters the
#' density every augmented path is weighted by, since it changes with the path.
#'
#' The starting values are zero over the free coefficients, the
#' `initial_parameters` of [set_algorithm_em()], or -- with
#' `set_augmenter_options(warm_start = TRUE)` -- one independent Newton fit per
#' process on a single augmented sequence. A process whose warm-start fit fails
#' starts at zero, with a warning naming it.
#'
#' Random numbers, and what `seed` and `n_cores` promise, are described under
#' "Reproducibility" in [set_algorithm_em()].
#'
#' @param spec A joint specification from [make_joint_specification()].
#' @param control_algo The algorithm controls, from [set_algorithm_em()].
#' @param control_prep The preprocessing controls, from [set_preprocessing()].
#'
#' @return A `goldfishDynesFit`: the last accepted `parameters` (named by the
#'   composite labels of [coef_layout()]), `converged`, the number of accepted
#'   `iterations`, the `em_trace` (one row per decision pass), the final `pool`
#'   with its per-sequence `loglik` and normalized `weights`, the `seed` the run
#'   used, the completed `spec` and the `authored_spec`, and two covariance
#'   matrices from one full-pool evaluation at the returned estimate, made
#'   whether or not the run converged. `covariance`, returned by `vcov()`, is
#'   the sandwich \eqn{I^{-1} (\sum_i w_i s_i s_i^\top) I^{-1}} with
#'   \eqn{I = \sum_i w_i I_i}. `mc_covariance` is the Monte-Carlo error of the
#'   estimate, \eqn{I^{-1} (\sum_i w_i^2 (s_i - \bar s)(s_i - \bar s)^\top)
#'   I^{-1}}. Both use the importance weights \eqn{w_i} over the whole pool.
#'
#'   `coef()` and `vcov()` are flat over the free coefficients. [coef_layout()]
#'   groups them by process, with both errors, and lists every fixed
#'   coefficient: an `offset()` term, or a completed default such as a pinned
#'   rate, is a `fixed = TRUE` row with its frozen value and no error.
#'   `print()`, `summary()`, `tidy()` and `glance()` report both errors and the
#'   convergence status. `logLik()` refuses, and `AIC()` and `BIC()` with it:
#'   the estimator ascends a Monte-Carlo estimate of the likelihood, not an
#'   exact one. `simulate()` runs the specification the fit holds, at its
#'   estimates.
#'
#' @references Levine, R. A., & Casella, G. (2001). Implementations of the
#' Monte Carlo EM algorithm. *Journal of Computational and Graphical
#' Statistics*, 10(3), 422--439. \doi{10.1198/106186001317115045}
#' @seealso [set_algorithm_em()], [coef_layout()], [set_parameters()]
#' @keywords internal
estimate_dynes <- function(
  spec,
  control_algo = set_algorithm_em(),
  control_prep = set_preprocessing()
) {
  lifecycle::signal_stage("experimental", "estimate_dynes()")
  if (!inherits(spec, "goldfishJointSpec")) {
    cli::cli_abort(
      c(
        "{.arg spec} must be a {.cls goldfishJointSpec}.",
        "i" = "Build one with {.fn make_joint_specification}."
      )
    )
  }
  if (!inherits(control_algo, "goldfishAlgoEm")) {
    cli::cli_abort(
      c(
        "{.arg control_algo} must be a {.cls goldfishAlgoEm}.",
        "i" = "Build one with {.fn set_algorithm_em}."
      )
    )
  }
  call <- rlang::current_env()

  # Completed once, here: every proposal evaluator the loop builds reads this
  # same object, so a pinned rate cannot move between proposals.
  completed <- complete_generative_spec(spec, consumer = "estimate_dynes")
  # A pinned rate reads no coefficient and gets no standard error; the pin's
  # provenance (a wave Hamming diff here) is announced by each consumer at its
  # own entry, in its own wording.
  warn_pinned_rates(completed, consumer = "estimate_dynes", call = call)
  decomposition <- decompose_joint_spec(completed, call = call)
  layout <- coef_layout(completed)
  free <- !layout$fixed
  labels <- paste0(layout$process[free], ": ", layout$name[free])
  if (length(labels) == 0L) {
    cli::cli_abort(
      c(
        "The specification has no free coefficient to estimate.",
        "i" = "Every effect is an {.fn offset} or a completed default."
      ),
      call = call
    )
  }
  augmenter <- resolve_dynes_augmenter(completed, control_algo, call = call)

  seed <- control_algo$seed %||% sample.int(.Machine$integer.max, 1L)
  restore_rng <- set_dynes_rng(seed)
  on.exit(restore_rng(), add = TRUE)
  keyer <- new_draw_keyer(get_rng_state())
  seam <- dynes_map_seam(control_algo, call = call)
  on.exit(seam$close(), add = TRUE, after = FALSE)
  augmenter <- keyed_augmenter(
    augmenter,
    keyer,
    seam$map,
    control_algo$augmenter$routine
  )

  theta0 <- dynes_initial_theta(
    control_algo,
    completed,
    labels,
    decomposition,
    augmenter,
    control_prep,
    call = call
  )

  res <- run_abem_loop(
    theta0,
    decomposition,
    augmenter,
    control_algo,
    spec = completed,
    control_prep = control_prep,
    map_fn = seam$map,
    call = call
  )
  res$parameters <- stats::setNames(as.numeric(res$parameters), labels)
  res$seed <- seed
  res$spec <- completed
  new_dynes_fit(res, spec, control_algo, fit_call = sys.call(), call = call)
}

# The augmenter the control selects. The augment_seq_*() augmenters land with
# the sequence-augmentation work; until then no routine has one, and
# estimate_dynes() stays unexported so no user can reach this abort.
resolve_dynes_augmenter <- function(spec, control, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "No augmenter is available for {.arg routine} =
       {.val {control$augmenter$routine}} yet.",
      "i" = "The sequence augmenters are not part of this version of
             {.pkg goldfish}."
    ),
    class = "goldfish_dynes_no_augmenter",
    call = call
  )
}

# The pool decomposition of a completed joint specification: one sub-model per
# fid in process_map order, and the pool engine spec every pooled sequence is
# walked through. Each sub-model numbers its free coefficients from
# coef_layout()'s `index`: the flat theta the loop estimates is that layout's
# free slots in canonical order (a timed rate's intercept included, offsets
# never), which is also how the proposal evaluator maps theta onto the walk for
# rate queries. A completed default or an all-offset fid has no free slot and
# becomes a fixed-only sub-model; a completed timed rate carries its pinned
# plateaus. A flavored fid needs nothing extra: the joint walk routes each
# event to its own flavor's fids. The rendered label rides along for messages.
decompose_joint_spec <- function(spec, call = rlang::caller_env()) {
  map <- spec$process_map
  layout <- coef_layout(spec)
  bundles <- joint_fid_bundles(spec)
  sub_models <- lapply(seq_len(nrow(map)), function(i) {
    fid <- map$fid[[i]]
    entry <- bundles[[as.character(fid)]]
    sub_model <- new_pool_submodel(
      entry$bundle$formula,
      model = entry$model,
      sub_model = entry$sub_model,
      theta_index = layout$index[layout$fid == fid & !layout$fixed],
      pinned_rate = spec$completed_rates[[as.character(fid)]],
      fid = fid,
      call = call
    )
    sub_model$label <- render_process_label(map, fid)
    sub_model
  })
  new_pool_decomposition(sub_models, pool_engine_spec(spec), call = call)
}

# theta0 over the free slots `labels`: the warm start when it is on (the
# explicit start, when given, has already disabled it in set_algorithm_em()),
# else the user's goldfishParams with unpinned slots at zero, else zero.
dynes_initial_theta <- function(
  control,
  spec,
  labels,
  decomposition,
  augmenter,
  control_prep,
  call
) {
  if (isTRUE(control$augmenter$warm_start)) {
    return(dynes_warm_start(
      decomposition,
      augmenter,
      length(labels),
      control_prep,
      call = call
    ))
  }
  theta <- numeric(length(labels))
  start <- control$initial_parameters
  if (!is.null(start)) {
    reconcile_joint_parameters(
      start,
      spec,
      arg = "initial_parameters",
      call = call
    )
    values <- joint_initial_parameters(start, call = call)
    at <- match(labels, names(values))
    known <- !is.na(at)
    theta[known] <- values[at[known]]
    theta[is.na(theta)] <- 0
  }
  theta
}

# One augmentation, drawn at iteration 0 from theta = 0, walked once as a pool
# entry is, and an independent default Newton fit per free fid on its joint
# statistics. The fits only need to land nearer the optimum than zero, so they
# need not converge; a fit that fails -- a single sequence easily makes a
# sub-model's information matrix singular -- leaves that fid at zero with a
# warning, and the other fids keep their fits.
dynes_warm_start <- function(
  decomposition,
  augmenter,
  n_free,
  control_prep,
  call
) {
  theta <- numeric(n_free)
  drawn <- augmenter$draw(0L, 1L, theta)[[1L]]
  stats <- tryCatch(
    preprocess_pool_sequence(
      decomposition$engine_spec,
      drawn$data,
      control_prep = control_prep,
      call = NULL
    ),
    error = function(e) NULL
  )
  for (sm in decomposition$sub_models) {
    if (sm$fixed_only) {
      next
    }
    # A walk that failed leaves no statistics, and every fid falls back.
    fitted <- if (!is.null(stats)) {
      tryCatch(
        dynes_warm_start_fit(sm, drawn$data, fid_stats(stats, sm)),
        error = function(e) NULL
      )
    }
    if (length(fitted) != length(sm$theta_index) || !all(is.finite(fitted))) {
      cli::cli_warn(
        c(
          "!" = "The warm-start fit for {.val {sm$label}} failed on the drawn
                 sequence; its free parameters start at zero.",
          "i" = "The other processes keep their warm-start values."
        ),
        class = "goldfish_dynes_warm_start_fallback",
        call = call
      )
      next
    }
    theta[sm$theta_index] <- fitted
  }
  theta
}

# A default set_algorithm_newton() fit of one sub-model over its statistics
# from the drawn sequence's joint walk, returning its estimates at the free
# coefficients. Non-convergence warnings are muffled: a warm start does not
# need a converged fit.
dynes_warm_start_fit <- function(sub_model, data, stats) {
  sub_model <- with_coefficient_masks(list(sub_model), data, call = NULL)[[1L]]
  fit <- suppressWarnings(estimate_wrapper(
    x = sub_model$engine_formula,
    model = sub_model$model,
    sub_model = sub_model$sub_model,
    data = data,
    preprocessed = stats,
    control_algo = set_algorithm_newton(),
    verbose = FALSE,
    progress = FALSE
  ))
  unname(fit$parameters[sub_model$coefficients$free])
}

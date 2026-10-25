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
#' At least one panel layer must be a modeled dependent process, with every
#' flavor of a modeled panel layer modeled; without one, nothing is latent and
#' the specification is a DyNAM. Windowed effects may read observed layers but
#' not a modeled panel layer. A process whose formulas read no modeled panel
#' layer is estimated jointly with the rest, which gives what estimating it on
#' its own would; a message names such processes.
#'
#' Estimation runs from the observation start, the first value of
#' `wave_times`, to the last wave, whatever the `start_time` and `end_time` of
#' `control_prep`: modeled events before the first wave are scored, and those
#' after the last wave are dropped.
#'
#' The specification is completed once, on entry: a modeled process missing
#' its rate or its choice receives a default with no free parameter (a pinned
#' intercept-only rate, bucketed per wave period, or a uniform choice).
#' Completed and all-`offset()` processes are never estimated, but their
#' log-likelihood still enters the density every augmented path is weighted
#' by, since it changes with the path.
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
#' The standard errors use Louis's (1982) identity: the observed-data
#' information is the expected complete-data information given the data,
#' \eqn{-Q''}, less the conditional variance of the complete-data score,
#' \eqn{E[s s^\top \mid y] - \bar s \bar s^\top} (McLachlan and Krishnan,
#' 2008, ch. 4). The bread-meat-bread form
#' \eqn{(-Q'')^{-1} E[s s^\top \mid y] (-Q'')^{-1}} does not apply. Its middle
#' term is a sampling variance only when it sums scores over independent
#' units, and here the scores belong to imputed paths of the same data, so
#' their spread is the information those paths miss. As Ruth (2024) puts it,
#' that form "applies to Monte Carlo variability within an iteration; it does
#' not measure sampling variability due to the observed data".
#'
#' @param spec A joint specification from [make_joint_specification()].
#' @param control_algo The algorithm controls, from [set_algorithm_em()].
#' @param control_prep The preprocessing controls, from [set_preprocessing()].
#' @param wave_times The wave grid the latent sequences are augmented on: the
#'   observation start followed by every wave, strictly increasing. By default
#'   it is inferred from the data, as the distinct times of the modeled panel
#'   layers' rows, opened by the `start_time` of `control_prep` when one is
#'   set and otherwise by the first recorded wave, whose state is then the
#'   initial condition.
#'
#' @return A `goldfishDynesFit`: the last accepted `parameters` (named by the
#'   composite labels of [coef_layout()]), `converged`, the number of accepted
#'   `iterations`, the `em_trace` (one row per decision pass), the final `pool`
#'   with its per-sequence `loglik` and normalized `weights`, the `seed` the run
#'   used, the completed `spec` and the `authored_spec`, and two covariance
#'   matrices from one full-pool evaluation at the returned estimate, made
#'   whether or not the run converged. `covariance`, returned by `vcov()`, is
#'   the inverse of Louis's observed-data information
#'   \eqn{I_{obs} = \sum_i w_i J_i - \sum_i w_i s_i s_i^\top +
#'   \bar s \bar s^\top}, from each sequence's complete-data score \eqn{s_i}
#'   and information \eqn{J_i}, with \eqn{\bar s = \sum_i w_i s_i}: the
#'   complete-data information less the variance of the scores across the
#'   imputed paths, which is the information the latent paths miss.
#'   `mc_covariance` is the Monte-Carlo error of the estimate,
#'   \eqn{I_{obs}^{-1} (\sum_i w_i^2 (s_i - \bar s)(s_i - \bar s)^\top)
#'   I_{obs}^{-1}}. Both use the run's normalized weights \eqn{w_i} over the
#'   whole pool. Louis's identity is exact for untransformed importance
#'   weights; under a weight `transformation` of `"clipping"` or
#'   `"smoothing"` (see [set_weights_options()]) both covariances are
#'   approximations. Both are `NA` when the pool stopped with no finite
#'   likelihood, when the pooled complete-data information is singular, or
#'   when the Monte-Carlo estimate of \eqn{I_{obs}} is not positive definite;
#'   the last two warn. `covariance_status` records which applied first:
#'   `"pool_dropped"`, `"singular"` or `"not_pd"`, and `"ok"` otherwise.
#'   `fraction_missing` is the largest eigenvalue of
#'   \eqn{I_c^{-1} (\sum_i w_i s_i s_i^\top - \bar s \bar s^\top)}, the
#'   share of the complete information the latent paths miss in the worst
#'   direction: below one when \eqn{I_{obs}} is positive definite, at least one
#'   when it is not, and `NA` when the complete information is singular or the
#'   pool was dropped. `covariance_ess` is the effective sample size
#'   \eqn{1 / \sum_i w_i^2} of the weights the covariances used, and `NA` on a
#'   dropped pool and under `weighting = "uniform"`, where it would equal the
#'   number of kept sequences; the `em_trace` chain diagnostics report the
#'   mixing of a uniform-weighting run. Neither warns. `information` is the
#'   decomposition the covariance inverts, three matrices over the free
#'   coefficients: `complete` (\eqn{I_c = \sum_i w_i J_i}), `missing`
#'   (\eqn{\sum_i w_i s_i s_i^\top - \bar s \bar s^\top}) and `observed`
#'   (\eqn{I_{obs}}, their difference). It is kept when an inverse fails, and
#'   is `NULL` on a dropped pool. `vcov(information = "complete_data")`
#'   returns \eqn{I_c^{-1}}.
#'
#'   `newton_se` is the largest coordinate of the exact Newton step from the
#'   returned estimate to the optimum of the final pool's importance-weighted
#'   objective, in standard errors,
#'   \eqn{\max_j |(I_{obs}^{-1} \bar s)_j| / SE_j}, read off the same
#'   covariance pass. `at_optimum` is `TRUE` when it is below 0.1, a fixed
#'   threshold independent of `sgd_tolerance` (see [set_sgd_options()]) and of
#'   the M-step's `convergence` mode. Under `use = "resampling"` (see
#'   [set_weights_options()]) it still reads the importance-weighted pool, not
#'   the resampled multiset the M-step optimized. Both are `NA` unless
#'   `covariance_status` is `"ok"`. `converged` reports the EM stop rule alone;
#'   the two fail for different reasons, so neither is read from the other.
#'   `print()` and `summary()` state both, and `use` records the run's
#'   `use` so that a resampling fit's status says which objective the gate
#'   reads.
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
#'
#' Louis, T. A. (1982). Finding the observed information matrix when using
#' the EM algorithm. *Journal of the Royal Statistical Society: Series B
#' (Methodological)*, 44(2), 226--233.
#' \doi{10.1111/j.2517-6161.1982.tb01203.x}
#'
#' McLachlan, G. J., & Krishnan, T. (2008). *The EM Algorithm and
#' Extensions* (2nd ed.). Wiley. \doi{10.1002/9780470191613}
#'
#' Ruth, W. (2024). A review of Monte Carlo-based versions of the EM
#' algorithm. *arXiv preprint* arXiv:2401.00945.
#' \doi{10.48550/arXiv.2401.00945}
#' @seealso [set_algorithm_em()], [coef_layout()], [set_parameters()]
#' @export
estimate_dynes <- function(
  spec,
  control_algo = set_algorithm_em(),
  control_prep = set_preprocessing(),
  wave_times = NULL
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
  abort_without_modeled_panel(spec, call = call)
  abort_on_panel_window(spec, call = call)
  wave_times <- panel_wave_grid(
    spec,
    start_time = control_prep$start_time,
    wave_times = wave_times,
    call = call
  )
  # Every path that scores a sequence -- pool entry, the proposal evaluator,
  # the warm start -- runs from the observation start to the last wave:
  # modeled events before the first wave are scored, later ones are dropped,
  # and each timed rate's trailing compensator ends at the last wave.
  control_prep$start_time <- wave_times[[1L]]
  control_prep$end_time <- wave_times[[length(wave_times)]]
  # An actor an interval's draw reads must be present over the whole interval,
  # or the draw can strand on its zero rate. Snapped before completion so the
  # pins, the target and every proposal read one composition.
  # `spec` stays the authored one the fit returns.
  snapped <- snap_composition_to_waves(spec, wave_times, call = call)

  # Completed once, here, on the wave grid the recipe is compiled on: every
  # proposal evaluator the loop builds reads this same object, so a pinned
  # rate cannot move between proposals. Without the grid a multi-wave pin
  # would fall back to one flat plateau over the data's event times.
  completed <- complete_generative_spec(
    snapped,
    consumer = "estimate_dynes",
    wave_times = wave_times
  )
  assert_pins_on_wave_grid(completed, wave_times, call = call)
  # A pinned rate reads no coefficient and gets no standard error. A completed
  # rate's pin was already announced by the completion above, with the count
  # source it was pinned from; this warns only for a pin it did not announce.
  warn_pinned_rates(completed, consumer = "estimate_dynes", call = call)
  inform_separable_fids(spec)
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
  # The wave diffs, chains and wave states are theta-free and the same for
  # every draw, so they are compiled once, here, and shared by every draw.
  recipe <- compile_augmentation_recipe(completed, wave_times, call = call)
  augmenter <- resolve_dynes_augmenter(
    completed,
    recipe,
    control_algo,
    control_prep = control_prep,
    call = call
  )

  seed <- control_algo$seed %||% sample.int(.Machine$integer.max, 1L)
  restore_rng <- set_dynes_rng(seed)
  on.exit(restore_rng(), add = TRUE)
  keyer <- new_draw_keyer(get_rng_state())
  seam <- dynes_map_seam(control_algo, call = call)
  on.exit(seam$close(), add = TRUE, after = FALSE)
  # The static header reaches the workers once, here; the store is released
  # before the daemons stop.
  store <- open_pool_store(
    seam,
    pool_store_header(
      decomposition,
      length(labels),
      control_prep,
      completed$data,
      call = call
    )
  )
  on.exit(close_pool_store(store), add = TRUE, after = FALSE)
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
    store = store,
    call = call
  )
  warn_fixed_term_degenerate(store, call = call)
  warn_pool_degenerate(store, call = call)
  res$parameters <- stats::setNames(as.numeric(res$parameters), labels)
  res$seed <- seed
  res$spec <- completed
  new_dynes_fit(
    res,
    spec,
    control_algo,
    fit_call = sys.call(),
    call = call,
    start = theta0
  )
}

# One warning per run for the drawn sequences a fixed-only term was lost to,
# from the store's run-long tally. Every fixed-only term (a pinned rate, a
# completed uniform choice, an all-offset fid) is derived by goldfish from the
# specification and the data, so a non-finite one is a goldfish bug, not
# something an argument of the user's could fix.
warn_fixed_term_degenerate <- function(store, call = rlang::caller_env()) {
  tally <- store$tally
  if (tally$fixed_term == 0L) {
    return(invisible(NULL))
  }
  n <- tally$fixed_term
  n_drawn <- tally$built
  labels <- tally$labels
  dropped <- if (pool_lost_to_fixed_terms(tally)) {
    c(
      "!" = "Every sequence in the pool was dropped; estimation stopped at the
             last accepted parameters and the fit is flagged
             {.code converged = FALSE}."
    )
  }
  cli::cli_warn(
    c(
      "!" = "{n} of {n_drawn} drawn sequences had a non-finite or unevaluable
             fixed-only log-likelihood in {.val {labels}} and were dropped as
             degenerate.",
      dropped,
      "i" = "This is likely a bug in goldfish; please report it."
    ),
    class = "goldfish_dynes_fixed_term_degenerate",
    call = call
  )
}

# One warning when the loop stopped on a pool in which no sequence carried
# weight, unless every sequence was lost to a fixed-only term: the fixed-term
# warning then reports the stop itself. The counts are those of the pool the
# loop stopped on.
warn_pool_degenerate <- function(store, call = rlang::caller_env()) {
  tally <- store$tally
  if (!isTRUE(tally$pool_dropped) || pool_lost_to_fixed_terms(tally)) {
    return(invisible(NULL))
  }
  lost <- tally$pool_lost
  causes <- c(
    "*" = "{lost[['walk']]} failed {?its/their} preprocessing walk.",
    "*" = "{lost[['evaluation']]} had a non-finite likelihood at the current
           parameters.",
    "*" = "{lost[['fixed_term']]} had a non-finite fixed-only term, reported
           in a separate warning."
  )[lost[c("walk", "evaluation", "fixed_term")] > 0L]
  cli::cli_warn(
    c(
      "!" = "No sequence of the pool had a finite likelihood; estimation
             stopped at the last accepted parameters and the fit is flagged
             {.code converged = FALSE}.",
      "i" = "Of the {sum(lost)} pooled sequences:",
      causes
    ),
    class = "goldfish_dynes_pool_degenerate",
    call = call
  )
}

# Whether the loop stopped on a pool every sequence of which was lost to a
# fixed-only term.
pool_lost_to_fixed_terms <- function(tally) {
  lost <- tally$pool_lost
  isTRUE(tally$pool_dropped) &&
    lost[["fixed_term"]] > 0L &&
    lost[["walk"]] == 0L &&
    lost[["evaluation"]] == 0L
}

# Something must be latent for DyNES to estimate. A panel layer is augmented
# iff a composed specification models it as its dependent process -- no
# separate flag -- and make_joint_specification() records exactly those layers
# in `modeled_panel`. The gate is that, not the `coupled` column: a modeled
# panel layer deriving no support constraint (a count panel) can have every
# fid separable while its event times are wholly latent. With no modeled panel
# process every panel reference is a static step-covariate, which is a DyNAM.
abort_without_modeled_panel <- function(spec, call = rlang::caller_env()) {
  if (length(spec$modeled_panel) > 0L) {
    return(invisible(spec))
  }
  cli::cli_abort(
    c(
      "No panel layer of {.arg spec} is a modeled dependent process.",
      "x" = "Nothing is latent, so this specification is a DyNAM, not a DyNES.",
      "i" = "Its panel layers would only be considered as static exogenous
             covariates, jumping at their wave times.",
      "i" = "Estimate its processes with {.fn estimate_dynam}."
    ),
    class = "goldfish_dynes_no_modeled_panel",
    call = call
  )
}

# A window over a modeled panel layer would decay events whose placement
# differs across augmented paths; the data path only knows panel layers as
# wave snapshots, so it is refused here, before anything is drawn. Windows
# over observed layers are fine.
abort_on_panel_window <- function(spec, call = rlang::caller_env()) {
  windowed <- unlist(lapply(joint_fid_bundles(spec), function(entry) {
    vapply(
      entry$bundle$parsed$window_derivations,
      `[[`,
      character(1),
      "source_name"
    )
  }))
  layers <- intersect(unique(windowed), spec$modeled_panel)
  if (length(layers) == 0L) {
    return(invisible(spec))
  }
  cli::cli_abort(
    c(
      "A {.arg window} cannot read the modeled panel
       {cli::qty(length(layers))}layer{?s} {.val {layers}}.",
      "x" = "Windows over a modeled panel layer, whose events are augmented
             between waves, are not supported in this version.",
      "i" = "Windows are supported over observed layers only; drop the
             {.arg window} argument."
    ),
    class = "goldfish_dynes_panel_window",
    call = call
  )
}

# Every pin of a modeled panel layer's rate must be bucketed on the grid the
# augmenters draw on. A pin on another grid can be -Inf over a period in which
# the draws place events, and every such sequence would be dropped as fixed-
# term degenerate. Both come from the one grid estimate_dynes() resolves, so a
# mismatch is a goldfish bug.
assert_pins_on_wave_grid <- function(spec, wave_times, call) {
  map <- spec$process_map
  panel_fids <- as.character(map$fid[map$layer %in% spec$modeled_panel])
  pins <- spec$completed_rates[intersect(
    names(spec$completed_rates),
    panel_fids
  )]
  off_grid <- Filter(
    function(pin) {
      !identical(as.numeric(stored_pin_grid(pin)), as.numeric(wave_times))
    },
    pins
  )
  if (length(off_grid) == 0L) {
    return(invisible(spec))
  }
  labels <- render_process_label(map, as.integer(names(off_grid)))
  cli::cli_abort(
    c(
      "The pinned {cli::qty(length(labels))}rate{?s} of {.val {labels}}
       {?is/are} not bucketed on the augmentation wave grid
       {.val {as.numeric(wave_times)}}."
    ),
    class = "goldfish_dynes_pin_grid_mismatch",
    .internal = TRUE,
    call = call
  )
}

# A fid reading no modeled panel layer touches no latent path, so estimating
# it jointly gives what estimating it on its own would. Joint estimation
# handles it correctly; the message only points out the alternative. The
# separability is make_joint_specification()'s (a fid on a modeled panel
# layer is never separable), read off the authored specification so that the
# defaults completion adds, which carry no coefficient, are not named.
inform_separable_fids <- function(spec) {
  separable <- joint_separable(spec)
  if (!any(separable)) {
    return(invisible(spec))
  }
  labels <- render_process_label(
    spec$process_map,
    spec$process_map$fid[separable]
  )
  cli::cli_inform(
    c(
      "i" = "{cli::qty(length(labels))}{?This process reads/These processes
             read} no modeled panel layer: {.val {labels}}.",
      " " = "{cli::qty(length(labels))}{?Its/Their} likelihood terms touch no
             latent path, so estimating {?it/them} jointly equals estimating
             {?it/them} on {?its/their} own."
    ),
    class = "goldfish_dynes_separable_fids"
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

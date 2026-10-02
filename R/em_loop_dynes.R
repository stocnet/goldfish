# The ascent-based Monte Carlo EM (ABMCEM) loop driven by estimate_dynes(),
# written against three contracts only:
#
# * the augmenter: a list with `draw(iteration, draw_index, theta)`, returning
#   one draw per entry of `draw_index` -- each a list with the drawn `data`, its
#   finite `log_proposal` density over the full path, and optionally
#   `restarted` (a reject-and-restart fired) -- and optionally
#   `init(proposal_evaluator, theta)`, called once per EM iteration with the
#   theta_k-closed evaluator an MCMC chain reads its acceptance ratio from, and
#   `diagnostics()`, whose return is recorded in the trace;
# * the evaluator: build_sequence_pool() / evaluate_sequence_pool();
# * the optimizer: optimize_abem_sgd().
#
# Q and its standard error come from compute_q()/compute_ase() dispatching on
# the E-step class, so a new weighting scheme adds methods, not loop branches.
#
# `draw_index` counts every sequence an iteration draws -- the initial pool,
# post-acceptance top-ups, growth appends, and redraws alike -- and restarts at
# 1 with each iteration, so a draw's (iteration, draw_index) key does not
# depend on how retries chunk the draws.

# Run the loop from the free parameter vector `theta0`.
#
# * `decomposition`: the pool decomposition (new_pool_decomposition()): the
#   sub-model descriptors and the pool engine spec sequences are walked through.
# * `augmenter`: an augmenter honoring the contract above.
# * `control`: a set_algorithm_em() object.
# * `spec`: the joint specification completed once by the caller, passed
#   through unchanged to every iteration's proposal evaluator (which reads it
#   for rate queries). Re-completing it here could move a pinned rate between
#   proposals with no parameter to explain the move.
# * `map_fn`: the per-sequence map seam the pool preprocesses and evaluates
#   through (lapply() by default).
# * `store`: the resident pool store (open_pool_store()) every draw is sent
#   to; without one, the first pool opens a serial store over `map_fn`.
#
# Returns a list: the last accepted `parameters`, `converged`, the number of
# accepted `iterations`, the `em_trace` (one row per pass), and the final
# `pool` with its per-sequence `loglik` and normalized `weights` at the
# returned parameters.
run_abem_loop <- function(
  theta0,
  decomposition,
  augmenter,
  control,
  spec = NULL,
  control_prep = set_preprocessing(),
  map_fn = lapply,
  store = NULL,
  call = rlang::caller_env()
) {
  if (!inherits(control, "goldfishAlgoEm")) {
    cli::cli_abort(
      "{.arg control} must be a {.cls goldfishAlgoEm} from
       {.fn set_algorithm_em}.",
      call = call
    )
  }
  if (!is.function(augmenter$draw)) {
    cli::cli_abort(
      "{.arg augmenter} must provide a {.fn draw} function.",
      call = call
    )
  }
  weights_ctrl <- control$weights
  scheme <- if (identical(weights_ctrl$weighting, "uniform")) {
    "uniform"
  } else {
    weights_ctrl$use
  }
  routine <- control$augmenter$routine
  z_accept <- stats::qnorm(1 - control$accept_quantile)
  z_growth <- stats::qnorm(1 - control$growth_quantile)
  z_stop <- stats::qnorm(1 - control$stop_quantile)

  run_state <- new_run_warning_state()
  run_state$pool_cap_warned <- FALSE
  run <- new.env(parent = emptyenv())
  run$trace <- list()
  run$pool <- NULL
  run$loglik <- numeric(0)

  # Draw `n` sequences at `theta`, continuing the iteration's draw_index, and
  # append them to the pool (or let them replace it) with their log-likelihood
  # at `theta` cached. Either way the store receives the new sequences once.
  draw_into_pool <- function(n, iteration, theta, replace = FALSE) {
    n <- as.integer(n)
    if (n <= 0L) {
      return(0L)
    }
    draw_index <- run$next_draw + seq_len(n) - 1L
    run$next_draw <- run$next_draw + n
    drawn <- augmenter$draw(iteration, draw_index, theta)
    warn_restart_coupling_once(
      run_state,
      any(vapply(drawn, function(d) isTRUE(d$restarted), logical(1))),
      routine = routine,
      constraint = augmenter$coupling
    )
    entries <- lapply(drawn, abem_pool_entry, theta = theta, call = call)
    fresh <- replace || is.null(run$pool)
    if (fresh) {
      run$pool <- build_sequence_pool(
        decomposition,
        entries,
        theta_length = length(theta),
        control_prep = control_prep,
        map_fn = map_fn,
        store = store %||% run$pool$store
      )
      added <- seq_along(run$pool$sequences)
    } else {
      added <- length(run$pool$sequences) + seq_along(entries)
      run$pool <- append_sequence_pool(run$pool, entries, call = call)
    }
    loglik <- evaluate_sequence_pool(
      pool_subset(run$pool, added),
      theta,
      what = "loglik"
    )$loglik
    for (j in seq_along(added)) {
      run$pool$sequences[[added[[j]]]]$reference$loglik_ref <- loglik[[j]]
    }
    run$loglik <- if (fresh) loglik else c(run$loglik, loglik)
    n
  }

  pool_weights <- function() {
    compute_pool_weights(
      run$loglik,
      pool_log_proposal(pool_references(run$pool)),
      weighting = weights_ctrl$weighting,
      transformation = weights_ctrl$transformation
    )
  }

  theta_k <- theta0
  target_size <- control$n_sequences
  streak <- 0L
  accepted_iterations <- 0L

  for (iteration in seq_len(control$max_iterations)) {
    run$next_draw <- 1L
    if (is.function(augmenter$init)) {
      augmenter$init(
        make_proposal_evaluator(
          decomposition,
          theta_k,
          spec = spec,
          control_prep = control_prep,
          call = call
        ),
        theta_k
      )
    }

    # Opening the iteration: the first pool, a refresh or an ESS-guard redraw
    # of the whole pool, then a top-up to the size the last acceptance set.
    if (iteration == 1L) {
      new_draws <- draw_into_pool(target_size, iteration, theta_k)
      weights <- pool_weights()
      if (pool_carries_weight(run$loglik, weights)) {
        cold_start_ess_diagnostic(weights, weights_ctrl$ess_threshold)
      }
    } else if (isTRUE(weights_ctrl$refresh)) {
      new_draws <- draw_into_pool(
        target_size,
        iteration,
        theta_k,
        replace = TRUE
      )
    } else {
      weights <- pool_weights()
      recommend_mcmc_on_ess_collapse(run_state, weights, routine)
      guard <- ess_guard(
        weights,
        ess_threshold = weights_ctrl$ess_threshold,
        pool_size = length(weights)
      )
      new_draws <- if (guard$redraw) {
        draw_into_pool(
          max(target_size, length(run$loglik)),
          iteration,
          theta_k,
          replace = TRUE
        )
      } else {
        0L
      }
      new_draws <- new_draws +
        draw_into_pool(target_size - length(run$loglik), iteration, theta_k)
    }

    iteration_start_size <- length(run$loglik)
    growth_k <- 2L

    for (retry in 0:control$max_retries) {
      # Every pass follows a build, a redraw or a growth append. A pool in
      # which no sequence has both a finite log-likelihood and a positive
      # weight at theta_k carries no information, whatever made each one
      # degenerate -- a failed walk or a non-finite fixed-only term at entry,
      # or a non-finite evaluation at theta_k, which no entry cause records.
      # Past this point both weighting schemes fail on it: importance weights
      # are all zero, so a minibatch M-step has nothing to sample from and a
      # full-batch one leaves Q = ASE = 0, meeting the stop rule at a theta
      # that never moved; uniform Q would average over no sequence. The
      # finite log-likelihood is tested beside the weight so the stop does not
      # lean on how a weighting scheme treats a degenerate sequence.
      weights <- pool_weights()
      if (!pool_carries_weight(run$loglik, weights)) {
        if (!is.null(run$pool$store)) {
          run$pool$store$tally$pool_dropped <- TRUE
          run$pool$store$tally$pool_lost <- pool_lost_causes(run$pool)
        }
        return(abem_loop_result(
          theta_k,
          FALSE,
          accepted_iterations,
          run,
          weights
        ))
      }
      pool_size <- length(weights)
      fixed_degenerate <- sum(pool_fixed_term_lost(run$pool))
      resample_indices <- if (identical(scheme, "resampling")) {
        resample_pool(weights, weights_ctrl$resampling_scheme)
      }

      pool <- run$pool
      score_fn <- function(theta, indices) {
        scores <- evaluate_sequence_pool(
          pool_subset(pool, indices),
          theta,
          what = "score"
        )$score
        # A degenerate sequence carries zero weight; a zero score keeps its
        # NA out of a cyclic batch's weighted sum.
        scores[!is.finite(scores)] <- 0
        scores
      }
      proposal <- optimize_abem_sgd(
        theta_k,
        score_fn,
        weights,
        control = control$optimizer,
        use = if (identical(scheme, "resampling")) {
          "resampling"
        } else {
          "importance"
        },
        resample_indices = resample_indices,
        call = call
      )$parameters

      # One pass yields all three quantities: the log-likelihood for Lambda,
      # the score and information for the E-step object and the sandwich.
      at_proposal <- evaluate_sequence_pool(
        pool,
        proposal,
        what = c("loglik", "score", "fisher")
      )
      loglik_proposal <- at_proposal$loglik
      lambda <- loglik_proposal - run$loglik
      # A zero-weight (degenerate) sequence contributes nothing; its
      # non-finite difference must not turn 0 * Inf into NaN.
      lambda[weights == 0] <- 0

      estep <- new_dynes_estep(
        lambda,
        weights = weights,
        scheme = scheme,
        resampling_scheme = weights_ctrl$resampling_scheme,
        resample_indices = resample_indices,
        mcmc = identical(routine, "mcmc"),
        score = at_proposal$score,
        fisher = at_proposal$fisher
      )
      q <- compute_q(estep)
      ase <- compute_ase(estep)

      stop_hit <- isTRUE(abs(q + z_stop * ase) < control$tolerance)
      streak <- if (stop_hit) streak + 1L else 0L
      terminal <- stop_hit && streak >= control$stop_count
      accepted <- !terminal && isTRUE(q - z_accept * ase > 0)
      decision <- if (terminal) {
        "stop"
      } else if (stop_hit) {
        "stop-hit"
      } else if (accepted) {
        "accept"
      } else {
        "grow"
      }

      pool_capped <- FALSE
      if (accepted) {
        # The ascent-based MCEM sample-size rule, with sigma^2 the
        # per-sequence variance K * ASE^2 (ASE is the standard error of Q).
        requested <- ceiling(
          pool_size * as.numeric(ase)^2 * (z_accept + z_growth)^2 / q^2
        )
        pool_capped <- requested > control$max_pool && requested > pool_size
        target_size <- max(pool_size, min(control$max_pool, requested))
        if (pool_capped && !run_state$pool_cap_warned) {
          run_state$pool_cap_warned <- TRUE
          warn_pool_capped(requested, control$max_pool)
        }
      }

      se <- if (accepted && isTRUE(control$em_trace_se)) {
        abem_sandwich_se(
          estep,
          at_proposal$degenerate,
          compute_pool_weights(
            loglik_proposal,
            pool_log_proposal(pool_references(pool)),
            weighting = weights_ctrl$weighting,
            transformation = weights_ctrl$transformation
          )
        )
      }

      run$trace[[length(run$trace) + 1L]] <- list(
        iteration = as.integer(iteration),
        retry = as.integer(retry),
        theta = proposal,
        q = q,
        ase = as.numeric(ase),
        ase_floored = ase_floored(ase),
        decision = decision,
        accepted = accepted,
        streak = as.integer(streak),
        pool_size = as.integer(pool_size),
        fixed_degenerate = as.integer(fixed_degenerate),
        new_draws = as.integer(new_draws),
        pool_capped = pool_capped,
        ess = effective_sample_size(weights),
        weighting = scheme,
        autocorrelation = estep$autocorrelation %||% NA_real_,
        chain_diagnostics = if (is.function(augmenter$diagnostics)) {
          augmenter$diagnostics()
        },
        se = se
      )

      if (terminal) {
        return(abem_loop_result(
          theta_k,
          TRUE,
          accepted_iterations,
          run,
          pool_weights()
        ))
      }
      if (accepted) {
        theta_k <- proposal
        run$loglik <- loglik_proposal
        accepted_iterations <- accepted_iterations + 1L
        break
      }
      if (retry == control$max_retries) {
        abort_retries_exhausted(run, iteration, iteration_start_size, call)
      }
      new_draws <- draw_into_pool(
        ceiling(iteration_start_size / growth_k),
        iteration,
        theta_k
      )
      growth_k <- growth_k + 1L
    }
  }

  cli::cli_warn(
    c(
      "!" = "The ABEM loop reached {.arg max_iterations}
             ({.val {control$max_iterations}}) without meeting the stop rule.",
      "i" = "Returning the last accepted parameters; the fit is flagged
             {.code converged = FALSE}."
    ),
    class = "goldfish_abem_max_iterations"
  )
  abem_loop_result(theta_k, FALSE, accepted_iterations, run, pool_weights())
}

# A drawn sequence as a pool entry: its data and a reference record at the
# parameters it was drawn under. The reference log-likelihood is filled in
# once the pool has evaluated the sequence there.
abem_pool_entry <- function(draw, theta, call) {
  if (
    !rlang::is_scalar_double(draw$log_proposal) ||
      !is.finite(draw$log_proposal)
  ) {
    cli::cli_abort(
      "An augmenter draw must report a single finite {.field log_proposal}.",
      call = call
    )
  }
  list(
    data = draw$data,
    reference = list(
      theta_ref = theta,
      loglik_ref = NA_real_,
      log_proposal = draw$log_proposal
    )
  )
}

# Which pooled sequences are degenerate because a fixed-only term was
# non-finite or unevaluable, as opposed to a failed walk.
pool_fixed_term_lost <- function(pool) {
  vapply(
    pool$sequences,
    function(s) identical(s$degenerate_cause, "fixed_term"),
    logical(1)
  )
}

# Whether any pooled sequence can carry the E-step: a finite log-likelihood
# and a positive weight at the current parameters.
pool_carries_weight <- function(loglik, weights) {
  any(is.finite(loglik) & weights > 0)
}

# What each sequence of a pool that carries no weight was lost to: a failed
# walk or a non-finite fixed-only term, both recorded at entry, or otherwise a
# non-finite evaluation at the current parameters.
pool_lost_causes <- function(pool) {
  cause <- vapply(
    pool$sequences,
    function(s) s$degenerate_cause %||% NA_character_,
    character(1)
  )
  c(
    walk = sum(cause %in% "walk"),
    fixed_term = sum(cause %in% "fixed_term"),
    evaluation = sum(is.na(cause))
  )
}

abem_loop_result <- function(theta, converged, iterations, run, weights) {
  list(
    parameters = theta,
    converged = converged,
    iterations = iterations,
    em_trace = abem_trace_tibble(run$trace),
    pool = run$pool,
    loglik = run$loglik,
    weights = weights
  )
}

# The per-pass rows as a tibble; `theta`, `chain_diagnostics`, and `se` are
# list columns (a vector, an augmenter's own record, and an optional vector
# per pass).
abem_trace_tibble <- function(rows) {
  scalar <- function(name, type) {
    vapply(rows, function(r) r[[name]], type)
  }
  listed <- function(name) {
    lapply(rows, function(r) r[[name]])
  }
  tibble::tibble(
    iteration = scalar("iteration", integer(1)),
    retry = scalar("retry", integer(1)),
    theta = listed("theta"),
    q = scalar("q", numeric(1)),
    ase = scalar("ase", numeric(1)),
    ase_floored = scalar("ase_floored", logical(1)),
    decision = scalar("decision", character(1)),
    accepted = scalar("accepted", logical(1)),
    streak = scalar("streak", integer(1)),
    pool_size = scalar("pool_size", integer(1)),
    fixed_degenerate = scalar("fixed_degenerate", integer(1)),
    new_draws = scalar("new_draws", integer(1)),
    pool_capped = scalar("pool_capped", logical(1)),
    ess = scalar("ess", numeric(1)),
    weighting = scalar("weighting", character(1)),
    autocorrelation = scalar("autocorrelation", numeric(1)),
    chain_diagnostics = listed("chain_diagnostics"),
    se = listed("se")
  )
}

# Per-iteration standard errors at the pass's proposal: the square roots of
# the sandwich I^-1 (sum_i w_i s_i s_i') I^-1 with I = sum_i w_i I_i, over the
# whole pool, read off the per-sequence score and information the E-step
# object carries. A singular information yields NA rather than stopping the
# loop.
abem_sandwich_se <- function(estep, degenerate, weights) {
  covariances <- sandwich_covariances(
    estep$score,
    estep$fisher,
    degenerate,
    weights
  )
  sqrt(diag(covariances$covariance))
}

warn_pool_capped <- function(requested, max_pool) {
  cli::cli_warn(
    c(
      "!" = "The post-acceptance pool size ({.val {requested}}) exceeds
             {.arg max_pool} ({.val {max_pool}}); the next pool is capped.",
      "i" = "A Q small enough to ask for this many sequences means the loop is
             near the optimum. Consider relaxing {.arg tolerance} or lowering
             {.arg stop_count}."
    ),
    class = "goldfish_abem_pool_capped"
  )
}

abort_retries_exhausted <- function(run, iteration, start_size, call) {
  trace <- abem_trace_tibble(run$trace)
  stalled <- trace[trace$iteration == iteration, ]
  q_range <- signif(range(stalled$q), 3)
  cli::cli_abort(
    c(
      "The EM ascent could not be certified at iteration {.val {iteration}}
       within {.val {nrow(stalled) - 1L}} pool-growth retr{?y/ies}.",
      "x" = "Q stayed below its acceptance bound across
             {.val {nrow(stalled)}} passes (Q from {.val {q_range[[1]]}} to
             {.val {q_range[[2]]}}); the pool grew from {.val {start_size}} to
             {.val {max(stalled$pool_size)}} sequences, with an effective
             sample size of {.val {signif(min(stalled$ess), 3)}} at its
             lowest.",
      "i" = "The accumulated trace is attached to this error as
             {.field em_trace}: {.code rlang::last_error()$em_trace}.",
      "i" = "Consider a larger {.arg n_sequences}, a smaller
             {.arg step_size}, or a larger {.arg max_retries}."
    ),
    class = "goldfish_abem_retry_exhausted",
    em_trace = trace,
    call = call
  )
}

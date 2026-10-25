# The stochastic-gradient M-step for the ascent-based Monte Carlo EM (ABMCEM)
# loop driven by estimate_dynes(). `optimize_abem_sgd()` owns its inner loop
# (design D6): the EM loop hands it an injected score-only batch evaluator
# (`score_fn`, current pool baked in) plus the normalized E-step selection
# weights; the optimizer draws batches, requests per-sequence scores, and
# ascends the weighted expected complete-data log-likelihood until its stopping
# rule fires. One M-step is one call, so the adaptive-schedule accumulators --
# held in the stepper closure created here -- reset with every call.

# The single-batch ascent gradient from a batch of per-sequence score vectors.
# `scores` is a B x p matrix (one row per drawn sequence); `batch_weights` are
# the normalized pool weights of those members.
#   * unweighted (`weighted = FALSE`): the plain batch mean -- unbiased for the
#     weighted full-pool gradient when members are drawn with replacement
#     proportional to the weights (the "weighted" batch scheme), and the correct
#     estimator over a uniformly drawn resampled multiset.
#   * weighted (`weighted = TRUE`, the "cyclic" scheme): each contribution is
#     importance-weighted by weight x pool_size/batch_size, so that fixed
#     deterministic batches average -- over one full cycle of the pool -- to the
#     same weighted full-pool gradient sum_i w_i s_i. The two forms coincide
#     under uniform weights.
abem_batch_gradient <- function(
  scores,
  batch_weights,
  weighted,
  pool_size,
  batch_size
) {
  if (isTRUE(weighted)) {
    (pool_size / batch_size) * colSums(batch_weights * scores)
  } else {
    colMeans(scores)
  }
}

# Contiguous wrap-around window of `size` positions over a pool of `m`, starting
# at 1-based `start` -- the deterministic cyclic batch. Every window has exactly
# `size` members (wrapping past m), so the pool_size/batch_size importance
# factor in abem_batch_gradient() is exact on every batch.
cyclic_batch_positions <- function(start, size, m) {
  ((start - 1L + seq_len(size) - 1L) %% m) + 1L
}

# A step-schedule closure over the free-parameter dimension `p`. Returns
# `function(gradient, iteration) -> step`, the ascent increment added to theta.
# Adaptive accumulators live in this closure's environment and are therefore
# fresh for every M-step (each optimize_abem_sgd() call builds a new stepper).
# The three adaptive schedules use fixed literature-default hyperparameters,
# not exposed on set_sgd_options() (design D6). `newton_direction` maps a
# gradient to P^-1 g with the information P held at the M-step's start:
# Fisher scoring on the pool objective, whose Hessian is -P while the weights
# stay fixed, so a step of size 1 lands on a quadratic's optimum.
make_sgd_stepper <- function(schedule, step_size, p, newton_direction = NULL) {
  switch(
    schedule,
    fisher_scoring = function(gradient, iteration) {
      step_size * newton_direction(gradient)
    },
    constant = function(gradient, iteration) step_size * gradient,
    # Bottou decay eta_0 / (1 + eta_0 * t), with eta_0 = step_size.
    decay = function(gradient, iteration) {
      (step_size / (1 + step_size * iteration)) * gradient
    },
    adagrad = {
      grad_sq <- numeric(p)
      eps <- 1e-8
      function(gradient, iteration) {
        grad_sq <<- grad_sq + gradient^2
        step_size * gradient / (sqrt(grad_sq) + eps)
      }
    },
    adam = {
      m_acc <- numeric(p)
      v_acc <- numeric(p)
      beta1 <- 0.9
      beta2 <- 0.999
      eps <- 1e-8
      function(gradient, iteration) {
        m_acc <<- beta1 * m_acc + (1 - beta1) * gradient
        v_acc <<- beta2 * v_acc + (1 - beta2) * gradient^2
        m_hat <- m_acc / (1 - beta1^iteration)
        v_hat <- v_acc / (1 - beta2^iteration)
        step_size * m_hat / (sqrt(v_hat) + eps)
      }
    },
    momentum = {
      velocity <- numeric(p)
      momentum <- 0.9
      function(gradient, iteration) {
        velocity <<- momentum * velocity + gradient
        step_size * velocity
      }
    }
  )
}

# The Newton step in standard-error units, max_j |(P^-1 g)_j| / sqrt((P^-1)_jj),
# built once per M-step from the information `P`. Returns a list: `newton_se`,
# the closure of a gradient, `solve`, the closure returning P^-1 g itself (the
# pseudo-inverse's, so null-space free, when `P` is singular), `project`,
# which maps a step onto the range of `P`
# (NULL when `P` is positive definite, where no direction is excluded), and
# `rank_deficient`. A Cholesky factor serves a positive definite `P`; otherwise
# `P^-1` is the Moore-Penrose pseudo-inverse from an eigendecomposition, with
# eigenvalues under `sqrt(.Machine$double.eps)` of the largest counted as the
# null space. The null space takes no step and is left out of the statistic,
# since a direction without information has no standard error to scale by.
# `newton_se` is NULL when `P` carries no information at all. An empty free
# block has no step to take, so its statistic is 0.
make_newton_se <- function(information) {
  if (ncol(information) == 0L) {
    return(list(
      newton_se = function(gradient) 0,
      solve = function(gradient) numeric(0),
      project = NULL,
      rank_deficient = FALSE
    ))
  }
  factor <- tryCatch(chol(information), error = function(e) NULL)
  if (!is.null(factor)) {
    sd_inverse <- sqrt(diag(chol2inv(factor)))
    solve_factor <- function(gradient) {
      drop(backsolve(factor, forwardsolve(t(factor), gradient)))
    }
    return(list(
      newton_se = function(gradient) {
        max(abs(solve_factor(gradient)) / sd_inverse)
      },
      solve = solve_factor,
      project = NULL,
      rank_deficient = FALSE
    ))
  }

  eigen_information <- eigen(information, symmetric = TRUE)
  values <- eigen_information$values
  in_range <- values > sqrt(.Machine$double.eps) * max(values)
  if (!any(in_range)) {
    return(list(
      newton_se = NULL,
      solve = function(gradient) numeric(length(gradient)),
      project = NULL,
      rank_deficient = TRUE
    ))
  }
  range_basis <- eigen_information$vectors[, in_range, drop = FALSE]
  pseudo_inverse <- range_basis %*%
    (t(range_basis) / values[in_range])
  sd_inverse <- sqrt(diag(pseudo_inverse))
  # A coordinate lying wholly in the null space has no scale to test against.
  tested <- sd_inverse > sqrt(.Machine$double.eps) * max(sd_inverse)
  solve_pseudo <- function(gradient) drop(pseudo_inverse %*% gradient)
  list(
    newton_se = function(gradient) {
      step <- solve_pseudo(gradient)
      max(abs(step[tested]) / sd_inverse[tested])
    },
    solve = solve_pseudo,
    project = function(step) drop(range_basis %*% crossprod(range_basis, step)),
    rank_deficient = TRUE
  )
}

# Run the SGD M-step from `theta` over the pool weighted by `weights`.
#
# * `score_fn(theta, indices)`: the injected evaluator, returning a
#   length(indices) x length(theta) matrix of per-sequence score vectors (the
#   loop bakes in the current pool and the `what = "score"` request). The
#   full variant asks for the whole pool, `seq_len(length(weights))`.
# * `information_fn(theta)`: the injected information request, called once, at
#   the starting `theta`. It returns `information`, the p x p complete-data
#   information of the pool objective (weighted as `score_fn`'s gradient is),
#   and `scores`, the K x p per-sequence scores of the whole pool there, with
#   non-finite entries zeroed. The full variant builds its first gradient from
#   those scores instead of evaluating the pool again.
# * `weights`: the normalized E-step selection weights over the K-sequence pool.
# * `control`: a `set_sgd_options()` bundle (variant, batch scheme/size, step
#   schedule/size, convergence mode, sgd_tolerance, max_iterations).
# * `use`: `"importance"` (weighted pool) or `"resampling"` -- the latter draws
#   batches uniformly from the supplied `resample_indices` multiset with
#   unweighted gradients (the E-step resampling device, design D6/D8).
# * `fixed`: optional positions of theta held immobile through the M-step --
#   their gradient and step are zeroed and they are excluded from the
#   convergence statistic. The DyNES path leaves this empty (fixed effects are the
#   specification's formula offsets, excluded from theta upstream); the mask is
#   the optimizer's own general capability.
#
# Returns a list: the updated `parameters`, the `iterations` run, a `converged`
# flag (TRUE only when the Newton step fell under `sgd_tolerance`), the Newton
# step in standard-error units at the starting
# theta (`start_newton_se`) and at the last iterate a gradient was taken at
# (`newton_se`), and whether the information was not positive definite
# (`rank_deficient`: the step and the statistic then leave out its null space,
# and both statistics are NA only when it carries no information at all).
optimize_abem_sgd <- function(
  theta,
  score_fn,
  weights,
  information_fn,
  control = set_sgd_options(),
  use = c("importance", "resampling"),
  resample_indices = NULL,
  fixed = NULL,
  call = rlang::caller_env()
) {
  rlang::check_required(information_fn, call = call)
  use <- rlang::arg_match(use)
  p <- length(theta)
  pool_size <- length(weights)

  free <- rep(TRUE, p)
  if (!is.null(fixed)) {
    if (is.logical(fixed)) {
      if (length(fixed) != p) {
        cli::cli_abort(
          "A logical {.arg fixed} mask must be as long as {.arg theta}
           ({.val {p}}).",
          call = call
        )
      }
      free <- !fixed
    } else {
      free[as.integer(fixed)] <- FALSE
    }
  }

  # The pool the optimizer draws from, and the per-member selection weights.
  # Under resampling the multiset IS the pool, drawn uniformly with unweighted
  # gradients; under importance the pool is the K sequences drawn/weighted by
  # their normalized weights.
  resampling <- identical(use, "resampling")
  if (resampling) {
    if (is.null(resample_indices)) {
      cli::cli_abort(
        "{.arg resample_indices} is required when {.arg use} = {.val
         resampling}.",
        call = call
      )
    }
    draw_pool <- as.integer(resample_indices)
    m <- length(draw_pool)
    select_prob <- rep(1 / m, m)
  } else {
    draw_pool <- seq_len(pool_size)
    m <- pool_size
    select_prob <- weights
  }

  batch_size <- control$batch_size
  if (is.null(batch_size)) {
    batch_size <- min(m, 10L)
  }
  batch_size <- min(as.integer(batch_size), m)

  full_variant <- identical(control$variant, "full")
  # The cyclic importance-weighting has nothing to act on over a uniform
  # resampled multiset, so it collapses to the unweighted mean there.
  weighted_cyclic <- identical(control$batch_scheme, "cyclic") && !resampling
  gradient_mode <- identical(control$convergence, "gradient")
  fisher_scoring <- identical(control$step_schedule, "fisher_scoring")

  # Bias-corrected EMA of batch gradients: the minibatch convergence statistic
  # (a single batch gradient is too noisy to test, design D6). The full variant
  # tests the exact weighted gradient directly.
  ema <- numeric(p)
  ema_beta <- 0.9
  cyclic_start <- 1L

  # The information and the pool's scores at the starting theta: requested
  # once, and held for every iteration of this M-step.
  requested <- information_fn(theta)
  statistic <- make_newton_se(
    requested$information[free, free, drop = FALSE]
  )
  newton_se_of <- statistic$newton_se
  rank_deficient <- statistic$rank_deficient
  # On the free block alone, so the fixed coordinates' cross terms in P never
  # leak into the free steps.
  newton_direction <- function(gradient) {
    direction <- numeric(p)
    direction[free] <- statistic$solve(gradient[free])
    direction
  }
  stepper <- make_sgd_stepper(
    control$step_schedule,
    control$step_size,
    p,
    newton_direction
  )
  start_newton_se <- NA_real_
  newton_se <- NA_real_

  converged <- FALSE
  iteration <- 0L

  for (step_index in seq_len(control$max_iterations)) {
    iteration <- step_index
    if (full_variant) {
      # One evaluation of the whole pool per iteration, each member once (the
      # first iteration's is the request's), and the multiset's gradient a
      # row lookup: duplicates repeat their row.
      scores <- if (step_index == 1L) {
        requested$scores
      } else {
        score_fn(theta, seq_len(pool_size))
      }
      scores <- scores[draw_pool, , drop = FALSE]
      gradient <- if (resampling) {
        colMeans(scores)
      } else {
        colSums(select_prob * scores)
      }
      test_grad <- gradient
    } else {
      if (identical(control$batch_scheme, "cyclic")) {
        batch_pos <- cyclic_batch_positions(cyclic_start, batch_size, m)
        cyclic_start <- ((cyclic_start - 1L + batch_size) %% m) + 1L
      } else {
        batch_pos <- sample.int(
          m,
          batch_size,
          replace = TRUE,
          prob = select_prob
        )
      }
      scores <- score_fn(theta, draw_pool[batch_pos])
      gradient <- abem_batch_gradient(
        scores,
        select_prob[batch_pos],
        weighted_cyclic,
        m,
        batch_size
      )
      ema <- ema_beta * ema + (1 - ema_beta) * gradient
      test_grad <- ema / (1 - ema_beta^iteration)
    }

    # Hold the fixed coordinates immobile: no gradient into the adaptive
    # accumulators, no step, no contribution to the convergence statistic.
    gradient[!free] <- 0
    test_grad[!free] <- 0

    # The test comes before the step, at the iterate the gradient was taken
    # at: a converged M-step returns a theta its last gradient was evaluated
    # at, and one that passes at its start does not move.
    if (!is.null(newton_se_of)) {
      newton_se <- newton_se_of(test_grad[free])
      if (step_index == 1L) {
        start_newton_se <- newton_se
      }
      if (gradient_mode && newton_se < control$sgd_tolerance) {
        converged <- TRUE
        break
      }
    }

    # Fisher scoring steps along the tested gradient, which under minibatch is
    # the bias-corrected EMA: a raw batch gradient scaled by P^-1 would carry
    # the batch noise at full size into theta.
    step <- stepper(if (fisher_scoring) test_grad else gradient, iteration)
    step[!free] <- 0
    if (!is.null(statistic$project)) {
      step[free] <- statistic$project(step[free])
    }
    theta <- theta + step
  }

  # Exiting the iteration cap with the Newton-step criterion still unmet is a
  # diagnostic, not an error -- the EM accept test (design D7) governs
  # correctness -- so warn and return. The iterations mode is the opt-out budget
  # and never warns.
  if (gradient_mode && !converged) {
    cli::cli_warn(c(
      "!" = "The SGD M-step reached its iteration cap ({.val
             {control$max_iterations}}) with its Newton step ({.val
             {signif(newton_se, 3)}} standard errors) still above
             {.arg sgd_tolerance} ({.val {control$sgd_tolerance}}).",
      "i" = "Estimation continues; the EM acceptance test certifies the ascent
             step."
    ))
  }

  list(
    parameters = theta,
    iterations = iteration,
    converged = converged,
    start_newton_se = start_newton_se,
    newton_se = newton_se,
    rank_deficient = rank_deficient
  )
}

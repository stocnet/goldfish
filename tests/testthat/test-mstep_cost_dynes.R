# What one ABEM pass costs in sequence evaluations, counted outside pool entry
# (helper-abem_loop.R, local_mstep_counter()). The M-step asks for the pool's
# information once, at its starting theta, and the loop answers from the
# per-sequence harvest it already holds there. Under the full variant every
# iteration after the first evaluates the whole pool once, and a converged
# M-step returns the theta of its last evaluation, so the pass's evaluation at
# the proposal is that one: n iterations cost (n - 1) x K, and a capped M-step
# (cap - 1) x K plus one evaluation at the proposal. A minibatch M-step costs
# its batches plus the evaluation at the proposal (none when it passed at its
# start).

# The ABEM loop over the analytic evaluator with the real M-step: the pool's
# sequences are the scalars `candidates`, so the score is mu - theta and the
# information the identity. Returns the counter's log.
stub_mstep_log <- function(
  optimizer,
  candidates = list(1, 2, 4),
  theta0 = 0,
  weights = set_weights_options(),
  max_iterations = 3L
) {
  local_analytic_evaluator()
  log <- local_mstep_counter()
  suppressWarnings(run_abem_loop(
    theta0 = theta0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(candidates),
    control = analytic_em_control(
      max_iterations = max_iterations,
      weights = weights,
      optimizer = optimizer
    )
  ))
  log
}

stub_full_sgd <- function(...) {
  set_sgd_options(
    variant = "full",
    step_schedule = "constant",
    step_size = 0.5,
    sgd_tolerance = 1e-3,
    max_iterations = 100L,
    ...
  )
}

# The stub logs the guard reads: full-variant M-steps that converge after
# stepping, under importance and under resampling; one that passes its test at
# its start (every sequence at theta0, so the gradient is zero there); one
# that runs to its cap; and a minibatch one.
stub_mstep_logs <- function() {
  withr::local_seed(7L)
  list(
    full = stub_mstep_log(stub_full_sgd()),
    resampling = stub_mstep_log(
      stub_full_sgd(),
      weights = set_weights_options(use = "resampling")
    ),
    at_start = stub_mstep_log(
      stub_full_sgd(),
      candidates = list(1),
      theta0 = 1
    ),
    capped = stub_mstep_log(
      set_sgd_options(
        variant = "full",
        step_schedule = "constant",
        step_size = 1e-3,
        sgd_tolerance = 1e-12,
        max_iterations = 3L
      )
    ),
    minibatch = stub_mstep_log(
      set_sgd_options(
        variant = "minibatch",
        batch_size = 2L,
        step_schedule = "constant",
        step_size = 0.5,
        sgd_tolerance = 1e-3,
        max_iterations = 50L
      )
    )
  )
}

# One estimate_dynes() run over two wave periods with the real evaluator and a
# full-variant M-step. Returns the counter's log.
dynes_mstep_log <- function() {
  local_dynes_augmenter(new_wave_grid_augmenter())
  log <- local_mstep_counter()
  suppressWarnings(estimate_dynes(
    dynes_varying_raw_spec(),
    set_algorithm_em(
      n_sequences = 6L,
      max_iterations = 3L,
      seed = 42L,
      augmenter = set_augmenter_options(routine = "random"),
      optimizer = set_sgd_options(
        variant = "full",
        step_schedule = "adam",
        step_size = 0.1,
        sgd_tolerance = 0.05,
        max_iterations = 30L
      )
    )
  ))
  log
}

expect_one_information_request <- function(log, label) {
  expect_gt(length(log$passes), 0L)
  for (pass in seq_along(log$passes)) {
    record <- log$passes[[pass]]
    info <- paste(label, "pass", pass)
    expect_length(record$information_thetas, 1L)
    if (length(record$information_thetas) == 1L) {
      expect_identical(
        record$information_thetas[[1L]],
        record$start,
        info = info
      )
    }
  }
}

expect_information_evaluates_nothing <- function(log, label) {
  expect_gt(length(log$passes), 0L)
  for (pass in seq_along(log$passes)) {
    # Without a request there is nothing to have answered cheaply.
    expect_gt(length(log$passes[[pass]]$information_thetas), 0L)
    expect_identical(
      pass_evaluated(log, pass, "information"),
      0L,
      info = paste(label, "pass", pass)
    )
  }
}

# The pass's sequence evaluations outside pool entry, against the cost of its
# M-step's verdict.
expect_pass_costs <- function(log, label) {
  expect_gt(length(log$passes), 0L)
  for (pass in seq_along(log$passes)) {
    record <- log$passes[[pass]]
    info <- paste(label, "pass", pass)
    k <- record$pool_size
    n <- record$result$iterations
    spent <- pass_evaluated(
      log,
      pass,
      c("information", "score", "proposal")
    )
    if (identical(record$control$variant, "full")) {
      expected <- if (isTRUE(record$result$converged)) {
        (n - 1L) * k
      } else {
        record$control$max_iterations * k
      }
      expect_identical(spent, as.integer(expected), info = info)
    } else {
      batch <- min(record$control$batch_size, k)
      expect_identical(
        pass_evaluated(log, pass, "score"),
        as.integer(n * batch),
        info = info
      )
      # A minibatch M-step that moved returns a theta it never evaluated the
      # pool at; one that passed at its start returns theta_k.
      moved <- !identical(record$result$parameters, record$start)
      expect_identical(
        pass_evaluated(log, pass, "proposal"),
        if (moved) k else 0L,
        info = info
      )
    }
  }
}

test_that("each M-step requests the information once, at its start", {
  logs <- stub_mstep_logs()
  for (label in names(logs)) {
    expect_one_information_request(logs[[label]], label)
  }
})

test_that("answering the information request evaluates no sequence", {
  logs <- stub_mstep_logs()
  for (label in names(logs)) {
    expect_information_evaluates_nothing(logs[[label]], label)
  }
})

test_that("every minibatch batch requests scores only", {
  log <- stub_mstep_logs()$minibatch
  batches <- Filter(function(e) identical(e$phase, "score"), log$evaluations)
  expect_gt(length(batches), 0L)
  for (e in batches) {
    expect_identical(e$what, "score")
  }
})

test_that("a pass costs what its M-step's verdict says", {
  logs <- stub_mstep_logs()
  # The cases the cost rules distinguish are all present.
  expect_true(any(vapply(
    logs$full$passes,
    function(p) p$result$converged && p$result$iterations > 1L,
    logical(1)
  )))
  expect_identical(logs$at_start$passes[[1L]]$result$iterations, 1L)
  expect_false(logs$capped$passes[[1L]]$result$converged)
  for (label in names(logs)) {
    expect_pass_costs(logs[[label]], label)
  }
})

test_that("an estimate_dynes() run keeps to the same costs", {
  skip_on_cran()
  log <- dynes_mstep_log()
  expect_one_information_request(log, "dynes")
  expect_information_evaluates_nothing(log, "dynes")
  expect_pass_costs(log, "dynes")
})

# The information request is answered from the harvest the loop holds at
# theta_k, beside its log-likelihoods: each sequence's pool-entry evaluation,
# and on an accepted pass the evaluation of the whole pool at the proposal.
# The audit below reads the pass's pool, weights, multiset and held harvest
# from the frame the loop builds its closures in, evaluates the whole pool
# afresh at the M-step's start, and compares every answer bit for bit. The
# first M-step's proposal is pushed 2 away, so that pass is rejected and its
# retry grows the pool.
local_information_audit <- function(env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$requests <- list()
  log$redraws <- logical()
  log$answering <- FALSE
  log$evaluated <- 0L
  log$pass <- 0L
  real_evaluate <- evaluate_sequence_pool
  real_optimize <- optimize_abem_sgd
  real_guard <- ess_guard
  local_mocked_bindings(
    evaluate_sequence_pool = function(
      pool,
      theta,
      what = c("loglik", "score", "fisher"),
      ...
    ) {
      res <- real_evaluate(pool, theta, what, ...)
      if (log$answering) {
        log$evaluated <- log$evaluated + length(res$degenerate)
      }
      res
    },
    ess_guard = function(...) {
      guard <- real_guard(...)
      log$redraws <- c(log$redraws, isTRUE(guard$redraw))
      guard
    },
    optimize_abem_sgd = function(
      theta,
      score_fn,
      weights,
      ...,
      information_fn
    ) {
      loop <- environment(information_fn)
      fresh <- real_evaluate(loop$pool, theta)
      log$pass <- log$pass + 1L
      pass <- log$pass
      audited <- function(at) {
        before <- log$evaluated
        log$answering <- TRUE
        answer <- information_fn(at)
        log$answering <- FALSE
        log$requests[[length(log$requests) + 1L]] <- list(
          pass = pass,
          evaluated = log$evaluated - before,
          answer = answer,
          expected = abem_information(
            fresh,
            loop$weights,
            loop$resample_indices
          ),
          held = loop$run$harvest,
          fresh = fresh
        )
        answer
      }
      res <- real_optimize(
        theta,
        score_fn,
        weights,
        ...,
        information_fn = audited
      )
      if (pass == 1L) {
        res$parameters <- res$parameters + 2
      }
      res
    },
    .env = env
  )
  log
}

# The wave-grid augmenter with its log proposal densities spread 20-fold, so
# the importance weights collapse and the ESS guard redraws the pool.
new_spread_wave_grid_augmenter <- function() {
  base <- new_wave_grid_augmenter()
  list(
    draw = function(iteration, draw_index, theta) {
      lapply(base$draw(iteration, draw_index, theta), function(d) {
        d$log_proposal <- 20 * d$log_proposal
        d
      })
    }
  )
}

audited_dynes_run <- function(use) {
  local_dynes_augmenter(new_spread_wave_grid_augmenter())
  log <- local_information_audit()
  log$fit <- suppressWarnings(estimate_dynes(
    dynes_varying_raw_spec(),
    set_algorithm_em(
      n_sequences = 6L,
      max_iterations = 3L,
      seed = 42L,
      augmenter = set_augmenter_options(routine = "random"),
      weights = set_weights_options(use = use),
      optimizer = set_sgd_options(
        variant = "full",
        step_schedule = "adam",
        step_size = 0.1,
        sgd_tolerance = 0.05,
        max_iterations = 30L
      )
    )
  ))
  log
}

test_that("the information request is served from the held harvest", {
  skip_on_cran()
  for (use in c("importance", "resampling")) {
    log <- audited_dynes_run(use)
    trace <- log$fit$em_trace
    # The run went through a rejected pass whose retry grew the pool, an
    # accepted pass and an ESS-guard redraw.
    expect_true(any(trace$decision == "grow"), info = use)
    expect_true(any(trace$accepted), info = use)
    expect_true(any(log$redraws), info = use)
    expect_length(log$requests, nrow(trace))
    for (request in log$requests) {
      info <- paste(use, "pass", request$pass)
      expect_identical(request$evaluated, 0L, info = info)
      expect_identical(request$answer, request$expected, info = info)
      expect_identical(request$held, request$fresh, info = info)
    }
  }
})

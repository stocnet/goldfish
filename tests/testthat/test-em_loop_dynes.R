# The ABEM loop: per EM iteration, one initial pass plus bounded growth
# retries. Each pass runs the M-step from the last accepted theta, evaluates the
# pool at the proposal, and decides STOP first, then ACCEPT, else GROW. The loop
# is written against the augmenter, evaluator, and optimizer contracts only, so
# these tests drive it with a stub augmenter, the analytic stub evaluator, and
# (where a transition must be forced) a scripted optimizer; see
# helper-abem_loop.R.

test_that("a rejected pass grows the pool, and the retry then accepts", {
  local_analytic_evaluator()
  proposals <- c(-0.5, 0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    proposals[[pass]]
  })
  augmenter <- new_stub_augmenter(list(1))

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = augmenter,
    control = analytic_em_control()
  )

  trace <- res$em_trace
  expect_identical(trace$iteration, c(1L, 1L, 2L))
  expect_identical(trace$retry, c(0L, 1L, 0L))
  expect_identical(trace$decision, c("grow", "accept", "stop"))
  # The growth increment is ceiling(m / k) with m the iteration-start pool
  # size and k = 2 on the first rejection.
  expect_identical(trace$pool_size, c(4L, 6L, 6L))
  expect_identical(trace$new_draws, c(4L, 2L, 0L))
  # The retry restarts the M-step from the last accepted theta, not from the
  # discarded proposal.
  expect_true(res$converged)
  expect_equal(res$parameters, 0.5)
})

test_that("draw_index is monotonic within an iteration and resets after it", {
  local_analytic_evaluator()
  proposals <- c(-0.5, 0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    proposals[[pass]]
  })
  augmenter <- new_stub_augmenter(list(1))

  run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = augmenter,
    control = analytic_em_control()
  )

  draws <- augmenter$state$draws
  expect_identical(draws[[1]]$iteration, 1L)
  expect_identical(draws[[1]]$draw_index, 1:4)
  # The growth append continues the iteration's counter.
  expect_identical(draws[[2]]$draw_index, 5:6)
  # Growth draws at the last accepted theta.
  expect_equal(draws[[2]]$theta, 0)
})

test_that("retry exhaustion aborts and carries the accumulated em_trace", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta - 1)
  augmenter <- new_stub_augmenter(list(1))

  cnd <- tryCatch(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control(max_retries = 2L)
    ),
    goldfish_abem_retry_exhausted = function(cnd) cnd
  )

  expect_s3_class(cnd, "goldfish_abem_retry_exhausted")
  expect_s3_class(cnd$em_trace, "tbl_df")
  # One initial pass plus max_retries growth retries, all rejected.
  expect_identical(nrow(cnd$em_trace), 3L)
  expect_identical(cnd$em_trace$decision, rep("grow", 3L))
  expect_identical(cnd$em_trace$pool_size, c(4L, 6L, 8L))
})

test_that("the retry-exhaustion error explains the stall", {
  local_abem_cli_context()
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta - 1)

  expect_snapshot(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = new_stub_augmenter(list(1)),
      control = analytic_em_control(max_retries = 2L)
    ),
    error = TRUE
  )
})

test_that("max_iterations returns the last accepted theta, unconverged", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1)
  augmenter <- new_stub_augmenter(list(100))

  expect_warning(
    res <- run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control(max_iterations = 3L)
    ),
    class = "goldfish_abem_max_iterations"
  )

  expect_false(res$converged)
  expect_equal(res$parameters, 3)
  expect_identical(res$em_trace$decision, rep("accept", 3L))
  expect_identical(res$iterations, 3L)
})

test_that("the em_trace records one row per pass with its diagnostics", {
  local_analytic_evaluator()
  proposals <- c(-0.5, 0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    proposals[[pass]]
  })

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1)),
    control = analytic_em_control()
  )
  trace <- res$em_trace

  expect_s3_class(trace, "tbl_df")
  expect_named(
    trace,
    c(
      "iteration",
      "retry",
      "theta",
      "q",
      "ase",
      "ase_floored",
      "decision",
      "accepted",
      "streak",
      "pool_size",
      "fixed_degenerate",
      "new_draws",
      "pool_capped",
      "ess",
      "weighting",
      "autocorrelation",
      "mstep_iterations",
      "mstep_converged",
      "mstep_start_newton_se",
      "mstep_newton_se",
      "mstep_rank_deficient",
      "mstep_damped",
      "chain_diagnostics",
      "se"
    )
  )
  expect_equal(trace$theta, list(-0.5, 0.5, 0.5 + 1e-5))
  # Every pooled sequence is identical, so Lambda is constant: Q is the
  # closed-form increment and its standard error is zero.
  expect_equal(trace$q[[2]], analytic_loglik(1, 0.5) - analytic_loglik(1, 0))
  expect_equal(trace$ase, c(0, 0, 0))
  expect_equal(trace$ess, c(4, 6, 6))
  expect_identical(trace$weighting, rep("importance", 3L))
  expect_identical(trace$accepted, c(FALSE, TRUE, FALSE))
  # Per-pass SEs are opt-in: absent by default.
  expect_true(all(vapply(trace$se, is.null, logical(1))))
})

mstep_verdict_columns <- c(
  mstep_iterations = "integer",
  mstep_converged = "logical",
  mstep_start_newton_se = "double",
  mstep_newton_se = "double",
  mstep_rank_deficient = "logical",
  mstep_damped = "integer"
)

test_that("the em_trace carries the M-step's verdict, one value per pass", {
  skip_on_cran()
  res <- run_abem_loop(
    theta0 = c(2, -2),
    decomposition = abem_pinned_choice_decomposition(),
    augmenter = new_stub_augmenter(abem_two_wave_paths()),
    control = analytic_em_control(max_iterations = 2L)
  ) |>
    suppressWarnings()
  trace <- res$em_trace

  expect_gte(nrow(trace), 1L)
  expect_contains(names(trace), names(mstep_verdict_columns))
  for (column in names(mstep_verdict_columns)) {
    expect_type(trace[[column]], mstep_verdict_columns[[column]])
    expect_length(trace[[column]], nrow(trace))
  }
  # One row per pass and nothing per M-step iteration: no other M-step column,
  # and no list column beyond the ones the trace already carried.
  expect_setequal(
    grep("^mstep_", names(trace), value = TRUE),
    names(mstep_verdict_columns)
  )
  expect_setequal(
    names(trace)[vapply(trace, is.list, logical(1))],
    c("theta", "chain_diagnostics", "se")
  )
  expect_true(all(trace$mstep_iterations >= 1L))
  expect_true(all(trace$mstep_damped >= 0L))
})

test_that("an M-step that passes at its start reports one statistic twice", {
  local_analytic_evaluator()
  # Every sequence sits at theta0, so the gradient is zero there.
  res <- run_abem_loop(
    theta0 = 1,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1)),
    control = analytic_em_control(max_iterations = 1L)
  ) |>
    suppressWarnings()
  trace <- res$em_trace

  expect_identical(trace$mstep_iterations, rep(1L, nrow(trace)))
  expect_true(all(trace$mstep_converged))
  expect_identical(trace$mstep_newton_se, trace$mstep_start_newton_se)
  expect_identical(trace$mstep_damped, rep(0L, nrow(trace)))
})

test_that("a stepping M-step's trace keeps its start and final statistics", {
  local_analytic_evaluator()
  counter <- local_mstep_counter()
  # From theta0 = 0 the sequences at 1, 2 and 4 pull the first M-step away,
  # so it steps before its statistic falls below the tolerance.
  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1, 2, 4)),
    control = analytic_em_control(
      max_iterations = 2L,
      optimizer = set_sgd_options(
        variant = "full",
        step_schedule = "constant",
        step_size = 0.5,
        sgd_tolerance = 1e-3,
        max_iterations = 100L
      )
    )
  ) |>
    suppressWarnings()
  trace <- res$em_trace

  expect_identical(length(counter$passes), nrow(trace))
  results <- lapply(counter$passes, `[[`, "result")
  expect_identical(
    trace$mstep_start_newton_se,
    vapply(results, function(r) as.numeric(r$start_newton_se), numeric(1))
  )
  expect_identical(
    trace$mstep_newton_se,
    vapply(results, function(r) as.numeric(r$newton_se), numeric(1))
  )
  stepped <- trace$mstep_iterations > 1L
  expect_true(stepped[[1L]])
  expect_true(all(
    trace$mstep_start_newton_se[stepped] != trace$mstep_newton_se[stepped]
  ))
})

# The classes of every warning `expr` raises, muffled, beside its value.
collect_warning_classes <- function(expr) {
  classes <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      classes <<- c(classes, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, classes = classes)
}

test_that("an M-step capped on every pass warns once for the run", {
  local_analytic_evaluator()

  run <- collect_warning_classes(run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1, 2, 4)),
    control = analytic_em_control(
      max_iterations = 4L,
      optimizer = set_sgd_options(max_iterations = 1L)
    )
  ))
  trace <- run$value$em_trace

  expect_gt(nrow(trace), 1L)
  expect_false(any(trace$mstep_converged))
  expect_identical(sum(run$classes == "goldfish_abem_mstep_unconverged"), 1L)
  # The optimizer warns nothing of its own: the run's only other warning is
  # the EM loop's own iteration cap.
  expect_setequal(
    run$classes,
    c("goldfish_abem_mstep_unconverged", "goldfish_abem_max_iterations")
  )
})

test_that("the iterations mode never warns that the M-step is unconverged", {
  local_analytic_evaluator()

  run <- collect_warning_classes(run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1, 2, 4)),
    control = analytic_em_control(
      max_iterations = 2L,
      optimizer = set_sgd_options(
        max_iterations = 1L,
        convergence = "iterations"
      )
    )
  ))

  expect_false(any(run$value$em_trace$mstep_converged))
  expect_false("goldfish_abem_mstep_unconverged" %in% run$classes)
})

test_that("the unconverged M-step warning names the pass and its halvings", {
  local_abem_cli_context()
  local_analytic_evaluator()
  local_scripted_optimizer(
    function(theta) theta + 1e-4,
    verdict = list(
      iterations = 20L,
      converged = FALSE,
      start_newton_se = 1.5,
      newton_se = 0.3125,
      damped = 2L
    )
  )
  run_two_passes <- function() {
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = new_stub_augmenter(list(1)),
      control = analytic_em_control(stop_count = 2L)
    )
  }

  # Two unconverged passes, one warning.
  run <- collect_warning_classes(run_two_passes())
  expect_identical(nrow(run$value$em_trace), 2L)
  expect_identical(run$classes, "goldfish_abem_mstep_unconverged")
  expect_identical(run$value$em_trace$mstep_damped, c(2L, 2L))

  expect_snapshot(res <- run_two_passes())
})

test_that("an undamped unconverged M-step's warning names no halvings", {
  local_abem_cli_context()
  local_analytic_evaluator()
  local_scripted_optimizer(
    function(theta) theta + 1e-4,
    verdict = list(iterations = 20L, converged = FALSE, newton_se = 0.3125)
  )

  cnd <- rlang::catch_cnd(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = new_stub_augmenter(list(1)),
      control = analytic_em_control(stop_count = 2L)
    ),
    classes = "goldfish_abem_mstep_unconverged"
  )

  expect_s3_class(cnd, "goldfish_abem_mstep_unconverged")
  expect_no_match(conditionMessage(cnd), "halv")
})

test_that("em_trace_se adds standard errors on accepted passes", {
  local_analytic_evaluator()
  proposals <- c(0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    proposals[[pass]]
  })

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1, 2)),
    control = analytic_em_control(em_trace_se = TRUE)
  )
  se <- res$em_trace$se

  expect_length(se[[1]], 1L)
  expect_true(is.finite(se[[1]]))
  expect_null(se[[2]])
})

test_that("a near-zero-Q acceptance clamps the next pool to max_pool", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.01)
  augmenter <- new_stub_augmenter(list(-0.2, 1))

  warnings <- character()
  res <- withCallingHandlers(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control(
        max_pool = 6L,
        max_iterations = 2L,
        tolerance = 1e-8
      )
    ),
    warning = function(w) {
      warnings <<- c(warnings, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )

  # The sizing rule asks for ~14 sequences after the first acceptance; the cap
  # holds it at 6 and warns once, even though the second acceptance binds the
  # cap again.
  expect_identical(res$em_trace$pool_capped, c(TRUE, TRUE))
  expect_identical(res$em_trace$pool_size, c(4L, 6L))
  expect_identical(res$em_trace$new_draws, c(4L, 2L))
  expect_identical(sum(warnings == "goldfish_abem_pool_capped"), 1L)
})

test_that("the max_pool warning names the near-optimum condition", {
  local_abem_cli_context()
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.01)

  expect_snapshot(
    res <- run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = new_stub_augmenter(list(-0.2, 1)),
      control = analytic_em_control(
        max_pool = 6L,
        max_iterations = 1L,
        tolerance = 1e-8
      )
    )
  )
})

test_that("a non-terminal stop-hit falls through and still accepts", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1e-4)

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1)),
    control = analytic_em_control(stop_count = 2L)
  )
  trace <- res$em_trace

  expect_identical(trace$decision, c("stop-hit", "stop"))
  expect_identical(trace$accepted, c(TRUE, FALSE))
  expect_identical(trace$streak, c(1L, 2L))
  # The first hit advanced theta; the terminal hit returns that last accepted
  # theta, not its own proposal.
  expect_true(res$converged)
  expect_equal(res$parameters, 1e-4)
})

test_that("a stop-rule miss resets the streak", {
  local_analytic_evaluator()
  steps <- c(1e-4, 0.5, 1e-4, 1e-4)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    theta + steps[[pass]]
  })

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1)),
    control = analytic_em_control(stop_count = 2L)
  )

  expect_identical(res$em_trace$streak, c(1L, 0L, 1L, 2L))
  expect_identical(
    res$em_trace$decision,
    c("stop-hit", "accept", "stop-hit", "stop")
  )
})

test_that("the loop consumes Q and its ASE through dispatch alone", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1)
  # A weighting scheme the loop has never heard of: its E-step class carries
  # its own compute_q()/compute_ase() methods, and the loop runs unmodified.
  local_mocked_bindings(
    new_dynes_estep = function(lambda, ...) {
      structure(list(lambda = lambda), class = c("goldfishEstepTest"))
    }
  )
  q_values <- c(1, 1e-6)
  calls <- 0L
  local_mocked_s3_method(
    "compute_q",
    "goldfishEstepTest",
    function(estep, ...) {
      calls <<- calls + 1L
      q_values[[min(calls, 2L)]]
    }
  )
  local_mocked_s3_method(
    "compute_ase",
    "goldfishEstepTest",
    function(estep, ...) structure(0, ase_floored = FALSE)
  )

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1)),
    control = analytic_em_control()
  )

  expect_identical(res$em_trace$decision, c("accept", "stop"))
  expect_identical(res$em_trace$q, q_values)
  expect_equal(res$parameters, 1)
})

test_that("each iteration injects a theta_k-closed evaluator over one spec", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1)
  built <- list()
  local_mocked_bindings(
    make_proposal_evaluator = function(
      decomposition,
      theta,
      spec = NULL,
      ...
    ) {
      built[[length(built) + 1L]] <<- list(theta = theta, spec = spec)
      function(candidate_seq, rate_queries = NULL) {
        list(loglik = analytic_loglik(candidate_seq, theta), rates = NULL)
      }
    },
    complete_generative_spec = function(...) {
      stop("the loop must never re-complete the specification")
    }
  )
  completed <- structure(list(id = "completed"), class = "goldfishJointSpec")
  augmenter <- new_stub_augmenter(list(100))

  suppressWarnings(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control(max_iterations = 3L),
      spec = completed
    )
  )

  # One evaluator per EM iteration, closed over that iteration's theta_k and
  # over the one specification completed at entry, passed through unchanged.
  expect_length(built, 3L)
  expect_equal(lapply(built, `[[`, "theta"), list(0, 1, 2))
  for (b in built) {
    expect_identical(b$spec, completed)
  }
  inits <- augmenter$state$inits
  expect_length(inits, 3L)
  expect_equal(lapply(inits, `[[`, "theta"), list(0, 1, 2))
  expect_equal(inits[[2]]$proposal_evaluator(3)$loglik, analytic_loglik(3, 1))
})

test_that("a collapsed persistent pool is redrawn at the current theta", {
  local_analytic_evaluator()
  proposals <- c(3, 3 + 1e-6)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    proposals[[pass]]
  })
  # The sequence at mu = 3 was drawn with a tiny proposal density, so it holds
  # almost all the weight: the ESS sits near 1 and the persistent pool is
  # redrawn when iteration 2 opens. (The guard is silent at startup.)
  augmenter <- new_stub_augmenter(
    list(0, 0, 0, 3),
    log_proposal = c(0, 0, 0, -10)
  )

  expect_warning(
    res <- run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control()
    ),
    "redrawing"
  )

  draws <- augmenter$state$draws
  expect_length(draws, 2L)
  # The redraw is iteration 2's, at the accepted theta, over a fresh
  # draw_index range.
  expect_identical(draws[[2]]$iteration, 2L)
  expect_identical(draws[[2]]$draw_index, 1:4)
  expect_equal(draws[[2]]$theta, 3)
  expect_identical(res$em_trace$new_draws, c(4L, 4L))
})

test_that("a restart-coupled draw warns once per run", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1)

  augmenter <- new_stub_augmenter(list(100), restarted = TRUE)
  augmenter$coupling <- "friendship: ~tie(friendship) | recip(friendship)"
  warnings <- character()
  withCallingHandlers(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control(
        max_iterations = 3L,
        augmenter = set_augmenter_options(routine = "sim")
      )
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  restart <- grepl("reject and\\s+restart", warnings)
  expect_identical(sum(restart), 1L)
  expect_match(warnings[restart], "recip(friendship)", fixed = TRUE)
})

test_that("a random augmenter's redraws never warn in the loop", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1)

  warnings <- character()
  withCallingHandlers(
    run_abem_loop(
      theta0 = 0,
      decomposition = analytic_decomposition(),
      augmenter = new_stub_augmenter(list(100), restarted = TRUE),
      control = analytic_em_control(max_iterations = 3L)
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("reject and\\s+restart", warnings)))
})

# The closure the loop injects scores fixed-only fids too: their θ-free term is
# path-dependent, so it must be in f wherever two candidate paths are compared.
# Fixtures in helper-abem_loop.R.

test_that("the proposal evaluator's loglik carries a pinned rate's term", {
  eval_fn <- make_proposal_evaluator(abem_pinned_decomposition(), numeric(0))
  path_a <- abem_two_wave_data()
  path_b <- abem_two_wave_data(calls_time = c(1, 3, 6, 8, 10, 15))

  # Moving one event across the interior boundary changes its log-hazard from
  # c2 = -2 to c1 = -1 and leaves every period's exposure unchanged.
  expect_equal(
    eval_fn(path_b)$loglik - eval_fn(path_a)$loglik,
    1,
    tolerance = 1e-10
  )
})

test_that("a candidate across a wave boundary is scored per period", {
  eval_fn <- make_proposal_evaluator(abem_pinned_decomposition(), numeric(0))
  # The interval (6, 11] crosses the boundary at 10. Its exposure splits into
  # 4 * 4 at c1 and 4 * 1 at c2, as if an event sat on the boundary.
  straddling <- abem_two_wave_data(calls_time = c(1, 3, 6, 11, 12, 15))
  c1 <- -1
  c2 <- -2
  expected <- 3 * c1 + 3 * c2 - exp(c1) * 4 * 9 - exp(c2) * 4 * 5

  out <- expect_no_error(eval_fn(straddling))
  expect_equal(out$loglik, expected, tolerance = 1e-10)
})

# A pool whose every sequence lost its fixed-only term carries no information:
# under importance weighting every weight is zero, under uniform weighting no
# sequence is left to average over. The loop stops before the M-step,
# unconverged, at its last accepted theta. The real evaluator scores the stub's
# paths; fixtures in helper-abem_loop.R.

test_that("a fully fixed-term-degenerate pool stops importance weighting", {
  skip_on_cran()
  local_fixed_term_loss()

  res <- expect_no_warning(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = analytic_em_control(optimizer = abem_full_sgd())
    )
  )

  expect_false(res$converged)
  expect_identical(res$parameters, c(0.3, -0.2))
  expect_identical(res$iterations, 0L)
  expect_identical(nrow(res$em_trace), 0L)
  expect_true(res$pool$store$tally$pool_dropped)
})

test_that("a fully fixed-term-degenerate pool stops uniform weighting", {
  skip_on_cran()
  local_fixed_term_loss()

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = analytic_em_control(
        augmenter = set_augmenter_options(routine = "mcmc"),
        weights = set_weights_options(weighting = "uniform", refresh = TRUE),
        optimizer = abem_full_sgd()
      )
    )
  )

  expect_false(res$converged)
  expect_identical(res$parameters, c(0.3, -0.2))
  expect_identical(nrow(res$em_trace), 0L)
  expect_true(res$pool$store$tally$pool_dropped)
})

test_that("a refresh into full fixed-term loss stops at the last accept", {
  skip_on_cran()
  # The refresh opening iteration 2 replaces a healthy pool by a lost one.
  lost <- FALSE
  local_fixed_term_loss(function() lost)
  stub <- new_stub_augmenter(abem_two_wave_paths())
  augmenter <- stub
  augmenter$draw <- function(iteration, draw_index, theta) {
    lost <<- iteration >= 2L
    stub$draw(iteration, draw_index, theta)
  }

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(2, -2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = augmenter,
      control = analytic_em_control(
        augmenter = set_augmenter_options(routine = "mcmc"),
        weights = set_weights_options(weighting = "uniform", refresh = TRUE),
        optimizer = abem_full_sgd()
      )
    )
  )

  trace <- res$em_trace
  expect_false(res$converged)
  expect_identical(res$iterations, 1L)
  expect_identical(unique(trace$iteration), 1L)
  expect_identical(trace$decision[[nrow(trace)]], "accept")
  expect_identical(res$parameters, trace$theta[[nrow(trace)]])
  expect_true(res$pool$store$tally$pool_dropped)
})

test_that("a partly fixed-term-degenerate pool does not stop the loop", {
  skip_on_cran()
  # Only the first path of each draw is lost.
  path <- 0L
  local_fixed_term_loss(function() {
    path <<- path + 1L
    path %% 3L == 1L
  })

  res <- run_abem_loop(
    theta0 = c(2, -2),
    decomposition = abem_pinned_choice_decomposition(),
    augmenter = new_stub_augmenter(abem_two_wave_paths()),
    control = analytic_em_control(
      max_iterations = 1L,
      optimizer = abem_full_sgd()
    )
  ) |>
    suppressWarnings()

  expect_gte(nrow(res$em_trace), 1L)
  expect_true(all(res$em_trace$fixed_degenerate > 0L))
  expect_false(res$pool$store$tally$pool_dropped)
})

# A pool can be lost to causes other than a fixed-only term: every walk failed
# at entry, or every evaluation at the current theta was non-finite, which no
# entry cause records. The loop reads the log-likelihoods and weights it is
# about to use, so it stops before Q and before an SGD minibatch is drawn from
# an all-zero weight vector, under either weighting scheme.

uniform_em_control <- function() {
  analytic_em_control(
    augmenter = set_augmenter_options(routine = "mcmc"),
    weights = set_weights_options(weighting = "uniform", refresh = TRUE)
  )
}

expect_stopped_at_start <- function(res, theta0, lost) {
  expect_false(res$converged)
  expect_identical(res$parameters, theta0)
  expect_identical(res$iterations, 0L)
  expect_identical(nrow(res$em_trace), 0L)
  expect_true(res$pool$store$tally$pool_dropped)
  expect_identical(res$pool$store$tally$pool_lost, lost)
}

test_that("a pool with every evaluation non-finite stops importance weights", {
  skip_on_cran()
  local_evaluation_failure()

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = analytic_em_control()
    )
  )

  expect_stopped_at_start(res, c(0.3, -0.2), pool_lost_counts(evaluation = 4L))
})

test_that("a pool with every evaluation non-finite stops uniform weighting", {
  skip_on_cran()
  local_evaluation_failure()

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = uniform_em_control()
    )
  )

  expect_stopped_at_start(res, c(0.3, -0.2), pool_lost_counts(evaluation = 4L))
})

test_that("a pool whose every walk failed stops importance weighting", {
  skip_on_cran()
  local_walk_failure()

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = analytic_em_control()
    )
  )

  expect_stopped_at_start(res, c(0.3, -0.2), pool_lost_counts(walk = 4L))
})

test_that("a pool whose every walk failed stops uniform weighting", {
  skip_on_cran()
  local_walk_failure()

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = uniform_em_control()
    )
  )

  expect_stopped_at_start(res, c(0.3, -0.2), pool_lost_counts(walk = 4L))
})

test_that("a fully fixed-term-degenerate pool records its cause", {
  skip_on_cran()
  local_fixed_term_loss()

  for (control in list(analytic_em_control(), uniform_em_control())) {
    res <- run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = control
    )
    expect_stopped_at_start(
      res,
      c(0.3, -0.2),
      pool_lost_counts(fixed_term = 4L)
    )
  }
})

test_that("a pool lost to mixed causes records each cause", {
  skip_on_cran()
  # Odd-numbered entries fail their walk; the others lose the fixed-only term.
  entry <- 0L
  local_walk_failure(function() {
    entry <<- entry + 1L
    entry %% 2L == 1L
  })
  local_fixed_term_loss()

  res <- expect_no_error(
    run_abem_loop(
      theta0 = c(0.3, -0.2),
      decomposition = abem_pinned_choice_decomposition(),
      augmenter = new_stub_augmenter(abem_two_wave_paths()),
      control = analytic_em_control()
    )
  )

  expect_stopped_at_start(
    res,
    c(0.3, -0.2),
    pool_lost_counts(walk = 2L, fixed_term = 2L)
  )
})

# End-to-end: the stub augmenter over a small candidate set, the real
# compiled pool evaluator, and the real SGD M-step. The candidates are three
# orderings of one event stream, drawn in rotation with the uniform proposal
# density 1/3, so the loop's target is the marginal likelihood
# log mean_j L_j(theta), whose maximizer is computed directly.
test_that("the loop ascends a toy to its marginal-likelihood optimum", {
  skip_on_cran()
  candidates <- abem_toy_candidates()
  decomposition <- hand_decomposition(
    candidates[[1]],
    choice = ~ inertia + recip,
    theta_index = list(choice = 1:2)
  )
  exact_pool <- build_sequence_pool(
    decomposition,
    lapply(candidates, function(d) {
      list(data = d, reference = new_reference_record(c(0, 0), -1, -1))
    }),
    theta_length = 2L
  )
  marginal <- function(theta) {
    ll <- evaluate_sequence_pool(exact_pool, theta, what = "loglik")$loglik
    max(ll) + log(mean(exp(ll - max(ll))))
  }
  optimum <- stats::optim(c(0, 0), marginal, control = list(fnscale = -1))$par

  res <- run_abem_loop(
    theta0 = c(0.8, -0.6),
    decomposition = decomposition,
    augmenter = new_stub_augmenter(candidates),
    control = set_algorithm_em(
      n_sequences = 3L,
      max_iterations = 20L,
      tolerance = 1e-2,
      augmenter = set_augmenter_options(routine = "random"),
      optimizer = set_sgd_options(
        variant = "full",
        step_schedule = "adam",
        step_size = 0.1,
        max_iterations = 200L,
        sgd_tolerance = 1e-2
      )
    )
  )

  expect_true(res$converged)
  expect_true(any(res$em_trace$accepted))
  expect_equal(res$parameters, optimum, tolerance = 0.1)
})

# Each decision pass evaluates the pool once at the proposal for its
# log-likelihood, score and information together -- one compiled pass yields
# all three -- and hands the per-sequence score and information to the E-step
# object, which the per-pass standard errors then read instead of evaluating
# the pool again.
test_that("each pass builds its E-step from per-sequence score and Fisher", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 1)
  built <- list()
  real_estep <- new_dynes_estep
  local_mocked_bindings(
    new_dynes_estep = function(lambda, ...) {
      estep <- real_estep(lambda, ...)
      built[[length(built) + 1L]] <<- estep
      estep
    }
  )

  run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1, 2)),
    control = analytic_em_control(max_iterations = 1L)
  ) |>
    suppressWarnings()

  estep <- built[[1]]
  mu <- rep(c(1, 2), 2)
  # The analytic score at the proposal theta = 1 is mu - 1, the information 1.
  expect_equal(estep$score, matrix(mu - 1, ncol = 1))
  expect_equal(estep$fisher, rep(list(diag(1)), 4))
})

test_that("per-pass standard errors reuse the pass's E-step quantities", {
  local_analytic_evaluator()
  proposals <- c(0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    proposals[[pass]]
  })
  log <- local_mstep_counter()

  res <- run_abem_loop(
    theta0 = 0,
    decomposition = analytic_decomposition(),
    augmenter = new_stub_augmenter(list(1, 2)),
    control = analytic_em_control(em_trace_se = TRUE)
  )

  # Beyond pool entry, one full evaluation per pass, at its proposal.
  at_proposal <- Filter(
    function(e) identical(e$phase, "proposal"),
    log$evaluations
  )
  expect_length(at_proposal, nrow(res$em_trace))
  for (e in at_proposal) {
    expect_identical(e$what, c("loglik", "score", "fisher"))
  }
  # The SE is Louis's over the pass's pool at the proposal theta = 0.5: with
  # score mu - theta and unit information, I_obs is sum w less the weighted
  # variance of the scores, w the weights at the proposal.
  mu <- c(1, 2, 1, 2)
  w <- compute_pool_weights(-(mu - 0.5)^2 / 2, rep(-log(2), 4))
  s <- mu - 0.5
  expected <- sqrt(1 / (sum(w) - (sum(w * s^2) - sum(w * s)^2)))
  expect_equal(res$em_trace$se[[1]], expected)
})

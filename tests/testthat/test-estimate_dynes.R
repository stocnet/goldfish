# The estimate_dynes() surface: completion once at entry, the decomposition of
# the completed joint specification into pool sub-models, theta0 (zero, the
# user's goldfishParams, or a per-fid warm start), the L'Ecuyer-CMRG substream
# keying of every draw by (iteration, draw_index), and the per-sequence map
# seam with its serial default and warn-once fallback. The loop itself is
# covered in test-em_loop_dynes.R; here it runs over the analytic evaluator
# and a keyed stub augmenter (helper-abem_loop.R, helper-estimate_dynes.R).

collect_warnings <- function(expr) {
  messages <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = messages)
}

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# grow, accept, then (after the ESS guard redraws the degenerate pool) stop.
local_grow_redraw_script <- function(env = parent.frame()) {
  proposals <- c(-0.5, 0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(
    function(theta) {
      pass <<- pass + 1L
      rep(proposals[[pass]], length(theta))
    },
    env = env
  )
}

surface_control <- function(...) {
  args <- list(
    n_sequences = 4L,
    seed = 42L,
    augmenter = set_augmenter_options(routine = "random")
  )
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(set_algorithm_em, args)
}

# --- Surface ----------------------------------------------------------------

test_that("estimate_dynes is exported now that real augmenters exist", {
  expect_contains(getNamespaceExports("goldfish"), "estimate_dynes")
})

test_that("estimate_dynes without an augmenter aborts informatively", {
  local_mocked_bindings(dynes_augmenter_constructors = function() list())
  expect_error(
    estimate_dynes(dynes_surface_spec(), surface_control()),
    class = "goldfish_dynes_no_augmenter"
  )
})

test_that("estimate_dynes rejects a non-joint spec and a non-EM control", {
  expect_error(estimate_dynes(list(), surface_control()), "goldfishJointSpec")
  expect_error(
    estimate_dynes(dynes_surface_spec(), set_algorithm_newton()),
    "goldfishAlgoEm"
  )
})

# --- Panel-augmentation trigger ---------------------------------------------

# friendship is panel-observed and modeled by its own specification, built
# directly on the panel data (no event-observed copy), so it is a modeled panel
# process: the augmentation target.
direct_panel_spec <- function() {
  data <- dynes_multiwave_data("panel")
  make_joint_specification(
    make_specification(
      choice = ~ inertia + offset(recip, coef = -0.5),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

test_that("a panel layer modeled as a process is the augmentation target", {
  data <- dynes_multiwave_data("panel")
  # No separate panel-semantics flag: the observation metadata plus the layer
  # being a modeled process is the whole trigger.
  expect_null(data$info$panel_semantics)
  expect_identical(direct_panel_spec()$modeled_panel, "friendship")
})

test_that("an exogenous-only panel reference is not an augmentation target", {
  # friendship is panel-observed but only read as a covariate.
  expect_length(dynes_surface_spec(calls = "event")$modeled_panel, 0)
})

test_that("estimate_dynes aborts to estimate_dynam without a modeled panel", {
  local_cli_context()
  expect_snapshot(
    estimate_dynes(dynes_surface_spec(calls = "event"), surface_control()),
    error = TRUE
  )
  expect_error(
    estimate_dynes(dynes_surface_spec(calls = "event"), surface_control()),
    class = "goldfish_dynes_no_modeled_panel"
  )
})

test_that("estimate_dynes accepts a modeled panel process", {
  # Past the gate, the run stops only for want of an augmenter.
  local_mocked_bindings(dynes_augmenter_constructors = function() list())
  expect_error(
    suppressWarnings(estimate_dynes(direct_panel_spec(), surface_control())),
    class = "goldfish_dynes_no_augmenter"
  )
})

test_that("estimate_dynes names the estimates by the composite labels", {
  local_analytic_evaluator()
  local_grow_redraw_script()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  spec <- dynes_surface_spec()

  res <- suppressWarnings(estimate_dynes(spec, surface_control()))

  layout <- coef_layout(spec)
  free <- !layout$fixed
  expect_named(
    res$parameters,
    paste0(layout$process[free], ": ", layout$name[free])
  )
  expect_true(res$converged)
})

test_that("a goldfishParams initial value is theta0, unpinned slots at zero", {
  local_analytic_evaluator()
  captured <- NULL
  local_mocked_bindings(
    run_abem_loop = function(theta0, ...) {
      captured <<- theta0
      list(parameters = theta0, converged = TRUE, em_trace = NULL)
    },
    new_dynes_fit = function(res, ...) res
  )
  local_dynes_augmenter(new_keyed_stub_augmenter())
  spec <- dynes_surface_spec()
  start <- set_parameters(
    spec,
    `calls › rate` = c(-1, NA)
  )

  estimate_dynes(
    spec,
    surface_control(
      initial_parameters = start,
      augmenter = set_augmenter_options(routine = "sim")
    )
  )

  expect_equal(unname(captured), c(-1, 0, 0, 0, 0, 0, 0))
})

test_that("estimate_dynes leaves the caller's RNG kind and state untouched", {
  local_analytic_evaluator()
  local_grow_redraw_script()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  withr::local_seed(7)
  kind <- RNGkind()
  before <- .Random.seed

  suppressWarnings(estimate_dynes(dynes_surface_spec(), surface_control()))

  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, before)
})

test_that("a NULL seed is drawn, recorded, and reproduces the run", {
  local_analytic_evaluator()
  local_grow_redraw_script()
  aug <- new_keyed_stub_augmenter()
  local_dynes_augmenter(aug)
  control <- set_algorithm_em(
    n_sequences = 4L,
    augmenter = set_augmenter_options(routine = "random")
  )

  first <- suppressWarnings(estimate_dynes(dynes_surface_spec(), control))
  first_log <- aug$state$log

  expect_true(rlang::is_scalar_integer(first$seed))
  aug$state$log <- list()
  local_grow_redraw_script()
  control$seed <- first$seed
  suppressWarnings(estimate_dynes(dynes_surface_spec(), control))
  expect_identical(aug$state$log, first_log)
})

# --- Sub-model decomposition -------------------------------------------------

test_that("the decomposition of a completed spec passes the layout check", {
  spec <- dynes_multiwave_spec()

  sub_models <- decompose_joint_spec(spec)$sub_models

  expect_length(sub_models, nrow(spec$process_map))
  expect_no_error(assert_theta_layout(spec, sub_models))
  layout <- coef_layout(spec)
  for (i in seq_along(sub_models)) {
    fid <- spec$process_map$fid[[i]]
    expect_identical(
      sub_models[[i]]$theta_index,
      layout$index[layout$fid == fid & !layout$fixed]
    )
  }
})

test_that("completed fids decompose into fixed-only sub-models", {
  spec <- dynes_multiwave_spec()
  map <- spec$process_map

  sub_models <- decompose_joint_spec(spec)$sub_models

  completed <- which(map$completed)
  expect_length(completed, 2L)
  for (i in completed) {
    expect_true(sub_models[[i]]$fixed_only)
    expect_identical(sub_models[[i]]$theta_index, integer(0))
  }
  rate <- completed[map$family[completed] == "rate"]
  expect_identical(
    sub_models[[rate]]$pinned_rate,
    spec$completed_rates[[as.character(map$fid[[rate]])]]
  )
  expect_length(sub_models[[rate]]$pinned_rate$intercept, 2L)
  choice <- completed[map$family[completed] == "choice"]
  expect_null(sub_models[[choice]]$pinned_rate)
  # coef_layout() numbers no slot for them.
  layout <- coef_layout(spec)
  expect_true(all(is.na(layout$index[layout$fid %in% map$fid[completed]])))
})

test_that("the offset term never takes a theta slot", {
  spec <- dynes_multiwave_spec()

  sub_models <- decompose_joint_spec(spec)$sub_models

  choice <- which(
    spec$process_map$layer == "friendship" &
      spec$process_map$family == "choice"
  )
  # inertia is free, the recip offset is not.
  expect_length(sub_models[[choice]]$theta_index, 1L)
})

test_that("a proposal evaluator over the decomposition answers rate queries", {
  spec <- dynes_multiwave_spec()
  decomposition <- decompose_joint_spec(spec)
  theta <- c(0.2, -1, 0.1)

  eval_fn <- make_proposal_evaluator(decomposition, theta, spec = spec)
  out <- suppressWarnings(eval_fn(
    spec$data,
    rate_queries = list(
      list(after = 1L, layer = "calls", sender = "N2", receiver = "N3")
    )
  ))

  expect_length(out$rates, 1L)
  expect_true(is.finite(out$rates) && out$rates > 0)
})

test_that("the decomposition of the rq layout matches its hand numbering", {
  spec <- rq_layout_spec()

  sub_models <- decompose_joint_spec(spec)$sub_models

  expect_identical(
    lapply(sub_models, `[[`, "theta_index"),
    list(1:2, 3:4, 5:6, integer(0))
  )
})

test_that("a flavored joint spec decomposes into one sub-model per fid", {
  spec <- dynes_flavored_spec()

  decomposition <- decompose_joint_spec(spec)

  expect_s3_class(decomposition, "goldfishDynesDecomposition")
  expect_s3_class(decomposition$engine_spec, "goldfishJointSpec")
  sub_models <- decomposition$sub_models
  expect_identical(
    vapply(sub_models, `[[`, integer(1), "fid"),
    spec$process_map$fid
  )
  expect_no_error(assert_theta_layout(spec, sub_models))
})

test_that("estimate_dynes accepts a flavored joint specification", {
  local_analytic_evaluator()
  local_grow_redraw_script()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  spec <- dynes_flavored_spec(dynes_flavored_data(observation = "panel"))

  res <- suppressWarnings(estimate_dynes(spec, surface_control()))

  layout <- coef_layout(suppressWarnings(complete_generative_spec(spec)))
  free <- !layout$fixed
  expect_named(
    res$parameters,
    paste0(layout$process[free], ": ", layout$name[free])
  )
  expect_true(res$converged)
})

# --- Warm start --------------------------------------------------------------

test_that("a failed warm-start fit falls back to zero for that fid only", {
  captured <- NULL
  local_mocked_bindings(
    run_abem_loop = function(theta0, ...) {
      captured <<- theta0
      list(parameters = theta0, converged = TRUE, em_trace = NULL)
    },
    new_dynes_fit = function(res, ...) res
  )
  drawn <- list()
  local_dynes_augmenter(list(
    draw = function(iteration, draw_index, theta) {
      drawn[[length(drawn) + 1L]] <<- list(iteration, draw_index, theta)
      lapply(draw_index, function(k) {
        list(data = rq_tied_data("panel"), log_proposal = 0)
      })
    }
  ))
  control <- surface_control(
    augmenter = set_augmenter_options(routine = "sim", warm_start = TRUE)
  )

  out <- collect_warnings(estimate_dynes(rq_layout_spec("panel"), control))

  # The warm-start draw is keyed at iteration 0, from theta = 0.
  expect_identical(drawn[[1]][[1]], 0L)
  expect_identical(drawn[[1]][[2]], 1L)
  expect_equal(drawn[[1]][[3]], rep(0, 6))
  # calls' choice is singular on the one drawn sequence.
  expect_equal(unname(captured[3:4]), c(0, 0))
  expect_length(grep("calls › choice", out$warnings, fixed = TRUE), 1L)
  # The two rates keep their fitted starts: a default Newton fit over the
  # drawn sequence's joint statistics, here from the raw spec's own walk (it
  # holds both rates, and the completed uniform emails choice adds no
  # censoring row). Each rate is censored at the other layer's events.
  walk <- suppressWarnings(preprocess_joint(
    rq_layout_raw_spec(),
    control_preprocessing = set_preprocessing()
  ))
  direct <- function(formula, stats) {
    suppressWarnings(estimate_wrapper(
      x = formula,
      model = "DyNAM",
      sub_model = "rate",
      data = rq_tied_data("panel"),
      preprocessed = stats,
      control_algo = set_algorithm_newton(),
      verbose = FALSE,
      progress = FALSE
    ))$parameters
  }
  expect_equal(
    unname(captured[1:2]),
    unname(direct(calls ~ 1 + indeg(friendship), walk[["1"]]))
  )
  expect_equal(
    unname(captured[5:6]),
    unname(direct(emails ~ 1 + indeg, walk[["3"]]))
  )
})

test_that("the warm-start fallback warning names the fid", {
  local_abem_cli_context()
  local_mocked_bindings(
    run_abem_loop = function(theta0, ...) {
      list(parameters = theta0, converged = TRUE, em_trace = NULL)
    },
    new_dynes_fit = function(res, ...) res,
    # The stub augmenter's draws are not sequences; the walk is not the point.
    preprocess_pool_sequence = function(...) list(),
    dynes_warm_start_fit = function(sub_model, data, stats) {
      if (identical(sub_model$sub_model, "choice")) {
        stop("Matrix cannot be inverted")
      }
      rep(0.5, length(sub_model$theta_index))
    }
  )
  local_dynes_augmenter(new_keyed_stub_augmenter())
  control <- surface_control(
    augmenter = set_augmenter_options(routine = "sim", warm_start = TRUE)
  )

  expect_snapshot(res <- estimate_dynes(rq_layout_spec("panel"), control))
})

# --- RNG keying and the map seam --------------------------------------------

test_that("a draw's substream depends on its key, not on the request order", {
  root <- withr::with_seed(
    1,
    {
      RNGkind("L'Ecuyer-CMRG")
      set.seed(3)
      seed <- .Random.seed
      RNGkind("default")
      seed
    }
  )
  sequential <- new_draw_keyer(root)
  for (k in 1:4) {
    draw_seed(sequential, 1L, k)
  }
  jumped <- new_draw_keyer(root)

  expect_identical(draw_seed(sequential, 1L, 5L), draw_seed(jumped, 1L, 5L))
  expect_identical(
    draw_seed(new_draw_keyer(root), 2L, 3L),
    draw_seed(jumped, 2L, 3L)
  )
  expect_false(identical(draw_seed(jumped, 1L, 1L), draw_seed(jumped, 2L, 1L)))
  expect_false(identical(draw_seed(jumped, 0L, 1L), draw_seed(jumped, 1L, 1L)))
})

test_that("draws match across worker counts through growth and redraw", {
  local_analytic_evaluator()
  run <- function(n_cores) {
    local_grow_redraw_script()
    aug <- new_keyed_stub_augmenter()
    local_dynes_augmenter(aug)
    control <- if (is.null(n_cores)) {
      surface_control()
    } else {
      surface_control(n_cores = n_cores)
    }
    out <- collect_warnings(estimate_dynes(dynes_surface_spec(), control))
    list(res = out$value, log = aug$state$log, warnings = out$warnings)
  }

  serial <- run(1L)
  backend <- local_reversed_parallel_backend()
  parallel <- run(2L)

  expect_gt(backend$maps, 0L)
  # The run grew the pool and the ESS guard redrew it.
  expect_contains(serial$res$em_trace$decision, "grow")
  expect_true(any(grepl("redrawing", serial$warnings)))
  expect_identical(parallel$log, serial$log)
  expect_identical(
    lapply(parallel$res$pool$sequences, `[[`, "data"),
    lapply(serial$res$pool$sequences, `[[`, "data")
  )
  expect_identical(parallel$res$parameters, serial$res$parameters)
  expect_identical(parallel$res$em_trace, serial$res$em_trace)
})

test_that("draw_index runs monotonically through a redraw", {
  local_analytic_evaluator()
  local_grow_redraw_script()
  aug <- new_keyed_stub_augmenter()
  local_dynes_augmenter(aug)

  suppressWarnings(estimate_dynes(dynes_surface_spec(), surface_control()))

  keys <- do.call(rbind, strsplit(names(aug$state$log), " "))
  second <- as.integer(keys[keys[, 1] == "2", 2])
  # The redraw consumes a fresh range at iteration 2, starting at 1.
  expect_identical(second, seq_along(second))
})

test_that("estimates are identical across worker counts under real SGD", {
  local_analytic_evaluator()
  run <- function(n_cores) {
    local_dynes_augmenter(list(
      draw = function(iteration, draw_index, theta) {
        lapply(draw_index, function(k) {
          list(data = stats::rnorm(1, 1, 0.5), log_proposal = 0)
        })
      }
    ))
    suppressWarnings(tryCatch(
      estimate_dynes(
        dynes_surface_spec(),
        surface_control(
          n_sequences = 10L,
          max_iterations = 3L,
          n_cores = n_cores,
          optimizer = set_sgd_options(step_size = 0.2)
        )
      ),
      goldfish_abem_retry_exhausted = function(e) e$em_trace
    ))
  }

  serial <- run(1L)
  local_reversed_parallel_backend()
  parallel <- run(2L)

  expect_identical(parallel, serial)
})

test_that("real mirai daemons give the serial estimates bit for bit", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  local_dynes_augmenter(new_wave_grid_augmenter())
  spec <- dynes_varying_raw_spec()
  run <- function(n_cores) {
    suppressWarnings(estimate_dynes(
      spec,
      surface_control(max_iterations = 2L, n_cores = n_cores)
    ))
  }

  serial <- run(1L)
  parallel <- run(2L)

  # The fit holds no seam or store handle, so nothing is meant to differ.
  expect_identical(parallel, serial)
})

test_that("the default core count stays serial and silent", {
  local_analytic_evaluator()
  local_grow_redraw_script()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  backend <- local_reversed_parallel_backend()

  out <- collect_warnings(
    estimate_dynes(dynes_surface_spec(), surface_control())
  )

  expect_identical(backend$maps, 0L)
  expect_false(any(grepl("mirai", out$warnings)))
})

test_that("an explicit n_cores without mirai warns once and runs serially", {
  local_analytic_evaluator()
  local_mocked_bindings(dynes_mirai_available = function() FALSE)
  run <- function(n_cores) {
    local_grow_redraw_script()
    aug <- new_keyed_stub_augmenter()
    local_dynes_augmenter(aug)
    out <- collect_warnings(
      estimate_dynes(dynes_surface_spec(), surface_control(n_cores = n_cores))
    )
    list(log = aug$state$log, res = out$value, warnings = out$warnings)
  }

  serial <- run(1L)
  requested <- run(2L)

  expect_length(grep("mirai", requested$warnings), 1L)
  expect_length(grep("mirai", serial$warnings), 0L)
  expect_identical(requested$log, serial$log)
  expect_identical(requested$res$parameters, serial$res$parameters)
})

test_that("the explicit-n_cores-without-mirai warning", {
  local_abem_cli_context()
  local_mocked_bindings(dynes_mirai_available = function() FALSE)

  expect_snapshot(seam <- dynes_map_seam(surface_control(n_cores = 3L)))
  seam$close()
})

test_that("the parallel seam keeps workers times BLAS threads within n_cores", {
  backend <- local_reversed_parallel_backend()

  seam <- dynes_map_seam(surface_control(n_cores = 4L))
  # A bare on.exit() would replace the handler that restores the mocks.
  withr::defer(seam$close())

  started <- backend$started[[1]]
  expect_lte(started[["n_workers"]] * started[["blas_threads"]], 4)
  expect_identical(
    seam$map(list(1, 2, 3), function(x) x * 10),
    list(10, 20, 30)
  )
})

# --- Fixed-term degeneracy ----------------------------------------------------

# A store whose tally is set by hand: the helpers read nothing else. A dropped
# pool is lost to the fixed-only cause alone unless `pool_lost` says otherwise.
tallied_store <- function(
  built,
  fixed_term,
  labels,
  pool_dropped = FALSE,
  pool_lost = if (pool_dropped) {
    c(walk = 0L, fixed_term = fixed_term, evaluation = 0L)
  }
) {
  store <- new.env(parent = emptyenv())
  store$tally <- list(
    built = built,
    fixed_term = fixed_term,
    labels = labels,
    pool_dropped = pool_dropped,
    pool_lost = pool_lost
  )
  store
}

# One estimate_dynes() run over the multiwave fixture, with every warning
# muffled and the fixed-term, pool-degenerate, singular-information and
# observed-information ones kept, by class.
fixed_term_run <- function(control = surface_control(max_iterations = 2L)) {
  kept <- list()
  pool <- list()
  singular <- list()
  not_pd <- list()
  fit <- withCallingHandlers(
    estimate_dynes(dynes_varying_raw_spec(), control),
    goldfish_dynes_fixed_term_degenerate = function(w) {
      kept[[length(kept) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    goldfish_dynes_pool_degenerate = function(w) {
      pool[[length(pool) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    goldfish_dynes_singular_information = function(w) {
      singular[[length(singular) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    goldfish_dynes_observed_information_not_pd = function(w) {
      not_pd[[length(not_pd) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  list(
    fit = fit,
    warnings = kept,
    pool_warnings = pool,
    singular_warnings = singular,
    not_pd_warnings = not_pd
  )
}

# The surface control under uniform weighting, whose one valid cell is the
# mcmc routine with a refreshed pool.
uniform_surface_control <- function() {
  surface_control(
    max_iterations = 2L,
    augmenter = set_augmenter_options(routine = "mcmc"),
    weights = set_weights_options(weighting = "uniform", refresh = TRUE)
  )
}

# A stop's own warning is the only report: neither covariance warning fires.
expect_stopped_fit <- function(run) {
  fit <- run$fit
  expect_s3_class(fit, "goldfishDynesFit")
  expect_false(fit$converged)
  expect_identical(nrow(fit$em_trace), 0L)
  expect_true(all(is.na(fit$covariance)))
  expect_identical(fit$covariance_status, "pool_dropped")
  expect_identical(fit$newton_se, NA_real_)
  expect_identical(fit$at_optimum, NA)
  expect_null(fit$information)
  expect_true(all(is.na(vcov(fit))))
  expect_true(all(is.na(vcov(fit, information = "complete_data"))))
  expect_length(run$singular_warnings, 0L)
  expect_length(run$not_pd_warnings, 0L)
}

test_that("the fixed-term degeneracy warning", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    warn_fixed_term_degenerate(
      tallied_store(12L, 5L, c("friendship (rate)", "calls (choice)"))
    )
  )
})

test_that("the fixed-term degeneracy warning after a full pool drop", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    warn_fixed_term_degenerate(
      tallied_store(4L, 4L, "friendship (rate)", pool_dropped = TRUE)
    )
  )
})

test_that("the fixed-term warning is classed and silent at a zero count", {
  expect_warning(
    warn_fixed_term_degenerate(tallied_store(4L, 1L, "friendship (rate)")),
    class = "goldfish_dynes_fixed_term_degenerate"
  )
  expect_no_warning(
    warn_fixed_term_degenerate(tallied_store(4L, 0L, character(0)))
  )
})

test_that("a fixed-only term lost for some sequences warns once, is traced", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  real_fixed_only_loglik <- fixed_only_loglik
  # Keyed on the drawn path, so only some sequences lose the pinned
  # friendship rate: those whose first latent friendship event falls early.
  local_mocked_bindings(
    fixed_only_loglik = function(sub_model, stats) {
      dependent <- stats$event_time[stats$is_dependent == 1L]
      if (!is.null(sub_model$pinned_rate) && min(dependent) < 1.6) {
        return(-Inf)
      }
      real_fixed_only_loglik(sub_model, stats)
    }
  )

  run <- fixed_term_run()

  expect_length(run$warnings, 1L)
  expect_no_match(
    conditionMessage(run$warnings[[1]]),
    "Every sequence in the pool was dropped"
  )
  trace <- run$fit$em_trace
  expect_type(trace$fixed_degenerate, "integer")
  expect_true(all(trace$fixed_degenerate > 0L))
  expect_true(all(trace$fixed_degenerate < trace$pool_size))
})

test_that("a run with finite fixed-only terms neither warns nor traces one", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())

  run <- fixed_term_run()

  expect_length(run$warnings, 0L)
  columns <- names(run$fit$em_trace)
  expect_identical(
    match("fixed_degenerate", columns),
    match("pool_size", columns) + 1L
  )
  expect_identical(
    run$fit$em_trace$fixed_degenerate,
    rep(0L, nrow(run$fit$em_trace))
  )
})

test_that("a pool lost entirely to a fixed-only term stops unconverged", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  local_fixed_term_loss()

  run <- expect_no_error(fixed_term_run())

  expect_length(run$warnings, 1L)
  expect_match(
    conditionMessage(run$warnings[[1]]),
    "Every sequence in the pool was dropped"
  )
  fit <- run$fit
  expect_s3_class(fit, "goldfishDynesFit")
  expect_false(fit$converged)
  expect_equal(unname(fit$parameters), rep(0, length(fit$parameters)))
  expect_identical(nrow(fit$em_trace), 0L)
  expect_true(all(is.na(fit$covariance)))
  expect_true(all(is.na(fit$mc_covariance)))
  expect_identical(fit$covariance_status, "pool_dropped")
  expect_identical(fit$fraction_missing, NA_real_)
  expect_identical(fit$covariance_ess, NA_real_)
  expect_length(run$pool_warnings, 0L)
  expect_length(run$singular_warnings, 0L)
  expect_length(run$not_pd_warnings, 0L)
})

test_that("a pool lost to a fixed-only term under uniform weighting", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  local_fixed_term_loss()

  run <- expect_no_error(fixed_term_run(uniform_surface_control()))

  expect_length(run$warnings, 1L)
  expect_match(
    conditionMessage(run$warnings[[1]]),
    "Every sequence in the pool was dropped"
  )
  expect_length(run$pool_warnings, 0L)
  expect_stopped_fit(run)
  expect_length(run$singular_warnings, 0L)
})

# --- Pool degeneracy for other causes ----------------------------------------

test_that("the pool degeneracy warning", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    warn_pool_degenerate(
      tallied_store(
        8L,
        0L,
        character(0),
        pool_dropped = TRUE,
        pool_lost = c(walk = 3L, fixed_term = 0L, evaluation = 1L)
      )
    )
  )
})

test_that("both degeneracy warnings after a pool lost to mixed causes", {
  local_reproducible_output(width = 80)
  store <- tallied_store(
    4L,
    2L,
    "friendship (rate)",
    pool_dropped = TRUE,
    pool_lost = c(walk = 2L, fixed_term = 2L, evaluation = 0L)
  )
  expect_snapshot({
    warn_fixed_term_degenerate(store)
    warn_pool_degenerate(store)
  })
})

test_that("the pool degeneracy warning is classed and silent otherwise", {
  expect_warning(
    warn_pool_degenerate(
      tallied_store(
        4L,
        0L,
        character(0),
        pool_dropped = TRUE,
        pool_lost = c(walk = 0L, fixed_term = 0L, evaluation = 4L)
      )
    ),
    class = "goldfish_dynes_pool_degenerate"
  )
  # No stop, or a stop the fixed-term warning already reports.
  expect_no_warning(
    warn_pool_degenerate(tallied_store(4L, 1L, "friendship (rate)"))
  )
  expect_no_warning(
    warn_pool_degenerate(
      tallied_store(4L, 4L, "friendship (rate)", pool_dropped = TRUE)
    )
  )
})

test_that("a pool whose every walk failed stops with one pool warning", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  local_walk_failure()

  for (control in list(
    surface_control(max_iterations = 2L),
    uniform_surface_control()
  )) {
    run <- expect_no_error(fixed_term_run(control))

    expect_length(run$pool_warnings, 1L)
    expect_length(run$warnings, 0L)
    expect_stopped_fit(run)
    expect_length(run$singular_warnings, 0L)
  }
})

test_that("a pool with every evaluation non-finite stops with one warning", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  local_evaluation_failure()

  for (control in list(
    surface_control(max_iterations = 2L),
    uniform_surface_control()
  )) {
    run <- expect_no_error(fixed_term_run(control))

    expect_length(run$pool_warnings, 1L)
    expect_length(run$warnings, 0L)
    expect_stopped_fit(run)
    expect_length(run$singular_warnings, 0L)
  }
})

test_that("a pool lost to mixed causes warns once per cause", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  entry <- 0L
  local_walk_failure(function() {
    entry <<- entry + 1L
    entry %% 2L == 1L
  })
  local_fixed_term_loss()

  run <- expect_no_error(fixed_term_run())

  expect_length(run$warnings, 1L)
  expect_no_match(
    conditionMessage(run$warnings[[1]]),
    "Every sequence in the pool was dropped"
  )
  expect_length(run$pool_warnings, 1L)
  expect_match(
    conditionMessage(run$pool_warnings[[1]]),
    "estimation stopped"
  )
  expect_stopped_fit(run)
  expect_length(run$singular_warnings, 0L)
})

# --- Specification validation and entry wiring ------------------------------

# One estimate_dynes() entry up to the loop, which is stubbed: what the entry
# hands every f path (the completed spec, the recipe, the preprocessing
# controls of the pool store, the warm start and the loop) and every call to
# complete_generative_spec(). Warnings are muffled; messages are kept.
dynes_entry <- function(
  spec,
  control_prep = set_preprocessing(),
  wave_times = NULL,
  warm_start = TRUE,
  completion = complete_generative_spec
) {
  # Forced before the mock below replaces the binding it names.
  force(completion)
  seen <- new.env(parent = emptyenv())
  seen$completions <- list()
  seen$messages <- list()
  real_header <- pool_store_header
  real_recipe <- compile_augmentation_recipe
  local_mocked_bindings(
    complete_generative_spec = function(joint_spec, ...) {
      seen$completions[[length(seen$completions) + 1L]] <- list(...)
      completion(joint_spec, ...)
    },
    compile_augmentation_recipe = function(spec, wave_times, ...) {
      seen$recipe <- real_recipe(spec, wave_times, ...)
      seen$recipe
    },
    pool_store_header = function(decomposition, n_free, control_prep, ...) {
      seen$header_prep <- control_prep
      real_header(decomposition, n_free, control_prep, ...)
    },
    dynes_warm_start = function(
      decomposition,
      augmenter,
      n_free,
      control_prep,
      call
    ) {
      seen$warm_prep <- control_prep
      numeric(n_free)
    },
    run_abem_loop = function(
      theta0,
      decomposition,
      augmenter,
      control_algo,
      spec,
      control_prep,
      ...
    ) {
      seen$loop_prep <- control_prep
      seen$spec <- spec
      seen$decomposition <- decomposition
      seen$augmenter <- augmenter
      list(parameters = theta0, converged = TRUE, em_trace = NULL)
    },
    new_dynes_fit = function(res, ...) res
  )
  control <- surface_control(
    augmenter = set_augmenter_options(
      routine = "random",
      warm_start = warm_start
    )
  )
  withCallingHandlers(
    estimate_dynes(spec, control, control_prep, wave_times = wave_times),
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) {
      seen$messages[[length(seen$messages) + 1L]] <- m
      invokeRestart("muffleMessage")
    }
  )
  seen
}

# The count panel calls beside the observed emails stream, with formulas
# chosen per test: calls reads emails, emails reads calls, or neither.
count_panel_spec <- function(calls_choice, emails_choice) {
  data <- recipe_count_data()
  make_joint_specification(
    make_specification(
      rate = ~1,
      choice = calls_choice,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = emails_choice,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# recipe_state_spec() with the calls choice formula replaced.
state_panel_spec <- function(calls_choice) {
  data <- recipe_state_data()
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = calls_choice,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

test_that("a partially modeled panel layer aborts", {
  local_cli_context()
  data <- recipe_state_data()
  spec <- make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )

  expect_snapshot(
    estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10)),
    error = TRUE
  )
})

test_that("a modeled count panel with only separable formulas estimates", {
  spec <- count_panel_spec(~ tie(emails), ~inertia)
  calls <- spec$process_map$layer == "calls"
  expect_false(any(spec$process_map$coupled[calls]))

  seen <- dynes_entry(spec, wave_times = c(0, 5, 10))

  expect_s3_class(seen$spec, "goldfishJointSpec")
})

test_that("mutually dependent panel and event layers estimate jointly", {
  spec <- count_panel_spec(~ tie(emails), ~ tie(calls))
  emails <- spec$process_map$layer == "emails"
  expect_true(all(spec$process_map$coupled[
    emails & spec$process_map$family == "choice"
  ]))

  seen <- dynes_entry(spec, wave_times = c(0, 5, 10))

  expect_s3_class(seen$spec, "goldfishJointSpec")
  # Only the emails rate, reading nothing, is separable; the choice that
  # reads the latent calls path is not.
  named <- vapply(seen$messages, conditionMessage, character(1))
  expect_no_match(named, "emails › choice", fixed = TRUE)
})

test_that("a mixed specification names its separable fids and proceeds", {
  local_cli_context()
  spec <- count_panel_spec(~inertia, ~inertia)

  seen <- dynes_entry(spec, wave_times = c(0, 5, 10))

  expect_s3_class(seen$spec, "goldfishJointSpec")
  expect_length(seen$messages, 1L)
  expect_s3_class(seen$messages[[1]], "goldfish_dynes_separable_fids")
  expect_snapshot(cat(conditionMessage(seen$messages[[1]])))
})

test_that("a window over a modeled panel layer is refused at entry", {
  local_cli_context()
  spec <- state_panel_spec(~ inertia + tie(friendship, window = 3))

  expect_snapshot(
    estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10)),
    error = TRUE
  )
  expect_error(
    estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10)),
    class = "goldfish_dynes_panel_window"
  )
})

test_that("a window over an observed layer passes validation", {
  spec <- state_panel_spec(~ inertia(calls, window = 3) + tie(friendship))

  seen <- dynes_entry(spec, wave_times = c(0, 5, 10))

  expect_s3_class(seen$spec, "goldfishJointSpec")
})

test_that("completion runs once at entry, on the panel wave grid", {
  seen <- dynes_entry(
    recipe_state_spec(),
    control_prep = set_preprocessing(start_time = 0)
  )

  expect_length(seen$completions, 1L)
  args <- seen$completions[[1]]
  expect_identical(args$consumer, "estimate_dynes")
  expect_identical(args$wave_times, c(0, 5, 10))
  expect_identical(seen$recipe$wave_times, c(0, 5, 10))
})

test_that("completing without the grid would pin one flat plateau", {
  spec <- recipe_state_spec()

  flat <- suppressWarnings(complete_generative_spec(spec))
  waved <- suppressWarnings(
    complete_generative_spec(spec, wave_times = c(0, 5, 10))
  )

  for (pin in flat$completed_rates) {
    expect_length(pin$intercept, 1L)
  }
  for (pin in waved$completed_rates) {
    expect_length(pin$intercept, 2L)
  }
})

test_that("every pin shares the recipe's grid", {
  seen <- dynes_entry(
    recipe_state_spec(),
    control_prep = set_preprocessing(start_time = 0)
  )

  pins <- seen$spec$completed_rates
  expect_length(pins, 2L)
  for (pin in pins) {
    expect_identical(as.numeric(stored_pin_grid(pin)), seen$recipe$wave_times)
  }
})

test_that("a pin on a grid other than the recipe's aborts as internal", {
  # A completion that pins on its own grid: the only route to a -Inf pin
  # with events in its period.
  real_completion <- complete_generative_spec
  off_grid <- function(joint_spec, ...) {
    args <- list(...)
    args$wave_times <- c(0, 4, 10)
    do.call(real_completion, c(list(joint_spec), args))
  }

  expect_error(
    dynes_entry(
      recipe_state_spec(),
      control_prep = set_preprocessing(start_time = 0),
      completion = off_grid
    ),
    class = "goldfish_dynes_pin_grid_mismatch"
  )
})

test_that("every f path runs on the window from t0 to the last wave", {
  seen <- dynes_entry(
    recipe_state_spec(),
    control_prep = set_preprocessing(start_time = 0, end_time = 20)
  )
  for (prep in list(seen$header_prep, seen$warm_prep, seen$loop_prep)) {
    expect_identical(prep$start_time, 0)
    expect_identical(prep$end_time, 10)
  }

  # An explicit grid sets the window when the controls set none.
  seen <- dynes_entry(recipe_state_spec(), wave_times = c(0, 5, 10))
  for (prep in list(seen$header_prep, seen$warm_prep, seen$loop_prep)) {
    expect_identical(prep$start_time, 0)
    expect_identical(prep$end_time, 10)
  }
})

test_that("modeled events before the first wave are scored", {
  seen <- dynes_entry(
    recipe_state_spec(),
    control_prep = set_preprocessing(start_time = 0)
  )
  # No wave is history: the first interval opens at t0, not at wave 5.
  expect_identical(seen$recipe$wave_times[[1]], 0)
  theta <- numeric(sum(!coef_layout(seen$spec)$fixed))
  withr::local_seed(1L)
  drawn <- seen$augmenter$draw(1L, 1L, theta)[[1]]$data

  calls_rate <- Filter(
    function(sm) identical(sm$label, "calls › rate"),
    seen$decomposition$sub_models
  )[[1]]
  scored <- function(prep) {
    stats <- suppressWarnings(preprocess_pool_sequence(
      seen$decomposition$engine_spec,
      drawn,
      control_prep = prep
    ))
    calls <- fid_stats(stats, calls_rate)
    calls$event_time[calls$is_dependent == 1L]
  }

  # The calls at 1 and 3 fall in the first interval and are scored.
  expect_identical(scored(seen$loop_prep), c(1, 3, 6, 8))
  expect_identical(
    scored(set_preprocessing(start_time = 5, end_time = 10)),
    c(6, 8)
  )
})

test_that("completed fids contribute no theta, pinned per wave period", {
  seen <- dynes_entry(
    recipe_state_spec(),
    control_prep = set_preprocessing(start_time = 0)
  )
  spec <- seen$spec
  layout <- coef_layout(spec)
  map <- spec$process_map

  pinned <- map$fid[map$completed & map$family == "rate"]
  expect_length(pinned, 2L)
  for (fid in pinned) {
    row <- layout[layout$fid == fid, ]
    pin <- spec$completed_rates[[as.character(fid)]]
    expect_identical(nrow(row), 1L)
    expect_true(row$fixed)
    expect_identical(row$index, NA_integer_)
    expect_length(pin$intercept, 2L)
    expect_identical(row$value, as.numeric(pin$intercept)[[1]])
  }
  # The free slots are numbered contiguously, skipping the pinned rows.
  expect_identical(layout$index[!layout$fixed], seq_len(sum(!layout$fixed)))
})

test_that("a statistic with no variation on any path runs under every routine", {
  skip_on_cran()
  # calls inertia is identically zero, so every pooled sequence has a singular
  # information matrix; none of them is degenerate.
  for (routine in c("random", "sim", "mcmc")) {
    control <- set_algorithm_em(
      n_sequences = 6L,
      max_iterations = 3L,
      seed = 7L,
      augmenter = set_augmenter_options(routine = routine)
    )
    expect_no_error(
      suppressWarnings(estimate_dynes(
        recipe_state_spec(),
        control,
        control_prep = set_preprocessing(start_time = 0)
      ))
    )
  }
})

# A count panel with exogenous effects only has an analytic observed-data
# likelihood: dyad counts Poisson in lambda_ij * dt, times uniform order
# statistics given the counts. The augmenter, the weights and the M-step must
# together reproduce the Poisson-regression MLE, a target none of them
# produced. Every path has the same likelihood, so the Monte-Carlo error is
# zero up to rounding and the bound is the optimizer's: 0.01 standard errors,
# a tenth of the at_optimum threshold.
test_that("the default fit reproduces the closed-form count-panel MLE", {
  skip_on_cran()
  oracle <- suppressMessages(oracle_count_panel())

  fit <- suppressMessages(estimate_dynes(
    oracle$spec,
    set_algorithm_em(
      n_sequences = 20L,
      max_iterations = 30L,
      seed = 1L,
      augmenter = set_augmenter_options(routine = "random")
    ),
    wave_times = oracle$wave_times
  ))

  expect_true(fit$at_optimum)
  labels <- names(oracle$mle$estimate)
  gap <- (coef(fit)[labels] - oracle$mle$estimate) / oracle$mle$se
  expect_lt(max(abs(gap)), 0.01)
  mc_se <- sqrt(diag(fit$mc_covariance))[labels]
  expect_lt(max(mc_se / oracle$mle$se), 1e-6)
})

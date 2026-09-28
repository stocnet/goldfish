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

test_that("estimate_dynes is internal until a real augmenter exists", {
  expect_false("estimate_dynes" %in% getNamespaceExports("goldfish"))
})

test_that("estimate_dynes without an augmenter aborts informatively", {
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
  spec <- dynes_flavored_spec()

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
        list(data = rq_tied_data(), log_proposal = 0)
      })
    }
  ))
  control <- surface_control(
    augmenter = set_augmenter_options(routine = "sim", warm_start = TRUE)
  )

  out <- collect_warnings(estimate_dynes(rq_layout_spec(), control))

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
      data = rq_tied_data(),
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

  expect_snapshot(res <- estimate_dynes(rq_layout_spec(), control))
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
  local_dynes_augmenter(new_multiwave_augmenter())
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

# Diagnostics are a function of the returned parameters, so they are computed
# once, after the Newton loop, rather than at every iteration and read once.

# The diagnostic flags a pass can carry, by the spelling each engine uses.
diagnostics_once_requested <- function(args, flags) {
  any(vapply(flags, function(flag) isTRUE(args[[flag]]), logical(1)))
}

# Counts the passes made with any diagnostic flag set during `code`: the
# compiled engines through their evaluator, the r engine through its pass.
diagnostics_once_count <- function(code) {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  real_engine <- make_engine_evaluator
  real_step <- compute_iteration_step
  local_mocked_bindings(
    make_engine_evaluator = function(...) {
      engine <- real_engine(...)
      evaluate <- engine$evaluate
      engine$evaluate <- function(pars, need_scores, ...) {
        args <- c(list(need_scores = need_scores), list(...))
        flags <- c(
          "need_scores",
          "need_ranks",
          "need_margins",
          "need_total_rate",
          "need_probabilities",
          "need_availability",
          "need_conditional_scores"
        )
        if (diagnostics_once_requested(args, flags)) {
          counter$n <- counter$n + 1L
        }
        evaluate(pars, need_scores, ...)
      }
      engine
    },
    compute_iteration_step = function(...) {
      args <- list(...)
      flags <- c(
        "return_event_scores",
        "returnEventProbabilities",
        "return_ranks",
        "return_margins",
        "return_total_rate",
        "return_availability",
        "return_conditional_scores"
      )
      if (diagnostics_once_requested(args, flags)) {
        counter$n <- counter$n + 1L
      }
      real_step(...)
    }
  )
  fit <- code
  list(fit = fit, n = counter$n)
}

diagnostics_once_fit <- function(entry, backend, data_list, ...) {
  control <- c(list(backend = backend), entry$estimation_args, list(...))
  args <- list(
    x = entry$formula,
    data = data_list[[entry$dataset]],
    control_algo = do.call(set_algorithm_newton, control),
    progress = FALSE,
    verbose = FALSE
  )
  if (identical(entry$model, "DyNAM")) {
    args$sub_model <- entry$sub_model
    suppressWarnings(suppressMessages(do.call(estimate_dynam, args)))
  } else {
    if (!is.null(entry$sub_model)) {
      args$sub_model <- entry$sub_model
    }
    suppressWarnings(suppressMessages(do.call(estimate_rem, args)))
  }
}

test_that("a fit computes its diagnostics in exactly one pass", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  entry <- baselines_model_grid()$se_rem
  data_list <- likelihood_pin_data()
  for (backend in c("cpp", "r", "gather")) {
    counted <- diagnostics_once_count(
      diagnostics_once_fit(entry, backend, data_list)
    )
    expect_gt(counted$fit$n_iterations, 1)
    expect_equal(counted$n, 1L, info = backend)
  }
})

# Every stored diagnostic equals what one pass at the returned parameters
# gives, on each family, on both Newton loops (the compiled one, which the
# gather backend shares, and the r one), and on a fit stopped at the
# iteration limit.
test_that("stored diagnostics are those of one pass at the returned vector", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  grid <- baselines_model_grid()
  data_list <- likelihood_pin_data()
  diagnostics <- c(
    "loglik",
    "scores",
    "ranks",
    "margins",
    "availability",
    "conditional_scores",
    "probabilities"
  )
  fields <- c(
    "interval_log_lik",
    "event_scores",
    "observed_rank",
    "margins",
    "availability",
    "total_rate",
    "conditional_logl",
    "conditional_scores",
    "event_probabilities"
  )
  for (name in grep("^se_", names(grid), value = TRUE)) {
    entry <- grid[[name]]
    for (backend in c("cpp", "r")) {
      for (limit in c(20L, 2L)) {
        fit <- diagnostics_once_fit(
          entry,
          backend,
          data_list,
          diagnostics = diagnostics,
          max_iterations = limit
        )
        label <- paste(name, backend, limit)
        reference <- diagnostics_once_fit(
          entry,
          backend,
          data_list,
          diagnostics = diagnostics,
          initial_parameters = fit$parameters,
          max_iterations = 0L
        )
        for (field in fields) {
          expect_equal(
            fit[[field]],
            reference[[field]],
            tolerance = 1e-10,
            info = paste(label, field)
          )
        }
      }
    }
  }
})

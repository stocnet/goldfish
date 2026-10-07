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

# Runs a fit whose `at_call`-th Newton pass without diagnostics returns a
# non-finite log-likelihood, as an overflowing step does: the loop rejects
# that step and, when it is the last one, returns the parameters accepted
# before it. Counts the passes without diagnostics, so `at_call = -1L`
# leaves the fit untouched and reports how many there were.
diagnostics_once_nonfinite_fit <- function(
  entry,
  backend,
  data_list,
  at_call,
  ...
) {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  poison <- function(res, flagged) {
    if (flagged) {
      return(res)
    }
    counter$n <- counter$n + 1L
    if (identical(counter$n, at_call)) {
      res$logLikelihood <- NaN
    }
    res
  }
  real_engine <- make_engine_evaluator
  real_step <- compute_iteration_step
  local_mocked_bindings(
    make_engine_evaluator = function(...) {
      engine <- real_engine(...)
      evaluate <- engine$evaluate
      engine$evaluate <- function(pars, need_scores, ...) {
        flags <- c(list(need_scores), list(...))
        flagged <- any(vapply(flags, isTRUE, logical(1)))
        poison(evaluate(pars, need_scores, ...), flagged)
      }
      engine
    },
    compute_iteration_step = function(...) {
      flagged <- diagnostics_once_requested(
        list(...),
        c(
          "return_event_scores",
          "returnEventProbabilities",
          "return_ranks",
          "return_margins",
          "return_total_rate",
          "return_availability",
          "return_conditional_scores"
        )
      )
      poison(real_step(...), flagged)
    }
  )
  list(
    fit = diagnostics_once_fit(entry, backend, data_list, ...),
    n = counter$n
  )
}

# The two exits that stop short of convergence: the iteration limit, and a
# last step rejected as non-finite. Either way the diagnostics are one pass
# at the parameters the fit returns, and requesting them changes neither
# those parameters nor the convergence status.
test_that("a fit stopped short of convergence keeps its status", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  entry <- baselines_model_grid()$se_rem
  data_list <- likelihood_pin_data()
  diagnostics <- c("loglik", "scores", "ranks", "margins", "availability")
  fields <- c(
    "interval_log_lik",
    "event_scores",
    "observed_rank",
    "margins",
    "availability",
    "total_rate",
    "conditional_logl"
  )
  status <- function(fit) {
    list(fit$parameters, fit$convergence, fit$n_iterations)
  }
  for (backend in c("cpp", "r")) {
    passes <- diagnostics_once_nonfinite_fit(
      entry,
      backend,
      data_list,
      at_call = -1L,
      max_iterations = 3L
    )$n
    for (at_call in c(-1L, passes)) {
      label <- paste(backend, if (at_call < 0L) "limit" else "non-finite")
      fit <- diagnostics_once_nonfinite_fit(
        entry,
        backend,
        data_list,
        at_call = at_call,
        max_iterations = 3L,
        diagnostics = diagnostics
      )$fit
      plain <- diagnostics_once_nonfinite_fit(
        entry,
        backend,
        data_list,
        at_call = at_call,
        max_iterations = 3L
      )$fit
      expect_false(fit$convergence$is_converged, label = label)
      expect_identical(status(fit), status(plain), label = label)
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
    # The rejected step is the last one: the fit returns what the
    # iteration before it accepted.
    rejected <- diagnostics_once_nonfinite_fit(
      entry,
      backend,
      data_list,
      at_call = passes,
      max_iterations = 3L
    )$fit
    shorter <- diagnostics_once_fit(
      entry,
      backend,
      data_list,
      max_iterations = 2L
    )
    expect_equal(rejected$parameters, shorter$parameters, tolerance = 1e-12)
  }
})

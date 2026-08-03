# Per-event score matrix: set_algorithm_newton(diagnostics = "scores") and the
# `event_scores` result component. The per-event score is the same
# observed-minus-expected statistic the estimators accumulate into the aggregate
# derivative each event; here we assert the two engines expose it consistently.

# Evaluate a baseline spec at a fixed parameter vector on a given engine with the
# per-event score matrix on. `max_iterations = 0` pins the evaluation to
# `params`, so the two engines can be compared at *identical* coefficients: their
# converged estimates agree only to the cross-engine ~1e-6, far coarser than the
# 1e-10 the per-event decomposition itself holds to.
event_scores_eval <- function(spec, backend, data_list, params) {
  opt <- set_algorithm_newton(
    backend = backend,
    initial_parameters = params,
    max_iterations = 0,
    diagnostics = c("loglik", "scores")
  )
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_algo = opt,
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    suppressWarnings(do.call(estimate_dynam, args))
  } else {
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    suppressWarnings(do.call(estimate_rem, args))
  }
}

# The six sub-models, all defined on Social Evolution so a single dataset covers
# choice, coordination, ordinal/timed rate, and ordinal/timed REM.
event_scores_specs <- function() {
  grid <- baselines_model_grid()
  grid[c(
    "se_dynam_rate",
    "se_dynam_rate_ordered",
    "se_dynam_choice",
    "se_dynam_choice_coord",
    "se_rem",
    "se_rem_ordered"
  )]
}

test_that("column sums of event_scores equal the aggregate score (cpp)", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  for (nm in names(event_scores_specs())) {
    spec <- event_scores_specs()[[nm]]
    beta <- suppressWarnings(
      baselines_fit(spec, "cpp", data_list)
    )$parameters
    # Evaluate away from the optimum (all-zero start) so the aggregate score is
    # far from zero and the column-sum identity is a strong check, not 0 == 0.
    #
    # This identity used to hold by construction and no longer does, which is
    # what makes it worth asserting. The event-loop kernels once stored each row
    # as the before/after difference of the running derivative, so summing the
    # rows telescoped back to that same total whatever the arithmetic did -- a
    # tautology that would have passed even if the reduction were wrong. The
    # rows now come from the shared `event_score_row()`, computed from the
    # weights and the statistics independently of the accumulated total, so
    # agreement is a real algebraic check on two separate computations. That is
    # also why the residual grew from ~1e-16 to ~1e-12: it is now measuring
    # something.
    fit <- event_scores_eval(spec, "cpp", data_list, rep(0, length(beta)))
    expect_false(is.null(fit$event_scores), info = nm)
    expect_equal(nrow(fit$event_scores), fit$n_events, info = nm)
    expect_equal(
      unname(colSums(fit$event_scores)),
      unname(fit$final_score),
      tolerance = 1e-10,
      info = nm
    )
  }
})

test_that("event_scores agree across engines and vanish at the optimum", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  for (nm in names(event_scores_specs())) {
    spec <- event_scores_specs()[[nm]]
    beta <- suppressWarnings(
      baselines_fit(spec, "cpp", data_list)
    )$parameters
    fit_c <- event_scores_eval(spec, "cpp", data_list, beta)
    es_c <- fit_c$event_scores
    es_r <- event_scores_eval(spec, "r", data_list, beta)$event_scores
    expect_equal(unname(es_c), unname(es_r), tolerance = 1e-10, info = nm)
    # At the (near-)MLE the aggregate score is at the convergence floor. Compare
    # on the scale-invariant criterion the optimiser itself stops on
    # (max|score| / max(1, |logLik|) <= score_tol), not raw magnitude, since the
    # timed sub-models carry a large |logLik|.
    expect_lt(
      max(abs(colSums(es_c))) / max(1, abs(fit_c$log_likelihood)),
      1e-3,
      label = nm
    )
  }
})

test_that("event_scores columns are named by effect", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_choice
  fit <- event_scores_eval(spec, "cpp", data_list, rep(0, 3))
  expect_identical(colnames(fit$event_scores), rownames(fit$names))
})

test_that("event_scores follows the scores diagnostic (default on)", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_choice
  for (backend in c("cpp", "r")) {
    # scores are in the default diagnostics, so the matrix is present by default
    fit_default <- suppressWarnings(baselines_fit(spec, backend, data_list))
    expect_false(is.null(fit_default$event_scores), info = backend)
    # dropping "scores" from diagnostics drops the matrix
    fit_no_scores <- suppressWarnings(estimate_dynam(
      spec$formula,
      data = data_list$social_evolution,
      sub_model = spec$sub_model,
      control_algo = set_algorithm_newton(
        backend = backend,
        diagnostics = "loglik"
      ),
      progress = FALSE
    ))
    expect_null(fit_no_scores$event_scores, info = backend)
  }
})

test_that("the gather backend stores scores, however they were requested", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_choice
  fit_scores <- function(...) {
    suppressWarnings(estimate_dynam(
      spec$formula,
      data = data_list$social_evolution,
      sub_model = spec$sub_model,
      control_algo = set_algorithm_newton(backend = "gather", ...),
      progress = FALSE
    ))
  }
  # An explicit request used to abort and a default-sourced one used to be
  # dropped without a word, so the same backend both refused and quietly
  # ignored the same primitive depending only on how it was asked for. The
  # gather kernels compute per-event scores, so both routes store them.
  for (fit in list(
    fit_scores(diagnostics = c("loglik", "scores")),
    fit_scores()
  )) {
    expect_equal(fit$backend, "gather")
    expect_false(is.null(fit$event_scores))
  }
})

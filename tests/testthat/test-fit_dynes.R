# The goldfishDynesFit result class: the covariance pass at the returned
# estimate, the flat coef()/vcov() over the free coefficients, the per-process
# coef_layout() with completed fids as fixed rows, and the verdicts the
# fit-class-hierarchy capability records for the class (override or refuse on
# every generic, inherit on none). The loop runs over the analytic evaluator
# (helper-abem_loop.R): each pooled sequence's data mu_i scores as
# -sum((theta - mu_i)^2) / 2, so the score is mu_i - theta and every Fisher
# information the identity, and both covariances are closed-form.

# accept 0 -> 0.5, then stop on a proposal that does not move.
local_accept_then_stop <- function(env = parent.frame()) {
  proposals <- list(function(theta) theta + 0.5, function(theta) theta + 1e-6)
  pass <- 0L
  local_scripted_optimizer(
    function(theta) {
      pass <<- pass + 1L
      proposals[[min(pass, 2L)]](theta)
    },
    env = env
  )
}

fit_dynes_control <- function(...) {
  args <- list(
    n_sequences = 4L,
    seed = 42L,
    augmenter = set_augmenter_options(routine = "random")
  )
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(set_algorithm_em, args)
}

fit_dynes_toy <- function(...) {
  suppressWarnings(estimate_dynes(
    dynes_pinned_raw_spec(),
    fit_dynes_control(...)
  ))
}

# --- Class and flat coefficients ------------------------------------------

test_that("estimate_dynes returns a goldfishDynesFit under the fit parent", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_s3_class(fit, c("goldfishDynesFit", "goldfishBaseFit"), exact = TRUE)
  expect_true(fit$converged)
})

test_that("coef() is flat over the free coefficients, by composite label", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_identical(
    names(coef(fit)),
    c(
      "friendship › choice: inertia/friendship",
      "calls › rate: Intercept",
      "calls › rate: indeg/friendship"
    )
  )
  expect_equal(unname(coef(fit)), fit$parameters, ignore_attr = TRUE)
})

# --- Covariance pass ------------------------------------------------------

test_that("vcov() is populated when max_iterations ends the run", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.3)
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy(max_iterations = 2L)

  expect_false(fit$converged)
  v <- vcov(fit)
  expect_false(is.null(v))
  expect_identical(dim(v), c(3L, 3L))
  expect_identical(dimnames(v), list(names(coef(fit)), names(coef(fit))))
  expect_true(all(is.finite(v)))
  expect_true(all(is.finite(fit$mc_covariance)))
})

test_that("the standard and MC covariances match the hand-computed forms", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  theta <- unname(coef(fit))
  w <- fit$weights
  mu <- vapply(fit$pool$sequences, function(s) s$data, numeric(1))
  scores <- t(vapply(mu, function(m) m - theta, numeric(length(theta))))
  # The Fisher information is the identity per sequence and the weights sum
  # to one, so I = diag(3) and both sandwiches reduce to their middle terms.
  sandwich <- crossprod(scores * sqrt(w))
  centered <- sweep(scores, 2L, colSums(scores * w))
  mc <- crossprod(centered * w)

  expect_equal(unname(vcov(fit)), sandwich)
  expect_equal(unname(fit$mc_covariance), mc)
  expect_identical(dimnames(fit$mc_covariance), dimnames(vcov(fit)))
})

# --- Completed fids -------------------------------------------------------

test_that("a completed pinned rate is a fixed layout row, not a coefficient", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  layout <- coef_layout(fit)

  pinned_fid <- names(fit$spec$completed_rates)
  expect_length(pinned_fid, 1L)
  row <- layout[layout$fid == as.integer(pinned_fid), ]
  expect_identical(nrow(row), 1L)
  expect_true(row$fixed)
  expect_equal(row$value, fit$spec$completed_rates[[pinned_fid]]$intercept)
  expect_true(is.na(row$se))
  expect_true(is.na(row$mc_se))

  expect_false(any(startsWith(names(coef(fit)), row$process)))
  expect_false(any(startsWith(rownames(vcov(fit)), row$process)))

  free <- layout[!layout$fixed, ]
  expect_identical(paste0(free$process, ": ", free$name), names(coef(fit)))
  expect_equal(free$value, unname(coef(fit)))
  expect_equal(free$se, unname(sqrt(diag(vcov(fit)))))
  expect_equal(free$mc_se, unname(sqrt(diag(fit$mc_covariance))))
})

# --- Recorded verdicts ----------------------------------------------------

test_that("logLik, AIC and BIC refuse on a Monte-Carlo fit", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  local_abem_cli_context()

  fit <- fit_dynes_toy()

  expect_error(logLik(fit), class = "goldfish_generic_refused")
  expect_error(stats::AIC(fit), class = "goldfish_generic_refused")
  expect_error(stats::BIC(fit), class = "goldfish_generic_refused")
  expect_snapshot(logLik(fit), error = TRUE)
})

test_that("the estimation-internals generics refuse, naming the class", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  refused <- list(
    model_terms = model_terms,
    augment = generics::augment,
    fitted = stats::fitted,
    predict = stats::predict,
    residuals = stats::residuals,
    evaluate_model = evaluate_model,
    test_gof = test_gof,
    test_parameter = test_parameter,
    test_time = test_time,
    diagnose_outliers = diagnose_outliers,
    diagnose_changepoints = diagnose_changepoints,
    diagnose_onset = diagnose_onset,
    margin_table = margin_table
  )

  for (generic in names(refused)) {
    err <- expect_error(
      refused[[generic]](fit),
      class = "goldfish_generic_refused"
    )
    expect_match(conditionMessage(err), "goldfishDynesFit", info = generic)
  }
})

test_that("times_of answers from the held specification", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_identical(times_of(fit), times_of(dynes_pinned_raw_spec()))
})

test_that("simulate() runs the held specification at the estimates", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  params <- dynes_fit_parameters(fit)

  expect_s3_class(params, "goldfishParams")
  layout <- coef_layout(params)
  free <- layout[!layout$fixed, ]
  expect_equal(
    free$value,
    unname(coef(fit)[paste0(free$process, ": ", free$name)])
  )

  sim <- suppressWarnings(simulate(fit, seed = 1, n_events = 3L))
  expected <- suppressWarnings(simulate(
    dynes_pinned_raw_spec(),
    seed = 1,
    coef = params,
    n_events = 3L
  ))
  expect_identical(sim$events, expected$events)
})

# --- Display --------------------------------------------------------------

test_that("tidy() carries the standard and MC error per coefficient", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  tidied <- tidy(fit)

  expect_s3_class(tidied, "tbl_df")
  expect_identical(tidied$term, names(coef(fit)))
  expect_named(
    tidied,
    c(
      "process",
      "term",
      "estimate",
      "std.error",
      "mc.error",
      "statistic",
      "p.value"
    )
  )
  expect_equal(tidied$std.error, unname(sqrt(diag(vcov(fit)))))
  expect_equal(tidied$mc.error, unname(sqrt(diag(fit$mc_covariance))))
})

test_that("glance() reports the run and no information criterion", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  glanced <- glance(fit)

  expect_named(
    glanced,
    c("converged", "iterations", "n_sequences", "df")
  )
  expect_true(glanced$converged)
  expect_identical(glanced$df, 3L)
  expect_identical(glanced$n_sequences, length(fit$pool$sequences))
})

test_that("print and summary group per process and show both errors", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.3)
  local_dynes_augmenter(new_keyed_stub_augmenter())
  local_abem_cli_context()

  fit <- fit_dynes_toy(max_iterations = 2L)
  # The analytic fixture's numbers carry no meaning; pin them so the snapshot
  # records the layout rather than floating-point noise.
  fit$parameters[] <- c(0.6, -1.25, 0.3)
  fit$covariance[] <- diag(c(0.04, 0.09, 0.01))
  fit$mc_covariance[] <- diag(c(0.0004, 0.0009, 0.0001))

  expect_s3_class(summary(fit), "goldfishSummDynesFit")
  expect_snapshot({
    print(fit)
    summary(fit)
  })
})

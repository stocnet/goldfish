# `parallel` is Suggests, not Imports: these baseline helpers are its only
# consumer, so requiring it would make every user install it to fit a model.
# That makes the serial fallback a real code path rather than a formality, and
# these tests force it instead of trusting a reading of the branch.

test_that("baselines_cores falls back to serial without the parallel package", {
  withr::local_envvar(TESTTHAT_CPUS = "")
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_identical(baselines_cores(8L), 1L)
})

test_that("baselines_cores honors TESTTHAT_CPUS and caps at the job count", {
  withr::local_envvar(TESTTHAT_CPUS = "4")
  expect_identical(baselines_cores(8L), 4L)
  # More workers than cells would fork idle children.
  expect_identical(baselines_cores(2L), 2L)
})

test_that("baselines_cores is serial for an unusable TESTTHAT_CPUS", {
  withr::local_envvar(TESTTHAT_CPUS = "not-a-number")
  expect_identical(baselines_cores(8L), 1L)
})

# baselines_build() is the shared loop the `_baselines/` generators drive. Those
# scripts are the documented provenance of the regression floor and nothing runs
# them, which is how both came to reference `baselines_engines` long after that
# rename. These tests exercise the loop itself -- with an injected fitter, so
# the fresh-fit path is covered without a full grid refit -- so a rename fails
# here instead of rotting in a script.

# Classed `result.goldfish` so baselines_build()'s coef() / logLik() calls go
# through the package's own methods, which is what the real loop exercises.
fake_fit <- function(log_lik) {
  structure(
    list(
      convergence = list(is_converged = TRUE),
      parameters = c(a = 1, b = 2),
      log_likelihood = log_lik,
      n_events = 10L,
      n_params = 2L,
      names = matrix(
        c("net", "net", "FALSE", "FALSE"),
        ncol = 2,
        dimnames = list(c("a", "b"), c("Object", "fixed"))
      )
    ),
    class = "result.goldfish"
  )
}

test_that("baselines_build fits each cell and records coef plus logLik", {
  grid <- list(m1 = list(dataset = "d", model = "DyNAM", sub_model = "choice"))
  built <- expect_output(baselines_build(
    grid = grid,
    get_data = function(dataset) list(),
    backends = c("r", "cpp"),
    fit_fn = function(spec, backend, data) {
      fake_fit(if (backend == "r") -1 else -2)
    }
  ))

  expect_named(built, "m1")
  expect_named(built$m1, c("r", "cpp"))
  expect_equal(built$m1$r$logLik, -1)
  expect_equal(built$m1$cpp$logLik, -2)
  expect_equal(built$m1$r$coef, c(a = 1, b = 2))
})

test_that("baselines_build copies a carried entry verbatim, never refitting", {
  # v2's policy: r and cpp come from v1 bit-identically, gather is fitted. A
  # carried entry must be the same object, not a refit that happens to be close.
  frozen <- list(coef = c(a = 99), logLik = -123.456)
  fitted_backends <- character()
  built <- expect_output(baselines_build(
    grid = list(m1 = list(dataset = "d")),
    get_data = function(dataset) list(),
    backends = c("r", "cpp", "gather"),
    carry = function(model_name, backend) {
      if (backend %in% c("r", "cpp")) frozen else NULL
    },
    fit_fn = function(spec, backend, data) {
      fitted_backends <<- c(fitted_backends, backend)
      fake_fit(-7)
    }
  ))

  expect_identical(fitted_backends, "gather")
  expect_identical(built$m1$r, frozen)
  expect_identical(built$m1$cpp, frozen)
  expect_equal(built$m1$gather$logLik, -7)
})

test_that("baselines_build stores under the key policy the generator passes", {
  # global_v1 records the legacy engine tokens it was written with, so a
  # regeneration reproduces its key scheme rather than silently rewriting it.
  built <- expect_output(baselines_build(
    grid = list(m1 = list(dataset = "d")),
    get_data = function(dataset) list(),
    backends = baselines_backends_global,
    key = function(backend) BACKEND_ENGINE_TOKENS[[backend]],
    fit_fn = function(spec, backend, data) fake_fit(-1)
  ))
  expect_named(built$m1, c("default", "default_c"))
})

test_that("baselines_build refuses a cell that did not converge", {
  # stopifnot() fires before the progress line, so there is no output to expect.
  expect_error(baselines_build(
    grid = list(m1 = list(dataset = "d")),
    get_data = function(dataset) list(),
    backends = "r",
    fit_fn = function(spec, backend, data) {
      list(convergence = list(is_converged = FALSE))
    }
  ))
})

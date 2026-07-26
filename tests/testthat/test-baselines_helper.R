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

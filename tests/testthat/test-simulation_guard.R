# The guard object: when a free-running replicate gives up. Construction
# validates every field, so a mistyped guard fails before a run starts; how
# the driver reads the guard is checked in test-simulate.R.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("the default guard caps the count and stops a stalled clock", {
  guard <- set_simulation_guard()

  expect_s3_class(guard, "goldfishSimGuard")
  expect_identical(
    unclass(guard),
    list(
      max_events = NULL,
      rate_multiple = Inf,
      wait_collapse = Inf,
      wait_window = 50L,
      clock_resolution = 0
    )
  )
})

test_that("a guard keeps the values it is given", {
  guard <- set_simulation_guard(
    max_events = 200,
    rate_multiple = 1e3,
    wait_collapse = 10,
    wait_window = 20,
    clock_resolution = NULL
  )

  expect_identical(guard$max_events, 200L)
  expect_identical(guard$rate_multiple, 1e3)
  expect_identical(guard$wait_collapse, 10)
  expect_identical(guard$wait_window, 20L)
  expect_null(guard$clock_resolution)
})

test_that("a guard refuses values it cannot use", {
  local_cli_context()

  expect_snapshot(error = TRUE, set_simulation_guard(max_events = 0))
  expect_snapshot(error = TRUE, set_simulation_guard(max_events = 2.5))
  expect_snapshot(error = TRUE, set_simulation_guard(max_events = Inf))
  expect_snapshot(error = TRUE, set_simulation_guard(rate_multiple = 0.5))
  expect_snapshot(error = TRUE, set_simulation_guard(wait_collapse = NA))
  expect_snapshot(error = TRUE, set_simulation_guard(wait_window = 0))
  expect_snapshot(error = TRUE, set_simulation_guard(clock_resolution = -1))
  expect_snapshot(error = TRUE, set_simulation_guard(clock_resolution = "0"))
  expect_error(
    set_simulation_guard(rate_multiple = c(10, 20)),
    class = "goldfish_sim_bad_guard"
  )
})

test_that("a guard prints which stops are on", {
  local_cli_context()

  expect_snapshot(print(set_simulation_guard()))
  expect_snapshot(
    print(set_simulation_guard(
      max_events = 200,
      rate_multiple = 1e3,
      wait_collapse = 10,
      wait_window = 20,
      clock_resolution = NULL
    ))
  )
})

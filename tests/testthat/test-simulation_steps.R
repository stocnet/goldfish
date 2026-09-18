# The simulation plug points: the constructors that validate a caller's step
# closures, and the accessor surface those closures read the handle through.
# The construction-time checks are the contract -- a step with the wrong arity
# or the wrong return shape must fail where it is written, not midway through a
# replicate where the traceback would name the driver.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

ok_clock <- function(rates, t, handle) {
  list(wait = 1, fid = 1L, kind = "event")
}
ok_mark <- function(fid, evaluation, handle) {
  list(layer = "toy", sender = 1L, receiver = 1L, increment = 1)
}
ok_accept <- function(event, handle) TRUE
ok_evaluate <- function(stats, theta, risk, meta) as.numeric(stats %*% theta)

test_that("set_parameter_provider needs an at() and defaults init()", {
  local_cli_context()

  expect_error(set_parameter_provider(), class = "goldfish_sim_bad_step")

  provider <- set_parameter_provider(
    at = function(k, t, handle, latent) list(parameters = list(`1` = 1))
  )
  expect_s3_class(provider, "goldfishParamProvider")
  expect_null(provider$init(1L, NULL))
})

test_that("a step with too few arguments is refused", {
  local_cli_context()

  expect_error(
    set_simulation_steps(clock = function(rates) 1),
    class = "goldfish_sim_bad_step"
  )
  expect_error(
    set_simulation_steps(accept = function(event) TRUE),
    class = "goldfish_sim_bad_step"
  )
  expect_error(
    set_parameter_provider(at = function(k, t) NULL),
    class = "goldfish_sim_bad_step"
  )
  expect_error(
    set_simulation_steps(clock = "not a function"),
    class = "goldfish_sim_bad_step"
  )
  # `...` absorbs whatever the driver passes, so it satisfies any arity.
  expect_s3_class(
    set_simulation_steps(accept = function(...) TRUE),
    "goldfishSimSteps"
  )
})

test_that("a step returning the wrong shape is refused at construction", {
  local_cli_context()

  expect_error(
    set_simulation_steps(clock = function(rates, t, handle) 1),
    class = "goldfish_sim_bad_step"
  )
  expect_error(
    set_simulation_steps(
      mark = function(fid, evaluation, handle) list(sender = 1L)
    ),
    class = "goldfish_sim_bad_step"
  )
  expect_error(
    set_simulation_steps(accept = function(event, handle) "yes"),
    class = "goldfish_sim_bad_step"
  )
  expect_error(
    set_simulation_steps(
      evaluate = function(stats, theta, risk, meta) c(1, 2, 3)
    ),
    class = "goldfish_sim_bad_step"
  )
})

test_that("a step whose dry call errors is refused, naming the step", {
  local_cli_context()

  expect_error(
    set_simulation_steps(
      clock = function(rates, t, handle) stop("boom")
    ),
    class = "goldfish_sim_bad_step"
  )
  expect_snapshot(
    error = TRUE,
    set_simulation_steps(clock = function(rates, t, handle) stop("boom"))
  )
})

test_that("an evaluate step written for a matrix theta is accepted", {
  # The dry call tries a vector first and a one-row matrix second: refusing a
  # matrix-only step would reject the very variant the plug point exists for.
  matrix_only <- function(stats, theta, risk, meta) {
    rowSums(stats * theta[rep(seq_len(nrow(theta)), each = 1L), , drop = FALSE])
  }

  expect_s3_class(
    set_simulation_steps(evaluate = matrix_only),
    "goldfishSimSteps"
  )
})

test_that("set_simulation_steps keeps what it was given", {
  steps <- set_simulation_steps(
    evaluate = ok_evaluate,
    clock = ok_clock,
    mark = ok_mark,
    accept = ok_accept
  )

  expect_s3_class(steps, "goldfishSimSteps")
  expect_identical(steps$clock, ok_clock)
  expect_identical(steps$mark, ok_mark)
  expect_identical(steps$accept, ok_accept)
  # Unsupplied slots stay NULL: the driver reads that as "use goldfish's own".
  expect_null(set_simulation_steps()$clock)
})

test_that("the parameters slot must be a provider", {
  local_cli_context()

  expect_error(
    set_simulation_steps(parameters = c(1, 2)),
    class = "goldfish_sim_bad_step"
  )
  provider <- set_parameter_provider(
    at = function(k, t, handle, latent) list(parameters = list(`1` = 1))
  )
  expect_identical(
    set_simulation_steps(parameters = provider)$parameters,
    provider
  )
})

test_that("the handle accessors read a handle and refuse anything else", {
  local_cli_context()
  handle <- goldfish:::toy_walk_handle()

  expect_identical(n_actors(handle), 1L)
  expect_identical(n_actors(handle, side = 2L), 1L)
  expect_identical(current_time(handle), 0)
  expect_s3_class(process_map(handle), "data.frame")
  expect_identical(unname(regime_of(handle)), "modeled")
  expect_identical(regime_of(handle, fid = 1L), "modeled")

  expect_error(n_actors(list()), class = "goldfish_sim_bad_handle")
  expect_error(current_time(NULL), class = "goldfish_sim_bad_handle")
  expect_error(process_map("x"), class = "goldfish_sim_bad_handle")
  expect_error(regime_of(1), class = "goldfish_sim_bad_handle")
})

test_that("the accessors report a real walk's actors and clock", {
  js <- sim_two_process()
  handle <- walk_open(js)

  expect_identical(n_actors(handle), 6L)
  expect_identical(n_actors(handle, side = 2L), 6L)
  expect_equal(current_time(handle), min(handle$schedule$time))
  expect_equal(nrow(process_map(handle)), 4)
  expect_true(all(regime_of(handle) == "modeled"))
})

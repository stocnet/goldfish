# The restart-ceiling helper the sim and random augmenters share: a bounded
# redraw loop, its acceptance-rate estimator, and the abort when the ceiling
# is reached.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A proposal that hands out 1, 2, 3, ... and counts its calls.
counting_proposal <- function() {
  calls <- 0L
  list(
    propose = function() {
      calls <<- calls + 1L
      calls
    },
    calls = function() calls
  )
}

test_that("the redraw loop returns the first accepted draw", {
  proposal <- counting_proposal()
  result <- redraw_until_accepted(
    proposal$propose,
    accept = function(value) value == 3L,
    constraint = "friendship: ~tie(friendship)"
  )
  expect_equal(result$value, 3L)
  expect_equal(result$attempts, 3L)
  expect_equal(proposal$calls(), 3L)
})

test_that("an accepted first draw is not redrawn", {
  proposal <- counting_proposal()
  result <- redraw_until_accepted(
    proposal$propose,
    accept = function(value) TRUE,
    constraint = "friendship: ~tie(friendship)"
  )
  expect_equal(result$value, 1L)
  expect_equal(result$attempts, 1L)
  expect_equal(proposal$calls(), 1L)
})

test_that("the acceptance rate is successes over attempts", {
  # Draws are legal on the pattern F F T F T: two redraw runs take three and
  # two attempts, so two successes in five attempts.
  pattern <- c(FALSE, FALSE, TRUE, FALSE, TRUE)
  proposal <- counting_proposal()
  accept <- function(value) pattern[[value]]
  runs <- replicate(
    2L,
    redraw_until_accepted(proposal$propose, accept, "calls: ~!tie(friendship)"),
    simplify = FALSE
  )
  attempts <- vapply(runs, `[[`, integer(1), "attempts")
  expect_equal(attempts, c(3L, 2L))

  rate <- acceptance_rate(length(runs), sum(attempts), theta_ref = c(0.5, -1))
  expect_equal(rate$rate, 2 / 5)
  expect_equal(rate$successes, 2L)
  expect_equal(rate$attempts, 5L)
  expect_equal(rate$theta_ref, c(0.5, -1))
})

test_that("a first-attempt success has an acceptance rate of one", {
  expect_equal(acceptance_rate(1L, 1L)$rate, 1)
  expect_null(acceptance_rate(1L, 1L)$theta_ref)
})

test_that("the ceiling is one positive integer", {
  expect_type(restart_ceiling(), "integer")
  expect_length(restart_ceiling(), 1L)
  expect_gt(restart_ceiling(), 1L)
})

test_that("reaching the ceiling aborts naming the coupled constraint", {
  local_cli_context()
  proposal <- counting_proposal()
  expect_snapshot(
    redraw_until_accepted(
      proposal$propose,
      accept = function(value) FALSE,
      constraint = "friendship: ~tie(friendship) | outdeg(friendship) < 1",
      ceiling = 5L
    ),
    error = TRUE
  )
  expect_equal(proposal$calls(), 5L)
  expect_error(
    redraw_until_accepted(
      proposal$propose,
      accept = function(value) FALSE,
      constraint = "friendship: ~tie(friendship)",
      ceiling = 5L
    ),
    class = "goldfish_dynes_restart_ceiling"
  )
})

# The availability effect is stepped on every row an exact-time rate
# stores. Most rows change nothing it holds: no composition change lands on
# them and no mask flips. Such a row only adds its exposure, so the step of
# the effect's variant is not entered for it. The guards count those calls
# rather than time them, so they separate from noise at any size.

count_presence_steps <- function(code) {
  calls <- 0L
  original <- availability_step_presence
  local_mocked_bindings(
    availability_step_presence = function(...) {
      calls <<- calls + 1L
      original(...)
    }
  )
  rows <- force(code)
  list(calls = calls, rows = rows)
}

test_that("a rate row with no composition change skips the presence step", {
  data <- constraint_cost_data()
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  ))

  counted <- count_presence_steps({
    pre <- suppressWarnings(suppressMessages(preprocess_joint(spec)))
    length(pre[[1L]]$event_time)
  })

  expect_gt(counted$rows, 1000L)
  expect_identical(counted$calls, 0L)
})

test_that("a REM row steps presence only where a composition change lands", {
  # Three receiver composition changes over 100 events: at most three rows
  # have anything to apply, and at least one does.
  fx <- suppressWarnings(suppressMessages(availability_vector_rem_data()))

  counted <- count_presence_steps({
    pre <- suppressWarnings(suppressMessages(estimate_rem(
      dep ~ 1 + inertia + recip,
      sub_model = "rate",
      data = fx$data,
      preprocessing_only = TRUE
    )))
    length(pre$event_time)
  })

  expect_gt(counted$rows, 3L)
  expect_gte(counted$calls, 1L)
  expect_lte(counted$calls, 3L)
})

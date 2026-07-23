# A global attribute has a single value, so its imputation pool is empty by
# construction. A missing global value -- initial or in an event stream --
# aborts at schedule construction, before the walk, rather than being imputed to
# a not-a-number or written as an arbitrary zero.

# cli error snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("a missing initial global value aborts, naming the object", {
  local_cli_context()
  state <- list(globals = data.frame(climate = NA_real_))
  registry <- data.frame(
    component = "globals",
    key = "climate",
    stringsAsFactors = FALSE
  )
  empty_schedule <- list(
    n = 0L,
    shape = character(),
    value = list(),
    target = integer(),
    time = numeric()
  )
  expect_snapshot(
    assert_globals_defined(state, registry, empty_schedule),
    error = TRUE
  )
})

test_that("a missing global replace event aborts, naming the object and time", {
  local_cli_context()
  state <- list(globals = data.frame(climate = 0))
  registry <- data.frame(
    component = "globals",
    key = "climate",
    stringsAsFactors = FALSE
  )
  schedule <- list(
    n = 1L,
    shape = "global",
    value = list(NA_real_),
    target = 1L,
    time = 1.5
  )
  expect_snapshot(
    assert_globals_defined(state, registry, schedule),
    error = TRUE
  )
})

test_that("a defined global passes the schedule-construction check", {
  state <- list(globals = data.frame(climate = 0))
  registry <- data.frame(
    component = "globals",
    key = "climate",
    stringsAsFactors = FALSE
  )
  schedule <- list(
    n = 1L,
    shape = "global",
    value = list(1),
    target = 1L,
    time = 1.5
  )
  expect_no_error(assert_globals_defined(state, registry, schedule))
})

test_that("a missing global aborts an end-to-end rate estimation", {
  expect_error(
    suppressWarnings(estimate_dynam(
      calls ~ 1 + global(climate),
      sub_model = "rate",
      data = make_stocnet_fixture_missing_global("init"),
      preprocessing_only = TRUE
    )),
    "missing initial value"
  )
  expect_error(
    suppressWarnings(estimate_dynam(
      calls ~ 1 + global(climate),
      sub_model = "rate",
      data = make_stocnet_fixture_missing_global("event"),
      preprocessing_only = TRUE
    )),
    "missing value at time"
  )
})

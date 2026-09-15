# times_of(): the simulation variant a model supports, read from what its
# processes carry rather than asserted against it.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("times_of reads the variant from what the model carries", {
  data <- sim_fixture_data()
  make <- function(...) {
    make_specification(..., layer = "calls", model = "DyNAM", data = data)
  }
  timed <- make(rate = ~ 1 + indeg, choice = ~ inertia + tie(friendship))
  ordered <- make(
    rate = ~indeg,
    rate_sub_model = "rate_ordered",
    choice = ~ inertia + tie(friendship)
  )
  choice_only <- make(choice = ~ inertia + tie(friendship))
  coordination <- make(
    choice = ~inertia,
    choice_sub_model = "choice_coordination"
  )

  expect_identical(
    times_of(timed),
    structure("generated", reason = "timed rate")
  )
  expect_identical(
    times_of(ordered),
    structure("observed", reason = "ordered rate")
  )
  expect_identical(
    times_of(choice_only),
    structure("observed", reason = "no rate")
  )
  expect_identical(
    times_of(coordination),
    structure("observed", reason = "coordination")
  )
})

test_that("a join carrying any timed rate generates", {
  data <- sim_fixture_data()
  calls <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails <- make_specification(
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )

  js <- make_joint_specification(calls, emails, data = data)

  expect_identical(times_of(js), structure("generated", reason = "timed rate"))
})

test_that("times_of reads a fitted model's resolved sub-model", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  rate_fit <- suppressMessages(estimate_dynam(spec, sub_model = "rate"))
  # A choice model on the six-actor fixture is singular, so the choice fit
  # comes from the packaged data.
  data("social_evolution", package = "goldfish", envir = environment())
  choice_fit <- suppressWarnings(suppressMessages(estimate_dynam(
    calls ~ inertia + recip + trans,
    sub_model = "choice",
    data = social_evolution
  )))

  expect_identical(
    times_of(rate_fit),
    structure("generated", reason = "timed rate")
  )
  expect_identical(
    times_of(choice_fit),
    structure("observed", reason = "no rate")
  )
})

test_that("times_of reads a flavored fit across its processes", {
  fit <- flavored_container_fit()

  expect_identical(times_of(fit), structure("generated", reason = "timed rate"))
})

test_that("times_of refuses what is not a model", {
  local_cli_context()

  expect_snapshot(times_of(data.frame()), error = TRUE)
})

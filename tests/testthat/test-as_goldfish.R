local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("as_goldfish validates and stamps a stocnet-shaped list", {
  d <- as_goldfish(make_stocnet_fixture())
  expect_s3_class(d, "goldfishData")
  expect_identical(class(d)[1], "goldfishData")
  # stamp is structural only: dropping it recovers the input unchanged
  expect_equal(unclass(d), unclass(make_stocnet_fixture()))
})

test_that("as_goldfish fails early on invalid data", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$update <- NULL
  expect_snapshot(as_goldfish(x), error = TRUE)
})

test_that("post-stamp mutation is caught on re-validation", {
  d <- as_goldfish(make_stocnet_fixture())
  # simulate a manynet verb / list assignment mutating a stamped object into
  # invalidity; re-validation (the estimation-time gate) must still abort
  d$ties$time <- as.character(d$ties$time)
  expect_error(validate_goldfish_data(d))
})

test_that("as_goldfish defers legacy environment conversion", {
  local_cli_context()
  expect_snapshot(as_goldfish(new.env()), error = TRUE)
})

test_that("printing a stamped object renders the list shape", {
  local_cli_context()
  d <- as_goldfish(make_stocnet_fixture())
  expect_snapshot(print(d))
})

test_that("the estimation surface rejects a legacy environment", {
  local_cli_context()
  env <- new.env()
  expect_snapshot(estimate_dynam(y ~ 1, data = env), error = TRUE)
  expect_snapshot(estimate_rem(y ~ 1, data = env), error = TRUE)
  expect_snapshot(estimate_dynami(y ~ 1, data = env), error = TRUE)
  expect_snapshot(
    make_specification(rate = list(y ~ 1), model = "DyNAM", data = env),
    error = TRUE
  )
})

# The legacy shape `make_data()` and the DyNAM-i path build: an environment of
# goldfish objects, classed `data.goldfish`. Built here rather than borrowed
# from a package fixture so the test states the shape it is contrasting.
legacy_data_environment <- function() {
  env <- new.env(parent = emptyenv())
  env$calls <- data.frame(time = 1, sender = "a", receiver = "b")
  class(env) <- c("data.goldfish", "environment")
  env
}

test_that("the stamp and the legacy environment are distinct classes", {
  # The one place in this rename where a mechanical replacement would have been
  # actively wrong: `data.goldfish` named two different objects, and only the
  # `as_goldfish()` stamp moves. Both producers are built here in one session,
  # because the property that matters is that neither inherits the other's
  # class -- which a test of either one alone cannot show.
  stamped <- as_goldfish(make_stocnet_fixture())
  legacy <- legacy_data_environment()

  expect_s3_class(stamped, "goldfishData")
  expect_false(inherits(stamped, "data.goldfish"))

  expect_s3_class(legacy, "data.goldfish")
  expect_false(inherits(legacy, "goldfishData"))
  expect_true(is.environment(legacy))
})

test_that("each of the two prints under its own method", {
  # Dispatch, not just the class vector. One method used to serve both by
  # branching on `is.environment()`; the split removed the branch, so a method
  # that failed to move with its class would now render the wrong shape.
  stamped <- as_goldfish(make_stocnet_fixture())
  legacy <- legacy_data_environment()

  expect_false(is.null(
    utils::getS3method("print", "goldfishData", optional = TRUE)
  ))
  expect_false(is.null(
    utils::getS3method("print", "data.goldfish", optional = TRUE)
  ))
  expect_output(print(legacy), "Goldfish Data Environment")
  expect_no_match(
    paste(utils::capture.output(print(stamped)), collapse = "\n"),
    "Goldfish Data Environment"
  )
})

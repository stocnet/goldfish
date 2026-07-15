local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("as_goldfish validates and stamps a stocnet-shaped list", {
  d <- as_goldfish(make_stocnet_fixture())
  expect_s3_class(d, "data.goldfish")
  expect_identical(class(d)[1], "data.goldfish")
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

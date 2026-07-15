# cli error snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("valid hand-built fixtures pass validation", {
  expect_no_error(validate_goldfish_data(make_stocnet_fixture()))
  expect_no_error(validate_goldfish_data(make_stocnet_fixture_twomode()))
})

test_that("missing / unnamed / short update coverage aborts", {
  local_cli_context()
  x <- make_stocnet_fixture()

  no_update <- x
  no_update$info$update <- NULL
  expect_snapshot(validate_goldfish_data(no_update), error = TRUE)

  unnamed <- x
  unnamed$info$update <- "increment"
  expect_snapshot(validate_goldfish_data(unnamed), error = TRUE)

  short <- x
  short$ties <- rbind(
    short$ties,
    data.frame(
      from = 1L,
      to = 2L,
      time = 3,
      layer = "sms"
    )
  )
  expect_snapshot(validate_goldfish_data(short), error = TRUE)
})

test_that("duplicate node labels abort", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$nodes$label <- c("A", "A", "C")
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("non-syntactic layer name aborts with a rename suggestion", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$ties$layer <- "phone calls"
  x$info$update <- c("phone calls" = "increment")
  x$info$directed <- c("phone calls" = TRUE)
  x$info$observation <- c("phone calls" = "event")
  x$info$focal <- "phone calls"
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("unmodeled observation type aborts", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$observation <- c(calls = "cross-sectional")
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("character time aborts with conversion guidance", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$ties$time <- as.character(x$ties$time)
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("mixed time axes abort", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$ties$time <- as.POSIXct(c(NA, 1, 2), origin = "1970-01-01", tz = "GMT")
  x$changes <- data.frame(time = 5, node = 1L, var = "floor")
  x$changes$value <- list(list(2))
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("panel focal layer aborts", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$observation <- c(calls = "panel")
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("partially overlapping mode sets abort", {
  local_cli_context()
  x <- make_stocnet_fixture_twomode()
  x$info$sender <- "p"
  x$info$receiver <- c("p", "o")
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("side-impure ties abort", {
  local_cli_context()
  x <- make_stocnet_fixture_twomode()
  x$ties$from <- c(1L, 3L)
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("out-of-range node index aborts", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$ties$from <- c(1L, 2L, 9L)
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("non-logical active values abort", {
  local_cli_context()
  x <- make_stocnet_fixture_twomode()
  x$changes$value <- list(list(1L), list(2L))
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("non-syntactic flavor values abort", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$ties$flavor <- c(NA, "create", "un create")
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

test_that("NA flavor entries are allowed", {
  x <- make_stocnet_fixture()
  x$ties$flavor <- c(NA, "create", "create")
  expect_no_error(validate_goldfish_data(x))
})

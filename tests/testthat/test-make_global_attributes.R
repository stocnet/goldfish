test_that("make_global_attributes creates a valid one-row global.goldfish object", {
  seasons <- make_global_attributes(
    data.frame(winter = 1, spring = 0, summer = 0, autumn = 0)
  )
  expect_s3_class(seasons, "global.goldfish")
  expect_s3_class(seasons, "data.frame")
  expect_equal(nrow(seasons), 1L)
  expect_equal(ncol(seasons), 4L)
  expect_equal(names(seasons), c("winter", "spring", "summer", "autumn"))
  expect_equal(seasons$winter, 1)
  expect_equal(attr(seasons, "events"), character(0))
})

test_that("make_global_attributes works with a single column", {
  g <- make_global_attributes(data.frame(gdp = 100))
  expect_s3_class(g, "global.goldfish")
  expect_equal(nrow(g), 1L)
  expect_equal(g$gdp, 100)
})

test_that("make_global_attributes rejects multi-row input", {
  expect_error(
    make_global_attributes(data.frame(winter = c(0, 1))),
    "one row"
  )
})

test_that("make_global_attributes rejects non-numeric columns", {
  expect_error(
    make_global_attributes(data.frame(season = "spring")),
    "numeric"
  )
})

test_that("make_global_attributes rejects non-data-frame input", {
  expect_error(
    make_global_attributes(c(winter = 1, spring = 0)),
    "data.frame"
  )
})

test_that("check_global_attribute rejects multi-row data frame", {
  g <- structure(
    data.frame(winter = c(0, 1)),
    class = c("global.goldfish", "data.frame")
  )
  expect_error(check_global_attribute(g), "one row")
})

test_that("check_global_attribute rejects non-numeric columns", {
  g <- structure(
    data.frame(season = "spring"),
    class = c("global.goldfish", "data.frame")
  )
  expect_error(check_global_attribute(g), "numeric")
})

test_that("check_global_attribute rejects missing class", {
  g <- data.frame(winter = 1)
  expect_error(check_global_attribute(g), "global.goldfish")
})

test_that("check_global_attribute passes for valid object", {
  g <- make_global_attributes(data.frame(winter = 1))
  expect_true(check_global_attribute(g))
})

test_that("link_events.global.goldfish links replace events successfully", {
  seasons <- make_global_attributes(data.frame(winter = 0, spring = 1))
  season_events <- data.frame(time = 3, replace = 1)
  seasons <- link_events(seasons, season_events)
  expect_equal(attr(seasons, "events"), "season_events")
  expect_equal(attr(season_events, "replace"), "replace")
})

test_that("link_events.global.goldfish rejects events with node column", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  bad_events <- data.frame(time = 3, node = 1, replace = 1)
  expect_error(
    link_events(seasons, bad_events),
    "node"
  )
})

test_that("link_events.global.goldfish rejects increment events", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  bad_events <- data.frame(time = 3, increment = 1)
  expect_error(
    link_events(seasons, bad_events),
    "increment"
  )
})

test_that("link_events.global.goldfish rejects missing time column", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  bad_events <- data.frame(replace = 1)
  expect_error(
    link_events(seasons, bad_events),
    "time"
  )
})

test_that("link_events.global.goldfish rejects missing replace column", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  bad_events <- data.frame(time = 3)
  expect_error(
    link_events(seasons, bad_events),
    "replace"
  )
})

test_that("link_events.global.goldfish returns object invisibly", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  season_events <- data.frame(time = 3, replace = 1)
  result <- withVisible(link_events(seasons, season_events))
  expect_false(result$visible)
})

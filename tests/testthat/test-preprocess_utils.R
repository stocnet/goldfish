# Tests for split_flavours function
test_that("split_flavours respects existing flavour column", {
  df <- tibble(type = c("a", "b"), flavour = c("x", "y"))
  out <- split_flavours(df)
  expect_identical(out, df)
})

test_that("split_flavours synthesizes flavours from increment column", {
  df <- tibble(type = c("a", "a", "b"), increment = c(1, 0, 1))
  out <- split_flavours(df)
  expect_equal(out$flavour, c("a_1", "a_0", "b_1"))
})

test_that("split_flavours applies global flavour_map", {
  df <- tibble(type = c("a", "a"), increment = c(1, 0))
  fm <- c("1" = "creation", "0" = "deletion")
  out <- split_flavours(df, flavour_map = fm)
  expect_equal(out$flavour, c("creation", "deletion"))
})

test_that("split_flavours uses per-type mapping", {
  df <- tibble(type = c("a", "a", "b"), increment = c(1, 0, 1))
  fm <- list(a = c("1" = "Ainc", "0" = "Adec"), b = c("1" = "Binc"))
  out <- split_flavours(df, flavour_map = fm)
  expect_equal(out$flavour, c("Ainc", "Adec", "Binc"))
})

test_that("split_flavours with no increment-like column uses type", {
  df <- tibble(type = c("x", "y"))
  out <- split_flavours(df, flavour_map = NULL)
  expect_equal(out$flavour, c("x", "y"))
})

# Tests for order_events function
test_that("order_events handles empty input", {
  empty <- data.frame()
  out <- order_events(empty)
  expect_equal(nrow(out), 0L)
  expect_true(is.integer(out$event_id))
})

test_that("order_events sorts by time with NA last and assigns event_id", {
  df <- tibble(time = c(5, NA, 2))
  out <- order_events(df)
  expect_equal(out$time, c(2, 5, NA))
  expect_equal(out$event_id, seq_len(nrow(out)))
})


# Test for prepare_events function
test_that("prepare_events extracts and filters network and covariate events", {
  flavour_map <- c("1" = "creation", "0" = "deletion") # set in test env
  nodes <- tibble::tibble(name = c("a", "b"))
  edges <- tibble::tibble(
    from = 1L,
    to = 2L,
    time = c(3),
    type = "t",
    increment = 1
  )
  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  res <- prepare_events(
    g,
    parsing_info = NULL,
    preprocessing_opt = list(startTime = 0, endTime = 10)
  )
  expect_true(is.list(res))
  expect_true("network_events" %in% names(res))
  expect_equal(nrow(res$network_events), 1)
  expect_true("flavour" %in% names(res$network_events))
  expect_equal(res$network_events$flavour[1], "creation")
})

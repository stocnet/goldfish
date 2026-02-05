test_that("pick_inc_col picks first matching increment-like column", {
  df1 <- tibble(a = 1, increment = 2, weight = 3)
  expect_equal(pick_inc_col(df1), "increment")
  df2 <- tibble(replace = 1)
  expect_equal(pick_inc_col(df2), "replace")
  df3 <- tibble(x = 1)
  expect_null(pick_inc_col(df3))
})

test_that("split_flavours synthesizes and respects mappings", {
  df <- tibble(type = c("a", "a", "b"), increment = c(1, 0, 1))
  out <- split_flavours(df)
  expect_equal(out$flavour, c("a_1", "a_0", "b_1"))

  fm <- c("1" = "creation", "0" = "deletion")
  out2 <- split_flavours(df, flavour_map = fm)
  expect_equal(out2$flavour, c("creation", "deletion", "creation"))
})

test_that("add_window_events marks history for times before start", {
  df <- tibble(time = c(1, 5, NA))
  res <- add_window_events(
    df,
    parsing_info = NULL,
    preprocessing_opt = list(startTime = 3)
  )
  expect_true("history" %in% names(res))
  expect_equal(res$history, c(TRUE, FALSE, FALSE))
})

test_that("get_pointer returns correct row indices and handles edge cases", {
  df <- tibble(time = c(2, 4, NA))
  expect_equal(get_pointer(df, 4), 2L)
  expect_equal(get_pointer(df, df$time[2]), 2L)
  expect_equal(get_pointer(df, -Inf), 1L)
  expect_true(is.na(get_pointer(NULL, 1)))
})

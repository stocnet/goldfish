test_that("apply_flat_update writes sender-indexed updates", {
  statsArray <- matrix(0, nrow = 3, ncol = 2)
  updates <- matrix(
    c(
      0, 0, 0, 1.5,
      2, 1, 1, -2
    ),
    nrow = 4
  )
  result <- apply_flat_update(statsArray, updates, is_sender = TRUE)
  expect_equal(result[1, 1], 1.5)
  expect_equal(result[3, 2], -2)
  expect_equal(sum(result != 0), 2L)
})

test_that("apply_flat_update writes dyad-indexed updates", {
  statsArray <- array(0, dim = c(3, 3, 2))
  updates <- matrix(
    c(
      0, 1, 0, 1.5,
      2, 0, 1, -2
    ),
    nrow = 4
  )
  result <- apply_flat_update(statsArray, updates, is_sender = FALSE)
  expect_equal(result[1, 2, 1], 1.5)
  expect_equal(result[3, 1, 2], -2)
  expect_equal(sum(result != 0), 2L)
})

test_that("apply_flat_update with duplicated cells keeps the last value", {
  statsArray <- matrix(0, nrow = 2, ncol = 1)
  updates <- matrix(
    c(
      0, 0, 0, 1,
      0, 0, 0, 7
    ),
    nrow = 4
  )
  result <- apply_flat_update(statsArray, updates, is_sender = TRUE)
  expect_equal(result[1, 1], 7)
})

test_that("apply_flat_update with an empty slice returns the array as is", {
  statsArray <- matrix(seq_len(6), nrow = 3)
  emptySlice <- matrix(numeric(0), nrow = 4, ncol = 0)
  expect_identical(
    apply_flat_update(statsArray, emptySlice, is_sender = TRUE),
    statsArray
  )
  statsArray3d <- array(seq_len(8), dim = c(2, 2, 2))
  expect_identical(
    apply_flat_update(statsArray3d, emptySlice, is_sender = FALSE),
    statsArray3d
  )
})

test_that("apply_flat_update replaces values instead of incrementing", {
  statsArray <- matrix(5, nrow = 2, ncol = 2)
  updates <- matrix(c(1, 1, 1, 0.5), nrow = 4)
  result <- apply_flat_update(statsArray, updates, is_sender = TRUE)
  expect_equal(result[2, 2], 0.5)
})

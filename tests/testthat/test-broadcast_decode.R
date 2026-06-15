# Decode of stat_mat_broadcast must reproduce the eager to_ego()/to_alter()/
# fillChanges() expansion applied through apply_flat_update().

# Build the eager 3D result by expanding a fan-out change into point columns and
# applying them with apply_flat_update(), then compare to apply_broadcast_update.

test_that("kind 1 (to_alter) reproduces eager expansion, one-mode", {
  n1 <- 5L
  n2 <- 5L
  p <- 2L
  arr0 <- array(0, dim = c(n1, n2, p))
  fixed0 <- 2L # 0-indexed held alter
  effect0 <- 1L # 0-indexed effect
  value <- 7

  bc <- matrix(c(1L, fixed0, effect0, value), nrow = 4)
  got <- apply_broadcast_update(arr0, bc, FALSE, n1, n2, FALSE)

  eager <- to_alter(
    matrix(c(node1 = fixed0 + 1L, replace = value), nrow = 1,
           dimnames = list(NULL, c("node1", "replace"))),
    n1, is_two_mode = FALSE
  )
  upd <- rbind(eager[, "node1"] - 1L, eager[, "node2"] - 1L, effect0, value)
  want <- apply_flat_update(arr0, upd, is_sender = FALSE)
  expect_equal(got, want)
  # diagonal cell (fixed, fixed) untouched
  expect_equal(got[fixed0 + 1L, fixed0 + 1L, effect0 + 1L], 0)
})

test_that("kind 2 (to_ego) reproduces eager expansion, one-mode", {
  n1 <- 4L
  n2 <- 4L
  p <- 3L
  arr0 <- array(0, dim = c(n1, n2, p))
  fixed0 <- 1L
  effect0 <- 2L
  value <- -3

  bc <- matrix(c(2L, fixed0, effect0, value), nrow = 4)
  got <- apply_broadcast_update(arr0, bc, FALSE, n1, n2, FALSE)

  eager <- to_ego(
    matrix(c(node1 = fixed0 + 1L, replace = value), nrow = 1,
           dimnames = list(NULL, c("node1", "replace"))),
    n2, is_two_mode = FALSE
  )
  upd <- rbind(eager[, "node1"] - 1L, eager[, "node2"] - 1L, effect0, value)
  want <- apply_flat_update(arr0, upd, is_sender = FALSE)
  expect_equal(got, want)
  expect_equal(got[fixed0 + 1L, fixed0 + 1L, effect0 + 1L], 0)
})

test_that("two-mode broadcast fills the whole row/column (no diagonal skip)", {
  n1 <- 3L
  n2 <- 4L
  p <- 1L
  arr0 <- array(0, dim = c(n1, n2, p))

  # kind 1: full column for the held alter
  bc1 <- matrix(c(1L, 1L, 0L, 5), nrow = 4)
  got1 <- apply_broadcast_update(arr0, bc1, FALSE, n1, n2, TRUE)
  expect_true(all(got1[, 2L, 1L] == 5))
  expect_equal(sum(got1 != 0), n1)

  # kind 2: full row for the held ego
  bc2 <- matrix(c(2L, 0L, 0L, 9), nrow = 4)
  got2 <- apply_broadcast_update(arr0, bc2, FALSE, n1, n2, TRUE)
  expect_true(all(got2[1L, , 1L] == 9))
  expect_equal(sum(got2 != 0), n2)
})

test_that("sender (2D) kind 3 sets the whole effect column", {
  n1 <- 6L
  p <- 3L
  mat0 <- matrix(0, n1, p)
  bc <- matrix(c(3L, 0L, 1L, 4.5), nrow = 4)
  got <- apply_broadcast_update(mat0, bc, TRUE, n1, NA_integer_, TRUE)
  expect_true(all(got[, 2L] == 4.5))
  expect_equal(sum(got != 0), n1)
})

test_that("kind 3 global on a one-mode dyad slice skips the diagonal", {
  n1 <- 4L
  n2 <- 4L
  p <- 1L
  arr0 <- array(0, dim = c(n1, n2, p))
  bc <- matrix(c(3L, 0L, 0L, 2), nrow = 4)
  got <- apply_broadcast_update(arr0, bc, FALSE, n1, n2, FALSE)
  slice <- got[, , 1L]
  expect_true(all(slice[row(slice) != col(slice)] == 2))
  expect_true(all(diag(slice) == 0))
})

test_that("empty broadcast slice is a no-op", {
  arr0 <- array(seq_len(2 * 3 * 2), dim = c(2L, 3L, 2L))
  empty <- matrix(0, 4L, 0L)
  expect_identical(
    apply_broadcast_update(arr0, empty, FALSE, 2L, 3L, FALSE), arr0
  )
  mat0 <- matrix(seq_len(6), 2L, 3L)
  expect_identical(
    apply_broadcast_update(mat0, empty, TRUE, 2L, NA_integer_, TRUE), mat0
  )
})

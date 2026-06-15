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

test_that("reflexive-allowed one-mode square slice writes the diagonal", {
  # twomode_or_reflexive = TRUE on a square (n1 == n2) array: the held cell is
  # written (no diagonal exclusion), matching allowReflexive models.
  n <- 4L
  arr0 <- array(0, dim = c(n, n, 1L))
  # kind 1 holding alter 2 (0-idx) -> full column incl. (2, 2)
  got1 <- apply_broadcast_update(
    arr0, matrix(c(1L, 2L, 0L, 5), nrow = 4), FALSE, n, n, TRUE
  )
  expect_true(all(got1[, 3L, 1L] == 5))
  expect_equal(got1[3L, 3L, 1L], 5)
  # kind 3 global writes every cell incl. the diagonal
  got3 <- apply_broadcast_update(
    arr0, matrix(c(3L, 0L, 0L, 7), nrow = 4), FALSE, n, n, TRUE
  )
  expect_true(all(got3[, , 1L] == 7))
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

# Synthetic C++ cross-check: estimate_DyNAM_choice() fed a non-empty broadcast
# buffer must match the same model fed the eager point-column expansion. This
# exercises the C++ apply_broadcast_updates() helper before the recipe goes
# live (group 5).

# Expand one broadcast entry (kind, fixed0, effect0, value) into 0-indexed
# point columns (node1, node2, effect, replace) reproducing to_alter/to_ego/
# fillChanges semantics for a one-mode (diagonal-skipped) dyad model.
.expand_broadcast_entry <- function(kind, fixed0, effect0, value, n1, n2) {
  if (kind == 1L) {
    rows <- setdiff(seq_len(n1) - 1L, fixed0)
    rbind(rows, fixed0, effect0, value)
  } else if (kind == 2L) {
    cols <- setdiff(seq_len(n2) - 1L, fixed0)
    rbind(fixed0, cols, effect0, value)
  } else {
    grid <- expand.grid(i = seq_len(n1) - 1L, j = seq_len(n2) - 1L)
    grid <- grid[grid$i != grid$j, ]
    rbind(grid$i, grid$j, effect0, value)
  }
}

test_that("C++ choice engine: broadcast buffer matches eager expansion", {
  skip_on_cran()
  n1 <- 5L
  n2 <- 5L
  p <- 2L
  parameters <- c(0.3, -0.5)
  stat_mat_init <- matrix(0, n1 * n2, p)
  dep_event_mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  presence2_init <- rep(1, n2)
  empty_pres <- matrix(0, 0, 0)
  pres_ptr <- numeric(ncol(dep_event_mat))

  # event 1: kind-1 hold alter 1 (0-idx) on effect 0; event 2: kind-2 hold ego
  # 2 on effect 1 + kind-3 on effect 0.
  bc1 <- matrix(c(1L, 1L, 0L, 2.0), nrow = 4)
  bc2 <- cbind(
    matrix(c(2L, 2L, 1L, -1.5), nrow = 4),
    matrix(c(3L, 0L, 0L, 0.7), nrow = 4)
  )
  broadcast <- cbind(bc1, bc2)
  broadcast_ptr <- c(ncol(bc1), ncol(bc1) + ncol(bc2))

  point1 <- .expand_broadcast_entry(1L, 1L, 0L, 2.0, n1, n2)
  point2 <- cbind(
    .expand_broadcast_entry(2L, 2L, 1L, -1.5, n1, n2),
    .expand_broadcast_entry(3L, 0L, 0L, 0.7, n1, n2)
  )
  point <- cbind(point1, point2)
  point_ptr <- c(ncol(point1), ncol(point1) + ncol(point2))

  res_bc <- estimate_DyNAM_choice(
    parameters, dep_event_mat, stat_mat_init,
    matrix(0, 4L, 0L), numeric(ncol(dep_event_mat)),
    broadcast, broadcast_ptr,
    presence2_init, empty_pres, pres_ptr,
    n1, n2, FALSE, FALSE
  )
  res_pt <- estimate_DyNAM_choice(
    parameters, dep_event_mat, stat_mat_init,
    point, point_ptr,
    matrix(0, 4L, 0L), numeric(ncol(dep_event_mat)),
    presence2_init, empty_pres, pres_ptr,
    n1, n2, FALSE, FALSE
  )
  expect_equal(res_bc$logLikelihood, res_pt$logLikelihood, tolerance = 1e-12)
  expect_equal(as.numeric(res_bc$derivative), as.numeric(res_pt$derivative),
               tolerance = 1e-12)
  expect_equal(res_bc$fisher, res_pt$fisher, tolerance = 1e-12)
  # broadcast actually wrote something (not a trivial all-zero match)
  expect_gt(sum(abs(res_pt$fisher)), 0)
})

test_that("gather port: broadcast buffer matches eager expansion", {
  n1 <- 5L
  n2 <- 5L
  p <- 2L
  stat_mat_init <- matrix(0, n1 * n2, p)
  event_mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  presence2_init <- rep(1, n2)
  empty_pres <- matrix(0, 0, 0)
  pres_ptr <- numeric(ncol(event_mat))

  bc1 <- matrix(c(1L, 1L, 0L, 2.0), nrow = 4)
  bc2 <- cbind(
    matrix(c(2L, 2L, 1L, -1.5), nrow = 4),
    matrix(c(3L, 0L, 0L, 0.7), nrow = 4)
  )
  broadcast <- cbind(bc1, bc2)
  broadcast_ptr <- c(ncol(bc1), ncol(bc1) + ncol(bc2))

  point1 <- .expand_broadcast_entry(1L, 1L, 0L, 2.0, n1, n2)
  point2 <- cbind(
    .expand_broadcast_entry(2L, 2L, 1L, -1.5, n1, n2),
    .expand_broadcast_entry(3L, 0L, 0L, 0.7, n1, n2)
  )
  point <- cbind(point1, point2)
  point_ptr <- c(ncol(point1), ncol(point1) + ncol(point2))

  g_bc <- gather_receiver_model_r(
    event_mat, stat_mat_init,
    matrix(0, 4L, 0L), numeric(ncol(event_mat)),
    broadcast, broadcast_ptr,
    presence2_init, empty_pres, pres_ptr,
    n1, n2, FALSE
  )
  g_pt <- gather_receiver_model_r(
    event_mat, stat_mat_init,
    point, point_ptr,
    matrix(0, 4L, 0L), numeric(ncol(event_mat)),
    presence2_init, empty_pres, pres_ptr,
    n1, n2, FALSE
  )
  expect_equal(g_bc$stat_all_events, g_pt$stat_all_events)
  expect_equal(g_bc$n_candidates, g_pt$n_candidates)
  expect_gt(sum(abs(g_pt$stat_all_events)), 0)
})

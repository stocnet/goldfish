# The shared vocabulary for values held at a broadcast kind. Every function here
# generalizes one that already existed with its target kind pinned to point, so
# these tests pin the general behavior the pinned callers never exercised:
# projecting between two NON-point kinds, and addressing entries at a kind that
# is not a dyad cell.

test_that("kind_length reports the stored size of each kind", {
  expect_identical(kind_length(3L, 5L, 7L), 1L)
  expect_identical(kind_length(2L, 5L, 7L), 5L)
  expect_identical(kind_length(1L, 5L, 7L), 7L)
  expect_identical(kind_length(0L, 5L, 7L), 35L)
  expect_error(kind_length(9L, 5L, 7L), "Unknown broadcast kind")
})

test_that("a value round-trips through every widening", {
  n1 <- 3L
  n2 <- 4L
  values <- list(
    "3" = TRUE,
    "2" = c(TRUE, FALSE, TRUE),
    "1" = c(TRUE, FALSE, FALSE, TRUE)
  )
  for (from in names(values)) {
    for (to in c("0", from)) {
      wide <- project_value(
        values[[from]],
        as.integer(from),
        as.integer(to),
        n1,
        n2
      )
      expect_identical(
        reduce_value(wide, as.integer(to), as.integer(from)),
        values[[from]],
        info = paste(from, "->", to)
      )
    }
  }
})

test_that("a scalar widens onto a single axis without materialising a grid", {
  # The case `support_to_grid()` could not express: global -> ego and
  # global -> alter, which is what lets a separable constraint stay separable.
  ego <- project_value(TRUE, 3L, 2L, 3L, 4L)
  alter <- project_value(TRUE, 3L, 1L, 3L, 4L)
  expect_null(dim(ego))
  expect_null(dim(alter))
  expect_length(ego, 3L)
  expect_length(alter, 4L)
})

test_that("ego and alter are incomparable and neither narrows into the other", {
  expect_error(
    project_value(c(TRUE, FALSE, TRUE), 2L, 1L, 3L, 4L),
    "Cannot project"
  )
  expect_error(project_value(TRUE, 2L, 3L, 3L, 4L), "Cannot project")
  expect_error(reduce_value(TRUE, 3L, 0L), "Cannot reduce")
})

test_that("a reduction reads past a zeroed diagonal", {
  # A dyad statistic is a broadcast everywhere EXCEPT on its own diagonal, which
  # is zeroed because a node has no tie to itself. Reading row or column 1
  # outright therefore returns the diagonal entry for node 1 and the true value
  # for every other node, which is exactly one wrong node and no visible
  # breakage. `ego(a)` with a = (1, 2, 3) is the shape.
  ego_grid <- matrix(c(1, 2, 3), 3L, 3L)
  alter_grid <- matrix(c(1, 2, 3), 3L, 3L, byrow = TRUE)
  diag(ego_grid) <- 0
  diag(alter_grid) <- 0

  expect_identical(reduce_value(ego_grid, 0L, 2L), c(1, 2, 3))
  expect_identical(reduce_value(alter_grid, 0L, 1L), c(1, 2, 3))

  scalar_grid <- matrix(7, 3L, 3L)
  diag(scalar_grid) <- 0
  expect_identical(reduce_value(scalar_grid, 0L, 3L), 7)
})

test_that("a single-node axis has no off-diagonal cell to read", {
  # Degenerate but reachable: with one receiver there is no column other than
  # the diagonal one, so the only index there is has to serve.
  expect_identical(reduce_value(matrix(4, 2L, 1L), 0L, 2L), c(4, 4))
  expect_identical(reduce_value(matrix(4, 1L, 2L), 0L, 1L), c(4, 4))
})

test_that("projection to point agrees with the axis it varies on", {
  ego <- c(TRUE, FALSE, TRUE)
  alter <- c(TRUE, FALSE, FALSE, TRUE)
  expect_identical(
    project_value(ego, 2L, 0L, 3L, 4L),
    matrix(ego, 3L, 4L)
  )
  expect_identical(
    project_value(alter, 1L, 0L, 3L, 4L),
    matrix(alter, 3L, 4L, byrow = TRUE)
  )
})

test_that("an alter delta names one entry at its kind and a column at point", {
  at_kind <- project_entries(1L, 2L, 9, 1L, 1L, 3L, 4L)
  expect_identical(at_kind$entries, 2L)
  expect_identical(at_kind$values, 9)

  at_point <- project_entries(1L, 2L, 9, 1L, 0L, 3L, 4L)
  expect_identical(dim(at_point$entries), c(3L, 2L))
  expect_true(all(at_point$entries[, 2L] == 2L))
  expect_identical(at_point$values, rep(9, 3L))
})

test_that("a global delta names the whole value at every target kind", {
  # The last write of an event wins, which is how a global attribute change
  # behaves when several rows land at one time.
  expect_identical(
    project_entries(1L, 1L, c(1, 5), 3L, 3L, 3L, 4L)$values,
    5
  )
  expect_length(project_entries(1L, 1L, 5, 3L, 2L, 3L, 4L)$entries, 3L)
  expect_length(project_entries(1L, 1L, 5, 3L, 1L, 3L, 4L)$entries, 4L)
  expect_identical(
    dim(project_entries(1L, 1L, 5, 3L, 0L, 3L, 4L)$entries),
    c(12L, 2L)
  )
})

test_that("write_entries writes every kind-shaped buffer in place", {
  # ADR-0059's invariant, extended to the buffer shapes the walk maintains: the
  # C++ write mutates the caller's value, so the assertion is on the ORIGINAL
  # binding, not on what was returned. A vector and a matrix, a double and a
  # logical, are all the same write behind one linear index.
  point <- matrix(0, 3L, 4L)
  invisible(write_entries(point, cbind(1L, 2L), 7))
  expect_identical(point[1L, 2L], 7)

  alter <- c(1, 2, 3, 4)
  invisible(write_entries(alter, 3L, 9))
  expect_identical(alter, c(1, 2, 9, 4))

  mask <- c(TRUE, TRUE, TRUE)
  invisible(write_entries(mask, 2L, FALSE))
  expect_identical(mask, c(TRUE, FALSE, TRUE))

  mask_grid <- matrix(TRUE, 2L, 2L)
  invisible(write_entries(mask_grid, cbind(2L, 1L), FALSE))
  expect_identical(mask_grid[2L, 1L], FALSE)
})

test_that("the in-place write leaves the buffer unduplicated", {
  # The point of the C++ write: passing the buffer as an argument marks it
  # shared, so an R subassignment would copy it here. `tracemem` reports every
  # such duplication, and there is none.
  skip_if_not(capabilities("profmem"), "R built without memory profiling")
  buffer <- c(1, 2, 3, 4)
  invisible(tracemem(buffer))
  # `tracemem` reports on stdout, so this must capture "output": capturing
  # "message" catches nothing, and the test would pass whether or not the
  # buffer was copied.
  copies <- utils::capture.output(
    for (k in 1:5) {
      # A double buffer takes double values; an integer here would fall back to
      # subassignment and copy, which is the fallback's job and not this test's.
      write_entries(buffer, k %% 4L + 1L, as.double(k))
    },
    type = "output"
  )
  untracemem(buffer)
  expect_length(copies, 0L)
})

test_that("write_entries subassigns when the value type does not match", {
  # Coercing in C++ would allocate the copy the write exists to avoid, and
  # silently changing a buffer's storage type is worse than falling back.
  buffer <- c(TRUE, TRUE, TRUE)
  written <- write_entries(buffer, 2L, 0)
  expect_identical(written, c(1, 0, 1))
  expect_identical(buffer, c(TRUE, TRUE, TRUE))
})

test_that("set_entries refuses a buffer type it cannot write", {
  expect_error(
    set_entries(c("a", "b"), 1L, "c"),
    "needs a double or logical buffer"
  )
  expect_error(
    set_entries(c(1, 2), 1L, TRUE),
    "needs values of the buffer's own type"
  )
  expect_error(set_entries(c(1, 2), 5L, 1), "index out of bounds")
})

test_that("write_entries is a no-op on an empty delta", {
  buffer <- c(TRUE, FALSE)
  expect_identical(write_entries(buffer, integer(0), logical(0)), buffer)
})

test_that("emit_crossings names only the entries that moved", {
  previous <- c(TRUE, FALSE, TRUE, TRUE)
  current <- c(TRUE, TRUE, TRUE, FALSE)
  crossing <- emit_crossings(previous, current)
  expect_identical(crossing$entries, c(2L, 4L))
  expect_identical(crossing$values, c(TRUE, FALSE))
})

test_that("emit_crossings restricted to entries compares only those", {
  # The locality every incremental consumer relies on: an entry that moved but
  # was not offered is not reported, because the caller has already established
  # that nothing outside its offered set could have changed.
  previous <- c(TRUE, FALSE, TRUE, TRUE)
  current <- c(FALSE, TRUE, TRUE, FALSE)
  crossing <- emit_crossings(previous, current, entries = c(2L, 3L))
  expect_identical(crossing$entries, 2L)
  expect_identical(crossing$values, TRUE)
})

test_that("emit_crossings addresses a point buffer by cell", {
  previous <- matrix(FALSE, 2L, 2L)
  current <- matrix(c(FALSE, TRUE, FALSE, FALSE), 2L, 2L)
  cells <- cbind(c(1L, 2L), c(1L, 1L))
  crossing <- emit_crossings(previous, current, entries = cells)
  expect_identical(dim(crossing$entries), c(1L, 2L))
  expect_identical(as.vector(crossing$entries), c(2L, 1L))
  expect_identical(crossing$values, TRUE)
})

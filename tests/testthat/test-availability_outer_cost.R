# Cost guards for a vector-kind constraint on a dyad loop. Two factor vectors
# describe an alter-, ego- or global-kind mask exactly, so neither the stored
# object nor the walk may carry an n1 x n2 grid for it.

test_that("a vector-kind REM constraint stores two vectors, not a grid", {
  # Recorded on the point fold: 10.4 MB above the unconstrained object
  # (one 8.6 MB logical grid plus its flips) against a 48 KB limit.
  skip_on_cran()
  data <- availability_cost_rem_data()
  unconstrained <- availability_cost_rem_prep(data)
  constrained <- availability_cost_rem_prep(data, ~ alter(allow) > 0)
  n_actors <- nrow(data$nodes)
  added <- as.numeric(lobstr::obj_size(constrained)) -
    as.numeric(lobstr::obj_size(unconstrained))
  expect_lt(added, 4 * (n_actors + n_actors) * 4)
})

test_that("a vector-kind REM constraint allocates no grid in the walk", {
  # Recorded on the point fold: the constrained walk made grid-sized
  # allocations the unconstrained one did not (the availability grid, its
  # columns, and the atom seeded as a dense statistic then reduced), 219.4 MB
  # in all. The dyadic statistic is an n1 x n2 array on both sides, so the
  # guard compares the count of allocations of a logical grid's size or more.
  skip_on_cran()
  skip_if_not_installed("bench")
  data <- availability_cost_rem_data()
  n_actors <- nrow(data$nodes)
  grid_bytes <- n_actors * n_actors * 4
  grids <- function(constraint) {
    memory <- bench::bench_memory(
      availability_cost_rem_prep(data, constraint)
    )$memory[[1]]
    sum(memory$bytes >= grid_bytes, na.rm = TRUE)
  }
  # Warm both paths so first-call compilation is not counted.
  invisible(availability_cost_rem_prep(data, NULL))
  invisible(availability_cost_rem_prep(data, ~ alter(allow) > 0))
  expect_identical(grids(~ alter(allow) > 0), grids(NULL))
})

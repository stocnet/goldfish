# A vector-kind support constraint on a dyad loop (REM, coordination) against
# its frozen fold. The reference stores the availability stream and the
# estimates; `availability_cells()` replays the stream to the dense cell values
# at every stored row, so a fold stored in a different encoding is compared
# cell for cell, and its estimates within 1e-10, on every backend.

availability_vector_refs <- function() {
  readRDS(test_path("fixtures", "availability_vector_refs.rds"))
}

test_that("a vector-kind dyad constraint keeps its availability at every row", {
  refs <- availability_vector_refs()
  for (family in availability_vector_families()) {
    prep <- availability_vector_run(family)
    expect_identical(prep$event_time, refs[[family]]$stream$event_time)
    expect_identical(
      availability_cells(prep),
      availability_cells(refs[[family]]$stream),
      info = family
    )
  }
})

test_that("a vector-kind dyad constraint keeps its estimates, every backend", {
  skip_on_cran()
  refs <- availability_vector_refs()
  for (family in availability_vector_families()) {
    for (backend in c("r", "cpp", "gather")) {
      fit <- availability_vector_run(family, backend)
      ref <- refs[[family]]$fits[[backend]]
      label <- paste(family, backend)
      expect_equal(coef(fit), ref$coef, tolerance = 1e-10, info = label)
      expect_equal(
        fit$log_likelihood,
        ref$log_likelihood,
        tolerance = 1e-10,
        info = label
      )
    }
  }
})

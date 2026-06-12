test_that("dynam choice recipe produces the flat preprocessing output", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      tie(networkExog, weighted = TRUE),
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_null(preproData$stats_change)
  expect_true(is.matrix(preproData$stat_mat_update))
  expect_identical(nrow(preproData$stat_mat_update), 4L)
  expect_length(preproData$stat_mat_pointer, length(preproData$is_dependent))
  expect_identical(
    ncol(preproData$stat_mat_update),
    as.integer(
      preproData$stat_mat_pointer[length(preproData$stat_mat_pointer)]
    )
  )
  expect_true(all(diff(preproData$stat_mat_pointer) >= 0))
  expect_length(dim(preproData$initialStats), 3L)
  expect_identical(
    dim(preproData$initialStats),
    c(5L, 5L, 2L)
  )
})

test_that("dynam choice recipe stores dependent events only", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE),
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_true(all(preproData$is_dependent == 1L))
  expect_null(preproData$n_dep_events)
  expect_null(preproData$total_time)
  expect_null(preproData$avg_active_actors)
  expect_null(preproData$presence1_update)
  expect_null(preproData$presence1_update_pointer)
})

test_that("dynam choice recipe replay matches the final statistics", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      tie(networkExog, weighted = TRUE),
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  statsArray <- preproData$initialStats
  pointer <- 0L
  for (i in seq_along(preproData$stat_mat_pointer)) {
    upto <- preproData$stat_mat_pointer[i]
    if (upto > pointer) {
      statsArray <- apply_flat_update(
        statsArray,
        preproData$stat_mat_update[, (pointer + 1L):upto, drop = FALSE],
        is_sender = FALSE
      )
    }
    pointer <- upto
  }
  expect_equal(
    statsArray[, , 1],
    matrix(
      # fmt: skip
      c(
        0, 4, 0, 0, 2,
        1, 0, 4, 1, 0,
        0, 2, 0, 3, 0,
        0, 1, 1, 0, 0,
        1, 1, 0, 0, 0
      ),
      nrow = 5,
      ncol = 5,
      byrow = TRUE
    ),
    label = "inertia stats after replaying all updates"
  )
})

test_that("flat choice preprocessing reused through preprocessing_init", {
  formulaFull <- depNetwork ~ inertia(networkState, weighted = TRUE) +
    tie(networkExog, weighted = TRUE)
  preproData <- estimate_wrapper(
    formulaFull,
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  prepSubset <- estimate_wrapper(
    depNetwork ~ tie(networkExog, weighted = TRUE),
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_init = preproData,
    preprocessing_only = TRUE
  )
  prepDirect <- estimate_wrapper(
    depNetwork ~ tie(networkExog, weighted = TRUE),
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(prepSubset$initialStats, prepDirect$initialStats)
  expect_equal(prepSubset$stat_mat_update, prepDirect$stat_mat_update)
  expect_equal(prepSubset$stat_mat_pointer, prepDirect$stat_mat_pointer)
})

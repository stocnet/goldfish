test_that("dynam rate recipe produces the flat preprocessing output", {
  preproData <- estimate_wrapper(
    depNetwork ~ 1 +
      outdeg(networkState, weighted = TRUE) +
      indeg(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate",
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
  expect_length(dim(preproData$initialStats), 2L)
  expect_identical(
    dim(preproData$initialStats),
    c(5L, 2L)
  )
})

test_that("dynam rate recipe stores the intercept scalars", {
  preproData <- estimate_wrapper(
    depNetwork ~ 1 + outdeg(networkState, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_identical(
    preproData$n_dep_events,
    sum(preproData$is_dependent == 1L)
  )
  expect_equal(preproData$total_time, sum(preproData$intervals))
  expect_gt(preproData$avg_active_entity, 0)
  expect_lte(preproData$avg_active_entity, 5)
})

test_that("dynam rate recipe stores presence updates in C format", {
  preproData <- estimate_wrapper(
    depNetwork ~ 1 + outdeg(networkState, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_identical(nrow(preproData$active_sender_update), 2L)
  expect_identical(
    ncol(preproData$active_sender_update),
    nrow(compChange)
  )
  expect_length(
    preproData$active_sender_update_pointer,
    length(preproData$is_dependent)
  )
})

test_that("dynam rate recipe replay matches the replayed initial stats", {
  preproData <- estimate_wrapper(
    depNetwork ~ 1 +
      outdeg(networkState, weighted = TRUE) +
      indeg(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate",
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
        is_sender = TRUE
      )
    }
    pointer <- upto
  }
  expect_equal(
    statsArray[, 1],
    c(6, 6, 5, 2, 2),
    label = "outdeg stats after replaying all updates"
  )
})

test_that("dynam rate ordered recipe stores dependent events only", {
  preproData <- estimate_wrapper(
    depNetwork ~ outdeg(networkState, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_null(preproData$stats_change)
  expect_true(is.matrix(preproData$stat_mat_update))
  expect_true(all(preproData$is_dependent == 1L))
  expect_length(dim(preproData$initialStats), 2L)
  expect_null(preproData$n_dep_events)
  expect_null(preproData$total_time)
  expect_null(preproData$avg_active_entity)
  expect_identical(nrow(preproData$active_sender_update), 2L)
  expect_length(
    preproData$active_sender_update_pointer,
    length(preproData$is_dependent)
  )
})

test_that("flat preprocessing reused through preprocessed", {
  formulaFull <- depNetwork ~ 1 +
    outdeg(networkState, weighted = TRUE) +
    indeg(networkExog, weighted = TRUE)
  preproData <- estimate_wrapper(
    formulaFull,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  prepSubset <- estimate_wrapper(
    depNetwork ~ 1 + indeg(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    preprocessed = preproData,
    preprocessing_only = TRUE
  )
  prepDirect <- estimate_wrapper(
    depNetwork ~ 1 + indeg(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(prepSubset$initialStats, prepDirect$initialStats)
  expect_equal(prepSubset$stat_mat_update, prepDirect$stat_mat_update)
  expect_equal(prepSubset$stat_mat_pointer, prepDirect$stat_mat_pointer)
})

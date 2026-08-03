# context("Effects for model = 'DyNAMi' and sub_model = 'choice'")

# test inertia and tie with different subtypes ----
test_that("inertia/tie with objects weighted with all possible options", {
  preproData <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      inertia(past_network_DyNAMi, weighted = TRUE, sub_type = "count") +
      tie(covnetwork_DyNAMi, weighted = TRUE, sub_type = "count") +
      inertia(past_network_DyNAMi, weighted = TRUE, sub_type = "proportion") +
      tie(covnetwork_DyNAMi, weighted = TRUE, sub_type = "proportion") +
      inertia(past_network_DyNAMi, weighted = TRUE, sub_type = "presence") +
      tie(covnetwork_DyNAMi, weighted = TRUE, sub_type = "presence") +
      inertia(past_network_DyNAMi, weighted = TRUE, sub_type = "min") +
      tie(covnetwork_DyNAMi, weighted = TRUE, sub_type = "min") +
      inertia(past_network_DyNAMi, weighted = TRUE, sub_type = "mean") +
      tie(covnetwork_DyNAMi, weighted = TRUE, sub_type = "mean") +
      inertia(past_network_DyNAMi, weighted = TRUE, sub_type = "max") +
      tie(covnetwork_DyNAMi, weighted = TRUE, sub_type = "max"),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  updFun <- function(stat, change) {
    if (!is.null(change)) {
      stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    }
    return(stat)
  }

  for (i in c(1, 3, 5, 7, 9, 11)) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix inertia"
    )
  }
  for (i in c(2, 4, 6, 8, 10, 12)) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          1,
          1,
          0,
          1,
          0,
          1,
          0,
          1,
          1,
          0,
          0,
          0,
          0,
          0,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix tie"
    )
  }

  # Detail for inertia/count
  stat <- preproData$initial_stats[,, 1]
  change <- preproData$dependent_stats_change[[2]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        1,
        0,
        1,
        1,
        1,
        0,
        1,
        2,
        0,
        0,
        1,
        1,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia (4/4)"
  )

  # Detail for tie/count
  stat <- preproData$initial_stats[,, 2]
  change <- preproData$dependent_stats_change[[2]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        0,
        1,
        1,
        0,
        0,
        2,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie (4/4)"
  )

  # Final stats for other inertia effects:
  #  proportion, presence, min, mean, mean
  stat <- preproData$initial_stats[,, 3]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[3]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        1,
        1,
        0,
        1,
        1,
        0,
        0,
        1,
        1,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia proportion"
  )
  stat <- preproData$initial_stats[,, 5]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[5]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        1,
        1,
        0,
        1,
        1,
        0,
        0,
        1,
        1,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia presence"
  )
  stat <- preproData$initial_stats[,, 7]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[7]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        1,
        1,
        0,
        1,
        1,
        0,
        0,
        1,
        1,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia min"
  )
  stat <- preproData$initial_stats[,, 9]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[9]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        1,
        1,
        0,
        1,
        1,
        0,
        0,
        1,
        1,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia mean"
  )
  stat <- preproData$initial_stats[,, 11]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[11]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        1,
        1,
        0,
        1,
        1,
        0,
        0,
        1,
        1,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia max"
  )

  # Final stats for other tie effects: proportion, presence, min, mean, mean
  stat <- preproData$initial_stats[,, 4]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[4]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0.5,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        0.5,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie proportion"
  )
  stat <- preproData$initial_stats[,, 6]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[6]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie presence"
  )
  stat <- preproData$initial_stats[,, 8]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[8]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie min"
  )
  stat <- preproData$initial_stats[,, 10]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[10]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0.5,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        0.5,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie mean"
  )
  stat <- preproData$initial_stats[,, 12]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[12]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix tie max"
  )
})


# test inertia with a window ----
test_that("inertia computes correct preprocessing objects with window", {
  preproData <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      inertia(past_network_DyNAMi, weighted = TRUE) +
      inertia(past_network_DyNAMi, window = 2, weighted = TRUE) +
      inertia(past_network_DyNAMi, window = 7, weighted = TRUE),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  updFun <- function(stat, change) {
    if (!is.null(change)) {
      stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    }
    return(stat)
  }

  # check intitialization
  for (i in c(1, 2, 3)) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix inertia with windows"
    )
  }

  # Detail for window 2s
  stat <- preproData$initial_stats[,, 2]
  change <- preproData$dependent_stats_change[[2]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia window (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0.5,
        0,
        0,
        0,
        0.5,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia window (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia window (4/4)"
  )

  # Detail for window 7s
  stat <- preproData$initial_stats[,, 3]
  change <- preproData$dependent_stats_change[[2]][[3]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia window (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[3]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia window (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[3]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix inertia window (4/4)"
  )
})


# test alterpop and alterdeg ----
test_that("alterpop/alterdeg with objects weighted with all possible options", {
  preproData <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      alterpop(
        past_network_DyNAMi,
        weighted = TRUE,
        sub_type = "mean_normalized"
      ) +
      alterdeg(
        covnetwork_DyNAMi,
        weighted = TRUE,
        sub_type = "mean_normalized"
      ) +
      alterpop(past_network_DyNAMi, weighted = TRUE, sub_type = "min") +
      alterdeg(covnetwork_DyNAMi, weighted = TRUE, sub_type = "min") +
      alterpop(past_network_DyNAMi, weighted = TRUE, sub_type = "mean") +
      alterdeg(covnetwork_DyNAMi, weighted = TRUE, sub_type = "mean") +
      alterpop(past_network_DyNAMi, weighted = TRUE, sub_type = "max") +
      alterdeg(covnetwork_DyNAMi, weighted = TRUE, sub_type = "max") +
      alterpop(
        past_network_DyNAMi,
        weighted = TRUE,
        sub_type = "mean_centered"
      ) +
      alterdeg(covnetwork_DyNAMi, weighted = TRUE, sub_type = "mean_centered"),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  updFun <- function(stat, change) {
    if (!is.null(change)) {
      stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    }
    return(stat)
  }

  for (i in c(1, 3, 5, 7, 9)) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix alterpop"
    )
  }
  for (i in c(2, 10)) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          0.5,
          0.5,
          -1.5,
          0.5,
          0,
          0.5,
          -1.5,
          0.5,
          0.5,
          0,
          -1.5,
          0.5,
          0.5,
          0.5,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix alterdeg"
    )
  }
  for (i in c(4, 6, 8)) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          2,
          2,
          0,
          2,
          0,
          2,
          0,
          2,
          2,
          0,
          0,
          2,
          2,
          2,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix alterdeg"
    )
  }

  # Detail for alterpop mean
  stat <- preproData$initial_stats[,, 5]
  change <- preproData$dependent_stats_change[[2]][[5]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        1,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[5]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[5]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        3,
        3,
        0,
        3,
        3,
        3,
        0,
        3,
        3,
        0,
        0,
        3,
        3,
        3,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop (4/4)"
  )

  # Detail for alterdeg mean
  stat <- preproData$initial_stats[,, 6]
  change <- preproData$dependent_stats_change[[2]][[6]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        2,
        0,
        0,
        2,
        2,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        2,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[6]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0,
        0,
        2,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[6]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        1,
        2,
        0,
        2,
        0,
        2,
        0,
        2,
        1,
        0,
        0,
        2,
        2,
        2,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg (4/4)"
  )

  # Final stats for other inertia effects:
  #  mean_normalized, min, max, mean_centered
  stat <- preproData$initial_stats[,, 1]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[1]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop mean centered"
  )
  stat <- preproData$initial_stats[,, 2]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[2]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -0.5,
        0.5,
        0,
        0.5,
        -1.5,
        0.5,
        0,
        0.5,
        -0.5,
        0,
        0,
        0.5,
        0.5,
        0.5,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg mean centered"
  )
  stat <- preproData$initial_stats[,, 3]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[3]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        3,
        3,
        0,
        3,
        3,
        3,
        0,
        3,
        3,
        0,
        0,
        3,
        3,
        3,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop min"
  )
  stat <- preproData$initial_stats[,, 4]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[4]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        2,
        0,
        2,
        0,
        2,
        0,
        2,
        0,
        0,
        0,
        2,
        2,
        2,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg min"
  )
  stat <- preproData$initial_stats[,, 7]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[7]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        3,
        3,
        0,
        3,
        3,
        3,
        0,
        3,
        3,
        0,
        0,
        3,
        3,
        3,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop max"
  )
  stat <- preproData$initial_stats[,, 8]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[8]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        2,
        0,
        2,
        0,
        2,
        0,
        2,
        2,
        0,
        0,
        2,
        2,
        2,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg max"
  )
  stat <- preproData$initial_stats[,, 9]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[9]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterpop mean centered"
  )
  stat <- preproData$initial_stats[,, 10]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[10]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -0.5,
        0.5,
        0,
        0.5,
        -1.5,
        0.5,
        0,
        0.5,
        -0.5,
        0,
        0,
        0.5,
        0.5,
        0.5,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alterdeg mean centered"
  )
})


# test size ----
test_that("size with objects weighted with all possible options", {
  preproData <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      size(interaction_network_DyNAMi, sub_type = "identity") +
      size(interaction_network_DyNAMi, sub_type = "squared"),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  updFun <- function(stat, change) {
    if (!is.null(change)) {
      stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    }
    return(stat)
  }

  expect_equal(
    preproData$initial_stats[,, 1],
    matrix(
      c(
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alterdeg"
  )
  expect_equal(
    preproData$initial_stats[,, 2],
    matrix(
      c(
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alterdeg"
  )

  # Detail for size
  stat <- preproData$initial_stats[,, 1]
  change <- preproData$dependent_stats_change[[2]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        1,
        1,
        0,
        2,
        1,
        1,
        0,
        2,
        1,
        1,
        0,
        2,
        1,
        1
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        3,
        0,
        1,
        0,
        3,
        0,
        1,
        0,
        3,
        0,
        1,
        0,
        3,
        0,
        1
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        1,
        2,
        1,
        0,
        1,
        2,
        1,
        0,
        1,
        2,
        1,
        0,
        1,
        2,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (4/4)"
  )

  # Detail for size squared
  stat <- preproData$initial_stats[,, 2]
  change <- preproData$dependent_stats_change[[2]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        4,
        1,
        1,
        0,
        4,
        1,
        1,
        0,
        4,
        1,
        1,
        0,
        4,
        1,
        1
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        9,
        0,
        1,
        0,
        9,
        0,
        1,
        0,
        9,
        0,
        1,
        0,
        9,
        0,
        1
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[2]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        1,
        4,
        1,
        0,
        1,
        4,
        1,
        0,
        1,
        4,
        1,
        0,
        1,
        4,
        1,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (4/4)"
  )
})


# test alter ----
test_that("alter with objects weighted with all possible options", {
  preproData <- estimate_wrapper(
    dependent.depevents_DyNAMi ~ alter(actors_DyNAMi$attr1, sub_type = "mean") +
      alter(actors_DyNAMi$attr1, sub_type = "mean_normalized") +
      alter(actors_DyNAMi$attr1, sub_type = "mean_squared") +
      alter(actors_DyNAMi$attr1, sub_type = "min") +
      alter(actors_DyNAMi$attr1, sub_type = "max") +
      alter(actors_DyNAMi$attr1, sub_type = "range") +
      alter(actors_DyNAMi$attr1, sub_type = "mean_centered"),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  updFun <- function(stat, change) {
    if (!is.null(change)) {
      stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    }
    return(stat)
  }

  expect_equal(
    preproData$initial_stats[,, 1],
    matrix(
      c(
        0,
        22,
        26,
        30,
        20,
        0,
        26,
        30,
        20,
        22,
        0,
        30,
        20,
        22,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter mean"
  )
  s <- sd(c(20, 22, 26, 30))
  m <- mean(c(20, 22, 26, 30))
  c <- c((20 - m) / s, (22 - m) / s, (26 - m) / s, (30 - m) / s)
  expect_equal(
    preproData$initial_stats[,, 2],
    matrix(
      c(
        0,
        c[2],
        c[3],
        c[4],
        c[1],
        0,
        c[3],
        c[4],
        c[1],
        c[2],
        0,
        c[4],
        c[1],
        c[2],
        c[3],
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter mean normalized"
  )
  expect_equal(
    preproData$initial_stats[,, 3],
    matrix(
      c(
        0,
        484,
        676,
        900,
        400,
        0,
        676,
        900,
        400,
        484,
        0,
        900,
        400,
        484,
        676,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter mean"
  )
  expect_equal(
    preproData$initial_stats[,, 4],
    matrix(
      c(
        0,
        22,
        26,
        30,
        20,
        0,
        26,
        30,
        20,
        22,
        0,
        30,
        20,
        22,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter min"
  )
  expect_equal(
    preproData$initial_stats[,, 5],
    matrix(
      c(
        0,
        22,
        26,
        30,
        20,
        0,
        26,
        30,
        20,
        22,
        0,
        30,
        20,
        22,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter max"
  )
  expect_equal(
    preproData$initial_stats[,, 6],
    matrix(
      c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter range"
  )
  expect_equal(
    preproData$initial_stats[,, 7],
    matrix(
      c(
        0,
        22 - m,
        26 - m,
        30 - m,
        20 - m,
        0,
        26 - m,
        30 - m,
        20 - m,
        22 - m,
        0,
        30 - m,
        20 - m,
        22 - m,
        26 - m,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "initialization of the statistics matrix alter mean centered"
  )

  # Detail for alter mean
  stat <- preproData$initial_stats[,, 1]
  change <- preproData$dependent_stats_change[[2]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        22,
        26,
        30,
        0,
        20,
        26,
        30,
        0,
        21,
        0,
        30,
        0,
        21,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        24,
        0,
        30,
        0,
        23,
        0,
        30,
        0,
        21,
        0,
        30,
        0,
        68 / 3,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        26,
        26,
        0,
        20,
        30,
        26,
        0,
        20,
        26,
        0,
        0,
        20,
        22,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix size (4/4)"
  )

  # Final stats for other alter effects:
  #  mean_normalized, mean_squared,  min, max, range, mean_centered
  stat <- preproData$initial_stats[,, 2]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[2]]
    stat <- updFun(stat, change)
  }
  c <- c((20 - m) / s, (22 - m) / s, (26 - m) / s, (30 - m) / s)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        c[3],
        c[3],
        0,
        c[1],
        c[4],
        c[3],
        0,
        c[1],
        c[3],
        0,
        0,
        c[1],
        c[2],
        c[3],
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alter mean normalized"
  )
  stat <- preproData$initial_stats[,, 3]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[3]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        676,
        676,
        0,
        400,
        900,
        676,
        0,
        400,
        676,
        0,
        0,
        400,
        484,
        676,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alter mean squared"
  )
  stat <- preproData$initial_stats[,, 4]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[4]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        22,
        26,
        0,
        20,
        30,
        26,
        0,
        20,
        22,
        0,
        0,
        20,
        22,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alter min"
  )
  stat <- preproData$initial_stats[,, 5]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[5]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        30,
        26,
        0,
        20,
        30,
        26,
        0,
        20,
        30,
        0,
        0,
        20,
        22,
        26,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alter max"
  )
  stat <- preproData$initial_stats[,, 6]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[6]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        8,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        8,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alter max"
  )
  stat <- preproData$initial_stats[,, 7]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[7]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        26 - m,
        26 - m,
        0,
        20 - m,
        30 - m,
        26 - m,
        0,
        20 - m,
        26 - m,
        0,
        0,
        20 - m,
        22 - m,
        26 - m,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix alter mean centered"
  )
})


# test same/diff/sim ----
test_that("same/diff/sim with objects weighted with all possible options", {
  preproData <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      same(actors_DyNAMi$attr2, sub_type = "proportion") +
      same(actors_DyNAMi$attr2, sub_type = "count") +
      same(actors_DyNAMi$attr2, sub_type = "presence") +
      diff(actors_DyNAMi$attr1, sub_type = "averaged_sum") +
      diff(actors_DyNAMi$attr1, sub_type = "mean") +
      diff(actors_DyNAMi$attr1, sub_type = "min") +
      diff(actors_DyNAMi$attr1, sub_type = "max") +
      sim(actors_DyNAMi$attr1, sub_type = "averaged_sum") +
      sim(actors_DyNAMi$attr1, sub_type = "mean") +
      sim(actors_DyNAMi$attr1, sub_type = "min") +
      sim(actors_DyNAMi$attr1, sub_type = "max"),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  updFun <- function(stat, change) {
    if (!is.null(change)) {
      stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    }
    return(stat)
  }

  for (i in 1:3) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          1,
          1,
          0,
          0,
          0,
          0,
          1,
          0,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix sim"
    )
  }
  for (i in 4:7) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          2,
          6,
          10,
          2,
          0,
          4,
          8,
          6,
          4,
          0,
          4,
          10,
          8,
          4,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix diff"
    )
  }
  for (i in 8:11) {
    expect_equal(
      preproData$initial_stats[,, i],
      matrix(
        c(
          0,
          -2,
          -6,
          -10,
          -2,
          0,
          -4,
          -8,
          -6,
          -4,
          0,
          -4,
          -10,
          -8,
          -4,
          0
        ),
        4,
        4,
        TRUE
      ),
      label = "initialization of the statistics matrix same"
    )
  }

  # Detail for sim proportion
  stat <- preproData$initial_stats[,, 1]
  change <- preproData$dependent_stats_change[[2]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0.5,
        0,
        0,
        0,
        0.5,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0.5,
        0,
        0,
        0,
        0,
        0,
        1,
        0,
        0.5,
        0,
        0,
        0,
        1 / 3,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[1]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        1,
        0,
        0,
        1,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same (4/4)"
  )

  # Detail for diff averaged sum
  stat <- preproData$initial_stats[,, 4]
  change <- preproData$dependent_stats_change[[2]][[4]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        6,
        10,
        0,
        2,
        4,
        8,
        0,
        5,
        0,
        4,
        0,
        9,
        4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix diff (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[4]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        4,
        0,
        10,
        0,
        3,
        0,
        8,
        0,
        5,
        0,
        4,
        0,
        22 / 3,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix diff (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[4]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        6,
        6,
        0,
        2,
        8,
        4,
        0,
        6,
        4,
        0,
        0,
        10,
        8,
        4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix diff (4/4)"
  )

  # Detail for diff averaged sum
  stat <- preproData$initial_stats[,, 8]
  change <- preproData$dependent_stats_change[[2]][[8]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -2,
        -6,
        -10,
        0,
        -2,
        -4,
        -8,
        0,
        -5,
        0,
        -4,
        0,
        -9,
        -4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same (2/4)"
  )
  change <- preproData$dependent_stats_change[[3]][[8]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -4,
        0,
        -10,
        0,
        -3,
        0,
        -8,
        0,
        -5,
        0,
        -4,
        0,
        -22 / 3,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same (3/4)"
  )
  change <- preproData$dependent_stats_change[[4]][[8]]
  stat <- updFun(stat, change)
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -6,
        -6,
        0,
        -2,
        -8,
        -4,
        0,
        -6,
        -4,
        0,
        0,
        -10,
        -8,
        -4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same (4/4)"
  )

  # Final stats for other effects
  stat <- preproData$initial_stats[,, 2]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[2]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        1,
        0,
        0,
        1,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same count"
  )
  stat <- preproData$initial_stats[,, 3]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[3]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        0,
        1,
        0,
        0,
        1,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix same presence"
  )
  stat <- preproData$initial_stats[,, 5]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[5]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        6,
        6,
        0,
        2,
        8,
        4,
        0,
        6,
        0,
        0,
        0,
        10,
        8,
        4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix diff mean"
  )
  stat <- preproData$initial_stats[,, 6]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[6]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        2,
        6,
        0,
        2,
        8,
        4,
        0,
        6,
        4,
        0,
        0,
        10,
        8,
        4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix diff min"
  )
  stat <- preproData$initial_stats[,, 7]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[7]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        10,
        6,
        0,
        2,
        8,
        4,
        0,
        6,
        4,
        0,
        0,
        10,
        8,
        4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix diff max"
  )
  stat <- preproData$initial_stats[,, 9]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[9]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -6,
        -6,
        0,
        -2,
        -8,
        -4,
        0,
        -6,
        0,
        0,
        0,
        -10,
        -8,
        -4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix sim mean"
  )
  stat <- preproData$initial_stats[,, 10]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[10]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -2,
        -6,
        0,
        -2,
        -8,
        -4,
        0,
        -6,
        -4,
        0,
        0,
        -10,
        -8,
        -4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix sim min"
  )
  stat <- preproData$initial_stats[,, 11]
  for (t in 1:4) {
    change <- preproData$dependent_stats_change[[t]][[11]]
    stat <- updFun(stat, change)
  }
  expect_equal(
    stat,
    matrix(
      c(
        0,
        -10,
        -6,
        0,
        -2,
        -8,
        -4,
        0,
        -6,
        -4,
        0,
        0,
        -10,
        -8,
        -4,
        0
      ),
      4,
      4,
      TRUE
    ),
    label = "update of the statistics matrix sim max"
  )
})

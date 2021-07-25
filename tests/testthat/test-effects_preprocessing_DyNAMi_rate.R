# context("Effects for model = 'DyNAMi' and subModel = 'rate'")


# test interecept leaving ----
test_that(
  "intercept computes correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~ 1 + intercept(interaction_network_DyNAMi, joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    expect_equal(preproData$initialStats[, , 1],
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "initialization of the statistics vector intercept"
    )


    # Detail
    stat <- preproData$initialStats[, , 1]
    change <- preproData$dependentStatsChange[[2]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics vector intercept (8/8)"
    )
  }
)


# test inertia with different subtypes ----
test_that(
  "inertia/tie compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      inertia(past_network_DyNAMi, weighted = TRUE, subType = "count", joining = -1) +
        tie(covnetwork_DyNAMi, weighted = TRUE, subType = "count", joining = -1) +
        inertia(past_network_DyNAMi, weighted = TRUE, subType = "proportion", joining = -1) +
        tie(covnetwork_DyNAMi, weighted = TRUE, subType = "proportion", joining = -1) +
        inertia(past_network_DyNAMi, weighted = TRUE, subType = "presence", joining = -1) +
        tie(covnetwork_DyNAMi, weighted = TRUE, subType = "presence", joining = -1) +
        inertia(past_network_DyNAMi, weighted = TRUE, subType = "min", joining = -1) +
        tie(covnetwork_DyNAMi, weighted = TRUE, subType = "min", joining = -1) +
        inertia(past_network_DyNAMi, weighted = TRUE, subType = "mean", joining = -1) +
        tie(covnetwork_DyNAMi, weighted = TRUE, subType = "mean", joining = -1) +
        inertia(past_network_DyNAMi, weighted = TRUE, subType = "max", joining = -1) +
        tie(covnetwork_DyNAMi, weighted = TRUE, subType = "max", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in c(1, 3, 5, 7, 9, 11)) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix inertia"
      )
    }
    for (i in c(2, 4, 6, 8, 10, 12)) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix tie"
      )
    }


    # Detail for inertia/count
    stat <- preproData$initialStats[, , 1]
    change <- preproData$dependentStatsChange[[2]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (8/8)"
    )

    # Detail for tie/count
    stat <- preproData$initialStats[, , 2]
    change <- preproData$dependentStatsChange[[2]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix tie (8/8)"
    )



    # Final stats for other inertia effects: proportion, presence, min, mean, max
    for (i in c(3, 5, 7, 9, 11)) {
      stat <- preproData$initialStats[, , i]
      for (t in 1:8) {
        change <- preproData$dependentStatsChange[[t]][[i]]
        stat <- updFun(stat, change)
      }
      expect_equal(stat,
        matrix(c(
          0, 0, 0, 0,
          1, 1, 1, 1,
          0, 0, 0, 0,
          1, 1, 1, 1
        ), 4, 4, TRUE),
        label = "update of the statistics matrix inertia all subtypes"
      )
    }

    # Final stats for other tie effects: proportion, presence, min, mean, mean
    for (i in c(4, 6, 8, 10, 12)) {
      stat <- preproData$initialStats[, , i]
      for (t in 1:8) {
        change <- preproData$dependentStatsChange[[t]][[i]]
        stat <- updFun(stat, change)
      }
      expect_equal(stat,
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "update of the statistics matrix tie all subtypes"
      )
    }
  }
)


# test inertia with windows ----
test_that(
  "inertia computes correct preprocessing objects with window",
  {
    skip_on_ci()
    skip_on_cran()
    skip_on_covr()
    skip_on_bioc()
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      inertia(past_network_DyNAMi, weighted = TRUE, subType = "count", joining = -1)
      + inertia(past_network_DyNAMi, weighted = TRUE, subType = "count", joining = -1, window = 2)
        + inertia(past_network_DyNAMi, weighted = TRUE, subType = "count", joining = -1, window = 7),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in c(1, 2, 3)) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix inertia"
      )
    }

    # Detail for inertia no window
    stat <- preproData$initialStats[, , 1]
    change <- preproData$dependentStatsChange[[2]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (8/8)"
    )

    # Detail for inertia with window = 2
    stat <- preproData$initialStats[, , 2]
    change <- preproData$dependentStatsChange[[2]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (8/8)"
    )


    # Detail for inertia with window = 7
    stat <- preproData$initialStats[, , 3]
    change <- preproData$dependentStatsChange[[2]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix inertia (8/8)"
    )
  }
)


# test egopop with different subtypes ----
test_that(
  "egopop/egodeg for joining and leaving compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      egopop(past_network_DyNAMi, weighted = TRUE, subType = "identity", joining = 1) +
        egodeg(covnetwork_DyNAMi, weighted = TRUE, subType = "identity", joining = 1) +
        egopop(past_network_DyNAMi, weighted = TRUE, subType = "normalized", joining = 1) +
        egodeg(covnetwork_DyNAMi, weighted = TRUE, subType = "normalized", joining = 1) +
        egopop(past_network_DyNAMi, weighted = TRUE, subType = "identity", joining = -1) +
        egodeg(covnetwork_DyNAMi, weighted = TRUE, subType = "identity", joining = -1) +
        egopop(past_network_DyNAMi, weighted = TRUE, subType = "normalized", joining = -1) +
        egodeg(covnetwork_DyNAMi, weighted = TRUE, subType = "normalized", joining = -1) +
        egopop(past_network_DyNAMi, weighted = TRUE, subType = "centered", joining = 1) +
        egodeg(covnetwork_DyNAMi, weighted = TRUE, subType = "centered", joining = 1) +
        egopop(past_network_DyNAMi, weighted = TRUE, subType = "centered", joining = -1) +
        egodeg(covnetwork_DyNAMi, weighted = TRUE, subType = "centered", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in c(1, 3, 5, 6, 7, 8, 9, 11, 12)) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix egopop"
      )
    }
    expect_equal(preproData$initialStats[, , 2],
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "initialization of the statistics matrix egopop"
    )
    for (i in c(4, 10)) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0.5, 0.5, 0.5, 0.5,
          0.5, 0.5, 0.5, 0.5,
          0.5, 0.5, 0.5, 0.5,
          -1.5, -1.5, -1.5, -1.5
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix egopop"
      )
    }


    # Detail for egopop/joining/identity
    stat <- preproData$initialStats[, , 1]
    change <- preproData$dependentStatsChange[[2]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        3, 3, 3, 3,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        3, 3, 3, 3,
        0, 0, 0, 0,
        3, 3, 3, 3,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        4, 4, 4, 4,
        0, 0, 0, 0,
        4, 4, 4, 4,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (8/8)"
    )


    # Detail for egopop/leaving/identity
    stat <- preproData$initialStats[, , 5]
    change <- preproData$dependentStatsChange[[2]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        3, 3, 3, 3,
        3, 3, 3, 3,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        3, 3, 3, 3,
        0, 0, 0, 0,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        4, 4, 4, 4,
        3, 3, 3, 3,
        4, 4, 4, 4,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[5]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        3, 3, 3, 3,
        0, 0, 0, 0,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (8/8)"
    )

    # Detail for egodeg/joining/identity
    stat <- preproData$initialStats[, , 2]
    change <- preproData$dependentStatsChange[[2]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        0, 0, 0, 0,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        0, 0, 0, 0,
        2, 2, 2, 2,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg (8/8)"
    )


    # Final stats for other effects
    stat <- preproData$initialStats[, , 3]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[3]]
      stat <- updFun(stat, change)
    }
    v <- (4 - 3.5) / sd(c(3, 3, 4, 4))
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop"
    )
    stat <- preproData$initialStats[, , 4]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[4]]
      stat <- updFun(stat, change)
    }
    v <- (2 - 1.5) / sd(c(2, 2, 2, 0))
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg"
    )
    stat <- preproData$initialStats[, , 6]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[6]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        2, 2, 2, 2,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg"
    )
    stat <- preproData$initialStats[, , 7]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[7]]
      stat <- updFun(stat, change)
    }
    v <- (3 - 3.5) / sd(c(3, 3, 4, 4))
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop"
    )
    stat <- preproData$initialStats[, , 8]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[8]]
      stat <- updFun(stat, change)
    }
    v <- (2 - 1.5) / sd(c(2, 2, 2, 0))
    v2 <- (0 - 1.5) / sd(c(2, 2, 2, 0))
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg"
    )
    stat <- preproData$initialStats[, , 9]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[9]]
      stat <- updFun(stat, change)
    }
    v <- (4 - 3.5)
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop"
    )
    stat <- preproData$initialStats[, , 10]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[10]]
      stat <- updFun(stat, change)
    }
    v <- (2 - 1.5)
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg"
    )
    stat <- preproData$initialStats[, , 11]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[11]]
      stat <- updFun(stat, change)
    }
    v <- (3 - 3.5)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop"
    )
    stat <- preproData$initialStats[, , 12]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[12]]
      stat <- updFun(stat, change)
    }
    v <- (2 - 1.5)
    v2 <- (0 - 1.5)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egodeg"
    )
  }
)


# test alterpop with different subtypes ----
test_that(
  "alterpop/alterdeg for leaving compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      alterpop(past_network_DyNAMi, weighted = TRUE, subType = "mean", joining = -1) +
        alterdeg(covnetwork_DyNAMi, weighted = TRUE, subType = "mean", joining = -1) +
        alterpop(past_network_DyNAMi, weighted = TRUE, subType = "mean_normalized", joining = -1) +
        alterdeg(covnetwork_DyNAMi, weighted = TRUE, subType = "mean_normalized", joining = -1) +
        alterpop(past_network_DyNAMi, weighted = TRUE, subType = "min", joining = -1) +
        alterdeg(covnetwork_DyNAMi, weighted = TRUE, subType = "min", joining = -1) +
        alterpop(past_network_DyNAMi, weighted = TRUE, subType = "max", joining = -1) +
        alterdeg(covnetwork_DyNAMi, weighted = TRUE, subType = "max", joining = -1) +
        alterpop(past_network_DyNAMi, weighted = TRUE, subType = "mean_centered", joining = -1) +
        alterdeg(covnetwork_DyNAMi, weighted = TRUE, subType = "mean_centered", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in 1:8) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix alterpop/alterdeg"
      )
    }


    # Detail for alterpop mean normalized
    stat <- preproData$initialStats[, , 3]
    change <- preproData$dependentStatsChange[[2]][[3]]
    stat <- updFun(stat, change)
    v <- (1 - 0.5) / sd(c(0, 0, 1, 1))
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        v, v, v, v,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[3]]
    stat <- updFun(stat, change)
    v <- (1 - mean(c(0, 1, 1, 1))) / sd(c(0, 1, 1, 1))
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        v, v, v, v,
        v, v, v, v,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[3]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix egopop (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[3]]
    stat <- updFun(stat, change)
    v1 <- (2 - 1.5) / sd(c(1, 1, 2, 2))
    v2 <- (1 - 1.5) / sd(c(1, 1, 2, 2))
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v1, v1, v1, v1,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[3]]
    stat <- updFun(stat, change)
    v <- (1 - 1.5) / sd(c(1, 1, 2, 2))
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop (8/8)"
    )


    # Detail for alterdeg mean normalized
    stat <- preproData$initialStats[, , 4]
    change <- preproData$dependentStatsChange[[2]][[4]]
    stat <- updFun(stat, change)
    v <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        v, v, v, v,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[4]]
    stat <- updFun(stat, change)
    v <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        v, v, v, v,
        v, v, v, v,
        v, v, v, v,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[4]]
    stat <- updFun(stat, change)
    v1 <- (mean(c(0, 2, 2)) - 1.5) / 1
    v2 <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        v1, v1, v1, v1,
        v1, v1, v1, v1,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[4]]
    stat <- updFun(stat, change)
    v1 <- (mean(c(0, 2)) - 1.5) / 1
    v2 <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        v1, v1, v1, v1,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[4]]
    stat <- updFun(stat, change)
    v1 <- (0 - 1.5) / 1
    v2 <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[4]]
    stat <- updFun(stat, change)
    v1 <- (0 - 1.5) / 1
    v2 <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        v2, v2, v2, v2,
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[4]]
    stat <- updFun(stat, change)
    v1 <- (0 - 1.5) / 1
    v2 <- (2 - 1.5) / 1
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg (8/8)"
    )


    # Final stats for other effects
    stat <- preproData$initialStats[, , 1]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[1]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        3, 3, 3, 3,
        0, 0, 0, 0,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop"
    )
    stat <- preproData$initialStats[, , 2]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[2]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        2, 2, 2, 2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg"
    )
    stat <- preproData$initialStats[, , 5]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[5]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        3, 3, 3, 3,
        0, 0, 0, 0,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop"
    )
    stat <- preproData$initialStats[, , 6]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[6]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        2, 2, 2, 2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg"
    )
    stat <- preproData$initialStats[, , 7]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[7]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        3, 3, 3, 3,
        0, 0, 0, 0,
        3, 3, 3, 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop"
    )
    stat <- preproData$initialStats[, , 8]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[8]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        2, 2, 2, 2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg"
    )
    stat <- preproData$initialStats[, , 9]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[9]]
      stat <- updFun(stat, change)
    }
    v <- (1 - 1.5)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v, v, v, v,
        0, 0, 0, 0,
        v, v, v, v
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterpop"
    )
    stat <- preproData$initialStats[, , 10]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[10]]
      stat <- updFun(stat, change)
    }
    v1 <- (0 - 1.5)
    v2 <- (2 - 1.5)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alterdeg"
    )
  }
)


# test ego with different subtypes ----
test_that(
  "ego for joining and leaving compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      ego(actors_DyNAMi$attr1, subType = "identity", joining = 1) +
        ego(actors_DyNAMi$attr1, subType = "identity", joining = -1) +
        ego(actors_DyNAMi$attr1, subType = "normalized", joining = 1) +
        ego(actors_DyNAMi$attr1, subType = "normalized", joining = -1) +
        ego(actors_DyNAMi$attr1, subType = "squared", joining = 1) +
        ego(actors_DyNAMi$attr1, subType = "squared", joining = -1) +
        ego(actors_DyNAMi$attr1, subType = "centered", joining = 1) +
        ego(actors_DyNAMi$attr1, subType = "centered", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in c(2, 4, 6, 8)) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix ego"
      )
    }
    expect_equal(preproData$initialStats[, , 1],
      matrix(c(
        20, 20, 20, 20,
        22, 22, 22, 22,
        26, 26, 26, 26,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "initialization of the statistics matrix ego"
    )
    v1 <- (20 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (22 - 24.5) / sd(c(20, 22, 26, 30))
    v3 <- (26 - 24.5) / sd(c(20, 22, 26, 30))
    v4 <- (30 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(preproData$initialStats[, , 3],
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v3, v3, v3, v3,
        v4, v4, v4, v4
      ), 4, 4, TRUE),
      label = "initialization of the statistics matrix ego"
    )
    expect_equal(preproData$initialStats[, , 5],
      matrix(c(
        400, 400, 400, 400,
        484, 484, 484, 484,
        676, 676, 676, 676,
        900, 900, 900, 900
      ), 4, 4, TRUE),
      label = "initialization of the statistics matrix ego"
    )
    v1 <- (20 - 24.5)
    v2 <- (22 - 24.5)
    v3 <- (26 - 24.5)
    v4 <- (30 - 24.5)
    expect_equal(preproData$initialStats[, , 7],
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v3, v3, v3, v3,
        v4, v4, v4, v4
      ), 4, 4, TRUE),
      label = "initialization of the statistics matrix ego"
    )


    # Detail for ego identity joining
    stat <- preproData$initialStats[, , 1]
    change <- preproData$dependentStatsChange[[2]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        26, 26, 26, 26,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        0, 0, 0, 0,
        26, 26, 26, 26,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        0, 0, 0, 0,
        26, 26, 26, 26,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (8/8)"
    )


    # Detail for ego identity leaving
    stat <- preproData$initialStats[, , 2]
    change <- preproData$dependentStatsChange[[2]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        22, 22, 22, 22,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        22, 22, 22, 22,
        26, 26, 26, 26,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        22, 22, 22, 22,
        26, 26, 26, 26,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        22, 22, 22, 22,
        26, 26, 26, 26,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        22, 22, 22, 22,
        0, 0, 0, 0,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        20, 20, 20, 20,
        22, 22, 22, 22,
        26, 26, 26, 26,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        22, 22, 22, 22,
        0, 0, 0, 0,
        30, 30, 30, 30
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego (8/8)"
    )


    # Final stats for other effects
    v1 <- (20 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (22 - 24.5) / sd(c(20, 22, 26, 30))
    v3 <- (26 - 24.5) / sd(c(20, 22, 26, 30))
    v4 <- (30 - 24.5) / sd(c(20, 22, 26, 30))
    stat <- preproData$initialStats[, , 3]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[3]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v3, v3, v3, v3,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego"
    )
    stat <- preproData$initialStats[, , 4]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[4]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v2, v2, v2, v2,
        0, 0, 0, 0,
        v4, v4, v4, v4
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego"
    )
    stat <- preproData$initialStats[, , 5]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[5]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        400, 400, 400, 400,
        0, 0, 0, 0,
        676, 676, 676, 676,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego"
    )
    stat <- preproData$initialStats[, , 6]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[6]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        484, 484, 484, 484,
        0, 0, 0, 0,
        900, 900, 900, 900
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego"
    )
    v1 <- (20 - 24.5)
    v2 <- (22 - 24.5)
    v3 <- (26 - 24.5)
    v4 <- (30 - 24.5)
    stat <- preproData$initialStats[, , 7]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[7]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v3, v3, v3, v3,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego"
    )
    stat <- preproData$initialStats[, , 8]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[8]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v2, v2, v2, v2,
        0, 0, 0, 0,
        v4, v4, v4, v4
      ), 4, 4, TRUE),
      label = "update of the statistics matrix ego"
    )
  }
)


# test alter with different subtypes ----
test_that(
  "alter for leaving compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      alter(actors_DyNAMi$attr1, subType = "mean", joining = -1) +
        alter(actors_DyNAMi$attr1, subType = "mean_squared", joining = -1) +
        alter(actors_DyNAMi$attr1, subType = "mean_normalized", joining = -1) +
        alter(actors_DyNAMi$attr1, subType = "min", joining = -1) +
        alter(actors_DyNAMi$attr1, subType = "max", joining = -1) +
        alter(actors_DyNAMi$attr1, subType = "mean_centered", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in 1:6) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix alterpop/alterdeg"
      )
    }


    # Detail for alter mean normalized
    stat <- preproData$initialStats[, , 3]
    change <- preproData$dependentStatsChange[[2]][[3]]
    stat <- updFun(stat, change)
    v1 <- (22 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (20 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[3]]
    stat <- updFun(stat, change)
    v1 <- (24 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (23 - 24.5) / sd(c(20, 22, 26, 30))
    v3 <- (21 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v3, v3, v3, v3,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[3]]
    stat <- updFun(stat, change)
    v1 <- (mean(c(22, 26, 30)) - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (mean(c(20, 26, 30)) - 24.5) / sd(c(20, 22, 26, 30))
    v3 <- (mean(c(20, 22, 30)) - 24.5) / sd(c(20, 22, 26, 30))
    v4 <- (mean(c(20, 22, 26)) - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v3, v3, v3, v3,
        v4, v4, v4, v4
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[3]]
    stat <- updFun(stat, change)
    v1 <- (28 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (26 - 24.5) / sd(c(20, 22, 26, 30))
    v3 <- (24 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v3, v3, v3, v3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[3]]
    stat <- updFun(stat, change)
    v1 <- (30 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (22 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[3]]
    stat <- updFun(stat, change)
    v1 <- (26 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (30 - 24.5) / sd(c(20, 22, 26, 30))
    v3 <- (20 - 24.5) / sd(c(20, 22, 26, 30))
    v4 <- (22 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        v1, v1, v1, v1,
        v2, v2, v2, v2,
        v3, v3, v3, v3,
        v4, v4, v4, v4
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[3]]
    stat <- updFun(stat, change)
    v1 <- (30 - 24.5) / sd(c(20, 22, 26, 30))
    v2 <- (22 - 24.5) / sd(c(20, 22, 26, 30))
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter (8/8)"
    )


    # Final stats for other effects
    stat <- preproData$initialStats[, , 1]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[1]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        30, 30, 30, 30,
        0, 0, 0, 0,
        22, 22, 22, 22
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter"
    )
    stat <- preproData$initialStats[, , 2]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[2]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        900, 900, 900, 900,
        0, 0, 0, 0,
        484, 484, 484, 484
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter"
    )
    stat <- preproData$initialStats[, , 4]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[4]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        30, 30, 30, 30,
        0, 0, 0, 0,
        22, 22, 22, 22
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter"
    )
    stat <- preproData$initialStats[, , 5]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[5]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        30, 30, 30, 30,
        0, 0, 0, 0,
        22, 22, 22, 22
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter"
    )
    stat <- preproData$initialStats[, , 6]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[6]]
      stat <- updFun(stat, change)
    }
    v1 <- (30 - 24.5)
    v2 <- (22 - 24.5)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        v1, v1, v1, v1,
        0, 0, 0, 0,
        v2, v2, v2, v2
      ), 4, 4, TRUE),
      label = "update of the statistics matrix alter"
    )
  }
)


# test same with different subtypes ----
test_that(
  "same for leaving compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      same(actors_DyNAMi$attr2, subType = "count", joining = -1) +
        same(actors_DyNAMi$attr2, subType = "proportion", joining = -1) +
        same(actors_DyNAMi$attr2, subType = "presence", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in 1:3) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix same"
      )
    }


    # Detail for same proportion
    stat <- preproData$initialStats[, , 2]
    change <- preproData$dependentStatsChange[[2]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0.5, 0.5, 0.5, 0.5,
        0, 0, 0, 0,
        0.5, 0.5, 0.5, 0.5,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1 / 3, 1 / 3, 1 / 3, 1 / 3,
        1 / 3, 1 / 3, 1 / 3, 1 / 3,
        1 / 3, 1 / 3, 1 / 3, 1 / 3,
        1 / 3, 1 / 3, 1 / 3, 1 / 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        0.5, 0.5, 0.5, 0.5,
        0, 0, 0, 0,
        0.5, 0.5, 0.5, 0.5
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[2]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same (8/8)"
    )


    # Final stats for other effects
    stat <- preproData$initialStats[, , 1]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[1]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same"
    )
    stat <- preproData$initialStats[, , 2]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[2]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1, 1, 1
      ), 4, 4, TRUE),
      label = "update of the statistics matrix same"
    )
  }
)


# test diff with different subtypes ----
test_that(
  "diff for leaving compute correct preprocessing objects weighted with all possible options",
  {
    preproData <- estimate(
      dependent.depevents_DyNAMi ~
      diff(actors_DyNAMi$attr1, subType = "averaged_sum", joining = -1) +
        diff(actors_DyNAMi$attr1, subType = "mean", joining = -1) +
        diff(actors_DyNAMi$attr1, subType = "min", joining = -1) +
        diff(actors_DyNAMi$attr1, subType = "max", joining = -1),
      model = "DyNAMi", subModel = "rate",
      preprocessingOnly = TRUE, silent = TRUE
    )

    updFun <- function(stat, change) {
      if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      return(stat)
    }

    for (i in 1:3) {
      expect_equal(preproData$initialStats[, , i],
        matrix(c(
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0
        ), 4, 4, TRUE),
        label = "initialization of the statistics matrix diff"
      )
    }


    # Detail for diff averaged_sum
    stat <- preproData$initialStats[, , 1]
    change <- preproData$dependentStatsChange[[2]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        2, 2, 2, 2,
        2, 2, 2, 2,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (2/8)"
    )
    change <- preproData$dependentStatsChange[[3]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        4, 4, 4, 4,
        3, 3, 3, 3,
        5, 5, 5, 5,
        0, 0, 0, 0
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (3/8)"
    )
    change <- preproData$dependentStatsChange[[4]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        6, 6, 6, 6,
        14 / 3, 14 / 3, 14 / 3, 14 / 3,
        14 / 3, 14 / 3, 14 / 3, 14 / 3,
        22 / 3, 22 / 3, 22 / 3, 22 / 3
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (4/8)"
    )
    change <- preproData$dependentStatsChange[[5]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        6, 6, 6, 6,
        4, 4, 4, 4,
        6, 6, 6, 6
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (5/8)"
    )
    change <- preproData$dependentStatsChange[[6]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        8, 8, 8, 8,
        0, 0, 0, 0,
        8, 8, 8, 8
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (6/8)"
    )
    change <- preproData$dependentStatsChange[[7]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        6, 6, 6, 6,
        8, 8, 8, 8,
        6, 6, 6, 6,
        8, 8, 8, 8
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (7/8)"
    )
    change <- preproData$dependentStatsChange[[8]][[1]]
    stat <- updFun(stat, change)
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        8, 8, 8, 8,
        0, 0, 0, 0,
        8, 8, 8, 8
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff (8/8)"
    )


    # Final stats for other effects
    stat <- preproData$initialStats[, , 2]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[2]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        8, 8, 8, 8,
        0, 0, 0, 0,
        8, 8, 8, 8
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff"
    )
    stat <- preproData$initialStats[, , 3]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[3]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        8, 8, 8, 8,
        0, 0, 0, 0,
        8, 8, 8, 8
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff"
    )
    stat <- preproData$initialStats[, , 4]
    for (t in 1:8) {
      change <- preproData$dependentStatsChange[[t]][[4]]
      stat <- updFun(stat, change)
    }
    expect_equal(stat,
      matrix(c(
        0, 0, 0, 0,
        8, 8, 8, 8,
        0, 0, 0, 0,
        8, 8, 8, 8
      ), 4, 4, TRUE),
      label = "update of the statistics matrix diff"
    )
  }
)

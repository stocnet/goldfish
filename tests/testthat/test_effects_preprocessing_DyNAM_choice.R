context("Effects for model = 'DyNAM' and subModel = 'choice'")
# not needed new testthat version

# # ToDo: check output of estimation: ignoreRep
# #       check decrements, delete not existing ties


# inertia/tie weighted----
test_that("inertia/tie compute correct preprocessing objects weighted", {
  #
  preproData <- estimate(depNetwork ~ inertia(networkState, weighted = TRUE) + tie(networkExog, weighted = TRUE),
    model = "DyNAM", subModel = "choice", # modelType = "DyNAM-M"
    preprocessingOnly = TRUE, silent = TRUE
  )
  outDependetStatChange <- goldfish:::ReducePreprocess(preproData, type = "withTime")
  expect_equivalent(preproData$initialStats[, , 1],
    matrix(c(
      0, 3, 0, 0, 0,
      1, 0, 1, 1, 0,
      0, 0, 0, 1, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0
    ), 5, 5, TRUE),
    label = "initialization of the statistics matrix"
  )
  expect_equivalent(preproData$initialStats[, , 2],
    matrix(c(
      0, 0, 0, 1, 0,
      0, 0, 0, 0, 0,
      0, 2, 0, 0, 0,
      1, 0, 0, 0, 0,
      1, 2, 0, 0, 0
    ), 5, 5, TRUE),
    label = "initialization of the statistics matrix"
  )
  expect_equivalent(outDependetStatChange[[1]][, c("node1", "node2", "replace")],
    cbind(
      node1 = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
      node2 = c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2),
      replace = c(4, 2, 2, 3, 1, 2, 2, 3, 1, 4, 1)
    ),
    label = "updating with increment works"
  ) # n-1 updates
  expect_equivalent(outDependetStatChange[[2]],
    cbind(
      time = c(9, 15, 16, 19, 19, 28, 28),
      node1 = c(4, 2, 5, 4, 4, 1, 3),
      node2 = c(2, 3, 1, 2, 5, 3, 5),
      replace = c(1, 1, 4, 0, 1, 2, 3)
    ),
    label = "updating with increment works"
  ) # n-1 updates
})

# inertia no-weighted----
test_that("inertia compute correct preprocessing objects no weighted", {
  # weighted = FALSE
  preproData <- estimate(depNetwork ~ inertia,
    model = "DyNAM", subModel = "choice",
    preprocessingOnly = TRUE, silent = TRUE
  )
  expect_equivalent(preproData$initialStats[, , 1],
    matrix(c(
      0, 1, 0, 0, 0,
      1, 0, 1, 1, 0,
      0, 0, 0, 1, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0
    ), 5, 5, TRUE),
    label = "initialization of the statistics matrix"
  )
  expect_equivalent(Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
    cbind(
      node1 = c(3, 5, 1, 4, 5),
      node2 = c(2, 1, 5, 2, 2),
      replace = c(1, 1, 1, 1, 1)
    ),
    label = "updating with increment works"
  ) # n-1 updates
})

# # add expected for eventsIncrement_2
# inertia window/weighted----
test_that("inertia compute correct preprocessing objects windowed and weighted", {
  # weighted = TRUE
  preproData <- estimate(depNetwork ~ inertia(networkState, weighted = TRUE, window = 2),
    model = "DyNAM", subModel = "choice",
    preprocessingOnly = TRUE, silent = TRUE
  )

  outDependetStatChange <- goldfish:::ReducePreprocess(preproData, type = "withTime")
  expect_equivalent(preproData$initialStats[, , 1],
    matrix(0, 5, 5, TRUE),
    label = "initialization of the statistics matrix"
  )
  expect_equivalent(outDependetStatChange[[1]],
    cbind(
      time =    c(6, 9, 13, 15, 16, 16, 19, 19, 23, 28, 29, 32, 32, 36),
      node1 =   c(1, 3,  2,  2,  2,  5,  1,  5,  3,  3,  4,  2,  4,  5),
      node2 =   c(2, 2,  3,  3,  3,  1,  5,  1,  4,  4,  2,  3,  2,  2),
      replace = c(0, 0,  0,  1,  0,  1,  0,  0,  0,  0,  1,  0,  0,  0)
    ),
    label = "updating with increment works"
  ) # n-1 updates
})

# transitivity unweighted----
test_that("transitivity compute correct preprocessing objects unweighted", {
  # weighted = FALSE
  preproData <- estimate(depNetwork ~ trans,
                         model = "DyNAM", subModel = "choice",
                         preprocessingOnly = TRUE, silent = TRUE
  )
  # This function orders the events by node 1, node 2 (within each time point, not between time points)
  outDependentStatChange <- goldfish:::ReducePreprocess(preproData, type = "withoutTime")
  expect_equivalent(preproData$initialStats[, , 1],
                    matrix(c(1, 0, 1, 1, 0,
                             0, 1, 1, 1, 0, # !!! NO SELF TIES
                             0, 0, 1, 0, 0,
                             0, 0, 0, 1, 0,
                             0, 0, 0, 0, 0), 5, 5, TRUE),
                    label = "initialization of the statistics matrix")
  expect_equivalent(outDependentStatChange[[1]],
                    cbind(node1 =   c(2, 3, 3, 3, 4, 5, 1, 2, 5, 2, 3, 4, 4, 4, 1, 5, 5, 5),
                          node2 =   c(2, 1, 3, 4, 2, 2, 1, 5, 5, 2, 2, 1, 3, 4, 2, 1, 3, 4),
                          replace = c(2, 1, 2, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1, 2, 1, 1, 1, 1)),
                    label = "updating with increment works") # n-1 updates
}) # !!!!! NORMALLY THERE SHOULDN'T BE ANY SELF TIES. However the preprocessing calculates them,
# but they are ignored in the estimation process. Hence, the testthat is constructed with self ties
# (although they normallyshouldn't be included).

# transitivity windowed----
test_that("transitivity compute correct preprocessing objects windowed", {
  preproData <- estimate(depNetwork ~ trans(networkState, window = 20),
                        model = "DyNAM", subModel = "choice",
                        preprocessingOnly = TRUE, silent = TRUE)

  outDependetStatChange <- goldfish:::ReducePreprocess(preproData, type = "withTime")
  expect_equivalent(preproData$initialStats[, , 1],
                    matrix(c(0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, # !!! NO SELF TIES
                             0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0), 5, 5, TRUE),
                    label = "initialization of the statistics matrix")
  expect_equivalent(outDependetStatChange[[1]],
                    cbind(time =    c(13, 13, 13, 16, 19, 19, 23, 23, 23, 28, 28, 29, 29, 36, 36, 36, 36),
                          node1 =   c(1, 2, 3, 5, 1, 5, 1, 2, 5, 2, 3, 3, 4, 1, 1, 5, 5),
                          node2 =   c(3, 2, 3, 2, 1, 5, 3, 4, 2, 2, 3, 2, 3, 1, 2, 3, 5),
                          replace = c(1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0)),
                    label = "updating with increment works") # n-1 updates
  # In this setting I ignored the repetition at time = 28, node1 = 2, node2 = 4, replace = 2;
  # time = 32, node1 = 2, node2 = 4, replace = 3.
})

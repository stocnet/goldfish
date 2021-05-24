# # inertia/tie weighted ----
test_that(
  "inertia/tie compute correct preprocessing objects weighted",
  {
    preproData <- estimate(
      depNetwork ~ inertia(networkState, weighted = TRUE) + tie(networkExog, weighted = TRUE),
      model = "DyNAM", subModel = "choice", # modelType = "DyNAM-M"
      preprocessingOnly = TRUE, silent = TRUE
    )
    outDependetStatChange <- Reduce(rbind, mapply(
      function(x, y, effectPos) {
        if (is.null(x[[effectPos]])) {
          return(NULL)
        } # no changes, no problem
        if (nrow(x[[effectPos]]) == 1) {
          return(cbind(time = y, x[[effectPos]]))
        } # just one update, no problem

        discard <- duplicated(x[[effectPos]][, c("node1", "node2")], fromLast = TRUE)
        changes <- cbind(time = y[1], x[[effectPos]][!discard, , drop = FALSE])
        if (nrow(changes) == 1) {
          return(changes)
        }
        # print(changes)
        changes <- changes[order(changes[, "node1"], changes[, "node2"]), ]
        return(changes) # multiple updates might be repeated, keep the last
      },
      preproData$dependentStatsChange, preproData$eventTime,
      effectPos = 2
    ))
    expect_equal(preproData$initialStats[, , 1],
      matrix(c(
        0, 3, 0, 0, 0,
        1, 0, 1, 1, 0,
        0, 0, 0, 1, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(preproData$initialStats[, , 2],
      matrix(c(
        0, 0, 0, 1, 0,
        0, 0, 0, 0, 0,
        0, 2, 0, 0, 0,
        1, 0, 0, 0, 0,
        1, 2, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      cbind(
        node1 = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
        node2 = c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2),
        replace = c(4, 2, 2, 3, 1, 2, 2, 3, 1, 4, 1)
      ),
      label = "updating with increment works"
    ) # n-1 updates
    expect_equal(outDependetStatChange,
      cbind(
        time = c(9, 15, 16, 19, 19, 28, 28),
        node1 = c(4, 2, 5, 4, 4, 1, 3),
        node2 = c(2, 3, 1, 2, 5, 3, 5),
        replace = c(1, 1, 4, 0, 1, 2, 3)
      ),
      label = "updating with increment works"
    ) # n-1 updates
  }
)

# inertia not weighted ----
test_that(
  "inertia compute correct preprocessing objects no weighted",
  {
    preproData <- estimate(
      depNetwork ~ inertia,
      model = "DyNAM", subModel = "choice",
      preprocessingOnly = TRUE, silent = TRUE
    )
    expect_equal(preproData$initialStats[, , 1],
      matrix(c(
        0, 1, 0, 0, 0,
        1, 0, 1, 1, 0,
        0, 0, 0, 1, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      cbind(
        node1 = c(3, 5, 1, 4, 5),
        node2 = c(2, 1, 5, 2, 2),
        replace = c(1, 1, 1, 1, 1)
      ),
      label = "updating with increment works"
    ) # n-1 updates
  }
)

# # inertia windowed size 2 weighted ----
test_that(
  "inertia compute correct preprocessing objects windowed and weighted",
  {
    skip_on_ci()
    skip_on_cran()
    skip_on_covr()
    skip_on_bioc()
    preproData <- estimate(depNetwork ~ inertia(networkState, weighted = TRUE, window = 2),
      model = "DyNAM", subModel = "choice",
      preprocessingOnly = TRUE, silent = TRUE
    )

    outDependetStatChange <- Reduce(rbind, mapply(
      function(x, y) {
        if (is.null(x[[1]])) {
          return(NULL)
        } # no changes, no problem
        if (nrow(x[[1]]) == 1) {
          return(cbind(time = y, x[[1]]))
        } # just one update, no problem

        discard <- duplicated(x[[1]][, c("node1", "node2")], fromLast = TRUE)
        changes <- cbind(time = y[1], x[[1]][!discard, , drop = FALSE])
        if (nrow(changes) == 1) {
          return(changes)
        }
        # print(changes)
        changes <- changes[order(changes[, "node1"], changes[, "node2"]), ]
        return(changes) # multiple updates might be repeated, keep the last
      },
      preproData$dependentStatsChange, preproData$eventTime
    ))
    expect_equal(preproData$initialStats[, , 1],
      matrix(0, 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(outDependetStatChange,
      cbind(
        time = c(6, 9, 13, 15, 16, 16, 19, 19, 23, 28, 29, 32, 32, 36),
        node1 = c(1, 3, 2, 2, 2, 5, 1, 5, 3, 3, 4, 2, 4, 5),
        node2 = c(2, 2, 3, 3, 3, 1, 5, 1, 4, 4, 2, 3, 2, 2),
        replace = c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
      ),
      label = "updating with increment works"
    ) # n-1 updates
  }
)

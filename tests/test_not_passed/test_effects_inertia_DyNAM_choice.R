#context("Effect inertia for model = 'DyNAM' and subModel = 'choice'")
# not needed nwe testthat version

# # ToDo: check output of estimation: ignoreRep

# defining objects
networkState <- defineNetwork(matrix = networkState, nodes = actors, directed = TRUE)
networkState <- linkEvents(x = networkState, changeEvent = eventsIncrement, nodes = actors)
depNetwork <- defineDependentEvents(events = eventsIncrement, nodes = actors, defaultNetwork = networkState)

# 
preproData <- estimate(depNetwork ~ inertia(networkState, weighted = TRUE),
                       model = "DyNAM", subModel = "choice", #modelType = "DyNAM-M"
                       preprocessingOnly = TRUE
)

# 
test_that("inertia compute correct preprocessing objects weighted", {
  expect_equivalent(preproData$initialStats[, , 1],
                    matrix(c(0, 3, 0, 0, 0,
                             1, 0, 1, 1, 0,
                             0, 0, 0, 1, 0,
                             0, 0, 1, 0, 0,
                             0, 0, 0, 0, 0), 5, 5, TRUE),
                    label = "initialization of the statistics matrix")
  expect_equivalent(Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
                    cbind(node1 =   c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
                          node2 =   c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2),
                          replace = c(4, 2, 2, 3, 1, 2, 2, 3, 1, 4, 1)),
                    label = "updating with increment works") # n-1 updates
})

# weighted = FALSE
preproData <- estimate(depNetwork ~ inertia,
                       model = "DyNAM", subModel = "choice",
                       preprocessingOnly = TRUE
)

# 
test_that("inertia compute correct preprocessing objects no weighted", {
  expect_equivalent(preproData$initialStats[, , 1],
                    matrix(c(0, 1, 0, 0, 0,
                             1, 0, 1, 1, 0,
                             0, 0, 0, 1, 0,
                             0, 0, 1, 0, 0,
                             0, 0, 0, 0, 0), 5, 5, TRUE),
                    label = "initialization of the statistics matrix")
  expect_equivalent(Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
                    cbind(node1 =   c(3, 5, 1, 4, 5),
                          node2 =   c(2, 1, 5, 2, 2),
                          replace = c(1, 1, 1, 1, 1)),
                    label = "updating with increment works") # n-1 updates
})

# weighted = TRUE
preproData <- estimate(depNetwork ~ inertia(networkState, weighted = TRUE, window = 2),
                       model = "DyNAM", subModel = "choice",
                       preprocessingOnly = TRUE
)

outDependetStatChange <- Reduce(rbind, mapply(
  function (x, y) {
    if (is.null(x[[1]])) return(NULL)  # no changes, no problem
    if (nrow(x[[1]]) == 1) return(cbind(time = y, x[[1]])) # just one update, no problem
    
    discard <- duplicated(x[[1]][, c("node1", "node2")], fromLast = TRUE)
    changes <- cbind(time= y[1], x[[1]][!discard, , drop = FALSE])
    if (nrow(changes) == 1) return(changes)
    # print(changes)
    changes <- changes[order(changes[, "node1"], changes[, "node2"]), ]
    return(changes) # multiple updates might be repeated, keep the last
  },
  preproData$dependentStatsChange, preproData$eventTime
))

# # add expected for eventsIncrement_2
test_that("inertia compute correct preprocessing objects windowed and weighted", {
  expect_equivalent(preproData$initialStats[, , 1],
                    matrix(c(0, 3, 0, 0, 0,
                             1, 0, 1, 1, 0,
                             0, 0, 0, 1, 0,
                             0, 0, 1, 0, 0,
                             0, 0, 0, 0, 0), 5, 5, TRUE),
                    label = "initialization of the statistics matrix")
  expect_equivalent(outDependetStatChange,
                    cbind(time =    c(6, 9, 13, 15, 16, 16, 19, 19, 23, 28, 29, 32, 32, 36),
                          node1 =   c(1, 3,  2,  2,  2,  5,  1,  5,  3,  3,  4,  2,  4,  5),
                          node2 =   c(2, 2,  3,  3,  3,  1,  5,  1,  4,  4,  2,  3,  2,  2),
                          replace = c(3, 0,  1,  2,  1,  1,  0,  0,  1,  1,  1,  1,  0,  0)),
                    label = "updating with increment works") # n-1 updates
})





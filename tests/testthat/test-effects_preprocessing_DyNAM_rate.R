test_that(
  "out/in/deg weighted right censored preprocessing",
  {
    preproData <- estimate(
      depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
        indeg(networkExog, weighted = TRUE),
      model = "DyNAM", subModel = "rate",
      preprocessingOnly = TRUE
    )
    statsChange <- ReducePreprocess(preproData)
    expect_equal(preproData$initialStats[, , 1],
      matrix(c(
        0, 3, 3, 3, 3,
        3, 0, 3, 3, 3,
        1, 1, 0, 1, 1,
        1, 1, 1, 0, 1,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "init outdeg stat matrix"
    )
    expect_equal(preproData$initialStats[, , 2],
      matrix(c(
        0, 2, 2, 2, 2,
        4, 0, 4, 4, 4,
        0, 0, 0, 0, 0,
        1, 1, 1, 0, 1,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "init indeg stat matrix"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      fillChanges(
        nodes   = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
        replace = c(4, 3, 4, 5, 1, 6, 4, 5, 2, 6, 2),
        time = NULL, set = 1:5),
      label = "updating outdeg with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[1]][["dependent"]],
      fillChanges(
        nodes   = c(1, 3,  2,  2,  5,  1,  3,  3,  4,  2,  5),
        replace = c(4, 3,  4,  5,  1,  6,  4,  5,  2,  6,  2),
        time    = c(6, 9, 13, 15, 16, 19, 23, 28, 29, 32, 36),
        set = 1:5),
      label = "updating outdeg times with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[2]][["dependent"]],
      fillChanges(
        nodes   = c(2,  3,  1,  2,  5,  3,  5),
        replace = c(5,  1,  5,  4,  1,  3,  4),
        time    = c(9, 15, 16, 19, 19, 28, 28),
        set = 1:5),
      label = "updating indeg times with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[1]][["rightCensored"]],
      fillChanges(
        nodes   = c(3,  2,  1,  3),
        replace = c(3,  5,  6,  5),
        time    = c(7, 14, 18, 25),
        set = 1:5),
      label = "updating outdeg times right censored"
    ) # n-1 updates
    expect_null(
      statsChange[[2]][["rightCensored"]],
      label = "updating indeg times right censored"
    )
    expect_equal(
      preproData$intervals,
      c(0, 5, 2, 4, 1, 1, 1, 4, 3, 1, 3, 4),
      label = "intervals dependent"
    )
    expect_equal(
      preproData$rightCensoredIntervals,
      c(1, 1, 2, 2),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      c(1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, rep(1, 4)),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      c(eventsIncrement$time, eventsExogenous$time) |> unique() |> sort(),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      c(1, 3, 4, rep(2, 3), 5, 1, 4, 3, 3, 1, 4, 2, 5, 1),
      label = "sender events"
    ) # ties just firts
    expect_equal(
      preproData$eventReceiver,
      c(rep(c(2, 3), each = 3), 1, 5, 5, 4, 4, 3, 2, 3, 2, 2),
      label = "receiver events"
    )
    expect_equal(
      preproData$startTime,
      min(c(eventsIncrement$time, eventsExogenous$time)),
      label = "start time"
    )
    expect_equal(
      preproData$endTime,
      max(c(eventsIncrement$time, eventsExogenous$time)),
      label = "end Time"
    )
  }
)

# test_that(
#   "in/out/deg windowed and weighted preprocessing",
#   {
#     preproData <- estimate(
#       depNetwork ~ outdeg(networkState, weighted = TRUE, window = 2) +
#         indeg(networkExog, weighted = TRUE, window = 2),
#       model = "DyNAM", subModel = "choice",
#       preprocessingOnly = TRUE
#     )
# 
#     outDependetStatChange <- ReducePreprocess(preproData)[[1]]
#     expect_equal(preproData$initialStats[, , 1],
#       matrix(0, 5, 5, TRUE),
#       label = "initialization of the statistics matrix"
#     )
#     expect_equal(outDependetStatChange,
#       cbind(
#         time = c(6, 9, 13, 15, 16, 16, 19, 19, 23, 28, 29, 32, 32, 36),
#         node1 = c(1, 3, 2, 2, 2, 5, 1, 5, 3, 3, 4, 2, 4, 5),
#         node2 = c(2, 2, 3, 3, 3, 1, 5, 1, 4, 4, 2, 3, 2, 2),
#         replace = c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
#       ),
#       label = "updating with increment works"
#     ) # n-1 updates
#   }
# )

test_that(
  "in/out/deg startTime endTime preprocessing",
  {
    preproData <- estimate(
      depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
        indeg(networkExog, weighted = TRUE),
      model = "DyNAM", subModel = "rate",
      preprocessingOnly = TRUE,
      estimationInit = list(startTime = 10, endTime = 30)
    )
    statsChange <- ReducePreprocess(preproData)
    expect_equal(preproData$initialStats[, , 1],
                 matrix(c(
                   0, 4, 4, 4, 4,
                   4, 0, 4, 4, 4,
                   3, 3, 0, 3, 3,
                   1, 1, 1, 0, 1,
                   0, 0, 0, 0, 0
                 ), 5, 5, TRUE),
                 label = "init outdeg stat matrix"
    )
    expect_equal(preproData$initialStats[, , 2],
                 matrix(c(
                   0, 2, 2, 2, 2,
                   5, 0, 5, 5, 5,
                   0, 0, 0, 0, 0,
                   1, 1, 1, 0, 1,
                   0, 0, 0, 0, 0
                 ), 5, 5, TRUE),
                 label = "init indeg stat matrix"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      fillChanges(
        nodes   = c(2, 5, 1, 3, 3, 4),
        replace = c(5, 1, 6, 4, 5, 2),
        time = NULL, set = 1:5),
      label = "updating outdeg with increment works"
    ) # n-1 updates, should update last 
    expect_equal(
      statsChange[[1]][["dependent"]],
      fillChanges(
        nodes   = c( 2,  5,  1,  3,  3,  4),
        replace = c( 5,  1,  6,  4,  5,  2),
        time    = c(15, 16, 19, 23, 28, 29),
        set = 1:5),
      label = "updating outdeg times with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[2]][["dependent"]],
      fillChanges(
        nodes   = c( 3,  1,  2,  5,  3,  5),
        replace = c( 1,  5,  4,  1,  3,  4),
        time    = c(15, 16, 19, 19, 28, 28),
        set = 1:5),
      label = "updating indeg times with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[1]][["rightCensored"]],
      fillChanges(
        nodes   = c( 2,  1,  3,  2),
        replace = c( 5,  6,  5,  6),
        time    = c(14, 18, 25, 30),
        set = 1:5),
      label = "updating outdeg times right censored"
    ) # n-1 updates
    expect_null(
      statsChange[[2]][["rightCensored"]],
      label = "updating indeg times right censored"
    )
    expect_equal(
      preproData$intervals,
      c(3, 1, 1, 1, 4, 3, 1),
      label = "intervals dependent"
    )
    expect_equal(
      preproData$rightCensoredIntervals,
      c(1, 2, 2, 1),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      c(eventsIncrement$time, eventsExogenous$time, 30) |> unique() |>
        sort() |> Filter(\(x) x >= 10 & x <= 30, x = _),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      c(2, 2, 5, 1, 4, 3, 3, 1, 4, 2, 5),
      label = "sender events"
    ) # ties just firts. Censor by end time adds next event sender
    expect_equal(
      preproData$eventReceiver,
      c(3, 3, 1, 5, 5, 4, 4, 3, 2, 3, 2),
      label = "receiver events"
    )
    expect_equal(
      preproData$startTime,
      10,
      label = "start time"
    )
    expect_equal(
      preproData$endTime,
      30,
      label = "end Time"
    )
  }
)
test_that(
  "in/out/deg startTime endTime exact preprocessing",
  {
    preproData <- estimate(
      depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
        indeg(networkExog, weighted = TRUE),
      model = "DyNAM", subModel = "rate",
      preprocessingOnly = TRUE,
      estimationInit = list(startTime = 6, endTime = 24)
    )
    statsChange <- ReducePreprocess(preproData)
    expect_equal(preproData$initialStats[, , 1],
                 matrix(c(
                   0, 4, 4, 4, 4,
                   3, 0, 3, 3, 3,
                   1, 1, 0, 1, 1,
                   1, 1, 1, 0, 1,
                   0, 0, 0, 0, 0
                 ), 5, 5, TRUE),
                 label = "init outdeg stat matrix"
    )
    expect_equal(preproData$initialStats[, , 2],
                 matrix(c(
                   0, 2, 2, 2, 2,
                   4, 0, 4, 4, 4,
                   0, 0, 0, 0, 0,
                   1, 1, 1, 0, 1,
                   0, 0, 0, 0, 0
                 ), 5, 5, TRUE),
                 label = "init indeg stat matrix"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      fillChanges(
        nodes   = c(3, 2, 2, 5, 1, 3),
        replace = c(3, 4, 5, 1, 6, 4),
        time = NULL, set = 1:5),
      label = "updating outdeg with increment works"
    ) # n-1 updates, should update last 
    expect_equal(
      statsChange[[1]][["dependent"]],
      fillChanges(
        nodes   = c(3,  2,  2,  5,  1,  3),
        replace = c(3,  4,  5,  1,  6,  4),
        time    = c(9, 13, 15, 16, 19, 23),
        set = 1:5),
      label = "updating outdeg times with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[2]][["dependent"]],
      fillChanges(
        nodes   = c(2,  3,  1,  2,  5),
        replace = c(5,  1,  5,  4,  1),
        time    = c(9, 15, 16, 19, 19),
        set = 1:5),
      label = "updating indeg times with increment works"
    ) # n-1 updates
    expect_equal(
      statsChange[[1]][["rightCensored"]],
      fillChanges(
        nodes   = c(3,  2,  1,  3),
        replace = c(3,  5,  6,  5),
        time    = c(7, 14, 18, 24),
        set = 1:5),
      label = "updating outdeg times right censored"
    ) # n-1 updates
    expect_null(
      statsChange[[2]][["rightCensored"]],
      label = "updating indeg times right censored"
    )
    expect_equal(
      preproData$intervals,
      c(0, 2, 4, 1, 1, 1, 4),
      label = "intervals dependent"
    )
    expect_equal(
      preproData$rightCensoredIntervals,
      c(1, 1, 2, 1),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      c(eventsIncrement$time, eventsExogenous$time, 24) |> unique() |>
        sort() |> Filter(\(x) x >= 6 & x <= 24, x = _),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      c(3, 4, rep(2, 3), 5, 1, 4, 3, 3, 4),
      label = "sender events"
    ) # ties just first, last event sender and receiver of next dependent 
    expect_equal(
      preproData$eventReceiver,
      c(2, 2, 3, 3, 3, 1, 5, 5, 4, 4, 2),
      label = "receiver events"
    ) # instead of using sender and receiver next exogenous right censored
    expect_equal(
      preproData$startTime,
      6,
      label = "start time"
    )
    expect_equal(
      preproData$endTime,
      24,
      label = "end Time"
    )
  }
)

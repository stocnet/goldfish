test_that(
  "inertia/tie weighted preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetwork ~ inertia(networkState, weighted = TRUE) +
        tie(networkExog, weighted = TRUE),
      model = "DyNAM", sub_model = "choice", # modelType = "DyNAM-M"
      data = dataTest,
      preprocessing_only = TRUE
    )
    outDependentStatChange <- ReducePreprocess(preproData)
    expect_equal(
      preproData$initialStats[, , 1],
      matrix(c(
        0, 3, 0, 0, 0,
        1, 0, 1, 1, 0,
        0, 0, 0, 1, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(
      preproData$initialStats[, , 2],
      matrix(c(
        0, 0, 0, 1, 0,
        0, 0, 0, 0, 0,
        0, 2, 0, 0, 0,
        1, 0, 0, 0, 0,
        1, 2, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      cbind(
        node1   = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
        node2   = c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2),
        replace = c(4, 2, 2, 3, 1, 2, 2, 3, 1, 4, 1)
      ),
      label = "updating with increment works"
    ) # n-1 updates
    expect_equal(
      outDependentStatChange[[2]],
      cbind(
        time = c(9, 15, 16, 19, 19, 28, 28),
        node1 = c(4, 2, 5, 4, 4, 1, 3),
        node2 = c(2, 3, 1, 2, 5, 3, 5),
        replace = c(1, 1, 4, 0, 1, 2, 3)
      ),
      label = "updating with increment works"
    ) # n-1 updates
    expect_equal(
      preproData$rightCensoredStatsChange,
      list(),
      label = "updating with increment works"
    )
    expect_equal(
      preproData$intervals,
      numeric(),
      label = "intervals dependent"
    ) # intervals are computed with right censored events
    expect_equal(
      preproData$rightCensoredIntervals,
      numeric(),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      rep(1, nrow(eventsIncrement)),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      eventsIncrement$time |> unique(),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$sender)),
      label = "sender events"
    )
    expect_equal(
      preproData$eventReceiver,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$receiver)),
      label = "receiver events"
    )
    expect_equal(
      preproData$startTime,
      head(eventsIncrement$time, 1),
      label = "start time"
    )
    expect_equal(
      preproData$endTime,
      tail(eventsIncrement$time, 1),
      label = "end Time"
    )
  }
)

test_that(
  "inertia  not weighted preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetwork ~ inertia,
      model = "DyNAM", sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    )
    expect_equal(
      preproData$initialStats[, , 1],
      matrix(c(
        0, 1, 0, 0, 0,
        1, 0, 1, 1, 0,
        0, 0, 0, 1, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      cbind(
        node1 = c(3, 5, 1, 4, 5),
        node2 = c(2, 1, 5, 2, 2),
        replace = c(1, 1, 1, 1, 1)
      ),
      label = "updating with increment works"
    ) # n-1 updates
    expect_equal(
      preproData$rightCensoredStatsChange,
      list(),
      label = "updating with increment works"
    )
    expect_equal(
      preproData$intervals,
      numeric(),
      label = "intervals dependent"
    ) # intervals are computed with right censored events
    expect_equal(
      preproData$rightCensoredIntervals,
      numeric(),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      rep(1, nrow(eventsIncrement)),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      eventsIncrement$time |> unique(),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$sender)),
      label = "sender events"
    )
    expect_equal(
      preproData$eventReceiver,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$receiver)),
      label = "receiver events"
    )
    expect_equal(
      preproData$startTime,
      head(eventsIncrement$time, 1),
      label = "start time"
    )
    expect_equal(
      preproData$endTime,
      tail(eventsIncrement$time, 1),
      label = "end Time"
    )
  }
)

test_that(
  "inertia windowed and weighted preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetwork ~ inertia(networkState, weighted = TRUE, window = 2),
      model = "DyNAM", sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    )

    outDependentStatChange <- ReducePreprocess(preproData)[[1]]
    expect_equal(preproData$initialStats[, , 1],
      matrix(0, 5, 5, TRUE),
      label = "initialization of the stats matrix"
    )
    expect_equal(
      outDependentStatChange,
      cbind(
        time = c(6, 9, 13, 15, 16, 16, 19, 19, 23, 28, 29, 32, 32, 36),
        node1 = c(1, 3, 2, 2, 2, 5, 1, 5, 3, 3, 4, 2, 4, 5),
        node2 = c(2, 2, 3, 3, 3, 1, 5, 1, 4, 4, 2, 3, 2, 2),
        replace = c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
      ),
      label = "updating with increment works"
    ) # n-1 updates
    expect_equal(
      preproData$rightCensoredStatsChange,
      list(),
      label = "updating with increment works"
    )
    expect_equal(
      preproData$intervals,
      numeric(),
      label = "intervals dependent"
    ) # intervals are computed with right censored events
    expect_equal(
      preproData$rightCensoredIntervals,
      numeric(),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      rep(1, nrow(eventsIncrement)),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      eventsIncrement$time |> unique(),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$sender)),
      label = "sender events"
    )
    expect_equal(
      preproData$eventReceiver,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$receiver)),
      label = "receiver events"
    )
    expect_equal(
      preproData$startTime,
      head(eventsIncrement$time, 1),
      label = "start time"
    )
    expect_equal(
      preproData$endTime,
      tail(eventsIncrement$time, 1),
      label = "end Time"
    ) # end time includes non dependent events
  }
)

test_that(
  "inertia/tie startTime endTime preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetwork ~ inertia(networkState, weighted = TRUE) +
        tie(networkExog, weighted = TRUE),
      model = "DyNAM", sub_model = "choice", # modelType = "DyNAM-M"
      data = dataTest,
      preprocessing_only = TRUE,
      control_preprocessing =
        set_preprocessing_opt(start_time = 10, end_time = 30)
    )
    outDependentStatChange <- ReducePreprocess(preproData)
    eventsIncrementSubset <- subset(eventsIncrement, time >= 10 & time <= 30)
    expect_equal(
      preproData$initialStats[, , 1],
      matrix(c(
        0, 4, 0, 0, 0,
        1, 0, 2, 1, 0,
        0, 2, 0, 1, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(
      preproData$initialStats[, , 2],
      matrix(c(
        0, 0, 0, 1, 0,
        0, 0, 0, 0, 0,
        0, 2, 0, 0, 0,
        1, 1, 0, 0, 0,
        1, 2, 0, 0, 0
      ), 5, 5, TRUE),
      label = "initialization of the statistics matrix"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$dependentStatsChange, "[[", 1)),
      cbind(
        node1 =   c(2, 5, 1, 3, 3, 4),
        node2 =   c(3, 1, 5, 4, 4, 2),
        replace = c(3, 1, 2, 2, 3, 1)
      ),
      label = "updating with increment works"
    ) # n-1 updates
    expect_equal(
      outDependentStatChange[[2]],
      cbind(
        time = c(15, 16, 19, 19, 28, 28),
        node1 = c(2, 5, 4, 4, 1, 3),
        node2 = c(3, 1, 2, 5, 3, 5),
        replace = c(1, 4, 0, 1, 2, 3)
      ),
      label = "updating with increment works"
    )
    expect_equal(
      preproData$rightCensoredStatsChange,
      list(),
      label = "updating with increment works"
    )
    expect_equal(
      preproData$intervals,
      numeric(),
      label = "intervals dependent"
    ) # intervals are computed with right censored events
    expect_equal(
      preproData$rightCensoredIntervals,
      numeric(),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$orderEvents,
      rep(1, nrow(eventsIncrementSubset)),
      label = "order events"
    )
    expect_equal(
      preproData$eventTime,
      eventsIncrementSubset$time |> unique(),
      label = "events times"
    )
    expect_equal(
      preproData$eventSender,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrementSubset$sender)),
      label = "sender events"
    )
    expect_equal(
      preproData$eventReceiver,
      as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrementSubset$receiver)),
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
  "trans preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetworkTrans ~ trans(networkStateTrans, history="cons"),
      model = "DyNAM", sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    )
    expect_equal(
      preproData$dependentStatsChange[[11]][[1]],
      cbind(
        node1 =   c(4),
        node2 =   c(3),
        replace = c(1)
      ),
      label = "updating with history = consecutive works"
    )
  }
)

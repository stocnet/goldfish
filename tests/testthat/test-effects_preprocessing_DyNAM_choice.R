test_that("inertia/tie weighted preprocessing", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      tie(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "choice", # modelType = "DyNAM-M"
    data = dataTest,
    preprocessing_only = TRUE
  )
  outDependentStatChange <- ReducePreprocess(preproData)
  initMatrix <- matrix(
    # fmt: skip
    c(
      0, 3, 0, 0, 0,
      1, 0, 1, 1, 0,
      0, 0, 0, 1, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0
    ),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 1],
    initMatrix,
    label = "initialization of the statistics matrix"
  )
  initMatrix2 <- matrix(
    # fmt: skip
    c(
      0, 0, 0, 1, 0,
      0, 0, 0, 0, 0,
      0, 2, 0, 0, 0,
      1, 0, 0, 0, 0,
      1, 2, 0, 0, 0
    ),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 2],
    initMatrix2,
    label = "initialization of the statistics matrix"
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
  expect_length(dim(preproData$initialStats), 3L)
  expect_identical(dim(preproData$initialStats), c(5L, 5L, 2L))
  expect_equal(
    ReducePreprocess(preproData, type = "withoutTime")[[1]],
    cbind(
      node1 = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
      node2 = c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2),
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
    preproData$intervals[preproData$is_dependent == 0],
    numeric(),
    label = "intervals right censored"
  )
  expect_equal(
    preproData$is_dependent,
    rep(1L, nrow(eventsIncrement)),
    label = "order events"
  )
  expect_equal(
    preproData$event_time,
    eventsIncrement$time |> unique(),
    label = "events times"
  )
  expect_equal(
    preproData$event_sender,
    as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$sender)),
    label = "sender events"
  )
  expect_equal(
    preproData$event_receiver,
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
})

test_that("inertia not weighted preprocessing", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  initMatrix <- matrix(
    # fmt: skip
    c(
      0, 1, 0, 0, 0,
      1, 0, 1, 1, 0,
      0, 0, 0, 1, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0
    ),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 1],
    initMatrix,
    label = "initialization of the statistics matrix"
  )
  expect_equal(
    ReducePreprocess(preproData, type = "withoutTime")[[1]],
    cbind(
      node1 = c(3, 5, 1, 4, 5),
      node2 = c(2, 1, 5, 2, 2),
      replace = c(1, 1, 1, 1, 1)
    ),
    label = "updating with increment works"
  ) # n-1 updates
  expect_null(preproData$stats_change)
  expect_equal(
    preproData$intervals[preproData$is_dependent == 0],
    numeric(),
    label = "intervals right censored"
  )
  expect_equal(
    preproData$is_dependent,
    rep(1L, nrow(eventsIncrement)),
    label = "order events"
  )
  expect_equal(
    preproData$event_time,
    eventsIncrement$time |> unique(),
    label = "events times"
  )
  expect_equal(
    preproData$event_sender,
    as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$sender)),
    label = "sender events"
  )
  expect_equal(
    preproData$event_receiver,
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
})

test_that("inertia windowed and weighted preprocessing", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE, window = 2),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )

  outDependentStatChange <- ReducePreprocess(preproData)[[1]]
  expect_equal(
    preproData$initialStats[,, 1],
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
  expect_null(preproData$stats_change)
  expect_equal(
    preproData$intervals[preproData$is_dependent == 0],
    numeric(),
    label = "intervals right censored"
  )
  expect_equal(
    preproData$is_dependent,
    rep(1L, nrow(eventsIncrement)),
    label = "order events"
  )
  expect_equal(
    preproData$event_time,
    eventsIncrement$time |> unique(),
    label = "events times"
  )
  expect_equal(
    preproData$event_sender,
    as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrement$sender)),
    label = "sender events"
  )
  expect_equal(
    preproData$event_receiver,
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
})

test_that("inertia/tie startTime endTime preprocessing", {
  preproData <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      tie(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "choice", # modelType = "DyNAM-M"
    data = dataTest,
    preprocessing_only = TRUE,
    control_preprocessing = set_preprocessing(
      start_time = 10,
      end_time = 30
    )
  )
  outDependentStatChange <- ReducePreprocess(preproData)
  eventsIncrementSubset <- subset(eventsIncrement, time >= 10 & time <= 30)
  initMatrix <- matrix(
    # fmt: skip
    c( 
      0, 4, 0, 0, 0,
      1, 0, 2, 1, 0,
      0, 2, 0, 1, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0
    ),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )

  expect_equal(
    preproData$initialStats[,, 1],
    initMatrix,
    label = "initialization of the statistics matrix"
  )
  initMatrix2 <- matrix(
    # fmt: skip
    c( 
      0, 0, 0, 1, 0,
      0, 0, 0, 0, 0,
      0, 2, 0, 0, 0,
      1, 1, 0, 0, 0,
      1, 2, 0, 0, 0
    ),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 2],
    initMatrix2,
    label = "initialization of the statistics matrix"
  )
  expect_equal(
    ReducePreprocess(preproData, type = "withoutTime")[[1]],
    cbind(
      node1 = c(2, 5, 1, 3, 3, 4),
      node2 = c(3, 1, 5, 4, 4, 2),
      replace = c(3, 1, 2, 2, 3, 1)
    ),
    label = "updating with increment works"
  ) # n-1 updates
  expect_null(preproData$stats_change)
  expect_equal(
    preproData$intervals[preproData$is_dependent == 0],
    numeric(),
    label = "intervals right censored"
  )
  expect_equal(
    preproData$is_dependent,
    rep(1L, nrow(eventsIncrementSubset)),
    label = "order events"
  )
  expect_equal(
    preproData$event_time,
    eventsIncrementSubset$time |> unique(),
    label = "events times"
  )
  expect_equal(
    preproData$event_sender,
    as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrementSubset$sender)),
    label = "sender events"
  )
  expect_equal(
    preproData$event_receiver,
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
  expect_equal(
    ReducePreprocess(preproData, type = "withoutTime")[[1]],
    cbind(
      node1 = c(2, 5, 1, 3, 3, 4),
      node2 = c(3, 1, 5, 4, 4, 2),
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
  expect_null(preproData$stats_change)
  expect_equal(
    preproData$intervals[preproData$is_dependent == 0],
    numeric(),
    label = "intervals right censored"
  )
  expect_equal(
    preproData$is_dependent,
    rep(1L, nrow(eventsIncrementSubset)),
    label = "order events"
  )
  expect_equal(
    preproData$event_time,
    eventsIncrementSubset$time |> unique(),
    label = "events times"
  )
  expect_equal(
    preproData$event_sender,
    as.numeric(gsub("\\D+(\\d)", "\\1", eventsIncrementSubset$sender)),
    label = "sender events"
  )
  expect_equal(
    preproData$event_receiver,
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
})

test_that("trans history consecutive", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ trans(networkStateTrans, history = "cons"),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTrans,
    preprocessing_only = TRUE
  )
  outDependentStatChange <- ReducePreprocess(preproData, type = "withoutTime")

  expect_equal(
    outDependentStatChange[[1]],
    cbind(
      node1 = c(5, 4),
      node2 = c(5, 3),
      replace = c(1, 1)
    ),
    label = "updating with history = consecutive works"
  )
})

test_that("trans history sequential", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ trans(networkStateTrans, history = "seq"),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTrans,
    preprocessing_only = TRUE
  )
  outDependentStatChange <- ReducePreprocess(preproData, type = "withoutTime")

  expect_equal(
    outDependentStatChange[[1]],
    cbind(
      node1 = c(5, 1, 3, 4, 1),
      node2 = c(5, 3, 3, 3, 2),
      replace = c(1, 1, 1, 1, 1)
    ),
    label = "updating with history = sequential works",
    ignore_attr = "dimnames"
  )
})

test_that("trans history sequential window", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ trans(networkStateTrans, window = 5, history = "seq"),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTrans,
    preprocessing_only = TRUE
  )
  outDependentStatChange <- ReducePreprocess(preproData, type = "withoutTime")

  expect_equal(
    outDependentStatChange[[1]],
    cbind(
      node1 = c(5, 1, 5, 4, 2, 3, 4, 5),
      node2 = c(5, 1, 5, 3, 4, 2, 3, 3),
      replace = c(1, 0, 0, 1, 1, 1, 0, 0)
    ),
    label = "updating with history = sequential works",
    ignore_attr = "dimnames"
  )
})

test_that("mixed_trans with window and list(net1, net2) preprocesses without error", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ mixed_trans(
      list(networkStateTrans, networkExog),
      window = 5
    ),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTransExog,
    preprocessing_only = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 1],
    matrix(0, nrow = 5, ncol = 5),
    label = "windowed mixed_trans initializes with zero stat",
    ignore_attr = TRUE
  )
})

test_that("mixed_cycle with window and list(net1, net2) preprocesses without error", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ mixed_cycle(
      list(networkStateTrans, networkExog),
      window = 5
    ),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTransExog,
    preprocessing_only = TRUE
  )
  outDependentStatChange <- ReducePreprocess(preproData, type = "withTime")
  expect_equal(
    preproData$initialStats[,, 1],
    matrix(0, nrow = 5, ncol = 5),
    label = "windowed mixed_cycle initializes with zero stat",
    ignore_attr = TRUE
  )
})

test_that("mixed_common_sender with window and list(net1, net2) preprocesses without error", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ mixed_common_sender(
      list(networkStateTrans, networkExog),
      window = 5
    ),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTransExog,
    preprocessing_only = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 1],
    matrix(0, nrow = 5, ncol = 5),
    label = "windowed mixed_common_sender initializes with zero stat",
    ignore_attr = TRUE
  )
})

test_that("mixed_common_receiver with window and list(net1, net2) preprocesses without error", {
  preproData <- estimate_wrapper(
    depNetworkTrans ~ mixed_common_receiver(
      list(networkStateTrans, networkExog),
      window = 5
    ),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTransExog,
    preprocessing_only = TRUE
  )
  expect_equal(
    preproData$initialStats[,, 1],
    matrix(0, nrow = 5, ncol = 5),
    label = "windowed mixed_common_receiver initializes with zero stat",
    ignore_attr = TRUE
  )
})

test_that("Object and attribute existence", {
  expect_error(
    estimate_wrapper(
      depNetwork ~ inertia + tie(notReal),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "is not in the data",
    label = "when object doesn't exist"
  )
  expect_error(
    estimate_wrapper(
      depNetwork ~ inertia + alter(actors_ex$uno),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "is not in the data",
    label = "when attribute of an object doesn't exist"
  )
  expect_error(
    estimate_wrapper(
      depNetwork ~ inertia + alter(actorsEx2$uno),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "is not in the data",
    label = "when nodeset doesn't exist"
  )
})

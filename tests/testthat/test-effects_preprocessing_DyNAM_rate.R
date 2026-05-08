test_that(
  "out/in/deg weighted right censored preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
        indeg(networkExog, weighted = TRUE),
      model = "DyNAM", sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE
    )
    statsChange <- ReducePreprocess(preproData)
    expect_equal(preproData$initialStats[, 1],
      c(3, 3, 1, 1, 0),
      label = "init outdeg stat vector"
    )
    expect_equal(preproData$initialStats[, 2],
      c(2, 4, 0, 1, 0),
      label = "init indeg stat vector"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$stats_change[preproData$is_dependent == 1], "[[", 1)),
      cbind(
        node1   = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
        replace = c(4, 3, 4, 5, 1, 6, 4, 5, 2, 6, 2)
      ),
      label = "updating outdeg with increment works"
    )
    expect_equal(
      statsChange[[1]][["dependent"]],
      cbind(
        time    = c(6, 9, 13, 15, 16, 19, 23, 28, 29, 32, 36),
        node1   = c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5),
        replace = c(4, 3, 4, 5, 1, 6, 4, 5, 2, 6, 2)
      ),
      label = "updating outdeg times with increment works"
    )
    expect_equal(
      statsChange[[2]][["dependent"]],
      cbind(
        time    = c(9, 15, 16, 19, 19, 28, 28),
        node1   = c(2, 3, 1, 2, 5, 3, 5),
        replace = c(5, 1, 5, 4, 1, 3, 4)
      ),
      label = "updating indeg times with increment works"
    )
    expect_equal(
      statsChange[[1]][["rightCensored"]],
      cbind(
        time    = c(7, 14, 18, 25),
        node1   = c(3, 2, 1, 3),
        replace = c(3, 5, 6, 5)
      ),
      label = "updating outdeg times right censored"
    )
    expect_null(
      statsChange[[2]][["rightCensored"]],
      label = "updating indeg times right censored"
    )
    expect_equal(
      preproData$intervals[preproData$is_dependent == 1],
      c(0, 5, 2, 4, 1, 1, 1, 4, 3, 1, 3, 4),
      label = "intervals dependent"
    )
    expect_equal(
      preproData$intervals[preproData$is_dependent == 0],
      c(1, 1, 2, 2),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$is_dependent,
      c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, rep(1, 4)),
      label = "order events"
    )
    expect_equal(
      preproData$event_time,
      c(eventsIncrement$time, eventsExogenous$time) |> unique() |> sort(),
      label = "events times"
    )
    expect_equal(
      preproData$event_sender,
      c(1, 3, 4, 2, 2, 2, 5, 1, 4, 3, 3, 1, 4, 2, 5, 1),
      label = "sender events"
    )
    expect_equal(
      preproData$event_receiver,
      c(2, 2, 2, 3, 3, 3, 1, 5, 5, 4, 4, 3, 2, 3, 2, 2),
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

test_that(
  "in/out/deg startTime endTime preprocessing",
  {
    preproData <- estimate_wrapper(
      depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
        indeg(networkExog, weighted = TRUE),
      model = "DyNAM", sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE,
      control_preprocessing =
        set_preprocessing_opt(start_time = 10, end_time = 30)
    )
    statsChange <- ReducePreprocess(preproData)
    expect_equal(
      preproData$initialStats[, 1],
      c(4, 4, 3, 1, 0),
      label = "init outdeg stat vector"
    )
    expect_equal(
      preproData$initialStats[, 2],
      c(2, 5, 0, 1, 0),
      label = "init indeg stat vector"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$stats_change[preproData$is_dependent == 1], "[[", 1)),
      cbind(
        node1   = c(2, 5, 1, 3, 3, 4),
        replace = c(5, 1, 6, 4, 5, 2)
      ),
      label = "updating outdeg with increment works"
    )
    expect_equal(
      statsChange[[1]][["dependent"]],
      cbind(
        time    = c(15, 16, 19, 23, 28, 29),
        node1   = c(2, 5, 1, 3, 3, 4),
        replace = c(5, 1, 6, 4, 5, 2)
      ),
      label = "updating outdeg times with increment works"
    )
    expect_equal(
      statsChange[[2]][["dependent"]],
      cbind(
        time    = c(15, 16, 19, 19, 28, 28),
        node1   = c(3, 1, 2, 5, 3, 5),
        replace = c(1, 5, 4, 1, 3, 4)
      ),
      label = "updating indeg times with increment works"
    )
    expect_equal(
      statsChange[[1]][["rightCensored"]],
      cbind(
        time    = c(14, 18, 25, 30),
        node1   = c(2, 1, 3, 2),
        replace = c(5, 6, 5, 6)
      ),
      label = "updating outdeg times right censored"
    )
    expect_null(
      statsChange[[2]][["rightCensored"]],
      label = "updating indeg times right censored"
    )
    expect_equal(
      preproData$intervals[preproData$is_dependent == 1],
      c(3, 1, 1, 1, 4, 3, 1),
      label = "intervals dependent"
    )
    expect_equal(
      preproData$intervals[preproData$is_dependent == 0],
      c(1, 2, 2, 1),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$is_dependent,
      c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0),
      label = "order events"
    )
    expect_equal(
      preproData$event_time,
      c(eventsIncrement$time, eventsExogenous$time, 30) |>
        unique() |>
        sort() |>
        Filter(\(x) x >= 10 & x <= 30, x = _),
      label = "events times"
    )
    expect_equal(
      preproData$event_sender,
      c(2, 2, 5, 1, 4, 3, 3, 1, 4, 2, 5),
      label = "sender events"
    )
    expect_equal(
      preproData$event_receiver,
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
    preproData <- estimate_wrapper(
      depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
        indeg(networkExog, weighted = TRUE),
      model = "DyNAM", sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE,
      control_preprocessing =
        set_preprocessing_opt(start_time = 6, end_time = 24)
    )
    statsChange <- ReducePreprocess(preproData)
    expect_equal(
      preproData$initialStats[, 1],
      c(4, 3, 1, 1, 0),
      label = "init outdeg stat vector"
    )
    expect_equal(
      preproData$initialStats[, 2],
      c(2, 4, 0, 1, 0),
      label = "init indeg stat vector"
    )
    expect_equal(
      Reduce(rbind, lapply(preproData$stats_change[preproData$is_dependent == 1], "[[", 1)),
      cbind(
        node1   = c(3, 2, 2, 5, 1, 3),
        replace = c(3, 4, 5, 1, 6, 4)
      ),
      label = "updating outdeg with increment works"
    )
    expect_equal(
      statsChange[[1]][["dependent"]],
      cbind(
        time    = c(9, 13, 15, 16, 19, 23),
        node1   = c(3, 2, 2, 5, 1, 3),
        replace = c(3, 4, 5, 1, 6, 4)
      ),
      label = "updating outdeg times with increment works"
    )
    expect_equal(
      statsChange[[2]][["dependent"]],
      cbind(
        time    = c(9, 15, 16, 19, 19),
        node1   = c(2, 3, 1, 2, 5),
        replace = c(5, 1, 5, 4, 1)
      ),
      label = "updating indeg times with increment works"
    )
    expect_equal(
      statsChange[[1]][["rightCensored"]],
      cbind(
        time    = c(7, 14, 18, 24),
        node1   = c(3, 2, 1, 3),
        replace = c(3, 5, 6, 5)
      ),
      label = "updating outdeg times right censored"
    )
    expect_null(
      statsChange[[2]][["rightCensored"]],
      label = "updating indeg times right censored"
    )
    expect_equal(
      preproData$intervals[preproData$is_dependent == 1],
      c(0, 2, 4, 1, 1, 1, 4),
      label = "intervals dependent"
    )
    expect_equal(
      preproData$intervals[preproData$is_dependent == 0],
      c(1, 1, 2, 1),
      label = "intervals right censored"
    )
    expect_equal(
      preproData$is_dependent,
      c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0),
      label = "order events"
    )
    expect_equal(
      preproData$event_time,
      c(eventsIncrement$time, eventsExogenous$time, 24) |>
        unique() |>
        sort() |>
        Filter(\(x) x >= 6 & x <= 24, x = _),
      label = "events times"
    )
    expect_equal(
      preproData$event_sender,
      c(3, 4, 2, 2, 2, 5, 1, 4, 3, 3, 4),
      label = "sender events"
    )
    expect_equal(
      preproData$event_receiver,
      c(2, 2, 3, 3, 3, 1, 5, 5, 4, 4, 2),
      label = "receiver events"
    )
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

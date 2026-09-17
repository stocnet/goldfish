# The rate's sender gate on a support mask. A sender is at risk only when
# some present receiver other than itself is allowed: on a one-mode layer
# the self-dyad is never a receiver, even where the mask allows it, as it
# always does for a creation flavor, since no actor holds a tie to itself.

test_that("the oracle drops the self-dyad only on a one-mode layer", {
  grid <- matrix(c(TRUE, FALSE, FALSE, FALSE), 2L, 2L)
  present <- c(TRUE, TRUE)

  expect_identical(
    oracle_sender_gate(grid, present, one_mode = TRUE),
    c(FALSE, FALSE)
  )
  expect_identical(
    oracle_sender_gate(grid, present, one_mode = FALSE),
    c(TRUE, FALSE)
  )
})

test_that("the fixture holds a sender whose only creation is itself", {
  data <- self_only_sender_data()
  ties <- as.data.frame(data$ties)
  state <- matrix(0, 4L, 4L)
  history <- ties[is.na(ties$time), ]
  state[cbind(history$from, history$to)] <- 1
  events <- ties[!is.na(ties$time), ]
  events <- events[order(events$time), ]

  self_only <- logical(nrow(events))
  for (k in seq_len(nrow(events))) {
    creation <- state == 0
    self_only[k] <- creation[1L, 1L] && !any(creation[1L, -1L])
    cell <- cbind(events$from[k], events$to[k])
    state[cell] <- state[cell] + events$weight[k]
  }

  # Saturated until N1 dissolves at t = 7, free at t = 8 and 9, and
  # saturated again once N1 recreates its tie at t = 9.
  expect_identical(self_only, events$time <= 7 | events$time == 10)
  expect_setequal(ties$flavor[!is.na(ties$time)], c("creation", "dissolution"))
})

test_that("the folded gate excludes the self-dyad at every mask kind", {
  # Point: sender 1 is allowed only its own cell, sender 3 nothing at all.
  point <- matrix(FALSE, 3L, 3L)
  point[1L, 1L] <- TRUE
  point[2L, c(1L, 3L)] <- TRUE
  masks <- list(as.logical(point), as.logical(point))
  present <- rep(TRUE, 3L)

  gate <- fold_gate_timeline(masks, 0L, present, present, drop_diagonal = TRUE)
  grids <- mask_grids(masks, 0L, 3L, 3L)

  expect_identical(
    gate,
    lapply(grids, oracle_sender_gate, present = present, one_mode = TRUE)
  )
  expect_identical(gate[[1L]], c(FALSE, TRUE, FALSE))

  # Alter: only receiver 1 is allowed, so sender 1 has nobody but itself.
  alter <- c(TRUE, FALSE, FALSE)
  gate <- fold_gate_timeline(
    list(alter, alter),
    1L,
    present,
    present,
    drop_diagonal = TRUE
  )
  expect_identical(gate[[2L]], c(FALSE, TRUE, TRUE))

  # Ego with one present receiver: only sender 1 loses its last receiver.
  only_first <- c(TRUE, FALSE, FALSE)
  gate <- fold_gate_timeline(
    list(rep(TRUE, 3L), rep(TRUE, 3L)),
    2L,
    present,
    only_first,
    drop_diagonal = TRUE
  )
  expect_identical(gate[[1L]], c(FALSE, TRUE, TRUE))

  # Global, same reduction.
  gate <- fold_gate_timeline(
    list(TRUE, TRUE),
    3L,
    present,
    only_first,
    drop_diagonal = TRUE
  )
  expect_identical(gate[[1L]], c(FALSE, TRUE, TRUE))
})

test_that("a two-mode layer keeps every allowed cell", {
  # Two node sets: index 1 on each axis names different nodes.
  point <- matrix(FALSE, 2L, 3L)
  point[1L, 1L] <- TRUE
  masks <- list(as.logical(point), as.logical(point))

  gate <- fold_gate_timeline(
    masks,
    0L,
    rep(TRUE, 2L),
    rep(TRUE, 3L),
    drop_diagonal = FALSE
  )

  expect_identical(gate[[1L]], c(TRUE, FALSE))
})

test_that("a flip of a sender's own cell moves no count", {
  off <- matrix(FALSE, 3L, 3L)
  off[1L, 2L] <- TRUE
  on <- off
  on[1L, 1L] <- TRUE
  present <- rep(TRUE, 3L)

  gate <- fold_gate_timeline(
    list(as.logical(off), as.logical(on), as.logical(off)),
    0L,
    present,
    present,
    drop_diagonal = TRUE
  )

  expect_identical(gate, rep(list(c(TRUE, FALSE, FALSE)), 3L))
})

test_that("a receiver leaving strands the sender it was allowed for", {
  # Sender 2 may only reach receiver 3; receiver 3 leaves at the second event.
  point <- matrix(FALSE, 3L, 3L)
  point[1L, 1L] <- TRUE
  point[2L, 3L] <- TRUE
  point[3L, 2L] <- TRUE
  masks <- rep(list(as.logical(point)), 3L)
  crossings <- list(
    update = matrix(c(3, 0), nrow = 2L),
    pointer = c(0L, 1L, 1L)
  )

  gate <- fold_gate_timeline(
    masks,
    0L,
    rep(TRUE, 3L),
    rep(TRUE, 3L),
    receiver_crossings = crossings,
    drop_diagonal = TRUE
  )

  expect_identical(gate[[1L]], c(FALSE, TRUE, TRUE))
  expect_identical(gate[[2L]], c(FALSE, FALSE, TRUE))
  expect_identical(gate[[3L]], c(FALSE, FALSE, TRUE))
})

test_that("the fixture's rate gates out the sender that can only self-create", {
  data <- self_only_sender_data()
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg),
    model = "DyNAM",
    data = data
  )

  pre <- suppressMessages(preprocess_joint(spec, set_preprocessing()))
  rate <- pre[[1L]]
  gate <- rate$active_sender_init
  update <- rate$active_sender_update
  pointer <- rate$active_sender_update_pointer
  seen <- 0L
  at_risk <- vapply(
    seq_along(pointer),
    function(e) {
      hi <- pointer[[e]]
      if (hi > seen) {
        cols <- (seen + 1L):hi
        gate[update[1L, cols]] <<- as.logical(update[2L, cols])
        seen <<- hi
      }
      gate[[1L]]
    },
    logical(1)
  )

  # N1 is at risk only while it can create a tie it does not already hold.
  expect_identical(at_risk, rate$event_time %in% c(8, 9))
  expect_equal(rate$avg_active_entity, 3.2)
})

test_that("the relational risk set matches the panel endpoint count", {
  data <- self_only_sender_data()
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg),
    model = "DyNAM",
    data = data
  )
  ties <- as.data.frame(data$ties)
  history <- ties[is.na(ties$time), ]
  state <- matrix(0, 4L, 4L)
  state[cbind(history$from, history$to)] <- 1

  pre <- suppressMessages(preprocess_joint(spec, set_preprocessing()))
  at_first_event <- sum(pre[[1L]]$active_sender_init)

  # The panel path counts a flavor's rate entities from a materialized state
  # with self-loops removed; the relational path folds them from the mask.
  expect_identical(
    at_first_event,
    endpoint_entity_count(state, "actor", "creation", one_mode = TRUE)
  )
})

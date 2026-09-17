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

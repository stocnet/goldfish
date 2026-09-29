# goldfish against classical fits of the same likelihood, on sequences whose
# generating process is known.
#
# `helper-simulate-dgp.R` simulates exact-time DyNAM sequences and, inside
# the same loop, writes the frames a Poisson regression (rate) and a
# conditional logit (choice) fit, with the risk-set rule the process used.
# Each fixture then checks two things: that goldfish's exported statistics
# are those frames row for row, and that goldfish's estimates equal the
# classical ones within the package's 1e-6 discipline.

# ---- the harness itself ----------------------------------------------------

test_that("a simulated sequence is reproducible under its seed", {
  run <- function() {
    simulate_relational_sequence(
      n_actors = 5,
      n_events = 20,
      processes = list(list(rate = c(-1, 0.2, -0.1), choice = c(0.5, 0.3))),
      seed = 11,
      rem = TRUE
    )
  }
  expect_identical(run(), run())
})

test_that("the frames follow the process's risk-set rule", {
  # A1 emails every other node by t = 0.012, so under `~ !tie(emails)` A1
  # has no allowed receiver from then on; A3 leaves at t = 0.02.
  exogenous <- data.frame(
    time = c(0.01, 0.011, 0.012),
    from = 1L,
    to = c(2L, 4L, 3L)
  )
  sim <- simulate_relational_sequence(
    n_actors = 4,
    n_events = 12,
    processes = list(list(rate = c(0, 0, 0), choice = c(0, 0))),
    seed = 3,
    exogenous = exogenous,
    constraint = function(tie, exogenous_tie) exogenous_tie == 0,
    composition = data.frame(time = 0.02, node = 3L, present = FALSE)
  )
  rate <- sim$rate[[1]]
  choice <- sim$choice[[1]]
  events <- sim$events

  # One censored interval per exogenous row and per composition change, in
  # time order: the last one is A3 leaving.
  censored <- unique(rate$interval[!rate$dependent])
  expect_length(censored, nrow(exogenous) + 1L)

  after_block <- rate$interval > max(censored[seq_len(3)])
  expect_false(any(rate$sender[after_block] == 1L))
  after_leave <- rate$interval > max(censored)
  expect_false(any(rate$sender[after_leave] == 3L))
  expect_false(any(
    choice$receiver[
      choice$interval > 0 &
        events$time[choice$interval] > 0.02
    ] ==
      3L
  ))
  expect_true(all(choice$sender != choice$receiver))

  # One realized sender per dependent interval, one chosen receiver per
  # event, and the realized dyad is the event's.
  dep <- rate[rate$dependent, ]
  expect_true(all(tapply(dep$outcome, dep$interval, sum) == 1L))
  expect_true(all(tapply(choice$outcome, choice$interval, sum) == 1L))
  chosen <- choice[choice$outcome == 1L, ]
  expect_identical(chosen$sender, events$from)
  expect_identical(chosen$receiver, events$to)
  expect_true(all(rate$dt > 0))
})

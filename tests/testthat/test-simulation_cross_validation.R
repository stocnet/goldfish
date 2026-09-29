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

# ---- shared pieces ----------------------------------------------------------

# Independently optimized parity, as in the classical equivalence tests.
simulation_tolerance <- 1e-6

# goldfish's default Newton stop (`score_tol = 1e-6`) leaves a last step of a
# few 1e-7 on these fixtures, which is most of the gate. Fitting to the
# maximum makes the gap the two optimizers' agreement rather than goldfish's
# stopping rule.
simulation_tight_fit <- function() {
  set_algorithm_newton(score_tol = 1e-10, step_tol = 1e-12, max_iterations = 50)
}

# The window opens at the simulation's clock origin, so the first interval
# has the length the process drew rather than zero.
simulation_window <- function() set_preprocessing(start_time = 0)

# goldfish's exported rows against a hand-built frame: the row keys and the
# realized outcome, the interval length where the frame has one, then every
# statistic, each under the effect's exported column name.
expect_frame_identity <- function(exported, frame, stats) {
  expect_identical(nrow(exported), nrow(frame))
  expect_equal(exported$event, frame$interval)
  expect_equal(exported$index_i, frame$sender)
  if (!all(is.na(frame$receiver))) {
    expect_equal(exported$index_j, frame$receiver)
  }
  expect_equal(exported$chosen, frame$outcome)
  if (!all(is.na(frame$dt))) {
    expect_equal(exported$timespan, frame$dt)
  }
  for (s in names(stats)) {
    expect_equal(exported[[stats[[s]]]], frame[[s]], info = s)
  }
}

# The Poisson form carries a `log dt` per observed event that the rate
# likelihood does not, a constant in theta.
expect_rate_equals_poisson <- function(fit, frame, formula) {
  twin <- stats::glm(
    formula,
    data = frame,
    family = stats::poisson(),
    control = stats::glm.control(epsilon = 1e-14, maxit = 100)
  )
  expect_equal(
    unname(coef(fit)),
    unname(coef(twin)),
    tolerance = simulation_tolerance
  )
  expect_equal(
    as.numeric(logLik(fit)),
    as.numeric(logLik(twin)) - sum(log(frame$dt[frame$outcome == 1L])),
    tolerance = simulation_tolerance
  )
}

# ---- F1: the unconstrained baseline -----------------------------------------

f1_simulation <- function() {
  sim <- simulate_relational_sequence(
    n_actors = 10,
    n_events = 150,
    processes = list(list(rate = c(-1, 0.3, -0.2), choice = c(1, 0.4))),
    seed = 42,
    rem = TRUE
  )
  sim$data <- dgp_stocnet(sim, n_actors = 10)
  sim
}

f1_dynam_spec <- function(sim) {
  make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ recip + trans,
    layer = "calls",
    model = "DyNAM",
    data = sim$data
  )
}

f1_rem_spec <- function(sim) {
  make_specification(
    rate = ~ 1 + indeg + outdeg,
    layer = "calls",
    model = "REM",
    data = sim$data
  )
}

test_that("F1: goldfish exports the hand-built frames row for row", {
  sim <- f1_simulation()
  spec <- f1_dynam_spec(sim)
  export <- function(x, model, sub_model) {
    suppressMessages(compute_statistics(
      x,
      model = model,
      sub_model = sub_model,
      output = "data.frame",
      control_prep = simulation_window()
    ))
  }

  expect_frame_identity(
    export(spec, "DyNAM", "rate"),
    sim$rate[[1]],
    c(indeg = "indeg_calls", outdeg = "outdeg_calls")
  )
  expect_frame_identity(
    export(spec, "DyNAM", "choice"),
    sim$choice[[1]],
    c(recip = "recip_calls", trans = "trans_calls")
  )
  expect_frame_identity(
    export(f1_rem_spec(sim), "REM", "rate"),
    sim$dyad,
    c(indeg = "indeg_calls", outdeg = "outdeg_calls")
  )
})

test_that("F1: the exact-time rate is a Poisson regression with an offset", {
  sim <- f1_simulation()
  fit <- estimate_dynam(
    f1_dynam_spec(sim),
    sub_model = "rate",
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  )
  expect_rate_equals_poisson(
    fit,
    sim$rate[[1]],
    outcome ~ indeg + outdeg + offset(log(dt))
  )
})

test_that("F1: the exact-time REM is a dyadic Poisson regression", {
  sim <- f1_simulation()
  fit <- suppressMessages(estimate_rem(
    f1_rem_spec(sim),
    sub_model = "rate",
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  ))
  expect_rate_equals_poisson(
    fit,
    sim$dyad,
    outcome ~ indeg + outdeg + offset(log(dt))
  )
})

test_that("F1: the choice is a conditional logit over the receivers", {
  sim <- f1_simulation()
  frame <- sim$choice[[1]]
  fit <- estimate_dynam(
    f1_dynam_spec(sim),
    sub_model = "choice",
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  )

  skip_if_not_installed("survival")
  withr::local_package("survival")
  twin <- clogit(outcome ~ recip + trans + strata(interval), data = frame)
  expect_equal(
    unname(coef(fit)),
    unname(coef(twin)),
    tolerance = simulation_tolerance
  )
  expect_equal(
    as.numeric(logLik(fit)),
    as.numeric(logLik(twin)),
    tolerance = simulation_tolerance
  )

  skip_if_not_installed("mlogit")
  skip_if_not_installed("dfidx")
  indexed <- dfidx::dfidx(
    frame,
    idx = c("interval", "receiver"),
    choice = "outcome"
  )
  twin <- mlogit::mlogit(outcome ~ recip + trans | 0, data = indexed)
  expect_equal(
    unname(coef(fit)),
    unname(coef(twin)),
    tolerance = simulation_tolerance
  )
})

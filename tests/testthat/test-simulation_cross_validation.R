# goldfish against classical fits of the same likelihood, on sequences whose
# generating process is known.
#
# `helper-simulate-dgp.R` simulates exact-time DyNAM sequences and, inside
# the same loop, writes the frames a Poisson regression (rate) and a
# conditional logit (choice) fit, with the risk-set rule the process used.
# Each fixture then checks two things: that goldfish's exported statistics
# are those frames row for row, and that goldfish's estimates equal the
# classical ones within the package's 1e-6 discipline.
#
# `choice_coordination` is excluded: its dyad weight carries two log-sum-exp
# terms indexed by the dyad's own endpoints, nonlinear in theta, so no
# conditional logit is the same likelihood (measured 0.2849 against 0.0575
# in `_references/classical_v1/README.md`). The reasoning and the measured
# deltas of every fixture are in `_references/simulation_v1/README.md`.

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

test_that("the harness's stocnet switches a departed node out", {
  sim <- simulate_relational_sequence(
    n_actors = 5,
    n_events = 40,
    processes = list(list(rate = c(0, 0, 0), choice = c(0, 0))),
    seed = 8,
    composition = data.frame(time = 1, node = 3L, present = FALSE)
  )
  data <- dgp_stocnet(sim, n_actors = 5)
  spec <- make_specification(
    choice = ~ recip + trans,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  exported <- suppressMessages(compute_statistics(
    spec,
    sub_model = "choice",
    output = "data.frame",
    control_prep = set_preprocessing(start_time = 0)
  ))
  # The choice reads presence at its own event, so after N3 leaves it is no
  # event's candidate receiver.
  after <- exported$event > sum(sim$events$time < 1)
  expect_gt(sum(after), 0L)
  expect_false(any(exported$index_j[after] == 3L))
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

# ---- a constraint on a layer no term reads ----------------------------------

# Senders 1-3 email every other node at spread times, so each is gated out
# of the rate partway through the window, between two dependent events.
gating_simulation <- function() {
  n_actors <- 8
  exogenous <- do.call(
    rbind,
    lapply(1:3, function(i) {
      data.frame(from = i, to = setdiff(seq_len(n_actors), i))
    })
  )
  withr::with_seed(7, {
    exogenous$time <- sort(stats::runif(nrow(exogenous), 0.5, 20))
  })
  sim <- simulate_relational_sequence(
    n_actors = n_actors,
    n_events = 120,
    processes = list(list(rate = c(-1, 0.2, -0.1), choice = c(1, 0.3))),
    seed = 9,
    exogenous = exogenous,
    constraint = function(tie, exogenous_tie) exogenous_tie == 0
  )
  sim$data <- dgp_stocnet(sim, n_actors = n_actors)
  sim
}

gating_spec <- function(sim) {
  make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ recip + trans,
    layer = "calls",
    model = "DyNAM",
    data = sim$data,
    support_constraint = ~ !tie(emails)
  )
}

test_that("a constraint-only event closes the constrained rate's interval", {
  sim <- gating_simulation()
  spec <- gating_spec(sim)
  exported <- suppressWarnings(suppressMessages(compute_statistics(
    spec,
    sub_model = "rate",
    output = "data.frame",
    control_prep = simulation_window()
  )))
  expect_frame_identity(
    exported,
    sim$rate[[1]],
    c(indeg = "indeg_calls", outdeg = "outdeg_calls")
  )
  fit <- suppressWarnings(suppressMessages(estimate_dynam(
    spec,
    sub_model = "rate",
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  )))
  expect_rate_equals_poisson(
    fit,
    sim$rate[[1]],
    outcome ~ indeg + outdeg + offset(log(dt))
  )
})

# ---- shared by the constrained fixtures --------------------------------------

# The conditional logit stratified by event, over the receivers the frame
# lists, equals goldfish's choice in coefficients and log-likelihood.
expect_choice_equals_clogit <- function(fit, frame, formula) {
  skip_if_not_installed("survival")
  withr::local_package("survival")
  twin <- clogit(formula, data = frame)
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
}

simulation_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# ---- F2: creation and dissolution under their derived masks ----------------

# A mutually exclusive layer: creation may only target an absent tie and
# dissolution a present one, so each flavor's risk set is its derived mask
# and a sender with no such receiver is not at risk. A third of the dyads
# start tied, so both flavors have senders at risk from the first interval.
f2_simulation <- function(unmodeled = character(0)) {
  n_actors <- 8
  initial <- withr::with_seed(5, {
    which(
      matrix(stats::runif(n_actors^2) < 0.3, n_actors) & !diag(n_actors),
      arr.ind = TRUE
    )
  })
  sim <- simulate_relational_sequence(
    n_actors = n_actors,
    n_events = 200,
    processes = list(
      creation = list(rate = c(-1, 0.2, -0.1), choice = c(0.8, 0.3)),
      dissolution = list(rate = c(-1.2, -0.1, 0.15), choice = c(-0.5, 0.2))
    ),
    seed = 21,
    initial = initial,
    unmodeled = unmodeled
  )
  sim$data <- dgp_stocnet(sim, n_actors = n_actors)
  sim
}

f2_formula <- function(flavor, family) {
  stats::as.formula(paste(
    flavor,
    if (family == "rate") "~ 1 + indeg + outdeg" else "~ recip + trans"
  ))
}

# One flavor of one family, alone on the flavored layer.
f2_standalone_spec <- function(sim, flavor, family) {
  args <- list(model = "DyNAM", data = sim$data)
  args[[family]] <- list(f2_formula(flavor, family))
  do.call(make_specification, args)
}

f2_container_spec <- function(sim) {
  make_specification(
    rate = list(
      f2_formula("creation", "rate"),
      f2_formula("dissolution", "rate")
    ),
    choice = list(
      f2_formula("creation", "choice"),
      f2_formula("dissolution", "choice")
    ),
    model = "DyNAM",
    data = sim$data
  )
}

# The same events with no flavor machinery: `calls` is the state both
# flavors write, and each flavor's events are a plain dependent layer of
# their own whose risk set is written by hand as the derived mask.
f2_deflavored_spec <- function(sim, flavor, family) {
  events <- sim$events
  own <- events$flavor == flavor
  layer <- if (flavor == "creation") "create" else "dissolve"
  ties <- rbind(
    data.frame(
      from = sim$initial[, 1],
      to = sim$initial[, 2],
      time = NA_real_,
      layer = "calls",
      weight = 1
    ),
    data.frame(
      from = events$from,
      to = events$to,
      time = events$time,
      layer = "calls",
      weight = ifelse(events$flavor == "dissolution", -1, 1)
    ),
    data.frame(
      from = events$from[own],
      to = events$to[own],
      time = events$time[own],
      layer = layer,
      weight = 1
    )
  )
  layers <- c("calls", layer)
  data <- list(
    info = list(
      name = "deflavored",
      focal = layer,
      update = stats::setNames(rep("increment", 2), layers),
      directed = stats::setNames(rep(TRUE, 2), layers),
      observation = stats::setNames(rep("event", 2), layers)
    ),
    nodes = sim$data$nodes,
    ties = ties
  )
  args <- list(
    layer = layer,
    model = "DyNAM",
    data = data,
    support_constraint = if (flavor == "creation") {
      ~ !tie(calls)
    } else {
      ~ tie(calls)
    }
  )
  args[[family]] <- if (family == "rate") {
    ~ 1 + indeg(calls) + outdeg(calls)
  } else {
    ~ recip(calls) + trans(calls)
  }
  do.call(make_specification, args)
}

f2_stats <- function(family) {
  if (family == "rate") {
    c(indeg = "indeg_calls", outdeg = "outdeg_calls")
  } else {
    c(recip = "recip_calls", trans = "trans_calls")
  }
}

test_that("F2: each flavor exports its hand-built frames row for row", {
  sim <- f2_simulation()
  for (flavor in c("creation", "dissolution")) {
    for (family in c("rate", "choice")) {
      exported <- simulation_quiet(compute_statistics(
        f2_standalone_spec(sim, flavor, family),
        sub_model = family,
        output = "data.frame",
        control_prep = simulation_window()
      ))
      frame <- if (family == "rate") {
        sim$rate[[flavor]]
      } else {
        sim$choice[[flavor]]
      }
      expect_frame_identity(exported, frame, f2_stats(family))
    }
  }
})

test_that("F2: the three goldfish routes agree and equal the oracle", {
  sim <- f2_simulation()
  container <- simulation_quiet(estimate_dynam(
    f2_container_spec(sim),
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  ))
  for (flavor in c("creation", "dissolution")) {
    for (family in c("rate", "choice")) {
      label <- paste(flavor, family)
      joint <- fit_of(container, flavor, family)
      others <- list(
        standalone = f2_standalone_spec(sim, flavor, family),
        deflavored = f2_deflavored_spec(sim, flavor, family)
      )
      for (route in names(others)) {
        fit <- simulation_quiet(estimate_dynam(
          others[[route]],
          sub_model = family,
          control_prep = simulation_window(),
          control_algo = simulation_tight_fit()
        ))
        expect_equal(
          unname(coef(fit)),
          unname(coef(joint)),
          tolerance = simulation_tolerance,
          label = paste(label, route)
        )
        expect_equal(
          as.numeric(logLik(fit)),
          as.numeric(logLik(joint)),
          tolerance = simulation_tolerance,
          label = paste(label, route)
        )
      }
      if (family == "rate") {
        expect_rate_equals_poisson(
          joint,
          sim$rate[[flavor]],
          outcome ~ indeg + outdeg + offset(log(dt))
        )
      } else {
        expect_choice_equals_clogit(
          joint,
          sim$choice[[flavor]],
          outcome ~ recip + trans + strata(interval)
        )
      }
    }
  }
})

test_that("F2: the handle replays both flavors, masks excluding", {
  replay <- walk_replay_against_batch(f2_container_spec(f2_simulation()))
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

# ---- F3: a constraint on an object no formula term reads, the choice -------

# The gating fixture above, read by a choice: `~ !tie(emails)` removes the
# receivers a sender has emailed, and senders 1-3 end up with none.
gating_choice_spec <- function(sim) {
  make_specification(
    choice = ~recip,
    layer = "calls",
    model = "DyNAM",
    data = sim$data,
    support_constraint = ~ !tie(emails)
  )
}

test_that("F3: the constrained choice exports the allowed receivers only", {
  sim <- gating_simulation()
  exported <- simulation_quiet(compute_statistics(
    gating_choice_spec(sim),
    sub_model = "choice",
    output = "data.frame",
    control_prep = simulation_window()
  ))
  expect_frame_identity(exported, sim$choice[[1]], c(recip = "recip_calls"))
})

test_that("F3: the constrained choice is a conditional logit", {
  sim <- gating_simulation()
  fit <- simulation_quiet(estimate_dynam(
    gating_choice_spec(sim),
    sub_model = "choice",
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  ))
  expect_choice_equals_clogit(
    fit,
    sim$choice[[1]],
    outcome ~ recip + strata(interval)
  )
})

test_that("F3: the handle replays the constraint, receivers excluded", {
  sim <- gating_simulation()
  spec <- make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~recip,
    layer = "calls",
    model = "DyNAM",
    data = sim$data,
    support_constraint = ~ !tie(emails)
  )
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

# ---- F4: composition change, the choice ------------------------------------

# F1's process with N3 and N7 leaving and returning. An absent node is no
# sender and no candidate receiver while it is away. There is no rate arm
# yet: the oracle splits the rate's exposure at every presence change, and
# goldfish does not yet close the rate interval there, so the two would
# integrate different risk sets.
f4_simulation <- function() {
  composition <- data.frame(
    time = c(1.5, 2.5, 4, 6),
    node = c(3L, 7L, 3L, 7L),
    present = c(FALSE, FALSE, TRUE, TRUE)
  )
  sim <- simulate_relational_sequence(
    n_actors = 10,
    n_events = 150,
    processes = list(list(rate = c(-1, 0.3, -0.2), choice = c(1, 0.4))),
    seed = 42,
    composition = composition
  )
  sim$data <- dgp_stocnet(sim, n_actors = 10)
  sim
}

f4_spec <- function(sim) {
  make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ recip + trans,
    layer = "calls",
    model = "DyNAM",
    data = sim$data
  )
}

test_that("F4: the choice exports no row for an absent node", {
  sim <- f4_simulation()
  exported <- simulation_quiet(compute_statistics(
    f4_spec(sim),
    sub_model = "choice",
    output = "data.frame",
    control_prep = simulation_window()
  ))
  expect_frame_identity(
    exported,
    sim$choice[[1]],
    c(recip = "recip_calls", trans = "trans_calls")
  )
  time <- sim$events$time[exported$event]
  away <- (exported$index_j == 3L & time > 1.5 & time <= 4) |
    (exported$index_j == 7L & time > 2.5 & time <= 6)
  expect_false(any(away))
})

test_that("F4: the choice under composition change is a conditional logit", {
  sim <- f4_simulation()
  fit <- simulation_quiet(estimate_dynam(
    f4_spec(sim),
    sub_model = "choice",
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  ))
  expect_choice_equals_clogit(
    fit,
    sim$choice[[1]],
    outcome ~ recip + trans + strata(interval)
  )
})

test_that("F4: the handle replays the composition, absent nodes excluded", {
  replay <- walk_replay_against_batch(f4_spec(f4_simulation()))
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

# ---- F5: one modeled flavor of a two-flavor layer ---------------------------

# F2's sequence with creation alone modeled: dissolution events still happen
# and move the state, but no process has them as dependent events, so each
# closes the creation rate's interval with a censored row.
f5_spec <- function(sim) {
  make_specification(
    rate = list(f2_formula("creation", "rate")),
    choice = list(f2_formula("creation", "choice")),
    model = "DyNAM",
    data = sim$data
  )
}

test_that("F5: dissolution events are censored rows of the creation rate", {
  sim <- f2_simulation(unmodeled = "dissolution")
  expect_named(sim$rate, c("creation", "dissolution"))
  expect_identical(sim$rate$dissolution[0, ], sim$rate$dissolution)
  expect_identical(sim$rate$creation, f2_simulation()$rate$creation)
  exported <- simulation_quiet(compute_statistics(
    f2_standalone_spec(sim, "creation", "rate"),
    sub_model = "rate",
    output = "data.frame",
    control_prep = simulation_window()
  ))
  expect_frame_identity(exported, sim$rate$creation, f2_stats("rate"))
  censored <- unique(exported$event[
    !exported$event %in%
      exported$event[
        exported$chosen == 1L
      ]
  ])
  expect_length(censored, sum(sim$events$flavor == "dissolution"))
})

test_that("F5: the creation-only fit equals F2's creation blocks and oracle", {
  sim <- f2_simulation(unmodeled = "dissolution")
  both <- simulation_quiet(estimate_dynam(
    f2_container_spec(f2_simulation()),
    control_prep = simulation_window(),
    control_algo = simulation_tight_fit()
  ))
  alone <- lapply(c(rate = "rate", choice = "choice"), function(family) {
    simulation_quiet(estimate_dynam(
      f5_spec(sim),
      sub_model = family,
      control_prep = simulation_window(),
      control_algo = simulation_tight_fit()
    ))
  })
  for (family in names(alone)) {
    expect_equal(
      unname(coef(alone[[family]])),
      unname(coef(fit_of(both, "creation", family))),
      tolerance = simulation_tolerance,
      label = family
    )
  }
  expect_rate_equals_poisson(
    alone$rate,
    sim$rate$creation,
    outcome ~ indeg + outdeg + offset(log(dt))
  )
  expect_choice_equals_clogit(
    alone$choice,
    sim$choice$creation,
    outcome ~ recip + trans + strata(interval)
  )
})

test_that("F5: the handle replays the modeled flavor, its mask excluding", {
  replay <- walk_replay_against_batch(f5_spec(f2_simulation()))
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

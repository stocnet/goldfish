# The per-actor availability primitive: the denominators the per-actor margins
# are read against. Two quantities, both counted per ACTOR MEMBERSHIP rather
# than per risk-set position -- `exposure`, the time at risk summed over every
# interval, and `n_opportunities`, the number of dependent events whose realized
# risk set contained the actor. The distinction is what the dyadic bound test
# below exists to hold: on a one-mode REM a position walk would report an
# exposure many times the observation window.

availability_control <- function(backend = "cpp", extra = character(0)) {
  set_algorithm_newton(
    diagnostics = c("loglik", "margins", "availability", extra),
    backend = backend
  )
}

fit_rate_availability <- function(backend = "cpp") {
  estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = availability_control(backend)
  )
}

# The same rate process read over an exogenous layer as well, so its events
# open right-censored intervals: the fixture where exposure (all intervals) and
# opportunities (dependent events only) can disagree.
fit_rate_censored <- function() {
  estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = availability_control(),
    return_preprocessed = TRUE
  )
}

test_that("availability rides along with the fit, labeled by actor", {
  fit <- fit_rate_availability()

  expect_named(fit$availability, c("exposure", "n_opportunities"))
  expect_length(fit$availability$exposure, nrow(actors_ex))
  expect_equal(names(fit$availability$exposure), actors_ex$label)
  expect_equal(names(fit$availability$n_opportunities), actors_ex$label)
  # Requesting no availability leaves the component off entirely, rather than
  # storing zeros that would read as "nobody was ever at risk".
  bare <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  expect_null(bare$availability)
})

test_that("a two-sided family reports each side separately", {
  fit <- estimate_wrapper(
    depNetwork ~ 1 + inertia + recip,
    model = "REM",
    sub_model = "rate",
    data = dataTest,
    control_algo = availability_control()
  )

  expect_named(
    fit$availability,
    c(
      "exposure_sender",
      "exposure_receiver",
      "n_opportunities_sender",
      "n_opportunities_receiver"
    )
  )
  # The suffix rule is the margins' own, so the two components describe the
  # same actor set under the same names.
  expect_equal(
    names(fit$availability$exposure_sender),
    names(fit$availability$exposure_receiver)
  )
  expect_equal(
    names(fit$availability$exposure_sender),
    names(fit$margins$observed_sender)
  )
})

test_that("the compensator-scale vector is absent, silently, off exact-time", {
  expect_no_warning(
    fit <- estimate_wrapper(
      depNetwork ~ inertia + recip,
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      control_algo = availability_control()
    )
  )

  expect_named(fit$availability, "n_opportunities")
  expect_null(fit$availability$exposure)
})

test_that("a dyadic fit counts intervals and events, not dyads", {
  skip_on_cran()
  data <- baselines_social_evolution_data()
  fit <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    data = data,
    control_algo = availability_control(),
    return_preprocessed = TRUE
  )

  window <- fit$preprocessed$end_time - fit$preprocessed$start_time
  n_dependent <- sum(!fit$right_censored_events)
  # 84 actors, so a REM risk set holds 6 972 dyads of which 166 contain any
  # given actor: counting positions rather than membership would report both
  # quantities orders of magnitude above these bounds.
  expect_lte(max(fit$availability$exposure_sender), window)
  expect_lte(max(fit$availability$exposure_receiver), window)
  expect_lte(max(fit$availability$n_opportunities_sender), n_dependent)
  expect_lte(max(fit$availability$n_opportunities_receiver), n_dependent)
  # Nobody is at risk in every interval on this fixture only if presence
  # varies; here it does not, so the bounds are attained rather than merely
  # respected -- which is what pins the count at exactly one per interval.
  expect_equal(max(fit$availability$exposure_sender), window)
  expect_equal(max(fit$availability$n_opportunities_sender), n_dependent)
})

test_that("exposure honors composition changes", {
  fit <- fit_rate_censored()
  prep <- fit$preprocessed

  # Rebuild presence independently of the engine: the initial `present` column
  # plus the composition-change events, replayed onto the interval clock. An
  # interval's risk set is the one in force at its END, which is where the
  # engine applies every pending composition update (`time <= current_time`).
  expected <- vapply(
    seq_len(nrow(actors_ex)),
    function(actor) {
      changes <- compChange[compChange$node == actors_ex$label[actor], ]
      changes <- changes[order(changes$time), , drop = FALSE]
      at_risk <- vapply(
        prep$event_time,
        function(time) {
          past <- changes$time <= time
          if (any(past)) {
            changes$replace[max(which(past))]
          } else {
            actors_ex$present[actor]
          }
        },
        logical(1)
      )
      sum(prep$intervals[at_risk])
    },
    numeric(1)
  )

  expect_equal(unname(fit$availability$exposure), expected)
  # Actor 5 starts absent and leaves again, so its time at risk is strictly
  # below the window every ever-present actor attains.
  window <- prep$end_time - prep$start_time
  expect_lt(fit$availability$exposure[["Actor 5"]], window)
  expect_equal(fit$availability$exposure[["Actor 2"]], window)
})

test_that("opportunities count dependent events only", {
  fit <- fit_rate_censored()
  n_dependent <- sum(!fit$right_censored_events)

  # This fixture's exogenous layer opens right-censored intervals, which is
  # what separates the two quantities: exposure integrates over all of them,
  # opportunities over the dependent events alone.
  expect_gt(length(fit$right_censored_events), n_dependent)
  expect_equal(max(fit$availability$n_opportunities), n_dependent)
  expect_equal(
    max(fit$availability$exposure),
    fit$preprocessed$end_time - fit$preprocessed$start_time
  )
})

test_that("the same vectors come out of every backend", {
  fits <- lapply(
    c("cpp", "gather", "r"),
    function(backend) fit_rate_availability(backend)$availability
  )

  expect_equal(fits[[2]], fits[[1]])
  expect_equal(fits[[3]], fits[[1]])
})

test_that("evaluated availability equals stored availability", {
  for (backend in c("cpp", "gather", "r")) {
    stored <- estimate_wrapper(
      depNetwork ~ 1 + indeg + outdeg,
      model = "DyNAM",
      sub_model = "rate",
      data = dataTest,
      control_algo = availability_control(backend)
    )
    plain <- estimate_wrapper(
      depNetwork ~ 1 + indeg + outdeg,
      model = "DyNAM",
      sub_model = "rate",
      data = dataTest,
      control_algo = set_algorithm_newton(backend = backend),
      return_preprocessed = TRUE
    )
    evaluated <- evaluate_model(
      plain,
      return = c("exposure", "n_opportunities")
    )

    expect_null(plain$availability)
    expect_equal(evaluated$exposure, stored$availability$exposure)
    expect_equal(
      evaluated$n_opportunities,
      stored$availability$n_opportunities
    )
  }
})

test_that("the evaluator refuses exposure where it is not defined", {
  fit <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = "loglik"),
    return_preprocessed = TRUE
  )

  expect_snapshot(error = TRUE, evaluate_model(fit, return = "exposure"))
  # The defined alternative the error names is available on the same fit.
  expect_length(
    evaluate_model(fit, return = "n_opportunities")$n_opportunities,
    nrow(actors_ex)
  )
})

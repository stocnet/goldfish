# The rate's exposure: the sum over the rows the rate likelihood integrates of
# each row's interval times the entity count the engine applies at that row.
# Its defining property is that `n_dep_events / exposure_entity_time` is the
# maximum-likelihood rate of an intercept-only model, which is what the
# intercept seed and a completed rate's pin both read. An intercept-only fit
# is written `~ 1 + offset(x, coef = 0)`, since a bare `~ 1` does not parse.

# Five actors on one node set; N2 leaves at 3 and returns at 7.5, so the
# intervals in which four and five senders are present differ in length and a
# per-event average of the risk set differs from the time-weighted one. The
# leave lands on a row two units long and the return on a row one unit long,
# so counting a row's flips after the row, not before it, moves the exposure.
composition_change_data <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:5),
    mode = "p",
    active = TRUE,
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 3L, 1L, 4L, 5L, 2L, 3L, 4L),
    to = c(2L, 4L, 3L, 5L, 1L, 3L, 1L, 2L),
    time = c(1, 2, 4, 5, 7, 8, 9.5, 11),
    layer = "calls",
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    time = c(3, 7.5),
    node = c(2L, 2L),
    var = "active",
    stringsAsFactors = FALSE
  )
  changes$value <- list(list(FALSE), list(TRUE))
  info <- list(
    name = "composition",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  list(info = info, nodes = nodes, ties = ties, changes = changes)
}

# The two-mode fixture with a sender that leaves at 6.5 and returns at 9.5,
# between its own events at 6 and 10.
twomode_composition_data <- function() {
  data <- make_stocnet_fixture_twomode_estimable()
  data$nodes$active <- TRUE
  presence <- data.frame(
    time = c(6.5, 9.5),
    node = c(4L, 4L),
    var = "active",
    stringsAsFactors = FALSE
  )
  presence$value <- list(list(FALSE), list(TRUE))
  data$changes <- rbind(data$changes, presence)
  data$changes <- data$changes[order(data$changes$time), ]
  data
}

tight_newton <- function() {
  set_algorithm_newton(
    score_tol = 1e-13,
    step_tol = 1e-13,
    max_iterations = 200
  )
}

expect_exposure_is_intercept_mle <- function(spec, model, estimate) {
  prep <- suppressWarnings(suppressMessages(
    compute_statistics(spec, model = model, sub_model = "rate")
  ))
  fit <- suppressWarnings(suppressMessages(
    estimate(spec, sub_model = "rate", control_algo = tight_newton())
  ))
  expect_equal(
    prep$n_dep_events / prep$exposure_entity_time,
    exp(unname(coef(fit)[[1L]])),
    tolerance = 1e-9
  )
  expect_equal(
    prep$avg_active_entity,
    prep$exposure_entity_time / prep$total_time
  )
  prep
}

test_that("a DyNAM rate's exposure is the intercept-only MLE's denominator", {
  data <- composition_change_data()
  spec <- make_specification(
    rate = ~ 1 + offset(indeg, coef = 0),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- expect_exposure_is_intercept_mle(spec, "DyNAM", estimate_dynam)

  # The per-event average weighs the rows N2 is absent equally with the rest;
  # the exposure weighs them by how long they last.
  sizes <- vapply(
    walk_presence_buffer(
      prep$active_sender_init,
      prep$active_sender_update,
      prep$active_sender_update_pointer,
      length(prep$event_time)
    ),
    sum,
    numeric(1)
  )
  expect_true(any(sizes == 4))
  expect_false(isTRUE(all.equal(prep$avg_active_entity, mean(sizes))))
})

test_that("a one-mode REM counts active dyads without the self-dyad", {
  data <- composition_change_data()
  spec <- make_specification(
    rate = ~ 1 + offset(inertia, coef = 0),
    layer = "calls",
    model = "REM",
    data = data
  )
  prep <- expect_exposure_is_intercept_mle(spec, "REM", estimate_rem)
  # Twelve dyads while N2 is away, twenty otherwise.
  expect_gt(prep$avg_active_entity, 12)
  expect_lt(prep$avg_active_entity, 20)
})

test_that("a two-mode REM counts every sender-receiver pair", {
  data <- twomode_composition_data()
  spec <- make_specification(
    rate = ~ 1 + offset(inertia, coef = 0),
    layer = "membership",
    model = "REM",
    data = data
  )
  prep <- expect_exposure_is_intercept_mle(spec, "REM", estimate_rem)
  # Three senders by three receivers while P4 is away, twelve pairs otherwise.
  expect_gt(prep$avg_active_entity, 9)
  expect_lt(prep$avg_active_entity, 12)
})

test_that("a constrained DyNAM rate counts the senders the gate leaves", {
  data <- self_only_sender_data()
  spec <- make_specification(
    rate = list(creation ~ 1 + offset(indeg, coef = 0)),
    model = "DyNAM",
    data = data
  )
  prep <- suppressMessages(preprocess_joint(spec, set_preprocessing()))[[1L]]
  fit <- suppressWarnings(suppressMessages(
    estimate_dynam(spec, sub_model = "rate", control_algo = tight_newton())
  ))
  expect_equal(
    prep$n_dep_events / prep$exposure_entity_time,
    exp(unname(coef(fit)[[1L]])),
    tolerance = 1e-9
  )
})

test_that("each flavor of a flavored rate carries its own exposure", {
  data <- flavored_fixture_data()
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- suppressWarnings(suppressMessages(
    compute_statistics(spec, model = "DyNAM", sub_model = "rate")
  ))
  for (out in prep[c("1", "2")]) {
    expect_gt(out$exposure_entity_time, 0)
    expect_equal(
      out$avg_active_entity,
      out$exposure_entity_time / out$total_time
    )
  }
  expect_gt(prep[["1"]]$exposure_entity_time, prep[["2"]]$exposure_entity_time)
})

test_that("an output with no stored rows has zero exposure and risk set", {
  out <- list(
    event_time = numeric(0),
    intervals = numeric(0),
    active_sender_init = c(TRUE, TRUE, TRUE),
    active_dyad_init = c(TRUE, TRUE, TRUE),
    active_dyad_encoding = "outer"
  )
  expect_identical(risk_set_exposure(out, "sender", drop_self = TRUE), 0)
  expect_identical(risk_set_exposure(out, "dyad", drop_self = TRUE), 0)
})

test_that("an ordered rate carries no exposure", {
  data <- composition_change_data()
  spec <- make_specification(
    rate = ~indeg,
    rate_sub_model = "rate_ordered",
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- suppressWarnings(suppressMessages(
    compute_statistics(spec, model = "DyNAM", sub_model = "rate_ordered")
  ))
  expect_null(prep$exposure_entity_time)
  expect_null(prep$avg_active_entity)
})

test_that("the preprocessed print lists the exposure", {
  data <- composition_change_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- suppressWarnings(suppressMessages(
    compute_statistics(spec, model = "DyNAM", sub_model = "rate")
  ))
  expect_output(print(prep), "exposure_entity_time")
})

test_that("a constrained REM counts the dyads its support leaves", {
  # Every observed call is a new tie, so `!tie(calls)` never excludes one.
  data <- composition_change_data()
  spec <- make_specification(
    rate = ~ 1 + offset(inertia, coef = 0),
    layer = "calls",
    model = "REM",
    data = data,
    support_constraint = ~ !tie(calls)
  )
  prep <- expect_exposure_is_intercept_mle(spec, "REM", estimate_rem)
  expect_identical(prep$active_dyad_encoding, "point")
})

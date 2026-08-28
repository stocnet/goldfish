# `test_time()`: is a coefficient constant over the sequence? Both methods are
# exact score tests of an augmented model, so the first thing to hold is that
# the trend arm reproduces the classical proportionality test it is the
# relational-event counterpart of -- that reference is the reason the kernel
# gained a weighted-information return at all.

# The frozen classical references, loaded the same way
# `test-classical_equivalence.R` loads them: these are numbers minted by
# `survival`, not by goldfish, so a disagreement is a goldfish bug.
time_classical_references <- function() {
  path <- test_path("_references", "classical_v1", "classical_references.rds")
  skip_if_not(file.exists(path), "classical references not minted")
  readRDS(path)
}

time_fixture <- function(...) {
  data("social_evolution", envir = environment())
  suppressMessages(estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    return_preprocessed = TRUE,
    ...
  ))
}

# A DyNAM choice sequence simulated in two regimes: the receiver of each event
# is drawn from the multinomial the choice sub-model specifies, under one
# coefficient vector before the cut and another after it. One fitted
# coefficient is then a compromise neither half holds, which is exactly the
# alternative both methods are meant to have power against -- and when the two
# vectors are equal the same generator is the time-constant null.
#
# The pre-stocnet constructors are what make a plain data frame of events both
# the network's history and the dependent sequence; their deprecation warnings
# are silenced because this fixture is about the regime change, not the
# builders.
simulate_two_regime_choice <- function(
  n_actors,
  n_events,
  theta_early,
  theta_late,
  seed
) {
  withr::local_seed(seed)
  withr::local_options(lifecycle_verbosity = "quiet")
  labels <- sprintf("A%d", seq_len(n_actors))
  network <- matrix(0, n_actors, n_actors, dimnames = list(labels, labels))
  cut <- floor(n_events / 2)
  sender <- integer(n_events)
  receiver <- integer(n_events)
  for (k in seq_len(n_events)) {
    theta <- if (k <= cut) theta_early else theta_late
    from <- sample.int(n_actors, 1L)
    alternatives <- setdiff(seq_len(n_actors), from)
    eta <- theta[["inertia"]] *
      network[from, alternatives] +
      theta[["recip"]] * network[alternatives, from]
    to <- alternatives[sample.int(length(alternatives), 1L, prob = exp(eta))]
    sender[k] <- from
    receiver[k] <- to
    network[from, to] <- 1
  }
  events <- data.frame(
    time = seq_len(n_events),
    sender = labels[sender],
    receiver = labels[receiver],
    increment = 1
  )
  actors <- make_nodes(data.frame(label = labels, present = TRUE))
  net <- make_network(nodes = actors, directed = TRUE)
  net <- link_events(net, events, nodes = actors)
  dependent <- make_dependent_events(
    events = events,
    nodes = actors,
    default_network = net
  )
  list(
    cut = cut + 0.5,
    fit = estimate_dynam(
      dependent ~ inertia + recip,
      sub_model = "choice",
      data = make_data(dependent, net, actors),
      return_preprocessed = TRUE
    )
  )
}

# The Cox-equivalent fixture: the same ordinal REM whose classical twin minted
# the frozen `cox_zph` table, built exactly as `test-classical_equivalence.R`
# builds it so the two read the same fit.
time_cox_fixture <- function() {
  data("social_evolution", envir = environment())
  calls <- social_evolution$ties[social_evolution$ties$layer == "calls", ]
  calls <- calls[order(as.numeric(calls$time)), ]
  suppressMessages(estimate_rem(
    calls ~ inertia(calls, weighted = TRUE) + recip(calls, weighted = TRUE),
    sub_model = "rate_ordered",
    data = social_evolution,
    control_prep = set_preprocessing(
      start_time = min(as.numeric(calls$time)) - 1
    ),
    return_preprocessed = TRUE
  ))
}

test_that("the trend test reproduces the classical proportionality test", {
  skip_on_cran()
  references <- time_classical_references()
  fit <- time_cox_fixture()

  for (transform in c("identity", "rank")) {
    result <- test_time(fit, method = "trend", transform = transform)
    reference <- references$cox_zph[[transform]]$table

    # Per effect and then the joint test, in the reference's own row order.
    expect_equal(
      result$effects$statistic,
      unname(reference[seq_len(nrow(result$effects)), "chisq"]),
      tolerance = 1e-6
    )
    expect_equal(result$effects$df, rep(1L, nrow(result$effects)))
    expect_equal(
      attr(result, "context")$global$statistic,
      unname(reference["GLOBAL", "chisq"]),
      tolerance = 1e-6
    )
    expect_equal(
      attr(result, "context")$global$df,
      unname(reference["GLOBAL", "df"])
    )
  }
})

test_that("an ordinal fit reads identity and rank as the same clock", {
  # The event index already is the rank, so the two transforms coincide -- and
  # that they do is the check that the transform is applied to the model's own
  # clock rather than to wall-clock time.
  fit <- time_fixture()
  expect_equal(
    test_time(fit, transform = "identity")$effects$statistic,
    test_time(fit, transform = "rank")$effects$statistic
  )
})

test_that("both methods need the statistics and say so", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  data("social_evolution", envir = environment())
  fit <- suppressMessages(estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution
  ))
  expect_null(fit$preprocessed)
  expect_snapshot(test_time(fit), error = TRUE)
  expect_snapshot(test_time(fit, method = "periods"), error = TRUE)

  # And the supplied route works, which is the other half of what the abort
  # names.
  supplied <- compute_statistics(
    calls ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = social_evolution
  )
  expect_equal(
    test_time(fit, preprocessed = supplied)$effects$statistic,
    test_time(time_fixture())$effects$statistic
  )
})

test_that("the periods method detects a coefficient that changes regime", {
  skip_on_cran()
  # Two regimes glued together: the receiver of each event is drawn with a
  # strong inertia coefficient in the first half and a reversed one in the
  # second, so a single fitted coefficient is a compromise neither half holds.
  fixture <- simulate_two_regime_choice(
    n_actors = 12L,
    n_events = 400L,
    theta_early = c(inertia = 2.5, recip = 0),
    theta_late = c(inertia = -1.5, recip = 0),
    seed = 11L
  )
  fit <- fixture$fit
  cut <- fixture$cut

  regimes <- test_time(
    fit,
    method = "periods",
    periods = c(cut),
    effects = "inertia"
  )
  expect_lt(regimes$effects$p_value, 0.05)

  # The one-step deltas carry the simulated direction: the later period pulls
  # the coefficient down from the pooled estimate.
  deltas <- regimes$periods
  expect_equal(deltas$delta[1], 0)
  expect_lt(deltas$delta[2], 0)
})

test_that("the trend test is quiet on a time-constant coefficient", {
  skip_on_cran()
  # Same generator, one regime: nothing drifts, so the test should not reject.
  fixture <- simulate_two_regime_choice(
    n_actors = 12L,
    n_events = 400L,
    theta_early = c(inertia = 1.5, recip = 0),
    theta_late = c(inertia = 1.5, recip = 0),
    seed = 5L
  )
  result <- test_time(fixture$fit, effects = "inertia")
  expect_gt(result$effects$p_value, 0.05)
})

test_that("periods accepts a count, cut times, and a grouping", {
  fit <- time_fixture()
  n <- length(fit$preprocessed$is_dependent)

  by_count <- test_time(fit, method = "periods", periods = 3L)
  expect_equal(by_count$effects$df, rep(2L, nrow(by_count$effects)))
  expect_equal(
    sort(unique(by_count$periods$period)),
    c("period_1", "period_2", "period_3")
  )

  # Cut times on the model's own clock, which is the event index here.
  by_cuts <- test_time(fit, method = "periods", periods = c(n / 3, 2 * n / 3))
  expect_equal(by_cuts$effects$df, rep(2L, nrow(by_cuts$effects)))

  # A full-length grouping is used verbatim, which is how an exogenous regime
  # is supplied.
  grouping <- rep(c("a", "b"), length.out = n)
  by_group <- test_time(fit, method = "periods", periods = grouping)
  expect_equal(by_group$effects$df, rep(1L, nrow(by_group$effects)))
  expect_equal(sort(unique(by_group$periods$period)), c("a", "b"))

  # Every period carries a delta, the first being the reference at zero.
  expect_equal(
    by_count$periods$delta[by_count$periods$period == "period_1"],
    rep(0, nrow(by_count$effects))
  )
})

test_that("the object carries plot-ready residual data", {
  fit <- time_fixture()
  result <- test_time(fit)
  n <- length(fit$preprocessed$is_dependent)

  expect_named(result, c("effects", "residuals", "periods"))
  expect_equal(nrow(result$residuals), n * nrow(result$effects))
  # The residual column is the residuals method's own scaled Schoenfeld, not a
  # restatement of it, so a scatter from either source is the same numbers.
  scaled <- stats::residuals(fit, type = "scaled_schoenfeld")
  expect_equal(
    result$residuals$residual[result$residuals$index == 1L],
    as.numeric(scaled[, 1])
  )
  # The trend arm has no periods, and reports that as an empty table rather
  # than a missing component, so the schema is the same under both methods.
  expect_equal(nrow(result$periods), 0L)
})

test_that("a selection and a family name choose what is tested", {
  fit <- time_fixture()
  all_effects <- test_time(fit)
  one <- test_time(fit, effects = "recip")

  expect_equal(one$effects$index, 2L)
  expect_equal(one$effects$statistic, all_effects$effects$statistic[2])
  # The joint test follows the selection rather than the whole model.
  expect_equal(attr(one, "context")$global$df, 1L)
})

test_that("a fixed coefficient is excluded, and naming one has a destination", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  data("social_evolution", envir = environment())
  fit <- suppressMessages(estimate_dynam(
    calls ~ inertia + offset(recip, coef = 0.5),
    sub_model = "choice",
    data = social_evolution,
    return_preprocessed = TRUE
  ))
  expect_equal(test_time(fit)$effects$index, 1L)
  expect_snapshot(test_time(fit, effects = "recip"), error = TRUE)
})

test_that("test_time rejects a grouping it cannot use", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- time_fixture()
  expect_snapshot(
    test_time(fit, method = "periods", periods = 1L),
    error = TRUE
  )
  expect_snapshot(
    test_time(fit, method = "periods", periods = c(-1e9, 1e9)),
    error = TRUE
  )
  expect_snapshot(
    test_time(fit, method = "periods", periods = c("a", "b")),
    error = TRUE
  )
})

test_that("the print reports the effects and the joint test", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- time_fixture()
  expect_snapshot(print(test_time(fit)))
  expect_snapshot(print(test_time(fit, method = "periods", periods = 2L)))
})

# The specification (multi-process) fit: each block is an independent fit, so
# the container may not compute anything of its own.

time_container <- function() {
  suppressWarnings(estimate_dynam(
    make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      choice = list(creation ~ trans, dissolution ~ trans),
      model = "DyNAM",
      data = flavored_fixture_data()
    ),
    return_preprocessed = TRUE
  ))
}

test_that("a flavored fit is tested one process at a time", {
  container <- time_container()
  blocked <- test_time(container)
  map <- container$process_map

  for (i in seq_len(nrow(map))) {
    single <- test_time(fit_of(container, map$flavor[i], map$family[i]))
    rows <- blocked$effects$flavor == map$flavor[i] &
      blocked$effects$family == map$family[i]
    expect_equal(blocked$effects$statistic[rows], single$effects$statistic)
    expect_equal(blocked$effects$p_value[rows], single$effects$p_value)
  }
  # The two identity columns are appended, so a plot facets on them rather
  # than needing a flavored plot method of its own.
  expect_contains(names(blocked$effects), c("flavor", "family"))
  expect_contains(names(blocked$residuals), c("flavor", "family"))
})

test_that("the blocked print names the processes and reports no joint test", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  expect_snapshot(print(test_time(time_container())))
})

test_that("the per-term table carries the same defined order", {
  # The same screening contract `test_gof()` has, on the other per-term table.
  # `df` is constant within a call here, which is what makes the statistics
  # comparable enough for a rank to mean anything.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())
  fit <- estimate_dynam(
    calls ~ 1 + indeg(calls) + outdeg(calls) + indeg(friendship),
    sub_model = "rate",
    data = social_evolution,
    return_preprocessed = TRUE,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )

  effects <- test_time(fit)$effects
  expect_true("rank" %in% names(effects))
  expect_setequal(effects$rank, seq_len(nrow(effects)))
  expect_identical(
    effects$term[effects$rank == 1L],
    effects$term[which.max(effects$statistic)]
  )
  expect_length(unique(effects$df), 1L)
})

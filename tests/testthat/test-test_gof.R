# `test_gof()`: the cumulative score processes, their supremum, and the two
# references a p-value can come from. Everything here reads stored score rows,
# so the first thing to hold is that nothing else is touched.

# The rows the test standardizes: one per dependent event, which is the basis
# `test_gof()` reads. Accumulating is the identity on a multinomial fit, where
# every span holds one interval, and is not on an exact-time fit carrying
# right-censored ones.
gof_rows <- function(fit) {
  goldfish:::accumulate_over_events(fit$event_scores, fit)
}

gof_fixture <- function(
  formula = depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
  control_algo = set_algorithm_newton(diagnostics = "scores"),
  ...
) {
  estimate_wrapper(
    formula,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = control_algo,
    ...
  )
}

# A cold-start DyNAM choice sequence, simulated from the model goldfish then
# fits: the receiver of each event is drawn from the multinomial the choice
# sub-model specifies, over an initially EMPTY network. That start is the
# point of the fixture -- while the history is empty every alternative looks
# alike, the score contributions are exactly zero, and the information arrives
# late in the sequence rather than proportionally.
#
# `inertia` and `recip` are unweighted by default, so a repeated dyad raises the
# stored tie weight without changing either statistic, and the binary network
# carried here is exactly the one the fitted statistics see.
simulate_cold_choice <- function(n_actors, n_events, theta) {
  labels <- sprintf("A%d", seq_len(n_actors))
  network <- matrix(0, n_actors, n_actors, dimnames = list(labels, labels))
  sender <- integer(n_events)
  receiver <- integer(n_events)
  for (k in seq_len(n_events)) {
    from <- sample.int(n_actors, 1L)
    alternatives <- setdiff(seq_len(n_actors), from)
    eta <- theta[1] *
      network[from, alternatives] +
      theta[2] * network[alternatives, from]
    to <- alternatives[sample.int(length(alternatives), 1L, prob = exp(eta))]
    sender[k] <- from
    receiver[k] <- to
    network[from, to] <- 1
  }
  list(
    actors = data.frame(label = labels, present = TRUE),
    events = data.frame(
      time = seq_len(n_events),
      sender = labels[sender],
      receiver = labels[receiver],
      increment = 1
    )
  )
}

# The pre-stocnet constructors are what make a plain data frame of events both
# the network's history and the dependent sequence; their deprecation warnings
# are silenced because this fixture is about the cold start, not the builders.
fit_cold_choice <- function(sim) {
  withr::local_options(lifecycle_verbosity = "quiet")
  actors <- make_nodes(sim$actors)
  events <- sim$events
  network <- make_network(nodes = actors, directed = TRUE)
  network <- link_events(network, events, nodes = actors)
  dependent <- make_dependent_events(
    events = events,
    nodes = actors,
    default_network = network
  )
  estimate_dynam(
    dependent ~ inertia + recip,
    sub_model = "choice",
    data = make_data(dependent, network, actors),
    control_algo = set_algorithm_newton(diagnostics = "scores")
  )
}

test_that("the test reads stored scores only, on either clock", {
  fit <- gof_fixture()
  expect_null(fit$preprocessed)
  # If either clock reached for an evaluation pass this would abort, which is
  # the whole claim: both are arithmetic on the stored rows, the simulated
  # reference included.
  local_mocked_bindings(
    evaluate_model = function(...) stop("an evaluation pass was triggered")
  )
  gof <- test_gof(fit)
  simulated <- test_gof(fit, clock = "information", n_sim = 50)

  expect_s3_class(gof, "test_gof")
  expect_named(gof, c("effects", "process", "omnibus"))
  for (component in gof) {
    expect_s3_class(component, "tbl_df")
  }
  expect_identical(attr(gof, "diagnostic"), "test_gof")
  expect_identical(attr(gof, "params")$clock, "event")
  expect_identical(attr(simulated, "params")$clock, "information")
  expect_identical(attr(simulated, "params")$n_sim, 50L)
  expect_identical(
    attr(gof, "context")$n_intervals,
    nrow(fit$event_scores)
  )
  expect_identical(
    attr(gof, "context")$n_events,
    sum(!fit$right_censored_events)
  )
  expect_identical(
    attr(gof, "version"),
    as.character(utils::packageVersion("goldfish"))
  )
})

test_that("each process is a bridge, and every effect is free", {
  fit <- gof_fixture()
  gof <- test_gof(fit)

  # Starts at zero by construction, ends at zero because the score sums to
  # zero at the maximum -- which is what makes the excursion between them
  # readable against a bridge.
  origin <- gof$process$process[gof$process$step == 0]
  endpoint <- gof$process$process[
    gof$process$step == max(gof$process$step)
  ]
  expect_equal(origin, rep(0, nrow(gof$effects)))
  expect_equal(endpoint, rep(0, nrow(gof$effects)), tolerance = 1e-5)
  expect_identical(gof$effects$index, seq_len(nrow(fit$names)))
  expect_identical(
    nrow(gof$process),
    (nrow(gof_rows(fit)) + 1L) * nrow(gof$effects)
  )
  expect_true(all(gof$effects$p_value >= 0 & gof$effects$p_value <= 1))
  expect_identical(gof$omnibus$n_effects, nrow(gof$effects))
})

test_that("the statistic is the same under both clocks; the axis is not", {
  fit <- gof_fixture()
  event <- test_gof(fit)
  set.seed(42)
  information <- test_gof(fit, clock = "information", n_sim = 200)

  # A supremum reads the values a path takes, never where they are plotted, so
  # this is an identity rather than an approximation.
  expect_identical(event$effects$statistic, information$effects$statistic)
  expect_identical(event$effects$scale, information$effects$scale)
  expect_false(isTRUE(all.equal(
    event$effects$p_value,
    information$effects$p_value
  )))

  n <- nrow(gof_rows(fit))
  expect_identical(unique(event$process$clock), "event")
  expect_equal(
    event$process$u[event$process$index == 1],
    seq.int(0, n) / n
  )
  # The information clock places increment k at the share of that effect's
  # outer-product information delivered by it -- its own axis per effect.
  expect_identical(unique(information$process$clock), "information")
  for (d in seq_len(nrow(event$effects))) {
    squares <- gof_rows(fit)[, d]^2
    expect_equal(
      information$process$u[information$process$index == d],
      c(0, cumsum(squares) / sum(squares))
    )
  }
  expect_equal(
    information$process$process,
    event$process$process
  )
})

test_that("the simulated reference draws through the session RNG", {
  fit <- gof_fixture()
  set.seed(11)
  first <- test_gof(fit, clock = "information", n_sim = 200)
  set.seed(11)
  again <- test_gof(fit, clock = "information", n_sim = 200)
  set.seed(12)
  other <- test_gof(fit, clock = "information", n_sim = 200)

  expect_identical(first$effects$p_value, again$effects$p_value)
  expect_false(identical(first$effects$p_value, other$effects$p_value))
  # A Monte Carlo p-value is never exactly zero: it would claim more than the
  # replications support.
  expect_true(all(first$effects$p_value >= 1 / 201))
})

test_that("offset terms are excluded, and naming one has a destination", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- gof_fixture(depNetwork ~ 1 + indeg + offset(outdeg, coef = 0.5))
  gof <- test_gof(fit)

  expect_identical(gof$effects$index, 1:2)
  expect_identical(nrow(fit$names), 3L)
  expect_snapshot(
    test_gof(fit, effects = "outdeg/networkState [Fx]"),
    error = TRUE
  )
  expect_snapshot(test_gof(fit, effects = 3), error = TRUE)
})

test_that("a selection names the terms it tests", {
  fit <- gof_fixture()
  selected <- test_gof(fit, effects = c("indeg/networkState", "Intercept"))

  expect_identical(selected$effects$index, c(2L, 1L))
  expect_identical(selected$omnibus$n_effects, 2L)
  expect_identical(
    selected$effects$statistic,
    test_gof(fit)$effects$statistic[c(2L, 1L)]
  )
})

test_that("a fit without the score rows says which primitive to store", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- gof_fixture(
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  expect_null(fit$event_scores)
  expect_snapshot(test_gof(fit), error = TRUE)
})

test_that("the replication count is checked before anything is computed", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  expect_snapshot(test_gof(gof_fixture(), n_sim = 0), error = TRUE)
})

test_that("an effect contributing no score at all is named, not divided by", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  scores <- cbind(a = c(1, -2, 1), b = c(0, 0, 0))
  expect_snapshot(
    gof_processes(scores, "event", tested = 1:2),
    error = TRUE
  )
})

test_that("the Kolmogorov series is the bridge distribution", {
  # The 5% and 1% points of the supremum of a standard Brownian bridge, which
  # are also the Kolmogorov-Smirnov critical values, to the three digits they
  # are tabulated at.
  expect_equal(kolmogorov_p(1.358), 0.05, tolerance = 1e-2)
  expect_equal(kolmogorov_p(1.628), 0.01, tolerance = 1e-2)
  expect_identical(kolmogorov_p(0), 1)
  expect_true(all(diff(kolmogorov_p(c(0.5, 1, 2))) < 0))
  # Under arbitrary dependence: the Cauchy combination of p-values that are
  # all one half is one half.
  expect_equal(cauchy_omnibus(c(0.5, 0.5, 0.5))$p_value, 0.5)
  expect_lt(cauchy_omnibus(c(0.001, 0.5, 0.5))$p_value, 0.01)
})

test_that("print names the reference the p-values came from", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- gof_fixture()
  # Only the header is goldfish's: the table below it is tibble's print, and
  # pinning that would make this a regression test on pillar.
  header <- function(x) {
    out <- capture.output(print(x))
    cat(out[seq_len(which(startsWith(out, "# A tibble"))[1] - 1L)], sep = "\n")
  }
  expect_snapshot(header(test_gof(fit)))
  set.seed(3)
  expect_snapshot(header(test_gof(fit, clock = "information", n_sim = 100)))
})

test_that("on a cold start the clocks separate as documented", {
  skip_on_cran()
  # 300 replications of a correctly specified fit, roughly 25 seconds: each
  # replication simulates the sequence, fits it, and tests it under both
  # clocks. The claim being checked has three parts -- the statistics are
  # identical, the information-clock p-values are uniform, and the event-clock
  # ones deviate toward 1 -- and the third only means anything once the
  # fixture's accrual is SHOWN to be concentrated: under near-uniform accrual
  # the two references coincide and there would be nothing to separate.
  n_rep <- 300L
  set.seed(4711)
  event_p <- matrix(NA_real_, n_rep, 2L)
  information_p <- matrix(NA_real_, n_rep, 2L)
  gap <- numeric(n_rep)
  early_share <- numeric(n_rep)
  for (i in seq_len(n_rep)) {
    fit <- fit_cold_choice(simulate_cold_choice(30, 400, c(1.5, 1)))
    event <- test_gof(fit)
    information <- test_gof(fit, clock = "information", n_sim = 500)
    event_p[i, ] <- event$effects$p_value
    information_p[i, ] <- information$effects$p_value
    gap[i] <- max(abs(event$effects$statistic - information$effects$statistic))
    accrual <- diagnose_onset(fit)$accrual$share
    early_share[i] <- accrual[round(0.1 * length(accrual))]
  }

  expect_identical(max(gap), 0)
  # Concentrated accrual: a tenth of the sequence carries well under a tenth
  # of the information. This is the precondition for everything below.
  expect_lt(stats::median(early_share), 0.05)

  for (d in 1:2) {
    # Uniform enough: the rejection proportion sits at the nominal level
    # within simulation error, and the mean of a uniform is a half.
    expect_gt(mean(information_p[, d] < 0.05), 0.02)
    expect_lt(mean(information_p[, d] < 0.05), 0.09)
    expect_gt(mean(information_p[, d]), 0.46)
    expect_lt(mean(information_p[, d]), 0.56)
    # And the event clock is conservative on the same replications: the
    # continuous supremum it compares against is stochastically larger than
    # the one this grid delivers, so its p-values sit higher.
    expect_gt(mean(event_p[, d] - information_p[, d]), 0.02)
    expect_gt(mean(event_p[, d]), 0.53)
  }
})

# The specification (multi-process) fit. Blocks are `process_map` rows -- one
# flavor's one sub-model -- so a two-flavor rate + choice specification has
# four of them, each an independent fit with its own effect set.

gof_container <- function(...) {
  suppressWarnings(estimate_dynam(
    make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      choice = list(creation ~ trans, dissolution ~ trans),
      model = "DyNAM",
      data = flavored_fixture_data()
    ),
    control_algo = set_algorithm_newton(diagnostics = "scores"),
    ...
  ))
}

test_that("each block is tested as the standalone fit it is", {
  container <- gof_container()
  blocked <- test_gof(container)

  # The container's method must not compute anything of its own: each block's
  # rows have to equal what testing that process alone produces, since the
  # competing-flavor likelihood factorizes and nothing may be pooled.
  map <- container$process_map
  for (i in seq_len(nrow(map))) {
    single <- test_gof(fit_of(container, map$flavor[i], map$family[i]))
    rows <- blocked$effects$flavor == map$flavor[i] &
      blocked$effects$family == map$family[i]
    expect_equal(blocked$effects$statistic[rows], single$effects$statistic)
    expect_equal(blocked$effects$p_value[rows], single$effects$p_value)
    expect_equal(blocked$effects$scale[rows], single$effects$scale)
  }
})

test_that("the container shape is the single shape, row-bound and labeled", {
  container <- gof_container()
  blocked <- test_gof(container)
  single <- test_gof(fit_of(container, "creation", "rate"))

  expect_s3_class(blocked, "test_gof")
  # Same three components under the same names: a consumer never has to ask
  # whether a flavored result nested one level deeper.
  expect_named(blocked, c("effects", "process", "omnibus"))
  for (component in blocked) {
    expect_s3_class(component, "tbl_df")
  }
  # `flavor` and `family` are APPENDED, so the single-fit columns keep their
  # positions and a plot method facets on the two new ones.
  expect_identical(
    names(blocked$effects),
    c(names(single$effects), "flavor", "family")
  )
  expect_identical(
    names(blocked$process),
    c(names(single$process), "flavor", "family")
  )
  expect_identical(nrow(blocked$omnibus), nrow(container$process_map))
  expect_setequal(unique(blocked$effects$flavor), c("creation", "dissolution"))
  expect_setequal(unique(blocked$effects$family), c("rate", "choice"))
})

test_that("the joint omnibus is metadata, not a repeated column", {
  container <- gof_container()
  blocked <- test_gof(container)
  joint <- attr(blocked, "context")$joint

  # A scalar summarizing the whole object stays out of the rows.
  expect_false("joint" %in% names(blocked$effects))
  expect_s3_class(joint, "tbl_df")
  expect_identical(nrow(joint), 1L)
  expect_identical(joint$n_blocks, nrow(container$process_map))
  expect_identical(joint$n_effects, nrow(blocked$effects))
  # Taken over every block's effect-level p-values, not over the block
  # omnibuses: the effect is the unit being combined.
  expect_equal(joint$p_value, cauchy_omnibus(blocked$effects$p_value)$p_value)
  # And the per-block rows are each that block's own combination.
  for (i in seq_len(nrow(blocked$omnibus))) {
    rows <- blocked$effects$flavor == blocked$omnibus$flavor[i] &
      blocked$effects$family == blocked$omnibus$family[i]
    expect_equal(
      blocked$omnibus$p_value[i],
      cauchy_omnibus(blocked$effects$p_value[rows])$p_value
    )
  }
  # `n_blocks` belongs to the joint combination alone; a single fit's omnibus
  # keeps the three columns it has always had.
  expect_false("n_blocks" %in% names(blocked$omnibus))
})

test_that("the context describes the container, not one of its processes", {
  container <- gof_container()
  context <- attr(test_gof(container), "context")

  expect_identical(context$model, container$model)
  expect_identical(context$layer, container$layer)
  expect_setequal(context$flavor, container$flavors)
  expect_setequal(context$sub_model, unique(container$process_map$family))
  # Per-block counts stay vectors: a rate process counts intervals where its
  # choice counterpart counts events, and a total would sum unlike things.
  expect_length(context$n_intervals, nrow(container$process_map))
  expect_length(context$n_events, nrow(container$process_map))
  expect_false(anyNA(context$n_intervals))
})

test_that("a term absent from one process names that process", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  container <- gof_container()
  # `trans` is a choice-block term; the rate blocks do not carry it.
  expect_snapshot(test_gof(container, effects = "trans/calls"), error = TRUE)
})

test_that("the blocked print groups by process, with no combination", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  container <- gof_container()
  # What this snapshot owns is the layout -- the grouping, the headers, the
  # absence of a combined line. The numbers are redacted rather than pinned:
  # honoring ADR-0003, no test asserts a p-value on the flavored fixture, and a
  # layout snapshot that also fixes the arithmetic churns on every change to
  # the basis the statistic is read on.
  expect_snapshot(
    print(test_gof(container)),
    transform = function(lines) {
      gsub("-?[0-9]+\\.[0-9]+(e[-+][0-9]+)?", "<num>", lines)
    }
  )
})

test_that("the Cauchy combination survives a p-value at the pole", {
  # Both poles are reachable: the Kolmogorov series is clamped into the unit
  # interval and the simulated p-value is `(1 + exceed) / (1 + n_sim)`, which
  # is exactly 1 when every replication exceeds. Neither may produce a
  # non-finite statistic.
  expect_true(is.finite(cauchy_omnibus(c(1, 0.5))$statistic))
  expect_true(is.finite(cauchy_omnibus(c(0, 0.5))$statistic))
  expect_true(is.finite(cauchy_omnibus(c(0, 1))$statistic))
  for (p in list(c(1, 0.5), c(0, 0.5), c(0, 1))) {
    combined <- cauchy_omnibus(p)$p_value
    expect_gte(combined, 0)
    expect_lte(combined, 1)
  }
})

# The intercept row. On the exact-time families it is a column of ones, so its
# score is the counting-process martingale increment itself, and its test is a
# test of baseline constancy rather than of an effect's functional form.

test_that("the intercept's score is the event indicator minus the compensator", {
  fit <- gof_fixture(
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )
  intercept <- which(colnames(fit$event_scores) == "Intercept")
  expect_length(intercept, 1L)

  # dN_k - Dt_k * total_rate_k, and the second term is exactly the Cox-Snell
  # residual of the same interval. Identical, not merely close: the intercept
  # column of the statistics is 1 everywhere, so the two are the same
  # arithmetic.
  # Per interval on both sides: `event_scores` is a per-interval matrix, so the
  # compensator it is compared against is the per-interval one, not the
  # accumulated series `residuals()` now returns.
  observed <- as.numeric(!fit$right_censored_events)
  compensator <- fit$intervals * fit$total_rate
  expect_equal(fit$event_scores[, intercept], observed - compensator)

  # Hence the cumulative process is N(t) - Lambda(t), and its score equation
  # is N(T) = Lambda(T): the fit reproduces the observed number of events.
  expect_equal(sum(observed), sum(compensator), tolerance = 1e-5)
  gof <- test_gof(fit)
  path <- gof$process$process[gof$process$index == intercept]
  expect_equal(path[1], 0)
  expect_equal(path[length(path)], 0, tolerance = 1e-5)
})

test_that("an ordinal fit has no intercept row to test", {
  # rate_ordered conditions on the event times, so the intercept cancels in the
  # softmax and goldfish drops it upstream. A constant column there would have
  # identically zero score, which is the degeneracy the test aborts on -- so
  # the absence is what keeps that abort unreachable by this route.
  fit <- suppressWarnings(estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate_ordered",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = "scores")
  ))
  expect_false("Intercept" %in% colnames(fit$event_scores))
  expect_false(any(fit$right_censored_events))
  expect_false("Intercept" %in% test_gof(fit)$effects$term)
})

test_that("the omnibus is carried on the object but never printed", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- gof_fixture()
  gof <- test_gof(fit)
  blocked <- test_gof(gof_container())

  # Still computed and still reachable -- the suppression is a print decision,
  # not a contract change, so a saved object stays usable by the validation
  # study the badge points at.
  expect_s3_class(gof$omnibus, "tbl_df")
  expect_s3_class(blocked$omnibus, "tbl_df")
  expect_s3_class(attr(blocked, "context")$joint, "tbl_df")

  # And absent from every printed line, at both levels.
  printed <- c(
    capture.output(print(gof)),
    capture.output(print(blocked)),
    capture.output(print(gof), type = "message"),
    capture.output(print(blocked), type = "message")
  )
  expect_false(any(grepl("omnibus", printed, ignore.case = TRUE)))
  expect_false(any(grepl("Cauchy", printed, ignore.case = TRUE)))
})

test_that("an effect name tests every one of its terms", {
  # The fixture carries indeg twice, over the dependent and the exogenous
  # network. Selecting the effect must give both, and must give the same
  # statistics as naming the two terms individually -- the expansion decides
  # which columns are read, nothing else.
  fit <- gof_fixture()
  expect_equal(rownames(fit$names), c("Intercept", "indeg", "outdeg", "indeg"))

  family <- test_gof(fit, effects = "indeg")
  expect_identical(family$effects$index, c(2L, 4L))
  expect_identical(
    family$effects$statistic,
    test_gof(fit)$effects$statistic[c(2L, 4L)]
  )
  expect_identical(
    family,
    test_gof(fit, effects = c("indeg/networkState", "indeg/networkExog"))
  )
})

test_that("the test reads accumulated rows, not the intervals inside them", {
  # The reference distribution is a Brownian bridge, so the increments have to
  # be uncorrelated martingale differences. The score at distinct event times
  # is one; the intervals inside a single waiting time are not.
  #
  # On THIS fixture the within-span contributions correlate positively, so
  # accumulating raises the standardizing constant and lowers the statistic.
  # That direction is a property of the fixture and not of the correction --
  # the test below it measures a fit where it goes the other way -- so it is
  # asserted here and generalized nowhere.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())
  fit <- estimate_dynam(
    calls ~ 1 + indeg(calls) + indeg(calls, window = 300) + indeg(friendship),
    sub_model = "rate",
    data = social_evolution,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )
  expect_gt(fit$n_intervals, fit$n_events)

  tested <- goldfish:::gof_tested_effects(fit, NULL)
  per_interval <- goldfish:::gof_processes(
    fit$event_scores[, tested, drop = FALSE],
    "event",
    tested
  )
  accumulated <- goldfish:::gof_processes(
    goldfish:::accumulate_over_events(fit$event_scores, fit)[,
      tested,
      drop = FALSE
    ],
    "event",
    tested
  )

  expect_true(all(accumulated$scale > per_interval$scale))
  # And the statistic falls with it, on every tested effect: the path visits
  # the same partial sums at the span ends, over a coarser grid, divided by a
  # larger constant.
  expect_true(all(
    apply(abs(accumulated$standardized), 2, max) <
      apply(abs(per_interval$standardized), 2, max)
  ))

  # What `test_gof()` reports is the accumulated reading.
  expect_equal(
    unname(test_gof(fit)$effects$scale),
    unname(accumulated$scale)
  )
})

test_that("a multinomial fit is untouched by the accumulation", {
  # Every span there holds exactly one interval, so the regrouping is the
  # identity and the test is the one it always was -- statistics and p-values
  # alike. Asserted as an equality between two readings, not as a p-value.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + trans,
    sub_model = "choice",
    data = social_evolution,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )
  expect_identical(fit$n_events, fit$n_intervals)

  tested <- goldfish:::gof_tested_effects(fit, NULL)
  per_interval <- goldfish:::gof_processes(
    fit$event_scores[, tested, drop = FALSE],
    "event",
    tested
  )
  reported <- test_gof(fit)

  expect_equal(unname(reported$effects$scale), unname(per_interval$scale))
  expect_equal(
    unname(reported$effects$statistic),
    unname(apply(abs(per_interval$standardized), 2, max))
  )
  expect_equal(
    reported$effects$p_value,
    kolmogorov_p(apply(abs(per_interval$standardized), 2, max)),
    ignore_attr = TRUE
  )
})

test_that("the accumulation is not a one-directional smoothing", {
  # The counter-case to the test above, and the reason neither states a general
  # direction. An exact-time rate model's intercept contributes `1 - c1` on the
  # dependent interval of a span and `-c2` on a censored one, with `c1 + c2`
  # near 1 by the score equation -- so the accumulated row is near zero while
  # its parts are not. The within-span contributions correlate NEGATIVELY, the
  # standardizing constant falls rather than rises, and the per-interval
  # reading was conservative rather than anti-conservative.
  skip_on_cran()
  container <- flavored_container_fit()

  fit <- fit_of(container, "creation", "rate")
  expect_gt(fit$n_intervals, fit$n_events)

  tested <- goldfish:::gof_tested_effects(fit, NULL)
  per_interval <- goldfish:::gof_processes(
    fit$event_scores[, tested, drop = FALSE],
    "event",
    tested
  )
  accumulated <- goldfish:::gof_processes(
    gof_rows(fit)[, tested, drop = FALSE],
    "event",
    tested
  )

  expect_true(all(accumulated$scale < per_interval$scale))
  expect_equal(
    unname(test_gof(fit)$effects$scale),
    unname(accumulated$scale)
  )
})

test_that("the per-term table has a defined order, not a new row order", {
  # Screening has to survive having no screen: a fit that ran on a cluster
  # returns a table a script can take the front of. That is a column rather
  # than a re-sort, because the rows have an order already -- model order here,
  # and flavor-major on a container -- and a statistic-sorted table would undo
  # it.
  fit <- gof_fixture()
  effects <- test_gof(fit)$effects

  expect_true("rank" %in% names(effects))
  expect_setequal(effects$rank, seq_len(nrow(effects)))
  expect_identical(
    effects$term[effects$rank == 1L],
    effects$term[which.max(effects$statistic)]
  )
  # Rows are still in model order: the rank says where a term stands, it does
  # not move it.
  expect_identical(effects$index, seq_len(nrow(fit$names)))
})

test_that("selecting effects re-ranks over what is returned", {
  fit <- gof_fixture()
  selected <- test_gof(fit, effects = "indeg")$effects

  expect_setequal(selected$rank, seq_len(nrow(selected)))
  expect_identical(
    selected$term[selected$rank == 1L],
    selected$term[which.max(selected$statistic)]
  )
})

test_that("a container ranks within each process, not across them", {
  # Statistics from different processes are not comparable -- different risk
  # sets, different event counts -- so a rank pooled over the stacked table
  # would be a comparison nobody asked for.
  skip_on_cran()
  container <- flavored_container_fit(return_preprocessed = TRUE)
  effects <- test_gof(container)$effects

  key <- interaction(effects$flavor, effects$family, drop = TRUE)
  for (level in unique(key)) {
    rows <- which(key == level)
    expect_setequal(effects$rank[rows], seq_along(rows))
  }
  # And the block order is untouched by the ranking.
  expect_identical(
    unique(paste(effects$flavor, effects$family)),
    c(
      "creation rate",
      "creation choice",
      "dissolution rate",
      "dissolution choice"
    )
  )
})

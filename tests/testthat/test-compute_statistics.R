test_that("the compute_stats name is gone, with no stub", {
  # Deleted outright rather than deprecated: the name only ever existed in the
  # unreleased 2.0.0 development line, so no released user is served by a stub.
  expect_false("compute_stats" %in% getNamespaceExports("goldfish"))
  expect_false(exists("compute_stats", envir = asNamespace("goldfish")))
})

test_that("max_length bounds the produced statistic-column names", {
  gathered <- compute_statistics(
    depNetwork ~ inertia(networkState) + recip,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather",
    max_length = 8L
  )
  expect_lte(max(nchar(gathered$names_effects)), 8L)
  expect_identical(anyDuplicated(gathered$names_effects), 0L)
})

test_that("compute_statistics returns a preprocessed.goldfish object", {
  prep <- compute_statistics(
    depNetwork ~ inertia + recip,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
  expect_s3_class(prep$model_spec, "dynam_choice_spec")
})

test_that("compute_statistics matches the estimate preprocessing only output", {
  formulaTest <- depNetwork ~ inertia + recip
  prep <- compute_statistics(
    formulaTest,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  prepEstimate <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(prep, prepEstimate)
})

test_that("compute_statistics output is usable for estimation", {
  formulaTest <- depNetwork ~ inertia + recip
  prep <- compute_statistics(
    formulaTest,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  fitInit <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest,
    preprocessed = prep
  )
  fitDirect <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest
  )
  expect_equal(coef(fitInit), coef(fitDirect))
})

test_that("compute_statistics validates the output argument", {
  gathered <- compute_statistics(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  expect_true(!is.null(gathered$stat_all_events))
  expect_true(!is.null(gathered$selected))
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "db"
    ),
    "DBI connection"
  )
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "data.frame"
    )
  )
})

test_that("rate_ordered flows through, for REM and for DyNAM", {
  # The vocabulary is validated once downstream, so this surface reaches every
  # sub_model estimation reaches -- gather_model_data() rejected this one at its
  # own match.arg while estimation ran it.
  # A REM is dyad-indexed, a DyNAM rate model sender-indexed, so each takes the
  # effect its own risk-set axis supports.
  formulas <- list(
    REM = depNetwork ~ inertia(networkState),
    DyNAM = depNetwork ~ indeg(networkState)
  )
  for (model in c("REM", "DyNAM")) {
    gathered <- expect_no_warning(compute_statistics(
      formulas[[model]],
      data = dataTest,
      model = model,
      sub_model = "rate_ordered",
      output = "gather"
    ))
    expect_false(gathered$has_intercept)
    expect_false("(Intercept)" %in% colnames(gathered$stat_all_events))
  }
})

test_that("an exact-time model reports its intercept and censoring", {
  # A fixture with real censoring intervals: dataTest's events leave none.
  suppressWarnings(suppressMessages({
    data("Social_Evolution", envir = environment())
    call_network <- make_network(nodes = actors, directed = TRUE)
    call_network <- link_events(
      x = call_network,
      change_event = calls,
      nodes = actors
    )
    dep <- make_dependent_events(
      events = calls,
      nodes = actors,
      default_network = call_network
    )
    dep <- dep[1:80, ]
    d <- make_data(dep, call_network, calls, actors)
    gathered <- compute_statistics(
      dep ~ inertia(call_network),
      model = "REM",
      sub_model = "rate",
      data = d,
      output = "gather"
    )
  }))

  expect_true(gathered$has_intercept)
  expect_true(gathered$right_censored)
  expect_true("Intercept" %in% colnames(gathered$stat_all_events))
  # Right-censored rows are the ones the waiting-time likelihood needs the
  # exposure for, so they must carry timespan and be marked non-dependent.
  censored <- gathered$is_dependent == 0
  expect_gt(sum(censored), 0)
  expect_true(all(is.finite(gathered$timespan[censored])))
})

test_that("an ordinal model reports neither, on gather and on the replay object", {
  args <- list(
    depNetwork ~ inertia(networkState),
    data = dataTest,
    model = "REM",
    sub_model = "rate_ordered"
  )
  gathered <- do.call(compute_statistics, c(args, output = "gather"))
  prep <- do.call(compute_statistics, args)

  expect_false(gathered$has_intercept)
  expect_false(gathered$right_censored)
  expect_false("Intercept" %in% colnames(gathered$stat_all_events))
  expect_null(gathered$timespan)
  # The replay object reports the same pair, so a consumer holding only the
  # preprocessed object knows the likelihood shape without re-deriving it.
  expect_false(prep$has_intercept)
  expect_false(prep$right_censored)
})

test_that("the replay object reports the flags for an exact-time model", {
  prep <- compute_statistics(
    depNetwork ~ indeg(networkState),
    data = dataTest,
    model = "DyNAM",
    sub_model = "rate"
  )
  expect_true(prep$has_intercept)
  expect_true(prep$right_censored)
})

test_that("an unavailable sub_model names the model's allowed set", {
  expect_snapshot(
    error = TRUE,
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "REM",
      sub_model = "choice_coordination"
    )
  )
})

test_that("REM sub_model = choice points at both successors", {
  expect_snapshot(invisible(compute_statistics(
    depNetwork ~ inertia(networkState),
    data = dataTest,
    model = "REM",
    sub_model = "choice",
    output = "gather"
  )))
})

test_that("compute_statistics validates model and sub_model values", {
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "SAOM",
      sub_model = "choice"
    )
  )
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "REM",
      sub_model = "choice_coordination"
    )
  )
})

test_that("preprocessed objects carry the format version", {
  prep <- compute_statistics(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  # Pinned to the constant, not a literal: the contract under test is "a fresh
  # object carries the current version and a stale one is refused", which a
  # hardcoded number restates as "the version is 4" and breaks on every bump.
  expect_identical(prep$version, PREPROCESSED_GOLDFISH_VERSION)
  expect_identical(prep$active_dyad_encoding, "alter")
  oldFormat <- prep
  oldFormat$version <- PREPROCESSED_GOLDFISH_VERSION - 1L
  expect_error(
    estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice",
      data = dataTest,
      preprocessed = oldFormat
    ),
    "outdated preprocessing format"
  )
})

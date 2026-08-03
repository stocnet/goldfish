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
  frame <- compute_statistics(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice",
    output = "data.frame"
  )
  expect_s3_class(frame, "data.frame")
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "long"
    ),
    "should be one of"
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
  # hardcoded number restates as a claim about the number and breaks on a bump.
  expect_identical(prep$prep_version, PREP_VERSION)
  expect_identical(prep$active_dyad_encoding, "alter")
  oldFormat <- prep
  oldFormat$prep_version <- PREP_VERSION - 1L
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

# DyNAM-i statistics export -------------------------------------------------
#
# The interaction front-end is fenced off the shared recipe path and ignores
# the writer, so its preprocessing product is the monolith object rather than a
# rendered stack. The export converts it exactly as estimation does, which is
# what these tests pin: not merely that a stack comes back, but that the stack
# describes the risk set the fit used. Each reconstructs the model's own
# log-likelihood from the stack alone and compares it to the fit's.

rfid_interaction_data <- function() {
  env <- new.env()
  data("RFID_Validity_Study", package = "goldfish", envir = env)
  participants <- env$participants
  participants$label <- as.character(participants$label)
  make_groups_interaction(env$video, participants, seed_randomization = 1)
}

event_row_starts <- function(gathered) {
  cumsum(c(0L, utils::head(gathered$n_candidates, -1L)))
}

test_that("a DyNAM-i choice gather describes the risk set the fit uses", {
  data <- suppressWarnings(rfid_interaction_data())
  formula <- interactions ~
    diff(age, subType = "averaged_sum") + same(gender, subType = "proportion")
  gathered <- suppressWarnings(compute_statistics(
    formula,
    model = "DyNAMi",
    sub_model = "choice",
    data = data,
    output = "gather"
  ))
  fit <- suppressWarnings(estimate_dynami(
    formula,
    sub_model = "choice",
    data = data,
    control_algo = set_algorithm_newton(backend = "r")
  ))

  actors <- data$nodes$label[data$nodes$mode == "actor"]
  groups <- data$nodes$label[data$nodes$mode == "group"]
  expect_identical(gathered$names_effects, c("diff_age", "same_gender"))
  expect_equal(nrow(gathered$stat_all_events), sum(gathered$n_candidates))
  # The joining choice set is the groups occupied at the decision point, so it
  # is bounded by the groups and genuinely narrower than all of them.
  expect_true(all(gathered$n_candidates <= length(groups)))
  expect_lt(min(gathered$n_candidates), length(groups))

  # An actor x group dyad: the selected row decodes to the observed pair.
  selected_rows <- event_row_starts(gathered) + gathered$selected
  expect_equal(actors[gathered$index_i[selected_rows]], gathered$sender)
  expect_equal(groups[gathered$index_j[selected_rows]], gathered$receiver)

  # The multinomial log-likelihood rebuilt from the stack alone.
  starts <- event_row_starts(gathered)
  log_lik <- sum(vapply(
    seq_along(gathered$n_candidates),
    function(e) {
      rows <- starts[e] + seq_len(gathered$n_candidates[e])
      utility <- gathered$stat_all_events[rows, , drop = FALSE] %*% coef(fit)
      utility[gathered$selected[e]] - log(sum(exp(utility)))
    },
    numeric(1)
  ))
  expect_equal(log_lik, fit$log_likelihood, tolerance = 1e-10)
})

test_that("a DyNAM-i rate gather carries the intercept and its exposure", {
  data <- suppressWarnings(rfid_interaction_data())
  formula <- interactions ~
    1 +
    intercept(interactions, joining = 1) +
    ego(age, joining = 1, subType = "centered")
  gathered <- suppressWarnings(compute_statistics(
    formula,
    model = "DyNAMi",
    sub_model = "rate",
    data = data,
    output = "gather"
  ))
  fit <- suppressWarnings(estimate_dynami(
    formula,
    sub_model = "rate",
    data = data,
    control_algo = set_algorithm_newton(backend = "r")
  ))

  # Exact-time: the forced time intercept column and the exposure fields.
  expect_true(gathered$has_intercept)
  expect_true(gathered$right_censored)
  expect_identical(gathered$names_effects[1L], "Intercept")
  expect_length(gathered$timespan, length(gathered$n_candidates))
  # Sender-indexed rows carry no receiver identity.
  expect_true(all(is.na(gathered$index_j)))
  expect_null(gathered$receiver)

  starts <- event_row_starts(gathered)
  log_lik <- sum(vapply(
    seq_along(gathered$n_candidates),
    function(e) {
      rows <- starts[e] + seq_len(gathered$n_candidates[e])
      rate <- exp(gathered$stat_all_events[rows, , drop = FALSE] %*% coef(fit))
      chosen <- if (gathered$is_dependent[e]) {
        log(rate[gathered$selected[e]])
      } else {
        0
      }
      chosen - gathered$timespan[e] * sum(rate)
    },
    numeric(1)
  ))
  expect_equal(log_lik, fit$log_likelihood, tolerance = 1e-8)
})

test_that("a DyNAM-i model exports to a database in the standard shape", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  data <- suppressWarnings(rfid_interaction_data())
  formula <- interactions ~ diff(age, subType = "averaged_sum")
  gathered <- suppressWarnings(compute_statistics(
    formula,
    model = "DyNAMi",
    sub_model = "choice",
    data = data,
    output = "gather"
  ))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  suppressWarnings(compute_statistics(
    formula,
    model = "DyNAMi",
    sub_model = "choice",
    data = data,
    output = "db",
    control_prep = set_preprocessing(db = con, db_table = "stats")
  ))

  # DyNAM-i reaches the interaction monolith through the legacy environment,
  # which carries no mode map -- so there is no node table to write, and the
  # map plus the process table are the whole export.
  expect_setequal(DBI::dbListTables(con), c("stats_1", "stats_map"))
  tbl <- DBI::dbReadTable(con, "stats_1")
  expect_equal(tbl[[gathered$names_effects]], gathered$stat_all_events[, 1L])
  expect_identical(
    DBI::dbReadTable(con, "stats_map")$stat_block,
    "DyNAMi:choice"
  )
})

# The ready-to-estimate frame ------------------------------------------------

se_frame_data <- baselines_social_evolution_data()

test_that("the frame reproduces the gather stack row for row", {
  args <- list(
    calls_dependent ~ inertia + recip,
    data = se_frame_data,
    model = "DyNAM",
    sub_model = "choice"
  )
  gathered <- do.call(compute_statistics, c(args, output = "gather"))
  frame <- do.call(compute_statistics, c(args, output = "data.frame"))

  expect_s3_class(frame, "data.frame")
  expect_identical(
    names(frame),
    c(
      "event",
      "chosen",
      "sender",
      "receiver",
      "index_i",
      "index_j",
      "timespan",
      "is_dependent",
      gathered$names_effects
    )
  )
  expect_equal(nrow(frame), nrow(gathered$stat_all_events))
  expect_equal(
    unname(as.matrix(frame[, gathered$names_effects])),
    unname(gathered$stat_all_events)
  )
  expect_identical(
    attr(frame, "effect_description"),
    gathered$effect_description
  )

  # One chosen row per dependent event, at the stack's selected position.
  expect_identical(sum(frame$chosen), length(gathered$n_candidates))
  expect_identical(
    as.integer(table(frame$event)),
    as.integer(gathered$n_candidates)
  )
  starts <- cumsum(c(0L, utils::head(gathered$n_candidates, -1L)))
  expect_equal(which(frame$chosen == 1L), starts + gathered$selected)

  # Labels are the row's own dyad, decoded from its own indices rather than the
  # event's observed pair repeated down its rows; on the chosen rows the two
  # coincide, which is what ties the decode to the stack's own labels.
  labels <- se_frame_data$nodes$label
  expect_identical(frame$sender, labels[frame$index_i])
  expect_identical(frame$receiver, labels[frame$index_j])
  expect_identical(frame$receiver[frame$chosen == 1L], gathered$receiver)
  expect_identical(frame$sender[frame$chosen == 1L], gathered$sender)
})

test_that("a rate frame carries exposure and no receiver", {
  args <- list(
    calls_dependent ~ 1 + indeg + outdeg,
    data = se_frame_data,
    model = "DyNAM",
    sub_model = "rate"
  )
  gathered <- do.call(compute_statistics, c(args, output = "gather"))
  frame <- do.call(compute_statistics, c(args, output = "data.frame"))

  # Sender-set rows have no receiver axis, so the label is NA exactly where the
  # index is; the exposure the waiting-time likelihood needs rides on every row.
  expect_true(all(is.na(frame$index_j)))
  expect_true(all(is.na(frame$receiver)))
  expect_equal(
    frame$timespan,
    rep.int(gathered$timespan, gathered$n_candidates)
  )
  expect_identical(names(frame)[9L], "Intercept")
})

test_that("an ordinal frame has neither intercept nor exposure", {
  frame <- compute_statistics(
    calls_dependent ~ indeg + outdeg,
    data = se_frame_data,
    model = "DyNAM",
    sub_model = "rate_ordered",
    output = "data.frame"
  )
  expect_true(all(is.na(frame$timespan)))
  expect_true(all(frame$is_dependent))
  expect_false("Intercept" %in% names(frame))
})

test_that("right-censored frame rows are marked and never chosen", {
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
    frame <- compute_statistics(
      dep ~ inertia(call_network),
      model = "REM",
      sub_model = "rate",
      data = d,
      output = "data.frame"
    )
  }))

  censored <- !frame$is_dependent
  expect_gt(sum(censored), 0)
  # A right-censored event contributes exposure but no choice: none of its rows
  # is chosen, and every one of them still carries the timespan the
  # waiting-time likelihood integrates over.
  expect_identical(sum(frame$chosen[censored]), 0L)
  expect_true(all(is.finite(frame$timespan[censored])))
  expect_identical(sum(frame$chosen), length(unique(frame$event[!censored])))
})

test_that("the frame reproduces the estimator through conditional logit", {
  # The parity gate on the export surface: goldfish's choice likelihood IS a
  # conditional logit with one case per stratum, so the same specification fitted
  # through `estimate_dynam()` and through `clogit()` on the frame must agree.
  # A drift between what is exported and what is estimated shows up here as
  # differing coefficients, whatever caused it.
  skip_if_not_installed("survival")
  withr::local_package("survival")

  formula <- calls_dependent ~ inertia + recip
  fit <- estimate_dynam(formula, sub_model = "choice", data = se_frame_data)
  frame <- compute_statistics(
    formula,
    data = se_frame_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "data.frame"
  )
  stats <- setdiff(names(frame), FRAME_RESERVED_COLUMNS)
  clogit_fit <- clogit(
    stats::as.formula(
      paste("chosen ~", paste(c(stats, "strata(event)"), collapse = " + "))
    ),
    data = frame,
    # One case per stratum, so the tie methods coincide and this is the exact
    # conditional likelihood -- the same one goldfish maximizes.
    method = "exact"
  )

  # Observed agreement on this fixture is ~2e-6 on the coefficients; the gate is
  # set at 1e-4, the distance between two optimizers stopping on their own
  # convergence rules rather than a claim about the arithmetic.
  expect_equal(
    unname(coef(fit)),
    unname(coef(clogit_fit)),
    tolerance = 1e-4
  )
  expect_equal(
    fit$log_likelihood,
    as.numeric(logLik(clogit_fit)),
    tolerance = 1e-4
  )
})

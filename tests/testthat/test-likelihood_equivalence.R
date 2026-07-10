# Old-vs-new equivalence harness for the likelihood-computation refactor
# (design D4). Captures the real per-event contribution inputs from a short
# `default`-engine fit of every baseline cell, then replays each event through
# both the live `compute_event_contribution()` and the frozen
# `contribution_reference()` (helper-contribution-reference.R), asserting
# per-event agreement of logLikelihood / score / informationMatrix / pMatrix
# within 1e-10.
#
# As §2-4 refactor the live helpers the reference stays frozen, so this test
# turns from an old-vs-old tautology (green now) into the per-component
# equivalence gate that must pass before each original helper is deleted.

equivalenceGrid <- baselines_model_grid()
equivalenceDataEnv <- new.env()

equivalence_get_data <- function(dataset) {
  if (is.null(equivalenceDataEnv[[dataset]])) {
    equivalenceDataEnv[[dataset]] <- switch(
      dataset,
      social_evolution = baselines_social_evolution_data(),
      fisheries = suppressWarnings(baselines_fisheries_data())
    )
  }
  mget(dataset, envir = equivalenceDataEnv)
}

# Capture the real per-event contribution inputs by wrapping the bound
# contribution function for one short `default`-engine fit. `bind_event_
# contribution()` is called once per likelihood evaluation, so resetting on each
# call keeps the final evaluation's first `limit` events — captured at a
# non-zero beta (after the first Newton step) so the replay is discriminating.
capture_contribution_events <- function(
  spec,
  data_list,
  limit = 15L,
  max_iterations = 2L
) {
  rec <- new.env(parent = emptyenv())
  rec$events <- list()
  orig_bind <- getFromNamespace("bind_event_contribution", "goldfish")
  testthat::local_mocked_bindings(
    bind_event_contribution = function(spec) {
      rec$events <- list()
      real_fn <- orig_bind(spec)
      function(...) {
        if (length(rec$events) < limit) {
          rec$events[[length(rec$events) + 1L]] <- list(...)
        }
        real_fn(...)
      }
    },
    .package = "goldfish"
  )
  spec$estimation_args <- utils::modifyList(
    if (is.null(spec$estimation_args)) list() else spec$estimation_args,
    list(max_iterations = max_iterations)
  )
  suppressWarnings(baselines_fit(spec, "default", data_list))
  rec$events
}

# The live contribution methods are internal and not S3-registered, so the
# engine resolves them by name via bind_event_contribution() rather than
# UseMethod(); the replay does the same to reach the concrete method.
live_bind <- getFromNamespace("bind_event_contribution", "goldfish")

for (modelName in names(equivalenceGrid)) {
  test_that(
    sprintf("contribution equivalence (old-vs-old): %s", modelName),
    {
      skip_on_cran()
      spec <- equivalenceGrid[[modelName]]
      events <- capture_contribution_events(
        spec,
        equivalence_get_data(spec$dataset)
      )
      expect_gt(length(events), 0)
      for (ev in events) {
        live <- do.call(live_bind(ev$spec), ev)
        ref <- do.call(contribution_reference, ev)
        expect_equal(live$logLikelihood, ref$logLikelihood, tolerance = 1e-10)
        expect_equal(live$score, ref$score, tolerance = 1e-10)
        expect_equal(
          live$informationMatrix,
          ref$informationMatrix,
          tolerance = 1e-10
        )
        expect_equal(live$pMatrix, ref$pMatrix, tolerance = 1e-10)
      }
    }
  )
}

# Direct old-vs-new checks for the rate/REM core (event_contribution_rate)
# scenarios the baseline grid does not exercise: the REM riskMask zeroing and
# reflexive-edge exclusion (§2.1 dim<- reshape, §2.2 crossprod), and the
# single-parameter rate path (the removed special-case Fisher loop).
expect_contribution_equal <- function(live, ref) {
  expect_equal(live$logLikelihood, ref$logLikelihood, tolerance = 1e-10)
  expect_equal(live$score, ref$score, tolerance = 1e-10)
  expect_equal(live$informationMatrix, ref$informationMatrix, tolerance = 1e-10)
  expect_equal(live$pMatrix, ref$pMatrix, tolerance = 1e-10)
}

test_that("rate/REM core: riskMask and reflexive zeroing match the reference", {
  live_rate <- getFromNamespace("event_contribution_rate", "goldfish")
  set.seed(42)
  n1 <- 5L
  n2 <- 5L
  p <- 3L
  statsArray <- array(stats::rnorm(n1 * n2 * p), dim = c(n1, n2, p))
  parameters <- c(0.3, -0.5, 0.8)
  activeDyad <- c(2L, 4L)
  riskMask <- matrix(TRUE, n1, n2)
  riskMask[cbind(c(1L, 4L, 5L), c(3L, 2L, 5L))] <- FALSE

  scenarios <- list(
    list(irc = FALSE, ts = 1.5, refl = FALSE, mask = riskMask),
    list(irc = TRUE, ts = 2.0, refl = TRUE, mask = riskMask),
    list(irc = FALSE, ts = 0.7, refl = FALSE, mask = NULL)
  )
  for (sc in scenarios) {
    live <- live_rate(
      statsArray,
      activeDyad,
      parameters,
      sc$irc,
      sc$ts,
      sc$refl,
      is_two_mode = FALSE,
      isREM = TRUE,
      riskMask = sc$mask
    )
    ref <- event_contribution_rate_ref(
      statsArray,
      activeDyad,
      parameters,
      sc$irc,
      sc$ts,
      sc$refl,
      is_two_mode = FALSE,
      isREM = TRUE,
      riskMask = sc$mask
    )
    expect_contribution_equal(live, ref)
  }
})

test_that("rate core: single-parameter path matches the reference", {
  live_rate <- getFromNamespace("event_contribution_rate", "goldfish")
  set.seed(7)
  statsArray <- matrix(stats::rnorm(6L), 6L, 1L)
  live <- live_rate(
    statsArray,
    c(3L, NA),
    0.6,
    FALSE,
    1.2,
    TRUE,
    is_two_mode = FALSE,
    isREM = FALSE
  )
  ref <- event_contribution_rate_ref(
    statsArray,
    c(3L, NA),
    0.6,
    FALSE,
    1.2,
    TRUE,
    is_two_mode = FALSE,
    isREM = FALSE
  )
  expect_contribution_equal(live, ref)
})

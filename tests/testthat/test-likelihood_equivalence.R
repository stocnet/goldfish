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

# Snapshot the guard text so a change to the engine capability surfaces as a
# deliberate snapshot update, and the "not consumed" message stays in sync
# with the supported-family list.

test_that("the capability rule answers for every supported variant", {
  # This was a nine-row table keyed by the variant class. The rule replacing
  # it has two clauses, so the test now enumerates the variants rather than
  # the table's rows -- the same coverage, asserted against behavior instead
  # of against a lookup that could disagree with it.
  wired <- list(
    c("DyNAM", "choice"),
    c("DyNAM", "choice_coordination"),
    c("DyNAM", "rate"),
    c("REM", "rate"),
    c("REM", "rate_ordered")
  )
  unwired <- list(
    c("DyNAM", "rate_ordered"),
    c("DyNAMi", "rate"),
    c("DyNAMi", "rate_ordered"),
    c("DyNAMi", "choice")
  )
  for (v in wired) {
    spec <- new_model_spec(v[1], v[2], nodes = "actors")
    expect_true(
      constrained_estimation_supported(spec),
      info = paste(v, collapse = "/")
    )
  }
  for (v in unwired) {
    spec <- new_model_spec(v[1], v[2], nodes = "actors")
    expect_false(
      constrained_estimation_supported(spec),
      info = paste(v, collapse = "/")
    )
  }
})

test_that("the unsupported-constraint abort enumerates the supported families", {
  local_reproducible_output()
  spec <- new_model_spec("DyNAM", "rate_ordered", nodes = "actors")
  expect_snapshot(
    error = TRUE,
    abort_constraint_unsupported(spec)
  )
})

# The collapse deleted the standalone REM/coordination mask fallback; it is safe
# only because every constrained REM / REM-ordered / coordination model folds its
# availability into `active_dyad` during preprocessing. This confirms the fold is
# unconditional, so no unfolded path reaches estimation.
make_folded_fixture <- function(n_events = 60L) {
  suppressWarnings({
    data("Social_Evolution", package = "goldfish", envir = environment())
    actors <- get("actors", environment())
    calls <- get("calls", environment())
    call_network <- make_network(nodes = actors, directed = TRUE)
    call_network <- link_events(call_network, calls, nodes = actors)
    calls_dependent <- make_dependent_events(
      calls,
      nodes = actors,
      default_network = call_network
    )
    calls_dependent <- calls_dependent[seq_len(n_events), ]
    data <- make_data(calls_dependent, call_network, calls, actors)
  })
  list(
    data = data,
    formula_intercept = calls_dependent ~ 1 + inertia,
    formula_ordinal = calls_dependent ~ inertia,
    formula_coord = calls_dependent ~ inertia,
    constraint = ~ indeg(call_network) >= 0
  )
}

test_that("constrained REM / REM-ordered / coordination always arrive folded", {
  fx <- make_folded_fixture()
  rem <- suppressWarnings(estimate_rem(
    fx$formula_intercept,
    sub_model = "rate",
    data = fx$data,
    support_constraint = fx$constraint,
    preprocessing_only = TRUE
  ))
  expect_true(isTRUE(rem$active_dyad_folded))
  rem_ordered <- suppressWarnings(estimate_rem(
    fx$formula_ordinal,
    sub_model = "rate_ordered",
    data = fx$data,
    support_constraint = fx$constraint,
    preprocessing_only = TRUE
  ))
  expect_true(isTRUE(rem_ordered$active_dyad_folded))
  coord <- suppressWarnings(estimate_dynam(
    fx$formula_coord,
    sub_model = "choice_coordination",
    data = fx$data,
    support_constraint = fx$constraint,
    preprocessing_only = TRUE
  ))
  expect_true(isTRUE(coord$active_dyad_folded))
})

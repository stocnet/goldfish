# Snapshot the map-generated guard text so a change to the engine-capability
# table (constrained_support_map) surfaces as a deliberate snapshot update, and
# the "not consumed" message stays in sync with the supported-family list.

test_that("constrained_support_map matches the wired families", {
  supported <- constrained_support_map()
  expect_identical(
    names(supported)[!is.na(supported)],
    c(
      "goldfishKindDnChoice",
      "goldfishKindDnCoord",
      "goldfishKindDnRate",
      "goldfishKindRemRate",
      "goldfishKindRemCox"
    )
  )
  expect_identical(
    names(supported)[is.na(supported)],
    c(
      "goldfishKindDnCox",
      "goldfishKindDniRate",
      "goldfishKindDniCox",
      "goldfishKindDniChoice"
    )
  )
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

# support_constraint estimate-path wiring. The
# parsed constraint threads from the estimate surfaces / a specification into
# build_spec_map(), which compiles a sibling sub-plan over the atoms and
# registers a `kind = "support_mask"` derivation. Until the recipe loop consumes
# the mask (later slice), a constrained model's statistics must stay identical to
# the unconstrained model (the additive, baseline-safe invariant).

# Self-contained fixture (testthat 3e): a small DyNAM/REM-ready data object.
make_constraint_fixture <- function() {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  calls_dependent <- calls_dependent[1:80, ]
  # The low-level compile tests drive the constraint builders directly against a
  # legacy environment (node-set names, dependent object); the estimate tests use
  # the stocnet make_data() returns.
  env <- new.env()
  assign("call_network", call_network, envir = env)
  assign("calls_dependent", calls_dependent, envir = env)
  assign("calls", calls, envir = env)
  assign("actors", actors, envir = env)
  list(
    data = make_data(calls_dependent, call_network, calls, actors),
    env = env
  )
}

test_that("compile_support_constraint builds a role-tagged sibling sub-plan", {
  d <- make_constraint_fixture()
  cp <- parse_and_validate_constraint(
    ~ tie(call_network),
    has_dyad_part = TRUE,
    envir = d$env
  )
  sub <- compile_support_constraint(
    cp,
    model = "DyNAM",
    dep_name = "call_network",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = d$env
  )
  expect_identical(unique(sub$effects$role), "constraint")
  expect_false(any(sub$effects$estimate))
  expect_identical(sub$atom_labels, "tie(call_network)")
  expect_false(is.null(sub$expr))
  # a genuinely dyadic (tie) atom stores the mask dense: point kind (0)
  expect_identical(sub$mask_kind, 0L)
  # one atom -> one registry row and one compiled closure
  expect_identical(nrow(sub$effects), 1L)
  expect_length(sub$effect_functions, 1L)
})

test_that("support_mask_derivation registers a support_mask derived object", {
  d <- make_constraint_fixture()
  cp <- parse_and_validate_constraint(
    ~ tie(call_network),
    has_dyad_part = TRUE,
    envir = d$env
  )
  sub <- compile_support_constraint(
    cp,
    model = "DyNAM",
    dep_name = "call_network",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = d$env
  )
  der <- support_mask_derivation(sub)
  expect_identical(der$kind, "support_mask")
  expect_identical(der$derived_name, "__support_mask__")
  expect_identical(der$params$mask_kind, 0L)
  expect_identical(der$source, "tie(call_network)")
})

test_that("constrained choice preprocessing is unchanged until the mask is consumed", {
  d <- make_constraint_fixture()
  prep0 <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = d$data,
    preprocessing_only = TRUE
  )
  prep1 <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = d$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(call_network)
  )
  expect_equal(prep1$initial_stats, prep0$initial_stats)
  expect_equal(prep1$stat_mat_update, prep0$stat_mat_update)
})

test_that("a rate-only spec accepts a dyadic support_constraint via the row-reduction", {
  d <- make_constraint_fixture()
  expect_message(
    prep <- estimate_dynam(
      calls_dependent ~ 1 + indeg,
      sub_model = "rate",
      data = d$data,
      preprocessing_only = TRUE,
      support_constraint = ~ tie(call_network)
    ),
    "row-reduction"
  )
  expect_s3_class(prep, "goldfishStat")
  expect_true(isTRUE(prep$active_sender_folded))
})

test_that("a specification threads its support_constraint into estimation", {
  d <- make_constraint_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "calls_dependent",
    support_constraint = ~ tie(call_network),
    data = d$data
  )
  expect_s3_class(spec$constraint, "goldfishSupportPlan")
  prep <- estimate_dynam(spec, sub_model = "choice", preprocessing_only = TRUE)
  expect_s3_class(prep, "goldfishStat")
})

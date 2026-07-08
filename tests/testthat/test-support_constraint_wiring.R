# support_constraint estimate-path wiring (tasks 1.4 threading + 2.1). The
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
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(
    x = callNetwork,
    change_event = calls,
    nodes = actors
  )
  callsDependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = callNetwork
  )
  callsDependent <- callsDependent[1:80, ]
  make_data(callsDependent, callNetwork, calls, actors)
}

test_that("compile_support_constraint builds a role-tagged sibling sub-plan", {
  d <- make_constraint_fixture()
  cp <- parse_and_validate_constraint(
    ~ tie(callNetwork),
    has_dyad_part = TRUE,
    envir = d
  )
  sub <- compile_support_constraint(
    cp,
    model = "DyNAM",
    dep_name = "callNetwork",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = d
  )
  expect_identical(unique(sub$effects$role), "constraint")
  expect_false(any(sub$effects$estimate))
  expect_identical(sub$atom_labels, "tie(callNetwork)")
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
    ~ tie(callNetwork),
    has_dyad_part = TRUE,
    envir = d
  )
  sub <- compile_support_constraint(
    cp,
    model = "DyNAM",
    dep_name = "callNetwork",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = d
  )
  der <- support_mask_derivation(sub)
  expect_identical(der$kind, "support_mask")
  expect_identical(der$derived_name, "__support_mask__")
  expect_identical(der$params$mask_kind, 0L)
  expect_identical(der$source, "tie(callNetwork)")
})

test_that("constrained choice preprocessing is unchanged until the mask is consumed", {
  d <- make_constraint_fixture()
  prep0 <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = d,
    preprocessing_only = TRUE
  )
  prep1 <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = d,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(callNetwork)
  )
  expect_equal(prep1$initialStats, prep0$initialStats)
  expect_equal(prep1$stat_mat_update, prep0$stat_mat_update)
})

test_that("a rate-only spec accepts a dyadic support_constraint via the row-reduction (design D12)", {
  d <- make_constraint_fixture()
  expect_message(
    prep <- estimate_dynam(
      callsDependent ~ 1 + indeg,
      sub_model = "rate",
      data = d,
      preprocessing_only = TRUE,
      support_constraint = ~ tie(callNetwork)
    ),
    "row-reduction"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
  expect_true(isTRUE(prep$active_sender_folded))
})

test_that("a specification threads its support_constraint into estimation", {
  d <- make_constraint_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "callsDependent",
    support_constraint = ~ tie(callNetwork),
    data = d
  )
  expect_s3_class(spec$constraint, "support_constraint_plan")
  prep <- estimate_dynam(spec, sub_model = "choice", preprocessing_only = TRUE)
  expect_s3_class(prep, "preprocessed.goldfish")
})

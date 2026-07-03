# make_specification() v1 (design D4, group 4). A specification bundles the
# rate/choice formulas with an empty LHS + a `layer`, round-trips to the same
# coefficients as the equivalent formula, and prints a single-glance overview.

# Self-contained fixture (testthat 3e): a small DyNAM/REM-ready data object.
make_spec_fixture <- function() {
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
  callsDependent <- callsDependent[1:120, ]
  make_data(callsDependent, callNetwork, calls, actors)
}

test_that("make_specification builds a DyNAM specification object", {
  d <- make_spec_fixture()
  spec <- make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    data = d,
    layer = "callsDependent"
  )
  expect_s3_class(spec, "specification.goldfish")
  expect_identical(spec$model, "DyNAM")
  expect_identical(names(spec$submodels), c("rate", "choice"))
  expect_identical(spec$layer, "callsDependent")
  expect_true(spec$valid)
  expect_identical(spec$dependent$network, "callNetwork")
  expect_identical(spec$dependent$n_events, 120L)
})

test_that("estimate from a specification equals estimate from a formula", {
  d <- make_spec_fixture()
  spec <- make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    data = d,
    layer = "callsDependent"
  )

  choice_spec <- estimate_dynam(spec, sub_model = "choice")
  choice_form <- estimate_dynam(
    callsDependent ~ inertia + recip + trans,
    sub_model = "choice",
    data = d
  )
  expect_equal(coef(choice_spec), coef(choice_form), tolerance = 1e-6)

  rate_spec <- estimate_dynam(spec, sub_model = "rate")
  rate_form <- estimate_dynam(
    callsDependent ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = d
  )
  expect_equal(coef(rate_spec), coef(rate_form), tolerance = 1e-6)
})

test_that("estimate_rem accepts a specification (REM round-trip)", {
  d <- make_spec_fixture()
  spec <- make_specification(
    rate = ~ 1 + inertia + recip,
    model = "REM",
    data = d,
    layer = "callsDependent"
  )
  rem_spec <- estimate_rem(spec, sub_model = "rate")
  rem_form <- estimate_rem(
    callsDependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d
  )
  expect_equal(coef(rem_spec), coef(rem_form), tolerance = 1e-6)
})

test_that("legacy formula path is unchanged when a spec is not passed", {
  d <- make_spec_fixture()
  m <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = d
  )
  expect_s3_class(m, "result.goldfish")
})

test_that("a dependent object on the LHS is rejected pointing at layer", {
  d <- make_spec_fixture()
  expect_error(
    make_specification(
      choice = callsDependent ~ inertia,
      model = "DyNAM",
      data = d,
      layer = "callsDependent"
    ),
    "empty left-hand side"
  )
})

test_that("make_specification enforces model / argument constraints", {
  d <- make_spec_fixture()
  # choice is not applicable to REM
  expect_error(
    make_specification(
      choice = ~inertia,
      model = "REM",
      data = d,
      layer = "callsDependent"
    ),
    "not applicable to"
  )
  # at least one formula required
  expect_error(
    make_specification(model = "DyNAM", data = d, layer = "callsDependent"),
    "At least one"
  )
  # layer must resolve to a dependent-events object
  expect_error(
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      data = d,
      layer = "nope"
    ),
    "does not name a dependent-events object"
  )
  # an unidentified bare main effect (ego in choice) aborts at build time
  expect_error(
    make_specification(
      choice = ~ indeg(callNetwork, type = "ego"),
      model = "DyNAM",
      data = d,
      layer = "callsDependent"
    ),
    "Unsupported main effect"
  )
})

test_that("estimating a spec with the wrong estimator is rejected", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    data = d,
    layer = "callsDependent"
  )
  expect_error(estimate_rem(spec, sub_model = "rate"), "for model")
})

test_that("specification print is a stable cli overview", {
  d <- make_spec_fixture()
  spec <- make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    data = d,
    layer = "callsDependent"
  )
  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = FALSE
  )
  expect_snapshot(print(spec))
})

test_that("specification print omits absent sub-model and shows support", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    support_constraint = ~ tie(callNetwork),
    model = "DyNAM",
    data = d,
    layer = "callsDependent"
  )
  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = FALSE
  )
  expect_snapshot(print(spec))
})

# ---- support_constraint parsing + validation (support-constraint change) ----

test_that("dyadic support_constraint parses into a plan-ready structure", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    support_constraint = ~ tie(callNetwork),
    layer = "callsDependent",
    data = d
  )
  cp <- spec$constraint
  expect_s3_class(cp, "support_constraint_plan")
  expect_identical(cp$atom_labels, "tie(callNetwork)")
  expect_identical(cp$expr, quote(.a1 != 0))
  # a genuinely dyadic (point) atom stores the mask dense
  expect_identical(cp$mask_kind, 0L)
  # the display field stays the formula
  expect_s3_class(spec$support_constraint, "formula")
})

test_that("multi-term boolean constraint records all atoms and the tree", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~inertia,
    model = "DyNAM",
    choice_sub_model = "choice",
    support_constraint = ~ tie(callNetwork) & indeg(callNetwork) > 1,
    layer = "callsDependent",
    data = d
  )
  cp <- spec$constraint
  expect_identical(cp$atom_labels, c("tie(callNetwork)", "indeg(callNetwork)"))
  expect_identical(cp$expr, quote(.a1 != 0 & .a2 > 1))
})

test_that("rate-only spec rejects a dyadic constraint atom (D13)", {
  d <- make_spec_fixture()
  expect_error(
    make_specification(
      rate = ~ 1 + indeg(callNetwork, type = "ego"),
      model = "DyNAM",
      rate_sub_model = "rate",
      support_constraint = ~ tie(callNetwork),
      layer = "callsDependent",
      data = d
    ),
    "sender-axis atoms only"
  )
})

test_that("out-of-grammar constraint is rejected at construction", {
  d <- make_spec_fixture()
  expect_error(
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      support_constraint = ~ log(indeg(callNetwork)) > 1,
      layer = "callsDependent",
      data = d
    ),
    "Unsupported construct"
  )
})

test_that("unconstrained specification carries no constraint plan", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~inertia,
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "callsDependent",
    data = d
  )
  expect_null(spec$constraint)
})

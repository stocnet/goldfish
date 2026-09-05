# make_specification() v1. A specification bundles the
# rate/choice formulas with an empty LHS + a `layer`, round-trips to the same
# coefficients as the equivalent formula, and prints a single-glance overview.

# Self-contained fixture (testthat 3e): a small DyNAM/REM-ready data object.
make_spec_fixture <- function() {
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
  calls_dependent <- calls_dependent[1:120, ]
  make_data(calls_dependent, call_network, calls, actors)
}

test_that("make_specification builds a DyNAM specification object", {
  d <- make_spec_fixture()
  spec <- make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    data = d,
    layer = "calls_dependent"
  )
  expect_s3_class(spec, "goldfishSpec")
  expect_identical(spec$model, "DyNAM")
  expect_identical(names(spec$submodels), c("rate", "choice"))
  expect_identical(spec$layer, "calls_dependent")
  expect_true(spec$valid)
  expect_identical(spec$dependent$network, "call_network")
  expect_identical(spec$dependent$n_events, 120L)
})

test_that("estimate from a specification equals estimate from a formula", {
  d <- make_spec_fixture()
  spec <- make_specification(
    rate = ~ 1 + indeg + outdeg,
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    data = d,
    layer = "calls_dependent"
  )

  choice_spec <- estimate_dynam(spec, sub_model = "choice")
  choice_form <- estimate_dynam(
    calls_dependent ~ inertia + recip + trans,
    sub_model = "choice",
    data = d
  )
  expect_equal(coef(choice_spec), coef(choice_form), tolerance = 1e-6)

  rate_spec <- estimate_dynam(spec, sub_model = "rate")
  rate_form <- estimate_dynam(
    calls_dependent ~ 1 + indeg + outdeg,
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
    layer = "calls_dependent"
  )
  rem_spec <- estimate_rem(spec, sub_model = "rate")
  rem_form <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d
  )
  expect_equal(coef(rem_spec), coef(rem_form), tolerance = 1e-6)
})

test_that("legacy formula path is unchanged when a spec is not passed", {
  d <- make_spec_fixture()
  m <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = d
  )
  expect_s3_class(m, "goldfishFit")
})

test_that("a dependent object on the LHS is rejected pointing at layer", {
  d <- make_spec_fixture()
  expect_error(
    make_specification(
      choice = calls_dependent ~ inertia,
      model = "DyNAM",
      data = d,
      layer = "calls_dependent"
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
      layer = "calls_dependent"
    ),
    "not applicable to"
  )
  # at least one formula required
  expect_error(
    make_specification(model = "DyNAM", data = d, layer = "calls_dependent"),
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
    "must name a layer present in"
  )
  # an unidentified bare main effect (ego in choice) aborts at build time
  expect_error(
    make_specification(
      choice = ~ indeg(call_network, type = "ego"),
      model = "DyNAM",
      data = d,
      layer = "calls_dependent"
    ),
    "Unsupported main effect"
  )
})

test_that("make_specification holds out interaction operands from the main check", {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    call_network,
    change_event = calls,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  calls_dependent <- calls_dependent[1:120, ]
  seasons <- make_global_attributes(data.frame(winter = 2))
  d <- make_data(calls_dependent, call_network, calls, actors, seasons)

  # A bare global() main is constant across the choice set and unidentified.
  expect_error(
    make_specification(
      choice = ~ inertia + global(seasons$winter),
      model = "DyNAM",
      data = d,
      layer = "calls_dependent"
    ),
    "Unsupported main effect"
  )
  # The same global as an interaction operand restores dyad-varying variation,
  # so it is held out of the main-effect check and the spec builds -- matching
  # the plain-formula estimation path.
  spec <- make_specification(
    choice = ~ inertia + global(seasons$winter):inertia,
    model = "DyNAM",
    data = d,
    layer = "calls_dependent"
  )
  expect_true(spec$valid)
})

test_that("estimating a spec with the wrong estimator is rejected", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    data = d,
    layer = "calls_dependent"
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
    layer = "calls_dependent"
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
    support_constraint = ~ tie(call_network),
    model = "DyNAM",
    data = d,
    layer = "calls_dependent"
  )
  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = FALSE
  )
  expect_snapshot(print(spec))
})

# ---- support_constraint parsing + validation ----

test_that("dyadic support_constraint parses into a plan-ready structure", {
  d <- make_spec_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    support_constraint = ~ tie(call_network),
    layer = "calls_dependent",
    data = d
  )
  cp <- spec$constraint
  expect_s3_class(cp, "goldfishSupportPlan")
  expect_identical(cp$atom_labels, "tie(call_network)")
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
    support_constraint = ~ tie(call_network) & indeg(call_network) > 1,
    layer = "calls_dependent",
    data = d
  )
  cp <- spec$constraint
  expect_identical(
    cp$atom_labels,
    c("tie(call_network)", "indeg(call_network)")
  )
  expect_identical(cp$expr, quote(.a1 != 0 & .a2 > 1))
})

test_that("rate-only spec accepts a dyadic constraint atom via the row-reduction", {
  d <- make_spec_fixture()
  expect_message(
    spec <- make_specification(
      rate = ~ 1 + indeg(call_network, type = "ego"),
      model = "DyNAM",
      rate_sub_model = "rate",
      support_constraint = ~ tie(call_network),
      layer = "calls_dependent",
      data = d
    ),
    "row-reduction"
  )
  expect_s3_class(spec$constraint, "goldfishSupportPlan")
})

test_that("out-of-grammar constraint is rejected at construction", {
  d <- make_spec_fixture()
  expect_error(
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      support_constraint = ~ log(indeg(call_network)) > 1,
      layer = "calls_dependent",
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
    layer = "calls_dependent",
    data = d
  )
  expect_null(spec$constraint)
})

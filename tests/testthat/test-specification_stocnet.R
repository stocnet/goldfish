# make_specification() over a stocnet object: the data acceptance, the layer /
# info$focal resolution, and the flavor-keyed list that selects which focal rows
# are modeled.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# The focal layer carrying two flavors: one row of each, plus NA-time history.
flavored_fixture <- function() {
  x <- make_stocnet_fixture()
  x$ties$flavor <- c(NA, "creation", "dissolution")
  x
}

# A mutually-exclusive flavored layer stamped by add_flavor(): an increment layer
# whose +1 rows are creation and -1 rows dissolution, with the flavor metadata
# recorded in info. Two creation rows and one dissolution row over [1, 3].
me_flavored_fixture <- function() {
  x <- make_stocnet_fixture()
  x$ties <- data.frame(
    from = c(1L, 2L, 3L, 1L),
    to = c(2L, 3L, 1L, 2L),
    time = c(NA, 1, 2, 3),
    layer = "calls",
    weight = c(1, 1, -1, 1),
    stringsAsFactors = FALSE
  )
  add_flavor(
    x,
    layer = "calls",
    values_equivalence = c(
      creation = 1,
      dissolution = -1
    )
  )
}

# The same event stream with no flavor column or metadata, for inference tests.
unflavored_increment_fixture <- function() {
  x <- make_stocnet_fixture()
  x$ties <- data.frame(
    from = c(1L, 2L, 3L, 1L),
    to = c(2L, 3L, 1L, 2L),
    time = c(NA, 1, 2, 3),
    layer = "calls",
    weight = c(1, 1, -1, 1),
    stringsAsFactors = FALSE
  )
  x
}

test_that("a raw stocnet is accepted as data", {
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    data = make_stocnet_fixture()
  )

  expect_s3_class(spec, "specification.goldfish")
  expect_true(spec$valid)
})

test_that("a stamped stocnet gives the same specification as the raw one", {
  raw <- make_stocnet_fixture()
  args <- list(
    choice = ~inertia,
    model = "DyNAM",
    choice_sub_model = "choice"
  )

  from_raw <- do.call(make_specification, c(args, list(data = raw)))
  from_stamped <- do.call(
    make_specification,
    c(args, list(data = as_goldfish(raw)))
  )

  expect_equal(from_raw$dependent, from_stamped$dependent)
  expect_equal(from_raw$focal, from_stamped$focal)
})

test_that("a stamped object is re-validated rather than trusted", {
  # The stamp records provenance, not validity: manynet verbs and plain list
  # assignment mutate an object while preserving its class vector.
  local_cli_context()
  stamped <- as_goldfish(make_stocnet_fixture())
  stamped$info$update <- NULL

  expect_error(
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      data = stamped
    )
  )
})

test_that("layer defaults to info$focal", {
  spec <- make_specification(
    choice = ~inertia,
    model = "DyNAM",
    choice_sub_model = "choice",
    data = make_stocnet_fixture()
  )

  expect_equal(spec$layer, "calls")
  expect_equal(spec$focal, "calls")
})

test_that("layer overrides info$focal", {
  x <- make_stocnet_fixture()
  x$ties <- rbind(
    x$ties,
    data.frame(from = 1L, to = 3L, time = 3, layer = "sms")
  )
  x$info$update <- c(calls = "increment", sms = "increment")
  x$info$directed <- c(calls = TRUE, sms = TRUE)
  x$info$observation <- c(calls = "event", sms = "event")

  spec <- make_specification(
    choice = ~ inertia(sms),
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "sms",
    data = x
  )

  expect_equal(
    spec$focal,
    "sms",
    label = "the spec carries the layer it parsed"
  )
  expect_equal(spec$dependent$n_events, 1L)
})

test_that("an unknown layer aborts listing the candidates", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      layer = "nope",
      data = make_stocnet_fixture()
    )
  )
})

test_that("a dependent process with no layer and no focal aborts", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$focal <- NULL

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      data = x
    )
  )
})

test_that("a data frame is not a stocnet", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      data = data.frame(x = 1)
    )
  )
})

# Flavor -----------------------------------------------------------------------

test_that("a keyed flavor models only its rows", {
  spec <- make_specification(
    choice = list(creation ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = flavored_fixture()
  )

  expect_equal(spec$modeled_flavor, "creation")
  expect_equal(
    spec$dependent$n_events,
    1L,
    label = "the dissolution row updates state but is not modeled"
  )
  expect_setequal(spec$dependent$flavors, c("creation", "dissolution"))
})

test_that("a plain formula on a flavored layer models all rows and says so", {
  local_cli_context()

  expect_snapshot(
    spec <- make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      data = flavored_fixture()
    )
  )
  expect_null(spec$modeled_flavor)
  expect_equal(spec$dependent$n_events, 2L)
})

test_that("the spec print nests the modeled and state-only flavors", {
  spec <- make_specification(
    choice = list(creation ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = flavored_fixture()
  )
  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = FALSE
  )
  expect_snapshot(print(spec))
})

test_that("the spec print lists all flavors when a plain formula models them", {
  spec <- suppressMessages(make_specification(
    choice = ~inertia,
    model = "DyNAM",
    choice_sub_model = "choice",
    data = flavored_fixture()
  ))
  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = FALSE
  )
  expect_snapshot(print(spec))
})

test_that("a plain formula on an unflavored layer stays quiet", {
  expect_no_message(
    make_specification(
      choice = ~inertia,
      model = "DyNAM",
      choice_sub_model = "choice",
      data = make_stocnet_fixture()
    )
  )
})

test_that("several flavor keys build parallel processes", {
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + inertia),
    choice = list(creation ~ inertia, dissolution ~ recip),
    model = "DyNAM",
    data = me_flavored_fixture()
  )

  expect_setequal(spec$modeled_flavors, c("creation", "dissolution"))
  expect_null(spec$modeled_flavor)
  expect_setequal(names(spec$processes), c("creation", "dissolution"))
  expect_equal(
    deparse1(spec$processes$creation$submodels$rate$input_formula),
    "~1 + indeg"
  )
  expect_equal(
    deparse1(spec$processes$dissolution$submodels$choice$input_formula),
    "~recip"
  )
})

test_that("mutually exclusive flavors derive complementary masks", {
  spec <- make_specification(
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = me_flavored_fixture()
  )

  expect_equal(
    deparse1(spec$processes$creation$derived_constraint),
    "~!tie(calls)"
  )
  expect_equal(
    deparse1(spec$processes$dissolution$derived_constraint),
    "~tie(calls)"
  )
  expect_s3_class(
    spec$processes$creation$constraint,
    "support_constraint_plan"
  )
})

test_that("a user constraint composes with each derived one", {
  local_cli_context()
  spec <- suppressMessages(make_specification(
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    support_constraint = ~ indeg(calls) > 0,
    data = me_flavored_fixture()
  ))

  expect_equal(
    deparse1(spec$processes$creation$constraint$formula),
    "~!tie(calls) & indeg(calls) > 0"
  )
  expect_equal(
    deparse1(spec$processes$dissolution$constraint$formula),
    "~tie(calls) & indeg(calls) > 0"
  )
})

test_that("a single modeled flavor still derives its mask", {
  spec <- make_specification(
    choice = list(creation ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = me_flavored_fixture()
  )

  expect_equal(spec$modeled_flavor, "creation")
  expect_equal(deparse1(spec$derived_constraint), "~!tie(calls)")
  expect_s3_class(spec$constraint, "support_constraint_plan")
})

test_that("a redundant layer derives no constraint", {
  x <- me_flavored_fixture()
  x$info$flavor_style[["calls"]] <- "redundant"
  spec <- make_specification(
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = x
  )

  expect_null(spec$processes$creation$derived_constraint)
  expect_null(spec$processes$creation$constraint)
})

test_that("an unflavored layer infers the mapping and says so", {
  local_cli_context()

  expect_snapshot(
    spec <- make_specification(
      choice = list(creation ~ inertia, dissolution ~ inertia),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = unflavored_increment_fixture()
    )
  )
  expect_setequal(spec$modeled_flavors, c("creation", "dissolution"))
  expect_equal(
    deparse1(spec$processes$creation$derived_constraint),
    "~!tie(calls)"
  )
})

test_that("a weighted layer aborts inference", {
  local_cli_context()
  x <- unflavored_increment_fixture()
  x$ties$weight <- c(1, 3, -2, 5)

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = list(creation ~ inertia, dissolution ~ inertia),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = x
    )
  )
})

test_that("a key matching no flavor value aborts", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = list(creation ~ inertia, deletion ~ inertia),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = me_flavored_fixture()
    )
  )
})

test_that("duplicate flavor keys abort", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = list(creation ~ inertia, creation ~ recip),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = me_flavored_fixture()
    )
  )
})

test_that("a plain sub-model with a multi-keyed sibling aborts", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      rate = ~ 1 + indeg,
      choice = list(creation ~ inertia, dissolution ~ recip),
      model = "DyNAM",
      data = me_flavored_fixture()
    )
  )
})

test_that("estimating a multi-flavor specification aborts for now", {
  local_cli_context()
  spec <- make_specification(
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = me_flavored_fixture()
  )

  expect_snapshot(error = TRUE, estimate_dynam(spec, sub_model = "choice"))
})

test_that("the multi-flavor print nests a section per flavor", {
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + inertia),
    choice = list(creation ~ inertia, dissolution ~ recip),
    model = "DyNAM",
    data = me_flavored_fixture()
  )
  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = FALSE
  )
  expect_snapshot(print(spec))
})

test_that("rate and choice must key the same flavor", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      rate = list(creation ~ 1 + indeg),
      choice = list(dissolution ~ inertia),
      model = "DyNAM",
      data = flavored_fixture()
    )
  )
})

test_that("rate and choice keying one flavor agree", {
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg),
    choice = list(creation ~ inertia),
    model = "DyNAM",
    data = flavored_fixture()
  )

  expect_equal(spec$modeled_flavor, "creation")
})

test_that("a flavor no focal row carries aborts", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = list(nope ~ inertia),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = flavored_fixture()
    )
  )
})

test_that("a keyed entry must be a formula with the flavor on the left", {
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    make_specification(
      choice = list(~inertia),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = flavored_fixture()
    )
  )
})

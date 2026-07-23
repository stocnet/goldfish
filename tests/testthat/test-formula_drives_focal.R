# The modeled layer -- the formula LHS or the specification `layer` -- drives
# focal/side/mode resolution during estimation, not `info$focal`. A hand-built
# stocnet omitting `info$focal` estimates, and an `info$focal` naming a different
# layer than the one modeled never wins over the modeled layer. `info$focal` is
# only the default for *which* layer to model.

test_that("a focal-less two-mode object estimates and resolves the modeled sides", {
  obj <- as_goldfish(make_stocnet_fixture_twomode_estimable_nofocal())
  fit <- estimate_dynam(membership ~ 1 + ego(x), sub_model = "rate", data = obj)
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))

  # Sides resolve against the modeled `membership` layer (p -> o), not a
  # zero-length focal: the export lookup carries both disjoint sides.
  out <- gather_model_data(
    membership ~ 1 + ego(x),
    model = "DyNAM",
    sub_model = "rate",
    data = make_stocnet_fixture_twomode_estimable_nofocal()
  )
  lookup <- out$node_lookup
  expect_setequal(lookup$side, c(1L, 2L))
  expect_equal(lookup$label[lookup$side == 1L], c("P1", "P2", "P3", "P4"))
  expect_equal(lookup$label[lookup$side == 2L], c("O1", "O2", "O3"))
})

test_that("a focal-less one-mode object estimates", {
  obj <- as_goldfish(make_stocnet_fixture_onemode_nofocal())
  fit <- estimate_rem(calls ~ 1 + inertia, data = obj)
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))
})

test_that("modeling a layer other than info$focal resolves against the modeled layer", {
  # multipartite: attend (actor -> event), coauthor (actor -> actor),
  # member (actor -> org). Point info$focal at the one-mode `coauthor` but model
  # the two-mode `attend`: side/mode resolution must follow `attend`.
  mp <- make_stocnet_fixture_multipartite()
  mp$info$focal <- "coauthor"

  fit <- estimate_dynam(
    attend ~ 1 + ego(size),
    sub_model = "rate",
    data = as_goldfish(mp)
  )
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))

  # The sides are `attend`'s disjoint pair (actor -> event), not the one-mode
  # `coauthor` set the stale focal names.
  out <- gather_model_data(
    attend ~ 1 + ego(size),
    model = "DyNAM",
    sub_model = "rate",
    data = mp
  )
  lookup <- out$node_lookup
  expect_setequal(lookup$side, c(1L, 2L))
  expect_equal(lookup$label[lookup$side == 1L], c("A1", "A2", "A3"))
  expect_equal(lookup$label[lookup$side == 2L], c("E1", "E2"))
})

test_that("two-mode side-validity is keyed to the modeled layer's mode pair", {
  # `inertia(member)` (actor -> org) over focal `attend` (actor -> event) is
  # invalid: the argument's receiver side (org) is not the modeled layer's
  # receiver side (event). The abort must name `attend`'s pair, proving the check
  # resolves the focal dyad against the modeled layer -- not the `coauthor` layer
  # `info$focal` still names.
  mp <- make_stocnet_fixture_multipartite()
  mp$info$focal <- "coauthor"

  expect_error(
    gather_model_data(
      attend ~ inertia(member),
      model = "DyNAM",
      sub_model = "choice",
      data = mp
    ),
    'receiver side of layer "attend" \\(mode "event"\\)'
  )
})

test_that("no resolvable dependent is a clear error, not an indexing crash", {
  tm <- make_stocnet_fixture_twomode_estimable_nofocal()

  # Specification surface: neither `layer` nor `info$focal`.
  expect_error(
    make_specification(rate = ~ 1 + ego(x), model = "DyNAM", data = tm),
    "dependent process is not identified"
  )

  # Formula surface: an LHS that names no layer aborts naming the layer, never
  # with a get1index / subscript error.
  err <- tryCatch(
    estimate_dynam(
      nosuch ~ 1 + ego(x),
      sub_model = "rate",
      data = as_goldfish(tm)
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "not a layer of the data")
  expect_no_match(err, "get1index|subscript out of bounds")
})

test_that("the stamp is a no-op when info$focal already names the modeled layer", {
  # Every prebuilt/assembled object (and every frozen baseline) sets info$focal
  # to the layer it models, so the stamp cannot move a coefficient. Estimating
  # the same model with info$focal set vs. unset gives identical coefficients.
  set <- make_stocnet_fixture_twomode_estimable()
  unset <- set
  unset$info$focal <- NULL

  coef_set <- coef(estimate_dynam(
    membership ~ 1 + ego(x),
    sub_model = "rate",
    data = as_goldfish(set)
  ))
  coef_unset <- coef(estimate_dynam(
    membership ~ 1 + ego(x),
    sub_model = "rate",
    data = as_goldfish(unset)
  ))
  expect_identical(coef_set, coef_unset)
})

test_that("ds_layer_map() degrades to NULL on an absent focal name", {
  src <- new_data_source(data = make_stocnet_fixture_twomode_estimable())
  expect_null(ds_layer_map(src, character(0)))
  expect_null(ds_layer_map(src, NA_character_))
  expect_false(is.null(ds_layer_map(src, "membership")))
})

# The DyNAM-i public surface on the single stocnet data object.
#
# estimate_dynami() accepts a stocnet for DyNAM-i: at the boundary the formula is
# rewritten onto the bridged environment's names (bare nodal attributes read off
# the actor node set, the focal layer name on the LHS becomes the dependent) and
# the stocnet is reversed into the legacy environment. The result must match the
# legacy constructor path exactly.

dynami_surface_fixture <- function() {
  as_goldfish(make_stocnet_fixture_dynami())
}

test_that("a bare-name rate formula on a stocnet matches the legacy path", {
  stocnet <- dynami_surface_fixture()
  surface <- estimate_dynami(
    interactions ~
      1 +
      intercept(interactions, joining = -1) +
      ego(attr1, joining = -1, subType = "centered"),
    sub_model = "rate",
    data = stocnet,
    preprocessing_only = TRUE
  )
  legacy <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      1 +
      intercept(interaction_network_DyNAMi, joining = -1) +
      ego(actors_DyNAMi$attr1, joining = -1, subType = "centered"),
    model = "DyNAMi",
    sub_model = "rate",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )
  expect_equal(surface$initial_stats, legacy$initial_stats)
  expect_equal(surface$dependent_stats_change, legacy$dependent_stats_change)
})

test_that("a bare-name choice formula on a stocnet matches the legacy path", {
  stocnet <- dynami_surface_fixture()
  surface <- estimate_dynami(
    interactions ~ diff(attr1, subType = "averaged_sum"),
    sub_model = "choice",
    data = stocnet,
    preprocessing_only = TRUE
  )
  legacy <- estimate_wrapper(
    dependent.depevents_DyNAMi ~ diff(
      actors_DyNAMi$attr1,
      subType = "averaged_sum"
    ),
    model = "DyNAMi",
    sub_model = "choice",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )
  expect_equal(surface$initial_stats, legacy$initial_stats)
  expect_equal(surface$dependent_stats_change, legacy$dependent_stats_change)
})

test_that("a keyed DyNAM-i rate specification equals the legacy flag formula", {
  stocnet <- dynami_surface_fixture()
  spec <- make_specification(
    rate = list(
      join ~ 1 + ego(attr1, subType = "centered"),
      leave ~ 1 + ego(attr1, subType = "centered")
    ),
    model = "DyNAMi",
    data = stocnet
  )
  surface <- estimate_dynami(
    spec,
    sub_model = "rate",
    preprocessing_only = TRUE
  )
  legacy <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      intercept(interaction_network_DyNAMi, joining = 1) +
      ego(actors_DyNAMi$attr1, joining = 1, subType = "centered") +
      intercept(interaction_network_DyNAMi, joining = -1) +
      ego(actors_DyNAMi$attr1, joining = -1, subType = "centered"),
    model = "DyNAMi",
    sub_model = "rate",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )
  expect_equal(surface$initial_stats, legacy$initial_stats)
  expect_equal(surface$dependent_stats_change, legacy$dependent_stats_change)
})

test_that("an effect under both rate flavors becomes two statistics", {
  stocnet <- dynami_surface_fixture()
  spec <- make_specification(
    rate = list(
      join ~ ego(attr1, subType = "centered"),
      leave ~ ego(attr1, subType = "centered")
    ),
    model = "DyNAMi",
    data = stocnet
  )
  surface <- estimate_dynami(
    spec,
    sub_model = "rate",
    preprocessing_only = TRUE
  )
  expect_equal(dim(surface$initial_stats)[3], 2L)
})

test_that("a flavor-keyed DyNAM-i choice is rejected", {
  stocnet <- dynami_surface_fixture()
  expect_error(
    make_specification(
      choice = list(join ~ diff(attr1)),
      model = "DyNAMi",
      data = stocnet
    ),
    "leaving choice is deterministic"
  )
})

test_that("an unknown DyNAM-i rate flavor is rejected", {
  stocnet <- dynami_surface_fixture()
  expect_error(
    make_specification(
      rate = list(bogus ~ 1),
      model = "DyNAMi",
      data = stocnet
    ),
    "Unknown DyNAM-i rate flavor"
  )
})

test_that("a DyNAM-i choice folds the derived availability constraint", {
  stocnet <- dynami_surface_fixture()
  # The choice preprocesses with the derived `indeg >= 1` availability folded
  # into the dense active_dyad; a rate model, with no dyad part, has no
  # availability to fold.
  expect_no_error(
    estimate_dynami(
      interactions ~ diff(attr1, subType = "averaged_sum"),
      sub_model = "choice",
      data = stocnet,
      preprocessing_only = TRUE
    )
  )
  expect_no_error(
    estimate_dynami(
      interactions ~ intercept(interactions, joining = 1),
      sub_model = "rate",
      data = stocnet,
      preprocessing_only = TRUE
    )
  )
})

test_that("a user support_constraint AND-composes with the derived one", {
  stocnet <- dynami_surface_fixture()
  expect_no_error(
    estimate_dynami(
      interactions ~ diff(attr1, subType = "averaged_sum"),
      sub_model = "choice",
      data = stocnet,
      support_constraint = ~ indeg(interactions) >= 0,
      preprocessing_only = TRUE
    )
  )
})

test_that("the derived availability constraint keeps the occupied groups", {
  # indeg(focal) >= 1 keeps the occupied groups -- Hoffman et al. Eq. 8's
  # denominator over the present second-mode nodes, own singleton included.
  constraint <- dynami_availability_constraint("interactions")
  expect_s3_class(constraint, "formula")
  rhs <- deparse(constraint[[length(constraint)]])
  expect_match(rhs, "indeg\\(interactions\\) >= 1")
  expect_no_match(rhs, "tie\\(interactions\\)")
})

test_that("a past-network effect resolves the past layer on a stocnet", {
  stocnet <- dynami_surface_fixture()
  surface <- estimate_dynami(
    interactions ~ egopop(past, joining = -1, subType = "normalized"),
    sub_model = "rate",
    data = stocnet,
    preprocessing_only = TRUE
  )
  legacy <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      egopop(past_network_DyNAMi, joining = -1, subType = "normalized"),
    model = "DyNAMi",
    sub_model = "rate",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )
  expect_equal(surface$initial_stats, legacy$initial_stats)
  expect_equal(surface$dependent_stats_change, legacy$dependent_stats_change)
})

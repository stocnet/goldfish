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
  expect_equal(surface$initialStats, legacy$initialStats)
  expect_equal(surface$dependentStatsChange, legacy$dependentStatsChange)
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
  expect_equal(surface$initialStats, legacy$initialStats)
  expect_equal(surface$dependentStatsChange, legacy$dependentStatsChange)
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
  expect_equal(surface$initialStats, legacy$initialStats)
  expect_equal(surface$dependentStatsChange, legacy$dependentStatsChange)
})

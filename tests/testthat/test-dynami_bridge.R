# The internal stocnet -> environment bridge for DyNAM-i.
#
# The bridge reverses an assembled DyNAM-i stocnet into the legacy environment
# the preprocessInteraction monolith consumes. Its correctness contract (design
# D3) is exact equivalence with the constructor path: the environment objects
# the monolith reads must equal the ones the legacy constructors build, and the
# environment must resolve internally (every name-recording attribute names an
# object it contains).

test_that("the bridge environment resolves internally", {
  env <- stocnet_to_dynami_env(as_goldfish(make_stocnet_fixture_dynami()))
  present <- ls(env)

  interactions <- get("interactions", env)
  expect_true(all(attr(interactions, "events") %in% present))
  expect_true(all(attr(interactions, "nodes") %in% present))

  dependent <- get("interactions_dependent_events", env)
  expect_true(attr(dependent, "default_network") %in% present)
  expect_true(all(attr(dependent, "nodes") %in% present))

  past <- get("past", env)
  expect_true(all(attr(past, "events") %in% present))
  expect_true(all(attr(past, "nodes") %in% present))
})

test_that("bridge network state equals the constructor path", {
  env <- stocnet_to_dynami_env(as_goldfish(make_stocnet_fixture_dynami()))
  n1 <- nrow(actors_DyNAMi)

  expect_equal(
    matrix(get("interactions", env), n1),
    matrix(interaction_network_DyNAMi, n1),
    ignore_attr = TRUE
  )
  expect_equal(
    matrix(get("past", env), n1),
    matrix(past_network_DyNAMi, n1),
    ignore_attr = TRUE
  )
})

test_that("bridge event streams equal the constructor path", {
  env <- stocnet_to_dynami_env(as_goldfish(make_stocnet_fixture_dynami()))

  dep <- get("interactions_dependent", env)
  exo <- get("interactions_exogenous", env)
  past <- get("past_updates", env)

  cols <- c("time", "sender", "receiver", "increment")
  expect_equal(as.data.frame(dep)[cols], as.data.frame(depevents_DyNAMi)[cols])
  expect_equal(as.data.frame(exo)[cols], as.data.frame(exoevents_DyNAMi)[cols])
  expect_equal(
    as.data.frame(past)[cols],
    as.data.frame(pastupdates_DyNAMi)[cols]
  )

  # The construction's total event order survives the round trip.
  expect_equal(attr(dep, "order"), attr(depevents_DyNAMi, "order"))
  expect_equal(attr(exo, "order"), attr(exoevents_DyNAMi, "order"))
  expect_equal(attr(past, "order"), attr(pastupdates_DyNAMi, "order"))

  # And the update classes the monolith dispatches on.
  expect_s3_class(dep, "interaction.groups.updates")
  expect_s3_class(exo, "interaction.groups.updates")
  expect_s3_class(past, "interaction.network.updates")
})

test_that("the dependent object is a dependent.goldfish on the focal network", {
  env <- stocnet_to_dynami_env(as_goldfish(make_stocnet_fixture_dynami()))
  dependent <- get("interactions_dependent_events", env)
  expect_s3_class(dependent, "dependent.goldfish")
  expect_identical(attr(dependent, "default_network"), "interactions")
})

test_that("preprocessing through the bridge equals the constructor path", {
  # The strongest D3 statement: a DyNAM-i model preprocessed through the bridged
  # environment yields identical statistics to the same model on the legacy
  # constructor environment. The bridge environment needs a parent that reaches
  # the base functions the operand expressions use.
  env <- stocnet_to_dynami_env(
    as_goldfish(make_stocnet_fixture_dynami()),
    parent_env = environment()
  )

  bridge_fit <- estimate_wrapper(
    interactions_dependent_events ~
      1 +
      intercept(interactions, joining = -1) +
      ego(actors$attr1, joining = -1, subType = "centered"),
    model = "DyNAMi",
    sub_model = "rate",
    data = env,
    preprocessing_only = TRUE
  )
  legacy_fit <- estimate_wrapper(
    dependent.depevents_DyNAMi ~
      1 +
      intercept(interaction_network_DyNAMi, joining = -1) +
      ego(actors_DyNAMi$attr1, joining = -1, subType = "centered"),
    model = "DyNAMi",
    sub_model = "rate",
    data = dataDyNAMi,
    preprocessing_only = TRUE
  )

  expect_equal(bridge_fit$initial_stats, legacy_fit$initial_stats)
  expect_equal(
    bridge_fit$dependent_stats_change,
    legacy_fit$dependent_stats_change
  )
})

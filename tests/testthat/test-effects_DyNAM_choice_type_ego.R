# Native `type = "ego"` in DyNAM choice degree-family effects.
# The choice effects delegate to the shared REM machinery, so a choice
# `type = "ego"` statistic must equal the REM-derived expansion (to 1e-6), the
# default must stay the alter perspective, and the two-mode ego guard must be
# preserved through the delegation.

test_that("DyNAM choice indeg(type = 'ego') equals the REM-derived expansion", {
  form <- depNetwork ~ indeg(networkState, type = "ego")
  choice_ego <- estimate_wrapper(
    form,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  # REM rate_ordered stores dependent events only (no right-censored rows), so
  # it is directly comparable to the choice preprocessing for the same effect.
  rem_ego <- estimate_rem(
    form,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )

  expect_equal(
    choice_ego$initial_stats,
    rem_ego$initial_stats,
    tolerance = 1e-6,
    label = "initial ego-perspective statistic matches REM"
  )
  expect_equal(
    ReducePreprocess(choice_ego),
    ReducePreprocess(rem_ego),
    tolerance = 1e-6
  )
})

test_that("DyNAM choice outdeg(type = 'ego') equals the REM-derived expansion", {
  form <- depNetwork ~ outdeg(networkState, type = "ego")
  choice_ego <- estimate_wrapper(
    form,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  rem_ego <- estimate_rem(
    form,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(choice_ego$initial_stats, rem_ego$initial_stats, tolerance = 1e-6)
  expect_equal(
    ReducePreprocess(choice_ego),
    ReducePreprocess(rem_ego),
    tolerance = 1e-6
  )
})

test_that("DyNAM choice degree default stays the alter perspective", {
  default_choice <- estimate_wrapper(
    depNetwork ~ indeg(networkState),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  alter_choice <- estimate_wrapper(
    depNetwork ~ indeg(networkState, type = "alter"),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  ego_choice <- estimate_wrapper(
    depNetwork ~ indeg(networkState, type = "ego"),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(
    default_choice$initial_stats,
    alter_choice$initial_stats,
    tolerance = 1e-12,
    label = "default perspective is unchanged (alter)"
  )
  # the ego perspective genuinely differs from the alter default.
  expect_false(isTRUE(all.equal(
    ego_choice$initial_stats,
    alter_choice$initial_stats
  )))
})

test_that("DyNAM choice preserves the two-mode ego guard (mirrors REM)", {
  network <- matrix(
    c(
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      2,
      0,
      0,
      0,
      3,
      1,
      0,
      0,
      0,
      0,
      4,
      1,
      2,
      0,
      0,
      0,
      0
    ),
    nrow = 5,
    ncol = 6,
    byrow = TRUE
  )
  effect_fun <- function(
    weighted = FALSE,
    is_two_mode = TRUE,
    transformer_fn = identity,
    type = "ego"
  ) {
    NULL
  }
  expect_error(
    init_DyNAM_choice.indeg(effect_fun, network, NULL, 5, 6),
    "cannot be computed on a two-mode network"
  )
})

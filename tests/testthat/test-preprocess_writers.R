se_data <- baselines_social_evolution_data()

test_that("compute_stats(output = 'default') returns a preprocessed object", {
  prep <- compute_stats(
    callsDependent ~ inertia + recip + trans,
    data = se_data, model = "DyNAM", sub_model = "choice"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
  expect_true(!is.null(prep$stat_mat_update))
  expect_null(prep$stats_change)
})

test_that("compute_stats(output = 'gather') matches gather_model_data (choice)", {
  skip_on_cran()
  old <- gather_model_data(
    callsDependent ~ inertia + recip + trans,
    model = "DyNAM", sub_model = "choice", data = se_data
  )
  new <- compute_stats(
    callsDependent ~ inertia + recip + trans,
    model = "DyNAM", sub_model = "choice", data = se_data,
    output = "gather"
  )
  for (f in c(
    "stat_all_events", "selected", "n_candidates", "sender", "receiver",
    "namesEffects", "has_intercept"
  )) {
    expect_equal(new[[f]], old[[f]], ignore_attr = TRUE, info = f)
  }
})

test_that("compute_stats(output = 'gather') matches gather_model_data (REM)", {
  skip_on_cran()
  old <- gather_model_data(
    callsDependent ~ 1 + inertia + recip,
    model = "REM", sub_model = "choice", data = se_data
  )
  new <- compute_stats(
    callsDependent ~ 1 + inertia + recip,
    model = "REM", sub_model = "rate", data = se_data, output = "gather"
  )
  for (f in c(
    "stat_all_events", "selected", "n_candidates", "n_candidates1",
    "n_candidates2", "selected_actor1", "selected_actor2", "sender",
    "receiver", "namesEffects"
  )) {
    expect_equal(new[[f]], old[[f]], ignore_attr = TRUE, info = f)
  }
})

test_that(
  "compute_stats(output = 'gather') matches gather_model_data (coordination)",
  {
    skip_on_cran()
    old <- gather_model_data(
      callsDependent ~ inertia + trans,
      model = "DyNAM", sub_model = "choice_coordination", data = se_data
    )
    new <- compute_stats(
      callsDependent ~ inertia + trans,
      model = "DyNAM", sub_model = "choice_coordination", data = se_data,
      output = "gather"
    )
    for (f in c(
      "stat_all_events", "selected", "n_candidates", "sender", "receiver",
      "namesEffects"
    )) {
      expect_equal(new[[f]], old[[f]], ignore_attr = TRUE, info = f)
    }
  }
)

test_that("gather output for rate models is internally consistent", {
  skip_on_cran()
  # gather_model_data() errors on one-mode rate (twomode_or_reflexive = FALSE
  # with a single receiver column); the gather writer follows the working
  # gather_compute estimation path (twomode_or_reflexive = TRUE) instead.
  gathered <- compute_stats(
    callsDependent ~ 1 + indeg + outdeg,
    model = "DyNAM", sub_model = "rate", data = se_data, output = "gather"
  )
  expect_equal(nrow(gathered$stat_all_events), sum(gathered$n_candidates))
  expect_true(gathered$has_intercept)
  expect_equal(ncol(gathered$stat_all_events), 3L)
  expect_equal(gathered$namesEffects[1], "Intercept")
})

test_that("compute_stats rejects unknown output values", {
  expect_error(
    compute_stats(
      callsDependent ~ inertia,
      data = se_data, model = "DyNAM", sub_model = "choice",
      output = "parquet"
    ),
    "default"
  )
})

test_that("compute_stats(output = 'db') is not yet implemented", {
  expect_error(
    compute_stats(
      callsDependent ~ inertia,
      data = se_data, model = "DyNAM", sub_model = "choice",
      output = "db"
    ),
    "not yet implemented"
  )
})

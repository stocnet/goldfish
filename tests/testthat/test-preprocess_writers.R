se_data <- baselines_social_evolution_data()

test_that("compute_statistics(output = 'preprocessed') returns a preprocessed object", {
  prep <- compute_statistics(
    calls_dependent ~ inertia + recip + trans,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
  expect_true(!is.null(prep$stat_mat_update))
  expect_null(prep$stats_change)
})

test_that("compute_statistics(output = 'gather') matches gather_model_data (choice)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  old <- gather_model_data(
    calls_dependent ~ inertia + recip + trans,
    model = "DyNAM",
    sub_model = "choice",
    data = se_data
  )
  new <- compute_statistics(
    calls_dependent ~ inertia + recip + trans,
    model = "DyNAM",
    sub_model = "choice",
    data = se_data,
    output = "gather"
  )
  for (f in c(
    "stat_all_events",
    "selected",
    "n_candidates",
    "sender",
    "receiver",
    "namesEffects",
    "has_intercept"
  )) {
    expect_equal(new[[f]], old[[f]], ignore_attr = TRUE, info = f)
  }
})

test_that("compute_statistics(output = 'gather') matches gather_model_data (REM)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  old <- gather_model_data(
    calls_dependent ~ 1 + inertia + recip,
    model = "REM",
    sub_model = "rate",
    data = se_data
  )
  new <- compute_statistics(
    calls_dependent ~ 1 + inertia + recip,
    model = "REM",
    sub_model = "rate",
    data = se_data,
    output = "gather"
  )
  for (f in c(
    "stat_all_events",
    "selected",
    "n_candidates",
    "index_i",
    "index_j",
    "sender",
    "receiver",
    "namesEffects"
  )) {
    expect_equal(new[[f]], old[[f]], ignore_attr = TRUE, info = f)
  }
})

test_that("compute_statistics(output = 'gather') matches gather_model_data (coordination)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  old <- gather_model_data(
    calls_dependent ~ inertia + trans,
    model = "DyNAM",
    sub_model = "choice_coordination",
    data = se_data
  )
  new <- compute_statistics(
    calls_dependent ~ inertia + trans,
    model = "DyNAM",
    sub_model = "choice_coordination",
    data = se_data,
    output = "gather"
  )
  for (f in c(
    "stat_all_events",
    "selected",
    "n_candidates",
    "index_i",
    "index_j",
    "sender",
    "receiver",
    "namesEffects"
  )) {
    expect_equal(new[[f]], old[[f]], ignore_attr = TRUE, info = f)
  }
})

test_that("gather output for rate models is internally consistent", {
  skip_on_cran()
  # gather_model_data() errors on one-mode rate (twomode_or_reflexive = FALSE
  # with a single receiver column); the gather writer follows the working
  # gather_compute estimation path (twomode_or_reflexive = TRUE) instead.
  gathered <- compute_statistics(
    calls_dependent ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = se_data,
    output = "gather"
  )
  expect_equal(nrow(gathered$stat_all_events), sum(gathered$n_candidates))
  expect_true(gathered$has_intercept)
  expect_equal(ncol(gathered$stat_all_events), 3L)
  expect_equal(gathered$namesEffects[1], "Intercept")
})

test_that("compute_statistics rejects unknown output values", {
  expect_error(
    compute_statistics(
      calls_dependent ~ inertia,
      data = se_data,
      model = "DyNAM",
      sub_model = "choice",
      output = "parquet"
    ),
    "preprocessed"
  )
})

test_that("compute_statistics(output = 'db') requires a DBI connection", {
  expect_error(
    compute_statistics(
      calls_dependent ~ inertia,
      data = se_data,
      model = "DyNAM",
      sub_model = "choice",
      output = "db"
    ),
    "DBI connection"
  )
})

test_that("preprocessed object carries a well-formed broadcast buffer", {
  data_list <- list(
    social_evolution = baselines_social_evolution_data(),
    fisheries = baselines_fisheries_data(),
    social_evolution_global = baselines_global_data()
  )
  grid <- c(baselines_model_grid(), baselines_global_model_grid())
  # models whose effects are all cell-specific emit no broadcasts; models with
  # alter/ego/degree/global fan-out emit a non-empty broadcast buffer.
  has_broadcast <- c(
    se_rem = TRUE,
    se_rem_ordered = TRUE,
    fish_dynam_choice = TRUE,
    fish_dynam_choice_coord = TRUE,
    fish_rem = TRUE,
    fish_rem_ordered = TRUE,
    global_dynam_rate = TRUE,
    global_rem = TRUE
  )

  for (nm in names(grid)) {
    spec <- grid[[nm]]
    prep <- compute_statistics(
      spec$formula,
      data = data_list[[spec$dataset]],
      model = spec$model,
      sub_model = if (spec$model == "DyNAM") spec$sub_model else "rate"
    )
    n_stored <- length(prep$stat_mat_pointer)
    expect_true(!is.null(prep$stat_mat_broadcast), info = nm)
    expect_equal(nrow(prep$stat_mat_broadcast), 4L, info = nm)
    expect_equal(
      length(prep$stat_mat_broadcast_pointer),
      n_stored,
      info = nm
    )
    # pointer is non-decreasing and ends at the buffer column count
    expect_false(is.unsorted(prep$stat_mat_broadcast_pointer), info = nm)
    expect_equal(
      prep$stat_mat_broadcast_pointer[n_stored],
      ncol(prep$stat_mat_broadcast),
      info = nm
    )
    expect_equal(prep$version, PREPROCESSED_GOLDFISH_VERSION, info = nm)
    if (isTRUE(has_broadcast[nm])) {
      expect_gt(ncol(prep$stat_mat_broadcast), 0L)
    }
  }
})

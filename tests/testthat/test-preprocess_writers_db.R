se_data <- baselines_social_evolution_data()

test_that("compute_stats(output = 'db') round-trips against the gather writer", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  formula <- callsDependent ~ inertia + recip + trans
  gathered <- compute_stats(
    formula, data = se_data, model = "DyNAM", sub_model = "choice",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  descriptor <- compute_stats(
    formula, data = se_data, model = "DyNAM", sub_model = "choice",
    output = "db",
    control_preprocessing = set_preprocessing_opt(db = con, db_table = "stats")
  )
  expect_s3_class(descriptor, "preprocessed_db.goldfish")
  expect_null(descriptor$stat_all_events)
  expect_identical(descriptor$db_table, "stats")

  tbl <- DBI::dbReadTable(con, "stats")
  stat_cols <- grep("^stat_", names(tbl), value = TRUE)
  written <- as.matrix(tbl[, stat_cols])
  dimnames(written) <- NULL
  expect_equal(nrow(tbl), nrow(gathered$stat_all_events))
  expect_equal(written, unname(gathered$stat_all_events))
  expect_equal(
    as.integer(table(tbl$event_id)), as.integer(gathered$n_candidates)
  )
})

test_that("compute_stats(output = 'db') errors when no connection is configured", {
  skip_on_cran()
  expect_error(
    compute_stats(
      callsDependent ~ inertia, data = se_data,
      model = "DyNAM", sub_model = "choice", output = "db"
    ),
    "DBI connection"
  )
})

test_that("db writer round-trips for a rate model", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  formula <- callsDependent ~ 1 + indeg + outdeg
  gathered <- compute_stats(
    formula, data = se_data, model = "DyNAM", sub_model = "rate",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  compute_stats(
    formula, data = se_data, model = "DyNAM", sub_model = "rate",
    output = "db",
    control_preprocessing = set_preprocessing_opt(db = con, db_table = "rate_stats")
  )
  tbl <- DBI::dbReadTable(con, "rate_stats")
  stat_cols <- grep("^stat_", names(tbl), value = TRUE)
  written <- as.matrix(tbl[, stat_cols])
  dimnames(written) <- NULL
  expect_equal(written, unname(gathered$stat_all_events))
  expect_equal(nrow(tbl), sum(gathered$n_candidates))
})

se_data <- baselines_social_evolution_data()

test_that("compute_stats(output = 'db') round-trips against the gather writer", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  formula <- calls_dependent ~ inertia + recip + trans
  gathered <- compute_stats(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  descriptor <- compute_stats(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
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
    as.integer(table(tbl$event_id)),
    as.integer(gathered$n_candidates)
  )
  # the index vocabulary is written to SQL and round-trips
  expect_true(all(c("index_i", "index_j") %in% names(tbl)))
  expect_equal(tbl$index_i, gathered$index_i)
  expect_equal(tbl$index_j, gathered$index_j)
})

test_that("gather index columns decode to node labels; coordination is filtered", {
  skip_on_cran()
  nodes <- se_data$nodes
  # DyNAM choice: index_i is the (constant) event sender, index_j the receiver;
  # the selected row decodes to the observed sender/receiver labels.
  gc <- compute_stats(
    calls_dependent ~ inertia + recip,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  ev_start <- cumsum(c(0L, utils::head(gc$n_candidates, -1L)))
  sel_row <- ev_start + gc$selected
  expect_equal(nodes$label[gc$index_i[sel_row]], gc$sender)
  expect_equal(nodes$label[gc$index_j[sel_row]], gc$receiver)

  # Coordination: no reflexive diagonal rows (index_i != index_j everywhere),
  # and the observed row's unordered pair matches {sender, receiver}.
  cc <- compute_stats(
    calls_dependent ~ inertia + trans,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice_coordination",
    output = "gather"
  )
  expect_true(all(cc$index_i != cc$index_j))
  cc_start <- cumsum(c(0L, utils::head(cc$n_candidates, -1L)))
  cc_sel <- cc_start + cc$selected
  obs_pair <- cbind(
    nodes$label[cc$index_i[cc_sel]],
    nodes$label[cc$index_j[cc_sel]]
  )
  expect_true(all(
    (obs_pair[, 1] == cc$sender & obs_pair[, 2] == cc$receiver) |
      (obs_pair[, 1] == cc$receiver & obs_pair[, 2] == cc$sender)
  ))
})

test_that("compute_stats(output = 'db') errors when no connection is configured", {
  skip_on_cran()
  expect_error(
    compute_stats(
      calls_dependent ~ inertia,
      data = se_data,
      model = "DyNAM",
      sub_model = "choice",
      output = "db"
    ),
    "DBI connection"
  )
})

test_that("db writer round-trips for a rate model", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  formula <- calls_dependent ~ 1 + indeg + outdeg
  gathered <- compute_stats(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "rate",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  compute_stats(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "rate",
    output = "db",
    control_preprocessing = set_preprocessing_opt(
      db = con,
      db_table = "rate_stats"
    )
  )
  tbl <- DBI::dbReadTable(con, "rate_stats")
  stat_cols <- grep("^stat_", names(tbl), value = TRUE)
  written <- as.matrix(tbl[, stat_cols])
  dimnames(written) <- NULL
  expect_equal(written, unname(gathered$stat_all_events))
  expect_equal(nrow(tbl), sum(gathered$n_candidates))
})

se_data <- baselines_social_evolution_data()

test_that("compute_statistics(output = 'db') round-trips against the gather writer", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  formula <- calls_dependent ~ inertia + recip + trans
  gathered <- compute_statistics(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  descriptor <- compute_statistics(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    control_prep = set_preprocessing(db = con, db_table = "stats")
  )
  expect_s3_class(descriptor, "goldfishStatDB")
  expect_null(descriptor$stat_all_events)
  expect_identical(descriptor$db_table, "stats")

  tbl <- DBI::dbReadTable(con, "stats_1")
  # The statistics columns are named by their effect, not by position, so the
  # table says which effect each column holds without the producing session.
  expect_identical(
    setdiff(names(tbl), c("event_id", "is_selected", "index_i", "index_j")),
    gathered$names_effects
  )
  written <- as.matrix(tbl[, gathered$names_effects])
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
  gc <- compute_statistics(
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
  cc <- compute_statistics(
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

test_that("compute_statistics(output = 'db') errors when no connection is configured", {
  skip_on_cran()
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

test_that("db writer round-trips for a rate model", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  formula <- calls_dependent ~ 1 + indeg + outdeg
  gathered <- compute_statistics(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "rate",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  compute_statistics(
    formula,
    data = se_data,
    model = "DyNAM",
    sub_model = "rate",
    output = "db",
    control_prep = set_preprocessing(
      db = con,
      db_table = "rate_stats"
    )
  )
  tbl <- DBI::dbReadTable(con, "rate_stats_1")
  written <- as.matrix(tbl[, gathered$names_effects])
  dimnames(written) <- NULL
  expect_equal(written, unname(gathered$stat_all_events))
  expect_equal(nrow(tbl), sum(gathered$n_candidates))
})

test_that("an effect colliding with an identity column is disambiguated", {
  # The reserved names take part in the uniqueness pass, so a statistic named
  # like an identity column gets its own column instead of overwriting one.
  expect_identical(
    stat_column_names(c("inertia", "index_j", "recip"), DB_RESERVED_COLUMNS),
    c("inertia", "index_j_1", "recip")
  )
  expect_identical(
    stat_column_names(character(0), DB_RESERVED_COLUMNS),
    character(0)
  )
  # The frame reserves labels and exposure on top of the indices, so an effect
  # colliding with one of those is disambiguated there and not in the db export.
  expect_identical(
    stat_column_names("timespan", FRAME_RESERVED_COLUMNS),
    "timespan_1"
  )
  expect_identical(
    stat_column_names("timespan", DB_RESERVED_COLUMNS),
    "timespan"
  )
})

test_that("a single-process export writes the flavored four-table shape", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  descriptor <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    control_prep = set_preprocessing(db = con, db_table = "stats")
  )

  # An export is always K processes, K >= 1: one reader handles both cases, and
  # a model that later gains flavors does not change the schema.
  expect_setequal(
    DBI::dbListTables(con),
    c("stats_1", "stats_map", "stats_nodes")
  )
  map <- DBI::dbReadTable(con, "stats_map")
  expect_identical(nrow(map), 1L)
  expect_identical(map$fid, 1L)
  expect_identical(map$table_name, "stats_1")
  expect_identical(map$family, "choice")
  expect_true(is.na(map$flavor))
  # The layer the process belongs to, so a table names its own provenance.
  expect_identical(map$layer, "call_network")
  expect_identical(
    unname(descriptor$db_tables),
    c("stats_1", "stats_map", "stats_nodes")
  )
})

test_that("a failed write names the process and the last written event", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  gathered <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = se_data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)
  # "event 0" is ambiguous across the tables of a flavored export, so the
  # process whose table failed is named alongside it.
  expect_error(
    write_gather_to_db(gathered, con, "stats", fid = 2L),
    "stats_2"
  )
  expect_error(
    write_gather_to_db(gathered, con, "stats", fid = 2L),
    "Process"
  )
})

test_that("the index columns join through the node table on a two-mode model", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  compute_statistics(
    membership ~ inertia,
    data = as_goldfish(make_stocnet_fixture_twomode()),
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    control_prep = set_preprocessing(db = con, db_table = "stats")
  )

  # The join the schema exists for: local indices alone cannot address the
  # original nodes rows of a two-mode model, so it runs in SQL through
  # `stats_nodes` rather than off-database.
  labelled <- DBI::dbGetQuery(
    con,
    "SELECT s.index_i, s.index_j, n1.label AS sender, n2.label AS receiver
     FROM stats_1 AS s
     JOIN stats_nodes AS n1 ON n1.side = 1 AND n1.local = s.index_i
     JOIN stats_nodes AS n2 ON n2.side = 2 AND n2.local = s.index_j"
  )
  expect_identical(nrow(labelled), nrow(DBI::dbReadTable(con, "stats_1")))
  expect_false(anyNA(labelled$sender))
  expect_setequal(labelled$sender, c("A", "B"))
  expect_setequal(labelled$receiver, c("X", "Y"))
})

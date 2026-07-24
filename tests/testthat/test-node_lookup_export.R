# The gather / db export carries a node lookup (side, local index, global id,
# label) so a consumer resolves the export's index_i / index_j local indices
# back to the original `nodes` row and label -- without re-deriving the mode map
# and without the raw nodes frame, which mislabels a subset or two-mode model
# where the local index is not the global row.

# A subset one-mode focal layer: identical sender/receiver sets over a strict
# subset of the modes, so one node ("D") is off the modeled side and the local
# indices skip its global row.
subset_one_mode_fixture <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = c("p", "p", "p", "o"),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 3L, 1L),
    to = c(2L, 3L, 1L, 3L),
    time = c(1, 2, 3, 4),
    layer = "calls",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "sub",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event"),
    sender = c(calls = "p"),
    receiver = c(calls = "p")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# Recover the labels a side's local indices point at, through the lookup alone.
join_side <- function(lookup, index, side) {
  rows <- lookup[lookup$side == side, , drop = FALSE]
  rows$label[match(index, rows$local)]
}

test_that("one-mode gather lookup joins index_i/index_j back to node labels", {
  x <- make_stocnet_fixture()
  out <- gather_model_data(
    calls ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = x
  )

  lookup <- out$node_lookup
  expect_named(lookup, c("side", "local", "global", "label"))
  expect_true(all(lookup$side == 1L))
  # global indexes the original nodes tibble: the lookup joins back to it.
  expect_equal(lookup$label, x$nodes$label[lookup$global])
  # A full one-mode model indexes every node, so local == global here.
  expect_equal(lookup$local, lookup$global)

  # Every candidate row decodes to a real label through the lookup.
  expect_false(anyNA(join_side(lookup, out$index_i, 1L)))
  expect_false(anyNA(join_side(lookup, out$index_j, 1L)))
})

test_that("subset one-mode lookup carries global ids that skip off-side nodes", {
  x <- subset_one_mode_fixture()
  out <- gather_model_data(
    calls ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = x
  )

  lookup <- out$node_lookup
  expect_true(all(lookup$side == 1L))
  # The off-side node "D" (global 4) is absent; the three modeled nodes keep
  # their global ids while local runs 1..3.
  expect_equal(lookup$global, c(1L, 2L, 3L))
  expect_equal(lookup$label, c("A", "B", "C"))
  expect_equal(lookup$label, x$nodes$label[lookup$global])

  # The lookup resolves the indices correctly where the raw nodes frame still
  # happens to agree (the subset is a prefix), but the point is the join is on
  # the lookup, not the full nodes frame.
  expect_false(anyNA(join_side(lookup, out$index_i, 1L)))
  expect_false(anyNA(join_side(lookup, out$index_j, 1L)))
})

test_that("two-mode gather lookup resolves each side's indices", {
  x <- make_stocnet_fixture_twomode()
  out <- gather_model_data(
    membership ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = x
  )

  lookup <- out$node_lookup
  expect_setequal(lookup$side, c(1L, 2L))
  # Sender side (p: A, B) and receiver side (o: X, Y) keep their global ids.
  side1 <- lookup[lookup$side == 1L, ]
  side2 <- lookup[lookup$side == 2L, ]
  expect_equal(side1$global, c(1L, 2L))
  expect_equal(side1$label, c("A", "B"))
  expect_equal(side2$global, c(3L, 4L))
  expect_equal(side2$label, c("X", "Y"))
  expect_equal(lookup$label, x$nodes$label[lookup$global])

  # index_i indexes side 1, index_j indexes side 2; both resolve through the
  # lookup, which the single raw nodes frame cannot do (local != global there).
  expect_false(anyNA(join_side(lookup, out$index_i, 1L)))
  expect_false(anyNA(join_side(lookup, out$index_j, 2L)))
})

test_that("a two-mode preprocessed result carries a per-side node lookup", {
  # The gather path is not the only consumer: preprocessing_only results feed
  # residuals / event-scores, so the lookup must ride on prep too, resolving each
  # side's local index (which is not the global row on a two-mode model).
  prep <- estimate_dynam(
    membership ~ inertia,
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_twomode()),
    preprocessing_only = TRUE
  )

  lookup <- prep$node_lookup
  expect_named(lookup, c("side", "local", "global", "label"))
  expect_setequal(lookup$side, c(1L, 2L))
  side1 <- lookup[lookup$side == 1L, ]
  side2 <- lookup[lookup$side == 2L, ]
  expect_equal(side1$label, c("A", "B"))
  expect_equal(side2$label, c("X", "Y"))
  expect_equal(side2$local, c(1L, 2L))
  expect_equal(side2$global, c(3L, 4L))
})

test_that("the db export descriptor carries the node lookup", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  descriptor <- compute_statistics(
    calls ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    data = make_stocnet_fixture(),
    control_prep = set_preprocessing(db = con, db_table = "stats")
  )

  expect_s3_class(descriptor, "preprocessed_db.goldfish")
  lookup <- descriptor$node_lookup
  expect_named(lookup, c("side", "local", "global", "label"))
  # The long SQL table's index_i / index_j join to this lookup off-database.
  tbl <- DBI::dbReadTable(con, "stats")
  expect_false(anyNA(join_side(lookup, tbl$index_i, 1L)))
})

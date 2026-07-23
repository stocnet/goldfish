# Deterministic ordering ------------------------------------------------------

make_order_events <- function() {
  data.frame(
    time = c(2, 1, 1, 1, 2),
    is_dependent = c(TRUE, FALSE, TRUE, FALSE, FALSE),
    component = c("ties", "changes", "ties", "global", "ties"),
    layer = c("calls", "", "calls", "", "friendship"),
    from = c(3L, 0L, 1L, 0L, 2L),
    to = c(1L, 0L, 2L, 0L, 3L),
    update = "increment",
    stringsAsFactors = FALSE
  )
}

test_that("ordering follows the sort key and is independent of input order", {
  events <- make_order_events()
  ordered <- order_events(events)

  expect_equal(
    ordered$time,
    c(1, 1, 1, 2, 2),
    label = "time is the primary key"
  )
  expect_equal(
    ordered$is_dependent[1:3],
    c(TRUE, FALSE, FALSE),
    label = "at t=1 the dependent event precedes the exogenous ones"
  )
  expect_equal(
    ordered$component[2:3],
    c("changes", "global"),
    label = "exogenous events rank ties < changes < global"
  )
  expect_equal(
    ordered$layer[4:5],
    c("calls", "friendship"),
    label = "at t=2 the dependent tie event precedes the exogenous layer"
  )

  set.seed(42)
  permuted <- events[sample(nrow(events)), , drop = FALSE]
  expect_equal(
    order_events(permuted),
    ordered,
    ignore_attr = "row.names",
    label = "a permuted input yields the same schedule"
  )
})

test_that("the order column replaces from/to as the final tie-break", {
  events <- data.frame(
    time = c(1, 1, 1),
    component = "ties",
    layer = "calls",
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 1L),
    order = c(3L, 1L, 2L),
    update = "increment",
    stringsAsFactors = FALSE
  )
  expect_equal(
    order_events(events)$order,
    c(1L, 2L, 3L),
    label = "same-time rows sequence by order, not by from/to"
  )
})

test_that("same-time replaces on one target abort without an order tie-break", {
  events <- data.frame(
    time = c(1, 1),
    component = "ties",
    layer = "friendship",
    from = c(1L, 1L),
    to = c(2L, 2L),
    update = "replace",
    stringsAsFactors = FALSE
  )
  expect_error(order_events(events), "Ambiguous same-time")

  resolved <- events
  resolved$order <- c(1L, 2L)
  expect_no_error(order_events(resolved))

  tied <- events
  tied$order <- c(1L, 1L)
  expect_error(
    order_events(tied),
    "Ambiguous same-time",
    label = "an order column with duplicate values resolves nothing"
  )
})

test_that("same-time increments on one target commute and pass silently", {
  events <- data.frame(
    time = c(1, 1),
    component = "ties",
    layer = "calls",
    from = c(1L, 1L),
    to = c(2L, 2L),
    update = "increment",
    stringsAsFactors = FALSE
  )
  expect_no_error(order_events(events))
})

test_that("same-time replaces on different targets are unambiguous", {
  events <- data.frame(
    time = c(1, 1),
    component = "ties",
    layer = "friendship",
    from = c(1L, 2L),
    to = c(2L, 3L),
    update = "replace",
    stringsAsFactors = FALSE
  )
  expect_no_error(order_events(events))
})

# Component splitting ---------------------------------------------------------

test_that("ties split into per-layer streams keeping NA-time history", {
  x <- make_stocnet_fixture()
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map)

  expect_named(streams$network, "calls")
  calls <- streams$network[["calls"]]
  expect_equal(calls$time, c(NA, 1, 2))
  expect_equal(calls$from, c(1L, 2L, 3L))
  expect_equal(calls$value, rep(1, 3), label = "absent weight defaults to 1")
  expect_equal(calls$update, rep("increment", 3))
})

test_that("the dependent stream carries the timed focal rows only", {
  x <- make_stocnet_fixture()
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map)

  expect_equal(
    streams$dependent$time,
    c(1, 2),
    label = "the NA-time history row is state, not a dependent event"
  )
  expect_equal(streams$focal, "calls")
})

test_that("a keyed flavor selects the modeled dependent rows only", {
  x <- make_stocnet_fixture()
  x$ties$flavor <- c(NA, "creation", "dissolution")
  map <- build_mode_map(x$info, x$nodes, "calls")

  streams <- split_stocnet_streams(x, map, modeled_flavor = "creation")
  expect_equal(streams$dependent$time, 1)
  expect_equal(
    nrow(streams$network[["calls"]]),
    3,
    label = "non-modeled flavors stay in the network stream as state updates"
  )

  all_rows <- split_stocnet_streams(x, map)
  expect_equal(
    all_rows$dependent$time,
    c(1, 2),
    label = "without a flavor key every timed focal row is modeled"
  )
})

test_that("weight supplies the tie value under the layer's update semantics", {
  x <- make_stocnet_fixture()
  x$ties$weight <- c(2, 5, 7)
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map)

  expect_equal(streams$network[["calls"]]$value, c(2, 5, 7))
})

test_that("active changes route to per-side composition streams", {
  x <- make_stocnet_fixture_twomode()
  x$changes <- rbind(
    x$changes,
    data.frame(
      time = 3,
      node = 4L,
      var = "active",
      value = I(list(list(FALSE)))
    )
  )
  map <- build_mode_map(x$info, x$nodes, "membership")
  streams <- split_stocnet_streams(x, map)

  expect_named(streams$composition, c("mode1", "mode2"))
  expect_equal(
    streams$composition$mode1$node,
    c(1L, 2L),
    label = "sender-side nodes keep their sender-side local indices"
  )
  expect_equal(
    streams$composition$mode2$node,
    2L,
    label = "global node 4 is receiver-side local 2"
  )
  expect_length(streams$attribute, 0)
})

test_that("non-active changes and global rows split per view and variable", {
  x <- make_stocnet_fixture()
  x$changes <- data.frame(time = c(1, 2), node = c(1L, 3L), var = "floor")
  x$changes$value <- list(list(3), list(4))
  x$global <- data.frame(time = 1, var = "season")
  x$global$value <- list(list(2))
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map)

  # Nodal streams are keyed by the view they write into; a global attribute has
  # no node space, so its stream stays keyed by the variable alone.
  expect_named(streams$attribute, "nodal:p$floor")
  expect_equal(streams$attribute[["nodal:p$floor"]]$node, c(1L, 3L))
  expect_equal(unlist(streams$attribute[["nodal:p$floor"]]$value), c(3, 4))
  expect_named(streams$global, "season")
  expect_equal(unlist(streams$global$season$value), 2)
})

test_that("composition splits on the focal pair, dropping other modes", {
  # Known limitation: the composition split is driven by the *focal* layer's
  # side pair alone, so on a multipartite object a node belonging only to a
  # covariate layer's mode (here `org`, reached by `member` but not `attend`)
  # appears in neither stream. Lifting this needs a per-layer composition
  # split, which the engine's two index spaces do not currently carry.
  x <- make_stocnet_fixture_multipartite()
  x$changes <- data.frame(
    time = c(1, 2),
    node = c(1L, 6L),
    var = "active",
    stringsAsFactors = FALSE
  )
  x$changes$value <- list(list(FALSE), list(FALSE))
  map <- build_mode_map(x$info, x$nodes, c("attend", "coauthor", "member"))
  streams <- split_stocnet_streams(x, map, focal = "attend")

  expect_equal(
    streams$composition$mode1$node,
    1L,
    label = "the actor is on the focal sender side"
  )
  expect_equal(
    nrow(streams$composition$mode2),
    0L,
    label = "the org is in neither focal side and is dropped"
  )
})

# Nodal attribute streams -----------------------------------------------------

test_that("a receiver-side attribute change splits into the receiver's view", {
  # Regression: `attend ~ alter(size)` with a `size` change on a receiver-side
  # node died with "missing value where TRUE/FALSE needed". The stream kept the
  # global node id (4 = E1) while the view it writes into is the side-local
  # length-2 event vector, so `attribute[4]` was NA.
  x <- make_stocnet_fixture_multipartite()
  map <- build_mode_map(x$info, x$nodes, c("attend", "coauthor", "member"))
  streams <- split_stocnet_streams(x, map, focal = "attend")

  expect_named(streams$attribute, "nodal:event$size")
  expect_equal(
    streams$attribute[["nodal:event$size"]]$node,
    1L,
    label = "global id 4 is E1, local 1 on the event view"
  )
  # Actors have no `size` change, so their view has no stream at all rather
  # than a stream of events that belong to someone else.
  expect_false("nodal:actor$size" %in% names(streams$attribute))
})

test_that("attribute changes split per view when neither side starts at 1", {
  # The sender-side counterpart: the multipartite fixture's senders are the
  # first mode, so their global and local ids coincide and a stream left in the
  # global space silently agrees. Here side 1 is global 3:5 and side 2 is 1:2.
  x <- make_stocnet_fixture_twomode_offset()
  map <- build_mode_map(x$info, x$nodes, "assign")
  streams <- split_stocnet_streams(x, map, focal = "assign")

  expect_setequal(
    names(streams$attribute),
    c("nodal:worker$skill", "nodal:task$skill")
  )
  expect_equal(
    streams$attribute[["nodal:worker$skill"]]$node,
    3L,
    label = "global id 5 is W3, local 3 on the worker view"
  )
  expect_equal(
    streams$attribute[["nodal:task$skill"]]$node,
    2L,
    label = "global id 2 is T2, local 2 on the task view"
  )
})

test_that("a one-mode object keeps one attribute stream per variable", {
  # The collapse that keeps the one-mode path on its existing route: one node
  # space, so one stream, and its local ids are the global ids.
  x <- make_stocnet_fixture()
  x$changes <- data.frame(time = 1.5, node = 3L, var = "floor")
  x$changes$value <- list(list(7))
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map, focal = "calls")

  expect_named(streams$attribute, "nodal:p$floor")
  expect_equal(streams$attribute[["nodal:p$floor"]]$node, 3L)
})

test_that("two-mode models with time-varying nodal covariates preprocess", {
  # The end-to-end regressions: both of these aborted before the streams were
  # split per view.
  expect_no_error(estimate_dynam(
    attend ~ alter(size),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_multipartite()),
    preprocessing_only = TRUE
  ))
  expect_no_error(estimate_dynam(
    assign ~ ego(skill),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_twomode_offset()),
    preprocessing_only = TRUE
  ))
})

test_that("two-mode broadcast expansion keeps the diagonal dyad", {
  # ReduceBroadcastFlat excludes node1 == node2 only on a one-mode model (no
  # self-tie). On two-mode the sender and receiver index spaces are unrelated, so
  # the alter broadcast for alter 1 must expand to every sender, including the
  # (1, 1) dyad a one-mode model would drop. The flag now rides on the spec, so
  # this also pins that source.
  prep <- estimate_dynam(
    attend ~ alter(size),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_multipartite()),
    preprocessing_only = TRUE
  )
  expect_true(prep$model_spec$is_two_mode)

  expanded <- ReducePreprocess(prep, "withoutTime")[[1]]
  expect_equal(sort(expanded[, "node1"]), c(1, 2, 3))
  diag_row <- expanded[
    expanded[, "node1"] == 1 & expanded[, "node2"] == 1,
    ,
    drop = FALSE
  ]
  expect_equal(nrow(diag_row), 1L)
})

test_that("a two-mode rate model preprocesses", {
  # The rate spec is sender-indexed but the focal `attend` network is 3x2.
  # Dropping the receiver side collapsed n2 to n1, so outdeg's rowSums ran with
  # the wrong second dimension and aborted with `'x' is too short` on the first
  # non-empty network.
  expect_no_error(estimate_dynam(
    attend ~ 1 + outdeg(attend),
    sub_model = "rate",
    data = as_goldfish(make_stocnet_fixture_multipartite()),
    preprocessing_only = TRUE
  ))
})

test_that("a receiver-side change reaches the effect as a local update", {
  # The value must arrive against the receiver's own index space: the update
  # encodes alter 1 (E1), not global node 4.
  prep <- estimate_dynam(
    attend ~ alter(size),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_multipartite()),
    preprocessing_only = TRUE
  )
  broadcast <- prep$stat_mat_broadcast
  expect_equal(ncol(broadcast), 1L)
  expect_equal(
    broadcast[2, 1] + 1L,
    1,
    label = "the fixed index is alter 1, E1's local id"
  )
  expect_equal(broadcast[4, 1], 99, label = "the replaced size")
})

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

test_that("non-active changes and global rows split per variable", {
  x <- make_stocnet_fixture()
  x$changes <- data.frame(time = c(1, 2), node = c(1L, 3L), var = "floor")
  x$changes$value <- list(list(3), list(4))
  x$global <- data.frame(time = 1, var = "season")
  x$global$value <- list(list(2))
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map)

  expect_named(streams$attribute, "floor")
  expect_equal(streams$attribute$floor$node, c(1L, 3L))
  expect_equal(unlist(streams$attribute$floor$value), c(3, 4))
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

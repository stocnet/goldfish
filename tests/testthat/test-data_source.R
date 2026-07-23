# The resolution seam is what lets the builders serve both the legacy
# environment and the single data object. Its contract is that both sources
# answer equivalently for equivalent data — verified here directly, before the
# constructors flip.

make_equivalent_inputs <- function() {
  nodes_df <- data.frame(
    label = c("A", "B", "C"),
    floor = c(1, 2, 1),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    time = c(1, 2, 3),
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    increment = c(1, 1, 1),
    stringsAsFactors = FALSE
  )
  stocnet <- list(
    info = list(
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = nodes_df,
    ties = data.frame(
      from = c(1L, 2L, 3L),
      to = c(2L, 3L, 1L),
      time = c(1, 2, 3),
      layer = "calls",
      stringsAsFactors = FALSE
    )
  )
  list(nodes_df = nodes_df, events = events, stocnet = stocnet)
}

# The legacy environment as preprocessing sees it: the network object holds the
# matrix it was constructed with and its linked events are still unfolded, since
# the walk replays them. `matrix` seeds that starting state.
legacy_env_with_network <- function(inputs, matrix = NULL) {
  # link_events() captures its event argument by name, so it must be a symbol.
  events <- inputs$events
  actors <- make_nodes(inputs$nodes_df)
  calls <- if (is.null(matrix)) {
    make_network(nodes = actors, directed = TRUE)
  } else {
    make_network(matrix, nodes = actors, directed = TRUE)
  }
  calls <- link_events(calls, events, nodes = actors)
  out <- new.env()
  out$actors <- actors
  out$calls <- calls
  out
}

test_that("both sources start from an empty matrix when there is no history", {
  # ds_network() answers the state the walk STARTS from. The walk replays every
  # timed event, so timed ties must not be folded in here -- they would count
  # twice.
  inputs <- make_equivalent_inputs()

  legacy <- new_data_source(envir = legacy_env_with_network(inputs))
  stocnet <- new_data_source(data = inputs$stocnet)

  expect_equal(
    ds_network(stocnet, "calls"),
    ds_network(legacy, "calls"),
    ignore_attr = "dimnames"
  )
  expect_equal(
    unname(ds_network(stocnet, "calls")),
    matrix(0, 3, 3),
    label = "the three timed calls belong to the schedule, not the state"
  )
})

test_that("both sources start from the same history matrix", {
  # A legacy constructor's starting matrix is what stocnet spells as `time = NA`
  # history rows, so the two must produce the same initial state.
  inputs <- make_equivalent_inputs()
  history <- matrix(c(0, 1, 0, 0, 0, 0, 1, 0, 0), 3, 3)

  legacy <- new_data_source(envir = legacy_env_with_network(inputs, history))
  with_history <- inputs$stocnet
  with_history$ties <- rbind(
    data.frame(
      from = c(2L, 1L),
      to = c(1L, 3L),
      time = NA_real_,
      layer = "calls",
      stringsAsFactors = FALSE
    ),
    with_history$ties
  )
  stocnet <- new_data_source(data = with_history)

  expect_equal(
    ds_network(stocnet, "calls"),
    ds_network(legacy, "calls"),
    ignore_attr = "dimnames"
  )
  expect_equal(unname(ds_network(stocnet, "calls")), history)
})

test_that("both sources agree on direction", {
  inputs <- make_equivalent_inputs()
  actors <- make_nodes(inputs$nodes_df)
  calls <- make_network(nodes = actors, directed = TRUE)
  friends <- make_network(nodes = actors, directed = FALSE)
  legacy <- new_data_source(envir = environment())

  expect_true(ds_is_directed(legacy, "calls"))
  expect_false(ds_is_directed(legacy, "friends"))

  expect_true(ds_is_directed(new_data_source(data = inputs$stocnet), "calls"))
  undirected <- inputs$stocnet
  undirected$info$directed <- c(calls = FALSE)
  expect_false(ds_is_directed(new_data_source(data = undirected), "calls"))
})

test_that("both sources resolve node counts and a nodal attribute alike", {
  inputs <- make_equivalent_inputs()
  actors <- make_nodes(inputs$nodes_df)
  legacy <- new_data_source(envir = environment())
  stocnet <- new_data_source(data = inputs$stocnet)

  expect_equal(ds_n_nodes(legacy, "actors"), ds_n_nodes(stocnet, "nodes"))
  expect_equal(
    ds_attribute(legacy, "actors", "floor"),
    ds_attribute(stocnet, "nodes", "floor")
  )
  expect_equal(ds_attribute(stocnet, "nodes", "floor"), c(1, 2, 1))
})

test_that("a two-mode focal layer names two sides; one-mode names one", {
  one_mode <- new_data_source(data = make_stocnet_fixture())
  expect_equal(
    ds_side_names(one_mode),
    c("nodes", "nodes"),
    label = "identical names keep identical(nodes, nodes2) deciding one-mode"
  )

  two_mode <- new_data_source(data = make_stocnet_fixture_twomode())
  expect_equal(ds_side_names(two_mode), c("nodes_side1", "nodes_side2"))
  expect_equal(ds_n_nodes(two_mode, "nodes_side1"), 2)
  expect_equal(ds_n_nodes(two_mode, "nodes_side2"), 2)
})

test_that("the stocnet path reads two-modeness only from the mode map", {
  # The canonical representation contract: no downstream branch decides
  # two-modeness by comparing two node-set names. The stocnet method ignores
  # both name arguments entirely, so even deliberately misleading ones cannot
  # override the focal layer's mode pair.
  two_mode <- new_data_source(data = make_stocnet_fixture_twomode())
  expect_true(ds_model_is_two_mode(two_mode))
  expect_true(
    ds_model_is_two_mode(two_mode, nodes = "same", nodes2 = "same"),
    label = "identical names cannot make a two-mode layer read one-mode"
  )

  one_mode <- new_data_source(data = make_stocnet_fixture())
  expect_false(ds_model_is_two_mode(one_mode))
  expect_false(
    ds_model_is_two_mode(one_mode, nodes = "a", nodes2 = "b"),
    label = "distinct names cannot make a one-mode layer read two-mode"
  )
})

test_that("per-argument two-modeness comes from each layer's own mode pair", {
  # Resolution is per network argument, never a blanket from the focal layer:
  # on a multipartite object the focal `attend` is two-mode while the covariate
  # `coauthor` is one-mode, and each argument answers for itself.
  src <- new_data_source(data = make_stocnet_fixture_multipartite())

  expect_true(ds_arg_is_two_mode(src, "attend", NULL))
  expect_false(ds_arg_is_two_mode(src, "coauthor", NULL))
  expect_true(ds_arg_is_two_mode(src, "member", NULL))
})

test_that("a two-mode layer's state is n1 x n2 with per-side labels", {
  src <- new_data_source(data = make_stocnet_fixture_twomode())
  mat <- ds_network(src, "membership")

  expect_equal(dim(mat), c(2L, 2L))
  expect_equal(dimnames(mat), list(c("A", "B"), c("X", "Y")))
})

test_that("attributes slice to the node set's own rows on a two-mode layer", {
  x <- make_stocnet_fixture_twomode()
  x$nodes$size <- c(10, 20, 30, 40)
  src <- new_data_source(data = x)

  expect_equal(
    ds_attribute(src, "nodes_side1", "size"),
    c(10, 20),
    label = "the sender side reads its own slice of the single nodes tibble"
  )
  expect_equal(ds_attribute(src, "nodes_side2", "size"), c(30, 40))
})

test_that("the stocnet source builds the legacy state container", {
  inputs <- make_equivalent_inputs()

  legacy_state <- build_state_container(
    "calls",
    nodes = "actors",
    envir = legacy_env_with_network(inputs)
  )
  stocnet_state <- build_state_container(
    "calls",
    nodes = "nodes",
    data = inputs$stocnet
  )

  expect_equal(
    stocnet_state$networks$calls,
    legacy_state$networks$calls,
    ignore_attr = "dimnames"
  )
  expect_equal(
    attr(stocnet_state, "object_keys"),
    attr(legacy_state, "object_keys")
  )
  # A one-mode layer names one node space, so both modeled sides resolve to a
  # single view rather than to two buckets one of which is empty.
  expect_length(grep("^nodal:", names(stocnet_state)), 1L)
})

# Event streams ---------------------------------------------------------------

test_that("a layer's translated stream equals the legacy sanitized events", {
  inputs <- make_equivalent_inputs()
  events <- inputs$events
  actors <- make_nodes(inputs$nodes_df)
  legacy <- sanitizeEvents(events, actors, envir = environment())

  src <- new_data_source(data = inputs$stocnet)
  expect_equal(
    ds_fetch_stream(src, "calls"),
    legacy,
    ignore_attr = "row.names",
    label = "same columns, same local indices, same numeric time"
  )
})

test_that("the value column is named for the layer's update semantics", {
  inputs <- make_equivalent_inputs()
  expect_named(
    ds_fetch_stream(new_data_source(data = inputs$stocnet), "calls"),
    c("time", "sender", "receiver", "increment")
  )

  replace_layer <- inputs$stocnet
  replace_layer$info$update <- c(calls = "replace")
  expect_named(
    ds_fetch_stream(new_data_source(data = replace_layer), "calls"),
    c("time", "sender", "receiver", "replace")
  )
})

test_that("NA-time history stays out of the event stream", {
  src <- new_data_source(data = make_stocnet_fixture())
  stream <- ds_fetch_stream(src, "calls")

  expect_equal(
    stream$time,
    c(1, 2),
    label = "the history row initializes state instead of being scheduled"
  )
  expect_equal(stream$sender, c(2L, 3L))
})

test_that("an attribute stream translates to the node/replace shape", {
  x <- make_stocnet_fixture()
  x$changes <- data.frame(time = c(2, 1), node = c(3L, 1L), var = "floor")
  x$changes$value <- list(list(8), list(7))
  src <- new_data_source(data = x)

  # Attribute streams are keyed by the view they write into, so the key names
  # the node space as well as the variable.
  expect_equal(
    ds_fetch_stream(src, "nodal:p$floor"),
    data.frame(time = c(1, 2), node = c(1L, 3L), replace = c(7, 8)),
    ignore_attr = "row.names",
    label = "rows are ordered by the sort key regardless of input arrangement"
  )
})

test_that("stream keys are reported only for objects that carry events", {
  inputs <- make_equivalent_inputs()
  src <- new_data_source(data = inputs$stocnet)
  expect_equal(ds_object_streams(src, "calls"), "calls")

  history_only <- inputs$stocnet
  history_only$ties$time <- NA_real_
  expect_length(
    ds_object_streams(new_data_source(data = history_only), "calls"),
    0
  )
})

# Mode-ness -------------------------------------------------------------------

test_that("both sources agree on whether a layer spans two modes", {
  inputs <- make_equivalent_inputs()
  actors <- make_nodes(inputs$nodes_df)
  calls <- make_network(nodes = actors, directed = TRUE)
  legacy <- new_data_source(envir = environment())

  expect_false(ds_layer_is_two_mode(legacy, "calls"))
  expect_false(
    ds_layer_is_two_mode(new_data_source(data = inputs$stocnet), "calls")
  )
  expect_true(
    ds_layer_is_two_mode(
      new_data_source(data = make_stocnet_fixture_twomode()),
      "membership"
    )
  )
})

test_that("model mode-ness comes from the map, not from node-set names", {
  # The legacy source has no map and must compare the names it is given.
  legacy <- new_data_source(envir = new.env())
  expect_false(ds_model_is_two_mode(legacy, "actors", "actors"))
  expect_true(ds_model_is_two_mode(legacy, "actors", "orgs"))

  # The stocnet source ignores the names entirely: the focal layer decided it
  # once, when the map was built.
  two_mode <- new_data_source(data = make_stocnet_fixture_twomode())
  expect_true(ds_model_is_two_mode(two_mode, "nodes", "nodes"))
  expect_false(
    ds_model_is_two_mode(new_data_source(data = make_stocnet_fixture()))
  )
})

test_that("a per-layer declaration makes mode-ness layer-specific", {
  x <- make_stocnet_fixture_multimode()
  x$ties <- rbind(
    x$ties,
    data.frame(from = 1L, to = 3L, time = 3, layer = "report")
  )
  x$info$update <- c(advice = "increment", report = "increment")
  x$info$directed <- c(advice = TRUE, report = TRUE)
  x$info$observation <- c(advice = "event", report = "event")
  x$info$sender <- list(
    advice = c("employee", "supervisor"),
    report = "employee"
  )
  x$info$receiver <- list(
    advice = c("employee", "supervisor"),
    report = "supervisor"
  )
  src <- new_data_source(data = x, focal = "advice")

  expect_false(ds_layer_is_two_mode(src, "advice"))
  expect_true(ds_layer_is_two_mode(src, "report"))
  expect_false(
    ds_model_is_two_mode(src),
    label = "the focal layer decides the model, not the other layers"
  )
  expect_equal(dim(ds_network(src, "report")), c(2L, 1L))
})

test_that("a window on a panel-layer effect aborts before preprocessing", {
  x <- make_stocnet_fixture()
  # Add an exogenous panel layer whose effect the model would window.
  x$ties <- rbind(
    x$ties,
    data.frame(from = 1L, to = 2L, time = 1, layer = "friendship")
  )
  x$info$update <- c(calls = "increment", friendship = "replace")
  x$info$directed <- c(calls = TRUE, friendship = TRUE)
  x$info$observation <- c(calls = "event", friendship = "panel")
  src <- new_data_source(data = x, focal = "calls")

  windowed <- list(list(
    kind = "window",
    source = "friendship",
    derived_name = "friendship_86400",
    params = list(window = 86400)
  ))
  expect_error(
    ds_realize_derivations(src, windowed),
    "panel layer"
  )

  # An event-layer window still realizes without error.
  event_window <- list(list(
    kind = "window",
    source = "calls",
    derived_name = "calls_86400",
    params = list(window = 86400)
  ))
  expect_no_error(ds_realize_derivations(src, event_window))
})

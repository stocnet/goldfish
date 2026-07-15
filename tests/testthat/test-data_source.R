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

# The legacy network object holds its initial matrix and folds linked events
# only in as.matrix(); the stocnet source materializes the layer over a window.
# The comparable pair is therefore the folded legacy matrix against an
# all-covering stocnet window.
legacy_env_with_folded_network <- function(inputs, time = Inf) {
  # link_events() captures its event argument by name, so it must be a symbol.
  events <- inputs$events
  actors <- make_nodes(inputs$nodes_df)
  calls <- make_network(nodes = actors, directed = TRUE)
  calls <- link_events(calls, events, nodes = actors)
  env <- environment()
  out <- new.env()
  out$actors <- actors
  out$calls <- as.matrix(calls, time = time, envir = env)
  out
}

test_that("both sources fold the same ties into the same matrix", {
  inputs <- make_equivalent_inputs()

  legacy <- new_data_source(envir = legacy_env_with_folded_network(inputs))
  stocnet <- new_data_source(data = inputs$stocnet)

  expect_equal(
    ds_network(stocnet, "calls", time = Inf),
    ds_network(legacy, "calls"),
    ignore_attr = "dimnames"
  )
  expect_equal(
    unname(ds_network(stocnet, "calls", time = Inf)),
    matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), 3, 3),
    label = "A->B, B->C, C->A each folded once"
  )
})

test_that("both sources window the state to the same partial fold", {
  inputs <- make_equivalent_inputs()

  legacy <- new_data_source(
    envir = legacy_env_with_folded_network(inputs, time = 3)
  )
  stocnet <- new_data_source(data = inputs$stocnet)

  expect_equal(
    ds_network(stocnet, "calls", time = 3),
    ds_network(legacy, "calls"),
    ignore_attr = "dimnames",
    label = "the tie at t=3 is excluded from both under a half-open window"
  )
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

test_that("a two-mode layer's state is n1 x n2 with per-side labels", {
  src <- new_data_source(data = make_stocnet_fixture_twomode())
  mat <- ds_network(src, "membership", time = 3)

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
    envir = legacy_env_with_folded_network(inputs)
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
  expect_null(stocnet_state$nodal2, label = "a one-mode layer has no nodal2")
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

  expect_equal(
    ds_fetch_stream(src, "floor"),
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
  expect_equal(dim(ds_network(src, "report", time = 4)), c(2L, 1L))
})

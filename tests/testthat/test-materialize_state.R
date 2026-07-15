# The materializer generalizes the vectorized update engine (methods_update.R)
# over component streams and a [start_time, time) window. The equivalence tests
# below are the contract: it must reproduce the legacy per-event startTime fold
# exactly on the shipped datasets.

# History and windowing -------------------------------------------------------

test_that("NA-time history folds into the state whatever the window", {
  stream <- data.frame(
    time = c(NA, 5, 20),
    from = c(1L, 1L, 2L),
    to = c(2L, 2L, 3L),
    value = c(3, 1, 1),
    update = "increment"
  )

  expect_equal(
    materialize_network_state(stream, 3, 3, start_time = 10, time = 15)[1, 2],
    3,
    label = "history is pre-observation state, never filtered by start_time"
  )
  expect_equal(
    materialize_network_state(stream, 3, 3, time = 10)[1, 2],
    4,
    label = "the timed event at t=5 adds to the history value"
  )
  expect_equal(
    materialize_network_state(stream, 3, 3, time = 20)[2, 3],
    0,
    label = "the window is half-open: t=20 is not yet applied at time=20"
  )
})

test_that("replace layers take the last value per cell, history first", {
  stream <- data.frame(
    time = c(NA, 3, 8),
    from = c(1L, 1L, 1L),
    to = c(2L, 2L, 2L),
    value = c(9, 1, 2),
    update = "replace"
  )

  expect_equal(materialize_network_state(stream, 2, 2, time = 2)[1, 2], 9)
  expect_equal(materialize_network_state(stream, 2, 2, time = 5)[1, 2], 1)
  expect_equal(materialize_network_state(stream, 2, 2, time = 20)[1, 2], 2)
})

test_that("undirected layers mirror across the diagonal", {
  stream <- data.frame(
    time = c(1, 2),
    from = c(1L, 1L),
    to = c(2L, 2L),
    value = c(1, 1),
    update = "increment"
  )
  mat <- materialize_network_state(stream, 2, 2, directed = FALSE, time = 10)
  expect_equal(mat[1, 2], 2)
  expect_equal(mat[2, 1], 2)
})

test_that("attribute states fold replaces and increments over the window", {
  stream <- data.frame(time = c(2, 4, 9), node = c(1L, 2L, 1L))
  stream$value <- list(1, 2, 3)

  expect_equal(
    materialize_attribute_state(stream, c(0, 0), time = 20),
    c(3, 2),
    label = "last replace per node wins"
  )
  expect_equal(
    materialize_attribute_state(stream, c(0, 0), start_time = 3, time = 20),
    c(3, 2),
    label = "the event at t=2 is outside [3, 20)"
  )
  expect_equal(
    materialize_attribute_state(
      stream,
      c(0, 0),
      time = 20,
      update = "increment"
    ),
    c(4, 2),
    label = "increments to node 1 sum to 1+3"
  )
})

test_that("an empty stream or window returns the initial state", {
  stream <- data.frame(
    time = 5,
    from = 1L,
    to = 2L,
    value = 1,
    update = "increment"
  )
  expect_equal(
    materialize_network_state(stream, 2, 2, time = 1),
    matrix(0, 2, 2)
  )
  expect_equal(materialize_network_state(NULL, 2, 2), matrix(0, 2, 2))
  expect_equal(materialize_attribute_state(NULL, c(1, 2)), c(1, 2))
})

test_that("the observation window defaults to the focal dependent span", {
  x <- make_stocnet_fixture()
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(x, map)

  expect_equal(
    resolve_observation_window(streams),
    list(start_time = 1, end_time = 2),
    label = "the NA-time history row is not part of the span"
  )
  expect_equal(
    resolve_observation_window(streams, start_time = 0),
    list(start_time = 0, end_time = 2),
    label = "an explicit argument overrides the default per side"
  )
  expect_equal(
    resolve_observation_window(streams, start_time = 0, end_time = 10),
    list(start_time = 0, end_time = 10)
  )
})

# Equivalence with the legacy fold --------------------------------------------

test_that("materializer equals the legacy fold on Social_Evolution calls", {
  skip_on_cran()
  data("Social_Evolution", package = "goldfish")

  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  mid <- median(calls$time)
  legacy <- as.matrix(call_network, time = mid, envir = environment())

  stream <- data.frame(
    time = calls$time,
    from = match(calls$sender, actors$label),
    to = match(calls$receiver, actors$label),
    value = calls$increment,
    update = "increment"
  )
  new <- materialize_network_state(
    stream,
    n1 = nrow(actors),
    n2 = nrow(actors),
    directed = TRUE,
    time = mid
  )

  expect_identical(new, unname(legacy))
})

test_that("materializer equals the legacy fold on Social_Evolution floor", {
  skip_on_cran()
  data("Social_Evolution", package = "goldfish")

  floor_changes <- data.frame(
    node = actors$label[c(1, 2, 1)],
    time = c(1, 2, 3),
    replace = c(7, 8, 9)
  )
  actors_gf <- make_nodes(actors)
  actors_gf <- link_events(actors_gf, floor_changes, attribute = "floor")
  legacy <- as.data.frame(actors_gf, time = 10, envir = environment())

  stream <- data.frame(
    time = floor_changes$time,
    node = match(floor_changes$node, actors$label)
  )
  stream$value <- as.list(floor_changes$replace)
  new <- materialize_attribute_state(stream, actors$floor, time = 10)

  expect_identical(new, legacy$floor)
})

test_that("materializer equals the legacy fold on Fisheries bilatnet", {
  skip_on_cran()
  data("Fisheries_Treaties_6070", package = "goldfish")

  bilat_gf <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilat_gf <- link_events(bilat_gf, bilatchanges, nodes = states)
  cutoff <- as.numeric(as.POSIXct("1965-12-31"))
  legacy <- as.matrix(bilat_gf, time = cutoff, envir = environment())

  # The initial matrix is the layer's pre-observation history: as an undirected
  # layer only one triangle is carried, the mirror being reconstructed.
  hist_cells <- which(upper.tri(bilatnet) & bilatnet != 0, arr.ind = TRUE)
  stream <- rbind(
    data.frame(
      time = NA_real_,
      from = hist_cells[, "row"],
      to = hist_cells[, "col"],
      value = bilatnet[hist_cells],
      update = "increment"
    ),
    data.frame(
      time = as.numeric(bilatchanges$time),
      from = match(bilatchanges$sender, states$label),
      to = match(bilatchanges$receiver, states$label),
      value = bilatchanges$increment,
      update = "increment"
    )
  )
  new <- materialize_network_state(
    stream,
    n1 = nrow(states),
    n2 = nrow(states),
    directed = FALSE,
    time = cutoff
  )

  expect_identical(new, unname(legacy))
})

test_that("materializer equals the legacy fold on Fisheries gdp", {
  skip_on_cran()
  data("Fisheries_Treaties_6070", package = "goldfish")

  states_gf <- make_nodes(states)
  states_gf <- link_events(states_gf, gdpchanges, attribute = "gdp")
  cutoff <- as.numeric(as.POSIXct("1965-12-31"))
  legacy <- as.data.frame(states_gf, time = cutoff, envir = environment())

  stream <- data.frame(
    time = as.numeric(gdpchanges$time),
    node = match(gdpchanges$node, states$label)
  )
  stream$value <- as.list(gdpchanges$replace)
  new <- materialize_attribute_state(stream, states$gdp, time = cutoff)

  expect_identical(new, legacy$gdp)
})

test_that("network_state_at returns the known matrix at each time point", {
  x <- make_stocnet_fixture()
  labels <- c("A", "B", "C")
  # calls: history A->B (NA), then B->C at t=1, C->A at t=2; increment layer.
  expected <- function(cells) {
    mat <- matrix(0, 3, 3, dimnames = list(labels, labels))
    for (cell in cells) {
      mat[cell[1], cell[2]] <- 1
    }
    mat
  }

  expect_equal(
    network_state_at(x, "calls", time = 1),
    expected(list(c("A", "B"))),
    label = "only the NA-time history row precedes t=1"
  )
  expect_equal(
    network_state_at(x, "calls", time = 2),
    expected(list(c("A", "B"), c("B", "C")))
  )
  expect_equal(
    network_state_at(x, "calls", time = 3),
    expected(list(c("A", "B"), c("B", "C"), c("C", "A")))
  )
})

test_that("network_state_at honors start_time and keeps history", {
  x <- make_stocnet_fixture()
  mat <- network_state_at(x, "calls", time = 3, start_time = 2)

  expect_equal(mat["A", "B"], 1, label = "history is never windowed out")
  expect_equal(mat["B", "C"], 0, label = "the t=1 event is before start_time")
  expect_equal(mat["C", "A"], 1)
})

test_that("network_state_at accepts a stamped object and calendar times", {
  x <- make_stocnet_fixture()
  x$ties$time <- as.POSIXct(
    c(NA, 1, 2),
    origin = "1970-01-01",
    tz = "GMT"
  )
  stamped <- as_goldfish(x)

  expect_equal(
    network_state_at(stamped, "calls", time = "1970-01-01 00:00:02"),
    network_state_at(x, "calls", time = 2),
    label = "a character timestamp resolves on the same axis as the ties"
  )
})

test_that("network_state_at on a two-mode layer is n1 x n2 with side labels", {
  x <- make_stocnet_fixture_twomode()
  mat <- network_state_at(x, "membership", time = 3)

  expect_equal(dim(mat), c(2L, 2L))
  expect_equal(dimnames(mat), list(c("A", "B"), c("X", "Y")))
  expect_equal(mat["A", "X"], 1)
  expect_equal(mat["B", "Y"], 1)
  expect_equal(mat["A", "Y"], 0)
})

test_that("network_state_at rejects an unknown layer, listing candidates", {
  x <- make_stocnet_fixture()
  expect_error(network_state_at(x, "sms", time = 1), "not among")
})

test_that("nodes_state_at updates dynamic attributes to the time point", {
  x <- make_stocnet_fixture()
  x$changes <- data.frame(
    time = c(1, 2, 3),
    node = c(1L, 3L, 1L),
    var = "floor"
  )
  x$changes$value <- list(list(7), list(8), list(9))

  expect_equal(
    nodes_state_at(x, time = 1)$floor,
    c(1, 2, 1),
    label = "no change precedes t=1, so the nodes column stands"
  )
  expect_equal(nodes_state_at(x, time = 3)$floor, c(7, 2, 8))
  expect_equal(
    nodes_state_at(x, time = 4)$floor,
    c(9, 2, 8),
    label = "the last replace per node wins"
  )
  expect_equal(
    nodes_state_at(x, time = 4, start_time = 2)$floor,
    c(9, 2, 8),
    label = "the t=1 change is outside the window but node 1 is replaced later"
  )
})

test_that("nodes_state_at returns nodes unchanged without changes rows", {
  x <- make_stocnet_fixture()
  expect_equal(nodes_state_at(x, time = 10), x$nodes)
})

test_that("state-at-t helpers agree with the legacy engine on Fisheries", {
  skip_on_cran()
  data("Fisheries_Treaties_6070", package = "goldfish")
  cutoff <- as.numeric(as.POSIXct("1965-12-31"))

  states_gf <- make_nodes(states)
  states_gf <- link_events(states_gf, gdpchanges, attribute = "gdp")
  legacy <- as.data.frame(states_gf, time = cutoff, envir = environment())

  changes <- data.frame(
    time = as.numeric(gdpchanges$time),
    node = match(gdpchanges$node, states$label),
    var = "gdp"
  )
  changes$value <- as.list(gdpchanges$replace)
  x <- list(
    info = list(
      focal = "treaties",
      update = c(treaties = "increment"),
      directed = c(treaties = FALSE),
      observation = c(treaties = "event")
    ),
    nodes = states[c("label", "gdp")],
    ties = data.frame(
      from = match(bilatchanges$sender, states$label),
      to = match(bilatchanges$receiver, states$label),
      time = as.numeric(bilatchanges$time),
      layer = "treaties",
      weight = bilatchanges$increment
    ),
    changes = changes
  )

  expect_identical(nodes_state_at(x, time = cutoff)$gdp, legacy$gdp)

  bilat_gf <- make_network(
    matrix(
      0,
      nrow(states),
      nrow(states),
      dimnames = list(
        states$label,
        states$label
      )
    ),
    nodes = states,
    directed = FALSE
  )
  bilat_gf <- link_events(bilat_gf, bilatchanges, nodes = states)
  legacy_net <- as.matrix(bilat_gf, time = cutoff, envir = environment())

  expect_identical(
    network_state_at(x, "treaties", time = cutoff),
    legacy_net
  )
})

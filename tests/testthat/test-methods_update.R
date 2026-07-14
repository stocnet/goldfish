# Helpers --------------------------------------------------------------------

make_test_nodes <- function(n = 3, score = 0) {
  make_nodes(data.frame(
    label = sprintf("Actor %d", seq_len(n)),
    score = score,
    stringsAsFactors = FALSE
  ))
}

make_test_network <- function(n = 3, directed = TRUE) {
  labels <- sprintf("Actor %d", seq_len(n))
  nodes <- make_test_nodes(n)
  make_network(
    matrix(0, n, n, dimnames = list(labels, labels)),
    nodes = nodes,
    directed = directed
  )
}

# as.data.frame.nodes.goldfish -----------------------------------------------

test_that("as.data.frame: replace applies last event per node within window", {
  nodes <- make_test_nodes()
  ev <- data.frame(
    node = c("Actor 1", "Actor 2", "Actor 1"),
    time = c(5, 8, 12),
    replace = c(1.0, 2.0, 3.0)
  )
  nodes <- link_events(nodes, ev, attribute = "score")

  result <- as.data.frame(nodes, time = 20, envir = environment())
  expect_equal(
    result$score,
    c(3.0, 2.0, 0.0),
    label = "last replace per node wins"
  )

  result_window <- as.data.frame(nodes, time = 10, envir = environment())
  expect_equal(
    result_window$score,
    c(1.0, 2.0, 0.0),
    label = "event at t=12 excluded when time=10"
  )
})

test_that("as.data.frame: increment — multiple events to the same node are summed", {
  nodes <- make_test_nodes(2)
  ev <- data.frame(
    node = c("Actor 1", "Actor 1", "Actor 2", "Actor 1"),
    time = c(2, 5, 6, 9),
    increment = c(1.0, 2.0, 3.0, 4.0)
  )
  nodes <- link_events(nodes, ev, attribute = "score")

  result <- as.data.frame(nodes, time = 20, envir = environment())
  expect_equal(
    result$score,
    c(7.0, 3.0),
    label = "increments to Actor 1 sum to 1+2+4=7"
  )
})

test_that("as.data.frame: startTime excludes events before the window", {
  nodes <- make_test_nodes(1)
  ev <- data.frame(
    node = c("Actor 1", "Actor 1"),
    time = c(3, 10),
    increment = c(5.0, 2.0)
  )
  nodes <- link_events(nodes, ev, attribute = "score")

  result <- as.data.frame(
    nodes,
    time = 20,
    startTime = 5,
    envir = environment()
  )
  expect_equal(
    result$score,
    2.0,
    label = "only event at t=10 is in [startTime=5, time=20)"
  )
})

test_that("as.data.frame: no linked events returns object unchanged", {
  nodes <- make_test_nodes(2, score = c(1.5, 2.5))
  result <- as.data.frame(nodes, time = 100, envir = environment())
  expect_equal(result$score, c(1.5, 2.5))
})

test_that("as.data.frame: mixed replace+increment is caught by link_events", {
  nodes <- make_test_nodes(1)
  ev_mixed <- data.frame(
    node = "Actor 1",
    time = 1,
    replace = 1.0,
    increment = 1.0
  )
  expect_error(
    link_events(nodes, ev_mixed, attribute = "score"),
    regexp = "Incompatible columns",
    label = "link_events rejects mixed replace+increment"
  )
})


# as.matrix.network.goldfish -------------------------------------------------

test_that("as.matrix: replace — last event per dyad wins within window", {
  net <- make_test_network(3)
  ev <- data.frame(
    time = c(2, 5, 8),
    sender = c("Actor 1", "Actor 1", "Actor 1"),
    receiver = c("Actor 2", "Actor 3", "Actor 2"),
    replace = c(1.0, 2.0, 3.0)
  )
  nodes <- make_test_nodes(3)
  net <- link_events(net, ev, nodes = nodes)

  result <- as.matrix(net, time = 10, envir = environment())
  expect_equal(
    result["Actor 1", "Actor 2"],
    3.0,
    label = "last replace to (1,2) is 3"
  )
  expect_equal(result["Actor 1", "Actor 3"], 2.0)

  result_early <- as.matrix(net, time = 6, envir = environment())
  expect_equal(
    result_early["Actor 1", "Actor 2"],
    1.0,
    label = "event at t=8 excluded when time=6"
  )
})

test_that("as.matrix: increment — multiple events to the same dyad are summed", {
  net <- make_test_network(3)
  nodes <- make_test_nodes(3)
  ev <- data.frame(
    time = c(1, 3, 5, 7),
    sender = c("Actor 1", "Actor 1", "Actor 2", "Actor 1"),
    receiver = c("Actor 2", "Actor 2", "Actor 3", "Actor 2"),
    increment = c(1.0, 2.0, 3.0, 4.0)
  )
  net <- link_events(net, ev, nodes = nodes)

  result <- as.matrix(net, time = 10, envir = environment())
  expect_equal(
    result["Actor 1", "Actor 2"],
    7.0,
    label = "increments to (1,2) sum to 1+2+4=7"
  )
  expect_equal(result["Actor 2", "Actor 3"], 3.0)
})

test_that("as.matrix: undirected network — increment updates both directions", {
  net <- make_test_network(3, directed = FALSE)
  nodes <- make_test_nodes(3)
  ev <- data.frame(
    time = c(1, 3),
    sender = c("Actor 1", "Actor 1"),
    receiver = c("Actor 2", "Actor 2"),
    increment = c(1.0, 2.0)
  )
  net <- link_events(net, ev, nodes = nodes)

  result <- as.matrix(net, time = 10, envir = environment())
  expect_equal(result["Actor 1", "Actor 2"], 3.0)
  expect_equal(
    result["Actor 2", "Actor 1"],
    3.0,
    label = "symmetric update for undirected network"
  )
})

test_that("as.matrix: startTime excludes events before the window", {
  net <- make_test_network(2)
  nodes <- make_test_nodes(2)
  ev <- data.frame(
    time = c(2, 10),
    sender = c("Actor 1", "Actor 1"),
    receiver = c("Actor 2", "Actor 2"),
    increment = c(5.0, 1.0)
  )
  net <- link_events(net, ev, nodes = nodes)

  result <- as.matrix(net, time = 20, startTime = 5, envir = environment())
  expect_equal(
    result["Actor 1", "Actor 2"],
    1.0,
    label = "only event at t=10 is in [startTime=5, time=20)"
  )
})

test_that("as.matrix: no linked events returns initial matrix unchanged", {
  labels <- c("Actor 1", "Actor 2")
  m0 <- matrix(c(0, 1, 0, 0), 2, 2, dimnames = list(labels, labels))
  nodes <- make_test_nodes(2)
  net <- make_network(m0, nodes = nodes, directed = TRUE)

  result <- as.matrix(net, time = 100, envir = environment())
  expect_equal(result, m0, ignore_attr = TRUE)
})

test_that("as.matrix: mixed replace+increment is caught by link_events", {
  net <- make_test_network(2)
  nodes <- make_test_nodes(2)
  ev_mixed <- data.frame(
    time = 1,
    sender = "Actor 1",
    receiver = "Actor 2",
    replace = 1.0,
    increment = 1.0
  )
  expect_error(
    link_events(net, ev_mixed, nodes = nodes),
    regexp = "Incompatible columns",
    label = "link_events rejects mixed replace+increment"
  )
})

test_that("as.matrix: Social_Evolution increment equals row-by-row reference", {
  skip_on_cran()
  data("Social_Evolution")
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  mid <- median(calls$time)

  result_new <- as.matrix(call_network, time = mid, envir = environment())

  net_ref <- call_network[seq_len(nrow(actors)), seq_len(nrow(actors))]
  calls_san <- sanitizeEvents(calls, actors, envir = environment())
  df <- calls_san[calls_san$time < mid, ]
  for (t in sort(unique(df$time))) {
    r <- df[df$time == t, ]
    net_ref[cbind(r$sender, r$receiver)] <-
      r$increment + net_ref[cbind(r$sender, r$receiver)]
  }

  expect_identical(
    result_new,
    net_ref,
    label = "vectorised tapply result equals row-by-row loop on Social_Evolution"
  )
})

# Hand-built stocnet fixtures for the validator/boundary tests.
#
# These are plain lists of data.frames assembled without any manynet call, so
# the boundary tests stay independent of manynet's release cadence and exercise
# the "reads structure, not class provenance" contract. from/to/node use integer
# indices into `nodes`, matching what manynet::make_stocnet() produces.

# One-mode, single event layer with a NA-time history row.
make_stocnet_fixture <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C"),
    floor = c(1, 2, 1),
    mode = c("p", "p", "p"),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 1L),
    time = c(NA, 1, 2),
    layer = "calls",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# Two-mode fixture: disjoint sender/receiver mode sets plus an `active`
# composition change (list-column value).
make_stocnet_fixture_twomode <- function() {
  nodes <- data.frame(
    label = c("A", "B", "X", "Y"),
    mode = c("p", "p", "o", "o"),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L),
    to = c(3L, 4L),
    time = c(1, 2),
    layer = "membership",
    stringsAsFactors = FALSE
  )
  changes <- data.frame(time = c(1, 2), node = c(1L, 2L), var = "active")
  changes$value <- list(list(FALSE), list(TRUE))
  info <- list(
    name = "toy2",
    focal = "membership",
    update = c(membership = "replace"),
    directed = c(membership = TRUE),
    observation = c(membership = "event"),
    sender = "p",
    receiver = "o"
  )
  list(info = info, nodes = nodes, ties = ties, changes = changes)
}

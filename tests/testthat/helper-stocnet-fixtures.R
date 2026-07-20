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

# Multimodal fixture: three modes, one layer. Left undeclared it spans every
# node (one-mode over all); declaring identical sets restricts it to a subset.
make_stocnet_fixture_multimode <- function() {
  nodes <- data.frame(
    label = c("E1", "E2", "S1", "O1"),
    mode = c("employee", "employee", "supervisor", "outsider"),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 3L),
    to = c(3L, 2L),
    time = c(1, 2),
    layer = "advice",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy3",
    focal = "advice",
    update = c(advice = "increment"),
    directed = c(advice = TRUE),
    observation = c(advice = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# Multipartite fixture: three modes and three layers over *different* mode
# pairs -- a two-mode focal `attend` (actor -> event), a one-mode covariate
# `coauthor` (actor -> actor), and a second two-mode covariate `member`
# (actor -> org). Distinguishes a genuinely multipartite object from the
# two-mode case: each layer resolves its own side pair, so a model over
# `attend` reads `coauthor`/`member` as exogenous covariates mapped through
# their own pairs.
#
# Global ids: 1:3 actors, 4:5 events, 6:7 orgs.
make_stocnet_fixture_multipartite <- function() {
  nodes <- data.frame(
    label = c("A1", "A2", "A3", "E1", "E2", "O1", "O2"),
    mode = c(rep("actor", 3), rep("event", 2), rep("org", 2)),
    size = c(3, 1, 2, 40, 25, 12, 8),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 3L, 1L, 2L, 1L, 3L),
    to = c(4L, 5L, 4L, 2L, 3L, 6L, 7L),
    time = c(1, 2, 3, NA, 1.5, NA, 2.5),
    layer = c(rep("attend", 3), rep("coauthor", 2), rep("member", 2)),
    stringsAsFactors = FALSE
  )
  layer_names <- c("attend", "coauthor", "member")
  info <- list(
    name = "multipartite",
    focal = "attend",
    update = stats::setNames(rep("increment", 3), layer_names),
    directed = stats::setNames(rep(TRUE, 3), layer_names),
    observation = stats::setNames(rep("event", 3), layer_names),
    # Repeated names carry one (layer, mode) entry each: the only per-layer
    # set encoding manynet admits.
    sender = c(attend = "actor", coauthor = "actor", member = "actor"),
    receiver = c(attend = "event", coauthor = "actor", member = "org")
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

# Legacy two node-set fixture: the same membership process expressed through the
# deprecated constructors, which name two distinct `nodes.goldfish` objects
# instead of one nodes tibble with a `mode` column. It is the input side of the
# translation the assembler performs, and the reference for the coefficient
# equivalence between the two construction paths.
#
# Returns the component objects rather than a `make_data()` result so callers
# choose when to assemble (and so the assembly itself stays under test). The
# constructors record node-set *names* by deparsing their arguments, so those
# names are fixed to this function's locals: a caller assembling the bundle must
# bind them back as `actors` and `clubs` for the recorded names to resolve.
make_legacy_fixture_twomode <- function() {
  actors <- data.frame(
    label = c("A1", "A2", "A3", "A4"),
    present = TRUE,
    size = c(3, 1, 2, 4),
    stringsAsFactors = FALSE
  )
  clubs <- data.frame(
    label = c("C1", "C2", "C3"),
    present = TRUE,
    budget = c(12, 8, 20),
    stringsAsFactors = FALSE
  )
  joins <- data.frame(
    time = c(1, 2, 3, 4, 5, 6),
    sender = c("A1", "A2", "A3", "A1", "A4", "A2"),
    receiver = c("C1", "C1", "C2", "C3", "C2", "C3"),
    increment = 1,
    stringsAsFactors = FALSE
  )

  actors <- make_nodes(actors)
  clubs <- make_nodes(clubs)
  membership <- make_network(nodes = actors, nodes2 = clubs, directed = TRUE)
  membership <- link_events(
    x = membership,
    change_events = joins,
    nodes = actors,
    nodes2 = clubs
  )
  joins_dependent <- make_dependent_events(
    events = joins,
    nodes = actors,
    nodes2 = clubs,
    default_network = membership
  )

  list(
    actors = actors,
    clubs = clubs,
    membership = membership,
    joins = joins,
    joins_dependent = joins_dependent
  )
}

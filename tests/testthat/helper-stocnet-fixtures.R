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
#
# `attend` carries a `time = NA` history row so the focal layer's initial
# network is non-empty: a rate model over an empty network returns before it
# ever reduces the matrix, which hides any sender/receiver dimension mix-up.
# `changes` carries a nodal attribute event on a *receiver-side* node (global
# id 4 = E1, side 2), the case where a global node id is used to index a
# side-local attribute slice. Sender-side changes cannot catch that: this
# object's senders are the first mode, so their global and local ids coincide
# (see make_stocnet_fixture_twomode_offset() for the side-1 counterpart).
make_stocnet_fixture_multipartite <- function() {
  nodes <- data.frame(
    label = c("A1", "A2", "A3", "E1", "E2", "O1", "O2"),
    mode = c(rep("actor", 3), rep("event", 2), rep("org", 2)),
    size = c(3, 1, 2, 40, 25, 12, 8),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(2L, 1L, 2L, 3L, 1L, 2L, 1L, 3L),
    to = c(4L, 4L, 5L, 4L, 2L, 3L, 6L, 7L),
    time = c(NA, 1, 2, 3, NA, 1.5, NA, 2.5),
    layer = c(rep("attend", 4), rep("coauthor", 2), rep("member", 2)),
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    time = 1.2,
    node = 4L,
    var = "size",
    stringsAsFactors = FALSE
  )
  changes$value <- list(list(99))
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
  list(info = info, nodes = nodes, ties = ties, changes = changes)
}

# Multipartite fixture whose covariate layer shares the focal *receiver* side
# but not the sender side: focal `attend` (actor -> event) plus `sponsor`
# (org -> event). This is the pair an attribute aggregated over a network's
# senders needs. `tertius(sponsor, size)` conforms -- both layers reach events,
# which is all the effect's index structure requires -- yet the values it
# summarizes belong to orgs, so the attribute must be read on the `org` mode
# and not on the focal sender side. Every other fixture's covariate layer shares
# the focal sender side, where reading the wrong one is indistinguishable.
#
# Global ids: 1:3 actors, 4:5 events, 6:7 orgs. `size` is distinct per mode so a
# slice read on the wrong side is visible in the value, not just in the length.
make_stocnet_fixture_tertius <- function() {
  nodes <- data.frame(
    label = c("A1", "A2", "A3", "E1", "E2", "O1", "O2"),
    mode = c(rep("actor", 3), rep("event", 2), rep("org", 2)),
    size = c(3, 1, 2, 40, 25, 12, 8),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(2L, 1L, 2L, 3L, 6L, 7L, 6L),
    to = c(4L, 4L, 5L, 4L, 4L, 5L, 5L),
    time = c(NA, 1, 2, 3, NA, NA, 1.5),
    layer = c(rep("attend", 4), rep("sponsor", 3)),
    stringsAsFactors = FALSE
  )
  # A `size` change on an org: the org view's own attribute stream, in the org
  # local index space (global 7 = O2 = local 2).
  changes <- data.frame(
    time = 2.2,
    node = 7L,
    var = "size",
    stringsAsFactors = FALSE
  )
  changes$value <- list(list(20))
  layer_names <- c("attend", "sponsor")
  info <- list(
    name = "tertius",
    focal = "attend",
    update = stats::setNames(rep("increment", 2), layer_names),
    directed = stats::setNames(rep(TRUE, 2), layer_names),
    observation = stats::setNames(rep("event", 2), layer_names),
    sender = c(attend = "actor", sponsor = "org"),
    receiver = c(attend = "event", sponsor = "event")
  )
  list(info = info, nodes = nodes, ties = ties, changes = changes)
}

# Two-mode fixture whose focal *sender* side is not `1:n1`: the `worker` mode is
# declared second, so side 1 is global ids 3:5 mapping to local 1:3. The
# multipartite fixture cannot expose a sender-side global/local mix-up because
# its senders are the first mode, making the two id spaces coincide; here they
# differ on both sides, so a stream left in the global space indexes the wrong
# element (or runs off the end) rather than silently agreeing.
#
# Global ids: 1:2 tasks, 3:5 workers. Nodal `skill` changes on each side.
make_stocnet_fixture_twomode_offset <- function() {
  nodes <- data.frame(
    label = c("T1", "T2", "W1", "W2", "W3"),
    mode = c(rep("task", 2), rep("worker", 3)),
    skill = c(0, 0, 5, 3, 8),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(4L, 3L, 4L, 5L),
    to = c(1L, 1L, 2L, 2L),
    time = c(NA, 1, 2, 3),
    layer = "assign",
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    time = c(1.5, 2.5),
    node = c(5L, 2L),
    var = "skill",
    stringsAsFactors = FALSE
  )
  changes$value <- list(list(11), list(7))
  info <- list(
    name = "offset",
    focal = "assign",
    update = c(assign = "increment"),
    directed = c(assign = TRUE),
    observation = c(assign = "event"),
    sender = c(assign = "worker"),
    receiver = c(assign = "task")
  )
  list(info = info, nodes = nodes, ties = ties, changes = changes)
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

# Two-mode fixture with enough repeated dyads and sender spread to actually
# estimate (not just preprocess): four senders `p`, three receivers `o`, twelve
# `membership` events, plus a time-varying nodal covariate on each side -- `x`
# on a sender, `y` on a receiver -- so a rate `ego(x)` and a choice `alter(y)`
# both exercise a mid-stream update on their own mode.
make_stocnet_fixture_twomode_estimable <- function() {
  nodes <- data.frame(
    label = c("P1", "P2", "P3", "P4", "O1", "O2", "O3"),
    mode = c("p", "p", "p", "p", "o", "o", "o"),
    x = c(1, 2, 1, 2, 5, 6, 7),
    y = c(0, 0, 0, 0, 3, 1, 2),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 3L, 2L, 4L, 1L, 3L),
    to = c(5L, 6L, 5L, 6L, 7L, 5L, 5L, 7L, 6L, 7L, 7L, 6L),
    time = 1:12,
    layer = "membership",
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    time = c(4.5, 6.5),
    node = c(2L, 5L),
    var = c("x", "y"),
    stringsAsFactors = FALSE
  )
  changes$value <- list(list(9), list(8))
  info <- list(
    name = "tm_est",
    focal = "membership",
    update = c(membership = "increment"),
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

  # Attribute events on *both* node sets: labels are unique only within a set,
  # so these are what catch a fused-id remap that resolves a reference against
  # the wrong set.
  #
  # Neither set gets `present` events: linking a two-mode network re-reads each
  # side's composition frame with `get()` in an environment whose parent is the
  # namespace, so a `present` frame must be globally visible and cannot be a
  # local of this function. Composition routing is exercised on the stocnet
  # fixtures instead.
  actor_growth <- data.frame(
    time = c(2, 4),
    node = c("A2", "A4"),
    replace = c(5, 6),
    stringsAsFactors = FALSE
  )
  club_funding <- data.frame(
    time = c(3, 5),
    node = c("C3", "C3"),
    replace = c(30, 25),
    stringsAsFactors = FALSE
  )

  actors <- make_nodes(actors)
  actors <- link_events(actors, actor_growth, attribute = "size")
  clubs <- make_nodes(clubs)
  clubs <- link_events(clubs, club_funding, attribute = "budget")
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
    joins_dependent = joins_dependent,
    actor_growth = actor_growth,
    club_funding = club_funding
  )
}

# The mode-map stocnet expressing the SAME data as
# make_legacy_fixture_twomode(): one nodes tibble with a `mode` column, one
# `membership` layer (actor -> club), and the same size/budget attribute
# changes. It is the canonical-path side of the coefficient equivalence -- a
# two-mode model built here must agree to 1e-6 with the same model assembled
# through the legacy two-node-set path.
make_stocnet_fixture_twomode_legacy_equiv <- function() {
  labels <- c("A1", "A2", "A3", "A4", "C1", "C2", "C3")
  nodes <- data.frame(
    label = labels,
    mode = c(rep("actor", 4), rep("club", 3)),
    size = c(3, 1, 2, 4, NA, NA, NA),
    budget = c(NA, NA, NA, NA, 12, 8, 20),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = match(c("A1", "A2", "A3", "A1", "A4", "A2"), labels),
    to = match(c("C1", "C1", "C2", "C3", "C2", "C3"), labels),
    time = c(1, 2, 3, 4, 5, 6),
    increment = 1,
    layer = "membership",
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    time = c(2, 4, 3, 5),
    node = match(c("A2", "A4", "C3", "C3"), labels),
    var = c("size", "size", "budget", "budget"),
    stringsAsFactors = FALSE
  )
  changes$value <- as.list(c(5, 6, 30, 25))
  changes <- changes[order(changes$time), ]
  info <- list(
    name = "legacy_equiv",
    focal = "membership",
    directed = c(membership = TRUE),
    update = c(membership = "increment"),
    observation = c(membership = "event"),
    sender = "actor",
    receiver = "club"
  )
  manynet::make_stocnet(
    info = info,
    nodes = nodes,
    ties = ties,
    changes = changes
  )
}

# DyNAM-i actors x groups fixture ---------------------------------------------
#
# The load-time legacy DyNAM-i fixture (`R/zzz_testthat_helpers.R`:
# `actors_DyNAMi`, `depevents_DyNAMi`, `exoevents_DyNAMi`, `pastupdates_DyNAMi`,
# and the constructor-built `interaction_network_DyNAMi` / `past_network_DyNAMi`
# / `dependent.depevents_DyNAMi`) expressed as the assembled multipartite
# stocnet -- the same events, both ways. It is the constructor-vs-stocnet
# equivalence input for the DyNAM-i boundary (the bridge reverses this stocnet
# back into an environment and its components must equal the load-time objects).
make_stocnet_fixture_dynami <- function() {
  assemble_interaction_stocnet(
    actors = as.data.frame(actors_DyNAMi),
    groups = data.frame(
      label = groups_DyNAMi$label,
      present = TRUE,
      stringsAsFactors = FALSE
    ),
    dependent_events = depevents_DyNAMi,
    exogenous_events = exoevents_DyNAMi,
    interaction_updates = pastupdates_DyNAMi
  )
}

# Focal-less fixtures ---------------------------------------------------------
#
# The estimable fixtures with `info$focal` stripped: the formula LHS (or the
# `make_specification` layer) names the dependent instead. Before the modeled
# layer drove focal resolution, these aborted in `check_effect_sides()`
# (`ds_layer_map(src, src$focal)`) and `ds_side_ids()`
# (`src$mode_map$layers[[src$focal]]`) with a zero-length focal name.

# Two-mode, estimable, no `info$focal`.
make_stocnet_fixture_twomode_estimable_nofocal <- function() {
  fixture <- make_stocnet_fixture_twomode_estimable()
  fixture$info$focal <- NULL
  fixture
}

# One-mode, estimable, no `info$focal`. The one-mode `calls` process; a rate/REM
# model naming `calls` on the LHS must resolve its sides from that layer alone.
make_stocnet_fixture_onemode_nofocal <- function() {
  fixture <- make_stocnet_fixture()
  fixture$info$focal <- NULL
  fixture
}

# Imputation-policy fixtures --------------------------------------------------
#
# Regression inputs for the imputation contract: the two nodal missingness
# shapes the policy surface distinguishes, and the two global-shape variants the
# schedule-construction abort rejects. All are plain stocnet lists (no manynet
# call), mirroring make_stocnet_fixture_multimode()'s mode-column machinery.

# irps_nuclear-shaped: one node set (actors) carrying a categorical attribute
# (`party`) *missing by design* for a subset -- the non-politicians, who sit
# inside the actor mode rather than in a mode of their own -- alongside a
# numeric attribute (`income`) with sparse missingness. This is the case summary
# default silently mis-fills (a non-politician gets the most common party) and
# the as-category policy recodes to a reserved level; the sparse numeric is
# imputed by the summary default from the observed values. A single fixture
# carries both: the reading formula, not the fixture, selects which attribute a
# test exercises. Missingness is sparse within the one mode -- an attribute
# *wholly* undefined for a mode is rejected earlier, by effect validity, before
# any imputation policy runs.
make_stocnet_fixture_missing_nodal <- function() {
  nodes <- data.frame(
    label = c("A1", "A2", "A3", "A4", "A5"),
    mode = "actor",
    party = c("left", "right", "left", NA, NA),
    income = c(50, NA, 70, 40, NA),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 4L, 2L, 5L),
    to = c(4L, 2L, 5L, 1L),
    time = c(1, 2, 3, 4),
    layer = "contact",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "missing_nodal",
    focal = "contact",
    update = c(contact = "increment"),
    directed = c(contact = TRUE),
    observation = c(contact = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# A global attribute carrying a missing value, for the D2 schedule-construction
# abort. `when = "init"` leaves the sole (history) value missing, so the global
# has no defined initial value; `when = "event"` gives an observed initial value
# and a later timed `replace` that is missing. Built on make_stocnet_fixture()'s
# one-mode `calls` process; a test reads the global through an effect to bring
# it into the effects link.
make_stocnet_fixture_missing_global <- function(when = c("init", "event")) {
  when <- match.arg(when)
  fixture <- make_stocnet_fixture()
  fixture$global <- if (identical(when, "init")) {
    g <- data.frame(time = NA_real_, var = "climate", stringsAsFactors = FALSE)
    g$value <- list(NA_real_)
    g
  } else {
    g <- data.frame(
      time = c(NA_real_, 1.5),
      var = "climate",
      stringsAsFactors = FALSE
    )
    g$value <- list(0, NA_real_)
    g
  }
  fixture
}

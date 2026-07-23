# make_joint_specification() composes several single-process specifications over
# one shared data object into the multivariate (DyNES) surface. These tests cover
# the construction validation matrix, the extended integer-fid process_map, and
# the direct-reference coupling flag -- the pieces estimate_dynes() and the merged
# walk build on.

# cli error/print snapshots are pinned to a reproducible width/no-color context so
# the rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# One node set, three layers: friendship (observation configurable), calls and
# emails (event). Built with friendship = "event" when a friendship-focal spec has
# to pass make_specification()'s panel-focal guard, then composed under
# friendship = "panel" so the join sees a modeled panel layer -- the only way to
# exercise a *modeled* panel at this stage, since make_specification() still
# rejects a panel focal directly.
joint_data <- function(friendship = "panel") {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      friendship = "increment",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = friendship, calls = "event", emails = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# The event stream for a mutually-exclusive flavored (creation/dissolution) calls
# process, reused from the flavored-estimation fixtures: deterministic under the
# seed, alternating creation and dissolution over a live adjacency state.
flavored_event_stream <- function(n_actors = 12L, n_events = 80L, seed = 3L) {
  withr::local_seed(seed)
  state <- matrix(0L, n_actors, n_actors)
  hist_from <- seq_len(n_actors)
  hist_to <- (hist_from %% n_actors) + 1L
  state[cbind(hist_from, hist_to)] <- 1L

  from <- integer(0)
  to <- integer(0)
  weight <- numeric(0)
  for (e in seq_len(n_events)) {
    creating <- (e %% 2L) == 1L
    cand <- which(if (creating) state == 0L else state == 1L, arr.ind = TRUE)
    cand <- cand[cand[, 1] != cand[, 2], , drop = FALSE]
    if (nrow(cand) == 0L) {
      next
    }
    pick <- cand[sample.int(nrow(cand), 1L), ]
    from <- c(from, as.integer(pick[[1]]))
    to <- c(to, as.integer(pick[[2]]))
    weight <- c(weight, if (creating) 1 else -1)
    state[pick[[1]], pick[[2]]] <- if (creating) 1L else 0L
  }

  data.frame(
    from = c(hist_from, from),
    to = c(hist_to, to),
    time = c(rep(NA_real_, n_actors), seq_along(from)),
    layer = "calls",
    weight = c(rep(1, n_actors), weight),
    stringsAsFactors = FALSE
  )
}

# A K=2 flavored rate+choice calls process, a plain rate+choice emails process,
# and friendship carried as a panel-observed exogenous covariate (read by the
# calls choice, so the panel-reference requirement is met without friendship
# being modeled). The composed process_map has six fids.
flavored_joint_data <- function() {
  n_actors <- 12L
  calls <- flavored_event_stream(n_actors)
  friendship <- data.frame(
    from = c(1L, 2L, 3L, 4L),
    to = c(2L, 3L, 4L, 5L),
    time = c(1, 2, 3, 4),
    layer = "friendship",
    weight = 1
  )
  emails <- data.frame(
    from = c(2L, 3L, 4L, 5L, 6L),
    to = c(1L, 2L, 3L, 4L, 5L),
    time = c(1, 2, 3, 4, 5),
    layer = "emails",
    weight = 1
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      calls = "increment",
      friendship = "replace",
      emails = "increment"
    ),
    directed = c(calls = TRUE, friendship = TRUE, emails = TRUE),
    observation = c(calls = "event", friendship = "panel", emails = "event")
  )
  add_flavor(
    list(
      info = info,
      nodes = data.frame(
        label = paste0("N", seq_len(n_actors)),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(calls, friendship, emails)
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# Two event specs (calls reads the friendship covariate, emails does not) over the
# data whose friendship layer is panel-observed but modeled by neither process.
exogenous_only_join <- function() {
  data <- joint_data(friendship = "panel")
  calls_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

# Compose a friendship-focal spec (built while friendship is an event layer, so it
# clears the panel-focal guard) with a calls spec, under data marking friendship
# panel -- so friendship is a *modeled* panel layer of the join. `calls_choice` is
# the choice formula whose coupling is under test.
modeled_panel_join <- function(calls_choice, support_constraint = NULL) {
  event_data <- joint_data(friendship = "event")
  friendship_spec <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = event_data
  )
  calls_spec <- make_specification(
    choice = calls_choice,
    support_constraint = support_constraint,
    layer = "calls",
    model = "DyNAM",
    data = event_data
  )
  make_joint_specification(
    friendship_spec,
    calls_spec,
    data = joint_data(friendship = "panel")
  )
}

coupled_of <- function(js, layer) {
  map <- js$process_map
  map$coupled[map$layer == layer]
}

# ---- Mode-map composition fixtures (D8) --------------------------------------
#
# One shared node universe, layers over distinct mode-pairs. Each is built with
# its panel layer as "event" (so a panel-focal spec clears make_specification()'s
# panel-focal guard) and composed under that layer marked "panel".

# Whole-shared-mode multilevel: advice staff -> director, nominations
# director -> project. nominations reads indeg(advice) on the shared director
# mode -- an identity-conforming cross-process read.
multilevel_data <- function(advice = "panel") {
  nodes <- data.frame(
    label = c("S1", "S2", "S3", "D1", "D2", "P1", "P2"),
    mode = c(
      "staff",
      "staff",
      "staff",
      "director",
      "director",
      rep("project", 2)
    ),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L),
      to = c(4L, 5L, 4L),
      time = c(1, 2, 3),
      layer = "advice",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(4L, 5L, 4L),
      to = c(6L, 7L, 7L),
      time = c(1, 2, 3),
      layer = "nominations",
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "ml",
    focal = "nominations",
    update = c(advice = "increment", nominations = "increment"),
    directed = c(advice = TRUE, nominations = TRUE),
    observation = c(advice = advice, nominations = "event"),
    sender = c(advice = "staff", nominations = "director"),
    receiver = c(advice = "director", nominations = "project")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# Two-mode multiplex: membership and attendance both p -> o. attendance reads
# tie(membership) over the identical mode-pair.
multiplex_data <- function(membership = "panel") {
  nodes <- data.frame(
    label = c("P1", "P2", "P3", "O1", "O2"),
    mode = c("p", "p", "p", "o", "o"),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L),
      to = c(4L, 5L, 4L),
      time = c(1, 2, 3),
      layer = "membership",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(1L, 2L, 3L),
      to = c(5L, 4L, 5L),
      time = c(1, 2, 3),
      layer = "attendance",
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "mx",
    focal = "attendance",
    update = c(membership = "increment", attendance = "increment"),
    directed = c(membership = TRUE, attendance = TRUE),
    observation = c(membership = membership, attendance = "event"),
    sender = c(membership = "p", attendance = "p"),
    receiver = c(membership = "o", attendance = "o")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# Mixed one/two-mode: advice one-mode over {director}, nominations two-mode
# director -> project. nominations reads indeg(advice) on the shared director
# mode.
mixed_mode_data <- function(advice = "panel") {
  nodes <- data.frame(
    label = c("D1", "D2", "P1", "P2"),
    mode = c("director", "director", "project", "project"),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L),
      to = c(2L, 1L),
      time = c(1, 2),
      layer = "advice",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(1L, 2L, 1L),
      to = c(3L, 4L, 4L),
      time = c(1, 2, 3),
      layer = "nominations",
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "mixed",
    focal = "nominations",
    update = c(advice = "increment", nominations = "increment"),
    directed = c(advice = TRUE, nominations = TRUE),
    observation = c(advice = advice, nominations = "event"),
    sender = c(advice = "director", nominations = "director"),
    receiver = c(advice = "director", nominations = "project")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# Subset/nested (Gap B): collab over {director, staff}, board over {director}.
# board reads collab through a support-constraint atom (the atom survives
# make_specification, whose side check does not validate constraints), so the
# {director} subset -> {director, staff} union bridge reaches the join.
subset_mode_data <- function(collab = "panel") {
  nodes <- data.frame(
    label = c("D1", "D2", "S1", "S2"),
    mode = c("director", "director", "staff", "staff"),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 1L, 1L, 2L),
      time = c(1, 2, 3, 4),
      layer = "collab",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(1L, 2L),
      to = c(2L, 1L),
      time = c(1, 2),
      layer = "board",
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "sub",
    focal = "board",
    update = c(collab = "increment", board = "increment"),
    directed = c(collab = TRUE, board = TRUE),
    observation = c(collab = collab, board = "event"),
    sender = c(collab = "director", collab = "staff", board = "director"),
    receiver = c(collab = "director", collab = "staff", board = "director")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# ---- Node-space generality: mode-map conformance (D8) ------------------------

test_that("a whole-shared-mode multilevel join composes", {
  event_data <- multilevel_data(advice = "event")
  advice_spec <- make_specification(
    choice = ~inertia,
    layer = "advice",
    model = "DyNAM",
    data = event_data
  )
  nominations_spec <- make_specification(
    rate = ~ 1 + indeg(advice),
    choice = ~inertia,
    layer = "nominations",
    model = "DyNAM",
    data = event_data
  )
  js <- make_joint_specification(
    advice_spec,
    nominations_spec,
    data = multilevel_data(advice = "panel")
  )
  expect_s3_class(js, "joint_specification.goldfish")
  expect_identical(
    sort(unique(js$process_map$layer)),
    c("advice", "nominations")
  )
})

test_that("a two-mode multiplex join composes", {
  event_data <- multiplex_data(membership = "event")
  membership_spec <- make_specification(
    choice = ~inertia,
    layer = "membership",
    model = "DyNAM",
    data = event_data
  )
  attendance_spec <- make_specification(
    choice = ~ inertia + tie(membership),
    layer = "attendance",
    model = "DyNAM",
    data = event_data
  )
  expect_s3_class(
    make_joint_specification(
      membership_spec,
      attendance_spec,
      data = multiplex_data(membership = "panel")
    ),
    "joint_specification.goldfish"
  )
})

test_that("a mixed one/two-mode join composes", {
  event_data <- mixed_mode_data(advice = "event")
  advice_spec <- make_specification(
    choice = ~inertia,
    layer = "advice",
    model = "DyNAM",
    data = event_data
  )
  nominations_spec <- make_specification(
    rate = ~ 1 + indeg(advice),
    choice = ~inertia,
    layer = "nominations",
    model = "DyNAM",
    data = event_data
  )
  js <- make_joint_specification(
    advice_spec,
    nominations_spec,
    data = mixed_mode_data(advice = "panel")
  )
  expect_s3_class(js, "joint_specification.goldfish")
  # The one-mode advice process and the two-mode nominations process land on
  # their own mode-pair rows.
  expect_true(all(c("advice", "nominations") %in% js$process_map$layer))
})

test_that("a subset/nested cross-process read aborts as future development", {
  # collab is panel-observed and modeled by collab_spec, so board's read of it
  # WOULD couple the board fids -- but the {director} subset -> {director, staff}
  # union bridge is rejected by the conformance check before coupling is
  # computed, never silently coupled on a wrong projection.
  local_cli_context()
  event_data <- subset_mode_data(collab = "event")
  collab_spec <- make_specification(
    choice = ~inertia,
    layer = "collab",
    model = "DyNAM",
    data = event_data
  )
  board_spec <- make_specification(
    choice = ~inertia,
    support_constraint = ~ !tie(collab),
    layer = "board",
    model = "DyNAM",
    data = event_data
  )
  expect_snapshot(
    make_joint_specification(
      collab_spec,
      board_spec,
      data = subset_mode_data(collab = "panel")
    ),
    error = TRUE
  )
})

test_that("each process resolves sides from its own layer, not info$focal", {
  # The shared object's single info$focal names only nominations, yet the
  # two-mode advice process (staff -> director) is modeled by advice_spec. Its
  # sides must resolve against advice, not the object-level focal -- the
  # formula-drives-focal contract this change consumes. Were the advice sides
  # resolved against nominations (director -> project), the identity-conforming
  # indeg(advice) read on the director mode would be validated against the wrong
  # pair and the join would not compose.
  event_data <- multilevel_data(advice = "event")
  expect_identical(event_data$info$focal, "nominations")
  advice_spec <- make_specification(
    choice = ~inertia,
    layer = "advice",
    model = "DyNAM",
    data = event_data
  )
  # advice resolved its own two-mode sides (staff -> director), not the
  # object-level focal's.
  expect_true(advice_spec$dependent$is_two_mode)
  expect_identical(
    advice_spec$dependent$mode_pair,
    list(sender = "staff", receiver = "director")
  )
  nominations_spec <- make_specification(
    rate = ~ 1 + indeg(advice),
    choice = ~inertia,
    layer = "nominations",
    model = "DyNAM",
    data = event_data
  )
  panel_data <- multilevel_data(advice = "panel")
  expect_identical(panel_data$info$focal, "nominations")
  expect_s3_class(
    make_joint_specification(advice_spec, nominations_spec, data = panel_data),
    "joint_specification.goldfish"
  )
})

# ---- Composition validation matrix ------------------------------------------

test_that("an exogenous-only panel reference composes and stays separable", {
  js <- exogenous_only_join()
  expect_s3_class(js, "joint_specification.goldfish")
  # The joint class deliberately does NOT inherit the single-process class.
  expect_false(inherits(js, "specification.goldfish"))
  # friendship is panel-observed but modeled by no process, so reading it is a
  # static exogenous step-covariate -- nothing latent, no fid coupled.
  expect_false(any(js$process_map$coupled))
})

test_that("a join referencing no panel layer is rejected as separable", {
  local_cli_context()
  data <- joint_data(friendship = "event")
  calls_spec <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  expect_snapshot(
    make_joint_specification(calls_spec, emails_spec, data = data),
    error = TRUE
  )
})

test_that("processes over different mode-map objects are rejected", {
  local_cli_context()
  data <- joint_data(friendship = "panel")
  calls_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  # The twomode spec is built over a different data object (a different node
  # universe), so the processes do not share one mode-map object. Distinct
  # mode-pairs over a SHARED object are allowed (see the multilevel tests); a
  # different node universe is not.
  twomode_spec <- make_specification(
    choice = ~inertia,
    layer = "membership",
    model = "DyNAM",
    data = make_stocnet_fixture_twomode()
  )
  expect_snapshot(
    make_joint_specification(calls_spec, twomode_spec, data = data),
    error = TRUE
  )
})

test_that("a DyNAM-i process cannot be composed", {
  local_cli_context()
  data <- joint_data(friendship = "panel")
  calls_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  # make_specification() cannot build a DyNAM-i process, so exercise the defensive
  # model-marker guard by stamping the marker directly.
  emails_spec$model <- "DyNAMi"
  expect_snapshot(
    make_joint_specification(calls_spec, emails_spec, data = data),
    error = TRUE
  )
})

test_that("two specifications modeling the same focal layer are rejected", {
  local_cli_context()
  data <- joint_data(friendship = "panel")
  calls_a <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  calls_b <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  expect_snapshot(
    make_joint_specification(calls_a, calls_b, data = data),
    error = TRUE
  )
})

test_that("reusing one layer as a covariate across specs is allowed", {
  # friendship is read as an exogenous covariate by both calls and emails while
  # each process still models a distinct focal layer -- covariate reuse is the
  # coupling that makes joining meaningful and must not trip focal uniqueness.
  data <- joint_data(friendship = "panel")
  calls_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  expect_s3_class(
    make_joint_specification(calls_spec, emails_spec, data = data),
    "joint_specification.goldfish"
  )
})

# ---- process_map row and constraint-id correctness --------------------------

test_that("a flavored process plus a plain process yields 2K + 2 fids", {
  data <- flavored_joint_data()
  calls_spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(
      creation ~ inertia + tie(friendship),
      dissolution ~ inertia + tie(friendship)
    ),
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  js <- make_joint_specification(calls_spec, emails_spec, data = data)
  map <- js$process_map

  expect_identical(
    names(map),
    c(
      "fid",
      "layer",
      "flavor",
      "family",
      "stat_block",
      "has_intercept",
      "constraint_id",
      "coupled"
    )
  )
  # K = 2 flavored rate+choice (4 fids) + plain rate+choice (2 fids) = 6.
  expect_identical(nrow(map), 6L)
  expect_identical(map$fid, 1:6)
  expect_identical(map$layer, c(rep("calls", 4), rep("emails", 2)))
  expect_identical(
    map$flavor,
    c("creation", "creation", "dissolution", "dissolution", NA, NA)
  )
  expect_identical(
    map$family,
    c("rate", "choice", "rate", "choice", "rate", "choice")
  )
})

test_that("constraint_id is shared per (layer, flavor) and NA when unconstrained", {
  data <- flavored_joint_data()
  calls_spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(
      creation ~ inertia + tie(friendship),
      dissolution ~ inertia + tie(friendship)
    ),
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  map <- make_joint_specification(
    calls_spec,
    emails_spec,
    data = data
  )$process_map

  ids <- split(map$constraint_id, list(map$layer, map$flavor))
  # A flavor's rate and choice read one derived-mask plan, so they share one id.
  expect_identical(
    map$constraint_id[map$flavor == "creation" & !is.na(map$flavor)][1],
    map$constraint_id[map$flavor == "creation" & !is.na(map$flavor)][2]
  )
  expect_identical(
    map$constraint_id[map$flavor == "dissolution" & !is.na(map$flavor)][1],
    map$constraint_id[map$flavor == "dissolution" & !is.na(map$flavor)][2]
  )
  # Distinct flavors carry distinct derived masks, hence distinct ids.
  expect_false(identical(
    map$constraint_id[map$flavor == "creation" & !is.na(map$flavor)][1],
    map$constraint_id[map$flavor == "dissolution" & !is.na(map$flavor)][1]
  ))
  # The unconstrained plain process has no constraint plan.
  expect_true(all(is.na(map$constraint_id[map$layer == "emails"])))
})

# ---- Coupling: direct reference to a modeled panel layer --------------------

test_that("an effect reading a modeled panel layer couples the fid", {
  js <- modeled_panel_join(calls_choice = ~ inertia + tie(friendship))
  # calls reads friendship, which is modeled and panel-observed -> coupled.
  expect_true(all(coupled_of(js, "calls")))
})

test_that("a windowed effect on a modeled panel layer couples the fid", {
  # The windowed effect reads a derived network (friendship_2); coupling maps it
  # back to the friendship source layer.
  js <- modeled_panel_join(
    calls_choice = ~ inertia + recip(friendship, window = 2)
  )
  expect_true(all(coupled_of(js, "calls")))
})

test_that("a support-constraint atom reading a modeled panel layer couples the fid", {
  # No effect references friendship; the coupling comes solely from the
  # support-constraint atom.
  js <- modeled_panel_join(
    calls_choice = ~inertia,
    support_constraint = ~ !tie(friendship)
  )
  expect_true(all(coupled_of(js, "calls")))
})

test_that("reading a static exogenous panel covariate does not couple", {
  # friendship is panel-observed but modeled by no process, so the calls fid that
  # reads it stays separable.
  js <- exogenous_only_join()
  expect_false(any(coupled_of(js, "calls")))
})

# ---- Print method ------------------------------------------------------------

test_that("the joint specification prints per-layer sections (all separable)", {
  local_cli_context()
  data <- flavored_joint_data()
  calls_spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(
      creation ~ inertia + tie(friendship),
      dissolution ~ inertia + tie(friendship)
    ),
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  js <- make_joint_specification(calls_spec, emails_spec, data = data)
  expect_snapshot(print(js))
})

test_that("the print marks coupled fids against a modeled panel layer", {
  local_cli_context()
  js <- modeled_panel_join(
    calls_choice = ~inertia,
    support_constraint = ~ !tie(friendship)
  )
  expect_snapshot(print(js))
})

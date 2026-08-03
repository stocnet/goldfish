enviro_builders <- function() {
  env <- new.env()
  assign("actors_ex", actors_ex, envir = env)
  assign("networkState", networkState, envir = env)
  assign("networkExog", networkExog, envir = env)
  seasons <- make_global_attributes(data.frame(winter = 0))
  assign("seasons", seasons, envir = env)
  env
}

# The legacy-environment inputs these builder unit tests resolve names against.
# make_data() now returns a stocnet rather than an environment, so the tests
# assemble the goldfish objects into a plain environment directly.
builders_env <- function() {
  env <- new.env()
  for (nm in c(
    "depNetwork",
    "networkState",
    "networkExog",
    "actors_ex",
    "eventsIncrement",
    "eventsExogenous"
  )) {
    assign(nm, get(nm), envir = env)
  }
  env
}

test_that("build_state_container assembles networks and nodal attributes", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "networkExog", "actors_ex$attr1"),
    nodes = "actors_ex",
    envir = env
  )
  # One view per referenced node space: the legacy source has no mode column,
  # so the node-set name is the only name that space has.
  expect_named(state, c("networks", "nodal:actors_ex", "globals"))
  expect_named(state$networks, c("networkState", "networkExog"))
  expect_true(is.matrix(state$networks$networkState))
  expect_null(attr(state$networks$networkState, "events"))
  expect_false(inherits(state$networks$networkState, "network.goldfish"))
  expect_equal(
    state$networks$networkState,
    matrix(
      networkState,
      nrow(networkState),
      ncol(networkState),
      dimnames = dimnames(networkState)
    ),
    ignore_attr = FALSE
  )
  expect_equal(state[["nodal:actors_ex"]]$attr1, actors_ex$attr1)
  expect_equal(ncol(state$globals), 0L)
})

test_that("build_state_container assembles globals", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "seasons$winter"),
    nodes = "actors_ex",
    envir = env
  )
  expect_equal(nrow(state$globals), 1L)
  expect_equal(state$globals$winter, 0)
})

test_that("build_state_container records object keys for routing", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "actors_ex$attr1", "seasons$winter"),
    nodes = "actors_ex",
    envir = env
  )
  objectKeys <- attr(state, "object_keys")
  expect_equal(
    objectKeys$name,
    c("networkState", "actors_ex$attr1", "seasons$winter")
  )
  expect_equal(
    objectKeys$component,
    c("networks", "nodal:actors_ex", "globals")
  )
  expect_equal(objectKeys$key, c("networkState", "attr1", "winter"))
})

test_that("build_state_container builds a view per two-mode node set", {
  env <- enviro_builders()
  clubsEx <- make_nodes(data.frame(
    label = sprintf("Club %d", 1:3),
    size = c(10, 20, 30)
  ))
  assign("clubsEx", clubsEx, envir = env)
  state <- build_state_container(
    c("networkState", "actors_ex$attr1", "clubsEx$size"),
    nodes = "actors_ex",
    nodes2 = "clubsEx",
    envir = env
  )
  expect_equal(state[["nodal:clubsEx"]]$size, c(10, 20, 30))
  expect_equal(nrow(state[["nodal:actors_ex"]]), 5L)
})

test_that("build_state_container aborts on unknown attribute or node set", {
  env <- enviro_builders()
  expect_error(
    build_state_container(
      "actors_ex$missing",
      nodes = "actors_ex",
      envir = env
    ),
    "not found"
  )
  otherNodes <- make_nodes(data.frame(label = "A", weight = 1))
  assign("otherNodes", otherNodes, envir = env)
  expect_error(
    build_state_container(
      "otherNodes$weight",
      nodes = "actors_ex",
      envir = env
    ),
    "none of the modeled node"
  )
})

test_that("a one-mode focal collapses both sides onto one nodal view", {
  # The collapse is what keeps the one-mode path on its existing code path:
  # both positions of a cross-side effect resolve to the same mode-set key, so
  # the reference set stays arity 1, the init receives a plain vector rather
  # than a list, and the frozen coefficient baselines never change route.
  src <- new_data_source(data = make_stocnet_fixture())
  expect_identical(
    ds_nodal_view(src, "nodes"),
    ds_nodal_view(src, ds_side_names(src)[2])
  )

  state <- build_state_container(
    "nodes$floor",
    nodes = "nodes",
    nodes2 = "nodes",
    src = src
  )
  expect_equal(names(state), c("networks", "nodal:p", "globals"))
  expect_equal(attr(state, "object_keys")$component, "nodal:p")
})

test_that("a two-mode focal keys each side to its own mode set", {
  src <- new_data_source(data = make_stocnet_fixture_multipartite())

  # Distinct keys are what give a cross-side effect arity 2, and what stops a
  # receiver-side attribute from being written into the sender's vector.
  expect_equal(ds_nodal_view(src, "nodes_side1"), "nodal:actor")
  expect_equal(ds_nodal_view(src, "nodes_side2"), "nodal:event")

  state <- build_state_container(
    c("nodes_side1$size", "nodes_side2$size"),
    nodes = "nodes_side1",
    nodes2 = "nodes_side2",
    src = src
  )
  expect_equal(
    names(state),
    c("networks", "nodal:actor", "nodal:event", "globals")
  )
  expect_equal(nrow(state[["nodal:actor"]]), 3L, label = "three actors send")
  expect_equal(nrow(state[["nodal:event"]]), 2L, label = "two events receive")
  expect_equal(
    attr(state, "object_keys")$component,
    c("nodal:actor", "nodal:event")
  )
})

test_that("a node space spanning several modes canonicalizes its key", {
  # A key names a *set*, so it must have one spelling however the modes were
  # ordered: the undeclared layer spans every mode, and sorting is what lets
  # key equality stand in for "the same node set".
  src <- new_data_source(data = make_stocnet_fixture_multimode())
  expect_equal(
    ds_nodal_view(src, "nodes"),
    "nodal:employee+outsider+supervisor"
  )
})

test_that("build_object_keys maps components without materialising data", {
  env <- enviro_builders()
  keys <- build_object_keys(
    c("networkState", "actors_ex$attr1", "seasons$winter"),
    nodes = "actors_ex",
    envir = env
  )
  expect_s3_class(keys, "data.frame")
  expect_equal(
    keys$name,
    c("networkState", "actors_ex$attr1", "seasons$winter")
  )
  expect_equal(keys$component, c("networks", "nodal:actors_ex", "globals"))
  expect_equal(keys$key, c("networkState", "attr1", "winter"))
  expect_equal(
    keys,
    attr(
      build_state_container(
        c("networkState", "actors_ex$attr1", "seasons$winter"),
        nodes = "actors_ex",
        envir = env
      ),
      "object_keys"
    )
  )
})

test_that("build_object_keys reads metadata only (mutates nothing)", {
  env <- enviro_builders()
  before <- sort(ls(env))
  build_object_keys(
    c("networkState", "networkExog", "actors_ex$attr1"),
    nodes = "actors_ex",
    envir = env
  )
  expect_equal(sort(ls(env)), before)
})

test_that("build_object_keys aborts on unknown attribute or node set", {
  env <- enviro_builders()
  expect_error(
    build_object_keys("actors_ex$missing", nodes = "actors_ex", envir = env),
    "not found"
  )
  otherNodes <- make_nodes(data.frame(label = "A", weight = 1))
  assign("otherNodes", otherNodes, envir = env)
  expect_error(
    build_object_keys("otherNodes$weight", nodes = "actors_ex", envir = env),
    "none of the modeled node"
  )
})

build_plan_fixture <- function(
  formula,
  model = "DyNAM",
  sub_model = "choice",
  stat_kind = "dyad"
) {
  env <- builders_env()
  parsed <- parse_formula(formula, envir = env)
  effects <- create_effects_functions(
    parsed$rhs_names,
    model,
    sub_model,
    envir = env
  )
  objects_effects_link <- get_objects_effects_link(parsed$rhs_names)
  events_and_link <- get_events_and_objects_link(
    parsed$dep_name,
    parsed$rhs_names,
    "actors_ex",
    "actors_ex",
    envir = env
  )
  events_effects_link <- get_events_effects_link(
    parsed$rhs_names,
    events_and_link[[2]]
  )
  state <- build_state_container(
    rownames(objects_effects_link),
    nodes = "actors_ex",
    envir = env
  )
  plan <- build_update_plan(
    effects,
    events_and_link[[2]],
    events_effects_link,
    objects_effects_link,
    state,
    stat_kind = stat_kind,
    envir = env
  )
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    state
  )
  list(
    plan = plan,
    effects_template = effects_template,
    events = events_and_link[[1]],
    events_objects_link = events_and_link[[2]],
    events_effects_link = events_effects_link,
    objects_effects_link = objects_effects_link,
    state = state,
    env = env,
    effects = effects
  )
}

test_that("build_update_plan registries cover effects and objects", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + recip + alter(actors_ex$attr1)
  )
  plan <- fixture$plan
  expect_equal(plan$effects$gid, 1:3)
  expect_equal(plan$effects$effect_name, c("inertia", "recip", "alter"))
  expect_equal(plan$effects$stat_kind, rep("dyad", 3))
  expect_equal(plan$objects$name, c("networkState", "actors_ex$attr1"))
  expect_equal(plan$objects$shape, c("dyad", "node"))
  expect_equal(plan$objects$component, c("networks", "nodal:actors_ex"))
})

test_that("build_update_plan populates the interaction/multivariate schema", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + recip + alter(actors_ex$attr1)
  )
  plan <- fixture$plan
  # single-formula main effects: role/estimate/fid/lid trivial
  expect_equal(plan$effects$role, rep("main", 3))
  expect_true(all(plan$effects$estimate))
  expect_equal(plan$effects$fid, rep(1L, 3))
  expect_equal(plan$effects$lid, 1:3)
  # interaction registries stay empty until the interaction parser
  expect_length(plan$interactions, 0)
  expect_length(plan$operand_of, 0)
  expect_equal(nrow(plan$stat_state_spec), 0)
  expect_named(plan$stat_state_spec, c("gid", "slot", "column"))
  # formula_effects seam: (fid, lid, gid) with fid = 1 for the single formula
  expect_equal(plan$formula_effects$fid, rep(1L, 3))
  expect_equal(plan$formula_effects$lid, 1:3)
  expect_equal(plan$formula_effects$gid, 1:3)
})

test_that("build_update_plan routing matches the link matrices", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actors_ex$attr1) + outdeg(networkExog)
  )
  plan <- fixture$plan
  expect_equal(plan$routing[[1]], 1L)
  expect_equal(plan$routing[[2]], 2L)
  expect_equal(plan$routing[[3]], 3L)
  expect_equal(
    plan$objects$name,
    c("networkState", "actors_ex$attr1", "networkExog")
  )
})

test_that("build_effects_template call templates resolve formals once", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actors_ex$attr1)
  )
  template_inertia <- fixture$effects_template[[1]]
  expect_identical(
    template_inertia$formal_names,
    names(formals(fixture$effects[[1]][["effect"]]))
  )
  expect_true(all(
    template_inertia$args_by_shape$dyad %in%
      c(template_inertia$formal_names)
  ))
  expect_true("sender" %in% template_inertia$args_by_shape$dyad)
  expect_false("sender" %in% template_inertia$args_by_shape$node)
  expect_equal(template_inertia$net_keys, "networkState")
  expect_equal(template_inertia$n_networks, 1L)
  expect_equal(template_inertia$n_attributes, 0L)
  template_alter <- fixture$effects_template[[2]]
  expect_equal(template_alter$att_components, "nodal:actors_ex")
  expect_equal(template_alter$att_keys, "attr1")
})

test_that("build_update_plan net_update positions for two-network effects", {
  fixture <- build_plan_fixture(
    depNetwork ~ mixed_trans(list(networkState, networkExog))
  )
  plan <- fixture$plan
  expect_equal(fixture$effects_template[[1]]$n_networks, 2L)
  expect_equal(
    fixture$effects_template[[1]]$net_keys,
    c("networkState", "networkExog")
  )
  pairs <- plan$effect_objects
  expect_equal(pairs$net_update[pairs$oid == 1], 1L)
  expect_equal(pairs$net_update[pairs$oid == 2], 2L)
  expect_true(all(is.na(pairs$att_update)))
  single <- build_plan_fixture(depNetwork ~ inertia)$plan
  expect_true(all(is.na(single$effect_objects$net_update)))
})

test_that("build_update_plan flags undirected networks for the second call", {
  env <- builders_env()
  undirNet <- make_network(nodes = actors_ex, directed = FALSE)
  undirEvents <- data.frame(
    time = c(10, 20),
    sender = c("Actor 1", "Actor 2"),
    receiver = c("Actor 2", "Actor 3"),
    increment = c(1, 1)
  )
  undirNet <- link_events(undirNet, undirEvents, nodes = actors_ex)
  assign("undirNet", undirNet, envir = env)
  assign("undirEvents", undirEvents, envir = env)
  parsed <- parse_formula(depNetwork ~ inertia + tie(undirNet), envir = env)
  effects <- create_effects_functions(
    parsed$rhs_names,
    "DyNAM",
    "choice",
    envir = env
  )
  objects_effects_link <- get_objects_effects_link(parsed$rhs_names)
  events_and_link <- get_events_and_objects_link(
    parsed$dep_name,
    parsed$rhs_names,
    "actors_ex",
    "actors_ex",
    envir = env
  )
  events_effects_link <- get_events_effects_link(
    parsed$rhs_names,
    events_and_link[[2]]
  )
  state <- build_state_container(
    rownames(objects_effects_link),
    nodes = "actors_ex",
    envir = env
  )
  plan <- build_update_plan(
    effects,
    events_and_link[[2]],
    events_effects_link,
    objects_effects_link,
    state,
    stat_kind = "dyad",
    envir = env
  )
  expect_equal(plan$objects$is_undirected, c(FALSE, TRUE))
})

test_that("build_update_plan aborts on inconsistent link matrices", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actors_ex$attr1)
  )
  broken <- fixture$events_effects_link
  broken[2, ] <- rev(broken[2, ])
  expect_error(
    build_update_plan(
      fixture$effects,
      fixture$events_objects_link,
      broken,
      fixture$objects_effects_link,
      fixture$state,
      stat_kind = "dyad",
      envir = fixture$env
    ),
    "inconsistent"
  )
})

test_that("build_event_schedule merges streams into a sorted timeline", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actors_ex$attr1) + outdeg(networkExog)
  )
  schedule <- build_event_schedule(
    fixture$events,
    fixture$events_objects_link,
    fixture$plan$objects
  )
  expect_equal(schedule$n, sum(vapply(fixture$events, nrow, integer(1))))
  expect_false(is.unsorted(schedule$time))
  expect_equal(sum(schedule$dependent), nrow(fixture$events[[1]]))
  expect_true(all(is.na(schedule$target[schedule$dependent])))
  expect_true(all(!is.na(schedule$target[!schedule$dependent])))
  expect_true(all(schedule$shape[!is.na(schedule$node)] == "node"))
  expect_true(all(schedule$shape[!is.na(schedule$sender)] == "dyad"))
})

test_that("build_event_schedule breaks timestamp ties dependent first", {
  depEvents <- data.frame(
    time = c(10, 20),
    sender = c(1L, 2L),
    receiver = c(2L, 3L),
    increment = c(1, 1)
  )
  streamA <- data.frame(
    time = c(10, 20),
    sender = c(4L, 4L),
    receiver = c(5L, 1L),
    increment = c(1, 1)
  )
  streamB <- data.frame(time = c(10, 10), node = c(1L, 2L), replace = c(5, 6))
  events <- list(dep = depEvents, streamA = streamA, streamB = streamB)
  events_objects_link <- data.frame(
    events = c("dep", "streamA", "streamB"),
    name = c(NA, "netX", "actors$attr"),
    object = c(NA, "netX", NA),
    nodeset = c(NA, NA, "actors"),
    attribute = c(NA, NA, "attr"),
    stringsAsFactors = FALSE
  )
  objectsRegistry <- data.frame(
    oid = 1:2,
    name = c("netX", "actors$attr"),
    shape = c("dyad", "node"),
    stringsAsFactors = FALSE
  )
  schedule <- build_event_schedule(events, events_objects_link, objectsRegistry)
  at10 <- which(schedule$time == 10)
  expect_equal(schedule$stream[at10], c(1L, 2L, 3L, 3L))
  expect_true(schedule$dependent[at10][1])
  expect_equal(schedule$node[at10][3:4], c(1L, 2L))
  at20 <- which(schedule$time == 20)
  expect_equal(schedule$stream[at20], c(1L, 2L))
})

test_that("build_event_schedule merges window expiry pseudo-events", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia(networkState, window = 3)
  )
  windowedStream <- grep("_3$", names(fixture$events), value = TRUE)
  expect_length(windowedStream, 1)
  windowedEvents <- fixture$events[[windowedStream]]
  expect_equal(nrow(windowedEvents), 2L * nrow(eventsIncrement))
  schedule <- build_event_schedule(
    fixture$events,
    fixture$events_objects_link,
    fixture$plan$objects
  )
  windowedStreamId <- match(windowedStream, names(fixture$events))
  fromWindowed <- schedule$stream == windowedStreamId
  expect_equal(sum(fromWindowed), nrow(windowedEvents))
  creations <- vapply(
    schedule$value[fromWindowed & unlist(schedule$value) > 0],
    identity,
    numeric(1)
  )
  expiries <- vapply(
    schedule$value[fromWindowed & unlist(schedule$value) < 0],
    identity,
    numeric(1)
  )
  expect_equal(sort(-expiries), sort(creations))
  creationTimes <- schedule$time[fromWindowed][
    unlist(schedule$value[fromWindowed]) > 0
  ]
  expiryTimes <- schedule$time[fromWindowed][
    unlist(schedule$value[fromWindowed]) < 0
  ]
  expect_equal(sort(expiryTimes), sort(creationTimes + 3))
  expect_false(is.unsorted(schedule$time))
})

test_that("build_event_schedule rebuilds deterministically", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actors_ex$attr1)
  )
  first <- build_event_schedule(
    fixture$events,
    fixture$events_objects_link,
    fixture$plan$objects
  )
  second <- build_event_schedule(
    fixture$events,
    fixture$events_objects_link,
    fixture$plan$objects
  )
  expect_identical(first, second)
})

test_that("build_event_schedule aborts on streams missing from the plan", {
  fixture <- build_plan_fixture(depNetwork ~ inertia)
  brokenRegistry <- fixture$plan$objects
  brokenRegistry$name <- "otherNetwork"
  expect_error(
    build_event_schedule(
      fixture$events,
      fixture$events_objects_link,
      brokenRegistry
    ),
    "missing from the update plan"
  )
})

test_that("state container supports in-place update round-trips", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "actors_ex$attr1", "seasons$winter"),
    nodes = "actors_ex",
    envir = env
  )
  before <- state$networks$networkState[1, 2]
  state$networks$networkState[1, 2] <- before + 5
  expect_equal(state$networks$networkState[1, 2], before + 5)
  state[["nodal:actors_ex"]]$attr1[3] <- -1.5
  expect_equal(state[["nodal:actors_ex"]]$attr1[3], -1.5)
  state$globals$winter <- 1
  expect_equal(state$globals$winter, 1)
  expect_equal(get("networkState", envir = env)[1, 2], before)
  expect_equal(get("actors_ex", envir = env)$attr1[3], actors_ex$attr1[3])
})

test_that("a covariate layer's own sender side becomes its own view", {
  # The third node space: neither modeled side names `org`, so the container
  # carries a view keyed by that mode set alongside the focal pair's two.
  src <- new_data_source(data = make_stocnet_fixture_tertius())

  state <- build_state_container(
    c("sponsor", "layer:sponsor:side1$size"),
    nodes = "nodes_side1",
    nodes2 = "nodes_side2",
    src = src
  )

  expect_equal(
    names(state),
    c("networks", "nodal:actor", "nodal:event", "nodal:org", "globals")
  )
  expect_equal(state[["nodal:org"]]$size, c(12, 8))
  expect_equal(attr(state, "object_keys")$component, c("networks", "nodal:org"))
})

test_that("a node set the source cannot resolve is still rejected", {
  # Admitting a third node space must not turn the membership check off: an
  # unrecognized identifier falls back to the sender side rather than failing,
  # so without the check a typo would silently read the wrong mode.
  src <- new_data_source(data = make_stocnet_fixture_tertius())
  args <- list(nodes = "nodes_side1", nodes2 = "nodes_side2", src = src)

  expect_error(
    do.call(build_object_keys, c(list("nodez$size"), args)),
    "none of the modeled node"
  )
  expect_error(
    do.call(build_object_keys, c(list("layer:nosuch:side1$size"), args)),
    "none of the modeled node"
  )
})

test_that("an attribute aggregated over a covariate's senders reads there", {
  # End to end: the statistic is each event's sponsor size (O1 = 12 for E1,
  # O2 = 8 for E2), constant over the three actor senders. Read on the focal
  # sender side it would have been the actors' own sizes.
  prep <- estimate_dynam(
    attend ~ tertius(sponsor, size),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_tertius()),
    preprocessing_only = TRUE
  )

  expect_equal(
    prep$initial_stats[,, 1],
    matrix(c(12, 12, 12, 8, 8, 8), nrow = 3, ncol = 2)
  )
  # The org view's own attribute stream drives the walk: O2's size replaced at
  # t = 2.2 moves E2's summary from mean(12, 8) to mean(12, 20).
  expect_equal(
    prep$stat_mat_update[4, ],
    c(10, 10, 10, 16, 16, 16),
    label = "the sponsor tie at t = 1.5, then O2's size change"
  )
})

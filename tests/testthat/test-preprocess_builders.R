enviro_builders <- function() {
  env <- new.env()
  assign("actorsEx", actorsEx, envir = env)
  assign("networkState", networkState, envir = env)
  assign("networkExog", networkExog, envir = env)
  seasons <- make_global_attributes(data.frame(winter = 0))
  assign("seasons", seasons, envir = env)
  env
}

test_that("build_state_container assembles networks and nodal attributes", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "networkExog", "actorsEx$attr1"),
    nodes = "actorsEx",
    envir = env
  )
  expect_named(state, c("networks", "nodal", "nodal2", "globals"))
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
  expect_equal(state$nodal$attr1, actorsEx$attr1)
  expect_null(state$nodal2)
  expect_equal(ncol(state$globals), 0L)
})

test_that("build_state_container assembles globals", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "seasons$winter"),
    nodes = "actorsEx",
    envir = env
  )
  expect_equal(nrow(state$globals), 1L)
  expect_equal(state$globals$winter, 0)
})

test_that("build_state_container records object keys for routing", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "actorsEx$attr1", "seasons$winter"),
    nodes = "actorsEx",
    envir = env
  )
  objectKeys <- attr(state, "object_keys")
  expect_equal(
    objectKeys$name,
    c("networkState", "actorsEx$attr1", "seasons$winter")
  )
  expect_equal(objectKeys$component, c("networks", "nodal", "globals"))
  expect_equal(objectKeys$key, c("networkState", "attr1", "winter"))
})

test_that("build_state_container builds nodal2 for two-mode node sets", {
  env <- enviro_builders()
  clubsEx <- make_nodes(data.frame(
    label = sprintf("Club %d", 1:3),
    size = c(10, 20, 30)
  ))
  assign("clubsEx", clubsEx, envir = env)
  state <- build_state_container(
    c("networkState", "actorsEx$attr1", "clubsEx$size"),
    nodes = "actorsEx",
    nodes2 = "clubsEx",
    envir = env
  )
  expect_equal(state$nodal2$size, c(10, 20, 30))
  expect_equal(nrow(state$nodal), 5L)
})

test_that("build_state_container aborts on unknown attribute or node set", {
  env <- enviro_builders()
  expect_error(
    build_state_container(
      "actorsEx$missing",
      nodes = "actorsEx",
      envir = env
    ),
    "not found"
  )
  otherNodes <- make_nodes(data.frame(label = "A", weight = 1))
  assign("otherNodes", otherNodes, envir = env)
  expect_error(
    build_state_container(
      "otherNodes$weight",
      nodes = "actorsEx",
      envir = env
    ),
    "neither"
  )
})

test_that("build_object_keys maps components without materialising data", {
  env <- enviro_builders()
  keys <- build_object_keys(
    c("networkState", "actorsEx$attr1", "seasons$winter"),
    nodes = "actorsEx",
    envir = env
  )
  expect_s3_class(keys, "data.frame")
  expect_equal(keys$name, c("networkState", "actorsEx$attr1", "seasons$winter"))
  expect_equal(keys$component, c("networks", "nodal", "globals"))
  expect_equal(keys$key, c("networkState", "attr1", "winter"))
  expect_equal(
    keys,
    attr(
      build_state_container(
        c("networkState", "actorsEx$attr1", "seasons$winter"),
        nodes = "actorsEx",
        envir = env
      ),
      "object_keys"
    )
  )
})

test_that("build_object_keys reads metadata only (mutates nothing, design D8)", {
  env <- enviro_builders()
  before <- sort(ls(env))
  build_object_keys(
    c("networkState", "networkExog", "actorsEx$attr1"),
    nodes = "actorsEx",
    envir = env
  )
  expect_equal(sort(ls(env)), before)
})

test_that("build_object_keys aborts on unknown attribute or node set", {
  env <- enviro_builders()
  expect_error(
    build_object_keys("actorsEx$missing", nodes = "actorsEx", envir = env),
    "not found"
  )
  otherNodes <- make_nodes(data.frame(label = "A", weight = 1))
  assign("otherNodes", otherNodes, envir = env)
  expect_error(
    build_object_keys("otherNodes$weight", nodes = "actorsEx", envir = env),
    "neither"
  )
})

build_plan_fixture <- function(
  formula,
  model = "DyNAM",
  sub_model = "choice",
  stat_kind = "dyad"
) {
  env <- rlang::env_clone(dataTest)
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
    "actorsEx",
    "actorsEx",
    envir = env
  )
  events_effects_link <- get_events_effects_link(
    parsed$rhs_names,
    events_and_link[[2]]
  )
  state <- build_state_container(
    rownames(objects_effects_link),
    nodes = "actorsEx",
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
    depNetwork ~ inertia + recip + alter(actorsEx$attr1)
  )
  plan <- fixture$plan
  expect_equal(plan$effects$gid, 1:3)
  expect_equal(plan$effects$effect_name, c("inertia", "recip", "alter"))
  expect_equal(plan$effects$stat_kind, rep("dyad", 3))
  expect_equal(plan$objects$name, c("networkState", "actorsEx$attr1"))
  expect_equal(plan$objects$shape, c("dyad", "node"))
  expect_equal(plan$objects$component, c("networks", "nodal"))
})

test_that("build_update_plan populates the interaction/multivariate schema (design D9/D10)", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + recip + alter(actorsEx$attr1)
  )
  plan <- fixture$plan
  # single-formula main effects: role/estimate/fid/lid trivial (task 2.4)
  expect_equal(plan$effects$role, rep("main", 3))
  expect_true(all(plan$effects$estimate))
  expect_equal(plan$effects$fid, rep(1L, 3))
  expect_equal(plan$effects$lid, 1:3)
  # interaction registries stay empty until the interaction parser (task 2.5)
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
    depNetwork ~ inertia + alter(actorsEx$attr1) + outdeg(networkExog)
  )
  plan <- fixture$plan
  expect_equal(plan$routing[[1]], 1L)
  expect_equal(plan$routing[[2]], 2L)
  expect_equal(plan$routing[[3]], 3L)
  expect_equal(
    plan$objects$name,
    c("networkState", "actorsEx$attr1", "networkExog")
  )
})

test_that("build_effects_template call templates resolve formals once", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actorsEx$attr1)
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
  expect_equal(template_alter$att_components, "nodal")
  expect_equal(template_alter$att_keys, "attr1")
})

test_that("build_update_plan netUpdate positions for two-network effects", {
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
  env <- rlang::env_clone(dataTest)
  undirNet <- make_network(nodes = actorsEx, directed = FALSE)
  undirEvents <- data.frame(
    time = c(10, 20),
    sender = c("Actor 1", "Actor 2"),
    receiver = c("Actor 2", "Actor 3"),
    increment = c(1, 1)
  )
  undirNet <- link_events(undirNet, undirEvents, nodes = actorsEx)
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
    "actorsEx",
    "actorsEx",
    envir = env
  )
  events_effects_link <- get_events_effects_link(
    parsed$rhs_names,
    events_and_link[[2]]
  )
  state <- build_state_container(
    rownames(objects_effects_link),
    nodes = "actorsEx",
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
    depNetwork ~ inertia + alter(actorsEx$attr1)
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
    depNetwork ~ inertia + alter(actorsEx$attr1) + outdeg(networkExog)
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
  eventsObjectsLink <- data.frame(
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
  schedule <- build_event_schedule(events, eventsObjectsLink, objectsRegistry)
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
    depNetwork ~ inertia + alter(actorsEx$attr1)
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
    c("networkState", "actorsEx$attr1", "seasons$winter"),
    nodes = "actorsEx",
    envir = env
  )
  before <- state$networks$networkState[1, 2]
  state$networks$networkState[1, 2] <- before + 5
  expect_equal(state$networks$networkState[1, 2], before + 5)
  state$nodal$attr1[3] <- -1.5
  expect_equal(state$nodal$attr1[3], -1.5)
  state$globals$winter <- 1
  expect_equal(state$globals$winter, 1)
  expect_equal(get("networkState", envir = env)[1, 2], before)
  expect_equal(get("actorsEx", envir = env)$attr1[3], actorsEx$attr1[3])
})

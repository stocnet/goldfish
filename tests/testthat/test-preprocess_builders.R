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
    nodes = "actorsEx", envir = env
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
      nrow(networkState), ncol(networkState),
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
    nodes = "actorsEx", envir = env
  )
  expect_equal(nrow(state$globals), 1L)
  expect_equal(state$globals$winter, 0)
})

test_that("build_state_container records object keys for routing", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "actorsEx$attr1", "seasons$winter"),
    nodes = "actorsEx", envir = env
  )
  objectKeys <- attr(state, "object_keys")
  expect_equal(objectKeys$name, c("networkState", "actorsEx$attr1", "seasons$winter"))
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
    nodes = "actorsEx", nodes2 = "clubsEx", envir = env
  )
  expect_equal(state$nodal2$size, c(10, 20, 30))
  expect_equal(nrow(state$nodal), 5L)
})

test_that("build_state_container aborts on unknown attribute or node set", {
  env <- enviro_builders()
  expect_error(
    build_state_container(
      "actorsEx$missing",
      nodes = "actorsEx", envir = env
    ),
    "not found"
  )
  otherNodes <- make_nodes(data.frame(label = "A", weight = 1))
  assign("otherNodes", otherNodes, envir = env)
  expect_error(
    build_state_container(
      "otherNodes$weight",
      nodes = "actorsEx", envir = env
    ),
    "neither"
  )
})

build_plan_fixture <- function(
    formula, model = "DyNAM", sub_model = "choice", stat_kind = "dyad") {
  env <- rlang::env_clone(dataTest)
  parsed <- parse_formula(formula, envir = env)
  effects <- create_effects_functions(
    parsed$rhs_names, model, sub_model,
    envir = env
  )
  objects_effects_link <- get_objects_effects_link(parsed$rhs_names)
  events_and_link <- get_events_and_objects_link(
    parsed$dep_name, parsed$rhs_names, "actorsEx", "actorsEx",
    envir = env
  )
  events_effects_link <- get_events_effects_link(
    events_and_link[[1]], parsed$rhs_names, events_and_link[[2]]
  )
  state <- build_state_container(
    rownames(objects_effects_link),
    nodes = "actorsEx", envir = env
  )
  plan <- build_update_plan(
    effects, events_and_link[[2]], events_effects_link,
    objects_effects_link, state,
    stat_kind = stat_kind, envir = env
  )
  list(
    plan = plan, events = events_and_link[[1]],
    events_objects_link = events_and_link[[2]],
    events_effects_link = events_effects_link,
    objects_effects_link = objects_effects_link,
    state = state, env = env, effects = effects
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

test_that("build_update_plan call templates resolve formals once", {
  fixture <- build_plan_fixture(
    depNetwork ~ inertia + alter(actorsEx$attr1)
  )
  template_inertia <- fixture$plan$templates[[1]]
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
  template_alter <- fixture$plan$templates[[2]]
  expect_equal(template_alter$att_components, "nodal")
  expect_equal(template_alter$att_keys, "attr1")
})

test_that("build_update_plan netUpdate positions for two-network effects", {
  fixture <- build_plan_fixture(
    depNetwork ~ mixed_trans(list(networkState, networkExog))
  )
  plan <- fixture$plan
  expect_equal(plan$templates[[1]]$n_networks, 2L)
  expect_equal(
    plan$templates[[1]]$net_keys,
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
    time = c(10, 20), sender = c("Actor 1", "Actor 2"),
    receiver = c("Actor 2", "Actor 3"), increment = c(1, 1)
  )
  undirNet <- link_events(undirNet, undirEvents, nodes = actorsEx)
  assign("undirNet", undirNet, envir = env)
  assign("undirEvents", undirEvents, envir = env)
  parsed <- parse_formula(depNetwork ~ inertia + tie(undirNet), envir = env)
  effects <- create_effects_functions(
    parsed$rhs_names, "DyNAM", "choice",
    envir = env
  )
  objects_effects_link <- get_objects_effects_link(parsed$rhs_names)
  events_and_link <- get_events_and_objects_link(
    parsed$dep_name, parsed$rhs_names, "actorsEx", "actorsEx",
    envir = env
  )
  events_effects_link <- get_events_effects_link(
    events_and_link[[1]], parsed$rhs_names, events_and_link[[2]]
  )
  state <- build_state_container(
    rownames(objects_effects_link),
    nodes = "actorsEx", envir = env
  )
  plan <- build_update_plan(
    effects, events_and_link[[2]], events_effects_link,
    objects_effects_link, state,
    stat_kind = "dyad", envir = env
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
      fixture$effects, fixture$events_objects_link, broken,
      fixture$objects_effects_link, fixture$state,
      stat_kind = "dyad", envir = fixture$env
    ),
    "inconsistent"
  )
})

test_that("state container supports in-place update round-trips", {
  env <- enviro_builders()
  state <- build_state_container(
    c("networkState", "actorsEx$attr1", "seasons$winter"),
    nodes = "actorsEx", envir = env
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

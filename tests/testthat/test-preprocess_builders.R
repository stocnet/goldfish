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

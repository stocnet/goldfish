test_that("define functions deprecation", {
  expect_snapshot(defineNodes(data.frame(label = "a")))
  expect_snapshot(defineGlobalAttribute(data.frame(time = 1, replace = 1)))
  # defineNetwork needs a matrix, nodes, and optionally nodes2
  # For simplicity, we'll mock minimal inputs just to trigger the deprecation
  nodes_df <- data.frame(label = c("a", "b"), present = c(TRUE, TRUE))
  # Create a simple matrix for defineNetwork
  mat <- matrix(
    0,
    nrow = 2,
    ncol = 2,
    dimnames = list(nodes_df$label, nodes_df$label)
  )
  expect_snapshot(mat <- defineNetwork(mat, nodes_df))
  # Create a simple events data frame
  events_df <- data.frame(
    time = seq.int(3),
    sender = rep("a", 3),
    receiver = rep("b", 3),
    increment = rep(1, 3)
  )
  expect_snapshot(mat <- linkEvents(mat, events_df, nodes_df))
  expect_snapshot(defineDependentEvents(
    events_df,
    nodes_df,
    default_network = mat
  ))
})
test_that("define dependent events deprecated", {
  # defineGroups_interaction requires records and actors
  records_df <- data.frame(
    nodeA = c(1, 3, 1, 4),
    nodeB = c(2, 4, 3, 2),
    Start = c(0, 0, 4, 5),
    End = c(3, 3, 5, 7)
  )
  actors_df <- data.frame(
    label = letters[seq.int(4)],
    present = rep(TRUE, 4)
  )
  expect_snapshot(defineGroups_interaction(
    records_df,
    actors_df,
    seed_randomization = 123
  ))
})

# test_that("Deprecated estimation and examination functions throw warnings", {
# estimate() is more complex, might need a formula and objects
# For now, just calling it to check the deprecation message primarily
# This might error out due to missing arguments after the deprecation warning, which is fine for this test's scope.
# If the test fails because of an error *after* the deprecation, we might need to adjust.
# expect_snapshot(tryCatch(estimate(), error = function(e) NULL))
# expect_snapshot(tryCatch(examineOutliers(), error = function(e) NULL)) # Needs arguments, wrap in tryCatch
# expect_snapshot(tryCatch(examineChangepoints(), error = function(e) NULL)) # Needs arguments, wrap in tryCatch
# })

# test_that("Deprecated effect functions throw warnings", {
#
# })

test_that("defineNetwork deprecated", {
  expect_warning(
    defineNetwork(
      matrix = networkState,
      nodes = actors_ex,
      directed = TRUE
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("defineDependentEvents deprecated", {
  expect_warning(
    defineDependentEvents(
      events = eventsIncrement,
      nodes = actors_ex,
      default_network = networkState
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("defineGlobalAttribute deprecated", {
  expect_warning(
    defineGlobalAttribute(
      data.frame(time = 1, replace = 1)
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("linkEvents deprecated", {
  compChange1 <- data.frame(
    node = sprintf("Actor %d", c(5, 4, 4, 1, 5, 1, 5)),
    time = c(10, 12, 17, 26, 26, 30, 30),
    replace = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_warning(
    linkEvents(
      x = actors_ex,
      change_events = compChange1,
      attribute = "present"
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("examineOutliers deprecated", {
  mod00 <- estimate_dynam(
    depNetwork ~ inertia + trans + indeg,
    sub_model = "choice",
    data = dataTest,
    control_preprocessing = set_preprocessing_opt(start_time = 0L),
    control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )
  expect_warning(
    examineOutliers(
      mod00,
      method = "Top",
      parameter = 2
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("examineChangepoints deprecated", {
  mod00 <- estimate_dynam(
    depNetwork ~ inertia + trans + indeg,
    sub_model = "choice",
    data = dataTest,
    control_preprocessing = set_preprocessing_opt(start_time = 0L),
    control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )
  expect_warning(
    examineChangepoints(
      mod00,
      moment = "mean",
      method = "PELT"
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("egoAlterInt deprecated", {
  expect_warning(
    # update_REM_choice_ego_alter_interaction(list(testAttr$fishingSkill,testAttr$fishCaught), node = 1, replace = 0, att_update = 1, n1 = 8, n2 = 0)
    egoAlterInt(
      list(testAttr$fishingSkill, testAttr$fishCaught),
      node = 1,
      replace = 0,
      att_update = 1,
      n1 = 8,
      n2 = 0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("nodeTrans deprecated", {
  cache <- c(0, 0, 1, 1, 0)
  expect_warning(
    nodeTrans(
      m,
      1,
      5,
      1,
      cache,
      5,
      5,
      type = "alter"
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("commonSender deprecated", {
  expect_warning(
    commonSender(
      m,
      sender = 1,
      receiver = 5,
      replace = 1,
      cache = m0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("commonReceiver deprecated", {
  expect_warning(
    commonReceiver(
      m,
      sender = 1,
      receiver = 5,
      replace = 1,
      cache = m0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("mixedTrans deprecated", {
  # update_REM_choice_mixed_trans(list(m,m1), 4, 3, 5, 1, m0)
  expect_warning(
    mixedTrans(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("mixedCycle deprecated", {
  # update_REM_choice_mixed_cycle(list(m,m1), 4, 3, 5, 1, m0)
  expect_warning(
    mixedCycle(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("mixedCommonSender deprecated", {
  expect_warning(
    mixedCommonSender(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("mixedCommonReceiver deprecated", {
  expect_warning(
    mixedCommonReceiver(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0
    ),
    "deprecated in goldfish 1.7.0."
  )
})

test_that("tertiusDiff deprecated", {
  # update_DyNAM_choice_tertius_diff(m, testAttr$fishingSkill, sender = 2, receiver = 3, node = NULL, 3, m0, n1 = 5, n2=5)
  expect_warning(
    tertiusDiff(
      m,
      testAttr$fishingSkill,
      sender = 2,
      receiver = 3,
      node = NULL,
      3,
      m0,
      n1 = 5,
      n2 = 5
    ),
    "deprecated in goldfish 1.7.0."
  )
})

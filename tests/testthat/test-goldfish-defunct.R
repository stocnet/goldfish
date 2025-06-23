test_that("define functions deprecation", {
  expect_snapshot(defineNodes(data.frame(label = "a")))
  expect_snapshot(defineGlobalAttribute(data.frame(time = 1, replace = 1)))
  # defineNetwork needs a matrix, nodes, and optionally nodes2
  # For simplicity, we'll mock minimal inputs just to trigger the deprecation
  nodes_df <- data.frame(label = c("a", "b"), present = c(TRUE, TRUE))
  # Create a simple matrix for defineNetwork
  mat <- matrix(0, nrow = 2, ncol = 2,
                dimnames = list(nodes_df$label, nodes_df$label))
  expect_snapshot(mat <- defineNetwork(mat, nodes_df))
  # Create a simple events data frame
  events_df <- data.frame(
    time = seq.int(3),
    sender = rep("a", 3), receiver = rep("b", 3),
    increment = rep(1, 3)
    )
  expect_snapshot(mat <- linkEvents(mat, events_df, nodes_df))
  expect_snapshot(defineDependentEvents(
    events_df, nodes_df, default_network = mat
  ))
})
test_that("define dependent events deprecated", {    
  # defineGroups_interaction requires records and actors
  records_df <- data.frame(
    nodeA = c(1, 3, 1, 4), nodeB = c(2, 4, 3, 2),
    Start = c(0, 0, 4, 5), End = c(3, 3, 5, 7)
  )
  actors_df <- data.frame(
    label = letters[seq.int(4)], present = rep(TRUE, 4)
  )
  expect_snapshot(defineGroups_interaction(
    records_df, actors_df, seed_randomization = 123
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

test_that(
  "Reduce size updates",
  {
    depUpdates <- matrix(
      c(1, 2, 0, 2, 3, 1, 4, 5, 1),
      nrow = 3,
      dimnames = list(NULL, c("node1", "node2", "replace")),
      byrow = TRUE
    )
    updates <- matrix(
      c(1, 2, 1, 4, 5, 2),
      nrow = 2,
      dimnames = list(NULL, c("node1", "node2", "replace")),
      byrow = TRUE
    )

    check1 <- ReduceUpdateNonDuplicates(NULL, updates)
    expect_type(check1, "double")
    expect_true(inherits(check1, "array"))
    expect_length(check1, 2 * 3)
    expect_equal(check1, updates)

    check2 <- ReduceUpdateNonDuplicates(depUpdates, updates)
    outcome <- matrix(
      c(2, 3, 1, 1, 2, 1, 4, 5, 2),
      nrow = 3,
      dimnames = list(NULL, c("node1", "node2", "replace")),
      byrow = TRUE
    )
    expect_type(check2, "double")
    expect_true(inherits(check2, "array"))
    expect_length(check2, 3 * 3)
    expect_equal(check2, outcome)

    check3 <- ReduceUpdateNonDuplicates(depUpdates, NULL)
    expect_type(check3, "double")
    expect_true(inherits(check3, "array"))
    expect_length(check3, 3 * 3)
    expect_equal(check3, depUpdates)

    expect_null(ReduceUpdateNonDuplicates(NULL, NULL))
  }
)

test_that(
  "Imputation of missing data when numerical data is missing.",
  {
    # redefine ActorsEx with NA data
    actorsEx <- data.frame(
      label = sprintf("Actor %d", 1:5),
      present = c(rep(TRUE, 5)),
      attr1 = c(1, 0, 1, NA, 1),
      stringsAsFactors = FALSE
    )

    attrChange <- data.frame(
      node = sprintf("Actor %d", c(5, 4, 3, 1, 2, 3, 4)),
      time = c(11, 18, 23, 31, 32, 33, 35),
      # Changed replacement values to binary
      replace = c(0, 1, 0, 1, 0, 1, 0),
      stringsAsFactors = FALSE
    )

    # have to relink here
    actorsEx <- make_nodes(actorsEx)
    actorsEx <- link_events(x = actorsEx, change_events = compChange, attribute = "present")
    actorsEx <- link_events(x = actorsEx, change_events = attrChange, attribute = "attr1")

    networkState <- make_network(matrix = networkState, nodes = actorsEx, directed = TRUE)
    networkState <- link_events(x = networkState, change_events = eventsIncrement, nodes = actorsEx)
    depNetwork <- make_dependent_events(events = eventsIncrement, nodes = actorsEx, default_network = networkState)

    dataTest <- make_data(depNetwork)

    effectTableTest <- data.frame(
      inertia = c(1, NA), same = c(NA, 1),
      row.names = c("networkState", "actorsEx$attr1")
    )
    expect_warning(imputeMissingData(effectTableTest, envir = environment()),
                   "Missing data has been detected. Mean is used to impute for numerical values")
  }
)

test_that(
  "Imputation of missing data when categorical/string data is missing.",
  {
    # redefine ActorsEx with NA data
    actorsEx <- data.frame(
      label = sprintf("Actor %d", 1:5),
      present = c(rep(TRUE, 4), FALSE),
      # String values for categorical grouping
      attr1 = c("Group A", "Group B", NA, "Group B", "Group A"), 
      stringsAsFactors = FALSE
    )
    
    attrChange <- data.frame(
      node = sprintf("Actor %d", c(5, 4, 3, 1, 2, 3, 4)),
      time = c(11, 18, 23, 31, 32, 33, 35),
      # Replacement values must be strings from the attribute set
      replace = c("Group B", "Group A", "Group B", "Group A", "Group B", "Group A", "Group B"), 
      stringsAsFactors = FALSE
    )
    
    # have to relink here
    actorsEx <- make_nodes(actorsEx)
    actorsEx <- link_events(x = actorsEx, change_events = compChange, attribute = "present")
    actorsEx <- link_events(x = actorsEx, change_events = attrChange, attribute = "attr1")
    
    networkState <- make_network(matrix = networkState, nodes = actorsEx, directed = TRUE)
    networkState <- link_events(x = networkState, change_events = eventsIncrement, nodes = actorsEx)
    depNetwork <- make_dependent_events(events = eventsIncrement, nodes = actorsEx, default_network = networkState)
    
    dataTest <- make_data(depNetwork)
    
    effectTableTest <- data.frame(
      inertia = c(1, NA), same = c(NA, 1),
      row.names = c("networkState", "actorsEx$attr1")
    )
    expect_warning(imputeMissingData(effectTableTest, envir = environment()),
                   "Missing data has been detected. Mode is used to impute for categorical values")
  }
)


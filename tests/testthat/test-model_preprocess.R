test_that("Imputation of missing data when numerical data is missing.", {
  # redefine ActorsEx with NA data
  actors_ex <- data.frame(
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
  actors_ex <- make_nodes(actors_ex)
  actors_ex <- link_events(
    x = actors_ex,
    change_events = compChange,
    attribute = "present"
  )
  actors_ex <- link_events(
    x = actors_ex,
    change_events = attrChange,
    attribute = "attr1"
  )

  networkState <- make_network(
    matrix = networkState,
    nodes = actors_ex,
    directed = TRUE
  )
  networkState <- link_events(
    x = networkState,
    change_events = eventsIncrement,
    nodes = actors_ex
  )
  depNetwork <- make_dependent_events(
    events = eventsIncrement,
    nodes = actors_ex,
    default_network = networkState
  )

  dataTest <- make_data(depNetwork)

  effectTableTest <- data.frame(
    inertia = c(1, NA),
    same = c(NA, 1),
    row.names = c("networkState", "actors_ex$attr1")
  )
  expect_warning(
    impute_missing_data(effectTableTest, envir = environment()),
    "Missing data has been detected. Mean is used to impute for numerical values"
  )
})

test_that("Imputation of missing data when categorical/string data is missing.", {
  # redefine ActorsEx with NA data
  actors_ex <- data.frame(
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
    replace = c(
      "Group B",
      "Group A",
      "Group B",
      "Group A",
      "Group B",
      "Group A",
      "Group B"
    ),
    stringsAsFactors = FALSE
  )

  # have to relink here
  actors_ex <- make_nodes(actors_ex)
  actors_ex <- link_events(
    x = actors_ex,
    change_events = compChange,
    attribute = "present"
  )
  actors_ex <- link_events(
    x = actors_ex,
    change_events = attrChange,
    attribute = "attr1"
  )

  networkState <- make_network(
    matrix = networkState,
    nodes = actors_ex,
    directed = TRUE
  )
  networkState <- link_events(
    x = networkState,
    change_events = eventsIncrement,
    nodes = actors_ex
  )
  depNetwork <- make_dependent_events(
    events = eventsIncrement,
    nodes = actors_ex,
    default_network = networkState
  )

  dataTest <- make_data(depNetwork)

  effectTableTest <- data.frame(
    inertia = c(1, NA),
    same = c(NA, 1),
    row.names = c("networkState", "actors_ex$attr1")
  )
  expect_warning(
    impute_missing_data(effectTableTest, envir = environment()),
    "Missing data has been detected. Mode is used to impute for categorical values"
  )
})

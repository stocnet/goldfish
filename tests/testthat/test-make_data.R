test_that(
  "NA data handled effectively in objects",
  {
    data("Fisheries_Treaties_6070")
    states <- make_nodes(states)
    sovchanges$time[5] <- NA
    expect_error(
      states <- link_events(states, sovchanges, attribute = "present"),
      "Check that all events have non-NA time"
    )
  }
)

test_that(
  "NA data handled effectively when linking to nodes.",
  {
    compChange1 <- data.frame(
      node = sprintf("Actor %d", c(5, 4, 4, 1, 5, 1, 5)),
      time = c(10, 12, 17, 26, 26, 30, 30),
      replace = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
    )
    compChange1$time[5] <- NA
    expect_error(
      link_events(
        x = actorsEx,
        change_events = compChange1,
        attribute = "present"
      ),
      "Check that all events have non-NA time",
      label = "Event time cannot be NA"
    )
    attrChange1 <- data.frame(
      node = sprintf("Actor %d", c(5, 4, 3, 1, 2, 3, 4)),
      time = c(11, 18, 23, 31, 32, 33, 35),
      replace = c(1.2, 1.67, 2.46, 7.89, 3.32, 2.32, 3.44)
    )
    attrChange1$replace[4] <- NA
    expect_warning(
      # changing attribute
      link_events(actorsEx, attrChange1, attribute = "attr1"),
      "Missing replace value data exists",
      label = "Warnings are issued when replace values are NA."
    )
    colnames(attrChange1) <- c("node", "time", "increment")
    expect_warning(
      # changing attribute
      link_events(actorsEx, attrChange1, attribute = "attr1"),
      "Missing increment value data exists",
      label = "Warnings are issued when increment values are NA."
    )
    attrChange1$increment[4] <- 7.89
    attrChange1$node[4] <- "Actor x"
    expect_error(
      # changing attribute
      link_events(actorsEx, attrChange1, attribute = "attr1"),
      "Make sure all node labels are present in the nodeset",
      label = "Node labels should not contain missing data"
    )
    attrChange1$node[4] <- NA_character_
    expect_error(
      # changing attribute
      link_events(actorsEx, attrChange1, attribute = "attr1"),
      "Check that node labels are not missing data",
      label = "Node labels should not contain missing data"
    )
  }
)

test_that(
  "NA data handled effectively when linking to networks.",
  {
    eventsIncrement1 <- data.frame(
      time = cumsum(c(NA, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4)),
      sender = sprintf("Actor %d", c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5, 1)),
      receiver = sprintf("Actor %d", c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2, 2)),
      replace = c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1),
      stringsAsFactors = FALSE
    )
    networkState1 <- make_network(
      matrix = networkState, nodes = actorsEx,
      directed = TRUE
    )
    expect_error(
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "have non-NA time",
      label = "Event time cannot be NA"
    )
    eventsIncrement1$time <- cumsum(c(1, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4))

    eventsIncrement1$replace[4] <- NA
    expect_warning(
      # changing attribute
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "Missing replace value data exists",
      label = "Warnings are issued when replace values are NA."
    )
    colnames(eventsIncrement1) <- c("time", "sender", "receiver", "increment")
    expect_warning(
      # changing attribute
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "Missing increment value data exists",
      label = "Warnings are issued when increment values are NA."
    )
    eventsIncrement1$increment[4] <- 7.89
    eventsIncrement1$sender[1] <- NA_character_
    expect_error(
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "have non-NA senders",
      label = "Senders must not be NA"
    )
    eventsIncrement1$receiver[1] <- NA_character_
    eventsIncrement1$sender[1] <- "Actor 1"
    expect_error(
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "have non-NA receivers",
      label = "Receivers must not be NA"
    )
  }
)

test_that(
  "make_nodes functions as expected.",
  {
    expect_no_error(make_nodes(clubsEx), message = "Function has unexpected error")
    expect_error(make_nodes(cbind(label = c("Club 1", "Club 2", "Club 3"), present = c(TRUE, TRUE, FALSE), clubSize = c(7, 9, 2))),
      "expects objects of class \"data.frame\"",
      label = "Function should only accept data frames"
    )
  }
)

test_that(
  "make_network functions as expected.",
  {
    matrixTestForNodes <- cbind(label = c("Club 1", "Club 2", "Club 3"), present = c(TRUE, TRUE, FALSE), clubSize = c(7, 9, 2))
    expect_no_error(make_network(
      matrix = networkState, nodes = actorsEx,
      directed = TRUE
    ), message = "Function has unexpected error")

    expect_error(make_network(matrix = networkState, nodes = matrixTestForNodes),
      "Invalid argument \"nodes\": this function expects objects of class \"data.frame\" or \"nodes.goldfish\".",
      label = "Function should only accept data frames in nodes"
    )

    expect_error(make_network(matrix = networkState, nodes = actorsEx, nodes2 = matrixTestForNodes),
      "Invalid argument \"nodes2\": this function expects objects of class \"data.frame\" or \"nodes.goldfish\".",
      label = "Function should only accept data frames in nodes2 when two mode network"
    )

    three_d_array <- array(1:8, dim = c(2, 2, 2))

    expect_error(make_network(matrix = three_d_array, nodes = actorsEx),
      "nvalid argument \"matrix\": this function expects an objects of class \"matrix\" or \"Matrix\".",
      label = "Function should only accept tables as matrix"
    )

    class(three_d_array) <- "table"

    expect_error(make_network(matrix = three_d_array, nodes = actorsEx),
      "\"matrix\" object has an incorrect number of dimensions.",
      label = "Matrix should be a two-dimensional table"
    )

    expect_error(make_network(matrix = three_d_array, nodes = actorsEx, directed = "Not a logical"),
      "Invalid argument \"directed\": this function expects a logical value.",
      label = "Directed must be a logical value"
    )
  }
)

test_that(
  "link_events works as expected.",
  {
    non_df_input <- array(1:8, dim = c(2, 2, 2))

    expect_error(link_events(x = networkState, change_events = eventsIncrement),
      "Invalid argument nodes: a network is specified, this function expects an argument nodes.",
      label = "Error if nodes is NULL"
    )

    expect_error(link_events(x = networkState, change_events = non_df_input, nodes = actorsEx),
      "Invalid argument change_events: this function expects a data frame.",
      label = "Error if change_events is not a data frame"
    )

    expect_error(link_events(x = networkState, change_events = eventsIncrement, nodes = non_df_input),
      "Invalid argument nodes: this function expects a nodeset \\(data frame or nodes.goldfish object\\).",
      label = "Error if nodes is not a data frame or nodes.goldfish object"
    )

    expect_error(link_events(x = networkState, change_events = eventsIncrement, nodes = actorsEx, nodes2 = non_df_input),
      "Invalid argument nodes2: this function expects a nodeset \\(data frame or nodes.goldfish object\\).",
      label = "Error if nodes2 is not a data frame or nodes.goldfish object"
    )


    expect_error(link_events(x = networkState, change_events = data.frame(node = "test"), nodes = actorsEx),
      "Parameter change events has to be the name of a data frame \\(rather than a data frame\\)",
      label = "Error if change_events is not passed as a variable name"
    )
    expect_warning(link_events(x = networkState, change_events = eventsIncrement, nodes = actorsEx),
      "were already linked to this object.",
      label = "Warning if event name is already linked"
    )
  }
)


test_that(
  "make_dependent_events works as expected.",
  {
    non_df_input <- array(1:8, dim = c(2, 2, 2))

    expect_error(make_dependent_events(events = non_df_input, nodes = actorsEx),
      "Invalid argument \"events\": this function expects objects of class \"data.frame\".",
      label = "Error if events is not a data frame"
    )
    expect_error(make_dependent_events_goldfish(events = non_df_input, nodes = actorsEx),
      "Invalid argument \"events\": this function expects objects of class \"data.frame\".",
      label = "Error if events is not a data frame"
    )

    expect_error(make_dependent_events(events = eventsIncrement, nodes = non_df_input),
      "Invalid argument \"nodes\": this function expects objects of class \"data.frame\" or \"nodes.goldfish\".",
      label = "Error if nodes is not a nodeset object"
    )

    expect_error(make_dependent_events(events = eventsIncrement, nodes = actorsEx, nodes2 = non_df_input),
      "Invalid argument \"nodes2\": this function expects objects of class \"data.frame\" or \"nodes.goldfish\".",
      label = "Error if nodes2 is not a nodeset object in two-mode network"
    )

    expect_error(make_dependent_events(events = eventsIncrement, nodes = actorsEx, default_network = non_df_input),
      "Invalid argument \"default_network\": this function expects objects of class \"network.goldfish\".",
      label = "Error if default_network is not a network.goldfish object"
    )

    expect_error(make_dependent_events(events = eventsIncrement, nodes = actorsEx, default_network = interaction_network_DyNAMi),
      "Node sets of default networks differ from node sets of dependent event data frame\\.",
      label = "Error if node sets of network and events mismatch"
    )

    expect_warning(make_dependent_events(events = eventsIncrement, nodes = actorsEx, default_network = networkStateTrans),
      "The events data frame is not linked to the default_network",
      label = "Warning if events are not linked to the default network"
    )
    expected_obj <- data.frame(list(
      time = c(1, 6, 9, 13, 15, 16, 19, 23, 28, 29, 32, 36),
      sender = c(
        "Actor 1", "Actor 3", "Actor 2", "Actor 2", "Actor 5",
        "Actor 1", "Actor 3", "Actor 3", "Actor 4", "Actor 2", "Actor 5", "Actor 1"
      ),
      receiver = c(
        "Actor 2", "Actor 2", "Actor 3", "Actor 3", "Actor 1", "Actor 5", "Actor 4",
        "Actor 4", "Actor 2", "Actor 3", "Actor 2", "Actor 2"
      ),
      increment = c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1)
    ))
    expect_equal(make_dependent_events(events = eventsIncrement, nodes = actorsEx, default_network = networkState),
      expected_obj,
      label = "Monadic case should succeed without error",
      ignore_attr = TRUE
    )
  }
)



test_that(
  "make_data works as expected",
  {
    expect_error(make_data(),
      "No arguments provided to make_data.",
      label = "No arguments detected for make_data"
    )
  }
)

test_that(
  "as.data.frame.nodes.goldfish works as expected",
  {
    expected_df <- data.frame(
      label = sprintf("Actor %d", 1:5),
      present = c(TRUE, TRUE, TRUE, TRUE, FALSE),
      attr1 = c(9.90, 0.10, 0.50, 0.45, 0.25),
      stringsAsFactors = FALSE
    )
    expect_equal(as.data.frame.nodes.goldfish(actorsEx),
      expected_df,
      ignore_attr = TRUE
    )
  }
)

test_that(
  "as.matrix.network.goldfish works as expected",
  {
    expect_equal(
      as.matrix.network.goldfish(networkExog),
      matrix(
        c(
          0, 0, 0, 1, 0,
          0, 0, 0, 0, 0,
          0, 2, 0, 0, 0,
          1, 0, 0, 0, 0,
          1, 2, 0, 0, 0
        ),
        nrow = 5, ncol = 5, byrow = TRUE,
        dimnames = list(
          sprintf("Actor %d", 1:5),
          sprintf("Actor %d", 1:5)
        )
      )
    )
  }
)

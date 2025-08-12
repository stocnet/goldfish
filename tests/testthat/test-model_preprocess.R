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

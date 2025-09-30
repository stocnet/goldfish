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
    states <- as_nodes_goldfish(states)
    sovchanges$time[5] <- NA
    expect_error(
      states <- link_events(states, sovchanges, attribute = "present"),
      "Event time cannot be NA"
    )
  }
)

test_that(
  "NA data handled effectively in objects",
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
      "Event time cannot be NA",
      label = "Event time cannot be NA"
    )
    attrChange1 <- data.frame(
      node = sprintf("Actor %d", c(5, 4, 3, 1, 2, 3, 4)),
      time = c(11, 18, 23, 31, 32, 33, 35),
      replace = c(1.2, 1.67, 2.46, 7.89, 3.32, 2.32, 3.44)
    )
    attrChange1$node[5] <- NA_character_
    expect_error(
      # changing attribute
      link_events(actorsEx, attrChange1, attribute = "attr1"),
      "Node labels should not contain missing data",
      label = "Node labels should not contain missing data"
    )
    eventsIncrement1 <- data.frame(
      time = cumsum(c(NA, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4)),
      sender = sprintf("Actor %d", c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5, 1)),
      receiver = sprintf("Actor %d", c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2, 2)),
      increment = c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1),
      stringsAsFactors = FALSE
    )
    networkState1 <- as_network_goldfish(
      matrix = networkState, nodes = actorsEx,
      directed = TRUE
    )
    expect_error(
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "Event time cannot be NA",
      label = "Event time cannot be NA"
    )
    eventsIncrement1$time <- cumsum(c(1, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4))
    eventsIncrement1$sender[1] <- NA_character_
    expect_error(
      link_events(x = networkState1, change_events = eventsIncrement1, nodes = actorsEx),
      "Senders and Receivers must not be NA",
      label = "Senders and Receivers must not be NA"
    )
  }
)

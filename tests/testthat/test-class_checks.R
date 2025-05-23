test_that("compositional change", {
  compositionalEvents <- find_presence(actorsEx)
  expect_s3_class(compositionalEvents, "character")
  expect_equal(compositionalEvents, "compChange")
  
  expect_null(find_presence(data.frame(label = "a", present = TRUE)))
  expect_null(find_presence(
    structure(
      data.frame(label = "a", present = TRUE),
      class = c("nodes.goldfish", "data.frame")
    )
  ))
})

test_that("last presence", {
  expect_equal(
    find_last_presence(1, 3, actorsEx, compChange),
    -1L
  )
  expect_true(find_last_presence(5, 30, actorsEx, compChange))
  expect_false(find_last_presence(5, 27, actorsEx, compChange))
})
test_that("check classes", {
  checks <- check_classes(
    c(1L, 3L), c("character", "numeric", "integer", "factor", "POSIXct")
  )
  expect_vector(checks, ptype = logical(), size = 5)
  expect_equal(checks, c(FALSE, TRUE, TRUE, FALSE, FALSE),
               ignore_attr = "names")
})  

test_that("assign category object", {
  assignment <- assign_category_object(list(
    logical(2), numeric(4), character(6),
    matrix(FALSE, 2, 2), matrix(0L, 1, 1), matrix(0, 2, 2)
  ))
  expect_type(assignment, "character")
  expect_length(assignment, 6)
  expect_false(attr(assignment, "none_class"))
  expect_true(all(attr(assignment, "many_classes") == 1L))
  expect_error(
    assign_category_object(
      list(numeric(4), character(2)),
      classes = c("numeric", "character"),
      category = rep("attribute", 3)
    )
  )
})

test_that("check columns", {
  expect_true(check_columns(
    data.frame(sender = "1", receiver = "2", time = 2),
    mandatory_names = c("sender", "receiver", "time"),
    classes = list(
      sender = c("character", "numeric"),
      receiver = c("character", "numeric"),
      time = c("POSIXct", "numeric"),
      .allow = c("character", "numeric", "logical")
    )
  ))
  expect_error(check_columns(
    data.frame(node = "1", replace = 2),
    mandatory_names = c("node", "time"),
    incompatible_names = c("replace", "increment"),
    classes = list(
      node = c("character", "numeric"),
      time = c("POSIXct", "numeric"),
      increment = "numeric",
      replace = c("logical", "numeric", "character")
    )
  ))
  expect_error(check_columns(
    data.frame(node = "1", replace = 2, time = 2, increment = 1),
    mandatory_names = c("node", "time"),
    incompatible_names = c("replace", "increment"),
    classes = list(
      node = c("character", "numeric"),
      time = c("POSIXct", "numeric"),
      increment = "numeric",
      replace = c("logical", "numeric", "character")
    )
  ))
  expect_error(check_columns(
    data.frame(node = "1", time = 2),
    mandatory_names = c("node", "time"),
    incompatible_names = c("replace", "increment"),
    classes = list(
      node = c("character", "numeric"),
      time = c("POSIXct", "numeric"),
      increment = "numeric",
      replace = c("logical", "numeric", "character")
    )
  ))
  expect_error(check_columns(
    data.frame(node = "1", time = 2, increment = "2"),
    mandatory_names = c("node", "time"),
    incompatible_names = c("replace", "increment"),
    classes = list(
      node = c("character", "numeric"),
      time = c("POSIXct", "numeric"),
      increment = "numeric",
      replace = c("logical", "numeric", "character")
    )
  ))
})

test_that("nodes", {
  expect_error(check_nodes(list(numeric(2))))
  expect_true(check_nodes(actorsEx))
  actorsEx$label[2] <- NA_character_
  expect_error(check_nodes(actorsEx))
  actorsEx$label[2] <- "Actor 1"
  expect_error(check_nodes(actorsEx))
})

test_that("network", {
  expect_error(check_network(
    matrix(0, nrow = 2, ncol = 2)
  ))
  expect_error(check_network(
    structure(
      matrix(0, nrow = 2, ncol = 2),
      class = c("network.goldfish", "matrix", "array")
    )
  ))
  expect_error(check_network(
    structure(
      matrix(0, nrow = 2, ncol = 2),
      class = c("network.goldfish", "matrix", "array"),
      nodes = "actorsEx"
    )
  ))
  expect_true(check_network(
    structure(
      networkState,
      class = c("network.goldfish", "matrix", "array"),
      nodes = "actorsEx", directed = FALSE
    ),
    nodes = actorsEx,
    nodes_name = "actorsEx"
  ))
})

test_that("events nodes", {
  envirTest <- new.env()
  assign("actorsEx", actorsEx, envir = envirTest)
  assign("compChange", compChange, envir = envirTest)
  assign("attrChange", attrChange, envir = envirTest)
  base::local({
    node_object <- structure(
      actorsEx,
      class = c("nodes.goldfish", "data.frame"),
      dynamic_attributes = c("present", "attr1"),
      events = c("compChange", "attrChange")
    )    
  }, envir = envirTest)
  
  expect_error(check_events.nodes.goldfish(
    object = envirTest$node_object,
    events = list(replace = character(2)),
    events_name = "compChange",
    update_column = TRUE,
    environment = envirTest,
    attribute = "present"
  ))
  expect_error(check_events.nodes.goldfish(
    object = structure(data.frame(
      label = "a", present = TRUE, attr1 = 1
    ), class = c("nodes.goldfish", "data.frame")),
    events = envirTest$compChange,,
    events_name = "compChange",
    update_column = TRUE,
    environment = envirTest,
    attribute = "present"
  ))
  expect_error(check_events.nodes.goldfish(
    object = structure(data.frame(
      label = "a", present = TRUE, attr1 = 1
    ), class = c("nodes.goldfish", "data.frame"),
    dynamic_attributes = c("present", NA_character_),
    events = c("compChange", "attrChange")
    ),
    events = envirTest$compChange,,
    events_name = "compChange",
    update_column = TRUE,
    environment = envirTest,
    attribute = "present"
  ))
  expect_error(check_events.nodes.goldfish(
    object = envirTest$node_object,
    events = envirTest$compChange,
    events_name = "compChange",
    update_column = TRUE,
    environment = envirTest,
    attribute = "attr2"
  ))
  expect_error(check_events.nodes.goldfish(
    object = envirTest$node_object,
    events = envirTest$compChange,
    events_name = "compChange2",
    update_column = TRUE,
    environment = envirTest,
    attribute = "present"
  ))
  expect_error(check_events.nodes.goldfish(
    object = envirTest$node_object,
    events = envirTest$compChange,
    events_name = "compChange2",
    update_column = TRUE,
    environment = envirTest,
    attribute = NULL
  ))

  expect_true(check_events.nodes.goldfish(
    object = envirTest$node_object,
    events = envirTest$compChange,
    events_name = "compChange",
    update_column = TRUE,
    environment = envirTest,
    attribute = "present"
  ))
  expect_true(check_events.nodes.goldfish(
    object = envirTest$node_object,
    events = envirTest$attrChange,
    events_name = "attrChange",
    update_column = TRUE,
    environment = envirTest,
    attribute = "attr1"
  ))
})

# test_that("events network", {
#   envirTest <- new.env()
#   assign("actorsEx", actorsEx, envir = envirTest)
#   assign("compChange", compChange, envir = envirTest)
#   assign("networkState", networkState, envir = envirTest)
#   assign("eventsIncrement", eventsIncrement, envir = envirTest)
#   base::local({
#     node_object <- structure(
#       actorsEx,
#       class = c("nodes.goldfish", "data.frame"),
#       dynamic_attributes = c("present"),
#       events = c("compChange")
#     )
#     network_object <- structure(
#       networkState,
#       class = c("network.goldfish", "matrix", "array"),
#       nodes = "node_object",
#       events = c("eventsIncrement")
#     )
#   }, envir = envirTest)
#   
#   expect_error(check_events.network.goldfish(
#     object = envirTest$network_object,
#     events = envirTest$eventsIncrement,
#     events_name = "eventsIncrement",
#     update_column = TRUE,
#     environment = envirTest,
#     nodes = "node_object"
#   ))
# })


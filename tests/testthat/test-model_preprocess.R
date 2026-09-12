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
    "The mean is used to impute"
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
    "The most common value is used to impute"
  )
})

test_that("preprocessing dispatches once, on the descriptor, not per variant", {
  # A method per variant is what this replaced: the recipe choice is a lookup,
  # so one method reads it off the spec and nothing dispatches on the kind.
  # Enumerated from the namespace: `preprocess` is internal with no
  # `S3method()` entry, so `methods()` finds nothing in an installed package
  # and this would pass under load_all() while failing under R CMD check.
  expect_identical(
    grep(
      "^preprocess[.]",
      ls(asNamespace("goldfish"), all.names = TRUE),
      value = TRUE
    ),
    "preprocess.goldfishKind"
  )
})

test_that("every standard variant routes to the merged single-clock walk", {
  # The recipe loops are retired: a standard specification of any model variant
  # now routes through the merged single-clock walk (the one-unit entry), and
  # the sender/dyad shape and the timing that once picked between two loops are
  # decided inside the walk from the same descriptor. This guard is that the
  # descriptor's input shape still reaches the standard substrate for every
  # variant; the grouped shape is covered by the DyNAM-i routing test below.
  specs <- list(
    dynam_rate = dynam_rate_spec(nodes = "actors"),
    dynam_rate_ordered = dynam_rate_ordered_spec(nodes = "actors"),
    dynam_choice = dynam_choice_spec(nodes = "actors"),
    dynam_choice_coord = dynam_choice_coord_spec(nodes = "actors"),
    rem_rate = rem_rate_spec(nodes = "actors"),
    rem_rate_ordered = rem_rate_ordered_spec(nodes = "actors")
  )
  # `preprocess` is an internal generic with no S3method() entry, so its
  # methods resolve only from inside the namespace -- which is where the one
  # caller lives. Dispatching from there is what the package actually does.
  dispatch <- function(...) {
    do.call(preprocess, list(...), envir = asNamespace("goldfish"))
  }
  local_mocked_bindings(
    preprocess_one_unit = function(...) {
      rlang::abort("merged", class = "route_merged")
    },
    run_dynami_monolith = function(...) {
      rlang::abort("grouped", class = "route_grouped")
    }
  )
  for (variant in names(specs)) {
    routed <- tryCatch(
      dispatch(specs[[variant]], recipe_spec = list(), family = "rate"),
      route_merged = function(cnd) "merged",
      route_grouped = function(cnd) "grouped"
    )
    expect_identical(routed, "merged", info = variant)
  }
})

test_that("the grouped input shape routes DyNAM-i to the interaction loop", {
  # DyNAM-i's difference is the shape of its input, not its model variant, so
  # it is the descriptor's `input_shape` that reaches the group loop -- and
  # the ordinal DyNAM-i variant, which `estimate_dynami()` offers no entry
  # point for, is covered here and nowhere else.
  specs <- list(
    dynami_rate = dynami_rate_spec(nodes = "actors"),
    dynami_rate_ordered = dynami_rate_ordered_spec(nodes = "actors"),
    dynami_choice = dynami_choice_spec(nodes = "actors")
  )
  # The interaction loop predates the ordinal split and knows two sub-models,
  # so both sender-indexed variants take its rate path.
  expected <- c(
    dynami_rate = "rate",
    dynami_rate_ordered = "rate",
    dynami_choice = "choice"
  )
  seen <- NULL
  dispatch <- function(...) {
    do.call(preprocess, list(...), envir = asNamespace("goldfish"))
  }
  local_mocked_bindings(
    preprocess_interaction = function(sub_model, ...) {
      seen <<- sub_model
      list()
    }
  )
  for (variant in names(specs)) {
    seen <- NULL
    dispatch(
      specs[[variant]],
      events = NULL,
      effects = NULL,
      events_objects_link = NULL,
      events_effects_link = NULL,
      objects_effects_link = NULL,
      nodes = "actors"
    )
    expect_identical(seen, unname(expected[[variant]]), info = variant)
  }
})

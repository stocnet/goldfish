test_that("choice formula", {
  formStat <- callsDependent ~ inertia + recip(callNetwork, weighted = TRUE) + 
    trans(callNetwork, transformer_fn = log1p) +
    tertius_diff(callNetwork, actors$floor, summarizer_fn = median) +
    recip(callNetwork, window = "5 minutos") +
    indeg(callNetwork, ignore_repetitions = TRUE)
  
  n_terms <- length(labels(terms(formStat)))
  
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local({
    
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors", "actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)
  
  expect_equal(get_dependent_name(formStat), "callsDependent")
  
  rhs_names <- get_rhs_names(formStat)
  expect_type(rhs_names, "list")
  expect_length(rhs_names, n_terms)
  
  rhs_names <- parse_intercept(rhs_names)
  expect_length(rhs_names, 2)
  expect_false(rhs_names[[2]])
  
  
  rhs_names <- parse_time_windows(rhs_names[[1]], envir = envirTest)
  expect_type(rhs_names, "list")
  expect_length(rhs_names, n_terms)
  
  # check window effect change name network argument
  expect_equal(rhs_names[[5]][[2]], "callNetwork_5minutos")
  # check auxiliar data for window effect is created
  expect_contains(ls(envirTest), c("callNetwork_5minutos", "calls_300"))
  
  # rhs_names <- parse_multiple_effects(rhs_names, envir = envirTest)
  
  expect_vector(parsed_formula, ptype = list(), size = 14)
  expect_setequal(
    names(parsed_formula),
    c("rhs_names", "dep_name", "has_intercept", "default_network_name",
      "window_parameters", "ignore_rep_parameter", "weighted_parameter",
      "type_parameter",
      "trans_parameter", "summ_parameter",
      "joining_parameter", "sub_type_parameter",
      "history_parameter", "window_derivations")
  )
  expect_true(which(!sapply(parsed_formula$window_parameters, is.null)) == 5)
  expect_true(which(as.logical(parsed_formula$ignore_rep_parameter)) == 6)
  expect_true(which(as.logical(parsed_formula$weighted_parameter)) == 2)
  expect_true(all(parsed_formula$type_parameter == ""))
  expect_true(which(as.character(parsed_formula$trans_parameter) != "") == 3)
  expect_true(which(as.character(parsed_formula$summ_parameter) != "") == 4)
  expect_true(all(parsed_formula$joining_parameter == ""))
  expect_true(all(parsed_formula$sub_type_parameter == ""))
})

test_that("parse_time_windows records a derivation recipe and gates realization", {
  envirTest <- new.env()
  base::local({
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
        dimnames = list(actors$label, actors$label)
      ),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    calls <- calls
  }, envir = envirTest)
  assign("calls", calls, envir = envirTest)

  rhs_names <- list(list("recip", "callNetwork", window = "300"))

  # realize_windows = FALSE: rewrite + recipe only, no assign into envir.
  rewritten <- parse_time_windows(rhs_names, envir = envirTest, realize_windows = FALSE)
  expect_equal(rewritten[[1]][[2]], "callNetwork_300")
  expect_false(any(c("callNetwork_300", "calls_300") %in% ls(envirTest)))

  derivations <- attr(rewritten, "window_derivations")
  expect_length(derivations, 1)
  expect_equal(derivations[[1]]$derived_name, "callNetwork_300")
  expect_equal(derivations[[1]]$source_name, "callNetwork")
  expect_equal(derivations[[1]]$window, 300)
  expect_equal(derivations[[1]]$kind, "window")

  # realize_windows = TRUE: derived network + dissolve stream materialized.
  parse_time_windows(rhs_names, envir = envirTest, realize_windows = TRUE)
  expect_contains(ls(envirTest), c("callNetwork_300", "calls_300"))
})

test_that("rate formula", {
  formStat <- callsDependent ~ 1 + indeg + outdeg +
    node_trans(callNetwork, transformer_fn = log1p) +
    tertius(callNetwork, actors$floor, summarizer_fn = median) +
    indeg(callNetwork, window = "5 minutos") +
    outdeg(callNetwork, ignore_repetitions = TRUE)
  
  expect_equal(get_dependent_name(formStat), "callsDependent")
  
  rhs_names <- get_rhs_names(formStat)
  expect_type(rhs_names, "list")
  expect_length(rhs_names, length(labels(terms(formStat))) + 1L) # add intercept
  
  rhs_names <- parse_intercept(rhs_names)
  expect_length(rhs_names, 2)
  expect_true(rhs_names[[2]])
})

test_that("get effects functions", {
  formStat <- callsDependent ~ inertia + recip(callNetwork, weighted = TRUE) + 
    trans(callNetwork, transformer_fn = log1p) +
    tertius_diff(callNetwork, actors$floor, summarizer_fn = median) +
    recip(callNetwork, window = "5 minutos") +
    indeg(callNetwork, ignore_repetitions = TRUE)
  
  n_terms <- length(labels(terms(formStat)))
  
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local({
    
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors"), directed = TRUE,
      events = c("calls")
    )
    
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)
  
  effects <- create_effects_functions(
    parsed_formula$rhs_names, "DyNAM", "choice",
    envir = envirTest
  )
  
  expect_type(effects, "list")
  expect_length(effects, n_terms)
  expect_true(all(sapply(effects, names) == c("effect", "initEffect")))
  
})

test_that("unknown effect", {
  formStat <- callsDependent ~ inertia + recip(callNetwork, weighted = TRUE) + 
    trans(callNetwork, transformer_fn = log1p) +
    tertius_diff(callNetwork, actors$floor, summarizer_fn = median) +
    recip(callNetwork, window = "5 minutos") +
    indegree(callNetwork, ignore_repetitions = TRUE)
  
  n_terms <- length(labels(terms(formStat)))
  
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local({
    
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors"), directed = TRUE,
      events = c("calls")
    )
    
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)

  expect_error(
    create_effects_functions(
      parsed_formula$rhs_names, "DyNAM", "choice",
      envir = envirTest
    ),
    "Unknown effect"
  )  
})

test_that("warning two mode", {
  formStat <- callsDependent ~ inertia + 
    trans(callNetwork, transformer_fn = log1p) 
  
  n_terms <- length(labels(terms(formStat)))
  
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local({
    
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors", "actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)
  
  expect_warning(
    create_effects_functions(
      parsed_formula$rhs_names, "DyNAM", "choice",
      envir = envirTest
    ),
    "Setting 'is_two_mode' parameter"
  )  
})

test_that("objects effects link", {
  formStat <- callsDependent ~ inertia + indeg +
    outdeg(networkExog, weighted = TRUE) +
    node_trans(callNetwork, transformer_fn = log1p) +
    tertius(callNetwork, actors$floor, summarizer_fn = median) +
    indeg(callNetwork, window = "5 minutos") +
    outdeg(callNetwork, ignore_repetitions = TRUE)
  
  n_terms <- length(labels(terms(formStat)))
  
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local({
    
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors", "actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
    
    networkExog <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)
  
  objects_effects_link <- get_objects_effects_link(parsed_formula$rhs_names)
  
  expect_true(inherits(objects_effects_link, "array"))
  expect_equal(dim(objects_effects_link), c(4, n_terms))
  expect_equal(
    apply(objects_effects_link, 2, \(x) sum(!is.na(x))),
    c(1, 1, 1, 1, 2, 1, 1),
    ignore_attr = "names"
  )
  
})

test_that("events, objects & effects links", {
  formStat <- callsDependent ~ inertia + indeg +
    outdeg(network_exog, weighted = TRUE) +
    node_trans(callNetwork, transformer_fn = log1p) +
    tertius(callNetwork, actors$floor, summarizer_fn = median) +
    indeg(callNetwork, window = "5 minutos") +
    outdeg(callNetwork, ignore_repetitions = TRUE)
  
  n_terms <- length(labels(terms(formStat)))
  
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local({
    
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors", "actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
    
    network_exog <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)
  
  objects_effects_link <- get_objects_effects_link(parsed_formula$rhs_names)
  events_objects_link <- get_events_and_objects_link(
    parsed_formula$dep_name, parsed_formula$rhs_names,
    "actors", "actors",
    envir = envirTest
  )
  events_effects_link <- get_events_effects_link(
    events_objects_link[[1]], parsed_formula$rhs_names,
    events_objects_link[[2]]
  )
  
  expect_vector(events_objects_link, ptype = list(), size = 2)
  # should be only three elements
  expect_length(events_objects_link[[1]], 4)
  expect_equal(
    names(events_objects_link[[1]]),
    c("callsDependent", "calls", "calls", "calls_300")
  )
  expect_s3_class(events_objects_link[[2]], "data.frame")
  expect_equal(dim(events_objects_link[[2]]), c(4, 5))
  
  expect_true(inherits(events_effects_link, "array"))
  expect_equal(dim(events_effects_link), c(4, n_terms))

})

test_that("window on single attribute effect raises cli error", {
  expect_error(
    estimate_wrapper(
      depNetwork ~ alter(actorsEx$attr1, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "not supported for attribute effects",
    label = "header mentions attribute effects"
  )
  expect_error(
    estimate_wrapper(
      depNetwork ~ alter(actorsEx$attr1, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "alter.*must not have",
    label = "violation names the effect"
  )
  expect_error(
    estimate_wrapper(
      depNetwork ~ alter(actorsEx$attr1, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "actorsEx\\$attr1",
    label = "violation names the attribute reference"
  )
})

test_that("window on list attribute effect raises cli error", {
  expect_error(
    estimate_wrapper(
      depNetwork ~ ego_alter_interaction(
        list(actorsEx$attr1, actorsEx$attr1),
        window = 5
      ),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "not supported for attribute effects",
    label = "header fires for list attribute case"
  )
  expect_error(
    estimate_wrapper(
      depNetwork ~ ego_alter_interaction(
        list(actorsEx$attr1, actorsEx$attr1),
        window = 5
      ),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "ego_alter_interaction.*must not have",
    label = "violation names the effect for list attribute case"
  )
})

test_that("multiple attribute+window violations are all reported in one error", {
  err <- tryCatch(
    estimate_wrapper(
      depNetwork ~ alter(actorsEx$attr1, window = 5) +
        same(actorsEx$attr1, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, "alter.*must not have", label = "first violation is reported")
  expect_match(msg, "same.*must not have", label = "second violation is reported")
})

test_that("window on network effect does not raise attribute error", {
  expect_no_error(
    estimate_wrapper(
      depNetworkTrans ~ trans(networkStateTrans, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    message = "network windowed effects must not raise the attribute error"
  )
})

test_that("get_events_and_objects_link handles global.goldfish without error", {
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)

  season_changes <- data.frame(time = c(30), replace = c(0))
  attr(season_changes, "replace") <- "replace"

  base::local({
    callNetwork <- structure(
      matrix(0, nrow(actors), nrow(actors),
             dimnames = list(actors$label, actors$label)),
      class = c("network.goldfish", "matrix", "array"),
      nodes = c("actors", "actors"), directed = TRUE,
      events = c("calls")
    )
    callsDependent <- structure(
      calls,
      class = c("dependent.goldfish", "data.frame"),
      nodes = c("actors", "actors"), events = c("calls"),
      default_network = "callNetwork", type = "dyadic"
    )
    seasons <- make_global_attributes(data.frame(winter = 1))
    seasons <- link_events(seasons, season_changes)
  }, envir = envirTest)

  assign("season_changes", season_changes, envir = envirTest)

  formStat <- callsDependent ~ indeg(callNetwork) + global(seasons$winter)
  parsed_formula <- parse_formula(formStat, envir = envirTest)

  expect_no_error(
    get_events_and_objects_link(
      parsed_formula$dep_name,
      parsed_formula$rhs_names,
      "actors", "actors",
      envir = envirTest
    )
  )
  result <- get_events_and_objects_link(
    parsed_formula$dep_name,
    parsed_formula$rhs_names,
    "actors", "actors",
    envir = envirTest
  )
  expect_true("season_changes" %in% names(result[[1]]))
})


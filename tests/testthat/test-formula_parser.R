test_that("choice formula", {
  formStat <- callsDependent ~ inertia + recip(callNetwork, weighted = TRUE) + 
    trans(callNetwork, transformFun = log1p) +
    tertius_diff(callNetwork, actors$floor, aggregateFun = median) +
    recip(callNetwork, window = "5 minutos") +
    indeg(callNetwork, ignoreRep = TRUE)
  
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
      defaultNetwork = "callNetwork", type = "dyadic"
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
  
  expect_vector(parsed_formula, ptype = list(), size = 12)
  expect_setequal(
    names(parsed_formula),
    c("rhs_names", "dep_name", "has_intercept", "default_network_name",
      "window_parameters", "ignore_rep_parameter", "weighted_parameter",
      "type_parameter",
      "trans_parameter", "aggre_parameter",
      "joining_parameter", "sub_type_parameter")
  )
  expect_true(which(!sapply(parsed_formula$window_parameters, is.null)) == 5)
  expect_true(which(as.logical(parsed_formula$ignore_rep_parameter)) == 6)
  expect_true(which(as.logical(parsed_formula$weighted_parameter)) == 2)
  expect_true(all(parsed_formula$type_parameter == ""))
  expect_true(which(as.character(parsed_formula$trans_parameter) != "") == 3)
  expect_true(which(as.character(parsed_formula$aggre_parameter) != "") == 4)
  expect_true(all(parsed_formula$joining_parameter == ""))
  expect_true(all(parsed_formula$sub_type_parameter == ""))
})

test_that("rate formula", {
  formStat <- callsDependent ~ 1 + indeg + outdeg + 
    nodeTrans(callNetwork, transformFun = log1p) +
    tertius(callNetwork, actors$floor, aggregateFun = median) +
    indeg(callNetwork, window = "5 minutos") +
    outdeg(callNetwork, ignoreRep = TRUE)
  
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
    trans(callNetwork, transformFun = log1p) +
    tertiusDiff(callNetwork, actors$floor, aggregateFun = median) +
    recip(callNetwork, window = "5 minutos") +
    indeg(callNetwork, ignoreRep = TRUE)
  
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
      defaultNetwork = "callNetwork", type = "dyadic"
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
    trans(callNetwork, transformFun = log1p) +
    tertius_diff(callNetwork, actors$floor, aggregateFun = median) +
    recip(callNetwork, window = "5 minutos") +
    indeg(callNetwork, ignoreRep = TRUE)
  
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
      defaultNetwork = "callNetwork", type = "dyadic"
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
    trans(callNetwork, transformFun = log1p) 
  
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
      defaultNetwork = "callNetwork", type = "dyadic"
    )
  }, envir = envirTest)
  
  parsed_formula <- parse_formula(formStat, envir = envirTest)
  
  expect_warning(
    create_effects_functions(
      parsed_formula$rhs_names, "DyNAM", "choice",
      envir = envirTest
    ),
    "Setting 'isTwoMode' parameter"
  )  
})

test_that("objects effects link", {
  formStat <- callsDependent ~ inertia + indeg +
    outdeg(networkExog, weighted = TRUE) +
    nodeTrans(callNetwork, transformFun = log1p) +
    tertius(callNetwork, actors$floor, aggregateFun = median) +
    indeg(callNetwork, window = "5 minutos") +
    outdeg(callNetwork, ignoreRep = TRUE)
  
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
      defaultNetwork = "callNetwork", type = "dyadic"
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
  expect_dim(objects_effects_link, c(4, n_terms))
  expect_equal(
    apply(objects_effects_link, 2, \(x) sum(!is.na(x))),
    c(1, 1, 1, 1, 2, 1, 1),
    ignore_attr = "names"
  )
  
})

test_that("events, objects & effects links", {
  formStat <- callsDependent ~ inertia + indeg +
    outdeg(networkExog, weighted = TRUE) +
    nodeTrans(callNetwork, transformFun = log1p) +
    tertius(callNetwork, actors$floor, aggregateFun = median) +
    indeg(callNetwork, window = "5 minutos") +
    outdeg(callNetwork, ignoreRep = TRUE)
  
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
      defaultNetwork = "callNetwork", type = "dyadic"
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
  events_objects_link <- get_events_and_objects_link(
    parsed_formula$dep_name, parsed_formula$rhs_names,
    "actors", "actors"
  )
  events_effects_link <- get_events_effects_link(
    events_objects_link[[1]], parsed_formula$rhs_names,
    events_objects_link[[2]]
  )
  
  expect_vector(events_objects_link, ptype = list(), size = 2)
  expect_length(events_objects_link[[1]], 3)
  expect_s3_class(events_objects_link[[2]], "data.frame")
  expect_equal(dim(events_objects_link[[2]]), c(3, 5))
  
  expect_true(inherits(events_effects_link, "array"))
  expect_equal(dim(events_effects_link), c(3, n_terms))
  
})


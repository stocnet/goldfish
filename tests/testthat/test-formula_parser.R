test_that("choice formula", {
  formStat <- calls_dependent ~ inertia +
    recip(call_network, weighted = TRUE) +
    trans(call_network, transformer_fn = log1p) +
    tertius_diff(call_network, actors$floor, summarizer_fn = median) +
    recip(call_network, window = "5 minutos") +
    indeg(call_network, ignore_repetitions = TRUE)

  n_terms <- length(labels(terms(formStat)))

  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )

      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed_formula <- parse_formula(formStat, envir = envirTest)

  expect_equal(get_dependent_name(formStat), "calls_dependent")

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
  expect_equal(rhs_names[[5]][[2]], "call_network_5minutos")
  # check auxiliar data for window effect is created
  expect_contains(ls(envirTest), c("call_network_5minutos", "calls_300"))

  # rhs_names <- parse_multiple_effects(rhs_names, envir = envirTest)

  expect_vector(parsed_formula, ptype = list(), size = 19)
  expect_setequal(
    names(parsed_formula),
    c(
      "rhs_names",
      "dep_name",
      "has_intercept",
      "default_network_name",
      "window_parameters",
      "ignore_rep_parameter",
      "weighted_parameter",
      "type_parameter",
      "trans_parameter",
      "summ_parameter",
      "joining_parameter",
      "sub_type_parameter",
      "history_parameter",
      "offset_parameter",
      "is_main_parameter",
      "is_operand_parameter",
      "estimate_parameter",
      "interactions",
      "window_derivations"
    )
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
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls <- calls
    },
    envir = envirTest
  )
  assign("calls", calls, envir = envirTest)

  rhs_names <- list(list("recip", "call_network", window = "300"))

  # realize_windows = FALSE: rewrite + recipe only, no assign into envir.
  rewritten <- parse_time_windows(
    rhs_names,
    envir = envirTest,
    realize_windows = FALSE
  )
  expect_equal(rewritten[[1]][[2]], "call_network_300")
  expect_false(any(c("call_network_300", "calls_300") %in% ls(envirTest)))

  derivations <- attr(rewritten, "window_derivations")
  expect_length(derivations, 1)
  expect_equal(derivations[[1]]$derived_name, "call_network_300")
  expect_equal(derivations[[1]]$source_name, "call_network")
  expect_equal(derivations[[1]]$window, 300)
  expect_equal(derivations[[1]]$kind, "window")

  # realize_windows = TRUE: derived network + dissolve stream materialized.
  parse_time_windows(rhs_names, envir = envirTest, realize_windows = TRUE)
  expect_contains(ls(envirTest), c("call_network_300", "calls_300"))
})

test_that("build_derivations builds the derived-input registry from metadata", {
  envirTest <- new.env()
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
    },
    envir = envirTest
  )

  window_derivations <- list(
    list(
      derived_name = "call_network_300",
      source_name = "call_network",
      window = 300,
      kind = "window"
    )
  )
  objects_effects_link <- matrix(
    c(NA, NA, NA, 1),
    nrow = 2,
    dimnames = list(
      c("call_network", "call_network_300"),
      c("inertia", "recip")
    )
  )

  derivations <- build_derivations(
    window_derivations,
    objects_effects_link,
    envir = envirTest
  )
  expect_length(derivations, 1)
  expect_equal(derivations[[1]]$derived_name, "call_network_300")
  expect_equal(derivations[[1]]$kind, "window")
  expect_equal(derivations[[1]]$source, "call_network")
  expect_equal(derivations[[1]]$source_streams, "calls")
  expect_equal(derivations[[1]]$params$window, 300)
  expect_equal(derivations[[1]]$gids, 2L)

  expect_null(build_derivations(
    list(),
    objects_effects_link,
    envir = envirTest
  ))
})

test_that("parse_formula(realize_windows = FALSE) leaves the env unmutated", {
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  before <- ls(envirTest)
  parsed <- parse_formula(
    calls_dependent ~ inertia + recip(call_network, window = 300),
    envir = envirTest,
    realize_windows = FALSE
  )
  # the shared parser did not fabricate the windowed network/dissolve stream ...
  expect_setequal(ls(envirTest), before)
  expect_equal(parsed$rhs_names[[2]][[2]], "call_network_300")
  expect_length(parsed$window_derivations, 1)

  # ... and the registry-driven realizer materializes them on demand.
  realize_windows_recipe(parsed$window_derivations, envirTest)
  expect_contains(ls(envirTest), c("call_network_300", "calls_300"))
})

test_that("create_effects_functions resolves the two-mode guard from source", {
  # A windowed effect on a two-mode network: with the derived network left
  # unrealized (recipe path), the two-mode guard must still read the nodesets
  # from the *source* network via the derivation recipe.
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed <- parse_formula(
    calls_dependent ~ trans(call_network, window = 300),
    envir = envirTest,
    realize_windows = FALSE
  )
  # derived network is NOT realized: the guard cannot get() it.
  expect_false("call_network_300" %in% ls(envirTest))

  # Injecting the value the data implies is the normal path, not a user error,
  # so it is silent; only a declared value that disagrees warns.
  expect_no_warning(
    effects <- create_effects_functions(
      parsed$rhs_names,
      "DyNAM",
      "choice",
      envir = envirTest,
      derivations = parsed$window_derivations
    )
  )
  # the guard resolved is_two_mode = TRUE from the source's nodesets.
  expect_true(eval(formals(effects[[1]]$effect)[["is_two_mode"]]))
  # and it did not fabricate the derived network as a side effect.
  expect_false("call_network_300" %in% ls(envirTest))
})

test_that("build_object_keys classifies an unrealized derived network from the recipe", {
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed <- parse_formula(
    calls_dependent ~ inertia + recip(call_network, window = 300),
    envir = envirTest,
    realize_windows = FALSE
  )
  objects_effects_link <- get_objects_effects_link(parsed$rhs_names)

  keys <- build_object_keys(
    rownames(objects_effects_link),
    "actors",
    "actors",
    envir = envirTest,
    derivations = parsed$window_derivations
  )
  derived_row <- keys[keys$name == "call_network_300", ]
  expect_equal(derived_row$component, "networks")
  expect_equal(derived_row$key, "call_network_300")
  # classification read the recipe only, not the (absent) derived network.
  expect_false("call_network_300" %in% ls(envirTest))
})

test_that("build_events_objects_link resolves derived streams from source", {
  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed <- parse_formula(
    calls_dependent ~ recip(call_network, window = 300),
    envir = envirTest,
    realize_windows = FALSE
  )
  link <- build_events_objects_link(
    parsed$dep_name,
    parsed$rhs_names,
    "actors",
    "actors",
    envir = envirTest,
    derivations = parsed$window_derivations
  )
  # the derived dissolve stream name is reconstructed from the source
  # (paste(source stream, window, sep = "_")) without realizing anything.
  expect_true("calls_300" %in% link$events_objects_link$events)
  stream_names <- vapply(link$fetch_plan, `[[`, character(1), "stream")
  expect_true("calls_300" %in% stream_names)
  expect_false("calls_300" %in% ls(envirTest))
})

test_that("rate formula", {
  formStat <- calls_dependent ~ 1 +
    indeg +
    outdeg +
    node_trans(call_network, transformer_fn = log1p) +
    tertius(call_network, actors$floor, summarizer_fn = median) +
    indeg(call_network, window = "5 minutos") +
    outdeg(call_network, ignore_repetitions = TRUE)

  expect_equal(get_dependent_name(formStat), "calls_dependent")

  rhs_names <- get_rhs_names(formStat)
  expect_type(rhs_names, "list")
  expect_length(rhs_names, length(labels(terms(formStat))) + 1L) # add intercept

  rhs_names <- parse_intercept(rhs_names)
  expect_length(rhs_names, 2)
  expect_true(rhs_names[[2]])
})

test_that("get effects functions", {
  formStat <- calls_dependent ~ inertia +
    recip(call_network, weighted = TRUE) +
    trans(call_network, transformer_fn = log1p) +
    tertius_diff(call_network, actors$floor, summarizer_fn = median) +
    recip(call_network, window = "5 minutos") +
    indeg(call_network, ignore_repetitions = TRUE)

  n_terms <- length(labels(terms(formStat)))

  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors"),
        directed = TRUE,
        events = c("calls")
      )

      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed_formula <- parse_formula(formStat, envir = envirTest)

  effects <- create_effects_functions(
    parsed_formula$rhs_names,
    "DyNAM",
    "choice",
    envir = envirTest
  )

  expect_type(effects, "list")
  expect_length(effects, n_terms)
  expect_true(all(sapply(effects, names) == c("effect", "init_effect")))
})

test_that("unknown effect", {
  formStat <- calls_dependent ~ inertia +
    recip(call_network, weighted = TRUE) +
    trans(call_network, transformer_fn = log1p) +
    tertius_diff(call_network, actors$floor, summarizer_fn = median) +
    recip(call_network, window = "5 minutos") +
    indegree(call_network, ignore_repetitions = TRUE)

  n_terms <- length(labels(terms(formStat)))

  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors"),
        directed = TRUE,
        events = c("calls")
      )

      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed_formula <- parse_formula(formStat, envir = envirTest)

  expect_error(
    create_effects_functions(
      parsed_formula$rhs_names,
      "DyNAM",
      "choice",
      envir = envirTest
    ),
    "Unknown effect"
  )
})

test_that("a declared is_two_mode disagreeing with the data warns", {
  # The mode map is the source of truth: a declared value that contradicts it
  # is a mistake about the data, so the data's reading wins and the user is
  # told. (Injecting the derived value when nothing is declared is silent.)
  formStat <- calls_dependent ~ inertia +
    trans(call_network, transformer_fn = log1p, is_two_mode = FALSE)

  n_terms <- length(labels(terms(formStat)))

  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )

      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envirTest
  )

  parsed_formula <- parse_formula(formStat, envir = envirTest)

  expect_warning(
    effects <- create_effects_functions(
      parsed_formula$rhs_names,
      "DyNAM",
      "choice",
      envir = envirTest
    ),
    "disagrees with the data"
  )
  expect_true(
    eval(formals(effects[[2]]$effect)[["is_two_mode"]]),
    label = "the mode map's reading wins over the declared FALSE"
  )
})

test_that("objects effects link", {
  formStat <- calls_dependent ~ inertia +
    indeg +
    outdeg(networkExog, weighted = TRUE) +
    node_trans(call_network, transformer_fn = log1p) +
    tertius(call_network, actors$floor, summarizer_fn = median) +
    indeg(call_network, window = "5 minutos") +
    outdeg(call_network, ignore_repetitions = TRUE)

  n_terms <- length(labels(terms(formStat)))

  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )

      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )

      networkExog <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
    },
    envir = envirTest
  )

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
  formStat <- calls_dependent ~ inertia +
    indeg +
    outdeg(network_exog, weighted = TRUE) +
    node_trans(call_network, transformer_fn = log1p) +
    tertius(call_network, actors$floor, summarizer_fn = median) +
    indeg(call_network, window = "5 minutos") +
    outdeg(call_network, ignore_repetitions = TRUE)

  n_terms <- length(labels(terms(formStat)))

  envirTest <- new.env()
  assign("actors", actors, envir = envirTest)
  assign("calls", calls, envir = envirTest)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )

      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )

      network_exog <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
    },
    envir = envirTest
  )

  parsed_formula <- parse_formula(formStat, envir = envirTest)

  objects_effects_link <- get_objects_effects_link(parsed_formula$rhs_names)
  events_objects_link <- get_events_and_objects_link(
    parsed_formula$dep_name,
    parsed_formula$rhs_names,
    "actors",
    "actors",
    envir = envirTest
  )
  events_effects_link <- get_events_effects_link(
    parsed_formula$rhs_names,
    events_objects_link[[2]]
  )

  expect_vector(events_objects_link, ptype = list(), size = 2)
  # should be only three elements
  expect_length(events_objects_link[[1]], 4)
  expect_equal(
    names(events_objects_link[[1]]),
    c("calls_dependent", "calls", "calls", "calls_300")
  )
  expect_s3_class(events_objects_link[[2]], "data.frame")
  expect_equal(dim(events_objects_link[[2]]), c(4, 5))

  expect_true(inherits(events_effects_link, "array"))
  expect_equal(dim(events_effects_link), c(4, n_terms))
})

test_that("window on single attribute effect raises cli error", {
  expect_error(
    estimate_wrapper(
      depNetwork ~ alter(actors_ex$attr1, window = 5),
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
      depNetwork ~ alter(actors_ex$attr1, window = 5),
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
      depNetwork ~ alter(actors_ex$attr1, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "attr1",
    label = "violation names the attribute reference"
  )
})

test_that("window on list attribute effect raises cli error", {
  expect_error(
    estimate_wrapper(
      depNetwork ~ ego_alter_interaction(
        list(actors_ex$attr1, actors_ex$attr1),
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
        list(actors_ex$attr1, actors_ex$attr1),
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
      depNetwork ~ alter(actors_ex$attr1, window = 5) +
        same(actors_ex$attr1, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "alter.*must not have",
    label = "first violation is reported"
  )
  expect_match(
    msg,
    "same.*must not have",
    label = "second violation is reported"
  )
})

test_that("window on network effect does not raise attribute error", {
  expect_no_error(
    estimate_wrapper(
      depNetworkTrans ~ trans(networkStateTrans, window = 5),
      model = "DyNAM",
      sub_model = "choice",
      data = dataTrans,
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

  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
      seasons <- make_global_attributes(data.frame(winter = 1))
      seasons <- link_events(seasons, season_changes)
    },
    envir = envirTest
  )

  assign("season_changes", season_changes, envir = envirTest)

  formStat <- calls_dependent ~ indeg(call_network) + global(seasons$winter)
  parsed_formula <- parse_formula(formStat, envir = envirTest)

  expect_no_error(
    get_events_and_objects_link(
      parsed_formula$dep_name,
      parsed_formula$rhs_names,
      "actors",
      "actors",
      envir = envirTest
    )
  )
  result <- get_events_and_objects_link(
    parsed_formula$dep_name,
    parsed_formula$rhs_names,
    "actors",
    "actors",
    envir = envirTest
  )
  expect_true("season_changes" %in% names(result[[1]]))
})

test_that("two-modeness resolves per network argument, not from the focal", {
  # D4's per-argument rule: each network argument answers from its OWN layer.
  # On a multipartite object the focal `attend` is two-mode, yet the one-mode
  # covariate `coauthor` must still read FALSE.
  d <- as_goldfish(make_stocnet_fixture_multipartite())
  parsed <- parse_formula(
    attend ~ indeg(coauthor) + indeg(member),
    data = d
  )
  effects <- create_effects_functions(
    parsed$rhs_names,
    "DyNAM",
    "choice",
    data = d
  )

  expect_false(
    eval(formals(effects[[1]]$effect)[["is_two_mode"]]),
    label = "coauthor is actor -> actor"
  )
  expect_true(
    eval(formals(effects[[2]]$effect)[["is_two_mode"]]),
    label = "member is actor -> org"
  )
})

test_that("the mismatch warning names the layer's actual mode pair", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())
  parsed <- parse_formula(
    attend ~ indeg(member, is_two_mode = FALSE),
    data = d
  )

  expect_warning(
    effects <- create_effects_functions(
      parsed$rhs_names,
      "DyNAM",
      "choice",
      data = d
    ),
    "sends from mode .actor. to mode .org."
  )
  expect_true(
    eval(formals(effects[[1]]$effect)[["is_two_mode"]]),
    label = "the mode map's reading wins"
  )
})

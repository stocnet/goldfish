baselines_engines <- c("default", "default_c")

baselines_social_evolution_data <- function() {
  data("Social_Evolution", envir = environment())
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(
    x = callNetwork,
    change_event = calls,
    nodes = actors
  )
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
  friendshipNetwork <- link_events(
    x = friendshipNetwork,
    change_event = friendship,
    nodes = actors
  )
  callsDependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = callNetwork
  )
  make_data(
    callsDependent,
    callNetwork,
    friendshipNetwork,
    calls,
    friendship,
    actors
  )
}

baselines_fisheries_data <- function() {
  data("Fisheries_Treaties_6070", envir = environment())
  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  states <- link_events(states, gdpchanges, attribute = "gdp")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE)
  contignet <- link_events(contignet, contigchanges, nodes = states)
  createBilat <- make_dependent_events(
    events = bilatchanges[bilatchanges$increment == 1, ],
    nodes = states,
    default_network = bilatnet
  )
  make_data(
    createBilat,
    bilatnet,
    contignet,
    states,
    bilatchanges,
    contigchanges,
    sovchanges,
    regchanges,
    gdpchanges
  )
}

baselines_global_data <- function() {
  data("Social_Evolution", envir = environment())
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(
    x = callNetwork,
    change_event = calls,
    nodes = actors
  )
  callsDependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = callNetwork
  )
  seasons <- make_global_attributes(data.frame(winter = 0))
  seasonChange <- data.frame(time = 1222553311, replace = 1)
  seasons <- link_events(seasons, seasonChange)
  make_data(callsDependent, callNetwork, calls, actors, seasons)
}

baselines_model_grid <- function() {
  list(
    se_dynam_rate = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "rate",
      formula = callsDependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork)
    ),
    se_dynam_rate_ordered = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "rate_ordered",
      formula = callsDependent ~ indeg + outdeg + indeg(friendshipNetwork)
    ),
    se_dynam_choice = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "choice",
      formula = callsDependent ~ inertia + recip + trans
    ),
    se_dynam_choice_coord = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "choice_coordination",
      formula = callsDependent ~ inertia + trans
    ),
    se_rem = list(
      dataset = "social_evolution",
      model = "REM",
      formula = callsDependent ~ 1 +
        indeg(callNetwork, type = "ego") +
        inertia +
        recip
    ),
    se_rem_ordered = list(
      dataset = "social_evolution",
      model = "REM",
      formula = callsDependent ~ indeg(callNetwork, type = "ego") +
        inertia +
        recip
    ),
    fish_dynam_rate = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "rate",
      formula = createBilat ~ 1 + indeg + ego(states$regime)
    ),
    fish_dynam_rate_ordered = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "rate_ordered",
      formula = createBilat ~ indeg + ego(states$regime)
    ),
    fish_dynam_choice = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "choice",
      formula = createBilat ~ inertia +
        tie(contignet) +
        alter(states$regime) +
        diff(states$regime)
    ),
    fish_dynam_choice_coord = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "choice_coordination",
      formula = createBilat ~ inertia +
        tie(contignet) +
        alter(states$regime) +
        diff(states$regime),
      estimation_args = list(initial_damping = 40, max_iterations = 30)
    ),
    fish_rem = list(
      dataset = "fisheries",
      model = "REM",
      formula = createBilat ~ 1 +
        inertia +
        tie(contignet) +
        alter(states$regime)
    ),
    fish_rem_ordered = list(
      dataset = "fisheries",
      model = "REM",
      formula = createBilat ~ inertia + tie(contignet) + alter(states$regime)
    )
  )
}

baselines_global_model_grid <- function() {
  list(
    global_dynam_rate = list(
      dataset = "social_evolution_global",
      model = "DyNAM",
      sub_model = "rate",
      formula = callsDependent ~ 1 + indeg + global(seasons$winter)
    ),
    global_rem = list(
      dataset = "social_evolution_global",
      model = "REM",
      formula = callsDependent ~ 1 + inertia + global(seasons$winter)
    )
  )
}

baselines_fit <- function(spec, engine, data_list) {
  controlArgs <- c(list(engine = engine), spec$estimation_args)
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_estimation = do.call(set_estimation_opt, controlArgs),
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    do.call(estimate_dynam, args)
  } else {
    do.call(estimate_rem, args)
  }
}

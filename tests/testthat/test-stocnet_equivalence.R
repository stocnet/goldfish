# Coefficient equivalence between the two input paths.
#
# The same model, expressed once through the legacy constructors and once as a
# stocnet object, must estimate to the same coefficients. These are hand-built
# stocnets (no manynet call) so they also pin the "reads structure, not class
# provenance" contract of the boundary.

# Social Evolution as a stocnet: one `calls` layer, no history, nodes carrying
# the actor attributes under their stocnet names (`present` -> `active`).
social_evolution_stocnet <- function(events, actors) {
  list(
    info = list(
      name = "socev",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = actors$label,
      active = actors$present,
      floor = actors$floor,
      gradeType = actors$gradeType,
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = match(events$sender, actors$label),
      to = match(events$receiver, actors$label),
      time = events$time,
      layer = "calls",
      stringsAsFactors = FALSE
    )
  )
}

# The legacy equivalent: an empty network the call events increment. The local
# names become the object names -- make_data() deparses its arguments -- so they
# are the ones the formulas below reference.
social_evolution_legacy <- function(events, actors) {
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = events,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = events,
    nodes = actors,
    default_network = call_network
  )
  make_data(calls_dependent, call_network, events, actors)
}

test_that("a stocnet choice model matches the legacy path", {
  skip_on_cran()
  data("Social_Evolution", package = "goldfish", envir = environment())
  events <- calls[1:150, ]

  legacy <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution_legacy(events, actors)
  )
  data_stocnet <- social_evolution_stocnet(events, actors)
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    data = data_stocnet
  )
  stocnet <- estimate_dynam(spec, sub_model = "choice", data = data_stocnet)

  expect_equal(unname(coef(stocnet)), unname(coef(legacy)), tolerance = 1e-6)
})

test_that("bare attribute names match the legacy df$var path", {
  skip_on_cran()
  data("Social_Evolution", package = "goldfish", envir = environment())
  events <- calls[1:150, ]

  # Both paths impute the same missing attribute values, and both warn about it.
  legacy <- suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + alter(actors$floor) + same(actors$gradeType),
    sub_model = "choice",
    data = social_evolution_legacy(events, actors)
  ))
  data_stocnet <- social_evolution_stocnet(events, actors)
  spec <- make_specification(
    choice = ~ inertia + alter(floor) + same(gradeType),
    model = "DyNAM",
    choice_sub_model = "choice",
    data = data_stocnet
  )
  stocnet <- suppressWarnings(
    estimate_dynam(spec, sub_model = "choice", data = data_stocnet)
  )

  expect_equal(unname(coef(stocnet)), unname(coef(legacy)), tolerance = 1e-6)
})

test_that("a stocnet rate model with an intercept matches the legacy path", {
  skip_on_cran()
  data("Social_Evolution", package = "goldfish", envir = environment())
  events <- calls[1:150, ]

  legacy <- estimate_dynam(
    calls_dependent ~ 1 + indeg + ego(actors$floor),
    sub_model = "rate",
    data = social_evolution_legacy(events, actors)
  )
  data_stocnet <- social_evolution_stocnet(events, actors)
  spec <- make_specification(
    rate = ~ 1 + indeg + ego(floor),
    model = "DyNAM",
    rate_sub_model = "rate",
    data = data_stocnet
  )
  stocnet <- estimate_dynam(spec, sub_model = "rate", data = data_stocnet)

  expect_equal(unname(coef(stocnet)), unname(coef(legacy)), tolerance = 1e-6)
})

test_that("the initial state is the layer's history alone", {
  # The walk replays every timed event from the start of the schedule, so
  # folding timed rows into the initial state would count them twice.
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())
  history <- which(bilatnet != 0 & upper.tri(bilatnet), arr.ind = TRUE)
  data_stocnet <- list(
    info = list(
      name = "fish",
      focal = "treaties",
      update = c(treaties = "increment"),
      directed = c(treaties = FALSE),
      observation = c(treaties = "event")
    ),
    nodes = data.frame(label = states$label, stringsAsFactors = FALSE),
    ties = rbind(
      data.frame(
        from = history[, 1],
        to = history[, 2],
        time = as.POSIXct(NA),
        layer = "treaties",
        weight = bilatnet[history],
        stringsAsFactors = FALSE
      ),
      data.frame(
        from = match(bilatchanges$sender, states$label),
        to = match(bilatchanges$receiver, states$label),
        time = bilatchanges$time,
        layer = "treaties",
        weight = bilatchanges$increment,
        stringsAsFactors = FALSE
      )
    )
  )
  src <- new_data_source(data = data_stocnet, focal = "treaties")

  expect_equal(
    unname(ds_network(src, "treaties")),
    unname(bilatnet * 1),
    label = "an undirected layer's history rebuilds the constructor's matrix"
  )
})

test_that("a keyed flavor reproduces the legacy filtered dependent events", {
  skip_on_cran()
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())

  # Legacy: the network sees every change, but only the creations are modeled.
  network <- make_network(bilatnet, nodes = states, directed = FALSE)
  network <- link_events(network, bilatchanges, nodes = states)
  create_bilat <- make_dependent_events(
    events = bilatchanges[bilatchanges$increment == 1, ],
    nodes = states,
    default_network = network
  )
  legacy <- suppressWarnings(estimate_dynam(
    create_bilat ~ inertia + trans + alter(states$regime),
    sub_model = "choice_coordination",
    data = make_data(create_bilat, network, bilatchanges, states)
  ))

  # stocnet: one layer whose flavor carries the same distinction.
  history <- which(bilatnet != 0 & upper.tri(bilatnet), arr.ind = TRUE)
  data_stocnet <- list(
    info = list(
      name = "fish",
      focal = "treaties",
      update = c(treaties = "increment"),
      directed = c(treaties = FALSE),
      observation = c(treaties = "event")
    ),
    nodes = data.frame(
      label = states$label,
      active = states$present,
      regime = states$regime,
      gdp = states$gdp,
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = history[, 1],
        to = history[, 2],
        time = as.POSIXct(NA),
        layer = "treaties",
        weight = bilatnet[history],
        flavor = NA_character_,
        stringsAsFactors = FALSE
      ),
      data.frame(
        from = match(bilatchanges$sender, states$label),
        to = match(bilatchanges$receiver, states$label),
        time = bilatchanges$time,
        layer = "treaties",
        weight = bilatchanges$increment,
        flavor = ifelse(bilatchanges$increment == 1, "creation", "dissolution"),
        stringsAsFactors = FALSE
      )
    )
  )
  spec <- make_specification(
    choice = list(creation ~ inertia + trans + alter(regime)),
    model = "DyNAM",
    choice_sub_model = "choice_coordination",
    data = data_stocnet
  )
  stocnet <- suppressWarnings(
    estimate_dynam(spec, sub_model = "choice_coordination", data = data_stocnet)
  )

  expect_equal(
    spec$dependent$n_events,
    nrow(create_bilat),
    label = "only the creations are modeled"
  )
  expect_equal(unname(coef(stocnet)), unname(coef(legacy)), tolerance = 1e-6)
})

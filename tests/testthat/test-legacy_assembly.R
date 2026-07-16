# The legacy constructors' fragments, gathered and assembled into a single
# stocnet, must estimate identically to the legacy path. This pins the assembler
# shape (layer = object name, history from the matrix, flavor stamping) before
# make_data() flips to return the assembled object.

test_that("assembled Social Evolution matches the legacy path", {
  skip_on_cran()
  data("Social_Evolution", package = "goldfish", envir = environment())
  events <- calls[1:150, ]

  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, events, nodes = actors)
  calls_dependent <- make_dependent_events(
    events = events,
    nodes = actors,
    default_network = call_network
  )
  legacy <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = make_data(calls_dependent, call_network, events, actors)
  )

  assembled <- assemble_stocnet_from_legacy(list(
    call_network = call_network,
    calls_dependent = calls_dependent,
    events = events,
    actors = actors
  ))
  # No subset -> the focal layer models all its rows, no flavor column.
  expect_false("flavor" %in% names(assembled$ties))
  expect_equal(assembled$info$focal, "call_network")

  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    data = assembled
  )
  stocnet <- estimate_dynam(spec, sub_model = "choice", data = assembled)
  expect_equal(unname(coef(stocnet)), unname(coef(legacy)), tolerance = 1e-6)
})

test_that("assembled Fisheries reproduces the filtered dependent via flavor", {
  skip_on_cran()
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())

  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  states <- link_events(states, gdpchanges, attribute = "gdp")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE)
  contignet <- link_events(contignet, contigchanges, nodes = states)
  create_bilat <- make_dependent_events(
    events = bilatchanges[bilatchanges$increment == 1, ],
    nodes = states,
    default_network = bilatnet
  )

  legacy <- suppressWarnings(estimate_dynam(
    create_bilat ~ inertia + trans + alter(states$regime),
    sub_model = "choice_coordination",
    data = make_data(
      create_bilat,
      bilatnet,
      contignet,
      states,
      bilatchanges,
      contigchanges,
      sovchanges,
      regchanges,
      gdpchanges
    )
  ))

  assembled <- assemble_stocnet_from_legacy(list(
    create_bilat = create_bilat,
    bilatnet = bilatnet,
    contignet = contignet,
    states = states,
    bilatchanges = bilatchanges,
    contigchanges = contigchanges,
    sovchanges = sovchanges,
    regchanges = regchanges,
    gdpchanges = gdpchanges
  ))
  expect_equal(assembled$info$focal, "bilatnet")
  expect_equal(
    assembled$info$dependents$create_bilat,
    list(layer = "bilatnet", flavor = "create_bilat")
  )
  # Only the creations carry the flavor.
  creations <- sum(assembled$ties$flavor == "create_bilat", na.rm = TRUE)
  expect_equal(creations, nrow(create_bilat))

  spec <- make_specification(
    choice = list(create_bilat ~ inertia + trans + alter(regime)),
    model = "DyNAM",
    choice_sub_model = "choice_coordination",
    data = assembled
  )
  stocnet <- suppressWarnings(
    estimate_dynam(spec, sub_model = "choice_coordination", data = assembled)
  )
  expect_equal(spec$dependent$n_events, nrow(create_bilat))
  expect_equal(unname(coef(stocnet)), unname(coef(legacy)), tolerance = 1e-6)

  # The legacy call surface: the dependent-events object name on the LHS resolves
  # to (focal layer, its flavor) with no make_specification() and no flavor list.
  via_alias <- suppressWarnings(estimate_dynam(
    create_bilat ~ inertia + trans + alter(regime),
    sub_model = "choice_coordination",
    data = assembled
  ))
  expect_equal(unname(coef(via_alias)), unname(coef(legacy)), tolerance = 1e-6)
})

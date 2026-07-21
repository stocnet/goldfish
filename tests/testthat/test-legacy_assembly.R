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

# Two-mode assembly -----------------------------------------------------------

# The legacy bundle names two node-set objects; assembly fuses them onto the one
# representation downstream consumes -- a single nodes tibble whose `mode` names
# the source set, with per-layer mode sets recovering the side pair.
assemble_legacy_twomode <- function() {
  fx <- make_legacy_fixture_twomode()
  assemble_stocnet_from_legacy(list(
    actors = fx$actors,
    clubs = fx$clubs,
    membership = fx$membership,
    joins = fx$joins,
    joins_dependent = fx$joins_dependent,
    actor_growth = fx$actor_growth,
    club_funding = fx$club_funding
  ))
}

test_that("a two-mode legacy bundle is assemblable", {
  fx <- make_legacy_fixture_twomode()
  expect_true(is_stocnet_assemblable(list(
    actors = fx$actors,
    clubs = fx$clubs,
    membership = fx$membership,
    joins_dependent = fx$joins_dependent
  )))
})

test_that("two-mode assembly fuses the node sets and declares the mode sets", {
  assembled <- assemble_legacy_twomode()
  expect_no_error(validate_goldfish_data(assembled))

  expect_equal(
    assembled$nodes$label,
    c("A1", "A2", "A3", "A4", "C1", "C2", "C3")
  )
  expect_equal(assembled$nodes$mode, c(rep("actors", 4), rep("clubs", 3)))
  # Unioned attribute columns: undefined on the other mode, not missing at
  # random.
  expect_equal(assembled$nodes$budget, c(rep(NA, 4), 12, 8, 20))
  expect_equal(assembled$nodes$size, c(3, 1, 2, 4, rep(NA, 3)))

  expect_equal(assembled$info$sender, c(membership = "actors"))
  expect_equal(assembled$info$receiver, c(membership = "clubs"))

  map <- build_mode_map(assembled$info, assembled$nodes, "membership")
  lm <- map$layers[["membership"]]
  expect_true(lm$is_two_mode)
  expect_equal(lm$side1, 1:4)
  expect_equal(lm$side2, 5:7)
})

test_that("two-mode assembly remaps ties and changes into the fused id space", {
  assembled <- assemble_legacy_twomode()
  ties <- as.data.frame(assembled$ties)

  # Receivers are club labels, which offset past the four actors.
  expect_equal(ties$from, c(1L, 2L, 3L, 1L, 4L, 2L))
  expect_equal(ties$to, c(5L, 5L, 6L, 7L, 6L, 7L))

  changes <- as.data.frame(assembled$changes)
  # Both node sets contribute, each resolved through its own frame: C3 is
  # global id 7, not the id its position would give within `actors`.
  expect_setequal(changes$var, c("size", "budget"))
  expect_equal(changes$node[changes$var == "size"], c(2L, 4L))
  expect_equal(changes$node[changes$var == "budget"], c(7L, 7L))
})

test_that("a one-mode bundle keeps its pre-two-mode shape", {
  data("Social_Evolution", package = "goldfish", envir = environment())
  events <- calls[1:40, ]
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, events, nodes = actors)

  assembled <- assemble_stocnet_from_legacy(list(
    call_network = call_network,
    events = events,
    actors = actors
  ))
  # A single node set declares no modes: one mode set would be redundant, and
  # the added column would change every existing one-mode object.
  expect_false("mode" %in% names(assembled$nodes))
  expect_null(assembled$info$sender)
  expect_null(assembled$info$receiver)
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

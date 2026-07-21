# Parse-time effect side signatures.
#
# Validity is derived per argument position: each index into a network argument
# must land on a side that argument's own layer can hold. The multipartite
# fixture is the workhorse -- a two-mode focal `attend` (actor -> event), a
# one-mode covariate `coauthor` (actor -> actor) and a second two-mode
# covariate `member` (actor -> org) -- so the focal pair, an argument's own
# pair and a mismatched pair are all distinguishable in one object.

gate_effects <- function(formula, data, sub_model = "choice", model = "DyNAM") {
  parsed <- parse_formula(formula, data = data)
  create_effects_functions(
    parsed$rhs_names,
    model,
    sub_model,
    data = data
  )
}

test_that("a one-mode-only effect on a two-mode focal names effect and layer", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  expect_error(
    gate_effects(attend ~ recip(attend), d),
    "`recip\\(\\)` cannot be computed on \"attend\""
  )
  # The message lists what a two-mode focal layer does admit.
  expect_error(gate_effects(attend ~ recip(attend), d), "four\\(\\)")
  expect_error(
    gate_effects(attend ~ trans(attend), d),
    "`trans\\(\\)` cannot be computed on \"attend\""
  )
  expect_error(
    gate_effects(attend ~ node_trans(attend), d),
    "`node_trans\\(\\)` cannot be computed on \"attend\""
  )
})

test_that("shared-partner effects need a one-mode focal layer", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  expect_error(
    gate_effects(attend ~ common_receiver(attend), d),
    "`common_receiver\\(\\)` cannot be computed on \"attend\""
  )
  expect_error(
    gate_effects(attend ~ common_sender(attend), d),
    "`common_sender\\(\\)` cannot be computed on \"attend\""
  )
})

test_that("a two-mode covariate projects onto a one-mode focal layer", {
  # The genuine two-mode reading of the shared-partner effects: the focal layer
  # is one-mode over the actors and `member` (actor -> org) contributes shared
  # affiliations.
  fixture <- make_stocnet_fixture_multipartite()
  fixture$info$focal <- "coauthor"
  d <- as_goldfish(fixture)

  expect_no_error(gate_effects(coauthor ~ common_receiver(member), d))
})

test_that("a degenerate type variant is rejected naming the type", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  expect_error(
    gate_effects(attend ~ indeg(attend, type = "ego"), d),
    "`indeg\\(\\)` with `type` = \"ego\""
  )
  # It is a structural zero, not merely a shape mismatch.
  expect_error(
    gate_effects(attend ~ indeg(attend, type = "ego"), d),
    "not identified"
  )
  expect_error(
    gate_effects(attend ~ outdeg(attend, type = "alter"), d),
    "`outdeg\\(\\)` with `type` = \"alter\""
  )
})

test_that("the two-mode-valid effects pass the gate", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  expect_no_error(gate_effects(attend ~ inertia(attend), d))
  expect_no_error(gate_effects(attend ~ tie(attend), d))
  expect_no_error(gate_effects(attend ~ four(attend), d))
  expect_no_error(gate_effects(attend ~ indeg(attend), d))
  expect_no_error(gate_effects(attend ~ outdeg(attend, type = "ego"), d))
  expect_no_error(gate_effects(attend ~ tertius_diff(attend, size), d))
})

test_that("a covariate layer is judged by its own pair, not the focal's", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  # `coauthor` is actor -> actor: the ego-type readings its own pair supports
  # are valid even though the focal layer is two-mode.
  expect_no_error(gate_effects(attend ~ indeg(coauthor, type = "ego"), d))
  expect_no_error(gate_effects(attend ~ outdeg(coauthor, type = "ego"), d))
  # ... and the alter-type reading is not: `coauthor` receives actors, the
  # focal layer receives events.
  expect_error(
    gate_effects(attend ~ indeg(coauthor), d),
    "cannot be computed on \"coauthor\""
  )
})

test_that("equal-sized distinct modes do not conform", {
  # `member` receives orgs and `attend` receives events -- two nodes each, so a
  # dimension comparison would accept what the mode sets reject.
  d <- as_goldfish(make_stocnet_fixture_multipartite())
  sides <- d$mode_map$layers

  expect_equal(
    length(sides$member$side2),
    length(sides$attend$side2),
    label = "the fixture's two receiver modes are the same size"
  )
  expect_error(
    gate_effects(attend ~ indeg(member), d),
    "cannot be computed on \"member\""
  )
})

test_that("rate reads the sender side: outdeg is valid, indeg is not", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  expect_no_error(gate_effects(attend ~ outdeg(attend), d, sub_model = "rate"))
  expect_error(
    gate_effects(attend ~ indeg(attend), d, sub_model = "rate"),
    "cannot be computed on \"attend\""
  )
})

test_that("a one-mode object is unaffected by the gate", {
  d <- as_goldfish(make_stocnet_fixture())

  expect_no_error(gate_effects(calls ~ recip(calls) + trans(calls), d))
  expect_no_error(gate_effects(calls ~ indeg(calls) + outdeg(calls), d))
})

test_that("rate outdeg initializes on a two-mode network", {
  # The lifted over-rejection: sender activity is well defined over two modes.
  effect_fun <- function(
    weighted = FALSE,
    is_two_mode = TRUE,
    transformer_fn = identity
  ) {
    NULL
  }
  network <- matrix(c(1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1), nrow = 4)

  expect_equal(
    init_DyNAM_rate.outdeg(effect_fun, network, NULL, 4, 3)$stat,
    .rowSums(network > 0, 4, 3)
  )
})

test_that("indeg with type = 'alter' initializes on a two-mode network", {
  # Receiver popularity, the canonical two-mode degree effect: it used to be
  # rejected through the rate init it delegates to.
  effect_fun <- function(
    weighted = FALSE,
    is_two_mode = TRUE,
    transformer_fn = identity,
    type = "alter"
  ) {
    NULL
  }
  network <- matrix(c(1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1), nrow = 4)

  stat <- init_REM_choice.indeg(effect_fun, network, NULL, 4, 3)$stat
  expect_equal(dim(stat), c(4L, 3L))
  expect_equal(
    stat[1, ],
    .colSums(network > 0, 4, 3),
    label = "every sender sees the same receiver in-degrees"
  )
})

# Attribute reads on a two-mode focal ------------------------------------------

test_that("attribute effects read the side their position names", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())
  parsed <- parse_formula(
    attend ~ ego(size) + alter(size) + same(size),
    data = d
  )
  refs <- vapply(parsed$rhs_names, function(t) t[[2]], character(1))

  expect_equal(refs[[1]], "nodes_side1$size", label = "ego reads the sender")
  expect_equal(
    refs[[2]],
    "nodes_side2$size",
    label = "alter reads the receiver"
  )
  # One written operand, both sides: the comparison effects expand here so the
  # init can be handed a vector per side.
  expect_equal(refs[[3]], "list(nodes_side1$size, nodes_side2$size)")
})

test_that("a one-mode focal collapses a comparison effect back to one read", {
  d <- as_goldfish(make_stocnet_fixture())
  parsed <- parse_formula(calls ~ same(floor), data = d)

  expect_equal(parsed$rhs_names[[1]][[2]], "list(nodes$floor, nodes$floor)")
  # Both positions name the same reference, so the object table collapses them
  # and the effect keeps the arity-1 route the frozen baselines run through.
  link <- get_objects_effects_link(parsed$rhs_names)
  expect_equal(nrow(link), 1L)
})

test_that("a comparison effect reports one operand on either kind of data", {
  # The user wrote one operand and must see one: a synthesized second position
  # is state, not something to render as `Object 2`.
  term_of <- function(data, formula) {
    parsed <- parse_formula(formula, data = data)
    GetDetailPrint(get_objects_effects_link(parsed$rhs_names), parsed)
  }
  two_mode <- term_of(
    as_goldfish(make_stocnet_fixture_multipartite()),
    attend ~ same(size)
  )
  one_mode <- term_of(as_goldfish(make_stocnet_fixture()), calls ~ same(floor))

  expect_equal(colnames(two_mode)[1], "Object")
  expect_equal(colnames(one_mode)[1], "Object")
  expect_equal(unname(two_mode[, ".term_export"]), "same_size")
  expect_equal(unname(one_mode[, ".term_export"]), "same_floor")
})

test_that("ego_alter_interaction keeps both operands the user wrote", {
  # Its second operand is written, not synthesized, so it renders as two --
  # and on two-mode data the two positions now read different sides, where
  # before they both collapsed onto the sender.
  d <- as_goldfish(make_stocnet_fixture_multipartite())
  parsed <- parse_formula(attend ~ ego_alter_interaction(size, size), data = d)
  link <- get_objects_effects_link(parsed$rhs_names)

  expect_equal(nrow(link), 2L)
  expect_setequal(rownames(link), c("nodes_side1$size", "nodes_side2$size"))
})

test_that("an attribute undefined on the mode it is read on aborts", {
  x <- make_stocnet_fixture_multipartite()
  # `budget` is measured on orgs only, so it is NA for every actor and event.
  x$nodes$budget <- c(NA, NA, NA, NA, NA, 12, 8)
  d <- as_goldfish(x)

  expect_error(
    create_effects_functions(
      parse_formula(attend ~ ego(budget), data = d)$rhs_names,
      "DyNAM",
      "choice",
      data = d
    ),
    regexp = "undefined"
  )
  expect_no_error(create_effects_functions(
    parse_formula(attend ~ ego(size), data = d)$rhs_names,
    "DyNAM",
    "choice",
    data = d
  ))
})

test_that("a two-mode alter statistic keeps no excluded diagonal", {
  # `alter` zeroed [i, i] on two-mode data because attribute-only effects never
  # received the injected flag and read the hardcoded is_two_mode = FALSE.
  prep <- estimate_dynam(
    attend ~ alter(size),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_multipartite()),
    preprocessing_only = TRUE
  )
  stat <- prep$initialStats[,, 1]

  expect_equal(
    nrow(unique(stat)),
    1L,
    label = "every sender sees the same alters"
  )
  expect_equal(as.vector(stat[1, ]), c(40, 25))
})

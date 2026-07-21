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
# The side contract as a table ------------------------------------------------
#
# One row per (object, term as written, sub-model) with the verdict the
# signature rules derive. A table rather than a test per effect because the
# contract *is* a table: adding an effect or a type variant is a row, and the
# whole taxonomy stays readable in one place instead of being spread over
# twenty assertions that each restate a fragment of it.
#
# The three objects give every argument-to-focal relation the contract
# distinguishes:
#
# | object     | focal              | arguments available                     |
# |------------|--------------------|-----------------------------------------|
# | two_mode   | attend actor->event| same pair; one-mode over the senders    |
# |            |                    | (coauthor); different receiver (member) |
# | one_mode   | coauthor actr->actr| a two-mode covariate to project (member)|
# | cross      | attend actor->event| shares the receiver, not the sender     |
# |            |                    | (sponsor org->event)                    |

validity_objects <- function() {
  one_mode <- make_stocnet_fixture_multipartite()
  one_mode$info$focal <- "coauthor"
  list(
    two_mode = as_goldfish(make_stocnet_fixture_multipartite()),
    one_mode = as_goldfish(one_mode),
    cross = as_goldfish(make_stocnet_fixture_tertius())
  )
}

# `valid` is the verdict; `why` is the reason the contract gives, carried so a
# failure reports what the row was meant to demonstrate rather than only which
# formula broke.
validity_case <- function(object, term, valid, why, sub_model = "choice") {
  data.frame(
    object = object,
    term = term,
    valid = valid,
    why = why,
    sub_model = sub_model,
    stringsAsFactors = FALSE
  )
}

# The gate's outcome as a value: the abort message, or NA where it accepted.
# Reducing to a value first is what lets both verdicts report through `info`
# with the row that produced them -- `expect_no_error()` has no such argument,
# and its `message` is a filter on which errors count, not a label.
gate_outcome <- function(row, objects) {
  tryCatch(
    {
      gate_effects(
        stats::as.formula(row$term),
        objects[[row$object]],
        sub_model = row$sub_model
      )
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
}

expect_verdict <- function(row, objects) {
  outcome <- gate_outcome(row, objects)
  info <- paste0(row$term, " [", row$sub_model, "] -- ", row$why)
  if (row$valid) {
    # Comparing against NA rather than asserting is.na() so a failure prints
    # the abort the row did not expect.
    expect_equal(outcome, NA_character_, info = info)
  } else {
    expect_match(outcome, "cannot be computed on", info = info)
  }
}

run_validity_cases <- function(cases) {
  objects <- validity_objects()
  for (i in seq_len(nrow(cases))) {
    expect_verdict(cases[i, ], objects)
  }
}

test_that("a two-mode focal layer admits exactly the derived set", {
  run_validity_cases(rbind(
    validity_case(
      "two_mode",
      "attend ~ inertia(attend)",
      TRUE,
      "w[i, j] direct"
    ),
    validity_case("two_mode", "attend ~ tie(attend)", TRUE, "w[i, j] direct"),
    validity_case(
      "two_mode",
      "attend ~ four(attend)",
      TRUE,
      "two-mode closure"
    ),
    validity_case("two_mode", "attend ~ ego(size)", TRUE, "z on the sender"),
    validity_case(
      "two_mode",
      "attend ~ alter(size)",
      TRUE,
      "z on the receiver"
    ),
    validity_case("two_mode", "attend ~ same(size)", TRUE, "z on both sides"),
    validity_case("two_mode", "attend ~ diff(size)", TRUE, "z on both sides"),
    validity_case("two_mode", "attend ~ sim(size)", TRUE, "z on both sides"),
    validity_case(
      "two_mode",
      "attend ~ ego_alter_interaction(size, size)",
      TRUE,
      "one operand per side"
    ),
    validity_case(
      "two_mode",
      "attend ~ tertius_diff(attend, size)",
      TRUE,
      "both reads on the sender side"
    ),
    # The square-path and reverse-dyad families need one node set on both ends
    # of a tie, which a two-mode focal layer does not have.
    validity_case(
      "two_mode",
      "attend ~ recip(attend)",
      FALSE,
      "w[j, i] reverses the pair"
    ),
    validity_case(
      "two_mode",
      "attend ~ trans(attend)",
      FALSE,
      "i -> k -> j needs a square"
    ),
    validity_case(
      "two_mode",
      "attend ~ cycle(attend)",
      FALSE,
      "i -> k -> j needs a square"
    ),
    validity_case(
      "two_mode",
      "attend ~ node_trans(attend)",
      FALSE,
      "i -> k -> j needs a square"
    ),
    validity_case(
      "two_mode",
      "attend ~ common_sender(attend)",
      FALSE,
      "i and j both index R2"
    ),
    validity_case(
      "two_mode",
      "attend ~ common_receiver(attend)",
      FALSE,
      "i and j both index R1"
    )
  ))
})

test_that("a type variant that reads the wrong side is rejected", {
  # Degenerate rather than merely non-conformable: senders never receive, so
  # the statistic would be a structural zero.
  run_validity_cases(rbind(
    validity_case(
      "two_mode",
      "attend ~ indeg(attend)",
      TRUE,
      "alter: receiver popularity"
    ),
    validity_case(
      "two_mode",
      "attend ~ outdeg(attend, type = \"ego\")",
      TRUE,
      "sender activity"
    ),
    validity_case(
      "two_mode",
      "attend ~ tertius(attend, size)",
      TRUE,
      "alter: summarized over the receiver's in-neighbors"
    ),
    validity_case(
      "two_mode",
      "attend ~ indeg(attend, type = \"ego\")",
      FALSE,
      "senders of a two-mode layer never receive"
    ),
    validity_case(
      "two_mode",
      "attend ~ outdeg(attend, type = \"alter\")",
      FALSE,
      "receivers of a two-mode layer never send"
    ),
    validity_case(
      "two_mode",
      "attend ~ tertius(attend, size, type = \"ego\")",
      FALSE,
      "the sender has no in-neighbors"
    )
  ))
})

test_that("an argument is judged by its own mode pair, not the focal's", {
  run_validity_cases(rbind(
    # `coauthor` is actor -> actor, so its ego-type readings stand even under a
    # two-mode focal layer.
    validity_case(
      "two_mode",
      "attend ~ indeg(coauthor, type = \"ego\")",
      TRUE,
      "coauthor receives actors, which the focal layer sends"
    ),
    validity_case(
      "two_mode",
      "attend ~ outdeg(coauthor, type = \"ego\")",
      TRUE,
      "idem"
    ),
    validity_case(
      "two_mode",
      "attend ~ outdeg(member, type = \"ego\")",
      TRUE,
      "member sends actors, which the focal layer sends"
    ),
    validity_case(
      "two_mode",
      "attend ~ indeg(coauthor)",
      FALSE,
      "coauthor receives actors, the focal layer receives events"
    ),
    validity_case(
      "two_mode",
      "attend ~ recip(coauthor)",
      FALSE,
      "the reverse pair still has to be the focal pair"
    ),
    validity_case(
      "two_mode",
      "attend ~ tertius_diff(member, size)",
      FALSE,
      "its ego read and its aggregate must share a side"
    ),
    # Sides are compared as node-id sets, never as sizes: `member` receives two
    # orgs and `attend` two events.
    validity_case(
      "two_mode",
      "attend ~ indeg(member)",
      FALSE,
      "equal-sized distinct modes must not conform"
    ),
    # Shares the focal receiver but not the sender: the case that motivates
    # resolving a neighbor-aggregated attribute on the argument's own side.
    validity_case(
      "cross",
      "attend ~ tertius(sponsor, size)",
      TRUE,
      "sponsor reaches events, which the focal layer receives"
    ),
    validity_case("cross", "attend ~ indeg(sponsor)", TRUE, "idem"),
    validity_case(
      "cross",
      "attend ~ outdeg(sponsor, type = \"ego\")",
      FALSE,
      "sponsor sends orgs, the focal layer sends actors"
    ),
    validity_case(
      "cross",
      "attend ~ four(sponsor)",
      FALSE,
      "the closure's outer ends must be the focal pair"
    ),
    validity_case(
      "cross",
      "attend ~ tertius_diff(sponsor, size)",
      FALSE,
      "its ego read is on actors, its aggregate on orgs"
    )
  ))
})

test_that("the rate submodel collapses every read onto the sender side", {
  run_validity_cases(rbind(
    validity_case(
      "two_mode",
      "attend ~ outdeg(attend)",
      TRUE,
      "sender activity",
      sub_model = "rate"
    ),
    validity_case(
      "two_mode",
      "attend ~ indeg(coauthor)",
      TRUE,
      "a one-mode covariate over the senders",
      sub_model = "rate"
    ),
    validity_case(
      "two_mode",
      "attend ~ tertius(coauthor, size)",
      TRUE,
      "the sender's in-neighbors exist in coauthor",
      sub_model = "rate"
    ),
    validity_case(
      "two_mode",
      "attend ~ indeg(attend)",
      FALSE,
      "the sender never receives in the focal layer",
      sub_model = "rate"
    ),
    validity_case(
      "two_mode",
      "attend ~ tertius(attend, size)",
      FALSE,
      "idem: no in-neighbors to summarize over",
      sub_model = "rate"
    ),
    validity_case(
      "two_mode",
      "attend ~ node_trans(attend)",
      FALSE,
      "i -> k -> j needs a square",
      sub_model = "rate"
    )
  ))
})

test_that("a one-mode focal layer projects a two-mode covariate", {
  # The genuine two-mode reading of the shared-partner effects: shared
  # affiliations among actors, contributed by a covariate over a second mode.
  run_validity_cases(rbind(
    validity_case(
      "one_mode",
      "coauthor ~ common_receiver(member)",
      TRUE,
      "member sends actors, which the focal layer sends and receives"
    ),
    validity_case(
      "one_mode",
      "coauthor ~ recip(coauthor)",
      TRUE,
      "a square focal pair"
    ),
    validity_case(
      "one_mode",
      "coauthor ~ trans(coauthor)",
      TRUE,
      "a square focal pair"
    ),
    validity_case(
      "one_mode",
      "coauthor ~ four(coauthor)",
      TRUE,
      "a square focal pair"
    ),
    validity_case(
      "one_mode",
      "coauthor ~ common_sender(member)",
      FALSE,
      "common_sender indexes R2, and member receives orgs"
    ),
    validity_case(
      "one_mode",
      "coauthor ~ indeg(member)",
      FALSE,
      "member receives orgs, the focal layer receives actors"
    )
  ))
})

test_that("a mixed chain conforms in one direction only", {
  # The chain is i -> k -> j: the first network's receivers must be the
  # second's senders, and the outer ends the focal layer's own sides. Reversing
  # the operands breaks both joins even though the same two layers are named.
  run_validity_cases(rbind(
    validity_case(
      "two_mode",
      "attend ~ mixed_trans(list(coauthor, attend))",
      TRUE,
      "actor -> actor -> event chains onto actor -> event"
    ),
    validity_case(
      "two_mode",
      "attend ~ mixed_cycle(list(coauthor, attend))",
      TRUE,
      "idem"
    ),
    validity_case(
      "two_mode",
      "attend ~ mixed_trans(list(attend, coauthor))",
      FALSE,
      "events do not send in coauthor"
    ),
    validity_case(
      "one_mode",
      "coauthor ~ mixed_trans(list(member, member))",
      FALSE,
      "orgs do not send in member"
    ),
    validity_case(
      "cross",
      "attend ~ mixed_trans(list(attend, sponsor))",
      FALSE,
      "events do not send in sponsor"
    )
  ))
})

test_that("a one-mode object is unaffected by the gate", {
  run_validity_cases(rbind(
    validity_case("one_mode", "coauthor ~ recip(coauthor)", TRUE, "square"),
    validity_case("one_mode", "coauthor ~ indeg(coauthor)", TRUE, "square"),
    validity_case("one_mode", "coauthor ~ outdeg(coauthor)", TRUE, "square")
  ))
  d <- as_goldfish(make_stocnet_fixture())
  expect_no_error(gate_effects(calls ~ recip(calls) + trans(calls), d))
  expect_no_error(gate_effects(calls ~ indeg(calls) + outdeg(calls), d))
})

test_that("a rejection names the effect, the mismatch and the alternatives", {
  d <- as_goldfish(make_stocnet_fixture_multipartite())

  expect_error(
    gate_effects(attend ~ recip(attend), d),
    "`recip\\(\\)` cannot be computed on \"attend\""
  )
  expect_error(gate_effects(attend ~ recip(attend), d), "four\\(\\)")
  # A degenerate read names the type that selected the side, and says why the
  # coefficient could not be identified rather than only that shapes differ.
  expect_error(
    gate_effects(attend ~ indeg(attend, type = "ego"), d),
    "`indeg\\(\\)` with `type` = \"ego\""
  )
  expect_error(
    gate_effects(attend ~ indeg(attend, type = "ego"), d),
    "not identified"
  )
  # The mismatch is reported in the user's terms: layer, end, and modes.
  expect_error(
    gate_effects(attend ~ indeg(member), d),
    "the receiver side of layer \"member\" \\(mode \"org\"\\)"
  )
})

test_that("four counts two-mode four-cycles", {
  # Hand-computed against the effect's own definition, stat[i, j] =
  # sum over k != j, l != i of w[i, k] w[l, k] w[l, j] -- an independent sum
  # rather than a second call of the matrix expression under test.
  #
  #        E1 E2 E3
  #   A1    1  1  0
  #   A2    1  0  1
  #   A3    0  1  1
  #
  # stat[1, 3]: k = 1 pairs A1-E1 with A2 (E1, E3) -> 1; k = 2 pairs A1-E2 with
  # A3 (E2, E3) -> 1; total 2. Every actor reaches every event by exactly one
  # such path, and none by the diagonal pairing.
  network <- matrix(c(1, 1, 0, 1, 0, 1, 0, 1, 1), nrow = 3, byrow = TRUE)
  effect_fun <- function(is_two_mode = TRUE, transformer_fn = identity) NULL

  brute <- matrix(0, nrow = 3, ncol = 3)
  for (i in 1:3) {
    for (j in 1:3) {
      for (k in 1:3) {
        for (l in 1:3) {
          if (k != j && l != i) {
            brute[i, j] <- brute[i, j] +
              network[i, k] * network[l, k] * network[l, j]
          }
        }
      }
    }
  }

  stat <- unname(init_DyNAM_choice.four(effect_fun, network, NULL, 3, 3)$stat)

  expect_equal(stat, brute)
  expect_equal(
    stat,
    matrix(c(0, 0, 2, 0, 2, 0, 2, 0, 0), nrow = 3, byrow = TRUE),
    label = "the hand-computed counts"
  )
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

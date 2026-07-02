# Interaction-term parsing (design D2/D9, task 2.5). `get_rhs_names()` /
# `parse_formula()` recognize `:` and `*`, deduplicate operands into the returned
# rhs terms, and carry the interaction structure + per-term roles (is_main /
# is_operand / estimate). Computation of the product statistic lands with task
# 2.6; until then estimation of an interaction formula aborts.

make_interaction_fixture <- function() {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
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
  callsDependent <- callsDependent[1:80, ]
  make_data(callsDependent, callNetwork, calls, actors)
}

test_that("a:b builds one interaction referencing both operands, no main effect", {
  d <- make_interaction_fixture()
  parsed <- parse_formula(callsDependent ~ inertia:recip, envir = d)

  # both operands are returned as rhs terms (deduplicated), neither as a main
  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("inertia", "recip")
  )
  expect_length(parsed$interactions, 1)
  expect_identical(parsed$interactions[[1]]$operands, c(1L, 2L))
  expect_identical(parsed$interactions[[1]]$order, 2L)

  # `:` alone: operands are kept but not estimated as main effects
  expect_identical(unlist(parsed$is_main_parameter), c(FALSE, FALSE))
  expect_identical(unlist(parsed$is_operand_parameter), c(TRUE, TRUE))
  expect_identical(unlist(parsed$estimate_parameter), c(FALSE, FALSE))
})

test_that("a*b expands to a + b + a:b (operands are also main effects)", {
  d <- make_interaction_fixture()
  parsed <- parse_formula(callsDependent ~ inertia * recip, envir = d)

  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("inertia", "recip")
  )
  expect_length(parsed$interactions, 1)
  expect_identical(parsed$interactions[[1]]$operands, c(1L, 2L))
  # both are requested main effects AND operands -> estimated
  expect_identical(unlist(parsed$is_main_parameter), c(TRUE, TRUE))
  expect_identical(unlist(parsed$is_operand_parameter), c(TRUE, TRUE))
  expect_identical(unlist(parsed$estimate_parameter), c(TRUE, TRUE))
})

test_that("operands keep their own arguments and dedup across terms", {
  d <- make_interaction_fixture()
  # trans is a main effect; inertia and recip feed the interaction; recip is
  # shared and must appear once.
  parsed <- parse_formula(
    callsDependent ~ trans + inertia:recip + recip,
    envir = d
  )
  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("trans", "inertia", "recip")
  )
  expect_length(parsed$interactions, 1)
  # operands reference the inertia + recip rhs positions (2, 3)
  expect_identical(parsed$interactions[[1]]$operands, c(2L, 3L))
  # trans and recip are mains; inertia is operand-only
  expect_identical(unlist(parsed$is_main_parameter), c(TRUE, FALSE, TRUE))
  expect_identical(unlist(parsed$estimate_parameter), c(TRUE, FALSE, TRUE))
})

test_that("a 3-way interaction records all operands (n-ary)", {
  d <- make_interaction_fixture()
  parsed <- parse_formula(callsDependent ~ inertia:recip:trans, envir = d)
  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("inertia", "recip", "trans")
  )
  expect_length(parsed$interactions, 1)
  expect_identical(parsed$interactions[[1]]$operands, c(1L, 2L, 3L))
  expect_identical(parsed$interactions[[1]]$order, 3L)
})

test_that("a non-interaction formula carries an empty interaction structure", {
  d <- make_interaction_fixture()
  parsed <- parse_formula(callsDependent ~ inertia + recip, envir = d)
  expect_length(parsed$interactions, 0)
  expect_identical(unlist(parsed$is_main_parameter), c(TRUE, TRUE))
  expect_identical(unlist(parsed$is_operand_parameter), c(FALSE, FALSE))
})

test_that("interactions abort for sender-indexed (rate) models", {
  d <- make_interaction_fixture()
  # dyad models (choice / REM) compute interactions (see
  # test-interaction_computation.R); sender-indexed rate models are not yet
  # supported.
  expect_error(
    estimate_dynam(
      callsDependent ~ 1 + indeg:outdeg,
      sub_model = "rate",
      data = d
    ),
    "not yet supported"
  )
})

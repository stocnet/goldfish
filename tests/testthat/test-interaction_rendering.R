# Interaction term rendering. An interaction's compact
# name is the `:`-join of its operands' rendered names, inheriting their short
# forms (coef/vcov) and object-disambiguated export forms (gather/db). Names must
# be correct and unique.

make_interaction_fixture <- function() {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  calls_dependent <- calls_dependent[1:120, ]
  make_data(calls_dependent, call_network, calls, actors)
}

test_that("a*b coef and vcov names join the operand short names", {
  d <- make_interaction_fixture()
  m <- estimate_dynam(
    calls_dependent ~ inertia * recip,
    sub_model = "choice",
    data = d
  )
  expect_identical(names(coef(m)), c("inrt", "rec", "inrt:rec"))
  expect_identical(colnames(vcov(m)), c("inrt", "rec", "inrt:rec"))
  expect_identical(rownames(vcov(m)), c("inrt", "rec", "inrt:rec"))
})

test_that("a:b renders the interaction name for the single estimated column", {
  d <- make_interaction_fixture()
  m <- estimate_dynam(
    calls_dependent ~ inertia:recip,
    sub_model = "choice",
    data = d
  )
  expect_identical(names(coef(m)), "inrt:rec")
})

test_that("interaction gather/export names join the operand export forms", {
  d <- make_interaction_fixture()
  g <- compute_stats(
    calls_dependent ~ inertia:recip,
    data = d,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  cn <- colnames(g$stat_all_events)
  expect_identical(cn[3], "inertia_call_network:recip_call_network")
})

test_that("a 3-way interaction name joins all three operands", {
  d <- make_interaction_fixture()
  g <- compute_stats(
    calls_dependent ~ inertia:recip:trans,
    data = d,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  cn <- colnames(g$stat_all_events)
  expect_identical(
    cn[4],
    "inertia_call_network:recip_call_network:trans_call_network"
  )
})

test_that("interaction names are unique within a mixed formula", {
  d <- make_interaction_fixture()
  g <- compute_stats(
    calls_dependent ~ inertia + recip + inertia:recip + recip:trans,
    data = d,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  cn <- colnames(g$stat_all_events)
  expect_false(as.logical(anyDuplicated(cn)))
  expect_true("inertia_call_network:recip_call_network" %in% cn)
  expect_true("recip_call_network:trans_call_network" %in% cn)
})

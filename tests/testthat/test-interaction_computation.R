# Interaction statistic computation in the dyad recipe loop (design D9, task
# 2.6). An interaction column equals the elementwise product of its operands
# over the whole event sequence; operands are kept but (for `:`) held out of
# estimation by fixing at 0; `a*b` estimates both operands and the product.

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
  callsDependent <- callsDependent[1:120, ]
  make_data(callsDependent, callNetwork, calls, actors)
}

test_that("a:b column equals the operand product over the full sequence", {
  d <- make_interaction_fixture()
  g <- compute_stats(
    callsDependent ~ inertia:recip,
    data = d,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  m <- g$stat_all_events
  expect_equal(ncol(m), 3)
  # columns: inertia (1), recip (2), inertia:recip (3)
  expect_equal(m[, 3], m[, 1] * m[, 2], tolerance = 1e-6)
})

test_that("a 3-way interaction equals the product of all operands", {
  d <- make_interaction_fixture()
  g <- compute_stats(
    callsDependent ~ inertia:recip:trans,
    data = d,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  m <- g$stat_all_events
  expect_equal(ncol(m), 4)
  expect_equal(m[, 4], m[, 1] * m[, 2] * m[, 3], tolerance = 1e-6)
})

test_that("a broadcast (ego) operand interaction equals the product", {
  d <- make_interaction_fixture()
  # outdeg(type = "ego") varies across senders (broadcast kind 2); its product
  # with a dyadic operand exercises the whole-row operand-state update.
  g <- compute_stats(
    callsDependent ~ outdeg(callNetwork, type = "ego"):inertia,
    data = d,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  m <- g$stat_all_events
  expect_equal(m[, 3], m[, 1] * m[, 2], tolerance = 1e-6)
})

test_that("a*b estimates both operands and the product", {
  d <- make_interaction_fixture()
  m <- estimate_dynam(
    callsDependent ~ inertia * recip,
    sub_model = "choice",
    data = d
  )
  # three estimated coefficients; none fixed
  expect_length(coef(m), 3)
  expect_false(any(GetFixed(m)))
})

test_that("a:b keeps operands but estimates only the product", {
  d <- make_interaction_fixture()
  m <- estimate_dynam(
    callsDependent ~ inertia:recip,
    sub_model = "choice",
    data = d
  )
  # operands kept in the design (fixed at 0), only the interaction estimated
  expect_equal(unname(m$parameters[1:2]), c(0, 0))
  expect_equal(unname(GetFixed(m)), c(TRUE, TRUE, FALSE))
  expect_length(coef(m), 1)
})

test_that("REM supports interaction terms", {
  d <- make_interaction_fixture()
  g <- compute_stats(
    callsDependent ~ inertia:recip,
    data = d,
    model = "REM",
    sub_model = "rate",
    output = "gather"
  )
  m <- g$stat_all_events
  expect_equal(m[, 3], m[, 1] * m[, 2], tolerance = 1e-6)
})

test_that("interactions are rejected for sender-indexed (rate) models", {
  d <- make_interaction_fixture()
  expect_error(
    estimate_dynam(
      callsDependent ~ 1 + indeg:outdeg,
      sub_model = "rate",
      data = d
    ),
    "not yet supported"
  )
})

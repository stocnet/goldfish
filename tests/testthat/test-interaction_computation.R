# Interaction statistic computation in the dyad recipe loop. An interaction
# column equals the elementwise product of its operands
# over the whole event sequence; operands are kept but (for `:`) held out of
# estimation by fixing at 0; `a*b` estimates both operands and the product.

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

test_that("a:b column equals the operand product over the full sequence", {
  d <- make_interaction_fixture()
  g <- compute_statistics(
    calls_dependent ~ inertia:recip,
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
  g <- compute_statistics(
    calls_dependent ~ inertia:recip:trans,
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
  g <- compute_statistics(
    calls_dependent ~ outdeg(call_network, type = "ego"):inertia,
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
    calls_dependent ~ inertia * recip,
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
    calls_dependent ~ inertia:recip,
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
  # Ordinal REM (no intercept) keeps the three effect columns aligned with the
  # operand product; the timed-rate case adds the intercept and is covered by
  # the DyNAM-rate interaction test below.
  g <- compute_statistics(
    calls_dependent ~ inertia:recip,
    data = d,
    model = "REM",
    sub_model = "rate_ordered",
    output = "gather"
  )
  m <- g$stat_all_events
  expect_equal(m[, 3], m[, 1] * m[, 2], tolerance = 1e-6)
})

test_that("sender-indexed (rate) interactions are now supported", {
  # rate interactions landed with sender interaction terms; the product
  # is per-sender (see test-sender_interaction.R). Only DyNAMi stays unsupported.
  d <- make_interaction_fixture()
  expect_no_error(
    compute_statistics(
      calls_dependent ~ 1 + indeg:outdeg,
      data = d,
      model = "DyNAM",
      sub_model = "rate"
    )
  )
})

# Interaction terms in sender-indexed (DyNAM rate / rate_ordered) models.
# A rate interaction's statistic is the
# per-sender elementwise product of its operands; operand validity is role-aware
# (sender-varying operands only; `global` allowed as an operand even where it is
# rejected as a bare main effect); DyNAMi interactions remain unsupported.

make_rate_fixture <- function() {
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
  calls_dependent <- calls_dependent[1:150, ]
  make_data(calls_dependent, call_network, calls, actors)
}

test_that("rate a:b column equals the per-sender operand product", {
  d <- make_rate_fixture()
  g <- compute_statistics(
    calls_dependent ~ 1 + indeg:outdeg,
    data = d,
    model = "DyNAM",
    sub_model = "rate",
    output = "gather"
  )
  m <- g$stat_all_events
  ii <- grep("^indeg", colnames(m))[1]
  oo <- grep("^outdeg", colnames(m))[1]
  xx <- grep(":", colnames(m))[1]
  expect_equal(m[, xx], m[, ii] * m[, oo], tolerance = 1e-6)
})

test_that("rate a*b estimates the intercept, both operands, and the product", {
  d <- make_rate_fixture()
  m <- estimate_dynam(
    calls_dependent ~ 1 + indeg * outdeg,
    sub_model = "rate",
    data = d
  )
  # Intercept + indeg + outdeg + indeg:outdeg, none fixed
  expect_length(coef(m), 4)
  expect_false(any(GetFixed(m)))
})

test_that("rate a:b keeps operands (fixed at 0) and estimates the product", {
  d <- make_rate_fixture()
  m <- estimate_dynam(
    calls_dependent ~ 1 + indeg:outdeg,
    sub_model = "rate",
    data = d
  )
  # params: [Intercept, indeg (fixed 0), outdeg (fixed 0), indeg:outdeg]
  expect_equal(unname(m$parameters[2:3]), c(0, 0))
  expect_equal(unname(GetFixed(m)), c(FALSE, TRUE, TRUE, FALSE))
  # coef drops the fixed operands: Intercept + the interaction
  expect_length(coef(m), 2)
})

test_that("an alter operand is rejected in a rate interaction", {
  d <- make_rate_fixture()
  expect_error(
    estimate_dynam(
      calls_dependent ~ 1 + alter(actors$floor):indeg,
      sub_model = "rate",
      data = d
    ),
    "Unsupported interaction operand"
  )
})

test_that("global is permitted as a rate_ordered operand but not as a main", {
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
  calls_dependent <- calls_dependent[1:150, ]
  seasons <- make_global_attributes(data.frame(winter = 2))
  d <- make_data(calls_dependent, call_network, calls, actors, seasons)

  # bare global() main effect is rejected in rate_ordered ...
  expect_error(
    estimate_dynam(
      calls_dependent ~ global(seasons$winter) + indeg,
      sub_model = "rate_ordered",
      data = d
    ),
    "Unsupported main effect"
  )
  # ... but the same global as an interaction operand is allowed (it restores
  # per-sender variation), so preprocessing produces the columns.
  prep <- compute_statistics(
    calls_dependent ~ global(seasons$winter):indeg,
    data = d,
    model = "DyNAM",
    sub_model = "rate_ordered"
  )
  expect_s3_class(prep, "goldfishStat")
})

test_that("DyNAMi rejects a one-mode stocnet as not an interaction object", {
  d <- make_rate_fixture()
  # DyNAMi accepts a stocnet only when it is a two-mode actors x groups object;
  # a one-mode rate fixture is rejected at the boundary.
  expect_error(
    estimate_dynami(
      calls_dependent ~ indeg:outdeg,
      sub_model = "rate",
      data = d
    ),
    "two-mode actors x groups"
  )
})

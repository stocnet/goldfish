# support_constraint consumption for REM on the default engine. REM's risk
# set is 2D (every dyad), so the mask cannot reduce to a
# separable sender/receiver filter; instead the contribution zeroes the
# disallowed dyads' rates — the same mechanism REM already uses to exclude
# reflexive edges. An all-allowing mask is therefore an identity; a restricting
# one changes the estimate; an observed dyad excluded by its own constraint
# errors.

make_rem_fixture <- function(n_events = 100L, seed = 1L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  lab <- actors$label
  n <- nrow(actors)
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
  calls_dependent <- calls_dependent[seq_len(n_events), ]
  obs <- cbind(
    match(as.data.frame(calls_dependent)$sender, lab),
    match(as.data.frame(calls_dependent)$receiver, lab)
  )
  list(
    actors = actors,
    calls = calls,
    call_network = call_network,
    calls_dependent = calls_dependent,
    lab = lab,
    n = n,
    obs = obs,
    seed = seed
  )
}

# All dyads allowed except `n_excluded` never-observed ones (so no observed dyad
# is excluded). `full = TRUE` allows every non-reflexive dyad (identity mask).
rem_data <- function(fx, n_excluded = 0L) {
  allowed <- matrix(1, fx$n, fx$n, dimnames = list(fx$lab, fx$lab))
  diag(allowed) <- 0
  if (n_excluded > 0) {
    set.seed(fx$seed)
    drawn <- 0L
    while (drawn < n_excluded) {
      i <- sample(fx$n, 1)
      j <- sample(fx$n, 1)
      if (i != j && !any(fx$obs[, 1] == i & fx$obs[, 2] == j)) {
        allowed[i, j] <- 0
        drawn <- drawn + 1L
      }
    }
  }
  # Bind the node set to a plain name so make_data() can resolve it (a layer
  # built with nodes = fx$actors records the unresolvable name "fx$actors").
  actors <- fx$actors
  allowedNet <- make_network(
    matrix = allowed,
    nodes = actors,
    directed = TRUE
  )
  calls <- fx$calls
  call_network <- fx$call_network
  calls_dependent <- fx$calls_dependent
  make_data(calls_dependent, call_network, calls, actors, allowedNet)
}

test_that("an all-allowing REM constraint is an identity (equals unconstrained)", {
  fx <- make_rem_fixture()
  d <- rem_data(fx)
  opt <- set_algorithm_newton(engine = "default")
  m_cstr <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt
  )
  m_unc <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    control_algo = opt
  )
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-8)
  expect_equal(m_cstr$logLikelihood, m_unc$logLikelihood, tolerance = 1e-8)
})

test_that("a restricting REM constraint changes the estimate", {
  fx <- make_rem_fixture()
  d <- rem_data(fx, n_excluded = 400L)
  opt <- set_algorithm_newton(engine = "default")
  m_cstr <- suppressWarnings(estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt
  ))
  m_unc <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    control_algo = opt
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("gather_compute / default_c consume the REM constraint natively", {
  fx <- make_rem_fixture(n_events = 40L)
  d <- rem_data(fx, n_excluded = 50L)
  spec <- calls_dependent ~ 1 + inertia + recip
  m_def <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(engine = "default")
  ))
  # both compiled engines read the folded dense point active_dyad,
  # so they match the default engine with no downgrade fallback.
  m_gc <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  ))
  m_dc <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(engine = "default_c")
  ))
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("an observed dyad excluded by its own REM constraint errors", {
  fx <- make_rem_fixture(n_events = 60L)
  d <- rem_data(fx)
  opt <- set_algorithm_newton(engine = "default")
  # `~ tie(call_network)` excludes the first event (no prior tie exists yet).
  expect_error(
    estimate_rem(
      calls_dependent ~ 1 + inertia + recip,
      sub_model = "rate",
      data = d,
      support_constraint = ~ tie(call_network),
      control_algo = opt
    )
  )
})

test_that("an all-allowing REM rate_ordered constraint is an identity", {
  fx <- make_rem_fixture()
  d <- rem_data(fx)
  opt <- set_algorithm_newton(engine = "default")
  m_cstr <- estimate_rem(
    calls_dependent ~ inertia + recip,
    sub_model = "rate_ordered",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt
  )
  m_unc <- estimate_rem(
    calls_dependent ~ inertia + recip,
    sub_model = "rate_ordered",
    data = d,
    control_algo = opt
  )
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-8)
  expect_equal(m_cstr$logLikelihood, m_unc$logLikelihood, tolerance = 1e-8)
})

test_that("a restricting REM rate_ordered constraint changes the estimate", {
  fx <- make_rem_fixture()
  d <- rem_data(fx, n_excluded = 400L)
  opt <- set_algorithm_newton(engine = "default")
  m_cstr <- suppressWarnings(estimate_rem(
    calls_dependent ~ inertia + recip,
    sub_model = "rate_ordered",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt
  ))
  m_unc <- estimate_rem(
    calls_dependent ~ inertia + recip,
    sub_model = "rate_ordered",
    data = d,
    control_algo = opt
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("REM rate_ordered constraint runs natively on gather / default_c", {
  fx <- make_rem_fixture(n_events = 40L)
  d <- rem_data(fx, n_excluded = 50L)
  spec <- calls_dependent ~ inertia + recip
  m_def <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate_ordered",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(engine = "default")
  ))
  # Ordinal REM masks the disallowed dyads' utility before the multinomial
  # normalizer (and the probability-weighted score / information sums). Both
  # compiled engines read the folded dense point active_dyad, so
  # they match the default engine with no downgrade fallback.
  m_gc <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate_ordered",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  ))
  m_dc <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate_ordered",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(engine = "default_c")
  ))
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("a REM support_constraint folds active_dyad at the point encoding", {
  fx <- make_rem_fixture(n_events = 40L)
  d <- rem_data(fx, n_excluded = 50L)
  prep <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    preprocessing_only = TRUE
  )
  # Both presences n support fold into a dense n1 x n2 point active_dyad — the
  # per-event risk mask the default engine consumes directly (no standalone
  # riskMask). Static presence + a static allowedNet keeps the mask constant, so
  # the reflexive diagonal and every excluded dyad are FALSE in the init.
  expect_identical(prep$active_dyad_encoding, "point")
  expect_identical(dim(prep$active_dyad_init), c(fx$n, fx$n))
  expect_equal(sum(diag(prep$active_dyad_init)), 0)
  expect_false(is.null(prep$support_mask$receiver_presence_init))
})

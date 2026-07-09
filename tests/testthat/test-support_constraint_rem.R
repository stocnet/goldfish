# support_constraint consumption for REM on the default engine (task 4.4, REM
# part). REM's risk set is 2D (every dyad), so the mask cannot reduce to a
# separable sender/receiver filter; instead the contribution zeroes the
# disallowed dyads' rates — the same mechanism REM already uses to exclude
# reflexive edges. An all-allowing mask is therefore an identity; a restricting
# one changes the estimate; an observed dyad excluded by its own constraint
# errors (design D8).

make_rem_fixture <- function(n_events = 100L, seed = 1L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  lab <- actors$label
  n <- nrow(actors)
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
  callsDependent <- callsDependent[seq_len(n_events), ]
  obs <- cbind(
    match(as.data.frame(callsDependent)$sender, lab),
    match(as.data.frame(callsDependent)$receiver, lab)
  )
  list(
    actors = actors,
    calls = calls,
    callNetwork = callNetwork,
    callsDependent = callsDependent,
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
  allowedNet <- make_network(
    matrix = allowed,
    nodes = fx$actors,
    directed = TRUE
  )
  actors <- fx$actors
  calls <- fx$calls
  callNetwork <- fx$callNetwork
  callsDependent <- fx$callsDependent
  make_data(callsDependent, callNetwork, calls, actors, allowedNet)
}

test_that("an all-allowing REM constraint is an identity (equals unconstrained)", {
  fx <- make_rem_fixture()
  d <- rem_data(fx)
  opt <- set_estimation_opt(engine = "default")
  m_cstr <- estimate_rem(
    callsDependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = opt
  )
  m_unc <- estimate_rem(
    callsDependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    control_estimation = opt
  )
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-8)
  expect_equal(m_cstr$logLikelihood, m_unc$logLikelihood, tolerance = 1e-8)
})

test_that("a restricting REM constraint changes the estimate", {
  fx <- make_rem_fixture()
  d <- rem_data(fx, n_excluded = 400L)
  opt <- set_estimation_opt(engine = "default")
  m_cstr <- suppressWarnings(estimate_rem(
    callsDependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = opt
  ))
  m_unc <- estimate_rem(
    callsDependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = d,
    control_estimation = opt
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("gather_compute / default_c consume the REM constraint natively", {
  fx <- make_rem_fixture(n_events = 40L)
  d <- rem_data(fx, n_excluded = 50L)
  spec <- callsDependent ~ 1 + inertia + recip
  m_def <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default")
  ))
  # both compiled engines read the folded dense point active_dyad (design D11),
  # so they match the default engine with no downgrade fallback.
  m_gc <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "gather_compute")
  ))
  m_dc <- suppressWarnings(estimate_rem(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default_c")
  ))
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("an observed dyad excluded by its own REM constraint errors (design D8)", {
  fx <- make_rem_fixture(n_events = 60L)
  d <- rem_data(fx)
  opt <- set_estimation_opt(engine = "default")
  # `~ tie(callNetwork)` excludes the first event (no prior tie exists yet).
  expect_error(
    estimate_rem(
      callsDependent ~ 1 + inertia + recip,
      sub_model = "rate",
      data = d,
      support_constraint = ~ tie(callNetwork),
      control_estimation = opt
    )
  )
})

test_that("support_constraint still aborts for REM rate_ordered (unwired)", {
  fx <- make_rem_fixture(n_events = 60L)
  d <- rem_data(fx)
  opt <- set_estimation_opt(engine = "default")
  expect_error(
    estimate_rem(
      callsDependent ~ inertia + recip,
      sub_model = "rate_ordered",
      data = d,
      support_constraint = ~ tie(allowedNet),
      control_estimation = opt
    ),
    "not yet consumed"
  )
})

test_that("a REM support_constraint folds active_dyad at the point encoding", {
  fx <- make_rem_fixture(n_events = 40L)
  d <- rem_data(fx, n_excluded = 50L)
  prep <- estimate_rem(
    callsDependent ~ 1 + inertia + recip,
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

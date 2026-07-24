# support_constraint consumption on the R (default) engine, choice family.
# The mask reduces to a per-event receiver filter routed
# through the existing opportunities machinery, so a constrained model matches
# the established opportunities_list restriction to numerical precision, and an
# observed dyad excluded by its own constraint errors.

make_estimate_fixture <- function(n_events = 120L, seed = 1L) {
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

  # A static allowed-dyad network: all dyads allowed except a set of
  # never-observed ones, so the restriction never excludes an observed dyad.
  obs <- cbind(
    match(as.data.frame(calls_dependent)$sender, lab),
    match(as.data.frame(calls_dependent)$receiver, lab)
  )
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  set.seed(seed)
  drawn <- 0L
  while (drawn < 150L) {
    i <- sample(n, 1)
    j <- sample(n, 1)
    if (i != j && !any(obs[, 1] == i & obs[, 2] == j)) {
      allowed[i, j] <- 0
      drawn <- drawn + 1L
    }
  }
  allowedNet <- make_network(matrix = allowed, nodes = actors, directed = TRUE)
  list(
    data = make_data(
      calls_dependent,
      call_network,
      calls,
      actors,
      allowedNet
    ),
    allowed = allowed,
    obs = obs,
    n_events = n_events
  )
}

test_that("a support_constraint matches the opportunities_list restriction", {
  # opportunities_list is soft-deprecated but still the reference restriction here
  withr::local_options(lifecycle_verbosity = "quiet")
  fx <- make_estimate_fixture()
  opp <- lapply(
    seq_len(fx$n_events),
    function(e) which(fx$allowed[fx$obs[e, 1], ] > 0)
  )
  m_ref <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(engine = "default"),
    control_preprocessing = set_preprocessing(opportunities_list = opp)
  )
  m_cstr <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(engine = "default"),
    support_constraint = ~ tie(allowedNet)
  )
  expect_equal(coef(m_cstr), coef(m_ref), tolerance = 1e-6)
})

test_that("gather_compute consumes the choice constraint natively (== default)", {
  fx <- make_estimate_fixture()
  m_def <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_algorithm_newton(engine = "default")
  )
  # the R gather filters candidates directly, so no downgrade warning fires
  m_gc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("default_c consumes the choice constraint natively (== default)", {
  fx <- make_estimate_fixture()
  m_def <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_algorithm_newton(engine = "default")
  )
  # the C++ estimator filters receivers directly (no downgrade)
  m_dc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_algorithm_newton(engine = "default_c")
  )
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-6)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-6)
})

test_that("a support_constraint actually restricts the risk set (vs unconstrained)", {
  fx <- make_estimate_fixture()
  m_cstr <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(engine = "default"),
    support_constraint = ~ tie(allowedNet)
  )
  m_unc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(engine = "default")
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("an observed dyad excluded by its own constraint errors", {
  fx <- make_estimate_fixture(n_events = 60L)
  # `~ tie(call_network)` excludes the very first call (no prior tie exists yet).
  expect_error(
    estimate_dynam(
      calls_dependent ~ inertia + recip,
      sub_model = "choice",
      data = fx$data,
      control_estimation = set_algorithm_newton(engine = "default"),
      support_constraint = ~ tie(call_network)
    )
  )
})

test_that("a no-intercept REM rate constraint adds the intercept and runs", {
  fx <- make_estimate_fixture(n_events = 60L)
  # Dropping auto-ordinal: `sub_model = "rate"` without a time intercept is a
  # waiting-time model (the intercept is added), so its support_constraint is
  # consumed like any standard REM rate constraint rather than aborting. Ordinal
  # (rate_ordered) REM constraints are covered in test-support_constraint_rem.R.
  m <- suppressMessages(estimate_rem(
    calls_dependent ~ inertia + recip,
    sub_model = "rate",
    data = fx$data,
    support_constraint = ~ tie(allowedNet)
  ))
  expect_s3_class(m$model_spec, "rem_rate_spec")
  expect_true(m$model_spec$has_intercept)
})

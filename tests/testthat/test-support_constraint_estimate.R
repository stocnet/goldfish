# support_constraint consumption on the R (default) engine, choice family
# (tasks 4.1-4.3, 4.5). The mask reduces to a per-event receiver filter routed
# through the existing opportunities machinery, so a constrained model matches
# the established opportunities_list restriction to numerical precision (design
# D5/D6), an observed dyad excluded by its own constraint errors (design D8),
# and the not-yet-wired paths abort rather than silently ignore the constraint.

make_estimate_fixture <- function(n_events = 120L, seed = 1L) {
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

  # A static allowed-dyad network: all dyads allowed except a set of
  # never-observed ones, so the restriction never excludes an observed dyad.
  obs <- cbind(
    match(as.data.frame(callsDependent)$sender, lab),
    match(as.data.frame(callsDependent)$receiver, lab)
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
      callsDependent,
      callNetwork,
      calls,
      actors,
      allowedNet
    ),
    allowed = allowed,
    obs = obs,
    n_events = n_events
  )
}

test_that("a support_constraint matches the opportunities_list restriction (D5/D6)", {
  fx <- make_estimate_fixture()
  opp <- lapply(
    seq_len(fx$n_events),
    function(e) which(fx$allowed[fx$obs[e, 1], ] > 0)
  )
  m_ref <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_estimation_opt(engine = "default"),
    control_preprocessing = set_preprocessing_opt(opportunities_list = opp)
  )
  m_cstr <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_estimation_opt(engine = "default"),
    support_constraint = ~ tie(allowedNet)
  )
  expect_equal(coef(m_cstr), coef(m_ref), tolerance = 1e-6)
})

test_that("gather_compute consumes the choice constraint natively (== default)", {
  fx <- make_estimate_fixture()
  m_def <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default")
  )
  # the R gather filters candidates directly, so no downgrade warning fires
  m_gc <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "gather_compute")
  )
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("default_c consumes the choice constraint natively (== default)", {
  fx <- make_estimate_fixture()
  m_def <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default")
  )
  # the C++ estimator filters receivers directly (no downgrade)
  m_dc <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default_c")
  )
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-6)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-6)
})

test_that("a support_constraint actually restricts the risk set (vs unconstrained)", {
  fx <- make_estimate_fixture()
  m_cstr <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_estimation_opt(engine = "default"),
    support_constraint = ~ tie(allowedNet)
  )
  m_unc <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_estimation_opt(engine = "default")
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("an observed dyad excluded by its own constraint errors (design D8)", {
  fx <- make_estimate_fixture(n_events = 60L)
  # `~ tie(callNetwork)` excludes the very first call (no prior tie exists yet).
  expect_error(
    estimate_dynam(
      callsDependent ~ inertia + recip,
      sub_model = "choice",
      data = fx$data,
      control_estimation = set_estimation_opt(engine = "default"),
      support_constraint = ~ tie(callNetwork)
    )
  )
})

test_that("support_constraint aborts on ordinal REM (no intercept, unwired)", {
  fx <- make_estimate_fixture(n_events = 60L)
  # `~ inertia + recip` (no time intercept) is ordinal REM (rem_rate_ordered),
  # which uses the multinomial path and is not yet wired.
  expect_error(
    estimate_rem(
      callsDependent ~ inertia + recip,
      sub_model = "rate",
      data = fx$data,
      support_constraint = ~ tie(allowedNet)
    ),
    "not yet consumed"
  )
})

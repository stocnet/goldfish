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
    control_algo = set_algorithm_newton(backend = "r"),
    control_prep = set_preprocessing(opportunities_list = opp)
  )
  m_cstr <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_algo = set_algorithm_newton(backend = "r"),
    support_constraint = ~ tie(allowedNet)
  )
  expect_equal(coef(m_cstr), coef(m_ref), tolerance = 1e-6)
})

test_that("gather consumes the choice constraint natively (== r)", {
  fx <- make_estimate_fixture()
  m_def <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(backend = "r")
  )
  # the R gather filters candidates directly, so no downgrade warning fires
  m_gc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$log_likelihood, m_def$log_likelihood, tolerance = 1e-8)
})

test_that("cpp consumes the choice constraint natively (== r)", {
  fx <- make_estimate_fixture()
  m_def <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(backend = "r")
  )
  # the C++ estimator filters receivers directly (no downgrade)
  m_dc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(backend = "cpp")
  )
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-6)
  expect_equal(m_dc$log_likelihood, m_def$log_likelihood, tolerance = 1e-6)
})

test_that("a support_constraint actually restricts the risk set (vs unconstrained)", {
  fx <- make_estimate_fixture()
  m_cstr <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_algo = set_algorithm_newton(backend = "r"),
    support_constraint = ~ tie(allowedNet)
  )
  m_unc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_algo = set_algorithm_newton(backend = "r")
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("a constrained gather emits only the allowed candidates", {
  # The gather product is rendered AFTER the constraint folds, so the expansion
  # enumerates the post-fold risk set. Before the writer render stage this path
  # errored (the fold ran on a stack with no `event_time`); it had never had a
  # test despite 20 files exercising `support_constraint`.
  fx <- make_estimate_fixture()
  g_cstr <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = fx$data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather",
    support_constraint = ~ tie(allowedNet)
  )
  g_unc <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = fx$data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  # The constraint removes candidate rows, never adds them.
  expect_lt(sum(g_cstr$n_candidates), sum(g_unc$n_candidates))
  # Per event the candidate count is exactly the sender's allowed-receiver
  # count: the static allowed-dyad network reduced to the choice risk set.
  expected <- unname(rowSums(fx$allowed)[fx$obs[, 1]])
  expect_identical(as.integer(g_cstr$n_candidates), as.integer(expected))
  expect_equal(nrow(g_cstr$stat_all_events), sum(g_cstr$n_candidates))
})

test_that("a point-encoded stored object renders through the gather stage", {
  # The fold upgrades availability to the `point` encoding; the gather expansion
  # must read that encoding (as estimation does) rather than flatten the dense
  # mask as a vector. This is the replay surface -- a stored constrained object
  # converted to a stack -- independent of how the object was produced.
  fx <- make_estimate_fixture()
  prep <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = fx$data,
    model = "DyNAM",
    sub_model = "choice",
    output = "preprocessed",
    support_constraint = ~ tie(allowedNet)
  )
  expect_identical(prep$active_dyad_encoding, "point")

  direct <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = fx$data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather",
    support_constraint = ~ tie(allowedNet)
  )
  rendered <- gather_from_prep(prep, prep$model_spec)
  expect_equal(rendered$n_candidates, direct$n_candidates)
  expect_equal(rendered$selected, direct$selected)
  expect_equal(unname(rendered$stat_all_events), unname(direct$stat_all_events))
})

test_that("a constrained db export writes only the allowed candidates", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  fx <- make_estimate_fixture()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  gathered <- compute_statistics(
    calls_dependent ~ inertia + recip,
    data = fx$data,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather",
    support_constraint = ~ tie(allowedNet)
  )
  compute_statistics(
    calls_dependent ~ inertia + recip,
    data = fx$data,
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    support_constraint = ~ tie(allowedNet),
    control_prep = set_preprocessing(db = con, db_table = "stats")
  )
  tbl <- DBI::dbReadTable(con, "stats_1")
  # Rows that reach the database are the rows the model actually had in its
  # risk set: the constraint folds before the stack is rendered and written.
  expect_identical(
    as.integer(table(tbl$event_id)),
    as.integer(unname(rowSums(fx$allowed)[fx$obs[, 1]]))
  )
  written <- as.matrix(tbl[, gathered$names_effects])
  dimnames(written) <- NULL
  expect_equal(written, unname(gathered$stat_all_events))
})

test_that("an observed dyad excluded by its own constraint errors", {
  fx <- make_estimate_fixture(n_events = 60L)
  # `~ tie(call_network)` excludes the very first call (no prior tie exists yet).
  expect_error(
    estimate_dynam(
      calls_dependent ~ inertia + recip,
      sub_model = "choice",
      data = fx$data,
      control_algo = set_algorithm_newton(backend = "r"),
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
  expect_s3_class(m$model_spec, "goldfishLikDyadPoisson")
  expect_true(m$model_spec$has_intercept)
})

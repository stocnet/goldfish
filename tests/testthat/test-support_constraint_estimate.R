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

test_that("a deferred check keeps its verdict for estimation to raise", {
  # Preprocessed on its own, the object is not rejected: the walk checks
  # each observed event and keeps what it found. Estimating from it raises
  # the error there, without reading the mask again.
  fx <- make_estimate_fixture(n_events = 60L)
  prep <- suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(call_network)
  ))
  expect_identical(prep$support_verdict$failure$type, "empty")
  expect_identical(prep$support_verdict$failure$event, 1L)
  expect_error(
    suppressWarnings(estimate_dynam(
      calls_dependent ~ inertia + recip,
      sub_model = "choice",
      data = fx$data,
      preprocessed = prep,
      control_algo = set_algorithm_newton(backend = "r"),
      support_constraint = ~ tie(call_network)
    )),
    "empty risk set at event 1"
  )
})

test_that("a constrained object without a verdict is not estimated", {
  fx <- make_estimate_fixture(n_events = 60L)
  prep <- suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(allowedNet)
  ))
  expect_null(prep$support_verdict$failure)
  prep$support_verdict <- NULL
  expect_error(
    suppressWarnings(estimate_dynam(
      calls_dependent ~ inertia + recip,
      sub_model = "choice",
      data = fx$data,
      preprocessed = prep,
      control_algo = set_algorithm_newton(backend = "r"),
      support_constraint = ~ tie(allowedNet)
    )),
    "preprocess it again"
  )
})

# Four actors, a panel friendship layer, and calls that only the constraint
# reads: N1 calls at t = 1, N2 at t = 2 and t = 3, and with `n2_calls = 3` once
# more at t = 4. Before each call its sender's weighted outdegree on `calls` is
# the number of its earlier calls.
weighted_focal_data <- function(n2_calls = 2L) {
  n2_to <- c(3L, 4L, 1L)[seq_len(n2_calls)]
  list(
    info = list(
      name = "weighted_focal",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = "panel", calls = "event")
    ),
    nodes = data.frame(label = paste0("N", 1:4), mode = "p"),
    ties = rbind(
      data.frame(
        from = 1:3,
        to = 2:4,
        time = NA_real_,
        weight = 1,
        layer = "friendship"
      ),
      data.frame(
        from = c(1L, rep(2L, n2_calls)),
        to = c(3L, n2_to),
        time = seq_len(n2_calls + 1L),
        weight = 1,
        layer = "calls"
      )
    )
  )
}

# The schedules of every `build_joint_schedule()` call that `run` makes.
capture_schedules <- function(run) {
  captured <- list()
  orig_schedule <- build_joint_schedule
  local_mocked_bindings(
    build_joint_schedule = function(
      units,
      shared_objects,
      support_constraints = NULL
    ) {
      schedule <- orig_schedule(units, shared_objects, support_constraints)
      captured[[length(captured) + 1L]] <<- schedule
      schedule
    }
  )
  force(run)
  captured
}

test_that("a weighted constraint on its unread focal layer reads calls once", {
  data <- weighted_focal_data()
  fit <- NULL
  schedules <- capture_schedules(
    fit <- estimate_dynam(
      calls ~ tie(friendship),
      sub_model = "choice",
      data = data,
      support_constraint = ~ outdeg(calls, type = "ego", weighted = TRUE) < 2
    )
  )

  expect_s3_class(fit, "goldfishFit")
  n_calls <- sum(data$ties$layer == "calls")
  expect_gt(length(schedules), 0L)
  for (schedule in schedules) {
    expect_identical(
      sum(!schedule$dependent & schedule$layer == "calls"),
      n_calls
    )
  }
})

test_that("an unweighted constraint on its unread focal layer still bites", {
  expect_error(
    estimate_dynam(
      calls ~ tie(friendship),
      sub_model = "choice",
      data = weighted_focal_data(),
      support_constraint = ~ outdeg(calls, type = "ego") < 1
    ),
    "empty risk set at event 3"
  )
})

test_that("a weighted focal constraint excludes at the observed count", {
  # N2's third call is event 4, made with weighted outdegree 2.
  expect_error(
    estimate_dynam(
      calls ~ tie(friendship),
      sub_model = "choice",
      data = weighted_focal_data(n2_calls = 3L),
      support_constraint = ~ outdeg(calls, type = "ego", weighted = TRUE) < 2
    ),
    "empty risk set at event 4"
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

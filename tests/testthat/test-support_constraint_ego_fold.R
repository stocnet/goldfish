# Ego-kind (outer-encoded) DyNAM-choice support_constraint fold equivalence.
# An ego-kind atom is sender-axis (row-constant): it gates only the sender axis
# and leaves the receiver axis untouched, so the choice availability factorizes
# into two vectors -- receiver presence (f2, `active_dyad`) and sender presence
# (f1, `active_sender`) -- and no dyad-shaped n1 x n2 object is allocated. This
# is the `"outer"` encoding the decision function already returns for this case;
# the fold now produces it instead of the dense `"point"` grid it used to. The
# constraint rides the folded `active_dyad` buffer on every engine, and no
# standalone mask reaches estimation.
#
# `_fixtures/ego_outer_standalone_ref.rds` freezes the fit from the OLD
# standalone `mask_to_opportunities` path (captured before the fold landed); the
# folded path must reproduce it to 1e-10 per event.

make_ego_fold_fixture <- function(n_events = 80L) {
  suppressWarnings({
    data("Social_Evolution", package = "goldfish", envir = environment())
    actors <- get("actors", environment())
    calls <- get("calls", environment())
    call_network <- make_network(nodes = actors, directed = TRUE)
    call_network <- link_events(call_network, calls, nodes = actors)
    calls_dependent <- make_dependent_events(
      calls,
      nodes = actors,
      default_network = call_network
    )
    calls_dependent <- calls_dependent[seq_len(n_events), ]
    data <- make_data(calls_dependent, call_network, calls, actors)
  })
  # An always-true ego-kind atom: `indeg(net, type = "ego") >= 0` classifies as
  # sender-axis (mask_kind 2 -> outer encoding) and never excludes an observed
  # sender, so the constraint is a valid identity ego mask. Built inside the
  # fixture so the formula's environment retains `call_network`.
  ego_constraint <- ~ indeg(call_network, type = "ego") >= 0
  list(
    data = data,
    formula = calls_dependent ~ inertia + recip,
    constraint = ego_constraint
  )
}

fit_ego <- function(fx, backend = "r", constrained = TRUE) {
  args <- list(
    fx$formula,
    sub_model = "choice",
    data = fx$data,
    control_algo = set_algorithm_newton(
      return_interval_loglik = TRUE,
      backend = backend
    )
  )
  if (constrained) {
    args$support_constraint <- fx$constraint
  }
  suppressWarnings(do.call(estimate_dynam, args))
}

test_that("an ego-kind choice constraint folds to the outer encoding", {
  fx <- make_ego_fold_fixture()
  prep <- suppressWarnings(estimate_dynam(
    fx$formula,
    sub_model = "choice",
    data = fx$data,
    support_constraint = fx$constraint,
    preprocessing_only = TRUE
  ))
  expect_true(isTRUE(prep$active_dyad_folded))
  # The decision function returns "outer" for an ego-kind choice constraint;
  # the fold now produces it. The availability is two factor vectors, so
  # `active_dyad` is the length-n2 receiver vector, NOT a dense n1 x n2 grid.
  expect_identical(prep$active_dyad_encoding, "outer")
  expect_false(is.matrix(prep$active_dyad_init))
  expect_length(prep$active_dyad_init, length(prep$active_sender_init))

  # Moving the encoding expectation to match new behavior is how a regression
  # gets ratified, so the edit is licensed only by the fit holding: the folded
  # outer path reproduces the frozen standalone-mask reference to 1e-10.
  ref <- readRDS(test_path("_fixtures", "ego_outer_standalone_ref.rds"))
  fit <- fit_ego(fx, backend = "r")
  expect_equal(fit$parameters, ref$parameters, tolerance = 1e-10)
  expect_equal(fit$interval_log_lik, ref$interval_log_lik, tolerance = 1e-10)
})

test_that("folded ego fit reproduces the captured standalone-mask reference", {
  ref <- readRDS(test_path("_fixtures", "ego_outer_standalone_ref.rds"))
  fx <- make_ego_fold_fixture()
  fit <- fit_ego(fx, backend = "r")
  expect_equal(fit$parameters, ref$parameters, tolerance = 1e-10)
  expect_equal(
    as.numeric(fit$log_likelihood),
    ref$log_likelihood,
    tolerance = 1e-10
  )
  expect_equal(fit$standard_errors, ref$standard_errors, tolerance = 1e-10)
  # per-event log-likelihood: the representation-invariant per-event quantity.
  expect_equal(fit$interval_log_lik, ref$interval_log_lik, tolerance = 1e-10)
})

test_that("folded ego constraint agrees across engines", {
  fx <- make_ego_fold_fixture()
  r <- fit_ego(fx, backend = "r")
  gather <- fit_ego(fx, backend = "gather")
  cpp <- fit_ego(fx, backend = "cpp")
  expect_equal(gather$parameters, r$parameters, tolerance = 1e-8)
  expect_equal(cpp$parameters, r$parameters, tolerance = 1e-8)
  expect_equal(gather$interval_log_lik, r$interval_log_lik, tolerance = 1e-8)
  expect_equal(cpp$interval_log_lik, r$interval_log_lik, tolerance = 1e-8)
})

test_that("an identity ego mask equals the unconstrained fit", {
  fx <- make_ego_fold_fixture()
  constrained <- fit_ego(fx, backend = "r", constrained = TRUE)
  unconstrained <- fit_ego(fx, backend = "r", constrained = FALSE)
  expect_equal(
    constrained$parameters,
    unconstrained$parameters,
    tolerance = 1e-10
  )
  expect_equal(
    constrained$interval_log_lik,
    unconstrained$interval_log_lik,
    tolerance = 1e-10
  )
})

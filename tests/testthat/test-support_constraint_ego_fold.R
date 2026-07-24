# Ego-kind (outer-encoded) DyNAM-choice support_constraint fold equivalence.
# An ego-kind atom is sender-axis (row-constant), so for a choice model its
# support row is constant across receivers and the fold reduces to dense point
# row flips (receiver presence ∩ the sender's support row). This closes the
# formerly-pending outer fold: the constraint now rides the folded `active_dyad`
# buffer on every engine, and no standalone mask reaches estimation.
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

fit_ego <- function(fx, engine = "default", constrained = TRUE) {
  args <- list(
    fx$formula,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(
      return_interval_loglik = TRUE,
      engine = engine
    )
  )
  if (constrained) {
    args$support_constraint <- fx$constraint
  }
  suppressWarnings(do.call(estimate_dynam, args))
}

test_that("an ego-kind choice constraint now folds into active_dyad", {
  fx <- make_ego_fold_fixture()
  prep <- suppressWarnings(estimate_dynam(
    fx$formula,
    sub_model = "choice",
    data = fx$data,
    support_constraint = fx$constraint,
    preprocessing_only = TRUE
  ))
  expect_true(isTRUE(prep$active_dyad_folded))
  expect_identical(prep$active_dyad_encoding, "point")
})

test_that("folded ego fit reproduces the captured standalone-mask reference", {
  ref <- readRDS(test_path("_fixtures", "ego_outer_standalone_ref.rds"))
  fx <- make_ego_fold_fixture()
  fit <- fit_ego(fx, engine = "default")
  expect_equal(fit$parameters, ref$parameters, tolerance = 1e-10)
  expect_equal(
    as.numeric(fit$logLikelihood),
    ref$logLikelihood,
    tolerance = 1e-10
  )
  expect_equal(fit$standardErrors, ref$standardErrors, tolerance = 1e-10)
  # per-event log-likelihood: the representation-invariant per-event quantity.
  expect_equal(fit$intervalLogL, ref$intervalLogL, tolerance = 1e-10)
})

test_that("folded ego constraint agrees across engines", {
  fx <- make_ego_fold_fixture()
  default <- fit_ego(fx, engine = "default")
  gather <- fit_ego(fx, engine = "gather_compute")
  default_c <- fit_ego(fx, engine = "default_c")
  expect_equal(gather$parameters, default$parameters, tolerance = 1e-8)
  expect_equal(default_c$parameters, default$parameters, tolerance = 1e-8)
  expect_equal(gather$intervalLogL, default$intervalLogL, tolerance = 1e-8)
  expect_equal(default_c$intervalLogL, default$intervalLogL, tolerance = 1e-8)
})

test_that("an identity ego mask equals the unconstrained fit", {
  fx <- make_ego_fold_fixture()
  constrained <- fit_ego(fx, engine = "default", constrained = TRUE)
  unconstrained <- fit_ego(fx, engine = "default", constrained = FALSE)
  expect_equal(
    constrained$parameters,
    unconstrained$parameters,
    tolerance = 1e-10
  )
  expect_equal(
    constrained$intervalLogL,
    unconstrained$intervalLogL,
    tolerance = 1e-10
  )
})

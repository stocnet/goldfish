# `ego(attribute)` reachable in DyNAM choice.
# `init_DyNAM_choice.ego` / `update_DyNAM_choice_ego` are thin aliases of the
# REM-choice binding, closing the `Unknown effect ego` dispatch hole so `ego()`
# works as an interaction operand and a support_constraint atom, WITHOUT a
# change in identification (a bare `ego` main effect stays rejected in softmax).

test_that("DyNAM choice ego(covariate) equals the REM-derived expansion", {
  form <- depNetwork ~ ego(actors_ex$attr1)
  choice_ego <- estimate_wrapper(
    form,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  # REM rate_ordered stores dependent events only (no right-censored rows), so
  # it is directly comparable to the choice preprocessing for the same effect.
  rem_ego <- estimate_rem(
    form,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(
    choice_ego$initial_stats,
    rem_ego$initial_stats,
    tolerance = 1e-6,
    label = "initial ego covariate statistic matches REM"
  )
  expect_equal(
    ReducePreprocess(choice_ego),
    ReducePreprocess(rem_ego),
    tolerance = 1e-6
  )
})

test_that("the ego covariate broadcasts the sender attribute to receivers", {
  choice_ego <- estimate_wrapper(
    depNetwork ~ ego(actors_ex$attr1),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  stat <- choice_ego$initial_stats[,, 1]
  n <- nrow(stat)
  # attr1's start value broadcast down each sender's row (one-mode diagonal 0).
  hand <- matrix(actors_ex$attr1, nrow = n, ncol = n, byrow = FALSE)
  diag(hand) <- 0
  expect_equal(stat, hand, tolerance = 1e-6)
})

test_that("ego() resolves as an interaction operand (no Unknown effect ego)", {
  expect_no_error(
    estimate_wrapper(
      depNetwork ~ inertia + ego(actors_ex$attr1):inertia,
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    )
  )
})

# A Social_Evolution fixture (an ego flag + a static allowed-dyad network) for
# the ego-atom support_constraint and the bare-ego identification guard.
make_ego_choice_fixture <- function(n_events = 120L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  actors$active_flag <- 1
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
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  allowedNet <- make_network(matrix = allowed, nodes = actors, directed = TRUE)
  make_data(calls_dependent, call_network, calls, actors, allowedNet)
}

test_that("an ego() atom in a choice support_constraint parses/constrains", {
  data <- make_ego_choice_fixture()
  # `ego(active_flag)` is all-TRUE, so the constraint reduces to
  # `tie(allowedNet)` (all off-diagonal dyads allowed) — same path, unblocked.
  m_cstr <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = data,
    control_algo = set_algorithm_newton(backend = "r"),
    support_constraint = ~ ego(actors$active_flag) & tie(allowedNet)
  )
  m_unc <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = data,
    control_algo = set_algorithm_newton(backend = "r")
  )
  # a vacuous ego ∩ full-allowed constraint leaves the estimate unchanged.
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-6)
})

test_that("a bare ego() main effect in choice is still rejected (softmax)", {
  data <- make_ego_choice_fixture(n_events = 60L)
  # dispatch succeeds (preprocessing computes the column), identification fails.
  expect_no_error(
    estimate_dynam(
      calls_dependent ~ inertia + ego(actors$active_flag),
      sub_model = "choice",
      data = data,
      preprocessing_only = TRUE
    )
  )
  expect_error(
    estimate_dynam(
      calls_dependent ~ inertia + ego(actors$active_flag),
      sub_model = "choice",
      data = data,
      control_algo = set_algorithm_newton(backend = "r")
    ),
    "ego"
  )
})

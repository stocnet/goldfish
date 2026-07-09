# `ego(attribute)` reachable in DyNAM choice (design D9).
# `init_DyNAM_choice.ego` / `update_DyNAM_choice_ego` are thin aliases of the
# REM-choice binding, closing the `Unknown effect ego` dispatch hole so `ego()`
# works as an interaction operand and a support_constraint atom, WITHOUT a
# change in identification (a bare `ego` main effect stays rejected in softmax).

test_that("DyNAM choice ego(covariate) equals the REM-derived expansion", {
  form <- depNetwork ~ ego(actorsEx$attr1)
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
    choice_ego$initialStats,
    rem_ego$initialStats,
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
    depNetwork ~ ego(actorsEx$attr1),
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  stat <- choice_ego$initialStats[,, 1]
  n <- nrow(stat)
  # attr1's start value broadcast down each sender's row (one-mode diagonal 0).
  hand <- matrix(actorsEx$attr1, nrow = n, ncol = n, byrow = FALSE)
  diag(hand) <- 0
  expect_equal(stat, hand, tolerance = 1e-6)
})

test_that("ego() resolves as an interaction operand (no Unknown effect ego)", {
  expect_no_error(
    estimate_wrapper(
      depNetwork ~ inertia + ego(actorsEx$attr1):inertia,
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
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  allowedNet <- make_network(matrix = allowed, nodes = actors, directed = TRUE)
  make_data(callsDependent, callNetwork, calls, actors, allowedNet)
}

test_that("an ego() atom in a choice support_constraint parses/constrains", {
  data <- make_ego_choice_fixture()
  # `ego(active_flag)` is all-TRUE, so the constraint reduces to
  # `tie(allowedNet)` (all off-diagonal dyads allowed) — same path, unblocked.
  m_cstr <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = data,
    control_estimation = set_estimation_opt(engine = "default"),
    support_constraint = ~ ego(actors$active_flag) & tie(allowedNet)
  )
  m_unc <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = data,
    control_estimation = set_estimation_opt(engine = "default")
  )
  # a vacuous ego ∩ full-allowed constraint leaves the estimate unchanged.
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-6)
})

test_that("a bare ego() main effect in choice is still rejected (softmax)", {
  data <- make_ego_choice_fixture(n_events = 60L)
  # dispatch succeeds (preprocessing computes the column), identification fails.
  expect_no_error(
    estimate_dynam(
      callsDependent ~ inertia + ego(actors$active_flag),
      sub_model = "choice",
      data = data,
      preprocessing_only = TRUE
    )
  )
  expect_error(
    estimate_dynam(
      callsDependent ~ inertia + ego(actors$active_flag),
      sub_model = "choice",
      data = data,
      control_estimation = set_estimation_opt(engine = "default")
    ),
    "ego"
  )
})

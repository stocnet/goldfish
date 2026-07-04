# Incremental support-mask maintenance (task 3.1) + from-scratch equivalence
# (task 3.3). The constraint sub-plan is maintained in a self-contained recipe
# pass; at every dependent event its support mask must equal a from-scratch
# evaluation of the constraint on the atoms' current statistics. The pass is
# attached additively to the preprocessed object, so an unconstrained model is
# unaffected.

make_mask_fixture <- function(n_events = 80L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
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
  list(
    data = make_data(callsDependent, callNetwork, calls, actors),
    actors = actors,
    calls = as.data.frame(calls),
    dep_times = callsDependent$time,
    n_events = n_events
  )
}

# Independent from-scratch reference for `~ tie(net)`: the support at a dependent
# event is the cumulative call adjacency (increments) strictly before its time.
tie_support_from_scratch <- function(fx, event_time) {
  lab <- fx$actors$label
  n <- length(lab)
  adj <- matrix(0, n, n)
  prev <- fx$calls[fx$calls$time < event_time, , drop = FALSE]
  for (r in seq_len(nrow(prev))) {
    i <- match(prev$sender[r], lab)
    j <- match(prev$receiver[r], lab)
    adj[i, j] <- adj[i, j] + prev$increment[r]
  }
  adj != 0
}

run_mask_pass <- function(fx) {
  d <- fx$data
  cp <- parse_and_validate_constraint(
    ~ tie(callNetwork),
    has_dyad_part = TRUE,
    envir = d
  )
  sub <- compile_support_constraint(
    cp,
    model = "DyNAM",
    sub_model = "choice",
    stat_kind = "dyad",
    dep_name = "callsDependent",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = d
  )
  preprocess_support_mask(
    sub,
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    symmetric = FALSE,
    prepEnvir = d
  )
}

test_that("the initial mask is empty for an initially-empty tie network", {
  fx <- make_mask_fixture()
  mt <- run_mask_pass(fx)
  expect_false(any(mt$initial))
  expect_identical(mt$n_dependent, fx$n_events)
  expect_length(mt$support, fx$n_events)
})

test_that("incrementally maintained mask equals from-scratch at every event", {
  fx <- make_mask_fixture()
  mt <- run_mask_pass(fx)
  for (e in seq_len(fx$n_events)) {
    ref <- tie_support_from_scratch(fx, fx$dep_times[e])
    expect_equal(unname(mt$support[[e]]), unname(ref))
  }
})

test_that("a constrained model attaches support_mask; unconstrained does not", {
  fx <- make_mask_fixture()
  prep_c <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(callNetwork)
  )
  prep_u <- estimate_dynam(
    callsDependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE
  )
  expect_null(prep_u$support_mask)
  expect_false(is.null(prep_c$support_mask))
  expect_length(prep_c$support_mask$support, fx$n_events)
  # the statistics output is unchanged by attaching the mask
  expect_equal(prep_c$initialStats, prep_u$initialStats)
})

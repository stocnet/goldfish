# support_constraint consumption for DyNAM-rate on the default engine (tasks
# 4.3, 5.2, 5.3, 5.4). The mask reduces to a per-event sender gate (a sender is
# at risk only with >= 1 allowed present receiver, design D3/D10), routed through
# the same sender `keepIn` filter presence uses. An all-allowing constraint is an
# identity (equals unconstrained); a restricting one excludes gated-out senders
# from the rate denominator and the constrained `avg_active_actors`; a dependent
# event whose own sender is gated out errors (design D8).

make_rate_fixture <- function(n_events = 120L) {
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
  observed_senders <- unique(match(
    as.data.frame(callsDependent)$sender,
    lab
  ))
  list(
    actors = actors,
    calls = calls,
    callNetwork = callNetwork,
    callsDependent = callsDependent,
    lab = lab,
    n = n,
    observed_senders = observed_senders
  )
}

# Build a data object carrying an allowed-dyad network with the given sender rows
# zeroed out (those senders have no allowed receiver -> gated out).
rate_data_with_gate <- function(fx, gated = integer(0)) {
  allowed <- matrix(1, fx$n, fx$n, dimnames = list(fx$lab, fx$lab))
  diag(allowed) <- 0
  if (length(gated) > 0) {
    allowed[gated, ] <- 0
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

test_that("an all-allowing rate constraint is an identity (equals unconstrained)", {
  fx <- make_rate_fixture()
  d <- rate_data_with_gate(fx)
  opt <- set_estimation_opt(engine = "default")
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "callsDependent",
    support_constraint = ~ tie(allowedNet),
    data = d
  )
  m_cstr <- estimate_dynam(spec, sub_model = "rate", control_estimation = opt)
  m_unc <- estimate_dynam(
    callsDependent ~ 1 + indeg,
    sub_model = "rate",
    data = d,
    control_estimation = opt
  )
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-8)
  expect_equal(m_cstr$logLikelihood, m_unc$logLikelihood, tolerance = 1e-8)
})

test_that("a restricting rate gate gives the hand-computed avg_active_actors (D4/5.4)", {
  fx <- make_rate_fixture()
  gated <- setdiff(seq_len(fx$n), fx$observed_senders)[1:5]
  d <- rate_data_with_gate(fx, gated)
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "callsDependent",
    support_constraint = ~ tie(allowedNet),
    data = d
  )
  prep <- estimate_dynam(spec, sub_model = "rate", preprocessing_only = TRUE)
  gate <- mask_to_sender_gate(prep$support_mask, prep$active_sender_init)
  hand_avg <- mean(vapply(
    gate,
    function(g) sum(prep$active_sender_init & g),
    numeric(1)
  ))
  # all actors present, 5 gated out -> 79 active senders at every event
  expect_equal(hand_avg, fx$n - length(gated))
  expect_equal(
    constrained_avg_active_actors(
      gate,
      prep$active_sender_init
    ),
    hand_avg
  )
})

test_that("a restricting rate gate changes the estimate vs unconstrained", {
  fx <- make_rate_fixture()
  gated <- setdiff(seq_len(fx$n), fx$observed_senders)[1:5]
  d <- rate_data_with_gate(fx, gated)
  opt <- set_estimation_opt(engine = "default")
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "callsDependent",
    support_constraint = ~ tie(allowedNet),
    data = d
  )
  # the 5 gated-out senders are never at risk -> a case-E warning (design D8)
  m_cstr <- suppressWarnings(
    estimate_dynam(spec, sub_model = "rate", control_estimation = opt)
  )
  m_unc <- estimate_dynam(
    callsDependent ~ 1 + indeg,
    sub_model = "rate",
    data = d,
    control_estimation = opt
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("gather_compute consumes the rate constraint natively (== default)", {
  fx <- make_rate_fixture()
  gated <- setdiff(seq_len(fx$n), fx$observed_senders)[1:5]
  d <- rate_data_with_gate(fx, gated)
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "callsDependent",
    support_constraint = ~ tie(allowedNet),
    data = d
  )
  m_def <- suppressWarnings(estimate_dynam(
    spec,
    sub_model = "rate",
    control_estimation = set_estimation_opt(engine = "default")
  ))
  m_gc <- suppressWarnings(estimate_dynam(
    spec,
    sub_model = "rate",
    control_estimation = set_estimation_opt(engine = "gather_compute")
  ))
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("a dependent event whose own sender is gated out errors (design D8)", {
  fx <- make_rate_fixture()
  # gate out an OBSERVED sender: the event where it acts has an empty risk set.
  gated <- fx$observed_senders[1]
  d <- rate_data_with_gate(fx, gated)
  opt <- set_estimation_opt(engine = "default")
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "callsDependent",
    support_constraint = ~ tie(allowedNet),
    data = d
  )
  expect_error(
    estimate_dynam(spec, sub_model = "rate", control_estimation = opt)
  )
})

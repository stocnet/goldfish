# active_dyad encoding decision and the DyNAM-choice
# alter-encoding fold + default-engine consumption.
# The point encoding (dense) and the REM/outer folds are wired with the engine
# consumption phase (§5); here choice folds a receiver-axis (alter/scalar) mask
# into `active_dyad` at the alter encoding.

# ---- 4.1: static encoding decision ----

test_that("active_dyad_encoding_decide reproduces the pre-fold assignment", {
  # unconstrained: the spec descriptor's base encoding passes straight through
  # -- receiver-only families (choice) are "alter", dyadic-risk-set families
  # (REM / REM-ordered / coordination) are "outer".
  expect_identical(active_dyad_encoding_decide("alter"), "alter")
  expect_identical(active_dyad_encoding_decide("outer"), "outer")
})

test_that("active_dyad_encoding_decide folds the constraint axis-union kind", {
  # mask_kind: 0 point, 1 alter, 2 ego, 3 scalar.
  # choice (base "alter", receiver axis only): alter/scalar stay alter; ego adds
  # a sender factor -> outer; point -> point.
  expect_identical(active_dyad_encoding_decide("alter", 1L), "alter")
  expect_identical(active_dyad_encoding_decide("alter", 3L), "alter")
  expect_identical(active_dyad_encoding_decide("alter", 2L), "outer")
  expect_identical(active_dyad_encoding_decide("alter", 0L), "point")
  # dyadic base ("outer") already folds both presences -> stays outer, unless a
  # point atom forces point.
  expect_identical(active_dyad_encoding_decide("outer", 1L), "outer")
  expect_identical(active_dyad_encoding_decide("outer", 2L), "outer")
  expect_identical(active_dyad_encoding_decide("outer", 0L), "point")
})

test_that("an opportunity list forces the point encoding", {
  expect_identical(
    active_dyad_encoding_decide("alter", 1L, has_opportunity = TRUE),
    "point"
  )
  expect_identical(
    active_dyad_encoding_decide("alter", NULL, has_opportunity = TRUE),
    "point"
  )
})

# ---- 4.2: DyNAM-choice alter fold + consumption ----

make_choice_fold_fixture <- function(n_events = 80L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  callsDep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  callsDep <- callsDep[seq_len(n_events), ]
  list(
    data = make_data(callsDep, call_network, calls, actors),
    call_network = call_network,
    n2 = nrow(actors)
  )
}

test_that("a receiver-axis constraint folds active_dyad at the alter encoding", {
  fx <- make_choice_fold_fixture()
  prep <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ indeg(call_network) > 0,
    preprocessing_only = TRUE
  )
  expect_identical(prep$active_dyad_encoding, "alter")
  expect_true(isTRUE(prep$active_dyad_folded))
  # alter is a length-n2 vector — no dense n1 x n2 allocation.
  expect_true(is.null(dim(prep$active_dyad_init)))
  expect_length(prep$active_dyad_init, fx$n2)
})

test_that("the folded active_dyad equals the from-scratch intersection every event", {
  fx <- make_choice_fold_fixture()
  prep <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ indeg(call_network) > 0,
    preprocessing_only = TRUE
  )
  supp <- prep$support_mask$support
  recv0 <- prep$support_mask$receiver_presence_init
  cur <- prep$active_dyad_init
  ptr <- prep$active_dyad_update_pointer
  upd <- prep$active_dyad_update
  prev <- 0L
  for (e in seq_along(prep$event_time)) {
    hi <- ptr[e]
    if (hi > prev) {
      cols <- (prev + 1L):hi
      cur[upd[1L, cols]] <- as.logical(upd[2L, cols])
    }
    prev <- hi
    # from-scratch: receiver presence AND the alter-broadcast support row
    expect_identical(cur, recv0 & supp[[e]][1L, ])
  }
})

test_that("a vacuous alter constraint reproduces the unconstrained fit (fold consumed)", {
  fx <- make_choice_fold_fixture()
  opt <- set_algorithm_newton(engine = "default")
  m_cstr <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    support_constraint = ~ indeg(call_network) >= 0,
    control_algo = opt
  )
  m_unc <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_algo = opt
  )
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-8)
  expect_equal(m_cstr$logLikelihood, m_unc$logLikelihood, tolerance = 1e-8)
})

# ---- 4.5: from-scratch equivalence at every encoding + invariants ----
# Encodings reachable after §4.2/§4.3: alter (choice receiver-axis fold, covered
# above), point (opportunity list, §4.3), and outer (unconstrained REM base).
# The support-point / choice-ego-outer / REM-support folds land with §5, so they
# are not exercised here.

make_opp_fold_fixture <- function(n_events = 60L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  callsDep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  callsDep <- callsDep[seq_len(n_events), ]
  n <- nrow(actors)
  senders <- match(as.data.frame(callsDep)$sender, actors$label)
  # A sender-dependent opportunity set: every receiver except the sender itself
  # and its cyclic successor — so `active_dyad`'s row genuinely varies by event.
  opp <- lapply(senders, function(s) setdiff(seq_len(n), c(s, (s %% n) + 1L)))
  list(
    data = make_data(callsDep, call_network, calls, actors),
    opp = opp,
    n = n,
    senders = senders
  )
}

test_that("opportunity folds active_dyad at the point encoding (dense)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  fx <- make_opp_fold_fixture()
  prep <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    control_algo = set_algorithm_newton(engine = "default"),
    control_prep = set_preprocessing(opportunities_list = fx$opp)
  )
  expect_identical(prep$active_dyad_encoding, "point")
  expect_true(isTRUE(prep$active_dyad_folded))
  # A dense n1 x n2 init and a 3-row (node1, node2, replace) buffer appear ONLY
  # at the point encoding.
  expect_true(is.matrix(prep$active_dyad_init))
  expect_identical(dim(prep$active_dyad_init), c(fx$n, fx$n))
  expect_identical(nrow(prep$active_dyad_update), 3L)
})

test_that("folded point buffer equals from-scratch intersection every event", {
  withr::local_options(lifecycle_verbosity = "quiet")
  fx <- make_opp_fold_fixture()
  prep <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    control_algo = set_algorithm_newton(engine = "default"),
    control_prep = set_preprocessing(opportunities_list = fx$opp)
  )
  # Receiver presence base = the unconstrained model's alter vector (no
  # composition change in this fixture, so it is constant across events).
  base <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE
  )
  recv0 <- base$active_dyad_init
  n2 <- fx$n
  cur <- prep$active_dyad_init
  ptr <- prep$active_dyad_update_pointer
  upd <- prep$active_dyad_update
  senders <- prep$event_sender
  prev <- 0L
  for (e in seq_along(prep$event_time)) {
    hi <- ptr[e]
    if (hi > prev) {
      cols <- (prev + 1L):hi
      # Apply this event's pre-likelihood slice before the read.
      cur[cbind(upd[1L, cols], upd[2L, cols])] <- as.logical(upd[3L, cols])
    }
    prev <- hi
    desired <- recv0 & (seq_len(n2) %in% fx$opp[[e]])
    # Row read for the event's sender equals the from-scratch intersection,
    # including event 1 (whose value rides in the init, its slice empty).
    expect_identical(as.logical(cur[senders[[e]], ]), desired)
  }
})

make_rem_fold_fixture <- function(n_events = 60L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
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
  list(
    data = make_data(calls_dependent, call_network, calls, actors),
    present = actors$present,
    n = nrow(actors)
  )
}

test_that("unconstrained REM: active_dyad outer, factors == presences", {
  fx <- make_rem_fold_fixture()
  prep <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = fx$data,
    preprocessing_only = TRUE
  )
  expect_identical(prep$active_dyad_encoding, "outer")
  # No dense allocation for a non-point encoding: both factors are vectors.
  expect_true(is.null(dim(prep$active_dyad_init)))
  expect_true(is.null(dim(prep$active_sender_init)))
  # The two outer factors ARE today's two presence vectors: sender presence
  # (f1 = active_sender) and receiver presence (f2 = active_dyad).
  expect_identical(as.logical(prep$active_sender_init), fx$present)
  expect_identical(as.logical(prep$active_dyad_init), fx$present)
})

test_that("outer-encoded active_dyad never receives point flips", {
  fx <- make_rem_fold_fixture()
  prep <- estimate_rem(
    calls_dependent ~ 1 + inertia + recip,
    sub_model = "rate",
    data = fx$data,
    preprocessing_only = TRUE
  )
  expect_identical(prep$active_dyad_encoding, "outer")
  # Outer updates are factor flips (a 2-row (node, replace) buffer) — never the
  # 3-row (node1, node2, replace) point buffer the point encoding alone uses.
  if (
    !is.null(prep$active_dyad_update) && length(prep$active_dyad_update) > 0
  ) {
    expect_false(identical(nrow(prep$active_dyad_update), 3L))
  }
})

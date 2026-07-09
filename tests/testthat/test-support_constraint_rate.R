# support_constraint consumption for DyNAM-rate on the default engine (tasks
# 4.3, 5.2, 5.3, 5.4). The mask reduces to a per-event sender gate (a sender is
# at risk only with >= 1 allowed present receiver, design D3/D10), routed through
# the same sender `keepIn` filter presence uses. An all-allowing constraint is an
# identity (equals unconstrained); a restricting one excludes gated-out senders
# from the rate denominator and the constrained `avg_active_entity`; a dependent
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

# Reconstruct the per-event folded active_sender from the stored crossings
# buffer (init + per-event flip slices), the same walk the engine performs.
folded_active_sender_per_event <- function(prep) {
  n_stored <- length(prep$event_time)
  cur <- prep$active_sender_init
  upd <- prep$active_sender_update
  ptr <- prep$active_sender_update_pointer
  out <- vector("list", n_stored)
  prev <- 0L
  for (e in seq_len(n_stored)) {
    hi <- if (!is.null(ptr)) ptr[e] else 0L
    if (hi > prev) {
      cols <- (prev + 1L):hi
      cur[upd[1L, cols]] <- as.logical(upd[2L, cols])
    }
    prev <- hi
    out[[e]] <- cur
  }
  out
}

test_that("the folded active_sender equals the from-scratch gate reduction (D12)", {
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

  # From-scratch predecessor reduction: presence AND
  # (rowSums(support & receiver-availability) > 0), per stored event.
  n1 <- length(prep$active_dyad_init)
  presence <- prep$support_mask$sender_presence_init
  reference <- lapply(prep$support_mask$support, function(s) {
    presence & (rowSums(s & rep(prep$active_dyad_init, each = n1)) > 0)
  })
  folded <- folded_active_sender_per_event(prep)
  expect_equal(folded, reference)

  # avg_active_entity is the event-averaged active-sender count, computed
  # in-loop (no estimation-time recombination). All actors present, 5 gated
  # out -> 79 active senders at every event.
  expect_equal(
    prep$avg_active_entity,
    mean(vapply(reference, sum, numeric(1)))
  )
  expect_equal(prep$avg_active_entity, fx$n - length(gated))
})

test_that("the folded active_sender buffer carries only crossings (D12)", {
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
  # A static gate over a static (present) composition never crosses after the
  # init, so no flip is emitted.
  expect_true(isTRUE(prep$active_sender_folded))
  expect_equal(ncol(prep$active_sender_update), 0L)
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

test_that("default_c consumes the rate constraint natively (== default)", {
  fx <- make_rate_fixture()
  gated <- setdiff(seq_len(fx$n), fx$observed_senders)[1:5]
  d <- rate_data_with_gate(fx, gated)
  spec <- callsDependent ~ 1 + indeg + outdeg
  m_def <- suppressWarnings(estimate_dynam(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default")
  ))
  # estimate_DyNAM_rate filters senders by the folded active_sender directly, so
  # no downgrade warning fires.
  m_dc <- suppressWarnings(estimate_dynam(
    spec,
    sub_model = "rate",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_estimation = set_estimation_opt(engine = "default_c")
  ))
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-8)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
})

test_that("constrained rate runs natively with no engine-downgrade warning (5.5)", {
  fx <- make_rate_fixture()
  gated <- setdiff(seq_len(fx$n), fx$observed_senders)[1:5]
  d <- rate_data_with_gate(fx, gated)
  spec <- callsDependent ~ 1 + indeg + outdeg
  downgrade_warnings <- function(engine) {
    w <- character(0)
    withCallingHandlers(
      estimate_dynam(
        spec,
        sub_model = "rate",
        data = d,
        support_constraint = ~ tie(allowedNet),
        control_estimation = set_estimation_opt(engine = engine)
      ),
      warning = function(cnd) {
        w <<- c(w, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    grep("does not yet consume", w, value = TRUE)
  }
  expect_identical(downgrade_warnings("gather_compute"), character(0))
  expect_identical(downgrade_warnings("default_c"), character(0))
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

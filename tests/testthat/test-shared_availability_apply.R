# Detector for the folded `active_sender` availability apply.
#
# The availability apply walks a per-event crossings buffer before each event's
# likelihood. The `active_dyad` point crossings are exercised by the
# coordination constraint tests, but the `active_sender` crossings branch fires
# only when a sender enters or leaves the risk set over time, which the static
# constraint fixtures never trigger (they fold entirely into the init and assert
# `ncol(active_sender_update) == 0`). This builds a time-varying gate that
# crosses non-sender actors out mid-stream, so the crossings buffer is
# non-empty, and pins that the r, gather and cpp backends walk it to the same
# fit. It is the sender-axis counterpart of the `active_dyad` agreement the
# constraint suite already covers.

# A rate fixture whose sender gate is time-varying: a handful of actors that are
# never dependent-event senders start with an allowed receiver and lose it at a
# mid-stream event, so each crosses out of the risk set. Non-sender actors are
# chosen so no dependent event's own sender is ever gated out.
make_sender_crossing_fixture <- function(n_events = 120L, n_cross = 5L) {
  suppressWarnings({
    data("Social_Evolution", package = "goldfish", envir = environment())
    actors <- get("actors", environment())
    calls <- get("calls", environment())
    lab <- actors$label
    n <- nrow(actors)
    call_network <- make_network(nodes = actors, directed = TRUE)
    call_network <- link_events(
      call_network,
      change_event = calls,
      nodes = actors
    )
    calls_dependent <- make_dependent_events(
      events = calls,
      nodes = actors,
      default_network = call_network
    )
    calls_dependent <- calls_dependent[seq_len(n_events), ]
    observed_senders <- unique(match(
      as.data.frame(calls_dependent)$sender,
      lab
    ))
    cross_actors <- setdiff(seq_len(n), observed_senders)[seq_len(n_cross)]

    allowed <- matrix(1, n, n, dimnames = list(lab, lab))
    diag(allowed) <- 0
    allowedNet <- make_network(
      matrix = allowed,
      nodes = actors,
      directed = TRUE
    )

    event_times <- as.data.frame(calls_dependent)$time
    cross_time <- event_times[n_events %/% 2L]
    # Strip every allowed receiver of the crossing actors at one mid-stream
    # time: each then has zero allowed receivers and leaves the sender risk set,
    # emitting one crossing per actor in that event's slice.
    change <- do.call(
      rbind,
      lapply(cross_actors, function(a) {
        data.frame(
          time = cross_time,
          sender = lab[a],
          receiver = setdiff(lab, lab[a]),
          replace = 0,
          stringsAsFactors = FALSE
        )
      })
    )
    allowedNet <- link_events(allowedNet, change_event = change, nodes = actors)

    data <- make_data(calls_dependent, call_network, calls, actors, allowedNet)
  })
  list(data = data, n_cross = n_cross)
}

fit_sender_crossing <- function(fx, backend) {
  suppressWarnings({
    spec <- make_specification(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      model = "DyNAM",
      layer = "calls_dependent",
      support_constraint = ~ tie(allowedNet),
      data = fx$data
    )
    estimate_dynam(
      spec,
      sub_model = "rate",
      control_algo = set_algorithm_newton(
        return_interval_loglik = TRUE,
        backend = backend
      )
    )
  })
}

test_that("the time-varying gate emits active_sender crossings", {
  fx <- make_sender_crossing_fixture()
  prep <- suppressWarnings(make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "calls_dependent",
    support_constraint = ~ tie(allowedNet),
    data = fx$data
  ))
  prep <- suppressWarnings(estimate_dynam(
    prep,
    sub_model = "rate",
    preprocessing_only = TRUE
  ))
  expect_true(isTRUE(prep$active_sender_folded))
  # One crossing per gated actor, all at the same event, so the apply walks a
  # multi-column per-event slice rather than a single flip.
  expect_equal(ncol(prep$active_sender_update), fx$n_cross)
})

test_that("active_sender crossings walk to the same fit on r, gather and cpp", {
  fx <- make_sender_crossing_fixture()
  r <- fit_sender_crossing(fx, "r")
  gather <- fit_sender_crossing(fx, "gather")
  cpp <- fit_sender_crossing(fx, "cpp")
  expect_equal(gather$parameters, r$parameters, tolerance = 1e-8)
  expect_equal(cpp$parameters, r$parameters, tolerance = 1e-8)
  expect_equal(gather$interval_log_lik, r$interval_log_lik, tolerance = 1e-8)
  expect_equal(cpp$interval_log_lik, r$interval_log_lik, tolerance = 1e-8)
})

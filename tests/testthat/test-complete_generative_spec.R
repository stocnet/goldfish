# Generative-readiness completion (D9 / D9a): complete_generative_spec() fills a
# half-specified / rate-only / choice-only DyNAM flavor with a zero-free-parameter
# default at a consumer entry, pins a missing timed rate from the shared panel
# risk-set helper, marks completed fids, and applies the D4 separability rule.
# The single-process estimation path is deliberately untouched.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# --- Fixtures ----------------------------------------------------------------

# A directed one-mode object: friendship (observation configurable, no flavor)
# and calls (event). Built with friendship = "event" so a friendship-focal spec
# clears make_specification()'s panel-focal guard, then composed under
# friendship = "panel".
plain_data <- function(friendship = "panel") {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(friendship = "increment", calls = "increment"),
    directed = c(friendship = TRUE, calls = TRUE),
    observation = c(friendship = friendship, calls = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# A timed join: friendship (modeled panel, choice-only -> completes to a pinned
# rate) + calls (rate-only reading friendship -> completes to a uniform choice).
timed_panel_join <- function() {
  ev <- plain_data("event")
  fr <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = ev
  )
  calls <- make_specification(
    rate = ~ 1 + indeg(friendship),
    layer = "calls",
    model = "DyNAM",
    data = ev
  )
  make_joint_specification(fr, calls, data = plain_data("panel"))
}

# A join with a rate-only calls process (needs a uniform choice) and an
# emails choice-only process (event, so the join is timed via calls' rate); both
# read friendship, a panel covariate modeled by neither.
rate_only_choice_join <- function() {
  data <- plain_data("panel")
  data$ties <- rbind(
    data$ties,
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  data$info$update <- c(data$info$update, emails = "increment")
  data$info$directed <- c(data$info$directed, emails = TRUE)
  data$info$observation <- c(data$info$observation, emails = "event")
  calls <- make_specification(
    rate = ~ 1 + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls, emails, data = data)
}

# --- Uniform choice ----------------------------------------------------------

test_that("a rate-only flavor completes to a uniform choice with a warning", {
  local_cli_context()
  js <- rate_only_choice_join()
  expect_warning(
    complete_generative_spec(js, consumer = "estimate_dynes"),
    class = "goldfish_completed_default_warning"
  )
  cc <- suppressWarnings(complete_generative_spec(
    js,
    consumer = "estimate_dynes"
  ))
  map <- cc$process_map
  # The calls process gains a choice fid, auto-supplied and zero-effect.
  calls_choice <- map$fid[map$layer == "calls" & map$family == "choice"]
  expect_length(calls_choice, 1L)
  expect_true(map$completed[map$fid == calls_choice])
  bundle <- goldfish:::joint_fid_bundles(cc)[[as.character(
    calls_choice
  )]]$bundle
  expect_identical(bundle$sub_model, "choice")
  expect_length(bundle$parsed$rhs_names, 0L)
  expect_false(bundle$has_intercept)
})

test_that("the completion warning snapshot names layer/family/default", {
  local_cli_context()
  js <- rate_only_choice_join()
  expect_snapshot(
    invisible(complete_generative_spec(js, consumer = "estimate_dynes"))
  )
})

test_that("a completed uniform choice inherits the layer's support constraint", {
  # calls carries a user support constraint; its auto-supplied choice must
  # inherit that same constraint (one constraint_id shared with the rate fid),
  # never fabricate or borrow another process's.
  data <- plain_data("panel")
  calls <- make_specification(
    rate = ~ 1 + tie(friendship),
    support_constraint = ~ !tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_data <- data
  fr <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = plain_data("event")
  )
  js <- make_joint_specification(calls, fr, data = data)
  cc <- suppressWarnings(complete_generative_spec(js))
  map <- cc$process_map
  calls_rows <- map[map$layer == "calls", , drop = FALSE]
  # The completed choice shares the rate's constraint_id (the process constraint),
  # so it inherits, not fabricates.
  expect_identical(
    calls_rows$constraint_id[calls_rows$family == "choice"],
    calls_rows$constraint_id[calls_rows$family == "rate"]
  )
})

# --- Timed pinned rate -------------------------------------------------------

test_that("a missing timed rate completes to a pinned intercept-only rate", {
  js <- timed_panel_join()
  cc <- suppressWarnings(
    complete_generative_spec(
      js,
      consumer = "estimate_dynes",
      wave_times = c(0, 2, 5)
    )
  )
  map <- cc$process_map
  fr_rate <- map$fid[map$layer == "friendship" & map$family == "rate"]
  expect_length(fr_rate, 1L)
  expect_true(map$completed[map$fid == fr_rate])
  expect_true(map$pinned[map$fid == fr_rate])
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  expect_s3_class(rate, "intercept_only_rate")
  # Zero free parameters: the pin contributes the empty theta block.
  expect_length(intercept_only_rate_theta_block(rate), 0L)
  # The per-period pin reproduces its count: exp(intercept_w) * T_w * |R_w| = count_w.
  rs <- goldfish:::panel_wave_risk_set(
    cc,
    layer = "friendship",
    flavor = NA,
    entity = "sender",
    wave_times = c(0, 2, 5)
  )
  expect_equal(
    exp(rate$intercept) * rs$duration * rs$risk_set_size,
    rs$count
  )
})

test_that("a timed choice-only flavor reaches the pinned primitive; ordered does not", {
  # Timed branch: friendship's missing rate is pinned (mark_pinned_rates ran).
  js <- timed_panel_join()
  cc <- suppressWarnings(complete_generative_spec(js, wave_times = c(0, 2, 5)))
  expect_true(any(cc$process_map$pinned))
  expect_length(cc$completed_rates, 1L)

  # Ordered branch: an all-ordered composition never reaches the pinned
  # primitive -- no pinned column, no completed rates.
  data <- plain_data("panel")
  a <- make_specification(
    rate = ~ 1 + indeg(friendship),
    rate_sub_model = "rate_ordered",
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  b <- make_specification(
    rate = ~ 1 + inertia,
    rate_sub_model = "rate_ordered",
    layer = "friendship",
    model = "DyNAM",
    data = plain_data("event")
  )
  js_ord <- make_joint_specification(a, b, data = data)
  expect_false(goldfish:::is_timed_joint_specification(js_ord))
  cc_ord <- complete_generative_spec(js_ord)
  expect_null(cc_ord$process_map$pinned)
  expect_null(cc_ord$completed_rates)
})

# --- D1/D2 numeric time coercion ---------------------------------------------

# `timed_panel_join()`'s friendship/calls topology with `ties$time` cast to a
# POSIXct or Date axis, so the D1 coercion path (`panel_wave_risk_set()`,
# `default_window()`) is exercised on the non-numeric time classes that
# previously crashed.
timed_panel_join_cast <- function(time_class = c("POSIXct", "Date")) {
  time_class <- match.arg(time_class)
  cast <- function(data) {
    t <- data$ties$time
    data$ties$time <- if (identical(time_class, "POSIXct")) {
      as.POSIXct("2020-01-01", tz = "GMT") + t * 86400
    } else {
      as.Date("2020-01-01") + t
    }
    data
  }
  ev <- cast(plain_data("event"))
  fr <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = ev
  )
  calls <- make_specification(
    rate = ~ 1 + indeg(friendship),
    layer = "calls",
    model = "DyNAM",
    data = ev
  )
  make_joint_specification(fr, calls, data = cast(plain_data("panel")))
}

cast_wave_times <- function(t, time_class = c("POSIXct", "Date")) {
  time_class <- match.arg(time_class)
  if (identical(time_class, "POSIXct")) {
    as.POSIXct("2020-01-01", tz = "GMT") + t * 86400
  } else {
    as.Date("2020-01-01") + t
  }
}

test_that("a completed rate pins on POSIXct data (explicit wave grid)", {
  js <- timed_panel_join_cast("POSIXct")
  wave_times <- cast_wave_times(c(0, 2, 5), "POSIXct")
  cc <- suppressWarnings(
    complete_generative_spec(
      js,
      consumer = "estimate_dynes",
      wave_times = wave_times
    )
  )
  fr_rate <- cc$process_map$fid[
    cc$process_map$layer == "friendship" & cc$process_map$family == "rate"
  ]
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  expect_true(all(is.finite(rate$intercept)))
  rs <- goldfish:::panel_wave_risk_set(
    cc,
    layer = "friendship",
    flavor = NA,
    entity = "sender",
    wave_times = wave_times
  )
  expect_type(rs$duration, "double")
  expect_equal(
    exp(rate$intercept) * rs$duration * rs$risk_set_size,
    rs$count
  )
})

test_that("a completed rate pins on Date data (explicit wave grid)", {
  js <- timed_panel_join_cast("Date")
  wave_times <- cast_wave_times(c(0, 2, 5), "Date")
  cc <- suppressWarnings(
    complete_generative_spec(
      js,
      consumer = "estimate_dynes",
      wave_times = wave_times
    )
  )
  fr_rate <- cc$process_map$fid[
    cc$process_map$layer == "friendship" & cc$process_map$family == "rate"
  ]
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  expect_true(all(is.finite(rate$intercept)))
  rs <- goldfish:::panel_wave_risk_set(
    cc,
    layer = "friendship",
    flavor = NA,
    entity = "sender",
    wave_times = wave_times
  )
  expect_type(rs$duration, "double")
  expect_equal(
    exp(rate$intercept) * rs$duration * rs$risk_set_size,
    rs$count
  )
})

test_that("a completed rate pins on POSIXct data (single-window fallback)", {
  js <- timed_panel_join_cast("POSIXct")
  cc <- suppressWarnings(
    complete_generative_spec(js, consumer = "estimate_dynes")
  )
  fr_rate <- cc$process_map$fid[
    cc$process_map$layer == "friendship" & cc$process_map$family == "rate"
  ]
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  expect_true(is.finite(rate$intercept))
})

test_that("a completed rate pins on Date data (single-window fallback)", {
  js <- timed_panel_join_cast("Date")
  cc <- suppressWarnings(
    complete_generative_spec(js, consumer = "estimate_dynes")
  )
  fr_rate <- cc$process_map$fid[
    cc$process_map$layer == "friendship" & cc$process_map$family == "rate"
  ]
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  expect_true(is.finite(rate$intercept))
})

test_that("the single-window fallback is scoped to the pinned layer's extent", {
  # friendship (the pinned layer) is only observed over [1, 4]; calls, an
  # unrelated layer in the same joint dataset, extends to time 9. Regression
  # test for D2: the pre-fix `default_window()` ranged over every layer's
  # ties, so it would have picked up calls' later extent instead of stopping
  # at friendship's own.
  data <- plain_data("event")
  data$ties <- rbind(
    data$ties,
    data.frame(from = 5L, to = 1L, time = 9, layer = "calls")
  )
  fr <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = data
  )
  calls <- make_specification(
    rate = ~ 1 + indeg(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  panel_data <- plain_data("panel")
  panel_data$ties <- rbind(
    panel_data$ties,
    data.frame(from = 5L, to = 1L, time = 9, layer = "calls")
  )
  js <- make_joint_specification(fr, calls, data = panel_data)
  cc <- suppressWarnings(complete_generative_spec(
    js,
    consumer = "estimate_dynes"
  ))
  expect_equal(
    goldfish:::default_window(panel_data, "friendship"),
    c(1, 4)
  )
  fr_rate <- cc$process_map$fid[
    cc$process_map$layer == "friendship" & cc$process_map$family == "rate"
  ]
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  rs <- goldfish:::panel_wave_risk_set(
    cc,
    layer = "friendship",
    flavor = NA,
    entity = "sender",
    wave_times = NULL
  )
  # duration == 4 - 1 == 3, not 9 - 1 == 8 (the unfiltered joint extent).
  expect_equal(rs$duration, 3)
  expect_equal(
    exp(rate$intercept) * rs$duration * rs$risk_set_size,
    rs$count
  )
})

# --- D9a shared risk-set helper ----------------------------------------------

test_that("panel |R_w| equals the wave-endpoint average for both rate entities", {
  # A mutually-exclusive creation flavor on a panel layer: its post-constraint
  # entity count is state-dependent, so the wave-endpoint average is non-trivial.
  js <- timed_panel_join()
  wave_times <- c(0, 2, 4, 6)
  # Hand-compute the endpoint counts from the materialized friendship states
  # (unconstrained "none" support here: the completed rate reads nothing).
  states <- lapply(wave_times, function(w) {
    unname(network_state_at(plain_data("panel"), "friendship", time = w))
  })
  n <- nrow(states[[1L]])
  sender_counts <- vapply(
    states,
    function(st) {
      grid <- matrix(TRUE, n, n)
      diag(grid) <- FALSE
      sum(rowSums(grid) > 0)
    },
    numeric(1)
  )
  dyad_counts <- vapply(
    states,
    function(st) {
      grid <- matrix(TRUE, n, n)
      diag(grid) <- FALSE
      sum(grid)
    },
    numeric(1)
  )
  avg <- function(x) (x[-length(x)] + x[-1L]) / 2

  rs_sender <- goldfish:::panel_wave_risk_set(
    js,
    layer = "friendship",
    flavor = NA,
    entity = "sender",
    wave_times = wave_times
  )
  rs_dyad <- goldfish:::panel_wave_risk_set(
    js,
    layer = "friendship",
    flavor = NA,
    entity = "dyad",
    wave_times = wave_times
  )
  expect_equal(rs_sender$risk_set_size, avg(sender_counts))
  expect_equal(rs_dyad$risk_set_size, avg(dyad_counts))
})

test_that("a single-period relational pin matches goldfish's starting value", {
  # The cross-check anchor: exp(intercept_1) == n_dep_events / total_time /
  # avg_active_entity, goldfish's own intercept-only baseline-rate starting value.
  data <- plain_data("event")
  rs <- goldfish:::relational_window_risk_set(data, "calls", model = "DyNAM")
  intercept <- pin_intercept_only_rate(
    rs$count,
    rs$duration,
    rs$risk_set_size
  )
  expect_equal(
    exp(intercept),
    rs$count / rs$duration / rs$risk_set_size
  )
})

# --- D3 panel/relational risk-set dispatch -----------------------------------

# A flavored (creation/dissolution) calls process, EVENT-observed at join time,
# composed with a plain emails process. The creation flavor is keyed in choice
# but omitted from rate, so its missing rate completes to a pinned intercept-only
# rate -- and because the flavor is non-NA, the dispatch keeps it on the panel
# wave-endpoint path (the flavored-relational stopgap), not the unflavored
# relational scalar path. `flavored_event_stream()` is the shared helper fixture.
flavored_relational_join <- function() {
  n_actors <- 12L
  calls <- flavored_event_stream(n_actors)
  emails <- data.frame(
    from = c(2L, 3L, 4L, 5L, 6L),
    to = c(1L, 2L, 3L, 4L, 5L),
    time = c(1, 2, 3, 4, 5),
    layer = "emails",
    weight = 1
  )
  data <- add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(calls = "increment", emails = "increment"),
        directed = c(calls = TRUE, emails = TRUE),
        observation = c(calls = "event", emails = "event")
      ),
      nodes = data.frame(
        label = paste0("N", seq_len(n_actors)),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(calls, emails)
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
  calls_spec <- make_specification(
    rate = list(dissolution ~ 1 + indeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

test_that("an unflavored relational layer's completed rate uses the relational scalars", {
  # calls is event-observed and unflavored, keyed in choice but not rate: its
  # missing rate completes to a pinned intercept-only rate. Because calls is not
  # a modeled panel layer and its process is unflavored, the dispatch sources
  # (count, duration, |R|) from relational_window_risk_set()'s preprocessing
  # scalars, NOT a synthesized wave-endpoint Hamming diff. friendship carries the
  # authored waiting-time rate that makes the composition timed.
  data <- plain_data("event")
  calls <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  friendship <- make_specification(
    rate = ~ 1 + inertia,
    layer = "friendship",
    model = "DyNAM",
    data = data
  )
  js <- make_joint_specification(calls, friendship, data = data)
  cc <- suppressWarnings(
    complete_generative_spec(js, consumer = "estimate_dynes")
  )
  map <- cc$process_map
  # Both layers are event-observed, so neither is a modeled panel layer.
  expect_length(cc$modeled_panel, 0L)
  calls_rate <- map$fid[map$layer == "calls" & map$family == "rate"]
  expect_length(calls_rate, 1L)
  expect_true(map$completed[map$fid == calls_rate])
  rate <- cc$completed_rates[[as.character(calls_rate)]]
  expect_s3_class(rate, "intercept_only_rate")
  rs <- goldfish:::relational_window_risk_set(cc$data, "calls", model = "DyNAM")
  # The pin reproduces the relational scalars, not the panel Hamming derivation.
  expect_equal(exp(rate$intercept), rs$count / rs$duration / rs$risk_set_size)
})

test_that("the relational risk-set source drives a REM tie-oriented pin", {
  # A REM process always authors its rate (REM has no choice to leave a gap), so
  # a completed REM rate never arises end to end; the dispatch's model plumbing
  # is exercised directly on relational_window_risk_set(model = "REM"), the exact
  # call pin_completed_rates() makes for an unflavored relational REM layer.
  data <- plain_data("event")
  rs <- goldfish:::relational_window_risk_set(data, "calls", model = "REM")
  intercept <- pin_intercept_only_rate(
    rs$count,
    rs$duration,
    rs$risk_set_size
  )
  expect_true(is.finite(intercept))
  expect_equal(exp(intercept), rs$count / rs$duration / rs$risk_set_size)
})

test_that("a flavored relational layer's completed rate stays on the panel path", {
  # calls is event-observed but FLAVORED: its creation flavor's missing rate must
  # route through panel_wave_risk_set() (the is.na(flavor) stopgap), not the
  # unflavored relational scalar path. Documents the current stopgap boundary --
  # a later flavor-aware relational risk-set will flip this.
  js <- flavored_relational_join()
  expect_length(js$modeled_panel, 0L)
  cc <- suppressWarnings(
    complete_generative_spec(js, consumer = "estimate_dynes")
  )
  map <- cc$process_map
  creation_rate <- map$fid[
    map$layer == "calls" & map$flavor == "creation" & map$family == "rate"
  ]
  expect_length(creation_rate, 1L)
  expect_true(map$completed[map$fid == creation_rate])
  rate <- cc$completed_rates[[as.character(creation_rate)]]
  # The pin reproduces the panel wave-endpoint/Hamming derivation for the
  # creation flavor over calls' own extent -- proof it took the panel path.
  rs <- goldfish:::panel_wave_risk_set(
    cc,
    layer = "calls",
    flavor = "creation",
    entity = "sender",
    wave_times = NULL
  )
  expect_equal(
    exp(rate$intercept) * rs$duration * rs$risk_set_size,
    rs$count
  )
})

# --- D4 degenerate (no-timed-events) layer handling --------------------------

# A join whose completed layer carries only `time = NA` history (no timed
# events). `friendship_obs` sets friendship's observation so the same tie set
# builds a friendship-focal spec ("event") and joins as a modeled panel layer
# ("panel"). calls carries the authored waiting-time rate that makes the
# composition timed; friendship is choice-only, so its rate is completed.
degenerate_panel_join <- function(friendship_obs) {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = NA_real_,
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    )
  )
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = friendship_obs, calls = "event")
    ),
    nodes = nodes,
    ties = ties
  )
}

test_that("a panel layer with no timed events warns and pins a zero hazard", {
  local_cli_context()
  ev <- degenerate_panel_join("event")
  fr <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = ev
  )
  calls <- make_specification(
    rate = ~ 1 + indeg(friendship),
    layer = "calls",
    model = "DyNAM",
    data = ev
  )
  js <- make_joint_specification(
    fr,
    calls,
    data = degenerate_panel_join("panel")
  )
  expect_identical(js$modeled_panel, "friendship")

  # Collect every warning class so the degenerate-layer warning is asserted
  # alongside (not masked by) the completion-default warnings.
  seen <- list()
  cc <- withCallingHandlers(
    complete_generative_spec(js, consumer = "estimate_dynes"),
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- class(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(
    seen,
    function(cl) "goldfish_degenerate_panel_layer_warning" %in% cl,
    logical(1)
  )))

  map <- cc$process_map
  fr_rate <- map$fid[map$layer == "friendship" & map$family == "rate"]
  expect_length(fr_rate, 1L)
  expect_true(map$completed[map$fid == fr_rate])
  rate <- cc$completed_rates[[as.character(fr_rate)]]
  # A well-defined zero hazard: the flavor cannot fire.
  expect_identical(rate$intercept, -Inf)
  expect_identical(exp(rate$intercept), 0)
})

test_that("an unflavored relational layer with no events aborts naming it", {
  local_cli_context()
  # calls is event-observed, unflavored, choice-only (its rate completes), but
  # carries only `time = NA` history: the relational risk-set scalars come back
  # with a zero denominator, so the pin aborts naming the layer -- before
  # pin_intercept_only_rate()'s generic positivity guard fires.
  data <- degenerate_panel_join("event")
  data$ties$time[data$ties$layer == "calls"] <- NA_real_
  data$ties$time[data$ties$layer == "friendship"] <- c(1, 2, 3, 4)
  calls <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  friendship <- make_specification(
    rate = ~ 1 + inertia,
    layer = "friendship",
    model = "DyNAM",
    data = data
  )
  js <- make_joint_specification(calls, friendship, data = data)
  # Both layers event-observed here, so calls is not a modeled panel layer and
  # its unflavored completed rate takes the relational dispatch.
  expect_length(js$modeled_panel, 0L)
  expect_error(
    suppressWarnings(
      complete_generative_spec(js, consumer = "estimate_dynes")
    ),
    class = "goldfish_degenerate_relational_layer_error"
  )
})

test_that("an empty inter-wave period in a grid stays a silent -Inf pin", {
  local_cli_context()
  # An explicit multi-wave grid whose middle period spans no state change is the
  # intended silent case: that period pins to -Inf, and NO degenerate-layer
  # warning fires (the warning is scoped to the single-window fallback).
  js <- timed_panel_join()
  seen <- list()
  rs <- withCallingHandlers(
    goldfish:::panel_wave_risk_set(
      js,
      layer = "friendship",
      flavor = NA,
      entity = "sender",
      wave_times = c(0, 1.5, 1.6, 5)
    ),
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- class(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(vapply(
    seen,
    function(cl) "goldfish_degenerate_panel_layer_warning" %in% cl,
    logical(1)
  )))
  # The middle period has a zero net Hamming diff -> a silent -Inf plateau.
  expect_identical(rs$count[[2]], 0)
  intercept <- pin_intercept_only_rate(
    rs$count,
    rs$duration,
    rs$risk_set_size
  )
  expect_identical(intercept[[2]], -Inf)
  expect_true(all(is.finite(intercept[-2])))
})

# --- Regime and completion scope --------------------------------------------

test_that("a mixed ordered+timed composition aborts at join time", {
  local_cli_context()
  data <- plain_data("panel")
  timed <- make_specification(
    rate = ~ 1 + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  ordered <- make_specification(
    rate = ~ 1 + inertia,
    rate_sub_model = "rate_ordered",
    layer = "friendship",
    model = "DyNAM",
    data = plain_data("event")
  )
  expect_snapshot(
    make_joint_specification(timed, ordered, data = data),
    error = TRUE
  )
})

test_that("the completion warning re-fires through a second consumer", {
  js <- rate_only_choice_join()
  expect_warning(
    complete_generative_spec(js, consumer = "estimate_dynes"),
    class = "goldfish_completed_default_warning"
  )
  # The SAME half-specified spec through a second consumer warns again.
  expect_warning(
    complete_generative_spec(js, consumer = "simulate"),
    class = "goldfish_completed_default_warning"
  )
})

test_that("completion is scoped by type to a joint specification", {
  local_cli_context()
  spec <- make_specification(
    rate = ~ 1 + inertia,
    layer = "calls",
    model = "DyNAM",
    data = plain_data("event")
  )
  expect_snapshot(complete_generative_spec(spec), error = TRUE)
})

# --- D4 separability ---------------------------------------------------------

test_that("a completed modeled-panel fid is not separable though it reads nothing", {
  js <- timed_panel_join()
  cc <- suppressWarnings(complete_generative_spec(js, wave_times = c(0, 2, 5)))
  map <- cc$process_map
  fr_rate <- map$fid[map$layer == "friendship" & map$family == "rate"]
  # The pinned rate reads nothing -> coupled FALSE, yet friendship is a modeled
  # panel layer, so it is NOT separable (D4).
  expect_false(map$coupled[map$fid == fr_rate])
  sep <- goldfish:::joint_separable(cc)
  expect_false(sep[map$fid == fr_rate])
  # No process is "all separable": a lone modeled-panel process never reports it.
  expect_false(all(sep[map$layer == "friendship"]))
})

# --- Case A: modeled panel must model all flavors ----------------------------

test_that("a modeled panel layer missing a whole flavor aborts", {
  local_cli_context()
  # friendship is a mutually-exclusive flavored PANEL layer carrying creation and
  # dissolution, but only creation is modeled -> Case A.
  make_flavored_panel <- function(friendship = "panel") {
    nodes <- data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    )
    fr <- data.frame(
      from = c(1L, 2L, 3L, 1L),
      to = c(2L, 3L, 4L, 2L),
      time = c(1, 2, 3, 4),
      layer = "friendship",
      weight = c(1, 1, 1, -1)
    )
    calls <- data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls",
      weight = 1
    )
    info <- list(
      name = "toy",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = friendship, calls = "event")
    )
    add_flavor(
      list(info = info, nodes = nodes, ties = rbind(fr, calls)),
      layer = "friendship",
      values_equivalence = c(creation = 1, dissolution = -1),
      flavor_style = "mutually_exclusive"
    )
  }
  ev <- make_flavored_panel("event")
  fr <- make_specification(
    rate = list(creation ~ 1 + indeg),
    choice = list(creation ~ inertia),
    layer = "friendship",
    model = "DyNAM",
    data = ev
  )
  calls <- make_specification(
    rate = ~ 1 + indeg(friendship),
    layer = "calls",
    model = "DyNAM",
    data = ev
  )
  js <- make_joint_specification(fr, calls, data = make_flavored_panel("panel"))
  expect_snapshot(
    complete_generative_spec(js, wave_times = c(0, 2, 5)),
    error = TRUE
  )
})

test_that("an RE (event) focal layer modeling a subset of flavors is not aborted", {
  # calls is a mutually-exclusive flavored EVENT layer; modeling only creation is
  # legal (the dissolution rows update state). Completion adds creation's choice,
  # never aborts.
  nodes <- data.frame(
    label = paste0("N", 1:8),
    mode = "p",
    stringsAsFactors = FALSE
  )
  calls <- data.frame(
    from = c(1L, 2L, 3L, 1L, 4L),
    to = c(2L, 3L, 4L, 2L, 5L),
    time = c(1, 2, 3, 4, 5),
    layer = "calls",
    weight = c(1, 1, 1, -1, 1)
  )
  friendship <- data.frame(
    from = c(1L, 2L),
    to = c(2L, 3L),
    time = c(1, 2),
    layer = "friendship",
    weight = 1
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(calls = "increment", friendship = "replace"),
    directed = c(calls = TRUE, friendship = TRUE),
    observation = c(calls = "event", friendship = "panel")
  )
  data <- add_flavor(
    list(info = info, nodes = nodes, ties = rbind(calls, friendship)),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
  calls_spec <- make_specification(
    rate = list(creation ~ 1 + tie(friendship)),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "friendship",
    model = "DyNAM",
    data = local({
      d <- data
      d$info$observation["friendship"] <- "event"
      d
    })
  )
  # friendship is exogenous panel here (not modeled): compose and complete.
  js <- make_joint_specification(calls_spec, emails_spec, data = data)
  expect_no_error(
    suppressWarnings(complete_generative_spec(js, wave_times = c(0, 3, 6)))
  )
})

# --- Idempotence, single-process untouched, process_map$completed ------------

test_that("completion is idempotent on an already-complete spec", {
  js <- timed_panel_join()
  cc <- suppressWarnings(complete_generative_spec(js, wave_times = c(0, 2, 5)))
  # Re-entering finds no gap: no warning, identical map and completed marks.
  cc2 <- expect_no_warning(
    complete_generative_spec(cc, wave_times = c(0, 2, 5))
  )
  expect_identical(cc2$process_map$completed, cc$process_map$completed)
  expect_identical(nrow(cc2$process_map), nrow(cc$process_map))
  expect_equal(
    cc2$completed_rates[[1L]]$intercept,
    cc$completed_rates[[1L]]$intercept
  )
})

test_that("process_map$completed marks exactly the auto-supplied fids", {
  js <- timed_panel_join()
  cc <- suppressWarnings(complete_generative_spec(js, wave_times = c(0, 2, 5)))
  map <- cc$process_map
  # Authored: friendship choice, calls rate. Completed: friendship rate, calls
  # choice.
  authored <- with(
    map,
    (layer == "friendship" & family == "choice") |
      (layer == "calls" & family == "rate")
  )
  expect_identical(map$completed, !authored)
})

test_that("a single-process rate-only spec is not completed by estimation", {
  # The excluded path: estimate_dynam() over a rate-only specification.goldfish
  # keeps it rate-only (no choice added), so its preprocessed output is unchanged.
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = plain_data("event")
  )
  prep <- estimate_dynam(spec, sub_model = "rate", preprocessing_only = TRUE)
  # A rate-only preprocessed object, no choice sub-model was synthesized.
  expect_null(spec$submodels$choice)
  expect_true(!is.null(prep))
})

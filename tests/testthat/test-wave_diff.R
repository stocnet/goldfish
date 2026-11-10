# Wave diffing of a panel-observed state layer into per-interval candidate flip
# sets, the endpoint-hitting sequence validator, and the incomplete-snapshot
# abort. The wave grid carries the observation start first, so M waves give M
# modeled intervals over M + 1 boundaries.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A flavored state panel (friendship, +1 creation / -1 dissolution) on five
# actors, observed at waves 5 and 10 after the observation start 0. `rows` are
# the timed friendship rows (from, to, time, weight); the history row (1, 2) is
# the starting network.
wave_panel_data <- function(rows, update = "increment") {
  history <- data.frame(
    from = 1L,
    to = 2L,
    time = NA_real_,
    weight = 1,
    layer = "friendship"
  )
  friendship <- rbind(history, cbind(rows, layer = "friendship"))
  values <- if (identical(update, "replace")) {
    c(creation = 1, dissolution = 0)
  } else {
    c(creation = 1, dissolution = -1)
  }
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(friendship = update, calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = "panel", calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:5),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(
        friendship,
        data.frame(
          from = c(2L, 4L),
          to = c(3L, 5L),
          time = c(2, 7),
          weight = 1,
          layer = "calls"
        )
      )
    ),
    layer = "friendship",
    values_equivalence = values,
    flavor_style = "mutually_exclusive"
  )
}

wave_rows <- function(from, to, time, weight) {
  data.frame(from = from, to = to, time = time, weight = weight)
}

# The flips as plain (interval, from, to, flavor) rows, sorted.
flip_rows <- function(diff) {
  f <- diff$flips[c("interval", "from", "to", "flavor")]
  f <- f[order(f$interval, f$from, f$to), , drop = FALSE]
  rownames(f) <- NULL
  f
}

expected_flips <- function(interval, from, to, flavor) {
  data.frame(
    interval = as.integer(interval),
    from = as.integer(from),
    to = as.integer(to),
    flavor = flavor,
    stringsAsFactors = FALSE
  )
}

# --- Wave diffing -----------------------------------------------------------

test_that("two waves diff into a dissolution and a creation", {
  data <- wave_panel_data(wave_rows(c(1L, 3L), c(2L, 4L), 10, c(-1, 1)))

  diff <- diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))

  expect_s3_class(diff, "goldfishWaveDiff")
  expect_identical(diff$wave_times, c(0, 5, 10))
  expect_identical(
    flip_rows(diff),
    expected_flips(2, c(1, 3), c(2, 4), c("dissolution", "creation"))
  )
  expect_identical(diff$flips$from_value, c(1, 0))
  expect_identical(diff$flips$to_value, c(0, 1))
  expect_identical(diff$counts, c(0L, 2L))
})

test_that("a row stamped at a wave belongs to the interval ending there", {
  data <- wave_panel_data(wave_rows(c(3L, 2L), c(4L, 3L), c(5, 10), 1))

  diff <- diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))

  expect_identical(
    flip_rows(diff),
    expected_flips(1:2, c(3, 2), c(4, 3), "creation")
  )
  expect_identical(diff$counts, c(1L, 1L))
})

test_that("the first wave closes a modeled interval, not a history span", {
  # A row strictly between the observation start and the first wave is a flip
  # of the first interval; the history row is the entering state.
  data <- wave_panel_data(wave_rows(4L, 5L, 2.5, 1))

  diff <- diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))

  expect_identical(flip_rows(diff), expected_flips(1, 4, 5, "creation"))
  expect_identical(unname(diff$snapshots[[1L]][1, 2]), 1)
  expect_identical(sum(diff$snapshots[[1L]]), 1)
  expect_length(diff$snapshots, 3L)
})

test_that("a net-zero change between waves produces no flip", {
  data <- wave_panel_data(wave_rows(3L, 4L, c(6, 8), c(1, -1)))

  diff <- diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))

  expect_identical(nrow(diff$flips), 0L)
  expect_identical(diff$counts, c(0L, 0L))
})

test_that("a replace-encoded state panel maps its flips to the same flavors", {
  data <- wave_panel_data(
    wave_rows(c(1L, 3L), c(2L, 4L), 10, c(0, 1)),
    update = "replace"
  )

  diff <- diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))

  expect_identical(
    flip_rows(diff),
    expected_flips(2, c(1, 3), c(2, 4), c("dissolution", "creation"))
  )
})

test_that("every flip is a one-step chain on its own dyad", {
  data <- wave_panel_data(wave_rows(c(1L, 3L), c(2L, 4L), c(5, 10), c(-1, 1)))

  diff <- diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))

  expect_identical(diff$flips$step, c(1L, 1L))
  expect_identical(anyDuplicated(diff$flips$chain), 0L)
})

test_that("an incomplete wave snapshot aborts before diffing", {
  local_cli_context()
  # add_flavor() refuses a missing value, so the gap is set after flavoring,
  # as an unobserved dyad at wave 5.
  data <- wave_panel_data(wave_rows(c(3L, 4L), c(4L, 5L), 5, 1))
  data$ties$weight[data$ties$from == 4L & data$ties$to == 5L] <- NA

  expect_snapshot(
    diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10)),
    error = TRUE
  )
  expect_error(
    diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10)),
    class = "goldfish_incomplete_wave_snapshot"
  )
})

test_that("a state outside the flavor mapping aborts", {
  local_cli_context()
  # Two creations on one dyad accumulate a state of 2, which no dichotomous
  # creation/dissolution flavor reaches.
  data <- suppressWarnings(wave_panel_data(wave_rows(3L, 4L, c(2, 4), 1)))

  expect_snapshot(
    diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10)),
    error = TRUE
  )
  expect_error(
    diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10)),
    class = "goldfish_invalid_wave_state"
  )
})

test_that("the wave grid needs increasing boundaries", {
  local_cli_context()
  data <- wave_panel_data(wave_rows(3L, 4L, 5, 1))

  expect_snapshot(
    diff_panel_waves(data, "friendship", wave_times = c(0, 10, 5)),
    error = TRUE
  )
  expect_error(
    diff_panel_waves(data, "friendship", wave_times = 5),
    class = "goldfish_invalid_wave_grid"
  )
})

# --- Endpoint-hitting validator ---------------------------------------------

# The observed panel of the validator tests: interval 1 creates (3, 4) and
# dissolves (1, 2), interval 2 creates (2, 3).
observed_wave_data <- function() {
  wave_panel_data(
    wave_rows(c(3L, 1L, 2L), c(4L, 2L, 3L), c(5, 5, 10), c(1, -1, 1))
  )
}

# The observed data with its timed friendship rows retimed to `times`.
retimed <- function(times, data = observed_wave_data()) {
  timed <- which(data$ties$layer == "friendship" & !is.na(data$ties$time))
  data$ties$time[timed] <- times
  data
}

test_that("a sequence placing each flip inside its interval hits the waves", {
  diff <- diff_panel_waves(observed_wave_data(), "friendship", c(0, 5, 10))

  expect_no_error(validate_endpoint_hitting(retimed(c(1.5, 4, 7)), diff))
  expect_no_error(validate_endpoint_hitting(retimed(c(4, 1.5, 9.9)), diff))
})

test_that("a flip on a wave time is not strictly inside its interval", {
  diff <- diff_panel_waves(observed_wave_data(), "friendship", c(0, 5, 10))

  expect_error(
    validate_endpoint_hitting(retimed(c(1.5, 5, 7)), diff),
    class = "goldfish_endpoint_violation"
  )
})

test_that("a flip moved to another interval misses both endpoints", {
  local_cli_context()
  diff <- diff_panel_waves(observed_wave_data(), "friendship", c(0, 5, 10))

  expect_snapshot(
    validate_endpoint_hitting(retimed(c(6, 4, 7)), diff),
    error = TRUE
  )
})

test_that("an excursion is not a minimal-flip sequence", {
  diff <- diff_panel_waves(observed_wave_data(), "friendship", c(0, 5, 10))
  # A create-then-dissolve pair on (4, 5) inside interval 2 reproduces both
  # snapshots but places events the wave diff does not contain.
  data <- retimed(c(1.5, 4, 7))
  data$ties <- rbind(
    data$ties,
    data.frame(
      from = 4L,
      to = 5L,
      time = c(8, 9),
      weight = c(1, -1),
      layer = "friendship",
      flavor = c("creation", "dissolution")
    )
  )

  expect_error(
    validate_endpoint_hitting(data, diff),
    class = "goldfish_endpoint_violation"
  )
})

test_that("a sequence missing a flip does not reach the next wave", {
  diff <- diff_panel_waves(observed_wave_data(), "friendship", c(0, 5, 10))
  data <- retimed(c(1.5, 4, 7))
  dropped <- data$ties$layer == "friendship" & data$ties$time %in% 7
  data$ties <- data$ties[!dropped, , drop = FALSE]

  expect_error(
    validate_endpoint_hitting(data, diff),
    class = "goldfish_endpoint_violation"
  )
})
# --- Shared wave snapshot ---------------------------------------------------

# The task-1.4 toy shape on a flavored state panel: the observation start 0,
# waves 5 and 10, and every friendship change stamped at the wave that first
# records it. friendship is modeled choice-only for both flavors, so a timed
# completion pins each flavor's rate from the wave snapshots; calls carries the
# authored rate that makes the joint specification timed.
toy_wave_data <- function() {
  friendship <- data.frame(
    from = c(1L, 2L, 3L, 4L, 1L, 5L, 2L),
    to = c(2L, 3L, 4L, 5L, 2L, 6L, 3L),
    time = c(NA, NA, 5, 5, 5, 10, 10),
    weight = c(1, 1, 1, 1, -1, 1, -1),
    layer = "friendship"
  )
  calls <- data.frame(
    from = c(1L, 2L, 3L, 6L),
    to = c(3L, 4L, 5L, 1L),
    time = c(1, 3, 6, 8),
    weight = 1,
    layer = "calls"
  )
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = "panel", calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:6),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(friendship, calls)
    ),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

toy_wave_spec <- function() {
  data <- toy_wave_data()
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# A flavor's flips per interval in the wave diff.
diff_counts <- function(diff, flavor) {
  tabulate(
    diff$flips$interval[diff$flips$flavor == flavor],
    nbins = length(diff$counts)
  )
}

test_that("the pin's per-period counts are the wave diff's per-interval counts", {
  js <- toy_wave_spec()
  waves <- c(0, 5, 10)
  diff <- diff_panel_waves(js$data, "friendship", wave_times = waves)

  for (flavor in c("creation", "dissolution")) {
    rs <- panel_wave_risk_set(
      js,
      layer = "friendship",
      flavor = flavor,
      entity = "sender",
      wave_times = waves
    )
    expect_identical(rs$count, as.numeric(diff_counts(diff, flavor)))
  }
  expect_identical(diff_counts(diff, "creation"), c(2L, 1L))
})

test_that("a row stamped at the first wave is pinned in the first period", {
  completed <- suppressWarnings(
    complete_generative_spec(toy_wave_spec(), wave_times = c(0, 5, 10))
  )
  map <- completed$process_map
  pinned <- map$fid[map$layer == "friendship" & map$family == "rate"]
  expect_length(pinned, 2L)
  diff <- diff_panel_waves(completed$data, "friendship", c(0, 5, 10))

  for (fid in pinned) {
    rate <- completed$completed_rates[[as.character(fid)]]
    flavor <- map$flavor[map$fid == fid]
    # A finite first plateau, not the zero-hazard pin of an empty period.
    expect_true(all(is.finite(rate$intercept)))
    rs <- panel_wave_risk_set(
      completed,
      layer = "friendship",
      flavor = flavor,
      entity = "sender",
      wave_times = c(0, 5, 10)
    )
    expect_equal(
      exp(rate$intercept) * rs$duration * rs$risk_set_size,
      as.numeric(diff_counts(diff, flavor))
    )
  }
})

test_that("a pinned period's risk set is read at the inclusive snapshots", {
  js <- toy_wave_spec()
  rs <- panel_wave_risk_set(
    js,
    layer = "friendship",
    flavor = "dissolution",
    entity = "sender",
    wave_times = c(0, 5, 10)
  )
  # Senders holding a tie: {1, 2} at 0, {2, 3, 4} at 5, {3, 4, 5} at 10.
  expect_identical(rs$risk_set_size, c(2.5, 3))
})

# --- Count panels -------------------------------------------------------------

# An unflavored count panel (calls, the cumulative number of calls per dyad as
# of each wave) beside an observed emails stream, on four actors, with waves 5
# and 10 after the observation start 0. `rows` are the timed calls rows (from,
# to, time, weight); `history` the counts before the observation start.
count_panel_data <- function(rows, history = NULL, update = "increment") {
  calls <- rbind(history, rows)
  list(
    info = list(
      name = "toy",
      focal = "emails",
      update = c(calls = update, emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "panel", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      cbind(calls, layer = "calls"),
      data.frame(from = 1L, to = 3L, time = 2, weight = 1, layer = "emails")
    )
  )
}

test_that("a count wave difference yields a multiset of unflavored events", {
  # 3 calls on (1, 2) by the first wave and 7 by the second: four
  # indistinguishable calls in the second interval.
  data <- count_panel_data(wave_rows(1L, 2L, c(5, 10), c(3, 4)))

  diff <- diff_panel_waves(data, "calls", wave_times = c(0, 5, 10))

  expect_identical(diff$kind, "count")
  expect_null(diff$transitions)
  second <- diff$flips[diff$flips$interval == 2L, , drop = FALSE]
  expect_identical(nrow(second), 4L)
  expect_identical(unique(second$from), 1L)
  expect_identical(unique(second$to), 2L)
  expect_true(all(is.na(second$flavor)))
  # One chain per dyad and interval, as long as the count difference; its
  # steps run the cumulative count up from the entering to the leaving wave.
  expect_identical(length(unique(second$chain)), 1L)
  expect_identical(second$step, 1:4)
  expect_identical(second$from_value, c(3, 4, 5, 6))
  expect_identical(second$to_value, c(4, 5, 6, 7))
  expect_identical(diff$counts, c(3L, 4L))
})

test_that("a replace-encoded count panel diffs to the same multiset", {
  increment <- count_panel_data(wave_rows(c(1L, 2L), c(2L, 3L), 5, c(3, 2)))
  replace <- count_panel_data(
    wave_rows(c(1L, 2L), c(2L, 3L), 5, c(3, 2)),
    update = "replace"
  )

  a <- diff_panel_waves(increment, "calls", wave_times = c(0, 5, 10))
  b <- diff_panel_waves(replace, "calls", wave_times = c(0, 5, 10))

  expect_identical(a$flips, b$flips)
  expect_identical(a$counts, c(5L, 0L))
})

test_that("a count panel's first interval starts from its initial counts", {
  # With no history the entering counts are zero; the calls stamped at the
  # first wave are events of the first modeled interval, not history.
  no_history <- count_panel_data(wave_rows(1L, 2L, 5, 2))
  diff <- diff_panel_waves(no_history, "calls", wave_times = c(0, 5, 10))
  expect_identical(sum(diff$snapshots[[1L]]), 0)
  expect_identical(diff$counts, c(2L, 0L))

  # History counts are the initial condition and are never diffed.
  with_history <- count_panel_data(
    wave_rows(1L, 2L, 5, 2),
    history = wave_rows(1L, 2L, NA_real_, 6)
  )
  diff <- diff_panel_waves(with_history, "calls", wave_times = c(0, 5, 10))
  expect_identical(unname(diff$snapshots[[1L]][1, 2]), 6)
  expect_identical(diff$counts, c(2L, 0L))
  expect_identical(diff$flips$from_value, c(6, 7))
})

test_that("a decreasing count aborts as a data error", {
  local_cli_context()
  data <- count_panel_data(
    wave_rows(1L, 2L, c(5, 10), c(3, 4)),
    update = "replace"
  )
  data$ties$weight[data$ties$layer == "calls" & data$ties$time == 10] <- 1

  expect_snapshot(
    diff_panel_waves(data, "calls", wave_times = c(0, 5, 10)),
    error = TRUE
  )
  expect_error(
    diff_panel_waves(data, "calls", wave_times = c(0, 5, 10)),
    class = "goldfish_decreasing_count_panel"
  )
})

test_that("a count panel's snapshots must hold whole non-negative counts", {
  data <- count_panel_data(wave_rows(1L, 2L, 5, 2.5))

  expect_error(
    diff_panel_waves(data, "calls", wave_times = c(0, 5, 10)),
    class = "goldfish_invalid_wave_state"
  )
})

test_that("unit calls strictly inside their intervals hit the count waves", {
  diff <- diff_panel_waves(
    count_panel_data(wave_rows(c(1L, 2L), c(2L, 3L), c(5, 10), c(2, 1))),
    "calls",
    wave_times = c(0, 5, 10)
  )
  augmented <- count_panel_data(
    wave_rows(c(1L, 1L, 2L), c(2L, 2L, 3L), c(1, 4.5, 7), 1)
  )
  expect_no_error(validate_endpoint_hitting(augmented, diff))

  # An extra call on (1, 2) overshoots the second wave's count.
  overshoot <- count_panel_data(
    wave_rows(c(1L, 1L, 2L, 1L), c(2L, 2L, 3L, 2L), c(1, 4.5, 7, 8), 1)
  )
  expect_error(
    validate_endpoint_hitting(overshoot, diff),
    class = "goldfish_endpoint_violation"
  )
})
test_that("a count panel's pinned periods count its events, not its dyads", {
  # (1, 2) rises by 3 then 4, (2, 3) by 2 in the second interval: 3 and 6
  # events, on 1 and 2 changed dyads.
  data <- count_panel_data(
    wave_rows(c(1L, 1L, 2L), c(2L, 2L, 3L), c(5, 10, 10), c(3, 4, 2))
  )
  js <- make_joint_specification(
    make_specification(
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
  diff <- diff_panel_waves(data, "calls", wave_times = c(0, 5, 10))

  rs <- panel_wave_risk_set(
    js,
    layer = "calls",
    flavor = NA_character_,
    entity = "sender",
    wave_times = c(0, 5, 10)
  )

  expect_identical(rs$count, as.numeric(diff$counts))
  expect_identical(rs$count, c(3, 6))
})
test_that("the panel path never mirrors an undirected two-mode layer", {
  # The square layer's history and wave-10 report are G->A, local cell (3, 1);
  # the non-square layer's are C->H, local cell (3, 5).
  check_shape <- function(shape, cell) {
    x_false <- two_mode_survey_fixture(shape, directed = FALSE)
    x_true <- two_mode_survey_fixture(shape, directed = TRUE)

    expect_no_error(diff_false <- diff_panel_waves(x_false, "survey", c(0, 10)))
    diff_true <- diff_panel_waves(x_true, "survey", c(0, 10))
    expect_identical(diff_false, diff_true)
    for (snapshot in diff_false$snapshots) {
      expect_identical(which(snapshot != 0, arr.ind = TRUE), cell)
    }
    expect_identical(nrow(diff_false$flips), 0L)

    mask_false <- panel_dyad_mask(panel_layer_context(x_false, "survey"))
    expect_true(all(mask_false))
    expect_identical(
      mask_false,
      panel_dyad_mask(panel_layer_context(x_true, "survey"))
    )
  }

  check_shape("square", cbind(row = 3L, col = 1L))
  check_shape("non_square", cbind(row = 3L, col = 5L))
})

test_that("an undirected two-mode replay writes only the cell it places", {
  # Wave 20 adds H->B (local (4, 2)) and F->D (local (2, 4)). A mirrored
  # replay of H->B would also write F->D and then find F->D already present.
  sequence_of <- function(directed) {
    x <- two_mode_survey_fixture("square", directed = directed)
    x$ties <- rbind(
      x$ties,
      data.frame(
        from = c(8L, 6L),
        to = c(2L, 4L),
        time = 20,
        weight = 1,
        layer = "survey"
      )
    )
    diff <- diff_panel_waves(x, "survey", c(0, 10, 20))
    x$ties <- data.frame(
      from = c(7L, 8L, 6L),
      to = c(1L, 2L, 4L),
      time = c(NA, 15, 16),
      weight = 1,
      layer = "survey"
    )
    list(sequence = x, diff = diff)
  }

  undirected <- sequence_of(FALSE)
  directed <- sequence_of(TRUE)
  expect_identical(undirected$diff, directed$diff)
  expect_no_error(validate_endpoint_hitting(directed$sequence, directed$diff))
  expect_no_error(
    validate_endpoint_hitting(undirected$sequence, undirected$diff)
  )
})

test_that("the panel path still folds an undirected one-mode layer", {
  # History A-B; wave 10 reports C->B, the dyad B-C reversed.
  x <- one_mode_undirected_fixture(
    data.frame(from = 3L, to = 2L, time = 10, weight = 1)
  )
  diff <- diff_panel_waves(x, "survey", c(0, 10))

  expect_identical(
    panel_dyad_mask(panel_layer_context(x, "survey")),
    upper.tri(matrix(TRUE, 3L, 3L))
  )
  expect_true(isSymmetric(diff$snapshots[[2L]]))
  expect_identical(sum(diff$snapshots[[2L]]), 4)
  expect_identical(diff$flips$from, 2L)
  expect_identical(diff$flips$to, 3L)

  sequence <- x
  sequence$ties <- data.frame(
    from = c(1L, 3L),
    to = c(2L, 2L),
    time = c(NA, 5),
    weight = 1,
    layer = "survey"
  )
  expect_no_error(validate_endpoint_hitting(sequence, diff))
})

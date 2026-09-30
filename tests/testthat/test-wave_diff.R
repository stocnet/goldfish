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

# Pool entry for DyNES sequences: each drawn sequence is fully re-preprocessed
# by one merged walk over the observation window [t0, t_M], the first to the
# last wave. Every endpoint-hitting sequence starts the window in the same
# observed state, so the θ-free start-state part of every fid's statistics is
# held once per process and referenced by every pooled sequence; only the
# path part is per sequence. These tests pin that sharing (by object
# identity), the memory bound it buys, the window's two jobs (each timed
# rate's censored tail reaches t_M; a pinned rate's interval across an
# interior wave is scored per plateau), and the optional precomputed
# attachment a pool entry adopts when it is consistent.

# The start-state fields of a goldfishStat: fixed by the window's start state
# and the specification, not by the drawn path.
start_state_fields <- c(
  "initial_stats",
  "active_sender_init",
  "active_dyad_init",
  "node_lookup",
  "nodes",
  "nodes2",
  "model_spec",
  "formula"
)

# A friendship choice whose free effect varies across the friendship events,
# so dynes_multiwave_spec() can be evaluated at theta.
indeg_friendship_choice <- ~ indeg + offset(recip, coef = -0.5)

multiwave_sequence <- function(data) {
  list(data = data, reference = new_reference_record(c(0, 0, 0), -1, -1))
}

# The observation window of dynes_multiwave_spec(): waves at 0, 2 and 5.
multiwave_window <- function() set_preprocessing(start_time = 0, end_time = 5)

drawn_multiwave_sequences <- function(n, theta = c(0.3, -1.2, 0.4)) {
  lapply(new_multiwave_augmenter()$draw(1L, seq_len(n), theta), function(d) {
    list(data = d$data, reference = new_reference_record(theta, -1, -1))
  })
}

# The distinct addresses each start-state field takes across `entries`, per
# fid and field: one address everywhere is one object held once.
start_state_addresses <- function(entries) {
  fids <- names(entries[[1]]$stats)
  out <- list()
  for (fid in fids) {
    for (field in start_state_fields) {
      values <- lapply(entries, function(e) e$stats[[fid]][[field]])
      if (is.null(values[[1]])) {
        next
      }
      out[[paste(fid, field)]] <- unique(vapply(
        values,
        rlang::obj_address,
        character(1)
      ))
    }
  }
  out
}

expect_one_start_state <- function(entries) {
  addresses <- start_state_addresses(entries)
  expect_gt(length(addresses), 0L)
  held <- lengths(addresses)
  expect_identical(
    held,
    rlang::rep_along(held, 1L) |> rlang::set_names(names(held))
  )
}

# A seam that behaves as daemons do: every value crossing it (task inputs,
# results, broadcast arguments) is a serialized copy, so nothing arrives
# shared with the sender.
roundtrip <- function(x) unserialize(serialize(x, NULL))

local_copying_seam <- function() {
  list(
    map = function(x, f) lapply(x, function(e) roundtrip(f(roundtrip(e)))),
    broadcast = function(f, args) list(do.call(f, roundtrip(args))),
    n_workers = 1L,
    close = function() invisible(NULL)
  )
}

test_that("the start state is held once across a pool of drawn sequences", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  pool <- build_sequence_pool(
    decompose_joint_spec(spec),
    drawn_multiwave_sequences(4L),
    theta_length = 3L,
    control_prep = multiwave_window()
  )

  expect_one_start_state(pool$sequences)
  expect_one_start_state(resident_pool_store(pool$store$key)$entries)

  # The path part stays per sequence: the drawn friendship times differ.
  paths <- lapply(pool$sequences, function(e) e$stats[["1"]]$intervals)
  expect_length(unique(paths), 4L)
})

test_that("start-state sharing survives values arriving as serialized copies", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  sequences <- drawn_multiwave_sequences(3L)
  store <- open_pool_store(
    local_copying_seam(),
    pool_store_header(
      decomposition,
      theta_length = 3L,
      control_prep = multiwave_window(),
      data = sequences[[1]]$data
    )
  )
  pool <- build_sequence_pool(
    decomposition,
    sequences,
    theta_length = 3L,
    store = store
  )

  # The coordinator received each entry from a "worker", and the resident
  # store received the whole chunk in one broadcast: each process holds one.
  expect_one_start_state(pool$sequences)
  expect_one_start_state(resident_pool_store(pool$store$key)$entries)
})

test_that("appended sequences refer to the pool's start state", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  pool <- build_sequence_pool(
    decompose_joint_spec(spec),
    drawn_multiwave_sequences(2L),
    theta_length = 3L,
    control_prep = multiwave_window()
  )
  grown <- append_sequence_pool(pool, drawn_multiwave_sequences(2L))

  expect_length(grown$sequences, 4L)
  expect_one_start_state(grown$sequences)
  expect_one_start_state(resident_pool_store(grown$store$key)$entries)
})

test_that("sharing the start state leaves evaluation unchanged", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  sequences <- drawn_multiwave_sequences(3L)
  theta <- c(0.3, -1.2, 0.4)
  pooled <- build_sequence_pool(
    decomposition,
    sequences,
    theta_length = 3L,
    control_prep = multiwave_window()
  )
  # Each sequence on its own pool: nothing to share with.
  alone <- lapply(sequences, function(s) {
    build_sequence_pool(
      decomposition,
      list(s),
      theta_length = 3L,
      control_prep = multiwave_window()
    )
  })

  shared <- evaluate_sequence_pool(pooled, theta)
  for (i in seq_along(alone)) {
    own <- evaluate_sequence_pool(alone[[i]], theta)
    expect_identical(shared$loglik[[i]], own$loglik)
    expect_identical(shared$score[i, ], own$score[1, ])
    expect_identical(shared$fisher[[i]], own$fisher[[1]])
  }
})

# --- The memory bound ------------------------------------------------------

# A two-layer panel at a scale where the start state dominates an unshared
# entry: `n` actors, `n_friendship` friendship ties created over the waves
# 0, 2, 5 and `n_calls` calls. The O(n^2) choice start state then outweighs
# the O(events) path part by an order of magnitude.
scaled_panel_data <- function(n, n_friendship, n_calls, observation) {
  withr::with_seed(7L, {
    dyads <- which(diag(n) == 0, arr.ind = TRUE)
    friendship <- dyads[sample(nrow(dyads), n_friendship), ]
    calls <- dyads[sample(nrow(dyads), n_calls, replace = TRUE), ]
    list(
      info = list(
        name = "scaled",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = observation, calls = "event")
      ),
      nodes = data.frame(label = paste0("N", seq_len(n)), mode = "p"),
      ties = rbind(
        data.frame(
          from = friendship[, 1],
          to = friendship[, 2],
          time = sort(stats::runif(n_friendship, 0, 5)),
          layer = "friendship"
        ),
        data.frame(
          from = calls[, 1],
          to = calls[, 2],
          time = sort(stats::runif(n_calls, 0, 5)),
          layer = "calls"
        )
      )
    )
  })
}

scaled_panel_spec <- function(n = 150L, n_events = 200L) {
  events <- scaled_panel_data(n, n_events, n_events, "event")
  raw <- make_joint_specification(
    make_specification(
      choice = ~ indeg + recip,
      layer = "friendship",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = events
    ),
    data = scaled_panel_data(n, n_events, n_events, "panel")
  )
  suppressWarnings(complete_generative_spec(raw, wave_times = c(0, 2, 5)))
}

# Endpoint-hitting redraws of the friendship times inside their wave periods.
scaled_panel_sequences <- function(data, n, theta) {
  friendship <- which(data$ties$layer == "friendship")
  first <- data$ties$time[friendship] <= 2
  lapply(seq_len(n), function(k) {
    data$ties$time[friendship] <- c(
      sort(stats::runif(sum(first), 0, 2)),
      sort(stats::runif(sum(!first), 2, 5))
    )
    list(data = data, reference = new_reference_record(theta, -1, -1))
  })
}

# The bytes the resident view of `entry` adds to a process already holding
# `template`: every statistics field it shares with `template` by address is
# already paid for.
marginal_entry_bytes <- function(entry, template) {
  view <- entry[c("data", "stats", "fixed_loglik", "fixed_degenerate")]
  paid <- 0
  for (fid in names(entry$stats)) {
    for (field in names(entry$stats[[fid]])) {
      value <- entry$stats[[fid]][[field]]
      if (
        !is.null(value) &&
          identical(
            rlang::obj_address(value),
            rlang::obj_address(template$stats[[fid]][[field]])
          )
      ) {
        paid <- paid + as.numeric(utils::object.size(value))
      }
    }
  }
  as.numeric(utils::object.size(view)) - paid
}

test_that("a pooled sequence costs at most 1 KB per path event", {
  skip_on_cran()
  withr::local_seed(20260930)
  spec <- scaled_panel_spec()
  theta <- numeric(6)
  pool <- build_sequence_pool(
    decompose_joint_spec(spec),
    scaled_panel_sequences(spec$data, 3L, theta),
    theta_length = 6L,
    control_prep = set_preprocessing(start_time = 0, end_time = 5)
  )

  path_events <- nrow(spec$data$ties)
  template <- pool$sequences[[1]]
  for (entry in pool$sequences[-1]) {
    expect_lte(marginal_entry_bytes(entry, template) / path_events, 1024)
  }
})

# --- The observation window [t0, t_M] -------------------------------------

test_that("each timed rate's trailing interval ends at the last wave", {
  # Calls stop at 3.8 and friendship at 4; the last wave is at 5.
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  data <- dynes_multiwave_data("panel")
  data$ties$time[data$ties$layer == "calls"] <- c(1, 2, 3, 3.5, 3.8)
  pool <- build_sequence_pool(
    decompose_joint_spec(spec),
    list(multiwave_sequence(data)),
    theta_length = 3L,
    control_prep = multiwave_window()
  )

  timed <- spec$process_map$fid[spec$process_map$has_intercept]
  expect_setequal(timed, c(1L, 3L))
  for (fid in as.character(timed)) {
    stats <- pool$sequences[[1]]$stats[[fid]]
    last <- length(stats$event_time)
    expect_identical(stats$event_time[[last]], 5)
    expect_identical(stats$is_dependent[[last]], 0L)
    expect_equal(stats$intervals[[last]], 1)
    # Exposed over the whole window, from t0 = 0 to t_M = 5.
    expect_equal(sum(stats$intervals), 5)
  }
})

test_that("a pinned plateau's straddling interval is split over the window", {
  # Wave times 0, 3.5, 5: friendship events at 1, 2, 3 pin
  # c1 = log(3 / (3.5 * 6)) and the one at 4 pins c2 = log(1 / (1.5 * 6)).
  spec <- suppressWarnings(complete_generative_spec(
    dynes_pinned_raw_spec(),
    wave_times = c(0, 3.5, 5)
  ))
  c1 <- log(3 / (3.5 * 6))
  c2 <- log(1 / (1.5 * 6))
  pool <- build_sequence_pool(
    decompose_joint_spec(spec),
    list(multiwave_sequence(spec$data)),
    theta_length = 3L,
    control_prep = multiwave_window()
  )

  # The window opens at t0 = 0, so the six-actor risk set is exposed over
  # (0, 3.5] at c1 and (3.5, 5] at c2: the interval (3, 4] straddles 3.5 and
  # is split half and half. The calls choice is uniform over five actors.
  stats <- pool$sequences[[1]]$stats[["1"]]
  expect_true(any(
    stats$event_time - stats$intervals < 3.5 &
      stats$event_time > 3.5
  ))
  pinned <- 3 * c1 + c2 - 6 * (exp(c1) * 3.5 + exp(c2) * 1.5)
  expect_equal(
    pool$sequences[[1]]$fixed_loglik,
    pinned - 5 * log(5),
    tolerance = 1e-10
  )
})

# --- The optional precomputed attachment ----------------------------------

# A sequence carrying a consistent attachment: statistics from its own walk
# over the pool's window, drawn at its reference theta, tagged with the
# pool's layout.
attached_sequence <- function(decomposition, sequence, control_prep) {
  stats <- preprocess_pool_sequence(
    decomposition$engine_spec,
    sequence$data,
    control_prep = control_prep
  )
  sequence$attachment <- new_pool_attachment(
    stats = stats,
    loglik_ref = sequence$reference$loglik_ref,
    theta_ref = sequence$reference$theta_ref,
    layout = pool_layout_tag(decomposition$sub_models, control_prep)
  )
  sequence
}

# Count the pool-entry walks while still running them.
local_counted_preprocess <- function(env = parent.frame()) {
  counter <- new.env()
  counter$n <- 0L
  original <- preprocess_pool_sequence
  local_mocked_bindings(
    preprocess_pool_sequence = function(...) {
      counter$n <- counter$n + 1L
      original(...)
    },
    .env = env
  )
  counter
}

test_that("a consistent attachment is adopted without a walk", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  window <- multiwave_window()
  sequences <- drawn_multiwave_sequences(2L)
  scratch <- build_sequence_pool(
    decomposition,
    sequences,
    theta_length = 3L,
    control_prep = window
  )
  attached <- lapply(sequences, function(s) {
    attached_sequence(decomposition, s, window)
  })

  walks <- local_counted_preprocess()
  adopted <- build_sequence_pool(
    decomposition,
    attached,
    theta_length = 3L,
    control_prep = window
  )

  expect_identical(walks$n, 0L)
  for (i in seq_along(sequences)) {
    expect_equal(adopted$sequences[[i]]$stats, scratch$sequences[[i]]$stats)
    expect_identical(
      adopted$sequences[[i]]$fixed_loglik,
      scratch$sequences[[i]]$fixed_loglik
    )
    # The adopted statistics are the entry's own: no second copy is kept.
    expect_null(adopted$sequences[[i]]$attachment)
  }
  expect_one_start_state(adopted$sequences)
  theta <- c(0.3, -1.2, 0.4)
  expect_equal(
    evaluate_sequence_pool(adopted, theta),
    evaluate_sequence_pool(scratch, theta)
  )
})

test_that("an attachment from another theta is recomputed from scratch", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  window <- multiwave_window()
  sequence <- attached_sequence(
    decomposition,
    drawn_multiwave_sequences(1L)[[1]],
    window
  )
  sequence$attachment$theta_ref <- sequence$reference$theta_ref + 0.1

  walks <- local_counted_preprocess()
  build_sequence_pool(
    decomposition,
    list(sequence),
    theta_length = 3L,
    control_prep = window
  )

  expect_identical(walks$n, 1L)
})

test_that("an attachment with another layout is recomputed from scratch", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  window <- multiwave_window()
  # Walked over a different window, so its statistics are not the pool's.
  sequence <- attached_sequence(
    decomposition,
    drawn_multiwave_sequences(1L)[[1]],
    set_preprocessing(start_time = 0, end_time = 4)
  )

  walks <- local_counted_preprocess()
  pool <- build_sequence_pool(
    decomposition,
    list(sequence),
    theta_length = 3L,
    control_prep = window
  )

  expect_identical(walks$n, 1L)
  stats <- pool$sequences[[1]]$stats[["3"]]
  expect_identical(max(stats$event_time), 5)
})

test_that("the layout tag follows the sub-models and the window", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  sub_models <- decompose_joint_spec(spec)$sub_models
  tag <- pool_layout_tag(sub_models, multiwave_window())

  expect_type(tag, "character")
  expect_length(tag, 1L)
  expect_identical(tag, pool_layout_tag(sub_models, multiwave_window()))
  expect_false(identical(
    tag,
    pool_layout_tag(sub_models, set_preprocessing(start_time = 0))
  ))
  expect_false(identical(
    tag,
    pool_layout_tag(sub_models[-1], multiwave_window())
  ))
})

test_that("new_pool_attachment validates its theta_ref and layout", {
  expect_snapshot(
    new_pool_attachment(
      stats = list(),
      loglik_ref = -1,
      theta_ref = "a",
      layout = "tag"
    ),
    error = TRUE
  )
  expect_snapshot(
    new_pool_attachment(
      stats = list(),
      loglik_ref = -1,
      theta_ref = 0,
      layout = c("a", "b")
    ),
    error = TRUE
  )
})

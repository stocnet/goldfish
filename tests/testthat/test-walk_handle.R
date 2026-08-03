# The stepping walk handle over the merged single-clock walk: walk_open /
# walk_advance / walk_evaluate / walk_inject. The handle maintains live per-fid
# statistics over one shared state; replaying an observed fixture through it must
# reproduce the batch driver's per-fid quantities (batch-vs-replay equality), and
# stepping / injection / misuse follow the documented contract.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A plain two-process join over one node set: calls and emails are DyNAM
# rate + choice processes; friendship is a panel layer both choice formulas read
# as an exogenous covariate (not itself modeled). Four fids across two blocks.
walk_fixture_data <- function() {
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
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      friendship = "increment",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = "panel", calls = "event", emails = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

walk_two_process <- function() {
  data <- walk_fixture_data()
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

# The process-state evaluator model type mirror the batch materializer path uses,
# for the batch-vs-replay cross-check.
walk_test_model_type <- function(map, fid) {
  if (identical(map$family[map$fid == fid], "rate")) {
    "DyNAM-M-Rate"
  } else {
    "DyNAM-M"
  }
}

test_that("replaying an observed fixture reproduces the batch per-fid quantities", {
  js <- walk_two_process()
  map <- js$process_map
  batch <- preprocess_joint(js)
  theta <- list(
    "1" = c(0.3, -0.2),
    "2" = c(0.5, -0.4),
    "3" = c(-0.1, 0.25),
    "4" = c(0.2, 0.1)
  )

  handle <- walk_open(js)
  schedule <- handle$schedule
  n2 <- 6L
  dep_count <- stats::setNames(integer(nrow(map)), as.character(map$fid))
  n_checked <- 0L

  for (k in seq_len(schedule$n)) {
    if (schedule$dependent[k]) {
      layer <- schedule$layer[k]
      sender <- schedule$sender[k]
      for (fid in map$fid[map$layer == layer]) {
        key <- as.character(fid)
        kk <- dep_count[[key]] + 1L
        live <- walk_evaluate(handle, fid, theta[[key]])

        is_rate <- identical(map$family[map$fid == fid], "rate")
        # A timed rate fid stores right-censored rows too, so the k-th DEPENDENT
        # event is at the k-th TRUE of is_dependent, not the k-th stored row.
        stored <- which(batch[[key]]$is_dependent == 1L)[kk]
        st <- materialize_process_state(
          batch[[key]],
          model_type = walk_test_model_type(map, fid),
          event_index = stored,
          has_intercept = is_rate
        )
        ref <- evaluate_process_state(st, theta[[key]])

        if (is_rate) {
          expect_equal(live$value, ref$value)
        } else {
          # walk_evaluate returns the whole choice matrix; the materializer gives
          # the observed sender's receiver row.
          row <- live$value[(sender - 1L) * n2 + seq_len(n2)]
          expect_equal(row, ref$value)
        }
        n_checked <- n_checked + 1L
      }
      for (fid in map$fid[map$layer == layer]) {
        key <- as.character(fid)
        dep_count[[key]] <- dep_count[[key]] + 1L
      }
    } else {
      walk_inject(
        handle,
        list(
          layer = schedule$layer[k],
          sender = schedule$sender[k],
          receiver = schedule$receiver[k],
          increment = schedule$value[[k]],
          time = schedule$time[k]
        )
      )
    }
  }
  expect_gt(n_checked, 0L)
})

test_that("a single-process specification opens and replays byte-identically", {
  data <- walk_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  batch <- preprocess_joint(spec)
  map <- attr(batch, "process_map")

  handle <- walk_open(spec)
  expect_s3_class(handle, "walk_handle.goldfish")

  schedule <- handle$schedule
  theta <- list("1" = c(0.4, -0.15), "2" = c(0.6, 0.3))
  n2 <- 6L
  dep_count <- stats::setNames(integer(nrow(map)), as.character(map$fid))
  for (k in seq_len(schedule$n)) {
    if (schedule$dependent[k]) {
      sender <- schedule$sender[k]
      for (fid in map$fid) {
        key <- as.character(fid)
        kk <- dep_count[[key]] + 1L
        live <- walk_evaluate(handle, fid, theta[[key]])
        is_rate <- identical(map$family[map$fid == fid], "rate")
        stored <- which(batch[[key]]$is_dependent == 1L)[kk]
        st <- materialize_process_state(
          batch[[key]],
          model_type = walk_test_model_type(map, fid),
          event_index = stored,
          has_intercept = is_rate
        )
        ref <- evaluate_process_state(st, theta[[key]])
        if (is_rate) {
          expect_equal(live$value, ref$value)
        } else {
          expect_equal(live$value[(sender - 1L) * n2 + seq_len(n2)], ref$value)
        }
        dep_count[[key]] <- kk
      }
    } else {
      # The calls network self-update is the state realization of a calls event:
      # the replay driver injects it, exactly as the multi-process replay does.
      walk_inject(
        handle,
        list(
          layer = schedule$layer[k],
          sender = schedule$sender[k],
          receiver = schedule$receiver[k],
          increment = schedule$value[[k]],
          time = schedule$time[k]
        )
      )
    }
  }
})

test_that("walk_advance applies exogenous events; evaluate reflects them", {
  js <- walk_two_process()
  # Only tie(friendship) drives the choice, so a friendship change is visible.
  theta_choice <- c(0, 2)

  before <- walk_open(js)
  eval_before <- walk_evaluate(before, 2L, theta_choice)

  after <- walk_open(js)
  # friendship (an exogenous, non-modeled layer) events are at t = 1..4; advance
  # past the first without touching the modeled calls / emails streams.
  walk_advance(after, 1.5)
  expect_equal(after$current_time, 1.5)
  eval_after <- walk_evaluate(after, 2L, theta_choice)

  # friendship 1 -> 2 raises sender 1's choice weight toward receiver 2.
  p_before <- eval_before$value[(1 - 1) * 6 + 2]
  p_after <- eval_after$value[(1 - 1) * 6 + 2]
  expect_gt(p_after, p_before)

  # walk_advance does NOT apply the modeled calls stream: the calls network is
  # untouched, so the inertia-driven part of the choice is unchanged at seed.
  expect_equal(sum(after$state$networks[["calls"]]), 0)
})

test_that("walk_inject makes an event visible to every reading fid", {
  js <- walk_two_process()
  theta_choice <- c(0, 2) # inertia 0, tie(friendship) 2

  handle <- walk_open(js)
  calls_before <- walk_evaluate(handle, 2L, theta_choice)$value
  emails_before <- walk_evaluate(handle, 4L, theta_choice)$value

  walk_inject(
    handle,
    list(layer = "friendship", sender = 5L, receiver = 6L, increment = 1)
  )

  calls_after <- walk_evaluate(handle, 2L, theta_choice)$value
  emails_after <- walk_evaluate(handle, 4L, theta_choice)$value

  cell <- (5 - 1) * 6 + 6 # dyad (5, 6) in sender-major order
  # Both processes read tie(friendship); the injected tie raises sender 5's
  # weight toward 6 in each.
  expect_gt(calls_after[cell], calls_before[cell])
  expect_gt(emails_after[cell], emails_before[cell])
  # The injection is durable: a second evaluation sees the same updated state.
  expect_equal(walk_evaluate(handle, 2L, theta_choice)$value, calls_after)
})

test_that("handle misuse aborts with cli errors", {
  local_cli_context()
  js <- walk_two_process()
  handle <- walk_open(js)

  # Evaluate on an unopened object.
  not_open <- structure(list(), class = "walk_handle.goldfish")
  expect_error(
    walk_evaluate(not_open, 1L, c(0, 0)),
    class = "goldfish_walk_not_open"
  )

  # Advance backward.
  walk_advance(handle, 3)
  expect_error(walk_advance(handle, 1), class = "goldfish_walk_out_of_order")

  # Inject an event before the current clock.
  expect_error(
    walk_inject(
      handle,
      list(
        layer = "friendship",
        sender = 1L,
        receiver = 2L,
        increment = 1,
        time = 0.5
      )
    ),
    class = "goldfish_walk_out_of_order"
  )

  # Unknown fid, wrong theta length, unknown layer.
  expect_error(
    walk_evaluate(handle, 99L, c(0, 0)),
    class = "goldfish_walk_bad_fid"
  )
  expect_error(
    walk_evaluate(handle, 1L, c(0, 0, 0)),
    class = "goldfish_walk_bad_theta"
  )
  expect_error(
    walk_inject(
      handle,
      list(layer = "nope", sender = 1L, receiver = 2L, increment = 1)
    ),
    class = "goldfish_walk_bad_event"
  )
})

test_that("walk_open aborts on an incomplete specification", {
  local_cli_context()
  data <- walk_fixture_data()
  # calls modeled rate-only (choice omitted): a half-specified generative spec.
  calls_rate_only <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  js <- suppressWarnings(
    make_joint_specification(calls_rate_only, emails_spec, data = data)
  )

  expect_error(
    walk_open(js),
    class = "goldfish_walk_incomplete_spec"
  )
  expect_snapshot(walk_open(js), error = TRUE)
})

test_that("walk_open defers effect-free completed defaults to the consumers", {
  local_cli_context()
  data <- walk_fixture_data()
  calls_rate_only <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  js <- suppressWarnings(
    make_joint_specification(calls_rate_only, emails_spec, data = data)
  )
  # Completion supplies a uniform (effect-free) calls choice; that block passes
  # the completeness assert but is walked by simulate() / estimate_dynes(), not
  # this substrate -- a clear boundary, not the incomplete-spec abort.
  completed <- suppressWarnings(
    complete_generative_spec(js, consumer = "simulate")
  )
  expect_error(
    walk_open(completed),
    class = "goldfish_walk_unsupported"
  )
})

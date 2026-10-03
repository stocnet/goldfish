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
  expect_s3_class(handle, "goldfishWalk")

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

test_that("a sender's row equals its block of the full choice matrix", {
  js <- walk_two_process()
  handle <- walk_open(js)
  walk_advance(handle, 4.5)
  theta_choice <- c(0, 2)
  n2 <- 6L

  full <- walk_evaluate(handle, 2L, theta_choice)$value
  for (s in seq_len(6L)) {
    row <- walk_evaluate(handle, 2L, theta_choice, sender = s)
    expect_equal(row$value, full[(s - 1L) * n2 + seq_len(n2)])
    expect_identical(row$index$index_i, rep(s, n2))
    expect_identical(row$index$index_j, seq_len(n2))
  }
})

test_that("handle misuse aborts with cli errors", {
  local_cli_context()
  js <- walk_two_process()
  handle <- walk_open(js)

  # Evaluate on an unopened object.
  not_open <- structure(list(), class = "goldfishWalk")
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

  # A sender out of range, and a sender asked of a fid that has no per-sender
  # row to give (fid 1 is the rate model).
  expect_error(
    walk_evaluate(handle, 2L, c(0, 0), sender = 99L),
    class = "goldfish_walk_bad_sender"
  )
  expect_error(
    walk_evaluate(handle, 1L, c(0, 0), sender = 1L),
    class = "goldfish_walk_bad_sender"
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

test_that("walk_open opens a process without a rate for an anchored run", {
  data <- walk_fixture_data()
  choice_only <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  expect_error(walk_open(choice_only), class = "goldfish_walk_incomplete_spec")
  handle <- walk_open(choice_only, times = "observed")
  expect_s3_class(handle, "goldfishWalk")
})

test_that("walk_open still refuses a missing choice for an anchored run", {
  data <- walk_fixture_data()
  rate_only <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  expect_error(
    walk_open(rate_only, times = "observed"),
    class = "goldfish_walk_incomplete_spec"
  )
})

test_that("walk_open defers a whole effect-free family without renumbering", {
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
  # Completion supplies an effect-free calls choice. Nothing can compile it,
  # so the walk defers it and keeps every fid the specification numbered.
  completed <- suppressWarnings(
    complete_generative_spec(js, consumer = "simulate")
  )
  handle <- walk_open(completed)

  expect_identical(handle$process_map$fid, completed$process_map$fid)
  expect_identical(handle$deferred$fid, 2L)
  expect_setequal(
    unlist(lapply(handle$engines, `[[`, "fids")),
    c(1L, 3L, 4L)
  )
  expect_snapshot(walk_evaluate(handle, 2L, numeric(0)), error = TRUE)

  # The fids after the deferred one still mean what they meant: emails'
  # choice evaluates exactly as it does with calls absent.
  alone <- walk_open(emails_spec)
  expect_equal(
    walk_evaluate(handle, 4L, c(0.2, 0.3), sender = 1L)$value,
    walk_evaluate(alone, 2L, c(0.2, 0.3), sender = 1L)$value
  )
})

# calls is the modeled layer; emails is an exogenous layer the choice reads, so
# a constraint on it is stepped by the shared walk. Email stamps sit between the
# call stamps: a support mask reads the atoms strictly before an event's time,
# and a tie would make the batch and the live read differ by definition.
walk_constrained_data <- function(emails) {
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    cbind(emails, layer = "emails")
  )
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

test_that("a constrained risk set is read off the availability effect", {
  # The handle keeps the availability the batch walk keeps, stepped when it
  # is read; it does not rebuild a gate or a grid from the mask.
  data <- walk_constrained_data(data.frame(
    from = c(2L, 3L, 4L, 6L),
    to = c(1L, 2L, 3L, 1L),
    time = c(1.5, 2.5, 3.5, 4.5)
  ))
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(emails),
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  handle <- walk_open(spec)
  walk_advance(handle, 3)
  rebuilt <- 0L
  local_mocked_bindings(
    sender_gate_from_mask = function(...) {
      rebuilt <<- rebuilt + 1L
      stop("rebuilt")
    }
  )
  for (fid in handle$process_map$fid) {
    engine <- walk_engine_of_fid(handle, fid)
    risk_set <- walk_risk_set(handle, engine, fid)
    av <- engine$availability[[as.character(fid)]]
    read <- if (engine$is_sender) {
      risk_set$active_sender
    } else {
      risk_set$active_dyad
    }
    expect_identical(read, av$value, info = fid)
  }
  expect_identical(rebuilt, 0L)
  # By then `emails` holds 2 -> 1 and 3 -> 2: the choice grid excludes them.
  choice <- walk_engine_of_fid(handle, 2L)$availability[["2"]]$value
  expect_false(choice[2L, 1L])
  expect_false(choice[3L, 2L])
  expect_true(choice[4L, 3L])
})

test_that("a dyadic support constraint is maintained live on the handle", {
  data <- walk_constrained_data(data.frame(
    from = c(2L, 3L, 4L, 6L),
    to = c(1L, 2L, 3L, 1L),
    time = c(1.5, 2.5, 3.5, 4.5)
  ))
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(emails),
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

test_that("a sender with only itself left replays live against batch", {
  # N1 starts tied to every other actor, so its creation rate is gated out
  # until it dissolves one of them; the live gate has to drop the self-dyad
  # the creation mask allows, at every event the batch walk stores.
  spec <- suppressMessages(make_specification(
    rate = list(creation ~ 1 + indeg),
    choice = list(creation ~ inertia),
    model = "DyNAM",
    data = self_only_sender_data()
  ))
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_gated, 0L)
})

test_that("a receiver-axis support constraint is maintained live", {
  data <- walk_constrained_data(data.frame(
    from = c(2L, 3L, 4L, 6L),
    to = c(6L, 6L, 2L, 3L),
    time = c(1.5, 2.5, 3.5, 4.5)
  ))
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(emails),
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ indeg(emails) < 1
  )
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

test_that("a flavored layer's derived masks are maintained live", {
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    layer = "calls",
    model = "DyNAM",
    data = flavored_fixture_data()
  )
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

test_that("a constraint on an object no formula reads replays exactly", {
  # `emails` is read only by the constraint, so the walk steps it for the
  # mask alone; the handle maintains that mask live and matches the batch.
  data <- walk_constrained_data(data.frame(
    from = c(2L, 3L, 4L, 6L),
    to = c(1L, 2L, 3L, 1L),
    time = c(1.5, 2.5, 3.5, 4.5)
  ))
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

test_that("an atom moved at an event's own stamp is not yet in force", {
  # Evaluating an observed event at its stamp, a driver steps strictly
  # before it, as the batch schedule puts the rows sharing a stamp after the
  # event. The mask then reads its atoms strictly before the stamp: at N2's
  # call at t = 2, 2 -> 4 is still excluded (emailed at 1.5, dissolved at 2)
  # and 2 -> 5 still allowed (emailed at 2).
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls",
      weight = 1
    ),
    data.frame(
      from = c(2L, 2L, 2L),
      to = c(4L, 4L, 5L),
      time = c(1.5, 2, 2),
      layer = "emails",
      weight = c(1, -1, 1)
    )
  )
  data <- list(
    info = list(
      name = "tie",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  batch <- suppressWarnings(preprocess_joint(spec))
  map <- attr(batch, "process_map")
  choice_fid <- map$fid[map$family == "choice"]
  out <- batch[[as.character(choice_fid)]]
  handle <- suppressWarnings(walk_open(spec))
  calls <- ties[ties$layer == "calls", ]
  theta <- 0.4
  for (e in seq_len(nrow(calls))) {
    walk_advance(handle, calls$time[e], inclusive = FALSE)
    live <- walk_evaluate(handle, choice_fid, theta)
    state <- materialize_process_state(
      out,
      model_type = "DyNAM-M",
      event_index = e,
      has_intercept = FALSE
    )
    reference <- evaluate_process_state(state, theta)$value
    n2 <- state$n_actors2
    sender <- calls$from[e]
    expect_equal(
      live$value[(sender - 1L) * n2 + seq_len(n2)],
      reference,
      info = paste("event", e)
    )
    walk_inject(
      handle,
      list(
        layer = "calls",
        sender = sender,
        receiver = calls$to[e],
        time = calls$time[e],
        increment = 1
      )
    )
  }
  # The tie is reached: at t = 2 the batch excludes 2 -> 4 and allows 2 -> 5.
  state <- materialize_process_state(
    out,
    model_type = "DyNAM-M",
    event_index = 2L,
    has_intercept = FALSE
  )
  at_tie <- evaluate_process_state(state, theta)$value
  expect_identical(at_tie[4] == 0, TRUE)
  expect_identical(at_tie[5] > 0, TRUE)
})

test_that("a tied dependent event of another process is not in force", {
  # The emails event 2 -> 5 shares N2's call stamp, t = 2, and is scheduled
  # ahead of it. Replayed one stamp at a time, every dependent event at t is
  # evaluated before any row at t applies, and the handle equals the batch.
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls",
      weight = 1
    ),
    data.frame(
      from = c(2L, 2L, 3L),
      to = c(4L, 5L, 1L),
      time = c(1.5, 2, 2.5),
      layer = "emails",
      weight = 1
    )
  )
  data <- list(
    info = list(
      name = "tie",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
  emails <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  calls <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  joint <- make_joint_specification(emails, calls, data = data)

  replay <- walk_replay_against_batch(joint)

  expect_identical(replay$n_checked, 16L)
  expect_gt(replay$n_excluded, 0L)
})

test_that("node-composition changes advance with the handle's clock", {
  data <- walk_constrained_data(data.frame(from = 2L, to = 6L, time = 1.5))
  # N6 is never called and N1 is called only after it returns, so no observed
  # event reaches an absent node; N1 is also an absent sender at t = 3 and 4.
  changes <- data.frame(
    time = c(1.5, 2.5, 3.5, 4.5),
    node = c(6L, 1L, 6L, 1L),
    var = "active"
  )
  changes$value <- list(list(FALSE), list(FALSE), list(TRUE), list(TRUE))
  data$changes <- changes
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)

  handle <- suppressWarnings(walk_open(spec))
  expect_equal(walk_next_breakpoint(handle), 1.5)
  walk_advance(handle, 2.5)
  expect_equal(walk_evaluate(handle, 1L, c(0, 0))$value[c(1L, 6L)], c(0, 0))
})

test_that("window effects replay against the batch walk", {
  for (spec in list(
    parity_toy_spec_windowed(),
    parity_toy_spec_windowed_rate()
  )) {
    replay <- walk_replay_against_batch(spec)
    expect_gt(replay$n_checked, 0L)
  }
})

test_that("a scheduled event applies when the clock reaches it", {
  js <- walk_two_process()
  event <- list(
    layer = "friendship",
    sender = 5L,
    receiver = 6L,
    increment = 1,
    time = 1.25
  )

  scheduled <- walk_open(js)
  walk_schedule(scheduled, event)
  expect_equal(walk_next_breakpoint(scheduled), 1)
  walk_advance(scheduled, 1)
  expect_equal(walk_next_breakpoint(scheduled), 1.25)
  expect_equal(scheduled$state$networks[["friendship"]][5, 6], 0)
  walk_advance(scheduled, 1.5)
  expect_equal(walk_next_breakpoint(scheduled), 2)
  expect_equal(scheduled$state$networks[["friendship"]][5, 6], 1)

  injected <- walk_open(js)
  walk_advance(injected, 1)
  walk_inject(injected, event)
  walk_advance(injected, 1.5)
  expect_equal(
    walk_evaluate(scheduled, 2L, c(0, 2))$value,
    walk_evaluate(injected, 2L, c(0, 2))$value
  )
})

test_that("scheduled events apply in time order, ties in scheduling order", {
  js <- walk_two_process()
  handle <- walk_open(js)
  replace_at <- function(value, time) {
    list(
      layer = "friendship",
      sender = 5L,
      receiver = 6L,
      replace = value,
      time = time
    )
  }
  walk_schedule(handle, replace_at(9, 1.75))
  walk_schedule(handle, replace_at(3, 1.25))
  walk_schedule(handle, replace_at(7, 1.25))

  walk_advance(handle, 1.5)
  expect_equal(handle$state$networks[["friendship"]][5, 6], 7)
  walk_advance(handle, 1.8)
  expect_equal(handle$state$networks[["friendship"]][5, 6], 9)
  expect_equal(walk_next_breakpoint(handle), 2)
})

test_that("scheduling misuse aborts with cli errors", {
  js <- walk_two_process()
  handle <- walk_open(js)
  walk_advance(handle, 3)
  event <- list(layer = "friendship", sender = 5L, receiver = 6L, increment = 1)
  expect_error(walk_schedule(handle, event), class = "goldfish_walk_bad_event")
  expect_error(
    walk_schedule(handle, c(event, time = 2)),
    class = "goldfish_walk_out_of_order"
  )
})

test_that("a vector-kind REM constraint at outer replays exactly", {
  # The handle reads the effect's two factors, as the batch stores them.
  fx <- suppressWarnings(availability_vector_rem_data())
  actors <- fx$actors
  for (constraint in list(~ alter(actors$allow) > 0, ~ ego(actors$allow) > 0)) {
    spec <- suppressWarnings(suppressMessages(make_specification(
      rate = ~ 1 + inertia + recip,
      model = "REM",
      layer = "dep",
      data = fx$data,
      support_constraint = constraint
    )))
    replay <- walk_replay_against_batch(spec)
    expect_gt(replay$n_checked, 0L)
    expect_gt(replay$n_excluded, 0L)
  }
})

test_that("a vector-kind coordination constraint at outer replays exactly", {
  fx <- suppressWarnings(availability_vector_coord_data())
  states <- fx$states
  spec <- suppressWarnings(suppressMessages(make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + indeg + trans,
    choice_sub_model = "choice_coordination",
    model = "DyNAM",
    layer = "create_bilat",
    data = fx$data,
    support_constraint = ~ alter(states$allow) > 0
  )))
  replay <- walk_replay_against_batch(spec)
  expect_gt(replay$n_checked, 0L)
  expect_gt(replay$n_excluded, 0L)
})

# The rate's sender gate on a support mask. A sender is at risk only when
# some present receiver other than itself is allowed: on a one-mode layer
# the self-dyad is never a receiver, even where the mask allows it, as it
# always does for a creation flavor, since no actor holds a tie to itself.

test_that("the oracle drops the self-dyad only on a one-mode layer", {
  grid <- matrix(c(TRUE, FALSE, FALSE, FALSE), 2L, 2L)
  present <- c(TRUE, TRUE)

  expect_identical(
    oracle_sender_gate(grid, present, one_mode = TRUE),
    c(FALSE, FALSE)
  )
  expect_identical(
    oracle_sender_gate(grid, present, one_mode = FALSE),
    c(TRUE, FALSE)
  )
})

test_that("the fixture holds a sender whose only creation is itself", {
  data <- self_only_sender_data()
  ties <- as.data.frame(data$ties)
  state <- matrix(0, 4L, 4L)
  history <- ties[is.na(ties$time), ]
  state[cbind(history$from, history$to)] <- 1
  events <- ties[!is.na(ties$time), ]
  events <- events[order(events$time), ]

  self_only <- logical(nrow(events))
  for (k in seq_len(nrow(events))) {
    creation <- state == 0
    self_only[k] <- creation[1L, 1L] && !any(creation[1L, -1L])
    cell <- cbind(events$from[k], events$to[k])
    state[cell] <- state[cell] + events$weight[k]
  }

  # Saturated until N1 dissolves at t = 7, free at t = 8 and 9, and
  # saturated again once N1 recreates its tie at t = 9.
  expect_identical(self_only, events$time <= 7 | events$time == 10)
  expect_setequal(ties$flavor[!is.na(ties$time)], c("creation", "dissolution"))
})

test_that("the folded gate excludes the self-dyad at every mask kind", {
  # Point: sender 1 is allowed only its own cell, sender 3 nothing at all.
  point <- matrix(FALSE, 3L, 3L)
  point[1L, 1L] <- TRUE
  point[2L, c(1L, 3L)] <- TRUE
  masks <- list(as.logical(point), as.logical(point))
  present <- rep(TRUE, 3L)

  gate <- fold_gate_timeline(masks, 0L, present, present, drop_diagonal = TRUE)
  grids <- mask_grids(masks, 0L, 3L, 3L)

  expect_identical(
    gate,
    lapply(grids, oracle_sender_gate, present = present, one_mode = TRUE)
  )
  expect_identical(gate[[1L]], c(FALSE, TRUE, FALSE))

  # Alter: only receiver 1 is allowed, so sender 1 has nobody but itself.
  alter <- c(TRUE, FALSE, FALSE)
  gate <- fold_gate_timeline(
    list(alter, alter),
    1L,
    present,
    present,
    drop_diagonal = TRUE
  )
  expect_identical(gate[[2L]], c(FALSE, TRUE, TRUE))

  # Ego with one present receiver: only sender 1 loses its last receiver.
  only_first <- c(TRUE, FALSE, FALSE)
  gate <- fold_gate_timeline(
    list(rep(TRUE, 3L), rep(TRUE, 3L)),
    2L,
    present,
    only_first,
    drop_diagonal = TRUE
  )
  expect_identical(gate[[1L]], c(FALSE, TRUE, TRUE))

  # Global, same reduction.
  gate <- fold_gate_timeline(
    list(TRUE, TRUE),
    3L,
    present,
    only_first,
    drop_diagonal = TRUE
  )
  expect_identical(gate[[1L]], c(FALSE, TRUE, TRUE))
})

test_that("a two-mode layer keeps every allowed cell", {
  # Two node sets: index 1 on each axis names different nodes.
  point <- matrix(FALSE, 2L, 3L)
  point[1L, 1L] <- TRUE
  masks <- list(as.logical(point), as.logical(point))

  gate <- fold_gate_timeline(
    masks,
    0L,
    rep(TRUE, 2L),
    rep(TRUE, 3L),
    drop_diagonal = FALSE
  )

  expect_identical(gate[[1L]], c(TRUE, FALSE))
})

test_that("a flip of a sender's own cell moves no count", {
  off <- matrix(FALSE, 3L, 3L)
  off[1L, 2L] <- TRUE
  on <- off
  on[1L, 1L] <- TRUE
  present <- rep(TRUE, 3L)

  gate <- fold_gate_timeline(
    list(as.logical(off), as.logical(on), as.logical(off)),
    0L,
    present,
    present,
    drop_diagonal = TRUE
  )

  expect_identical(gate, rep(list(c(TRUE, FALSE, FALSE)), 3L))
})

test_that("a receiver leaving strands the sender it was allowed for", {
  # Sender 2 may only reach receiver 3; receiver 3 leaves at the second event.
  point <- matrix(FALSE, 3L, 3L)
  point[1L, 1L] <- TRUE
  point[2L, 3L] <- TRUE
  point[3L, 2L] <- TRUE
  masks <- rep(list(as.logical(point)), 3L)
  crossings <- list(
    update = matrix(c(3, 0), nrow = 2L),
    pointer = c(0L, 1L, 1L)
  )

  gate <- fold_gate_timeline(
    masks,
    0L,
    rep(TRUE, 3L),
    rep(TRUE, 3L),
    receiver_crossings = crossings,
    drop_diagonal = TRUE
  )

  expect_identical(gate[[1L]], c(FALSE, TRUE, TRUE))
  expect_identical(gate[[2L]], c(FALSE, FALSE, TRUE))
  expect_identical(gate[[3L]], c(FALSE, FALSE, TRUE))
})

test_that("the folded and from-scratch gates match the oracle under flips", {
  # A seeded walk per mask kind and per layer shape: the mask flips at every
  # event (a point mask also flips one diagonal cell every other event) and
  # receivers come and go. Four actors with sparse presence make a sender
  # whose only present, allowed receiver is itself common at every kind,
  # which is where a counter that forgot the self-dyad would go wrong.
  random_case <- function(kind, n1, n2, one_mode, n_events, seed) {
    withr::local_seed(seed)
    flip_some <- function(x, rate) {
      hit <- runif(length(x)) < rate
      x[hit] <- !x[hit]
      x
    }
    size <- switch(as.character(kind), "3" = 1L, "2" = n1, "1" = n2, n1 * n2)
    masks <- vector("list", n_events)
    masks[[1L]] <- runif(size) < runif(1L, 0.2, 0.7)
    for (e in seq_len(n_events)[-1L]) {
      masks[[e]] <- flip_some(masks[[e - 1L]], 0.2)
      if (kind == 0L && one_mode && e %% 2L == 0L) {
        own <- (sample.int(n1, 1L) - 1L) * (n1 + 1L) + 1L
        masks[[e]][[own]] <- !masks[[e]][[own]]
      }
    }
    presence <- vector("list", n_events)
    presence[[1L]] <- runif(n2) < 0.5
    pointer <- integer(n_events)
    updates <- vector("list", n_events)
    for (e in seq_len(n_events)[-1L]) {
      presence[[e]] <- flip_some(presence[[e - 1L]], 0.25)
      moved <- which(presence[[e]] != presence[[e - 1L]])
      updates[[e]] <- rbind(moved, as.numeric(presence[[e]][moved]))
      pointer[[e]] <- pointer[[e - 1L]] + length(moved)
    }
    list(
      masks = masks,
      presence = presence,
      crossings = list(
        update = do.call(cbind, c(list(matrix(0, 2L, 0L)), updates)),
        pointer = pointer
      )
    )
  }

  for (one_mode in c(TRUE, FALSE)) {
    n1 <- if (one_mode) 4L else 3L
    n2 <- if (one_mode) 4L else 5L
    for (kind in 0:3) {
      wrong_fold <- 0L
      wrong_scratch <- 0L
      self_only <- 0L
      crossed <- 0L
      for (seed in 1:12) {
        case <- random_case(kind, n1, n2, one_mode, 20L, seed)
        gate <- fold_gate_timeline(
          case$masks,
          kind,
          rep(TRUE, n1),
          case$presence[[1L]],
          receiver_crossings = case$crossings,
          drop_diagonal = one_mode
        )
        grids <- mask_grids(case$masks, kind, n1, n2)
        crossed <- crossed + ncol(case$crossings$update)
        for (e in seq_along(grids)) {
          present <- case$presence[[e]]
          oracle <- oracle_sender_gate(grids[[e]], present, one_mode)
          scratch <- sender_gate_from_mask(
            case$masks[[e]],
            kind,
            present,
            n1,
            n2,
            drop_diagonal = one_mode
          )
          wrong_fold <- wrong_fold + sum(gate[[e]] != oracle)
          wrong_scratch <- wrong_scratch + sum(scratch != oracle)
          self_only <- self_only +
            sum(oracle_sender_gate(grids[[e]], present, FALSE) & !oracle)
        }
      }
      label <- paste0(
        "kind ",
        kind,
        if (one_mode) ", one-mode" else ", two-mode"
      )
      expect_identical(wrong_fold, 0L, label = paste("folded gate,", label))
      expect_identical(wrong_scratch, 0L, label = paste("from scratch,", label))
      expect_gt(crossed, 0L, label = paste("receiver crossings,", label))
      # The self-only case is reached on one node set, never on two.
      if (one_mode) {
        expect_gt(self_only, 0L, label = paste("self-only senders,", label))
      } else {
        expect_identical(self_only, 0L, label = label)
      }
    }
  }
})

test_that("an undirected flavored layer gates out its self-only sender", {
  data <- self_only_sender_data()
  data$info$directed <- c(calls = FALSE)
  # N2 -> N1 at t = 6 re-creates the N1 - N2 tie N1 already holds once the
  # layer is undirected, so the undirected history leaves it out.
  data$ties <- data$ties[!(data$ties$time %in% 6), ]
  spec <- suppressMessages(make_specification(
    rate = list(creation ~ 1 + indeg),
    model = "DyNAM",
    data = data
  ))

  rate <- suppressWarnings(suppressMessages(
    preprocess_joint(spec, set_preprocessing())
  ))[[1L]]
  gate <- walk_presence_buffer(
    rate$active_sender_init,
    rate$active_sender_update,
    rate$active_sender_update_pointer,
    length(rate$event_time)
  )

  ties <- as.data.frame(data$ties)
  state <- matrix(0, 4L, 4L)
  history <- ties[is.na(ties$time), ]
  state[cbind(history$from, history$to)] <- 1
  state[cbind(history$to, history$from)] <- 1
  events <- ties[!is.na(ties$time), ]
  events <- events[order(events$time), ]
  oracle <- vector("list", nrow(events))
  for (k in seq_len(nrow(events))) {
    oracle[[k]] <- oracle_sender_gate(state == 0, rep(TRUE, 4L), TRUE)
    cell <- rbind(
      c(events$from[k], events$to[k]),
      c(events$to[k], events$from[k])
    )
    state[cell] <- state[cell[1L, , drop = FALSE]] + events$weight[k]
  }

  expect_length(gate, nrow(events))
  expect_identical(lapply(gate, as.logical), oracle)
  # N1 holds every undirected tie until it dissolves one, and again once it
  # recreates it, so it is gated out while the others are not.
  expect_false(oracle[[1L]][[1L]])
  expect_true(any(vapply(oracle, `[[`, logical(1), 1L)))
})

test_that("the fixture's rate gates out the sender that can only self-create", {
  data <- self_only_sender_data()
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg),
    model = "DyNAM",
    data = data
  )

  pre <- suppressMessages(preprocess_joint(spec, set_preprocessing()))
  rate <- pre[[1L]]
  gate <- rate$active_sender_init
  update <- rate$active_sender_update
  pointer <- rate$active_sender_update_pointer
  seen <- 0L
  at_risk <- vapply(
    seq_along(pointer),
    function(e) {
      hi <- pointer[[e]]
      if (hi > seen) {
        cols <- (seen + 1L):hi
        gate[update[1L, cols]] <<- as.logical(update[2L, cols])
        seen <<- hi
      }
      gate[[1L]]
    },
    logical(1)
  )

  # N1 is at risk only while it can create a tie it does not already hold.
  expect_identical(at_risk, rate$event_time %in% c(8, 9))
  # Three senders at risk on every row and N1 on two more: 32 over the ten
  # rows, less the first row's three, which integrate a zero interval, over
  # the nine unit intervals.
  expect_equal(rate$avg_active_entity, 29 / 9)
})

test_that("the self-only sender is no rate candidate and carries no exposure", {
  data <- self_only_sender_data()
  spec <- suppressMessages(make_specification(
    rate = list(creation ~ 1 + indeg),
    model = "DyNAM",
    data = data
  ))

  gathered <- suppressMessages(compute_statistics(
    spec,
    sub_model = "rate",
    output = "gather"
  ))
  n_events <- length(gathered$n_candidates)
  candidates <- split(
    gathered$index_i,
    rep(seq_len(n_events), gathered$n_candidates)
  )
  holds_n1 <- vapply(candidates, function(i) 1L %in% i, logical(1))

  expect_identical(gathered$n_candidates, c(3, 3, 3, 3, 3, 3, 3, 4, 4, 3))
  # N1 is a candidate only at the two events it can create a tie at, and a
  # right-censored row, which carries exposure and no event, leaves it out
  # too while it can only reach itself.
  expect_identical(unname(holds_n1), seq_len(n_events) %in% c(8L, 9L))
  expect_gt(sum(!gathered$is_dependent & !holds_n1), 0L)
})

test_that("a separable constraint gates out a self-only sender at alter kind", {
  # Only N1 has x > 0, so N1 is the only allowed receiver and N1 itself has
  # no one to reach. The mask stays one value per receiver.
  data <- list(
    info = list(
      name = "alter-self",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      x = c(1, 0, 0, 0),
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = c(2L, 3L, 4L, 2L),
      to = c(1L, 1L, 1L, 1L),
      time = as.numeric(1:4),
      weight = 1,
      layer = "calls"
    )
  )
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    support_constraint = ~ alter(x) > 0,
    data = data
  ))

  pre <- suppressWarnings(suppressMessages(preprocess_joint(spec)))
  rate <- pre[[1L]]
  gate <- walk_presence_buffer(
    rate$active_sender_init,
    rate$active_sender_update,
    rate$active_sender_update_pointer,
    length(rate$event_time)
  )

  expect_identical(rate$support_mask$stored_kind, 1L)
  expect_null(dim(rate$support_mask$initial))
  expect_length(rate$support_mask$initial, 4L)
  expect_identical(
    lapply(gate, as.logical),
    rep(list(c(FALSE, TRUE, TRUE, TRUE)), length(gate))
  )
})

test_that("the relational risk set matches the panel endpoint count", {
  data <- self_only_sender_data()
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg),
    model = "DyNAM",
    data = data
  )
  ties <- as.data.frame(data$ties)
  history <- ties[is.na(ties$time), ]
  state <- matrix(0, 4L, 4L)
  state[cbind(history$from, history$to)] <- 1

  pre <- suppressMessages(preprocess_joint(spec, set_preprocessing()))
  at_first_event <- sum(pre[[1L]]$active_sender_init)

  # The panel path counts a flavor's rate entities from a materialized state
  # with self-loops removed; the relational path folds them from the mask.
  expect_identical(
    at_first_event,
    endpoint_entity_count(state, "actor", "creation", one_mode = TRUE)
  )
})

test_that("the from-scratch gate matches the oracle at every kind", {
  present <- rep(TRUE, 3L)
  point <- matrix(FALSE, 3L, 3L)
  point[1L, 1L] <- TRUE
  point[2L, c(1L, 3L)] <- TRUE
  kinds <- list(
    list(mask = as.logical(point), kind = 0L),
    list(mask = c(TRUE, FALSE, FALSE), kind = 1L),
    list(mask = c(TRUE, TRUE, FALSE), kind = 2L),
    list(mask = TRUE, kind = 3L)
  )

  for (case in kinds) {
    grid <- mask_grids(list(case$mask), case$kind, 3L, 3L)[[1L]]
    expect_identical(
      sender_gate_from_mask(
        case$mask,
        case$kind,
        present,
        3L,
        3L,
        drop_diagonal = TRUE
      ),
      oracle_sender_gate(grid, present, one_mode = TRUE),
      label = paste("kind", case$kind)
    )
    expect_identical(
      sender_gate_from_mask(
        case$mask,
        case$kind,
        present,
        3L,
        3L,
        drop_diagonal = FALSE
      ),
      oracle_sender_gate(grid, present, one_mode = FALSE),
      label = paste("kind", case$kind, "two-mode")
    )
  }
})

test_that("the live walk gates out the self-only sender until it can create", {
  data <- self_only_sender_data()
  spec <- single_process_joint(make_specification(
    rate = list(creation ~ 1 + indeg),
    choice = list(creation ~ inertia),
    model = "DyNAM",
    data = data
  ))
  completed <- suppressMessages(complete_generative_spec(
    spec,
    consumer = "simulate",
    times = "generated"
  ))
  handle <- suppressMessages(walk_open(completed, set_preprocessing()))
  map <- completed$process_map
  rate_fid <- map$fid[map$family == "rate"]
  engine <- walk_engine_of_fid(handle, rate_fid)
  batch <- suppressMessages(preprocess_joint(spec, set_preprocessing()))

  # The walk opens on the state the history left, which is the state the
  # batch gate reads at its first stored event.
  expect_identical(
    walk_risk_set(handle, engine, rate_fid)$active_sender,
    as.logical(batch[[1L]]$active_sender_init)
  )
  expect_false(walk_risk_set(handle, engine, rate_fid)$active_sender[[1L]])

  # Once N1's tie to N2 is removed, N1 can create again.
  walk_inject(
    handle,
    list(layer = "calls", sender = 1L, receiver = 2L, increment = -1, time = 7)
  )

  expect_true(walk_risk_set(handle, engine, rate_fid)$active_sender[[1L]])
})

test_that("the candidate set of a validated event excludes the sender", {
  # Sender 1's only allowed cell is its own: an empty risk set, not a
  # forced choice to itself.
  s <- matrix(FALSE, 4L, 4L)
  s[1L, 1L] <- TRUE
  s[2L, c(1L, 3L)] <- TRUE
  masks <- list(s, s)

  expect_error(
    validate_support_stream(
      mask_from_timeline(masks),
      event_sender = c(1L, 2L),
      event_receiver = c(2L, 3L),
      is_dependent = c(1L, 1L),
      active_1 = rep(TRUE, 4L),
      active_2 = rep(TRUE, 4L),
      family = "choice",
      drop_diagonal = TRUE
    ),
    "empty risk set"
  )

  # With two receivers allowed, one of them the sender itself, the event is
  # a forced choice over the one real candidate.
  s2 <- matrix(FALSE, 4L, 4L)
  s2[1L, c(1L, 2L)] <- TRUE
  s2[2L, c(1L, 3L)] <- TRUE
  expect_warning(
    validate_support_stream(
      mask_from_timeline(list(s2, s2)),
      event_sender = c(1L, 2L),
      event_receiver = c(2L, 3L),
      is_dependent = c(1L, 1L),
      active_1 = rep(TRUE, 4L),
      active_2 = rep(TRUE, 4L),
      family = "choice",
      drop_diagonal = TRUE
    ),
    "forced choice"
  )
})

test_that("a self-only sender's observed event meets an empty risk set", {
  # Only N1 has x > 0: N1 is the one receiver the constraint allows, so N1's
  # own event at t = 3 has no candidate at all, rather than a candidate set
  # of itself that the observed receiver falls outside of.
  data <- list(
    info = list(
      name = "alter-self",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      x = c(1, 0, 0, 0),
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = c(2L, 3L, 1L, 4L),
      to = c(1L, 1L, 2L, 1L),
      time = as.numeric(1:4),
      weight = 1,
      layer = "calls"
    )
  )
  spec <- suppressMessages(make_specification(
    choice = ~inertia,
    layer = "calls",
    support_constraint = ~ alter(x) > 0,
    data = data
  ))

  expect_error(
    suppressWarnings(suppressMessages(preprocess_joint(spec))),
    "empty risk set at event 3"
  )
})

test_that("a coordination self-only sender meets an empty risk set", {
  # Coordination symmetrises a vector mask: dyad (i, j) is allowed iff x[i]
  # and x[j] are. Only N1 has x > 0, so N1 is allowed and its one allowed
  # receiver is itself, which is no receiver on one node set.
  coordination_data <- function(x, from, to) {
    list(
      info = list(
        name = "coordination-self",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = FALSE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:4),
        mode = "p",
        x = x,
        stringsAsFactors = FALSE
      ),
      ties = data.frame(
        from = from,
        to = to,
        time = as.numeric(seq_along(from)),
        weight = 1,
        layer = "calls"
      )
    )
  }
  coordination_spec <- function(data) {
    suppressMessages(make_specification(
      choice = ~inertia,
      choice_sub_model = "choice_coordination",
      layer = "calls",
      support_constraint = ~ alter(x) > 0,
      data = data
    ))
  }

  self_only <- coordination_spec(
    coordination_data(c(1, 0, 0, 0), from = c(1L, 2L), to = c(2L, 3L))
  )
  # Matched apart from the capture: a count that kept the self-dyad raises
  # the observed-dyad-excluded error instead, which then fails this match
  # and still lets the forced-choice case below run.
  failure <- expect_error(
    suppressWarnings(suppressMessages(preprocess_joint(self_only)))
  )
  expect_match(conditionMessage(failure), "empty risk set at event 1")

  # With N2 allowed as well, N1 and N2 each reach only the other: a forced
  # choice, which a count that kept the self-dyad would see as two
  # candidates.
  pair <- coordination_spec(
    coordination_data(c(1, 1, 0, 0), from = c(1L, 2L), to = c(2L, 1L))
  )
  warnings <- capture_warnings(pre <- suppressMessages(preprocess_joint(pair)))
  expect_identical(pre[[1L]]$active_dyad_encoding, "outer")
  expect_match(warnings, "2 events with a single candidate", all = FALSE)
})

test_that("a free-running run never draws a sender with no receiver", {
  data <- self_only_sender_data()
  js <- single_process_joint(make_specification(
    rate = list(creation ~ 1 + indeg),
    choice = list(creation ~ inertia),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › rate` = c(-1, 0.5),
    `calls › creation › choice` = 0.2
  )

  # Four actors saturate quickly, so a rate that kept a saturated sender
  # would draw one within a few events.
  for (seed in 1:5) {
    out <- simulate(js, nsim = 1, seed = seed, coef = parameters)
    expect_false(anyNA(out$events$receiver), label = paste("seed", seed))
    expect_true(
      all(out$events$sender != out$events$receiver),
      label = paste("seed", seed)
    )
  }
})

test_that("a deferred intercept-only rate never draws a saturated sender", {
  # A rate whose family carries no effect gets no engine of its own: its
  # senders are gated over the choice's live mask when it draws, a second
  # reading of the same gate that has to drop the self-dyad as well.
  data <- self_only_sender_data()
  js <- single_process_joint(suppressMessages(make_specification(
    rate = list(creation ~ 1),
    choice = list(creation ~ inertia),
    model = "DyNAM",
    data = data
  )))
  parameters <- set_parameters(
    js,
    `calls › creation › rate` = -1,
    `calls › creation › choice` = 0.2
  )

  expect_identical(walk_deferred_fids(js)$family, "rate")
  for (seed in 1:5) {
    out <- simulate(js, nsim = 1, seed = seed, coef = parameters)
    expect_false(anyNA(out$events$receiver), label = paste("seed", seed))
    expect_true(
      all(out$events$sender != out$events$receiver),
      label = paste("seed", seed)
    )
  }
})

test_that("a two-mode constrained rate reads no diagonal", {
  # A two-mode grid has no self-dyad to read: an index built for a square
  # grid would run past the mask and hand back NA rather than fail.
  support <- as.logical(matrix(
    c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
    3L,
    2L
  ))
  present <- rep(TRUE, 2L)

  gate <- sender_gate_from_mask(
    support,
    0L,
    present,
    3L,
    2L,
    drop_diagonal = FALSE
  )

  # Sender 2 is allowed no receiver at all; the other two keep one each.
  expect_identical(gate, c(TRUE, FALSE, TRUE))
  expect_false(anyNA(gate))
  expect_snapshot(
    error = TRUE,
    sender_gate_from_mask(support, 0L, present, 3L, 2L, drop_diagonal = TRUE)
  )
})

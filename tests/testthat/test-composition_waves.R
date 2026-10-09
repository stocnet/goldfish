# Composition-to-waves: which actors an interval's draw reads, and (later) the
# rewrite of their presence. Fixtures are small joint specifications with one
# modeled panel layer; the specification is built on event-observed data and
# joined on the panel-observed copy, the way the estimate_dynes() fixtures do.

# Six one-mode actors, wave grid c(0, 2, 5). `friendship` is the modeled panel
# layer (flips 1 -> 2 in interval 1, 3 -> 4 in interval 2), `calls` a modeled
# relational layer, `emails` a relational layer no process models.
involvement_data <- function(observation) {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(
        friendship = "increment",
        calls = "increment",
        emails = "increment"
      ),
      directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
      observation = c(
        friendship = observation,
        calls = "event",
        emails = "event"
      )
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 3L),
        to = c(2L, 4L),
        time = c(2, 5),
        layer = "friendship"
      ),
      data.frame(
        from = c(5L, 2L),
        to = c(6L, 5L),
        time = c(2, 4),
        layer = "calls"
      ),
      data.frame(from = 3L, to = 4L, time = 1, layer = "emails")
    )
  )
}

involvement_spec <- function() {
  events <- involvement_data("event")
  make_joint_specification(
    make_specification(
      choice = ~inertia,
      layer = "friendship",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = events
    ),
    data = involvement_data("panel")
  )
}

# Two node sets (p -> o). The modeled panel layer `membership` flips P2 -> O1,
# whose local indices are (2, 1) but whose global ids are (2, 3).
involvement_twomode_spec <- function() {
  data <- function(observation) {
    list(
      info = list(
        name = "toy2",
        focal = "membership",
        update = c(membership = "increment", talks = "increment"),
        directed = c(membership = TRUE, talks = TRUE),
        observation = c(membership = observation, talks = "event"),
        sender = "p",
        receiver = "o"
      ),
      nodes = data.frame(
        label = c("P1", "P2", "O1", "O2"),
        mode = c("p", "p", "o", "o"),
        stringsAsFactors = FALSE
      ),
      ties = rbind(
        data.frame(from = 2L, to = 3L, time = 2, layer = "membership"),
        data.frame(from = 1L, to = 4L, time = 1, layer = "talks")
      )
    )
  }
  events <- data("event")
  make_joint_specification(
    make_specification(
      choice = ~inertia,
      layer = "membership",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "talks",
      model = "DyNAM",
      data = events
    ),
    data = data("panel")
  )
}

test_that("a modeled relational event involves its sender and receiver", {
  involved <- composition_involvement(involvement_spec(), c(0, 2, 5))
  expect_length(involved, 2L)
  # calls 5 -> 6 sits exactly at w_1 = 2, so it belongs to interval 1.
  expect_true(all(c(5L, 6L) %in% involved[[1L]]))
  # calls 2 -> 5 at t = 4 lies inside interval 2.
  expect_true(all(c(2L, 5L) %in% involved[[2L]]))
})

test_that("a flip involves its from and to nodes in its own interval", {
  involved <- composition_involvement(involvement_spec(), c(0, 2, 5))
  expect_true(all(c(1L, 2L) %in% involved[[1L]]))
  expect_true(all(c(3L, 4L) %in% involved[[2L]]))
  expect_false(any(c(3L, 4L) %in% involved[[1L]]))
})

test_that("an exogenous relational event involves nobody", {
  involved <- composition_involvement(involvement_spec(), c(0, 2, 5))
  # emails 3 -> 4 at t = 1 is replayed, not selected; 3 and 4 flip only in
  # interval 2.
  expect_identical(involved[[1L]], c(1L, 2L, 5L, 6L))
  expect_identical(involved[[2L]], c(2L, 3L, 4L, 5L))
})

test_that("a two-mode flip involves its receiver on the second node set", {
  involved <- composition_involvement(involvement_twomode_spec(), c(0, 2))
  # P2 -> O1 flips (global ids 2 and 3); the talks event P1 -> O2 is at t = 1
  # inside the same interval (global ids 1 and 4).
  expect_true(all(c(2L, 3L) %in% involved[[1L]]))
  expect_true(all(c(1L, 4L) %in% involved[[1L]]))
})

test_that("a two-mode receiver is not read as the same-index first-set node", {
  spec <- involvement_twomode_spec()
  spec$process_map <- spec$process_map[spec$process_map$layer != "talks", ]
  involved <- composition_involvement(spec, c(0, 2))
  expect_identical(involved[[1L]], c(2L, 3L))
})

# The presence-path rewrite. Wave grid c(0, 2, 5): interval 1 = (0, 2],
# interval 2 = (2, 5]. Involvement is passed directly, one vector of global ids
# per interval, so these tests exercise the rewrite alone.

snap_waves <- c(0, 2, 5)

# The warning has its own tests below; the rewrite tests read the data only.
rewrite_quiet <- function(...) {
  suppressWarnings(rewrite_composition(...))
}

# Six actors with the given initial presence and `active` changes.
snap_data <- function(
  active = rep(TRUE, 6L),
  time = numeric(),
  node = integer(),
  value = logical()
) {
  changes <- data.frame(
    time = time,
    node = node,
    var = rep("active", length(time))
  )
  changes$value <- lapply(value, list)
  list(
    nodes = data.frame(label = paste0("N", 1:6), active = active),
    changes = changes
  )
}

# One actor's rewritten `active` rows as (time, value), in time order.
snap_rows <- function(data, actor) {
  ch <- as.data.frame(data$changes)
  sel <- ch$var == "active" & to_global_id(ch$node, data$nodes) == actor
  out <- data.frame(
    time = coerce_time(ch$time[sel]),
    value = as.logical(unlist(unwrap_values(ch$value[sel])))
  )
  out <- out[order(out$time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

snap_expect <- function(time = numeric(), value = logical()) {
  data.frame(time = as.numeric(time), value = as.logical(value))
}

test_that("an involved actor's join inside its interval moves to its start", {
  data <- snap_data(
    active = c(FALSE, FALSE, rep(TRUE, 4L)),
    time = c(1, 3),
    node = 1:2,
    value = c(TRUE, TRUE)
  )
  out <- rewrite_quiet(data, list(1L, 2L), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(0, TRUE))
  expect_identical(snap_rows(out, 2L), snap_expect(2, TRUE))
})

test_that("a join exactly at w_k of an actor involved in k moves to w_{k-1}", {
  data <- snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = 2,
    node = 1L,
    value = TRUE
  )
  out <- rewrite_quiet(data, list(1L, integer()), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(0, TRUE))
})

test_that("a join exactly at w_k of an actor involved only in k + 1 is kept", {
  data <- snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = 2,
    node = 1L,
    value = TRUE
  )
  out <- rewrite_quiet(data, list(integer(), 1L), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(2, TRUE))
})

test_that("an involved actor's leave inside its interval moves to its end", {
  data <- snap_data(time = c(1, 3), node = 1:2, value = c(FALSE, FALSE))
  out <- rewrite_quiet(data, list(1L, 2L), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(2, FALSE))
  expect_identical(snap_rows(out, 2L), snap_expect(5, FALSE))
})

test_that("a leave exactly at w_{k-1} of an actor involved in k moves to w_k", {
  data <- snap_data(time = 2, node = 1L, value = FALSE)
  out <- rewrite_quiet(data, list(integer(), 1L), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(5, FALSE))
})

test_that("an uninvolved actor's join and leave are kept", {
  data <- snap_data(
    time = c(1, 3, 4),
    node = c(3L, 3L, 1L),
    value = c(FALSE, TRUE, FALSE)
  )
  out <- rewrite_quiet(data, list(1L, 1L), snap_waves)
  expect_identical(snap_rows(out, 3L), snap_expect(c(1, 3), c(FALSE, TRUE)))
})

test_that("a leave and rejoin inside an involved interval keep the actor in", {
  # Moving each row on its own would give join @ 2, leave @ 5: absent at w_2.
  leave_first <- snap_data(
    time = c(3, 4),
    node = c(1L, 1L),
    value = c(FALSE, TRUE)
  )
  join_first <- snap_data(
    time = c(4, 3),
    node = c(1L, 1L),
    value = c(TRUE, FALSE)
  )
  for (data in list(leave_first, join_first)) {
    out <- rewrite_quiet(data, list(integer(), 1L), snap_waves)
    expect_identical(snap_rows(out, 1L), snap_expect())
  }
})

test_that("join, leave, join inside an involved interval is one early join", {
  data <- snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = c(3, 3.5, 4),
    node = rep(1L, 3L),
    value = c(TRUE, FALSE, TRUE)
  )
  out <- rewrite_quiet(data, list(integer(), 1L), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(2, TRUE))
})

test_that("an actor involved on both sides of a wave is present across it", {
  # Really absent at w_1 = 2 (leave 1.5, join 3).
  data <- snap_data(time = c(1.5, 3), node = c(1L, 1L), value = c(FALSE, TRUE))
  out <- rewrite_quiet(data, list(1L, 1L), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect())
})

test_that("an actor involved only before a wave leaves at the wave", {
  # Really absent at w_1 = 2; the join at 3 sits in an uninvolved interval.
  data <- snap_data(time = c(1.5, 3), node = c(1L, 1L), value = c(FALSE, TRUE))
  out <- rewrite_quiet(data, list(1L, integer()), snap_waves)
  expect_identical(snap_rows(out, 1L), snap_expect(c(2, 3), c(FALSE, TRUE)))
})

# Untouched data: the rewrite returns its input unless some actor's presence
# path differs, and otherwise edits only the rewritten actors' in-window
# composition rows.

test_that("data with no composition change comes back identical", {
  data <- snap_data()
  expect_identical(rewrite_quiet(data, list(1L, 2L), snap_waves), data)

  data$changes <- NULL
  expect_identical(rewrite_quiet(data, list(1L, 2L), snap_waves), data)
})

test_that("changes of uninvolved actors leave the data identical", {
  data <- snap_data(
    time = c(1, 3),
    node = c(3L, 3L),
    value = c(FALSE, TRUE)
  )
  expect_identical(rewrite_quiet(data, list(1L, 2L), snap_waves), data)
})

test_that("changes outside the wave window leave the data identical", {
  # Actor 1 is involved but joins before w_0 and leaves after w_K, so it is
  # present across the whole window already.
  data <- snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = c(-1, 7),
    node = c(1L, 1L),
    value = c(TRUE, FALSE)
  )
  expect_identical(rewrite_quiet(data, list(1L, 1L), snap_waves), data)
})

test_that("a rewrite keeps every row it has no reason to touch", {
  data <- snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = c(-1, 1, 4, 7, 3),
    node = c(1L, 1L, 1L, 1L, 2L),
    value = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  other <- data.frame(time = 1, node = 1L, var = "age")
  other$value <- list(list(30))
  data$changes <- rbind(data$changes, other)

  out <- rewrite_quiet(data, list(1L, 1L), snap_waves)

  window_active <- data$changes$var == "active" &
    data$changes$node == 1L &
    data$changes$time >= 0 &
    data$changes$time <= 5
  kept <- data$changes[!window_active, ]
  head_rows <- out$changes[seq_len(nrow(kept)), ]
  rownames(kept) <- rownames(head_rows) <- NULL
  expect_identical(head_rows, kept)
  expect_false(identical(out$changes, data$changes))
})

test_that("the rewrite never edits nodes", {
  data <- snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = 1,
    node = 1L,
    value = TRUE
  )
  out <- rewrite_quiet(data, list(1L, integer()), snap_waves)
  expect_false(identical(out$changes, data$changes))
  expect_identical(out$nodes, data$nodes)
})

# The warning: one per fit, listing each (actor, interval) whose presence path
# changed with the real change times now ignored.

snapped_data <- function() {
  # N1 joins at 1 (interval 1); N2 leaves at 3 (interval 2).
  snap_data(
    active = c(FALSE, rep(TRUE, 5L)),
    time = c(1, 3),
    node = 1:2,
    value = c(TRUE, FALSE)
  )
}

test_that("a rewrite of two actors in two intervals warns exactly once", {
  warnings <- list()
  withCallingHandlers(
    rewrite_composition(snapped_data(), list(1L, 2L), snap_waves),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "goldfish_composition_snapped_warning")
})

test_that("the composition warning names each actor, interval and time", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    out <- rewrite_composition(snapped_data(), list(1L, 2L), snap_waves)
  )
})

test_that("a long list of snapped actors is truncated", {
  local_reproducible_output(width = 80)
  n <- 30L
  data <- list(
    nodes = data.frame(label = paste0("N", seq_len(n)), active = FALSE),
    changes = data.frame(
      time = rep(1, n),
      node = seq_len(n),
      var = "active"
    )
  )
  data$changes$value <- lapply(rep(TRUE, n), list)
  expect_snapshot(
    out <- rewrite_composition(data, list(seq_len(n), integer()), c(0, 2, 5))
  )
})

# Wiring into estimate_dynes(). Six actors, wave grid c(0, 2, 5). `friendship`
# is the modeled panel layer with an authored rate; `calls` is a modeled
# relational layer with no rate, so the completion pins one from the calls
# risk set, which reads presence. N6 is absent until it joins at 3 and calls
# N1 at 4, so it is involved in interval 2 and its join snaps to w_1 = 2.
fit_waves <- c(0, 2, 5)

composition_fit_data <- function(observation) {
  changes <- data.frame(time = 3, node = "N6", var = "active")
  changes$value <- list(list(TRUE))
  list(
    info = list(
      name = "cw",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = observation, calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      active = c(rep(TRUE, 5L), FALSE),
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 2L, 3L, 4L),
        to = c(2L, 3L, 4L, 5L),
        time = c(1, 2, 3, 4),
        layer = "friendship"
      ),
      data.frame(
        from = c(1L, 2L, 3L, 6L, 5L),
        to = c(2L, 3L, 4L, 1L, 1L),
        time = c(1, 2, 3, 4, 5),
        layer = "calls"
      )
    ),
    changes = changes
  )
}

composition_fit_spec <- function(data = composition_fit_data("panel")) {
  events <- composition_fit_data("event")
  make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg,
      choice = ~ inertia + recip,
      layer = "friendship",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      choice = ~ inertia + tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = events
    ),
    data = data
  )
}

# A one-iteration fit, its warnings' classes collected rather than raised.
composition_fit <- function(spec = composition_fit_spec(), routine = "random") {
  classes <- character()
  fit <- withCallingHandlers(
    estimate_dynes(
      spec,
      set_algorithm_em(
        n_sequences = 4L,
        max_iterations = 1L,
        seed = 1L,
        augmenter = set_augmenter_options(routine = routine)
      ),
      wave_times = fit_waves
    ),
    warning = function(w) {
      classes <<- c(classes, class(w)[[1L]])
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = classes)
}

relational_pin <- function(data) {
  rs <- relational_window_risk_set(data, layer = "calls")
  pin_intercept_only_rate(rs$count, rs$duration, rs$risk_set_size)
}

test_that("estimate_dynes() fits on the snapped composition", {
  spec <- composition_fit_spec()
  rewritten <- suppressWarnings(rewrite_composition(
    spec$data,
    composition_involvement(spec, fit_waves),
    fit_waves
  ))
  expect_false(identical(rewritten$changes, spec$data$changes))

  run <- composition_fit(spec)

  expect_identical(
    sum(run$warnings == "goldfish_composition_snapped_warning"),
    1L
  )
  fit_spec <- run$fit$spec
  expect_identical(fit_spec$data$changes, rewritten$changes)
  for (part in fit_spec$specifications) {
    expect_identical(part$data$changes, rewritten$changes)
  }
  # The completed calls rate is pinned on the snapped composition, which the
  # original one would not give.
  map <- fit_spec$process_map
  calls_rate <- as.character(map$fid[map$layer == "calls" & map$completed])
  pinned <- fit_spec$completed_rates[[calls_rate]]$intercept
  expect_identical(pinned, relational_pin(rewritten))
  expect_false(isTRUE(all.equal(pinned, relational_pin(spec$data))))
})

test_that("a rewrite that changes nothing raises no warning", {
  data <- snap_data(
    time = c(1, 3),
    node = c(3L, 3L),
    value = c(FALSE, TRUE)
  )
  expect_no_warning(rewrite_composition(data, list(1L, 2L), snap_waves))
  expect_no_warning(rewrite_composition(snap_data(), list(1L, 2L), snap_waves))
})

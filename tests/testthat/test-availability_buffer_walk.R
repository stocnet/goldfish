# Composition changes reach every backend as the availability buffers: an
# initial value, a flat update buffer and a per-row pointer. The default
# engine once filtered a per-change table by time at every event instead;
# these tests pin that the buffer walk is the same presence and that the
# engine no longer carries the table.

# Social Evolution calls, with two actors no event touches leaving and
# re-entering; one change is stamped at a dependent event's own time.
buffer_walk_data <- function() {
  data("Social_Evolution", package = "goldfish", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  dep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  dep <- dep[seq_len(60L), ]
  observed <- as.data.frame(dep)
  idle <- setdiff(actors$label, unique(c(observed$sender, observed$receiver)))
  comp <- data.frame(
    time = c(
      observed$time[10],
      observed$time[20] + 1,
      observed$time[30],
      observed$time[45]
    ),
    node = idle[c(1, 2, 1, 2)],
    replace = c(FALSE, FALSE, TRUE, TRUE)
  )
  labels <- actors$label
  actors <- make_nodes(actors)
  actors <- link_events(actors, comp, attribute = "present")
  data <- suppressWarnings(make_data(dep, call_network, calls, actors, comp))
  list(data = data, comp = comp, node = match(comp$node, labels))
}

test_that("the buffer walk and the time filter agree at every stored row", {
  # The rule both encode: a change at time t is in effect from the first
  # stored row at or after t, so a change stamped at a dependent event's own
  # time already holds for that event.
  fx <- suppressWarnings(buffer_walk_data())
  preps <- suppressWarnings(suppressMessages(list(
    rem = estimate_rem(
      dep ~ 1 + inertia,
      sub_model = "rate",
      data = fx$data,
      preprocessing_only = TRUE
    ),
    choice = estimate_dynam(
      dep ~ inertia,
      sub_model = "choice",
      data = fx$data,
      preprocessing_only = TRUE
    )
  )))
  change_time <- as.numeric(fx$comp$time)
  for (family in names(preps)) {
    prep <- preps[[family]]
    row_time <- as.numeric(prep$event_time)
    expect_true(any(change_time %in% row_time[prep$is_dependent == 1L]))
    # A choice output stores no sender side; its loop never folds one.
    axes <- intersect(
      c("active_sender", "active_dyad"),
      sub("_init$", "", names(prep))
    )
    for (axis in axes) {
      walked <- prep[[paste0(axis, "_init")]] == 1
      filtered <- walked
      update <- prep[[paste0(axis, "_update")]]
      pointer <- prep[[paste0(axis, "_update_pointer")]]
      seen <- 0
      previous <- -Inf
      for (row in seq_along(row_time)) {
        if (pointer[[row]] > seen) {
          cols <- (seen + 1):pointer[[row]]
          walked[update[1L, cols]] <- as.logical(update[2L, cols])
          seen <- pointer[[row]]
        }
        due <- change_time <= row_time[row] & change_time > previous
        filtered[fx$node[due]] <- fx$comp$replace[due]
        previous <- row_time[row]
        expect_identical(walked, filtered, info = paste(family, axis, row))
      }
    }
  }
})

test_that("the default engine carries no per-change composition table", {
  # Measured on Fisheries before the table went: one r-backend iteration
  # allocated 300 MB (choice) and 7.5 MB (rate) with 62 changes and 282 MB
  # and 7.0 MB with 620, so the per-event filter does not separate from the
  # array work. The guard is therefore the table's absence from the context.
  fx <- suppressWarnings(buffer_walk_data())
  contexts <- list()
  original <- bind_compute_step
  local_mocked_bindings(
    bind_compute_step = function(spec) {
      step <- original(spec)
      function(spec, state, i, ctx) {
        if (i == 1L) {
          contexts[[length(contexts) + 1L]] <<- names(ctx)
        }
        step(spec, state, i, ctx)
      }
    }
  )
  control <- set_algorithm_newton(backend = "r", max_iterations = 1)
  suppressWarnings(suppressMessages({
    estimate_rem(
      dep ~ 1 + inertia,
      sub_model = "rate",
      data = fx$data,
      control_algo = control
    )
    estimate_dynam(
      dep ~ inertia,
      sub_model = "choice",
      data = fx$data,
      control_algo = control
    )
  }))
  expect_gt(length(contexts), 0L)
  for (fields in contexts) {
    expect_disjoint(fields, c("compChange1", "compChange2"))
  }
})

test_that("a preprocessed object stores its availability as buffers only", {
  fx <- suppressWarnings(buffer_walk_data())
  retired <- c(
    "active_sender_changes",
    "active_dyad_changes",
    "active_sender_folded",
    "active_dyad_folded"
  )
  preps <- suppressWarnings(suppressMessages(list(
    estimate_rem(
      dep ~ 1 + inertia,
      sub_model = "rate",
      data = fx$data,
      preprocessing_only = TRUE
    ),
    estimate_dynam(
      dep ~ inertia,
      sub_model = "choice",
      data = fx$data,
      preprocessing_only = TRUE
    ),
    constraint_atoms_prep("rate"),
    constraint_atoms_prep("choice")
  )))
  for (prep in preps) {
    expect_disjoint(names(prep), retired)
  }
  # The dyad loop stores one object: a choice keeps no sender presence (its
  # loop never folds the sender), an outer REM keeps it as its own factor.
  sender_fields <- c(
    "active_sender_init",
    "active_sender_update",
    "active_sender_update_pointer"
  )
  expect_disjoint(names(preps[[2]]), sender_fields)
  expect_disjoint(names(preps[[4]]), sender_fields)
  expect_identical(preps[[1]]$active_dyad_encoding, "outer")
  expect_contains(names(preps[[1]]), sender_fields)
  point_rem <- suppressWarnings(suppressMessages(constraint_atoms_prep("rem")))
  expect_identical(point_rem$active_dyad_encoding, "point")
  expect_disjoint(names(point_rem), sender_fields)
})

test_that("a constrained object's support mask is its stream alone", {
  # The raw presences the fold consumed are not stashed beside it: the folded
  # availability is the only presence-bearing object on the output.
  stream <- c(
    "initial",
    "update",
    "update_pointer",
    "n_stored",
    "mask_kind",
    "stored_kind",
    "symmetric"
  )
  for (family in c("rate", "choice", "rem", "coord")) {
    mask <- constraint_atoms_prep(family)$support_mask
    expect_named(mask, stream, info = family)
  }
})

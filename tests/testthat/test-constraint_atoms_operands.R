# Pins and detectors for moving the support-constraint atoms off their private
# walk and onto the main walk as `role = "constraint"` plan effects.
#
# The estimated columns of an unconstrained model are guarded by the frozen
# coefficient baselines; these pins guard the two things the baselines cannot
# see -- that plan construction is untouched when no constraint is present, and
# that a constrained model's folded presence axes and support-mask stream do not
# move as the atoms migrate.

constraint_atoms_refs <- function() {
  readRDS(test_path("fixtures", "constraint_atoms_refs.rds"))
}

# ---- 1.1 unconstrained plan construction is untouched ----------------------

test_that("an unconstrained model's plan effects carry no constraint role", {
  spec <- parity_toy_spec()
  for (family in c("rate", "choice")) {
    effects <- parity_plan(spec, family)$effects
    expect_false(
      "constraint" %in% effects$role,
      info = family
    )
  }
})

test_that("an unconstrained model's plan effects match the frozen capture", {
  refs <- constraint_atoms_refs()
  spec <- parity_toy_spec()
  expect_equal(
    parity_plan(spec, "rate")$effects,
    refs$unconstrained_plan_effects$rate
  )
  expect_equal(
    parity_plan(spec, "choice")$effects,
    refs$unconstrained_plan_effects$choice
  )
})

# ---- 1.2 the constrained references are the byte-identity target -----------

test_that("constrained mask and presence axes match the frozen capture", {
  refs <- constraint_atoms_refs()
  for (family in constraint_atoms_families()) {
    live <- constraint_atoms_capture(constraint_atoms_prep(family))
    expect_equal(live, refs[[family]], info = family)
  }
})

test_that("a choice's point availability is the true grid at every row", {
  # Every sender's row, not only the event sender's: the stored grid is the
  # present receivers each sender's mask allows, row after row.
  for (family in c("choice", "uncovered_choice")) {
    prep <- constraint_atoms_prep(family)
    expect_identical(prep$active_dyad_encoding, "point")
    n1 <- nrow(prep$active_dyad_init)
    mask <- as.logical(prep$support_mask$initial)
    grid <- prep$active_dyad_init
    presence <- prep$support_mask$receiver_presence_init
    mask_seen <- 0
    grid_seen <- 0
    presence_seen <- 0
    for (e in seq_along(prep$event_time)) {
      hi <- prep$support_mask$update_pointer[[e]]
      if (hi > mask_seen) {
        cols <- (mask_seen + 1):hi
        mask[prep$support_mask$update[1L, cols]] <-
          as.logical(prep$support_mask$update[2L, cols])
        mask_seen <- hi
      }
      hi <- prep$active_dyad_update_pointer[[e]]
      if (hi > grid_seen) {
        cols <- (grid_seen + 1):hi
        grid[cbind(
          prep$active_dyad_update[1L, cols],
          prep$active_dyad_update[2L, cols]
        )] <- as.logical(prep$active_dyad_update[3L, cols])
        grid_seen <- hi
      }
      pointer <- prep$support_mask$receiver_presence_update_pointer
      hi <- if (is.null(pointer)) 0 else pointer[[e]]
      if (hi > presence_seen) {
        cols <- (presence_seen + 1):hi
        update <- prep$support_mask$receiver_presence_update
        presence[update[1L, cols]] <- as.logical(update[2L, cols])
        presence_seen <- hi
      }
      truth <- matrix(mask, n1) & rep(presence, each = n1)
      expect_identical(
        as.vector(grid),
        as.vector(truth),
        info = paste(family, e)
      )
    }
  }
})

test_that("joint constrained outputs match the frozen capture per fid", {
  refs <- constraint_atoms_refs()
  for (family in constraint_atoms_joint_families()) {
    live <- constraint_atoms_joint_capture(family)
    expect_identical(names(live), names(refs[[family]]), info = family)
    for (fid in names(live)) {
      expect_equal(
        live[[fid]],
        refs[[family]][[fid]],
        info = paste(family, fid)
      )
    }
  }
})

# ---- atoms join the estimated plan's effect registry ----

test_that("a constrained model's plan effects carry the atoms as operands", {
  fx <- constraint_atoms_social_data()
  spec <- suppressWarnings(make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "dep",
    support_constraint = ~ tie(call_network),
    data = fx$data
  ))
  for (family in c("rate", "choice")) {
    plan <- parity_plan(spec, family)
    atoms <- plan$effects[plan$effects$role == "constraint", , drop = FALSE]
    # The atoms are the sub-plan's atoms, tagged non-estimated, held at the dyad
    # kernel whatever the estimated model's is, and placed above the estimated
    # columns so no estimated indexing reaches them.
    expect_equal(
      atoms$effect_name,
      plan$support_constraint$effects$effect_name,
      info = family
    )
    expect_false(any(atoms$estimate), info = family)
    expect_equal(unique(atoms$stat_kind), "dyad", info = family)
    estimated <- plan$effects[plan$effects$role != "constraint", , drop = FALSE]
    expect_true(all(estimated$gid < min(atoms$gid)), info = family)
  }
})

# ---- the covered atom pool is maintained once, on the shared walk ----

test_that("a covered constrained model builds no private atom walk", {
  # The payoff of moving the atoms onto the merged walk: a constraint whose
  # objects the shared schedule visits is maintained inline by the recorder, so
  # the private `build_atom_maintainer()` walk is not built at all and the atom
  # store is seeded exactly once for the layer. `constraint_atoms_capture()`
  # already pins that the mask this produces is byte-identical to the private
  # walk's frozen output, so this is the walk COUNT that removing it buys.
  stores <- 0L
  orig_store <- build_constraint_atom_store
  local_mocked_bindings(
    build_constraint_atom_store = function(...) {
      stores <<- stores + 1L
      orig_store(...)
    }
  )
  invisible(constraint_atoms_prep("rate"))
  expect_identical(stores, 1L)
})

test_that("no constrained reference family builds a private atom walk", {
  # The uncovered families' references were captured while the private
  # `build_atom_maintainer()` walk still kept their constraint; they now
  # come from the shared walk, which the byte-identity test above checks.
  # The private walk is no longer package code (it is the test oracle), so
  # no preprocessing can reach it; each family's constraint is kept by atom
  # stores the shared walk seeds.
  expect_false(exists(
    "build_atom_maintainer",
    envir = asNamespace("goldfish"),
    inherits = FALSE
  ))
  stores <- 0L
  orig_store <- build_constraint_atom_store
  local_mocked_bindings(
    build_constraint_atom_store = function(...) {
      stores <<- stores + 1L
      orig_store(...)
    }
  )
  for (family in constraint_atoms_families()) {
    stores <- 0L
    invisible(constraint_atoms_prep(family))
    expect_gt(stores, 0L, label = family)
  }
})

test_that("a constraint on an object no term reads is kept by the one walk", {
  # A cost guard: the constraint's layer is stepped by the shared schedule,
  # not by a second, private walk with its own state and event loop.
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = constraint_cost_data(),
    support_constraint = ~ !tie(emails)
  ))
  schedule_layers <- character(0)
  orig_schedule <- build_joint_schedule
  local_mocked_bindings(
    build_joint_schedule = function(...) {
      schedule <- orig_schedule(...)
      schedule_layers <<- unique(c(schedule_layers, schedule$layer))
      schedule
    }
  )
  invisible(suppressWarnings(estimate_dynam(
    spec,
    sub_model = "choice",
    preprocessing_only = TRUE
  )))
  expect_contains(schedule_layers, "emails")
})

test_that("threading a constraint-only object leaves every other oid", {
  refs <- constraint_atoms_refs()$substrate
  covered <- setdiff(
    constraint_atoms_substrate_families(),
    uncovered_constraint_families()
  )
  for (family in covered) {
    live <- constraint_atoms_substrate(family)$registry
    expect_identical(live, refs[[family]]$registry, info = family)
  }
  # The constraint's own layer joins after every object that was there, so
  # no oid a term resolved to moves, and no unit reads it.
  for (family in uncovered_constraint_families()) {
    live <- constraint_atoms_substrate(family)$registry
    before <- refs[[family]]$registry
    kept <- seq_len(nrow(before))
    expect_identical(live[kept, , drop = FALSE], before, info = family)
    expect_identical(live$name[-kept], "emails", info = family)
    expect_identical(live$oid[-kept], nrow(before) + 1L, info = family)
  }
})

test_that("threading a constraint-only stream leaves every other row", {
  refs <- constraint_atoms_refs()$substrate
  covered <- setdiff(
    constraint_atoms_substrate_families(),
    uncovered_constraint_families()
  )
  for (family in covered) {
    live <- constraint_atoms_substrate(family)$schedule
    expect_identical(live, refs[[family]]$schedule, info = family)
  }
  # The constraint's stream is the last one planned, and dropping its rows
  # gives back the schedule the walk had before, row for row.
  for (family in uncovered_constraint_families()) {
    live <- constraint_atoms_substrate(family)$schedule
    before <- refs[[family]]$schedule
    added <- live$layer == "emails"
    expect_gt(sum(added), 0L)
    expect_true(
      all(live$stream_index[added] > max(before$stream_index)),
      info = family
    )
    kept <- lapply(
      live[setdiff(names(live), "n")],
      function(column) column[!added]
    )
    expect_identical(
      kept,
      before[setdiff(names(before), "n")],
      info = family
    )
  }
})

test_that("the atom recorder's log grows linearly with its events", {
  # A cost guard: the recorder appends one entry per constraint event. Twice
  # the events must cost about twice the allocation; a log that copies itself
  # on every append costs four times as much.
  orig_advance <- advance_constraint_atom_store
  allocated <- 0
  local_mocked_bindings(
    advance_constraint_atom_store = function(...) {
      used <- bench::bench_memory(orig_advance(...))
      allocated <<- allocated + as.numeric(used$mem_alloc)
      invisible(NULL)
    }
  )
  recorder_allocation <- function(n_emails) {
    spec <- suppressMessages(make_specification(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = constraint_cost_data(n_emails = n_emails),
      support_constraint = ~ !tie(emails)
    ))
    allocated <<- 0
    invisible(suppressWarnings(estimate_dynam(
      spec,
      sub_model = "choice",
      preprocessing_only = TRUE
    )))
    allocated
  }
  skip_if_not_installed("bench")
  ratio <- recorder_allocation(3000L) / recorder_allocation(1500L)
  expect_lt(ratio, 2.5)
})

test_that("availability per stored row costs the same at any size", {
  # A cost guard: a constrained fid's availability changes only where a mask
  # cell flips or a node enters or leaves, so keeping it up to date costs
  # the same per stored row whatever the number of actors. Recomputing it
  # over every sender at every row costs O(n) a row, and doubling the actors
  # doubles that. The slope over the number of calls isolates the per-row
  # cost from the one-off grids, and the unconstrained twin takes out what
  # the statistics cost per row. The observed-event check is stubbed: a
  # choice's reads its sender's row, and it has its own guard below.
  skip_on_cran()
  skip_if_not_installed("bench")
  local_mocked_bindings(
    availability_check_event = function(...) invisible(NULL)
  )
  allocation <- function(n_actors, n_calls, sub_model, constrained) {
    args <- list(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = constraint_cost_data(
        n_actors = n_actors,
        n_calls = n_calls,
        n_emails = n_calls
      )
    )
    if (constrained) {
      args$support_constraint <- ~ !tie(emails)
    }
    spec <- suppressMessages(do.call(make_specification, args))
    prep <- NULL
    used <- bench::bench_memory(
      prep <- suppressWarnings(estimate_dynam(
        spec,
        sub_model = sub_model,
        preprocessing_only = TRUE
      ))
    )
    c(bytes = as.numeric(used$mem_alloc), rows = length(prep$event_time))
  }
  per_row <- function(n_actors, sub_model) {
    small <- allocation(n_actors, 1500L, sub_model, TRUE)
    large <- allocation(n_actors, 3000L, sub_model, TRUE)
    extra <- (large[["bytes"]] - small[["bytes"]]) -
      (allocation(n_actors, 3000L, sub_model, FALSE)[["bytes"]] -
        allocation(n_actors, 1500L, sub_model, FALSE)[["bytes"]])
    extra / (large[["rows"]] - small[["rows"]])
  }
  for (sub_model in c("rate", "choice")) {
    growth <- per_row(1500L, sub_model) - per_row(750L, sub_model)
    expect_lt(growth, 4096, label = paste(sub_model, "per-row growth"))
  }
})

test_that("the observed-event check reads no mask copy per event", {
  # A cost guard: each dependent event is checked against the availability
  # the walk keeps. A rate reads one gate entry and one count, so its cost
  # per row does not grow with the actors; a choice reads its sender's row
  # (as its likelihood does), so its cost grows with one row, never with
  # the grid. Copying the mask to answer each event costs n1 x n2 a row.
  skip_on_cran()
  skip_if_not_installed("bench")
  allocation <- function(n_actors, n_calls, family, constrained) {
    args <- list(
      layer = "calls",
      model = "DyNAM",
      data = constraint_cost_data(
        n_actors = n_actors,
        n_calls = n_calls,
        n_emails = n_calls
      )
    )
    args[[family]] <- if (family == "rate") ~ 1 + indeg else ~inertia
    if (constrained) {
      args$support_constraint <- ~ !tie(emails)
    }
    spec <- suppressMessages(do.call(make_specification, args))
    prep <- NULL
    used <- bench::bench_memory(
      prep <- suppressWarnings(preprocess_joint(single_process_joint(spec)))
    )
    c(bytes = as.numeric(used$mem_alloc), rows = length(prep[[1L]]$event_time))
  }
  per_row <- function(n_actors, family) {
    small <- allocation(n_actors, 1500L, family, TRUE)
    large <- allocation(n_actors, 3000L, family, TRUE)
    extra <- (large[["bytes"]] - small[["bytes"]]) -
      (allocation(n_actors, 3000L, family, FALSE)[["bytes"]] -
        allocation(n_actors, 1500L, family, FALSE)[["bytes"]])
    extra / (large[["rows"]] - small[["rows"]])
  }
  bound <- c(rate = 1024, choice = 65536)
  for (family in names(bound)) {
    growth <- per_row(1500L, family) - per_row(750L, family)
    expect_lt(growth, bound[[family]], label = paste(family, "per-row growth"))
  }
})

test_that("a constrained walk composes its masks as it goes", {
  # A cost guard: the masks and the folded availability are written during
  # the walk, not realized afterwards by replaying the atoms to each stored
  # row and folding every row again, which allocates per stored row.
  # The availability folds, the post-walk realization and the readers that
  # replayed the mask after the walk are gone: the walk keeps the masks live
  # and availability as an effect. What is left to count is what may still
  # run after the walk.
  retired <- c(
    "fold_active_sender_support",
    "fold_active_dyad_support",
    "fold_active_dyad_support_rem",
    "realize_pending_masks",
    "recorder_atoms_factory",
    "constraint_replay_atoms",
    "mask_cursor",
    "mask_flips",
    "risk_set_exposure"
  )
  for (name in retired) {
    expect_false(exists(name, envir = asNamespace("goldfish")), label = name)
  }
  post_walk <- "build_active_dyad_point"
  calls <- stats::setNames(integer(length(post_walk)), post_walk)
  counting <- lapply(post_walk, function(name) {
    orig <- get(name, envir = asNamespace("goldfish"))
    function(...) {
      calls[[name]] <<- calls[[name]] + 1L
      orig(...)
    }
  })
  names(counting) <- post_walk
  do.call(local_mocked_bindings, counting)
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = constraint_cost_data(n_calls = 300L, n_emails = 300L),
    support_constraint = ~ !tie(emails)
  ))
  for (sub_model in c("rate", "choice")) {
    invisible(suppressWarnings(estimate_dynam(
      spec,
      sub_model = sub_model,
      preprocessing_only = TRUE
    )))
  }
  expect_identical(sum(calls), 0L)
})

# ---- 1.3 the two-layer DAG check survives the migration --------------------

test_that("an availability-derived constraint atom is rejected at parse time", {
  # The mask may not read the risk set it defines. No availability-derived
  # effect ships today, so the guard's registry is injected; the parse path
  # must keep routing through it once atoms are ordinary plan effects.
  is_avail <- function(name) name %in% "n_available"
  expect_error(
    reject_availability_atoms(
      atom_labels = c("tie(net)", "n_available(x)"),
      atom_names = c("tie", "n_available"),
      is_availability = is_avail
    ),
    "may not depend on the risk set"
  )
})

test_that("parse_and_validate_constraint routes through the DAG guard", {
  # Pins WHERE the check lives: in the parser, before the atoms are compiled
  # into the plan. A sentinel raised from the guard must surface, proving the
  # guard is on the parse path and not bypassed.
  called <- FALSE
  local_mocked_bindings(
    reject_availability_atoms = function(...) {
      called <<- TRUE
      rlang::abort("guard reached", class = "constraint_atoms_guard")
    }
  )
  expect_error(
    parse_and_validate_constraint(~ tie(net), has_dyad_part = TRUE),
    class = "constraint_atoms_guard"
  )
  expect_true(called)
})

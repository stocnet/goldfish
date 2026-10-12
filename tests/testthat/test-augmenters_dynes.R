# The walk substrate the augmenters drive, characterized on a realistic DyNES
# fixture before any augmenter is built on it.

# A mutually exclusive flavored friendship panel (waves 5 and 10, observation
# start 0) modeled beside an observed calls stream, six actors. The calls
# choice reads friendship and carries a windowed inertia over calls; calls
# carries a user support constraint (no call along a friendship tie); N6 is
# absent until it joins at t = 2. Call stamps sit off the join time and off
# each other's window expiries (window 2: the call at 1 expires at 3).
substrate_data <- function() {
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
    time = c(1, 3.5, 6, 8),
    weight = 1,
    layer = "calls"
  )
  changes <- data.frame(time = 2, node = 6L, var = "active")
  changes$value <- list(list(TRUE))
  data <- list(
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
      active = c(rep(TRUE, 5L), FALSE),
      stringsAsFactors = FALSE
    ),
    ties = rbind(friendship, calls),
    changes = changes
  )
  add_flavor(
    data,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# The specification completed as estimate_dynes() completes it, and the
# augmenter context over its recipe. fid 5 is the calls rate, fid 6 the calls
# choice.
substrate_context <- function() {
  data <- substrate_data()
  spec <- make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia(calls, window = 2) + tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data,
      support_constraint = ~ !tie(friendship)
    ),
    data = data
  )
  waves <- panel_wave_grid(spec, start_time = 0)
  # The friendship rates are completed as pinned plateaus, which warns.
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  new_augmenter_context(
    completed,
    compile_augmentation_recipe(completed, waves)
  )
}

# The calls constraint reads emails, a network no formula term reads, so only
# the live support can see it change. An email 2 -> 4 lands at t = 3.
unread_constraint_context <- function() {
  data <- substrate_data()
  data$info$update <- c(data$info$update, emails = "increment")
  data$info$directed <- c(data$info$directed, emails = TRUE)
  data$info$observation <- c(data$info$observation, emails = "event")
  emails <- data.frame(
    from = c(2L, 4L),
    to = c(4L, 6L),
    time = c(3, 7),
    weight = 1,
    layer = "emails"
  )
  for (column in setdiff(names(data$ties), names(emails))) {
    emails[[column]] <- NA
  }
  data$ties <- rbind(data$ties, emails[names(data$ties)])
  spec <- make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia(calls, window = 2),
      layer = "calls",
      model = "DyNAM",
      data = data,
      support_constraint = ~ !tie(emails)
    ),
    data = data
  )
  waves <- panel_wave_grid(spec, start_time = 0)
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  new_augmenter_context(
    completed,
    compile_augmentation_recipe(completed, waves)
  )
}

test_that("a constraint on a network no term reads follows it mid-interval", {
  handle <- open_augmenter_walk(unread_constraint_context())
  supported <- function(sender, receiver) {
    walk_event_supported(handle, "calls", NA_character_, sender, receiver)
  }

  walk_advance(handle, 2.5)
  expect_true(supported(2L, 4L))
  expect_true(supported(1L, 3L))

  walk_advance(handle, 3.5)
  expect_false(supported(2L, 4L))
  expect_true(supported(1L, 3L))
})

test_that("walk_evaluate() masks the choice by the user support constraint", {
  context <- substrate_context()
  handle <- open_augmenter_walk(context)
  walk_advance(handle, 1.5)
  theta <- c(0.3, 0.5)
  choice <- matrix(walk_evaluate(handle, 6L, theta)$value, 6L, byrow = TRUE)

  # Friendship holds 1 -> 2 and 2 -> 3 before the first wave: those receivers
  # leave their sender's risk set, beside the sender itself and absent N6.
  friendship <- handle$state$networks[["friendship"]]
  expect_equal(sum(friendship), 2)
  excluded <- friendship == 1
  diag(excluded) <- TRUE
  excluded[, 6L] <- TRUE
  active <- 1:5
  expect_true(all(choice[active, ][excluded[active, ]] == 0))
  expect_true(all(choice[active, ][!excluded[active, ]] > 0))
  expect_equal(choice[1L, 2L], 0)
  expect_equal(choice[2L, 3L], 0)
})

test_that("walk_evaluate(sender =) agrees row for row with the full matrix", {
  context <- substrate_context()
  handle <- open_augmenter_walk(context)
  walk_advance(handle, 1.5)
  theta <- c(0.3, 0.5)
  choice <- matrix(walk_evaluate(handle, 6L, theta)$value, 6L, byrow = TRUE)
  for (sender in 1:6) {
    expect_equal(
      walk_evaluate(handle, 6L, theta, sender = sender)$value,
      choice[sender, ]
    )
  }
})

test_that("walk_next_breakpoint() reports the join, then the window expiry", {
  context <- substrate_context()
  handle <- open_augmenter_walk(context)
  walk_advance(handle, 1.5)
  expect_equal(walk_next_breakpoint(handle), 2)
  expect_equal(walk_evaluate(handle, 5L, c(0.1, 0.2))$value[[6L]], 0)

  # Past the join N6 has a rate; the next change is the call at 1 leaving the
  # inertia window, before the next call at 3.5.
  walk_advance(handle, 2.5)
  expect_gt(walk_evaluate(handle, 5L, c(0.1, 0.2))$value[[6L]], 0)
  expect_equal(walk_next_breakpoint(handle), 3)
})

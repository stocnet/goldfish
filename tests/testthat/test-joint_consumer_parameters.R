# The shared consumer acceptance surface both joint consumers import:
# `joint_initial_parameters()` (the free-theta projection `estimate_dynes()`
# estimates), `joint_simulation_parameters()` (the full per-fid projection
# `simulate()` walks, gated on completeness), and `reconcile_joint_parameters()`
# (the design-D16 check that every completed-spec fid absent from the object is a
# trivially resolved autocompleted default). v1 accepts ONLY a
# `goldfishParams`.

# A two-process DyNAM join (calls, emails); the calls choice carries an
# inline-coef offset, so its `tie/friendship [Fx]` slot is the one fixed coefficient
# and every other slot is free.
consumer_join <- function() {
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
  data <- list(info = info, nodes = nodes, ties = ties)
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + offset(tie(friendship), coef = -0.5),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

# Every free slot pinned -- a complete object a `simulate()` can drive.
complete_parameters <- function(join = consumer_join()) {
  set_init_param(
    join,
    `calls › rate` = c(0.1, 0.2),
    `calls › choice` = c(0.3, NA),
    `emails › rate` = c(0.4, 0.5),
    `emails › choice` = c(0.6)
  )
}

test_that("the consumer surface accepts only a goldfishParams", {
  expect_error(
    joint_initial_parameters(list(0.1, 0.2), arg = "initial_parameters"),
    "must be a"
  )
  expect_error(
    joint_simulation_parameters(c(0.1, 0.2), arg = "coef"),
    "must be a"
  )
})

test_that("estimate reads the free-theta projection of a partial object", {
  p <- set_init_param(consumer_join()) # nothing pinned
  theta <- joint_initial_parameters(p)

  # The free vector is returned verbatim -- partial (NA) slots and all, the
  # normal warm-start case for estimation.
  expect_identical(theta, p$free)
  expect_true(anyNA(theta))
  # Offsets are not in the free projection: only the six non-fixed slots.
  expect_length(theta, 6L)
})

test_that("simulate reads the full projection of a complete object", {
  full <- joint_simulation_parameters(complete_parameters())

  expect_identical(full, complete_parameters()$full)
  # The offset slot carries the specification's value, not a free NA.
  expect_identical(unname(full[["calls › choice"]]["tie/friendship [Fx]"]), -0.5)
})

test_that("simulate's value gate rejects an unpinned free slot", {
  p <- set_init_param(consumer_join(), `calls › rate` = c(0.1, 0.2))
  expect_error(
    joint_simulation_parameters(p, arg = "coef"),
    "unpinned"
  )
})

test_that("reconciliation treats an absent autocompleted fid as resolved", {
  # A rate-only calls process: completion synthesizes a zero-free-parameter
  # uniform choice absent from the parameter object.
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
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
    update = c(calls = "increment", emails = "increment"),
    directed = c(calls = TRUE, emails = TRUE),
    observation = c(calls = "event", emails = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  join <- make_joint_specification(calls_spec, emails_spec, data = data)
  p <- set_init_param(
    join,
    `calls › rate` = c(0.1, 0.2),
    `emails › rate` = c(0.4, 0.5),
    `emails › choice` = c(0.6)
  )
  cc <- suppressWarnings(complete_generative_spec(
    join,
    consumer = "estimate_dynes"
  ))

  # The completed spec carries the autocompleted calls-choice fid the object
  # never keyed; reconciliation passes because that fid is autocompleted and so
  # trivially resolved -- it needs no user value.
  expect_silent(reconcile_joint_parameters(p, cc, arg = "coef"))
})

test_that("simulate's value-gate abort names the unpinned free effect(s)", {
  local_cli_context <- function(env = parent.frame()) {
    withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
  }
  local_cli_context()
  p <- set_init_param(consumer_join(), `calls › rate` = c(0.1, 0.2))
  expect_snapshot(joint_simulation_parameters(p, arg = "coef"), error = TRUE)
})

test_that("reconciliation flags a process the specification lacks", {
  p <- complete_parameters()
  # A completed spec whose process_map omits a process the object still carries
  # is the guard's target: drop the emails-choice row so its label strays.
  cc <- suppressWarnings(complete_generative_spec(
    consumer_join(),
    consumer = "estimate_dynes"
  ))
  labels <- render_process_label(cc$process_map, cc$process_map$fid)
  cc$process_map <- cc$process_map[labels != "emails › choice", ]
  expect_error(
    reconcile_joint_parameters(p, cc, arg = "coef"),
    "different specification"
  )
})

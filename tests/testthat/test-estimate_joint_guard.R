# The event-stream estimators guard their entry against inputs they cannot
# estimate: a joint (multivariate) specification -- which belongs to
# estimate_dynes() -- and a single specification whose focal layer is
# panel-observed (the PE-dependent case, which belongs to estimate_dynes()).

# cli error snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A one-node-set data object with a panel-observed exogenous layer (friendship)
# read as a covariate by an event process (calls), plus a second event process
# (emails). No process is focal on the panel layer, so this composes into a
# valid joint specification with no modeled panel process.
joint_fixture_data <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L),
      to = c(2L, 3L, 4L),
      time = c(1, 1, 1),
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
      friendship = "replace",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = "panel", calls = "event", emails = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

make_joint_fixture <- function() {
  data <- joint_fixture_data()
  calls_spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

test_that("estimate_dynam rejects a joint specification, pointing to dynes", {
  local_cli_context()
  js <- make_joint_fixture()
  expect_s3_class(js, "goldfishJointSpec")
  # The joint class does not inherit goldfishSpec, so the rejection is
  # the class guard firing before the single-specification dispatch branch --
  # never a fall-through into estimate_wrapper.
  expect_snapshot(estimate_dynam(js), error = TRUE)
})

test_that("estimate_rem rejects a joint specification, pointing to dynes", {
  local_cli_context()
  js <- make_joint_fixture()
  expect_snapshot(estimate_rem(js), error = TRUE)
})

# A panel-observed focal layer is a valid *specification* -- it is how
# estimate_dynes() receives a modeled panel process -- so the refusal is scoped
# per estimator: the event-stream estimators abort at their own entry, pointing
# to estimate_dynes(), on the specification and the formula path alike.

panel_focal_fixture <- function() {
  x <- make_stocnet_fixture()
  x$info$observation <- c(calls = "panel")
  x
}

test_that("make_specification accepts a panel-focal layer", {
  spec <- make_specification(
    rate = ~1,
    layer = "calls",
    model = "DyNAM",
    data = panel_focal_fixture()
  )
  expect_s3_class(spec, "goldfishSpec")
  expect_identical(spec$focal, "calls")
})

test_that("estimate_dynam rejects a panel-focal single specification", {
  local_cli_context()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = panel_focal_fixture()
  )
  expect_snapshot(estimate_dynam(spec, sub_model = "rate"), error = TRUE)
  expect_error(
    estimate_dynam(spec, sub_model = "rate"),
    class = "goldfish_panel_focal"
  )
})

test_that("estimate_rem rejects a panel-focal single specification", {
  local_cli_context()
  spec <- make_specification(
    rate = ~inertia,
    layer = "calls",
    model = "REM",
    data = panel_focal_fixture()
  )
  expect_snapshot(estimate_rem(spec), error = TRUE)
  expect_error(estimate_rem(spec), class = "goldfish_panel_focal")
})

test_that("event-stream estimators reject a panel focal on the formula path", {
  local_cli_context()
  x <- panel_focal_fixture()
  expect_snapshot(
    estimate_dynam(calls ~ inertia, sub_model = "rate", data = x),
    error = TRUE
  )
  expect_error(
    estimate_dynam(calls ~ inertia, data = x),
    class = "goldfish_panel_focal"
  )
  expect_error(
    estimate_rem(calls ~ inertia, data = x),
    class = "goldfish_panel_focal"
  )
  expect_error(
    estimate_dynami(calls ~ inertia, data = x),
    class = "goldfish_panel_focal"
  )
})

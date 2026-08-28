# The event-stream estimators guard their entry against inputs they cannot
# estimate: a joint (multivariate) specification -- which belongs to
# estimate_dynes() -- and a single specification whose focal layer is
# panel-observed (the PE-dependent case, caught by check_dependent_panel).

# cli error snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A one-node-set data object with a panel-observed exogenous layer (friendship)
# read as a covariate by an event process (calls), plus a second event process
# (emails). No process is focal on the panel layer, so this composes into a
# valid joint specification without needing the modeled-panel focal that
# make_specification() still rejects at this stage.
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
  # The joint class does not inherit specification.goldfish, so the rejection is
  # the class guard firing before the single-specification dispatch branch --
  # never a fall-through into estimate_wrapper.
  expect_snapshot(estimate_dynam(js), error = TRUE)
})

test_that("estimate_rem rejects a joint specification, pointing to dynes", {
  local_cli_context()
  js <- make_joint_fixture()
  expect_snapshot(estimate_rem(js), error = TRUE)
})

# A single specification whose focal/dependent layer is panel-observed cannot be
# estimated by the event-stream estimators. The guard is check_dependent_panel,
# invoked from validate_goldfish_data, so it fires the moment make_specification
# builds the process the estimator would receive -- the single-specification
# path is protected before any estimator sees it.

test_that("estimate_dynam rejects a panel-focal single specification", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$observation <- c(calls = "panel")
  expect_snapshot(
    estimate_dynam(make_specification(
      rate = ~1,
      layer = "calls",
      model = "DyNAM",
      data = x
    )),
    error = TRUE
  )
})

test_that("estimate_rem rejects a panel-focal single specification", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$observation <- c(calls = "panel")
  expect_snapshot(
    estimate_rem(make_specification(
      rate = ~1,
      layer = "calls",
      model = "REM",
      data = x
    )),
    error = TRUE
  )
})

test_that("the panel-focal guard fires on the shared preprocessing path", {
  # estimate_dynami() takes no specification.goldfish object (DyNAM-i is not a
  # make_specification() model), so its PE-focal protection is the same
  # check_dependent_panel guard reached through validate_goldfish_data during
  # preprocessing. Confirm the guard fires there, covering the estimator whose
  # entry cannot be exercised with a specification object.
  local_cli_context()
  x <- make_stocnet_fixture()
  x$info$observation <- c(calls = "panel")
  expect_snapshot(validate_goldfish_data(x), error = TRUE)
})

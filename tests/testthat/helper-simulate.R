# Fixtures shared by the simulation tests: a two-process joint specification on
# a small node set, and the parameter shapes a run takes. They live here rather
# than in one test file because the driver tests and the plug-point tests both
# open the same walk.

sim_fixture_data <- function() {
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

# Two competing DyNAM processes (calls, emails) on one node set, each with a
# rate and a choice carrying effects, plus a panel layer both choices read.
sim_two_process <- function() {
  data <- sim_fixture_data()
  calls <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls, emails, data = data)
}

sim_two_process_parameters <- function(js) {
  set_parameters(
    js,
    `calls › rate` = c(-1, 0.1),
    `calls › choice` = c(0.2, 0.3),
    `emails › rate` = c(-1, 0.1),
    `emails › choice` = c(0.2, 0.3)
  )
}

# The per-fid vectors the parameters above hold, keyed by fid, which is what a
# provider returns.
sim_two_process_theta <- function() {
  list(
    `1` = c(-1, 0.1),
    `2` = c(0.2, 0.3),
    `3` = c(-1, 0.1),
    `4` = c(0.2, 0.3)
  )
}

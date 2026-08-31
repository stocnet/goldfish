# set_init_param() builds a self-validating goldfishParams over a joint
# specification: it resolves each `...` key by membership against the rendered
# process label (never by parsing the label back into components), validates
# each per-fid vector against the process's coefficient layout, and classifies
# every slot free/fixed from the offset mask projected into coefficient space --
# an `offset()` term keeps the specification's value, a value supplied there is
# warned about and dropped.

# cli error/warning snapshots are pinned to a reproducible width/no-color
# context so the rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A two-process DyNAM join (calls, emails), each with a rate (intercept + indeg)
# and a choice sub-model. The calls choice carries an inline-coef offset, so its
# `tie(friendship)` slot is the fixed coefficient the classifier must resolve
# from the specification; every other slot is free. Non-flavored, so the
# rendered labels elide the flavor segment (`calls > rate`, not
# `calls > NA > rate`).
parameters_join <- function() {
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

test_that("set_init_param() returns a goldfishParams, not the retired parameters.goldfish", {
  p <- set_init_param(parameters_join())
  expect_s3_class(p, "goldfishParams")
  expect_false(inherits(p, "parameters.goldfish"))
})

test_that("labels elide the flavor segment for a non-flavored process", {
  p <- set_init_param(parameters_join())
  expect_identical(
    names(p$full),
    c("calls › rate", "calls › choice", "emails › rate", "emails › choice")
  )
})

test_that("an omitted process key leaves every non-fixed slot free", {
  p <- set_init_param(parameters_join())

  # Nothing supplied: every free slot is NA and the object is incomplete, but
  # the offset slot already carries the specification's fixed value.
  expect_false(p$complete)
  expect_true(anyNA(p$free))
  expect_identical(
    names(p$free),
    c(
      "calls › rate: Intercept",
      "calls › rate: indeg(calls)",
      "calls › choice: inertia(calls)",
      "emails › rate: Intercept",
      "emails › rate: indeg(emails)",
      "emails › choice: inertia(emails)"
    )
  )

  # The calls-choice offset is the one fixed slot, held at the formula's value.
  choice <- p$fids[["2"]]
  expect_identical(unname(choice$fixed), c(FALSE, TRUE))
  expect_identical(choice$values[choice$fixed], -0.5)
})

test_that("a per-fid vector resolves by rendered-label membership", {
  p <- set_init_param(
    parameters_join(),
    `calls › rate` = c(0.1, 0.2),
    `emails › choice` = c(`inertia(emails)` = 0.7)
  )

  expect_identical(
    p$full[["calls › rate"]],
    c(Intercept = 0.1, `indeg(calls)` = 0.2)
  )
  expect_identical(unname(p$full[["emails › choice"]]["inertia(emails)"]), 0.7)
  # A process whose key is omitted stays all-free.
  expect_true(anyNA(p$full[["emails › rate"]]))
})

test_that("named and positional per-fid vectors agree", {
  join <- parameters_join()
  positional <- set_init_param(join, `calls › rate` = c(0.1, 0.2))
  # Fully named, supplied out of coefficient order -- the names fix the slots.
  named <- set_init_param(
    join,
    `calls › rate` = c(`indeg(calls)` = 0.2, Intercept = 0.1)
  )
  expect_identical(
    named$full[["calls › rate"]],
    positional$full[["calls › rate"]]
  )
})

test_that("pinning every free slot marks the object complete", {
  p <- set_init_param(
    parameters_join(),
    `calls › rate` = c(0.1, 0.2),
    `calls › choice` = c(0.3, NA), # NA at offset slot: silent, spec prevails
    `emails › rate` = c(0.4, 0.5),
    `emails › choice` = c(0.6)
  )
  expect_true(p$complete)
  expect_false(anyNA(p$free))
})

test_that("a value supplied for a fixed coefficient warns and is ignored", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    p <- set_init_param(
      join,
      `calls › choice` = c(`inertia(calls)` = 0.3, `tie(friendship)` = 9)
    )
  )
  # Offset prevails: the supplied 9 is dropped, the spec's -0.5 stands.
  expect_identical(unname(p$full[["calls › choice"]]["tie(friendship)"]), -0.5)
})

test_that("a key matching no process aborts naming the valid labels", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls:rate` = c(0.1, 0.2)),
    error = TRUE
  )
})

test_that("the same process given twice aborts", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(
      join,
      `calls › rate` = c(0.1, 0.2),
      `calls › rate` = c(0.3, 0.4)
    ),
    error = TRUE
  )
})

test_that("a wrong-length per-fid vector aborts naming the fid", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls › rate` = c(0.1, 0.2, 0.3)),
    error = TRUE
  )
})

test_that("a partly named per-fid vector is rejected", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls › rate` = c(Intercept = 0.1, 0.2)),
    error = TRUE
  )
})

test_that("a per-fid vector naming an unknown coefficient aborts", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls › rate` = c(Intercept = 0.1, wrong = 0.2)),
    error = TRUE
  )
})

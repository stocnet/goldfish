local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A small stocnet with two panel layers (friendship under `replace`, calls
# under `increment`), a second panel layer used as a covariate (advice), and an
# event layer (messages). Three survey waves at times 10, 20, 30.
make_prepare_fixture <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 1L, 1L, 3L, 1L, 2L, 4L),
    to = c(2L, 3L, 2L, 3L, 4L, 2L, 3L, 1L),
    time = c(10, 10, 20, 20, 20, 10, 20, 15),
    weight = c(1, 1, 1, 1, 1, 2, 3, 1),
    layer = c(
      rep("friendship", 5L),
      "calls",
      "calls",
      "messages"
    ),
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy",
    focal = "friendship",
    update = c(
      friendship = "replace",
      calls = "increment",
      advice = "replace",
      messages = "increment"
    ),
    directed = c(
      friendship = TRUE,
      calls = TRUE,
      advice = TRUE,
      messages = TRUE
    ),
    observation = c(
      friendship = "panel",
      calls = "panel",
      advice = "panel",
      messages = "event"
    )
  )
  list(info = info, nodes = nodes, ties = ties)
}

test_that("a layer without a form entry is read as states", {
  x <- make_prepare_fixture()
  out <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(friendship = "mutually_exclusive")
  ))
  expect_equal(
    unname(out$info$dynes_prepared["friendship"]),
    "mutually_exclusive"
  )
})

test_that("a none layer given as changes is returned unchanged", {
  x <- make_prepare_fixture()
  out <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(advice = "none"),
    form = c(advice = "changes")
  ))
  expect_equal(out$ties, x$ties)
})

test_that("a count layer given as changes is returned unchanged", {
  x <- make_prepare_fixture()
  out <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(calls = "count"),
    form = c(calls = "changes")
  ))
  expect_equal(out$ties, x$ties)
})

test_that("layers argument errors are reported before any row is rewritten", {
  local_cli_context()
  x <- make_prepare_fixture()
  expect_snapshot(error = TRUE, prepare_dynes_data(x, layers = character()))
  expect_snapshot(error = TRUE, prepare_dynes_data(x, layers = "friendship"))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(friendship = "count", friendship = "none"))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(friendship = "ordinal"))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(missing = "count"))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(messages = "count"))
  )
})

test_that("form argument errors are reported before any row is rewritten", {
  local_cli_context()
  x <- make_prepare_fixture()
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(friendship = "count"),
      form = c(friendship = "sideways")
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(friendship = "count"),
      form = c(calls = "per_wave")
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(friendship = "count"),
      form = c(friendship = "states", friendship = "per_wave")
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(friendship = "mutually_exclusive"),
      form = c(friendship = "per_wave")
    )
  )
})

test_that("flavor_names errors are reported before any row is rewritten", {
  local_cli_context()
  x <- make_prepare_fixture()
  me <- c(friendship = "mutually_exclusive")
  roles <- c(creation = "formed", dissolution = "ended")
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = me, flavor_names = list(calls = roles))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(friendship = "mutually_exclusive", advice = "none"),
      flavor_names = list(advice = roles)
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = me,
      flavor_names = list(friendship = c(creation = "formed", other = "ended"))
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = me,
      flavor_names = list(
        friendship = c(creation = "not valid", dissolution = "ended")
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = me,
      flavor_names = list(
        friendship = c(creation = "same", dissolution = "same")
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = me,
      flavor_names = list(friendship = roles, friendship = roles)
    )
  )
})

test_that("a flavor for a count layer aborts with the count guidance", {
  local_cli_context()
  x <- make_prepare_fixture()
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(calls = "count"),
      flavor_names = list(
        calls = c(creation = "signed", dissolution = "ended")
      )
    )
  )
})

test_that("an invalid call leaves the input object untouched", {
  x <- make_prepare_fixture()
  before <- x
  try(
    prepare_dynes_data(x, layers = c(friendship = "ordinal")),
    silent = TRUE
  )
  expect_identical(x, before)
})

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
    from = c(1L, 2L, 1L, 1L, 3L, 1L, 2L, 4L, 2L, 3L),
    to = c(2L, 3L, 2L, 3L, 4L, 2L, 3L, 1L, 4L, 1L),
    time = c(10, 10, 20, 20, 20, 10, 20, 15, 10, 20),
    weight = c(1, 1, 1, 1, 1, 2, 3, 1, 1, 1),
    layer = c(
      rep("friendship", 5L),
      "calls",
      "calls",
      "messages",
      "advice",
      "advice"
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
  spec <- resolve_prepare_layers(
    x,
    layers = c(friendship = "mutually_exclusive", calls = "count"),
    form = c(calls = "per_wave"),
    flavor_names = NULL,
    call = rlang::current_env()
  )
  expect_equal(spec$layer, c("friendship", "calls"))
  expect_equal(spec$form, c("states", "per_wave"))
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

# One panel layer from a data frame of (from, to, time, weight) rows.
make_wave_fixture <- function(
  rows,
  directed = TRUE,
  update = "replace",
  two_mode = FALSE
) {
  rows$layer <- "survey"
  nodes <- data.frame(
    label = c("A", "B", "C", "D", "E", "F", "G", "H"),
    mode = if (two_mode) rep(c("x", "y"), each = 4L) else "p",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy",
    focal = "survey",
    update = c(survey = update),
    directed = c(survey = directed),
    observation = c(survey = "panel")
  )
  if (two_mode) {
    info$sender <- c(survey = "x")
    info$receiver <- c(survey = "y")
  }
  list(info = info, nodes = nodes, ties = rows)
}

wave_rows <- function(from, to, time, weight = 1) {
  data.frame(from = from, to = to, time = time, weight = weight)
}

# The shared checks, called the way prepare_dynes_data() calls them.
check_rows <- function(x, form = "states") {
  clean_wave_rows(x, "survey", form, call = rlang::current_env())
}

test_that("a dyad repeated at one wave aborts", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(c(3, 3), c(7, 7), c(10, 10), c(1, 1)))
  expect_snapshot(error = TRUE, check_rows(x))
  expect_snapshot(error = TRUE, check_rows(x, "per_wave"))
})

test_that("a zero row is read as absence on both forms", {
  x <- make_wave_fixture(
    wave_rows(c(3, 4), c(7, 8), c(10, 10), c(1, 0))
  )
  for (form in c("states", "per_wave")) {
    out <- check_rows(x, form)
    expect_equal(out$from, 3)
    expect_equal(out$to, 7)
  }
})

test_that("an NA tie value aborts", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(3, 7, 10, NA_real_))
  expect_snapshot(error = TRUE, check_rows(x))
  expect_snapshot(error = TRUE, check_rows(x, "per_wave"))
})

test_that("a self-tie on a one-mode layer aborts", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(4, 4, 10))
  expect_snapshot(error = TRUE, check_rows(x))
})

test_that("a two-mode layer has no diagonal and is not reordered", {
  x <- make_wave_fixture(
    wave_rows(c(4, 2), c(4, 1), c(10, 10)),
    directed = FALSE,
    two_mode = TRUE
  )
  out <- check_rows(x)
  expect_equal(out$from, c(4, 2))
  expect_equal(out$to, c(4, 1))
})

test_that("an undirected tie reported both ways warns and counts once", {
  local_cli_context()
  x <- make_wave_fixture(
    wave_rows(c(3, 7), c(7, 3), c(10, 10), c(1, 1)),
    directed = FALSE
  )
  expect_snapshot(out <- check_rows(x))
  expect_equal(nrow(out), 1L)
  expect_equal(c(out$from, out$to), c(3, 7))
})

test_that("an undirected count reported both ways is read as one report", {
  x <- make_wave_fixture(
    wave_rows(c(3, 7), c(7, 3), c(10, 10), c(2, 2)),
    directed = FALSE,
    update = "increment"
  )
  out <- suppressWarnings(check_rows(x, "per_wave"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$value, 2)
})

test_that("an undirected tie reported both ways with two values aborts", {
  local_cli_context()
  x <- make_wave_fixture(
    wave_rows(c(3, 7), c(7, 3), c(10, 10), c(1, 2)),
    directed = FALSE
  )
  expect_snapshot(error = TRUE, check_rows(x))
})

test_that("an undirected tie that switches orientation keeps one key", {
  x <- make_wave_fixture(
    wave_rows(c(3, 7), c(7, 3), c(10, 20)),
    directed = FALSE
  )
  out <- check_rows(x)
  expect_equal(out$from, c(3, 3))
  expect_equal(out$to, c(7, 7))
})

test_that("a directed layer keeps (i, j) and (j, i) as different dyads", {
  x <- make_wave_fixture(wave_rows(c(3, 7), c(7, 3), c(10, 10)))
  expect_equal(nrow(check_rows(x)), 2L)
})

test_that("repeated time = NA rows are not checked row by row", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 4), c(7, 7, 8), c(NA, NA, 10), c(1, 1, 1))
  )
  out <- check_rows(x)
  expect_equal(nrow(out), 1L)
  expect_equal(out$from, 4)
})

# The row and starting-state checks of one layer, called the way
# prepare_dynes_data() calls them.
check_layer <- function(x, style, form = "states") {
  check_prepare_layer(x, "survey", style, form, call = rlang::current_env())
}

test_that("a mutually exclusive state other than 0 or 1 aborts", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(c(3, 4), c(7, 8), c(10, 10), c(2, 1)))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(survey = "mutually_exclusive"))
  )
})

test_that("a mutually exclusive change outside the update encoding aborts", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(3, 7, 10, 2))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(survey = "mutually_exclusive"),
      form = c(survey = "changes")
    )
  )
})

test_that("a mutually exclusive change-list contradicting the style aborts", {
  local_cli_context()
  twice_created <- make_wave_fixture(
    wave_rows(c(3, 3), c(7, 7), c(10, 20), c(1, 1)),
    update = "increment"
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      twice_created,
      layers = c(survey = "mutually_exclusive"),
      form = c(survey = "changes")
    )
  )
  dissolved_absent <- make_wave_fixture(wave_rows(3, 7, 10, 0))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      dissolved_absent,
      layers = c(survey = "mutually_exclusive"),
      form = c(survey = "changes")
    )
  )
})

test_that("a fractional or negative count aborts", {
  local_cli_context()
  for (value in c(1.5, -1)) {
    x <- make_wave_fixture(wave_rows(3, 7, 10, value), update = "increment")
    expect_snapshot(
      error = TRUE,
      prepare_dynes_data(
        x,
        layers = c(survey = "count"),
        form = c(survey = "per_wave")
      )
    )
  }
})

test_that("a count change-list is returned unchanged when it never falls", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 3, 4), c(7, 7, 7, 8), c(NA, 10, 20, 20), c(2, 1, 3, 1)),
    update = "increment"
  )
  out <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(survey = "count"),
    form = c(survey = "changes")
  ))
  expect_equal(out$ties, x$ties)
})

test_that("a count change-list that falls aborts in its update semantics", {
  local_cli_context()
  negative <- make_wave_fixture(
    wave_rows(c(3, 3), c(7, 7), c(10, 20), c(2, -2)),
    update = "increment"
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      negative,
      layers = c(survey = "count"),
      form = c(survey = "changes")
    )
  )
  falling <- make_wave_fixture(wave_rows(c(3, 3), c(7, 7), c(10, 20), c(6, 4)))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      falling,
      layers = c(survey = "count"),
      form = c(survey = "changes")
    )
  )
  below_start <- make_wave_fixture(
    wave_rows(c(3, 3), c(7, 7), c(NA, 10), c(6, 4))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      below_start,
      layers = c(survey = "count"),
      form = c(survey = "changes")
    )
  )
})

test_that("a count total that falls aborts, naming the wave and the dyad", {
  local_cli_context()
  falling <- make_wave_fixture(
    wave_rows(c(3, 3), c(7, 7), c(10, 20), c(6, 4)),
    update = "increment"
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(falling, layers = c(survey = "count"))
  )
  dropped <- make_wave_fixture(
    wave_rows(c(3, 4, 4), c(7, 8, 8), c(10, 10, 20), c(6, 1, 1))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(dropped, layers = c(survey = "count"))
  )
})

test_that("rising count totals pass the checks", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 4, 4), c(7, 7, 8, 8), c(10, 20, 10, 20), c(6, 9, 1, 1)),
    update = "increment"
  )
  expect_no_error(check_layer(x, "count"))
})

test_that("a none layer takes any value that is not NA", {
  x <- make_wave_fixture(
    wave_rows(c(3, 4, 3), c(7, 8, 7), c(10, 10, 20), c(-2.5, 0.3, 7))
  )
  expect_no_error(check_layer(x, "none"))
  expect_no_error(check_layer(x, "none", "per_wave"))
})

test_that("a mutually exclusive starting level other than 0 or 1 aborts", {
  local_cli_context()
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 3), c(7, 7, 7), c(NA, NA, 10), c(1, 1, 1)),
    update = "increment"
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(survey = "mutually_exclusive"))
  )
})

test_that("a count total below its starting level aborts at the first wave", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(c(3, 3), c(7, 7), c(NA, 10), c(6, 4)))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(survey = "count"))
  )
})

test_that("a passing starting state adds no wave and no row", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 3), c(7, 7, 7), c(NA, 10, 20), c(6, 6, 9)),
    update = "increment"
  )
  checked <- check_layer(x, "count")
  expect_equal(checked$rows, check_rows(x))
  expect_equal(sort(unique(checked$rows$time)), c(10, 20))
  expect_equal(checked$waves, c(10, 20))
})

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

test_that("the prepared layers are recorded with their declared styles", {
  x <- make_prepare_fixture()
  out <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(calls = "count", advice = "none"),
    form = c(calls = "changes", advice = "changes")
  ))
  expect_identical(
    out$info$dynes_prepared,
    c(calls = "count", advice = "none")
  )
  expect_null(x$info$dynes_prepared)
})

test_that("a later call adds to the record", {
  x <- make_prepare_fixture()
  first <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(calls = "count"),
    form = c(calls = "changes")
  ))
  second <- suppressMessages(prepare_dynes_data(
    first,
    layers = c(advice = "none"),
    form = c(advice = "changes")
  ))
  expect_identical(
    second$info$dynes_prepared,
    c(calls = "count", advice = "none")
  )
})

test_that("a prepared layer cannot be prepared again", {
  local_cli_context()
  x <- make_prepare_fixture()
  out <- suppressMessages(prepare_dynes_data(
    x,
    layers = c(calls = "count"),
    form = c(calls = "changes")
  ))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      out,
      layers = c(calls = "count"),
      form = c(calls = "changes")
    )
  )
  x$info$dynes_prepared <- c(friendship = "mutually_exclusive")
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive"))
  )
})

test_that("a layer that is already flavored aborts, naming every such layer", {
  local_cli_context()
  x <- make_prepare_fixture()
  x <- suppressWarnings(add_flavor(
    x,
    "friendship",
    c(creation = 1, dissolution = 0)
  ))
  x <- suppressWarnings(add_flavor(
    x,
    "advice",
    c(creation = 1, dissolution = 0)
  ))
  expect_error(
    prepare_dynes_data(
      x,
      layers = c(friendship = "mutually_exclusive", advice = "none")
    ),
    class = "goldfish_layer_already_flavored"
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      layers = c(friendship = "mutually_exclusive", advice = "none")
    )
  )
})

test_that("a flavored count layer gets the count guidance", {
  local_cli_context()
  x <- make_prepare_fixture()
  x$info$values_equivalence <- list(calls = c(signed = 1, ended = -1))
  expect_error(
    prepare_dynes_data(x, layers = c(calls = "count")),
    class = "goldfish_layer_already_flavored"
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, layers = c(calls = "count"))
  )
})

# The timed rows a call wrote for one layer, in the order they were returned.
written_rows <- function(out, layer = "survey") {
  ties <- as.data.frame(out$ties)
  rows <- ties[ties$layer == layer & !is.na(ties$time), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

prepare_quietly <- function(...) {
  suppressMessages(prepare_dynes_data(...))
}

test_that("the first wave is written at its own time on an empty network", {
  x <- make_wave_fixture(
    wave_rows(c(3, 4, 1, 3), c(7, 8, 2, 7), c(10, 10, 10, 20))
  )
  out <- prepare_quietly(x, layers = c(survey = "none"))
  rows <- written_rows(out)
  first <- rows[rows$time == 10, ]
  expect_equal(nrow(first), 3L)
  expect_equal(first$weight, c(1, 1, 1))
  expect_false(anyNA(out$ties$time))
})

test_that("the first wave is diffed against the time = NA rows", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 4), c(7, 7, 8), c(NA, 10, 10))
  )
  out <- prepare_quietly(x, layers = c(survey = "none"))
  history <- out$ties[is.na(out$ties$time), ]
  expect_equal(c(history$from, history$to, history$weight), c(3, 7, 1))
  rows <- written_rows(out)
  expect_equal(c(rows$from, rows$to, rows$time), c(4, 8, 10))
})

test_that("a dropped tie gets 0 under replace and -1 under increment", {
  rows <- wave_rows(c(3, 1, 1), c(7, 2, 2), c(10, 10, 20))
  replace <- prepare_quietly(
    make_wave_fixture(rows, update = "replace"),
    layers = c(survey = "none")
  )
  increment <- prepare_quietly(
    make_wave_fixture(rows, update = "increment"),
    layers = c(survey = "none")
  )
  for (out in list(replace, increment)) {
    at_two <- written_rows(out)
    at_two <- at_two[at_two$time == 20, ]
    expect_equal(c(at_two$from, at_two$to), c(3, 7))
  }
  expect_equal(
    written_rows(replace)$weight[written_rows(replace)$time == 20],
    0
  )
  expect_equal(
    written_rows(increment)$weight[written_rows(increment)$time == 20],
    -1
  )
})

test_that("a stable tie is not repeated", {
  x <- make_wave_fixture(wave_rows(c(1, 1, 3), c(2, 2, 7), c(10, 20, 20)))
  rows <- written_rows(prepare_quietly(x, layers = c(survey = "none")))
  expect_equal(sum(rows$from == 1 & rows$to == 2), 1L)
  expect_equal(rows$time[rows$from == 1 & rows$to == 2], 10)
})

test_that("no row is moved to time = NA", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 1, 4), c(7, 7, 2, 8), c(NA, 10, 10, 20))
  )
  out <- prepare_quietly(x, layers = c(survey = "none"))
  expect_equal(sum(is.na(out$ties$time)), 1L)
})

test_that("an undirected tie that switches orientation writes no row", {
  x <- make_wave_fixture(
    wave_rows(c(3, 7, 1), c(7, 3, 2), c(10, 20, 20)),
    directed = FALSE
  )
  rows <- written_rows(prepare_quietly(x, layers = c(survey = "none")))
  expect_equal(nrow(rows[rows$time == 20, ]), 1L)
  expect_equal(c(rows$from[rows$time == 20], rows$to[rows$time == 20]), c(1, 2))
})

test_that("an increment layer sums history rows and reads waves as levels", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3, 3, 4), c(7, 7, 7, 8), c(NA, NA, 10, 10), c(1, 1, 2, 1)),
    update = "increment"
  )
  rows <- written_rows(prepare_quietly(x, layers = c(survey = "count")))
  expect_equal(c(rows$from, rows$to, rows$time, rows$weight), c(4, 8, 10, 1))
})

test_that("count totals are written as rises under increment", {
  x <- make_wave_fixture(
    wave_rows(c(3, 3), c(7, 7), c(10, 20), c(6, 9)),
    update = "increment"
  )
  rows <- written_rows(prepare_quietly(x, layers = c(survey = "count")))
  expect_equal(rows$time, c(10, 20))
  expect_equal(rows$weight, c(6, 3))
})

test_that("an order column is not carried onto the written rows", {
  rows <- wave_rows(c(3, 1, 1), c(7, 2, 2), c(10, 10, 20))
  rows$order <- c(2L, 1L, 1L)
  out <- prepare_quietly(make_wave_fixture(rows), layers = c(survey = "none"))
  expect_true(all(is.na(written_rows(out)$order)))
})

test_that("values are compared exactly, with no tolerance", {
  x <- make_wave_fixture(wave_rows(
    c(3, 3),
    c(7, 7),
    c(10, 20),
    c(0.3, 0.1 + 0.2)
  ))
  rows <- written_rows(prepare_quietly(x, layers = c(survey = "none")))
  expect_equal(nrow(rows), 2L)
  expect_identical(rows$weight[rows$time == 20], 0.1 + 0.2)
})

test_that("other layers keep position; written rows are sorted and appended", {
  x <- make_prepare_fixture()
  out <- prepare_quietly(x, layers = c(advice = "none"))
  others <- x$ties$layer != "advice"
  n_other <- sum(others)
  expect_equal(
    out$ties[seq_len(n_other), ],
    x$ties[others, ],
    ignore_attr = "row.names"
  )
  appended <- out$ties[-seq_len(n_other), ]
  expect_equal(appended$layer, rep("advice", 3L))
  expect_equal(appended$time, c(10, 20, 20))
  expect_equal(appended$from, c(2L, 2L, 3L))
  expect_equal(appended$to, c(4L, 4L, 1L))
  expect_equal(appended$weight, c(1, 0, 1))
})

test_that("a missing value column is created with 1 on other layers' rows", {
  x <- make_prepare_fixture()
  x$ties$weight <- NULL
  out <- prepare_quietly(x, layers = c(advice = "none"))
  others <- out$ties$layer != "advice"
  expect_true(all(out$ties$weight[others] == 1))
  expect_equal(out$ties$weight[!others], c(1, 0, 1))
})

test_that("rows written at a Date grid time on a POSIXct column are POSIXct", {
  t1 <- as.POSIXct("2008-09-09", tz = "GMT")
  t2 <- as.POSIXct("2008-10-19", tz = "GMT")
  x <- make_wave_fixture(wave_rows(c(3, 1), c(7, 2), c(t1, t1)))
  x$ties$time <- .POSIXct(x$ties$time, tz = "GMT")
  checked <- check_layer(x, "none")
  written <- diff_wave_states(
    checked$rows,
    checked$start,
    waves = as.Date(c("2008-09-09", "2008-10-19")),
    update = "replace",
    template = x$ties$time
  )
  expect_s3_class(written$time, "POSIXct")
  expect_equal(written$time[written$value == 0], c(t2, t2))
})

# The social_evolution friendship panel among the 51 actors who answered both
# surveys (the dynes-panel vignette's respondent selection).
make_social_evolution_panel <- function() {
  se <- social_evolution
  ties <- as.data.frame(se$ties)
  friendship <- ties[ties$layer == "friendship", ]
  waves <- sort(unique(friendship$time))
  both <- intersect(
    friendship$from[friendship$time == waves[1]],
    friendship$from[friendship$time == waves[2]]
  )
  ties <- ties[ties$from %in% both & ties$to %in% both, ]
  ties$from <- match(ties$from, both)
  ties$to <- match(ties$to, both)
  rownames(ties) <- NULL
  se$nodes <- as.data.frame(se$nodes)[both, ]
  se$ties <- ties
  list(data = se, waves = waves)
}

test_that("social_evolution: 276 first-wave rows, 70 formed, 70 dissolved", {
  panel <- make_social_evolution_panel()
  out <- prepare_quietly(panel$data, layers = c(friendship = "none"))
  rows <- written_rows(out, "friendship")
  first <- rows$time == panel$waves[1]
  expect_equal(sum(first), 276L)
  expect_equal(sum(!first & rows$weight == 1), 70L)
  expect_equal(sum(!first & rows$weight == 0), 70L)
  expect_s3_class(rows$time, "POSIXct")
  expect_equal(sum(out$ties$layer == "calls"), 364L)
})

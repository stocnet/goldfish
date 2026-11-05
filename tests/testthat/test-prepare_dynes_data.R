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

test_that("amounts are kept under increment and summed under replace", {
  rows <- wave_rows(c(3, 3), c(7, 7), c(10, 20), c(6, 3))
  increment <- written_rows(prepare_quietly(
    make_wave_fixture(rows, update = "increment"),
    layers = c(survey = "count"),
    form = c(survey = "per_wave")
  ))
  replace <- written_rows(prepare_quietly(
    make_wave_fixture(rows, update = "replace"),
    layers = c(survey = "count"),
    form = c(survey = "per_wave")
  ))
  expect_equal(increment$time, c(10, 20))
  expect_equal(increment$weight, c(6, 3))
  expect_equal(replace$time, c(10, 20))
  expect_equal(replace$weight, c(6, 9))
})

test_that("amounts are added to the level folded from the history rows", {
  x <- make_wave_fixture(wave_rows(c(3, 3), c(7, 7), c(NA, 10), c(2, 6)))
  rows <- written_rows(prepare_quietly(
    x,
    layers = c(survey = "none"),
    form = c(survey = "per_wave")
  ))
  expect_equal(c(rows$time, rows$weight), c(10, 8))
})

test_that("a dyad without a row at a per_wave wave writes nothing there", {
  x <- make_wave_fixture(
    wave_rows(c(3, 1), c(7, 2), c(10, 20), c(6, 1)),
    update = "replace"
  )
  rows <- written_rows(prepare_quietly(
    x,
    layers = c(survey = "count"),
    form = c(survey = "per_wave")
  ))
  expect_equal(nrow(rows), 2L)
  expect_equal(c(rows$from[rows$time == 20], rows$to[rows$time == 20]), c(1, 2))
})

# The survey's level at each wave as an 8 x 8 matrix, every dyad not reported
# being 0, against the state the walk materializes from the returned rows.
survey_level <- function(rows) {
  level <- matrix(0, 8L, 8L)
  level[cbind(rows$from, rows$to)] <- rows$weight
  level
}

expect_round_trip <- function(out, levels, waves) {
  for (k in seq_along(waves)) {
    state <- network_state_at(out, "survey", time = waves[k] + 1)
    expect_equal(unname(state), levels[[k]], info = paste("wave", waves[k]))
  }
}

test_that("a binary state panel round-trips at every wave", {
  history <- wave_rows(1, 2, NA)
  waves <- list(
    wave_rows(c(1, 3, 4), c(2, 7, 8), 10),
    wave_rows(c(1, 5, 3), c(2, 6, 7), 20),
    wave_rows(c(5, 4, 2), c(6, 8, 1), 30)
  )
  levels <- lapply(waves, survey_level)
  for (update in c("increment", "replace")) {
    x <- make_wave_fixture(
      do.call(rbind, c(list(history), waves)),
      update = update
    )
    out <- prepare_quietly(x, layers = c(survey = "none"))
    expect_round_trip(out, levels, c(10, 20, 30))
  }
})

test_that("calls given as totals and as amounts round-trip to the totals", {
  totals <- list(
    wave_rows(c(3, 4), c(7, 8), 10, c(2, 3)),
    wave_rows(c(3, 4, 1), c(7, 8, 2), 20, c(5, 3, 1)),
    wave_rows(c(3, 4, 1), c(7, 8, 2), 30, c(5, 3, 4))
  )
  amounts <- list(
    wave_rows(c(3, 4), c(7, 8), 10, c(2, 3)),
    wave_rows(c(3, 1), c(7, 2), 20, c(3, 1)),
    wave_rows(1, 2, 30, 3)
  )
  levels <- lapply(totals, survey_level)
  for (update in c("increment", "replace")) {
    as_totals <- prepare_quietly(
      make_wave_fixture(do.call(rbind, totals), update = update),
      layers = c(survey = "count")
    )
    as_amounts <- prepare_quietly(
      make_wave_fixture(do.call(rbind, amounts), update = update),
      layers = c(survey = "count"),
      form = c(survey = "per_wave")
    )
    expect_round_trip(as_totals, levels, c(10, 20, 30))
    expect_round_trip(as_amounts, levels, c(10, 20, 30))
  }
})

# A binary survey layer: (1, 2) stable, (3, 7) dropped at wave 2, (4, 8) formed
# at wave 2, with one history row.
binary_survey_rows <- function() {
  wave_rows(
    c(1, 1, 3, 1, 4),
    c(2, 2, 7, 2, 8),
    c(NA, 10, 10, 20, 20)
  )
}

test_that("a mutually exclusive layer equals the diff, then add_flavor()", {
  for (update in c("increment", "replace")) {
    x <- make_wave_fixture(binary_survey_rows(), update = update)
    flavored <- prepare_quietly(x, layers = c(survey = "mutually_exclusive"))
    map <- c(creation = 1, dissolution = if (update == "replace") 0 else -1)
    piped <- add_flavor(
      prepare_quietly(x, layers = c(survey = "none")),
      "survey",
      values_equivalence = map,
      flavor_style = "mutually_exclusive"
    )
    expect_equal(flavored$ties, piped$ties, info = update)
    expect_equal(
      flavored$info$values_equivalence,
      piped$info$values_equivalence,
      info = update
    )
    expect_equal(flavored$info$flavor_style, piped$info$flavor_style)
    expect_equal(flavored$info$values_equivalence$survey, map)
  }
})

test_that("a mutually exclusive change-list gains the map, rows unchanged", {
  x <- make_wave_fixture(
    wave_rows(c(1, 3, 1), c(2, 7, 2), c(10, 10, 20), c(1, 1, -1)),
    update = "increment"
  )
  out <- prepare_quietly(
    x,
    layers = c(survey = "mutually_exclusive"),
    form = c(survey = "changes")
  )
  expect_equal(out$ties[names(x$ties)], x$ties)
  expect_equal(
    out$ties$flavor,
    c("creation", "creation", "dissolution")
  )
  expect_equal(
    out$info$values_equivalence$survey,
    c(creation = 1, dissolution = -1)
  )
  expect_equal(out$info$flavor_style, c(survey = "mutually_exclusive"))
})

test_that("a count or none layer gets no map and no flavor value", {
  x <- make_prepare_fixture()
  out <- prepare_quietly(
    x,
    layers = c(calls = "count", advice = "none", friendship = "none"),
    form = c(calls = "per_wave")
  )
  expect_null(out$info$values_equivalence)
  expect_null(out$info$flavor_style)
  expect_false("flavor" %in% names(out$ties))
})

test_that("a flavor column is created with NA on the other layers' rows", {
  x <- make_prepare_fixture()
  out <- prepare_quietly(
    x,
    layers = c(friendship = "mutually_exclusive", calls = "count"),
    form = c(calls = "per_wave")
  )
  mine <- out$ties$layer == "friendship" & !is.na(out$ties$time)
  expect_true(all(is.na(out$ties$flavor[!mine])))
  expect_false(anyNA(out$ties$flavor[mine]))
  expect_null(out$info$values_equivalence$calls)
})

test_that("flavor_names relabels the map and the flavor column", {
  x <- make_wave_fixture(binary_survey_rows(), update = "replace")
  default <- prepare_quietly(x, layers = c(survey = "mutually_exclusive"))
  relabeled <- prepare_quietly(
    x,
    layers = c(survey = "mutually_exclusive"),
    flavor_names = list(survey = c(creation = "formed", dissolution = "ended"))
  )
  expect_equal(
    relabeled$info$values_equivalence$survey,
    c(formed = 1, ended = 0)
  )
  rows <- written_rows(relabeled)
  expect_setequal(rows$flavor, c("formed", "ended"))
  relabel <- c(creation = "formed", dissolution = "ended")
  expect_equal(rows$flavor, unname(relabel[written_rows(default)$flavor]))
  expect_equal(
    rows[c("from", "to", "time", "weight")],
    written_rows(default)[c("from", "to", "time", "weight")]
  )
})

test_that("a mutually exclusive binary panel round-trips at every wave", {
  history <- wave_rows(1, 2, NA)
  waves <- list(
    wave_rows(c(1, 3, 4), c(2, 7, 8), 10),
    wave_rows(c(1, 5, 3), c(2, 6, 7), 20),
    wave_rows(c(5, 4, 2), c(6, 8, 1), 30)
  )
  levels <- lapply(waves, survey_level)
  for (update in c("increment", "replace")) {
    x <- make_wave_fixture(
      do.call(rbind, c(list(history), waves)),
      update = update
    )
    out <- prepare_quietly(x, layers = c(survey = "mutually_exclusive"))
    expect_round_trip(out, levels, c(10, 20, 30))
  }
})

test_that("social_evolution as mutually exclusive: 70 formed, 70 ended", {
  panel <- make_social_evolution_panel()
  out <- prepare_quietly(
    panel$data,
    layers = c(friendship = "mutually_exclusive"),
    flavor_names = list(
      friendship = c(creation = "formed", dissolution = "ended")
    )
  )
  rows <- written_rows(out, "friendship")
  later <- rows$time != panel$waves[1]
  expect_equal(sum(rows$flavor[!later] == "formed"), 276L)
  expect_equal(sum(rows$flavor[later] == "formed"), 70L)
  expect_equal(sum(rows$flavor[later] == "ended"), 70L)
})

# A survey with no row at time 20: (1, 2) and (3, 7) at 10, (1, 2) and (4, 8)
# at 30.
gapped_survey_rows <- function() {
  wave_rows(c(1, 3, 1, 4), c(2, 7, 2, 8), c(10, 10, 30, 30))
}

# Two panel layers, friendship and advice, from (from, to, time, weight, layer)
# rows.
make_two_layer_fixture <- function(rows) {
  nodes <- data.frame(
    label = c("A", "B", "C", "D", "E", "F", "G", "H"),
    mode = "p",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy",
    focal = "friendship",
    update = c(friendship = "replace", advice = "replace"),
    directed = c(friendship = TRUE, advice = TRUE),
    observation = c(friendship = "panel", advice = "panel")
  )
  list(info = info, nodes = nodes, ties = rows)
}

test_that("an empty declared states wave dissolves every tie", {
  x <- make_wave_fixture(gapped_survey_rows())
  for (wave_times in list(c(10, 20, 30), list(survey = c(10, 20, 30)))) {
    rows <- written_rows(prepare_quietly(
      x,
      layers = c(survey = "none"),
      wave_times = wave_times
    ))
    at_20 <- rows[rows$time == 20, ]
    expect_equal(c(at_20$from, at_20$to, at_20$weight), c(1, 3, 2, 7, 0, 0))
    expect_equal(sum(rows$time == 30), 2L)
  }
})

test_that("an empty declared per_wave wave writes nothing", {
  x <- make_wave_fixture(gapped_survey_rows())
  expect_warning(
    out <- prepare_quietly(
      x,
      layers = c(survey = "count"),
      form = c(survey = "per_wave"),
      wave_times = c(10, 20, 30)
    ),
    class = "goldfish_empty_wave"
  )
  expect_equal(sum(written_rows(out)$time == 20), 0L)
  expect_equal(
    network_state_at(out, "survey", time = 21),
    network_state_at(out, "survey", time = 11)
  )
})

test_that("an inferred grid assumes no empty wave", {
  x <- make_wave_fixture(gapped_survey_rows())
  rows <- written_rows(prepare_quietly(x, layers = c(survey = "none")))
  expect_equal(unique(rows$time), c(10, 30))
})

test_that("layers on different schedules get their own grids", {
  rows <- rbind(
    cbind(wave_rows(c(1, 1), c(2, 2), c(10, 20)), layer = "friendship"),
    cbind(wave_rows(c(3, 4), c(7, 8), c(10, 30)), layer = "advice")
  )
  # friendship's tie is stable at 20, so that wave writes nothing and warns.
  expect_warning(
    out <- prepare_quietly(
      make_two_layer_fixture(rows),
      layers = c(friendship = "none", advice = "none"),
      wave_times = list(friendship = c(10, 20), advice = c(10, 30))
    ),
    class = "goldfish_empty_wave"
  )
  advice <- written_rows(out, "advice")
  expect_equal(sum(advice$time == 20), 0L)
  expect_equal(advice$time, c(10, 30, 30))
  expect_equal(written_rows(out, "friendship")$time, 10)
})

test_that("a row off the grid aborts, naming the layer and the time", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(c(1, 3, 4), c(2, 7, 8), c(10, 15, 20)))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, c(survey = "none"), wave_times = c(10, 20))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      c(survey = "count"),
      form = c(survey = "per_wave"),
      wave_times = list(survey = c(10, 20))
    )
  )
})

test_that("a Date grid matches POSIXct rows at the same instants", {
  day <- as.Date(c("2008-09-09", "2008-10-19", "2008-11-29"))
  at <- as.POSIXct(format(day), tz = "GMT")
  x <- make_wave_fixture(gapped_survey_rows())
  x$ties$time <- at[match(x$ties$time, c(10, 20, 30))]
  rows <- written_rows(prepare_quietly(
    x,
    layers = c(survey = "none"),
    wave_times = day
  ))
  expect_s3_class(rows$time, "POSIXct")
  expect_equal(rows$time[rows$weight == 0], c(at[2], at[2]))
  expect_equal(sum(rows$time == at[3]), 2L)
})

test_that("a malformed wave_times aborts before any row is rewritten", {
  local_cli_context()
  rows <- rbind(
    cbind(wave_rows(c(1, 1), c(2, 2), c(10, 20)), layer = "friendship"),
    cbind(wave_rows(c(3, 4), c(7, 8), c(10, 30)), layer = "advice")
  )
  x <- make_two_layer_fixture(rows)
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      c(friendship = "none"),
      wave_times = list(advice = c(10, 30))
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      c(friendship = "none", advice = "none"),
      form = c(advice = "changes"),
      wave_times = list(advice = c(10, 30))
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      c(friendship = "none"),
      wave_times = list(friendship = c(10, 20), c(10, 30))
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, c(friendship = "none"), wave_times = c(10, NA))
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      c(friendship = "none"),
      wave_times = list(friendship = c(10, Inf))
    )
  )
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(
      x,
      c(friendship = "none", advice = "none"),
      form = c(friendship = "changes", advice = "changes"),
      wave_times = c(10, 20)
    )
  )
})

test_that("a states or per_wave layer with no wave aborts", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(c(1, 3), c(2, 7), c(NA, NA)))
  expect_snapshot(error = TRUE, prepare_dynes_data(x, c(survey = "none")))
  expect_snapshot(
    error = TRUE,
    prepare_dynes_data(x, c(survey = "count"), form = c(survey = "per_wave"))
  )
})

test_that("an unsorted grid gives the sorted result silently", {
  x <- make_wave_fixture(gapped_survey_rows())
  sorted <- prepare_quietly(
    x,
    layers = c(survey = "none"),
    wave_times = c(10, 20, 30)
  )
  expect_no_warning(
    unsorted <- prepare_quietly(
      x,
      layers = c(survey = "none"),
      wave_times = c(20, 10, 30)
    )
  )
  expect_identical(unsorted, sorted)
})

test_that("a duplicated grid time warns once and is kept once", {
  local_cli_context()
  x <- make_wave_fixture(gapped_survey_rows())
  once <- prepare_quietly(
    x,
    layers = c(survey = "none"),
    wave_times = c(10, 20, 30)
  )
  expect_snapshot(
    twice <- prepare_quietly(
      x,
      layers = c(survey = "none"),
      wave_times = list(survey = c(10, 20, 20, 30))
    )
  )
  expect_identical(twice, once)
  expect_warning(
    prepare_quietly(
      x,
      layers = c(survey = "none"),
      wave_times = c(10, 20, 20, 30)
    ),
    class = "goldfish_duplicate_wave_time"
  )
})

test_that("layers not named are unchanged and event rows keep their times", {
  se <- social_evolution
  out <- prepare_quietly(se, layers = c(friendship = "mutually_exclusive"))
  before <- as.data.frame(se$ties)
  after <- as.data.frame(out$ties)
  calls_before <- before[before$layer == "calls", ]
  calls_after <- after[after$layer == "calls", names(before)]
  expect_equal(calls_after, calls_before, ignore_attr = "row.names")
  expect_true(all(is.na(after$flavor[after$layer == "calls"])))

  first_wave <- min(before$time[before$layer == "friendship"])
  early <- calls_after$time < first_wave
  expect_equal(sum(early), 1L)
  expect_equal(
    format(calls_after$time[early], "%Y-%m-%d", tz = "GMT"),
    "2008-09-06"
  )
  expect_false(anyNA(calls_after$time))
})

test_that("an untouched panel layer keeps its rows when another is prepared", {
  x <- make_prepare_fixture()
  out <- prepare_quietly(
    x,
    layers = c(friendship = "mutually_exclusive"),
    wave_times = list(friendship = c(10, 20))
  )
  kept <- x$ties$layer != "friendship"
  expect_equal(
    out$ties[seq_len(sum(kept)), names(x$ties)],
    x$ties[kept, ],
    ignore_attr = "row.names"
  )
})

# The empty-wave warnings a call raises, collected so they can be counted.
collect_empty_wave_warnings <- function(expr) {
  caught <- list()
  value <- withCallingHandlers(
    expr,
    goldfish_empty_wave = function(w) {
      caught[[length(caught) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = caught)
}

test_that("an unchanged middle wave warns, naming the layer and the wave", {
  local_cli_context()
  x <- make_wave_fixture(
    wave_rows(c(1, 3, 1, 3, 1), c(2, 7, 2, 7, 2), c(10, 10, 20, 20, 30))
  )
  expect_snapshot(
    out <- prepare_quietly(x, layers = c(survey = "mutually_exclusive"))
  )
  expect_equal(sum(written_rows(out)$time == 20), 0L)
  expect_warning(
    prepare_quietly(x, layers = c(survey = "mutually_exclusive")),
    class = "goldfish_empty_wave"
  )
})

test_that("a first wave equal to the time = NA rows warns, naming it", {
  local_cli_context()
  x <- make_wave_fixture(wave_rows(c(1, 1, 3), c(2, 2, 7), c(NA, 10, 20)))
  expect_snapshot(
    out <- prepare_quietly(x, layers = c(survey = "none"))
  )
  expect_equal(unique(written_rows(out)$time), 20)
})

test_that("empty waves on several layers raise exactly one warning", {
  local_cli_context()
  rows <- rbind(
    cbind(
      wave_rows(c(1, 1, 1), c(2, 2, 2), c(10, 20, 30)),
      layer = "friendship"
    ),
    cbind(wave_rows(c(3, 3), c(7, 7), c(10, 20)), layer = "advice")
  )
  rows$from[rows$layer == "friendship" & rows$time == 30] <- 4
  rows$to[rows$layer == "friendship" & rows$time == 30] <- 8
  x <- make_two_layer_fixture(rows)
  prepare <- function() {
    prepare_quietly(
      x,
      layers = c(friendship = "none", advice = "count"),
      form = c(advice = "per_wave"),
      wave_times = list(advice = c(10, 20, 30))
    )
  }
  caught <- collect_empty_wave_warnings(prepare())
  expect_length(caught$warnings, 1L)
  expect_snapshot(out <- prepare())
  expect_equal(sum(written_rows(out, "advice")$time == 30), 0L)
  expect_equal(sum(written_rows(out, "friendship")$time == 20), 0L)
})

test_that("a call whose every wave writes a row raises no empty-wave warning", {
  x <- make_wave_fixture(gapped_survey_rows())
  caught <- collect_empty_wave_warnings(
    prepare_quietly(x, layers = c(survey = "none"), wave_times = c(10, 20, 30))
  )
  expect_length(caught$warnings, 0L)
})

# The summary messages a call raises, collected so they can be counted.
collect_summaries <- function(expr) {
  caught <- list()
  value <- withCallingHandlers(
    expr,
    goldfish_prepare_summary = function(m) {
      caught[[length(caught) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = caught)
}

test_that("a binary layer reports its flavors and its starting state", {
  local_cli_context()
  panel <- make_social_evolution_panel()
  prepare <- function() {
    prepare_dynes_data(
      panel$data,
      layers = c(friendship = "mutually_exclusive"),
      flavor_names = list(
        friendship = c(creation = "formed", dissolution = "ended")
      )
    )
  }
  caught <- collect_summaries(prepare())
  expect_length(caught$messages, 1L)
  expect_snapshot(out <- prepare())
})

test_that("each style reports its own totals, one line per layer", {
  local_cli_context()
  x <- make_prepare_fixture()
  caught <- collect_summaries(
    prepare_dynes_data(
      x,
      layers = c(
        friendship = "mutually_exclusive",
        calls = "count",
        advice = "none"
      ),
      form = c(calls = "per_wave", advice = "changes")
    )
  )
  expect_length(caught$messages, 1L)
  expect_snapshot(
    out <- prepare_dynes_data(
      x,
      layers = c(
        friendship = "mutually_exclusive",
        calls = "count",
        advice = "none"
      ),
      form = c(calls = "per_wave", advice = "changes")
    )
  )
})

test_that("a count reports unit events and changed dyads under either update", {
  local_cli_context()
  # (3, 7) rises by 3, then 4: 7 unit events on one dyad after the first wave.
  totals <- wave_rows(
    c(3, 4, 3, 4, 3, 4),
    c(7, 8, 7, 8, 7, 8),
    c(10, 10, 20, 20, 30, 30),
    c(2, 1, 5, 1, 9, 1)
  )
  for (update in c("replace", "increment")) {
    x <- make_wave_fixture(totals, update = update)
    expect_snapshot(out <- prepare_dynes_data(x, layers = c(survey = "count")))
  }
  changes <- wave_rows(c(3, 3, 4), c(7, 7, 8), c(10, 20, 20), c(2, 3, 1))
  expect_snapshot(
    out <- prepare_dynes_data(
      make_wave_fixture(changes, update = "increment"),
      layers = c(survey = "count"),
      form = c(survey = "changes")
    )
  )
})

test_that("the summary is silenced by class and leaves the result unchanged", {
  x <- make_prepare_fixture()
  prepare <- function() {
    prepare_dynes_data(
      x,
      layers = c(friendship = "mutually_exclusive", calls = "count"),
      form = c(calls = "per_wave")
    )
  }
  expect_silent(
    silenced <- suppressMessages(
      prepare(),
      classes = "goldfish_prepare_summary"
    )
  )
  expect_message(shown <- prepare(), class = "goldfish_prepare_summary")
  expect_identical(silenced, shown)
})

# --- End to end: prepared data through estimate_dynes() ---------------------
# These fits exercise the estimator on prepared data and never pipe into
# add_flavor(): the function flavors. A failure caused by the estimator is
# fixed in the estimator, not worked around in the preparation.

# The friendship panel modeled with a creation and a dissolution process,
# beside the observed calls that read it.
prepared_friendship_spec <- function(data) {
  make_joint_specification(
    make_specification(
      rate = list(creation ~ 1, dissolution ~ 1),
      choice = list(creation ~ recip, dissolution ~ recip),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~ tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# The warm start fits the model on one augmented path as if it were observed.
# An intercept-only rate has the same exposure on every path, so its estimate
# after that fit and one EM pass is the closed-form count / exposure.
fit_one_pass <- function(spec, control_prep = set_preprocessing()) {
  suppressWarnings(suppressMessages(estimate_dynes(
    spec,
    set_algorithm_em(
      n_sequences = 2L,
      max_iterations = 1L,
      seed = 1L,
      augmenter = set_augmenter_options(routine = "random", warm_start = TRUE)
    ),
    control_prep
  )))
}

test_that("social_evolution prepared and fitted scores the 70 changes only", {
  skip_on_cran()
  panel <- make_social_evolution_panel()
  data <- prepare_quietly(
    panel$data,
    layers = c(friendship = "mutually_exclusive")
  )

  fit <- fit_one_pass(prepared_friendship_spec(data))

  # The default start is the first survey: its 276 ties are the entering
  # state, so the creation rate counts the 70 ties formed between the waves
  # over the 51 actors' exposure, not 346.
  span <- as.numeric(panel$waves[2]) - as.numeric(panel$waves[1])
  intercept <- coef(fit)[["friendship › creation › rate: Intercept"]]
  expect_equal(intercept, log(70 / (51 * span)), tolerance = 1e-6)
})

# The augmentation recipe estimate_dynes() compiles at entry, captured as it is
# built: the run stops there, before any draw.
entry_recipe <- function(spec, control_prep = set_preprocessing()) {
  real_recipe <- compile_augmentation_recipe
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    compile_augmentation_recipe = function(spec, wave_times, ...) {
      seen$recipe <- real_recipe(spec, wave_times, ...)
      rlang::abort("Recipe captured.", class = "recipe_captured")
    }
  )
  expect_error(
    suppressWarnings(suppressMessages(estimate_dynes(
      spec,
      set_algorithm_em(
        n_sequences = 2L,
        seed = 1L,
        augmenter = set_augmenter_options(routine = "random")
      ),
      control_prep
    ))),
    class = "recipe_captured"
  )
  seen$recipe
}

# A calls panel given as the amounts since the previous survey (waves 10, 20,
# 30) beside an observed emails stream. (1, 2) gets 2, 3, 1 calls, (2, 3) 1 at
# wave 20 and (3, 1) 4 at wave 30.
make_amounts_panel <- function(update) {
  list(
    info = list(
      name = "toy",
      focal = "emails",
      update = c(calls = update, emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "panel", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 1L, 2L, 1L, 3L),
        to = c(2L, 2L, 3L, 2L, 1L),
        time = c(10, 20, 20, 30, 30),
        weight = c(2, 3, 1, 1, 4),
        layer = "calls"
      ),
      data.frame(
        from = c(1L, 3L, 2L),
        to = c(3L, 4L, 1L),
        time = c(12, 22, 27),
        weight = 1,
        layer = "emails"
      )
    )
  )
}

test_that("a count panel prepared from amounts is estimated as a count panel", {
  skip_on_cran()
  for (update in c("increment", "replace")) {
    data <- prepare_quietly(
      make_amounts_panel(update),
      layers = c(calls = "count"),
      form = c(calls = "per_wave")
    )
    spec <- make_joint_specification(
      make_specification(
        rate = ~1,
        choice = ~inertia,
        layer = "calls",
        model = "DyNAM",
        data = data
      ),
      make_specification(
        rate = ~1,
        choice = ~inertia,
        layer = "emails",
        model = "DyNAM",
        data = data
      ),
      data = data
    )

    recipe <- entry_recipe(spec)

    expect_identical(recipe$layers$calls$kind, "count")
    # Under the default start the first survey is the entering state, so the
    # intervals end at waves 20 and 30 and hold that wave's summed amounts.
    expect_identical(recipe$wave_times, c(10, 20, 30))
    expect_identical(tabulate(recipe$events$interval, 2L), c(3L + 1L, 1L + 4L))
  }
})

# A friendship panel surveyed at 10, 20, 30 and 40 over six actors, beside
# observed calls, as reported states (`"states"`) or as the equivalent
# change-list under `replace` (`"changes"`). The wave-3 survey is
# {1 -> 2, 2 -> 1, 4 -> 5, 5 -> 6}; wave 4 drops 1 -> 2 and forms 3 -> 1 and
# 6 -> 1.
make_four_wave_panel <- function(form) {
  friendship <- if (form == "states") {
    data.frame(
      from = c(1, 2, 3, 1, 3, 4, 1, 4, 5, 2, 4, 5, 2, 3, 6),
      to = c(2, 3, 4, 2, 4, 5, 2, 5, 6, 1, 5, 6, 1, 1, 1),
      time = rep(c(10, 20, 30, 40), c(3, 3, 4, 5)),
      weight = 1
    )
  } else {
    data.frame(
      from = c(1, 2, 3, 2, 4, 3, 5, 2, 1, 3, 6),
      to = c(2, 3, 4, 3, 5, 4, 6, 1, 2, 1, 1),
      time = rep(c(10, 20, 30, 40), c(3, 2, 3, 3)),
      weight = c(1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1)
    )
  }
  friendship$layer <- "friendship"
  calls <- data.frame(
    from = c(1, 2, 3, 4, 5, 6, 1),
    to = c(3, 4, 5, 6, 1, 2, 4),
    time = c(12, 18, 25, 33, 36, 38, 39),
    weight = 1,
    layer = "calls"
  )
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(friendship = "replace", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = "panel", calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(friendship, calls)
  )
}

survey_matrix <- function(from, to, n = 6L) {
  state <- matrix(0, n, n)
  state[cbind(from, to)] <- 1
  state
}

test_that("a fit started at the third wave enters with that wave's survey", {
  skip_on_cran()
  start <- set_preprocessing(start_time = 30)
  for (form in c("states", "changes")) {
    data <- prepare_quietly(
      make_four_wave_panel(form),
      layers = c(friendship = "mutually_exclusive"),
      form = c(friendship = form)
    )
    spec <- prepared_friendship_spec(data)

    recipe <- entry_recipe(spec, start)

    expect_identical(recipe$wave_times, c(30, 40))
    expect_equal(
      recipe$start_states[[1]]$friendship,
      survey_matrix(c(1, 2, 4, 5), c(2, 1, 5, 6)),
      ignore_attr = TRUE
    )
    # Only the wave-4 changes are augmented; the wave-2 and wave-3 changes
    # were applied on the way to the entering state.
    flips <- recipe$events
    expect_identical(unique(flips$interval), 1L)
    expect_equal(
      flips[c("from", "to", "flavor")],
      data.frame(
        from = c(1L, 3L, 6L),
        to = c(2L, 1L, 1L),
        flavor = c("dissolution", "creation", "creation")
      ),
      ignore_attr = TRUE
    )

    # Scored, the earlier changes would add the wave-2 and wave-3 creations
    # to the count and the earlier span to the exposure: the intercept counts
    # the two wave-4 creations over six actors and ten time units only.
    fit <- fit_one_pass(spec, start)
    intercept <- coef(fit)[["friendship › creation › rate: Intercept"]]
    expect_equal(intercept, log(2 / (6 * 10)), tolerance = 1e-6)
  }
})

# A friendship panel over six actors with history rows {1 -> 2, 2 -> 3} and
# surveys at 10 ({1 -> 2, 3 -> 4}) and 20 ({3 -> 4, 4 -> 5}), beside observed
# calls, one of them before the first survey.
make_history_panel <- function() {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(friendship = "replace", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = "panel", calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = c(1, 2, 1, 3, 3, 4, 1, 2, 3, 4),
      to = c(2, 3, 2, 4, 4, 5, 3, 4, 5, 6),
      time = c(NA, NA, 10, 10, 20, 20, 5, 12, 18, 25),
      weight = 1,
      layer = rep(c("friendship", "calls"), c(6, 4))
    )
  )
}

test_that("the default start never augments the first-wave diff", {
  skip_on_cran()
  data <- prepare_quietly(
    make_history_panel(),
    layers = c(friendship = "mutually_exclusive")
  )

  recipe <- entry_recipe(prepared_friendship_spec(data))

  # The first survey is the entering state; the only interval ends at the
  # second, and its flips are the wave-2 diff alone.
  expect_identical(recipe$wave_times, c(10, 20))
  expect_equal(
    recipe$start_states[[1]]$friendship,
    survey_matrix(c(1, 3), c(2, 4)),
    ignore_attr = TRUE
  )
  expect_equal(
    recipe$events[c("interval", "from", "to", "flavor")],
    data.frame(
      interval = 1L,
      from = c(1L, 4L),
      to = c(2L, 5L),
      flavor = c("dissolution", "creation")
    ),
    ignore_attr = TRUE
  )
})

test_that("a start before the first survey augments the first-wave diff", {
  skip_on_cran()
  data <- prepare_quietly(
    make_history_panel(),
    layers = c(friendship = "mutually_exclusive")
  )

  recipe <- entry_recipe(
    prepared_friendship_spec(data),
    set_preprocessing(start_time = 0)
  )

  # The first survey is now the right endpoint of the first interval, which
  # enters with the history rows and holds the first wave's diff.
  expect_identical(recipe$wave_times, c(0, 10, 20))
  expect_equal(
    recipe$start_states[[1]]$friendship,
    survey_matrix(c(1, 2), c(2, 3)),
    ignore_attr = TRUE
  )
  expect_equal(
    recipe$events[c("interval", "from", "to", "flavor")],
    data.frame(
      interval = c(1L, 1L, 2L, 2L),
      from = c(2L, 3L, 1L, 4L),
      to = c(3L, 4L, 2L, 5L),
      flavor = c("dissolution", "creation", "dissolution", "creation")
    ),
    ignore_attr = TRUE
  )
})

test_that("history rows of a count panel are never augmented", {
  skip_on_cran()
  # Totals under increment: (1, 2) holds 2 calls before the panel, then 3, 5
  # and 6; (2, 3) 1, 1 and 2; (3, 1) 1 at the third wave.
  totals <- data.frame(
    from = c(1, 1, 2, 1, 2, 1, 2, 3),
    to = c(2, 2, 3, 2, 3, 2, 3, 1),
    time = c(NA, 10, 10, 20, 20, 30, 30, 30),
    weight = c(2, 3, 1, 5, 1, 6, 2, 1),
    layer = "panel_calls"
  )
  emails <- data.frame(
    from = c(1, 2, 3, 4),
    to = c(3, 4, 5, 6),
    time = c(12, 18, 25, 28),
    weight = 1,
    layer = "emails"
  )
  x <- list(
    info = list(
      name = "toy",
      focal = "emails",
      update = c(panel_calls = "increment", emails = "increment"),
      directed = c(panel_calls = TRUE, emails = TRUE),
      observation = c(panel_calls = "panel", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(totals, emails)
  )
  data <- prepare_quietly(x, layers = c(panel_calls = "count"))
  spec <- make_joint_specification(
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "panel_calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )

  recipe <- entry_recipe(spec)

  # The intervals lie between the waves; the history total of 2 and the rise
  # to the first wave's totals are the entering state, never flips.
  expect_identical(recipe$wave_times, c(10, 20, 30))
  expect_equal(
    recipe$events[c("interval", "from", "to")],
    data.frame(
      interval = c(1L, 1L, 2L, 2L, 2L),
      from = c(1L, 1L, 1L, 2L, 3L),
      to = c(2L, 2L, 2L, 3L, 1L)
    ),
    ignore_attr = TRUE
  )
})

# cli abort snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A one-mode increment layer with +-1 weights and a NA-time history row: the
# creation/dissolution shape add_flavor() is built for, assembled without any
# manynet call.
#
# The dyads are chosen so the stream is a VALID mutually exclusive sequence --
# each creation lands on an absent tie and each dissolution on a present one.
# Keep it that way: the default `flavor_style` is mutually_exclusive, and
# add_flavor() now warns when an event contradicts the declared style, so
# collapsing these onto one dyad would make every test here emit that warning.
make_flavor_fixture <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C"),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 1L, 2L, 1L),
    to = c(2L, 3L, 3L, 2L),
    time = c(NA, 1, 2, 3),
    weight = c(1, 1, 1, -1),
    layer = "friendship",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy",
    focal = "friendship",
    update = c(friendship = "increment"),
    directed = c(friendship = TRUE),
    observation = c(friendship = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

test_that("add_flavor stamps timed ties and records metadata", {
  x <- add_flavor(
    make_flavor_fixture(),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1)
  )

  expect_equal(
    x$ties$flavor,
    c(NA, "creation", "creation", "dissolution")
  )
  expect_equal(unname(x$info$flavor_style["friendship"]), "mutually_exclusive")
  expect_equal(
    x$info$values_equivalence$friendship,
    c(creation = 1, dissolution = -1)
  )
})

test_that("add_flavor leaves history (NA-time) rows unflavored", {
  x <- add_flavor(
    make_flavor_fixture(),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
  history <- is.na(x$ties$time)
  expect_true(all(is.na(x$ties$flavor[history])))
})

test_that("add_flavor stamps a replace layer from 1/0", {
  fix <- make_flavor_fixture()
  fix$info$update <- c(friendship = "replace")
  # Its own dyads: under replace the state is the value, so a legal
  # creation/dissolution/creation sequence needs the dissolution to land on the
  # dyad the history set and the second creation on the one it just cleared.
  fix$ties$from <- c(1L, 2L, 1L, 1L)
  fix$ties$to <- c(2L, 3L, 2L, 2L)
  fix$ties$weight <- c(1, 1, 0, 1)

  x <- add_flavor(
    fix,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = 0)
  )
  expect_equal(
    x$ties$flavor,
    c(NA, "creation", "dissolution", "creation")
  )
})

test_that("add_flavor preserves an existing flavor column on other layers", {
  fix <- make_flavor_fixture()
  fix$ties <- rbind(
    fix$ties,
    data.frame(
      from = 2L,
      to = 3L,
      time = 4,
      weight = 1,
      layer = "calls",
      stringsAsFactors = FALSE
    )
  )
  fix$ties$flavor <- c(NA, NA, NA, NA, "phone")
  fix$info$update <- c(friendship = "increment", calls = "increment")

  x <- add_flavor(
    fix,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
  expect_equal(x$ties$flavor[x$ties$layer == "calls"], "phone")
})

test_that("a flavor-stamped object passes validation", {
  x <- add_flavor(
    make_flavor_fixture(),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
  expect_no_error(validate_goldfish_data(x))
})

test_that("add_flavor aborts on non-dichotomous and non-syntactic mappings", {
  local_cli_context()
  x <- make_flavor_fixture()

  expect_snapshot(
    add_flavor(x, "friendship", c(a = 1, b = -1, c = 1)),
    error = TRUE
  )
  expect_snapshot(
    add_flavor(x, "friendship", c(`tie created` = 1, dissolution = -1)),
    error = TRUE
  )
})

test_that("add_flavor aborts when values do not match the update encoding", {
  local_cli_context()
  x <- make_flavor_fixture()
  expect_snapshot(
    add_flavor(x, "friendship", c(creation = 2, dissolution = -3)),
    error = TRUE
  )
})

test_that("add_flavor aborts on a weighted layer with an uncovered value", {
  local_cli_context()
  x <- make_flavor_fixture()
  x$ties$weight <- c(1, 1, 2, -1)
  expect_snapshot(
    add_flavor(x, "friendship", c(creation = 1, dissolution = -1)),
    error = TRUE
  )
})

test_that("add_flavor aborts on a bad style and an unknown layer", {
  local_cli_context()
  x <- make_flavor_fixture()
  expect_snapshot(
    add_flavor(
      x,
      "friendship",
      c(creation = 1, dissolution = -1),
      flavor_style = "both"
    ),
    error = TRUE
  )
  expect_snapshot(
    add_flavor(x, "nope", c(creation = 1, dissolution = -1)),
    error = TRUE
  )
})

test_that("the validator rejects malformed flavor metadata on info", {
  local_cli_context()
  x <- make_flavor_fixture()

  bad_style <- x
  bad_style$info$flavor_style <- c(friendship = "wrong")
  expect_snapshot(validate_goldfish_data(bad_style), error = TRUE)

  unknown_style_layer <- x
  unknown_style_layer$info$flavor_style <- c(nope = "mutually_exclusive")
  expect_snapshot(validate_goldfish_data(unknown_style_layer), error = TRUE)

  not_a_list <- x
  not_a_list$info$values_equivalence <- c(friendship = 1)
  expect_snapshot(validate_goldfish_data(not_a_list), error = TRUE)

  bad_mapping <- x
  bad_mapping$info$values_equivalence <- list(friendship = c(creation = 5))
  expect_snapshot(validate_goldfish_data(bad_mapping), error = TRUE)
})

test_that("a declared mutually exclusive style is checked against the events", {
  local_cli_context()
  # Two creations on one dyad with no dissolution between them: the second
  # lands where a derived `!tie()` mask would forbid it, which is the
  # "observed dyad is excluded" failure estimation would raise later.
  fix <- make_flavor_fixture()
  fix$ties$from <- c(1L, 1L, 1L, 1L)
  fix$ties$to <- c(2L, 3L, 3L, 2L)
  fix$ties$weight <- c(1, 1, 1, -1)

  expect_snapshot(
    invisible(add_flavor(
      fix,
      layer = "friendship",
      values_equivalence = c(creation = 1, dissolution = -1)
    ))
  )
})

test_that("the style check passes on a valid stream and on redundant", {
  fix <- make_flavor_fixture()
  expect_no_warning(
    add_flavor(
      fix,
      layer = "friendship",
      values_equivalence = c(creation = 1, dissolution = -1)
    )
  )

  # Repeated same-direction events are the point of `redundant`, so the check
  # does not apply to it at all.
  repeated <- fix
  repeated$ties$from <- c(1L, 1L, 1L, 1L)
  repeated$ties$to <- c(2L, 3L, 3L, 2L)
  expect_no_warning(
    add_flavor(
      repeated,
      layer = "friendship",
      values_equivalence = c(creation = 1, dissolution = -1),
      flavor_style = "redundant"
    )
  )
})

test_that("history alone never contradicts the style", {
  # A dyad lifted to 2 by history and then dissolved: the state is not binary,
  # but no event lands where its own mask forbids it, so nothing is wrong. The
  # check is about events, not about the trajectory.
  fix <- make_flavor_fixture()
  fix$ties$from <- c(1L, 1L, 2L, 1L)
  fix$ties$to <- c(2L, 2L, 3L, 2L)
  fix$ties$time <- c(NA, NA, 1, 2)
  fix$ties$weight <- c(1, 1, 1, -1)

  expect_no_warning(
    add_flavor(
      fix,
      layer = "friendship",
      values_equivalence = c(creation = 1, dissolution = -1)
    )
  )
})

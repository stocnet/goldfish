# The dependent process on the stocnet path.
#
# Legacy pairs a dependent-events object with the network it updates, so the two
# streams carry different names. A stocnet focal layer is both, so the seam has
# to keep them apart itself -- these tests pin that separation, the layer's role
# as its own default network, and the dependent check.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# The focal layer carrying two flavors: one modeled, one state-only.
flavored_fixture <- function() {
  x <- make_stocnet_fixture()
  x$ties$flavor <- c(NA, "creation", "dissolution")
  x
}

test_that("the dependent stream is keyed apart from the network stream", {
  src <- new_data_source(data = make_stocnet_fixture(), focal = "calls")

  expect_identical(ds_dependent_key(src, "calls"), DEPENDENT_STREAM)
  expect_false(
    identical(ds_dependent_key(src, "calls"), "calls"),
    label = "the focal layer is both the dependent process and a network"
  )
})

test_that("a legacy dependent stream keeps its own name as its key", {
  src <- new_data_source(envir = new.env())

  expect_identical(
    ds_dependent_key(src, "calls_dependent"),
    "calls_dependent",
    label = "legacy names the two streams differently already"
  )
})

test_that("the dependent stream carries only the modeled rows", {
  x <- flavored_fixture()
  map <- build_mode_map(x$info, x$nodes, "calls")
  streams <- split_stocnet_streams(
    x,
    map,
    focal = "calls",
    modeled_flavor = "creation"
  )

  expect_equal(nrow(streams$dependent), 1L)
  expect_equal(streams$dependent$flavor, "creation")
  expect_equal(
    sum(!is.na(streams$network$calls$time)),
    2L,
    label = "every timed focal row still updates state"
  )
})

test_that("the dependent stream is fetched in the walk's event shape", {
  src <- new_data_source(data = make_stocnet_fixture(), focal = "calls")

  events <- ds_fetch_stream(src, DEPENDENT_STREAM)

  expect_named(events, c("time", "sender", "receiver", "increment"))
  expect_equal(events$time, c(1, 2), label = "NA-time history is not an event")
})

test_that("the dependent stream takes the focal layer's update semantics", {
  x <- make_stocnet_fixture()
  x$info$update <- c(calls = "replace")
  src <- new_data_source(data = x, focal = "calls")

  expect_named(
    ds_fetch_stream(src, DEPENDENT_STREAM),
    c("time", "sender", "receiver", "replace")
  )
})

test_that("a stocnet focal layer is its own default network", {
  src <- new_data_source(data = make_stocnet_fixture(), focal = "calls")

  expect_identical(
    ds_default_network(src, "calls"),
    "calls",
    label = "the events and the ties they update are the same rows"
  )
})

test_that("a dependent name that is not a layer aborts listing the layers", {
  local_cli_context()
  src <- new_data_source(data = make_stocnet_fixture(), focal = "nope")

  expect_snapshot(error = TRUE, ds_check_dependent(src, "nope"))
})

test_that("a focal naming no layer leaves the dependent stream empty", {
  # Reporting an unknown focal belongs to the validator and the dependent
  # check, which name the candidates; splitting must not fail first.
  x <- make_stocnet_fixture()
  map <- build_mode_map(x$info, x$nodes, "calls")

  streams <- split_stocnet_streams(x, map, focal = "nope")

  expect_null(streams$dependent)
})

test_that("a valid dependent layer passes the check", {
  src <- new_data_source(data = make_stocnet_fixture(), focal = "calls")

  expect_identical(ds_check_dependent(src, "calls"), "calls")
})

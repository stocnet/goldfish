# With one data object a formula name is a layer, a `nodes` column, or a
# `global` variable, and only a lookup can tell which. These tests pin what the
# resolver rewrites each reference into, since every builder downstream reads
# the result rather than the name the user wrote.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# Resolve a one-sided formula's references and return them as "effect(ref)"
# strings, which is the whole observable output of the resolver.
resolved_refs <- function(f, src) {
  resolved <- resolve_formula_names(get_rhs_names(f), src)
  vapply(
    resolved,
    function(term) paste0(term[[1]], "(", term[[2]], ")"),
    character(1)
  )
}

fixture_with_global <- function() {
  x <- make_stocnet_fixture()
  x$global <- data.frame(time = c(NA, 5), var = "gdp")
  x$global$value <- list(10, 20)
  x
}

test_that("a bare attribute name resolves against the nodes component", {
  src <- new_data_source(data = make_stocnet_fixture())

  expect_equal(resolved_refs(y ~ ego(floor), src), "ego(nodes$floor)")
})

test_that("a layer name resolves to itself", {
  src <- new_data_source(data = make_stocnet_fixture())

  expect_equal(resolved_refs(y ~ inertia(calls), src), "inertia(calls)")
})

test_that("a global variable resolves to the global component", {
  src <- new_data_source(data = fixture_with_global())

  expect_equal(resolved_refs(y ~ global(gdp), src), "global(.global$gdp)")
})

test_that("a layer name wins over a same-named nodes column", {
  x <- make_stocnet_fixture()
  x$nodes$calls <- c(1, 2, 3)
  src <- new_data_source(data = x)

  expect_equal(
    resolved_refs(y ~ inertia(calls), src),
    "inertia(calls)",
    label = "the layer namespace is searched first"
  )
})

test_that("ego reads the sender side and alter the receiver side", {
  x <- make_stocnet_fixture_twomode()
  x$nodes$size <- c(1, 2, 3, 4)
  src <- new_data_source(data = x)

  expect_equal(resolved_refs(y ~ ego(size), src), "ego(nodes_side1$size)")
  expect_equal(resolved_refs(y ~ alter(size), src), "alter(nodes_side2$size)")
})

test_that("both sides name the one node set on a one-mode layer", {
  src <- new_data_source(data = make_stocnet_fixture())

  expect_equal(resolved_refs(y ~ ego(floor), src), "ego(nodes$floor)")
  expect_equal(resolved_refs(y ~ alter(floor), src), "alter(nodes$floor)")
})

test_that("every name inside a list() reference resolves", {
  x <- make_stocnet_fixture()
  x$nodes$rank <- c(3, 2, 1)
  src <- new_data_source(data = x)

  expect_equal(
    resolved_refs(y ~ ego_alter_interaction(list(floor, rank)), src),
    "ego_alter_interaction(list(nodes$floor, nodes$rank))"
  )
})

test_that("the legacy df$var prefix is dropped with a deprecation warning", {
  withr::local_options(lifecycle_verbosity = "warning")
  src <- new_data_source(data = make_stocnet_fixture())

  expect_snapshot(
    refs <- resolved_refs(y ~ ego(actors$floor), src)
  )
  expect_equal(
    refs,
    "ego(nodes$floor)",
    label = "the prefixed term behaves exactly as the bare one"
  )
})

test_that("the df$var deprecation warns once per session by default", {
  withr::local_options(lifecycle_verbosity = NULL)
  src <- new_data_source(data = make_stocnet_fixture())
  # `deprecate_warn()` is id-keyed, so a fresh id makes "once" observable
  # without depending on whether another test already tripped it.
  local_mocked_bindings(
    drop_data_frame_prefix = function(name, user_env = rlang::caller_env(2)) {
      lifecycle::deprecate_warn(
        when = "1.9.0",
        what = I("A prefix"),
        id = paste0("goldfish_prefix_test_", Sys.getpid())
      )
      strsplit(name, "$", fixed = TRUE)[[1]][2]
    }
  )

  expect_warning(resolved_refs(y ~ ego(actors$floor), src))
  expect_no_warning(resolved_refs(y ~ ego(actors$floor), src))
})

test_that("an unknown name aborts listing the available candidates", {
  local_cli_context()
  src <- new_data_source(data = fixture_with_global())

  expect_snapshot(
    error = TRUE,
    resolved_refs(y ~ inertia(callz), src)
  )
})

test_that("a name in both nodes and global aborts as ambiguous", {
  local_cli_context()
  x <- make_stocnet_fixture()
  x$global <- data.frame(time = NA, var = "floor")
  x$global$value <- list(9)
  src <- new_data_source(data = x)

  expect_snapshot(
    error = TRUE,
    resolved_refs(y ~ ego(floor), src)
  )
})

test_that("the legacy environment source is left untouched", {
  # The legacy path resolves names by get() in its environment, where `df$var`
  # is the syntax rather than a deprecated prefix.
  src <- new_data_source(envir = new.env())
  parsed <- get_rhs_names(y ~ ego(actors$floor) + inertia(calls))

  expect_identical(resolve_formula_names(parsed, src), parsed)
})

test_that("the resolver serves support_constraint atoms too", {
  src <- new_data_source(data = make_stocnet_fixture())
  atoms <- get_rhs_names(~ tie(calls) + ego(floor))

  expect_equal(
    vapply(
      resolve_formula_names(atoms, src),
      function(term) term[[2]],
      character(1)
    ),
    c("calls", "nodes$floor"),
    label = "atoms are plain effects and resolve identically"
  )
})

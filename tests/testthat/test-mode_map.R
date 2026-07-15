# The mode map translates stocnet's single global node space into the engine's
# per-layer local index spaces. The identical-or-disjoint contract itself is
# enforced by the validator (test-validate_goldfish.R); these tests cover what
# the map produces once an object is valid.

test_that("undeclared layer is one-mode over all nodes in a multimodal object", {
  x <- make_stocnet_fixture_multimode()
  map <- build_mode_map(x$info, x$nodes, "advice")
  lm <- map$layers[["advice"]]

  expect_false(lm$is_two_mode)
  expect_equal(lm$n1, 4L)
  expect_equal(lm$n2, 4L)
  expect_equal(lm$side1, 1:4, label = "every mode is in the node space")

  remapped <- remap_layer_refs(map, "advice", x$ties$from, x$ties$to, x$nodes)
  expect_equal(
    remapped,
    list(from = c(1L, 3L), to = c(3L, 2L)),
    label = "local index equals global id when the side spans all nodes"
  )
})

test_that("identical mode sets give a one-mode layer over the declared subset", {
  x <- make_stocnet_fixture_multimode()
  x$info$sender <- c("employee", "supervisor")
  x$info$receiver <- c("employee", "supervisor")
  expect_no_error(validate_goldfish_data(x))

  map <- build_mode_map(x$info, x$nodes, "advice")
  lm <- map$layers[["advice"]]

  expect_false(lm$is_two_mode)
  expect_equal(lm$side1, c(1L, 2L, 3L), label = "the outsider is excluded")
  expect_identical(lm$side1, lm$side2)
  expect_equal(c(lm$n1, lm$n2), c(3L, 3L))

  remapped <- remap_layer_refs(map, "advice", x$ties$from, x$ties$to, x$nodes)
  expect_equal(
    remapped,
    list(from = c(1L, 3L), to = c(3L, 2L)),
    label = "subset local indices, both sides sharing one space"
  )
})

test_that("disjoint mode sets give two-mode local index spaces", {
  x <- make_stocnet_fixture_twomode()
  map <- build_mode_map(x$info, x$nodes, "membership")
  lm <- map$layers[["membership"]]

  expect_true(lm$is_two_mode)
  expect_equal(lm$side1, c(1L, 2L))
  expect_equal(lm$side2, c(3L, 4L))
  expect_equal(c(lm$n1, lm$n2), c(2L, 2L))

  remapped <- remap_layer_refs(
    map,
    "membership",
    x$ties$from,
    x$ties$to,
    x$nodes
  )
  expect_equal(
    remapped,
    list(from = c(1L, 2L), to = c(1L, 2L)),
    label = "global ids 3 and 4 become receiver-side local 1 and 2"
  )
})

test_that("character labels remap like the equivalent integer ids", {
  x <- make_stocnet_fixture_twomode()
  map <- build_mode_map(x$info, x$nodes, "membership")

  by_label <- remap_layer_refs(
    map,
    "membership",
    c("A", "B"),
    c("X", "Y"),
    x$nodes
  )
  by_id <- remap_layer_refs(map, "membership", x$ties$from, x$ties$to, x$nodes)
  expect_equal(by_label, by_id)
})

test_that("layer_node_lookup resolves local indices back to node identity", {
  x <- make_stocnet_fixture_twomode()
  map <- build_mode_map(x$info, x$nodes, "membership")
  lookup <- layer_node_lookup(map, "membership")

  expect_equal(lookup$side, c(1L, 1L, 2L, 2L))
  expect_equal(lookup$local, c(1L, 2L, 1L, 2L))
  expect_equal(lookup$global, c(1L, 2L, 3L, 4L))
  expect_equal(lookup$label, c("A", "B", "X", "Y"))

  one_mode <- layer_node_lookup(
    build_mode_map(
      make_stocnet_fixture()$info,
      make_stocnet_fixture()$nodes,
      "calls"
    ),
    "calls"
  )
  expect_equal(
    one_mode$side,
    rep(1L, 3),
    label = "a one-mode layer has no second side"
  )
  expect_equal(one_mode$label, c("A", "B", "C"))
})

test_that("partial overlap and side impurity are rejected before remapping", {
  overlap <- make_stocnet_fixture_twomode()
  overlap$info$sender <- "p"
  overlap$info$receiver <- c("p", "o")
  expect_error(validate_goldfish_data(overlap), "partially overlap")

  impure <- make_stocnet_fixture_twomode()
  impure$ties$from <- c(1L, 3L)
  expect_error(validate_goldfish_data(impure), "side-pure")
})

# The mode map translates stocnet's single global node space into the engine's
# per-layer local index spaces. The identical-or-disjoint contract itself is
# enforced by the validator (test-validate_goldfish.R); these tests cover what
# the map produces once an object is valid.

test_that("an undeclared layer is one-mode over all nodes when multimodal", {
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

test_that("identical mode sets give one-mode over the declared subset", {
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

# Per-layer declarations ------------------------------------------------------

make_mixed_layers_stocnet <- function(sender, receiver) {
  x <- make_stocnet_fixture_multimode()
  x$ties <- rbind(
    x$ties,
    data.frame(from = 1L, to = 3L, time = 3, layer = "report")
  )
  x$info$update <- c(advice = "increment", report = "increment")
  x$info$directed <- c(advice = TRUE, report = TRUE)
  x$info$observation <- c(advice = "event", report = "event")
  x$info$sender <- sender
  x$info$receiver <- receiver
  x
}

test_that("one object mixes a one-mode and a two-mode layer", {
  x <- make_mixed_layers_stocnet(
    sender = list(advice = c("employee", "supervisor"), report = "employee"),
    receiver = list(advice = c("employee", "supervisor"), report = "supervisor")
  )
  map <- build_mode_map(x$info, x$nodes, c("advice", "report"))

  advice <- map$layers[["advice"]]
  expect_false(advice$is_two_mode)
  expect_equal(advice$side1, c(1L, 2L, 3L), label = "employees + supervisor")
  expect_identical(advice$side1, advice$side2)

  report <- map$layers[["report"]]
  expect_true(report$is_two_mode)
  expect_equal(report$side1, c(1L, 2L), label = "employees send")
  expect_equal(report$side2, 3L, label = "the supervisor receives")
  expect_equal(c(report$n1, report$n2), c(2L, 1L))
})

test_that("both declaration encodings produce identical mode maps", {
  as_list <- make_mixed_layers_stocnet(
    sender = list(advice = c("employee", "supervisor"), report = "employee"),
    receiver = list(advice = c("employee", "supervisor"), report = "supervisor")
  )
  # The repeated-name character vector is the shape manynet's validate_info()
  # admits; the list is the readable equivalent.
  as_vector <- make_mixed_layers_stocnet(
    sender = c(
      advice = "employee",
      advice = "supervisor",
      report = "employee"
    ),
    receiver = c(
      advice = "employee",
      advice = "supervisor",
      report = "supervisor"
    )
  )

  expect_equal(
    build_mode_map(as_vector$info, as_vector$nodes, c("advice", "report")),
    build_mode_map(as_list$info, as_list$nodes, c("advice", "report"))
  )
  expect_no_error(validate_goldfish_data(as_vector))
})

test_that("an unnamed declaration applies to every layer", {
  x <- make_mixed_layers_stocnet(
    sender = c("employee", "supervisor"),
    receiver = c("employee", "supervisor")
  )
  map <- build_mode_map(x$info, x$nodes, c("advice", "report"))

  expect_false(map$layers[["advice"]]$is_two_mode)
  expect_false(map$layers[["report"]]$is_two_mode)
  expect_equal(map$layers[["report"]]$side1, c(1L, 2L, 3L))
})

test_that("a layer with no declaration stays one-mode over all nodes", {
  x <- make_mixed_layers_stocnet(
    sender = list(report = "employee"),
    receiver = list(report = "supervisor")
  )
  map <- build_mode_map(x$info, x$nodes, c("advice", "report"))

  expect_false(map$layers[["advice"]]$is_two_mode)
  expect_equal(
    map$layers[["advice"]]$side1,
    1:4,
    label = "an undeclared layer spans every mode, outsider included"
  )
  expect_true(map$layers[["report"]]$is_two_mode)
})

test_that("normalize_mode_sets recovers the same sets from either encoding", {
  layers <- c("advice", "report")
  expect_equal(
    normalize_mode_sets(
      c(advice = "employee", advice = "supervisor", report = "employee"),
      layers
    ),
    list(advice = c("employee", "supervisor"), report = "employee")
  )
  expect_equal(
    normalize_mode_sets(list(advice = c("employee", "supervisor")), layers),
    list(advice = c("employee", "supervisor"))
  )
  expect_equal(
    normalize_mode_sets(c("employee", "supervisor"), layers),
    list(
      advice = c("employee", "supervisor"),
      report = c("employee", "supervisor")
    ),
    label = "unnamed applies to every layer"
  )
  expect_null(normalize_mode_sets(NULL, layers))
  expect_null(normalize_mode_sets(character(0), layers))
})

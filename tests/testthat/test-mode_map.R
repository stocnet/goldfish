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
  # Repeated names give `advice` two modes and `report` one per side.
  x <- make_mixed_layers_stocnet(
    sender = c(advice = "employee", advice = "supervisor", report = "employee"),
    receiver = c(
      advice = "employee",
      advice = "supervisor",
      report = "supervisor"
    )
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

test_that("a list declaration is rejected with the vector form", {
  x <- make_mixed_layers_stocnet(
    sender = list(advice = c("employee", "supervisor"), report = "employee"),
    receiver = list(advice = c("employee", "supervisor"), report = "supervisor")
  )
  # manynet type-checks these entries as character and add_info() does not
  # validate, so a list would pass where it is written and abort later inside
  # bind_changes(). Fail here instead, where the fix is obvious.
  expect_error(
    validate_goldfish_data(x),
    "must be a character vector, not a list"
  )
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
    sender = c(report = "employee"),
    receiver = c(report = "supervisor")
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

test_that("a multipartite object resolves each layer's own mode pair", {
  x <- make_stocnet_fixture_multipartite()
  expect_no_error(validate_goldfish_data(x))

  layers <- c("attend", "coauthor", "member")
  map <- build_mode_map(x$info, x$nodes, layers)

  attend <- map$layers[["attend"]]
  expect_true(attend$is_two_mode)
  expect_equal(attend$side1, 1:3, label = "actors send")
  expect_equal(attend$side2, 4:5, label = "events receive")

  coauthor <- map$layers[["coauthor"]]
  expect_false(coauthor$is_two_mode)
  expect_identical(coauthor$side1, coauthor$side2)
  expect_equal(coauthor$side1, 1:3)

  member <- map$layers[["member"]]
  expect_true(member$is_two_mode)
  expect_equal(member$side1, 1:3)
  expect_equal(member$side2, 6:7, label = "orgs receive")

  # A covariate layer keeps its own local space rather than the focal pair's:
  # `member`'s org receivers are 6 and 7 globally, local 1 and 2 on its side 2.
  expect_equal(
    remap_layer_refs(map, "member", c(1L, 3L), c(6L, 7L), x$nodes),
    list(from = c(1L, 3L), to = c(1L, 2L))
  )
})

test_that("the legacy two node-set fixture is a two-mode legacy bundle", {
  fx <- make_legacy_fixture_twomode()

  expect_s3_class(fx$membership, "network.goldfish")
  expect_equal(dim(fx$membership), c(4L, 3L))
  expect_true(attr(fx$membership, "is_two_mode"))
  expect_equal(attr(fx$membership, "nodes"), c("actors", "clubs"))
  expect_equal(attr(fx$joins_dependent, "nodes"), c("actors", "clubs"))
})

test_that("normalize_mode_sets recovers the per-layer sets", {
  layers <- c("advice", "report")
  expect_equal(
    normalize_mode_sets(
      c(advice = "employee", advice = "supervisor", report = "employee"),
      layers
    ),
    list(advice = c("employee", "supervisor"), report = "employee")
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

test_that("the vector declaration survives the manynet assembly workflow", {
  # The reason the vector form is the only one accepted: validate_stocnet() runs
  # in make_stocnet() and bind_changes() but NOT in add_info(), so an encoding
  # manynet rejects would pass where it is written and abort later. Pin the
  # whole path rather than validate_info() alone.
  skip_if_not_installed("manynet")
  nodes <- data.frame(
    name = c("E1", "E2", "S1"),
    mode = c("employee", "employee", "supervisor"),
    gdp = c(1, 2, 3)
  )
  ties <- data.frame(
    from = c(1L, 1L),
    to = c(2L, 3L),
    layer = c("advice", "report")
  )
  changes <- data.frame(time = 1, node = 1L, var = "gdp")
  changes$value <- list(list(2))

  x <- manynet::make_stocnet(nodes = nodes, ties = ties)
  x <- manynet::add_info(
    x,
    sender = c(advice = "employee", advice = "supervisor", report = "employee"),
    receiver = c(
      advice = "employee",
      advice = "supervisor",
      report = "supervisor"
    )
  )
  expect_no_error(
    manynet::bind_changes(x, changes),
    message = "bind_changes() re-validates; a list would abort here"
  )
  expect_equal(
    x$info$sender,
    c(advice = "employee", advice = "supervisor", report = "employee"),
    label = "add_info() carries the repeated names through untouched"
  )
  expect_equal(
    normalize_mode_sets(x$info$sender, c("advice", "report")),
    list(advice = c("employee", "supervisor"), report = "employee"),
    label = "the sets goldfish maps survive the round trip"
  )
})

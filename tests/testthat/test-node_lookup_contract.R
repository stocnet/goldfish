# `node_lookup` is *the* resolver for a per-event diagnostic index: the fit
# carries one (side, local, global, label) table, and a position in a per-event
# component is joined to the side `risk_set_axis()` names. The alternative --
# attaching names()/dimnames() to every per-event component -- would repeat that
# one table once per event, so the storage property is part of the contract and
# is pinned here. `test-node_lookup_export.R` covers the same table on the
# gather / db export surface; this file covers the fitted object.

# Recover the labels a side's local indices point at, through the lookup alone.
lookup_labels <- function(lookup, index, side) {
  rows <- lookup[lookup$side == side, , drop = FALSE]
  rows$label[match(index, rows$local)]
}

test_that("a per-event index resolves to one node row and label", {
  data("social_evolution", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  lookup <- fit$node_lookup
  expect_named(lookup, c("side", "local", "global", "label"))
  # A one-mode choice model: both the sender and the receiver axis draw on the
  # same node set, so the lookup carries side 1 only and every position on the
  # declared axis resolves against it.
  expect_identical(risk_set_axis(fit), "receiver_given_sender")
  expect_setequal(lookup$side, 1L)
  positions <- seq_along(fit$event_probabilities[[1]])
  expect_false(anyNA(lookup_labels(lookup, positions, 1L)))
  # Exactly one row per local index -- a join, not a many-to-one collapse.
  expect_equal(anyDuplicated(lookup$local[lookup$side == 1L]), 0L)
})

test_that("both sides resolve on a two-mode fit", {
  skip_on_cran()
  # The frozen irps_nuclear subset rather than the toy two-mode fixture: the
  # latter is collinear under a full choice fit, and this scenario needs a
  # genuinely fitted object, not a preprocessed one.
  data <- as_goldfish(readRDS(test_path("fixtures", "irps_nuclear_subset.rds")))
  fit <- estimate_dynam(
    make_specification(
      choice = list(modeled ~ indeg(support) + indeg(contestation)),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = data
    )
  )

  lookup <- fit$node_lookup
  expect_named(lookup, c("side", "local", "global", "label"))
  expect_setequal(lookup$side, c(1L, 2L))
  side1 <- lookup[lookup$side == 1L, ]
  side2 <- lookup[lookup$side == 2L, ]
  # Each side keeps its own 1-based local index space while `global` stays the
  # row of the single nodes frame -- which is why the raw frame cannot do this
  # join on a two-mode model, and the lookup can.
  expect_equal(side1$local, seq_len(nrow(side1)))
  expect_equal(side2$local, seq_len(nrow(side2)))
  expect_false(identical(side2$local, side2$global))
  expect_equal(side1$label, data$nodes$label[side1$global])
  expect_equal(side2$label, data$nodes$label[side2$global])
  # A receiver position on the choice axis resolves on side 2, independently.
  positions <- seq_along(fit$event_probabilities[[1]])
  expect_false(anyNA(lookup_labels(lookup, positions, 2L)))
})

test_that("a node-subset fit resolves local back to the original global row", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # "D" is off the modeled side, so local runs 1..3 while global skips its row.
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = c("p", "p", "p", "o"),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 3L, 1L, 2L, 3L),
    to = c(2L, 3L, 1L, 3L, 1L, 2L),
    time = seq_len(6),
    layer = "calls",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "sub",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event"),
    sender = c(calls = "p"),
    receiver = c(calls = "p")
  )
  prep <- compute_statistics(
    calls ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    output = "preprocessed",
    data = list(info = info, nodes = nodes, ties = ties)
  )

  lookup <- prep$node_lookup
  expect_equal(lookup$local, 1:3)
  expect_equal(lookup$global, 1:3)
  expect_equal(lookup$label, nodes$label[lookup$global])
  expect_false("D" %in% lookup$label)
})

test_that("per-event components carry no per-event copy of the node labels", {
  data("social_evolution", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  # The storage property the resolution contract rests on. If someone
  # "helpfully" adds names() here, the one-table-per-fit mapping becomes one
  # copy per event and this fails.
  expect_null(names(fit$event_probabilities[[1]]))
  expect_null(names(fit$event_probabilities[[length(fit$event_probabilities)]]))
  expect_null(rownames(fit$event_scores))
})

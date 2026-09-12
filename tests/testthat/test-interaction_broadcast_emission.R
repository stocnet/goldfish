# An interaction product is maintained at its own broadcast kind. When every
# operand varies on a single axis, the product does too, so an alter-by-alter
# product rides the broadcast stream (one entry per fixed receiver) exactly as a
# plain alter effect does, rather than fanning out to one point cell per sender.
# A product that genuinely varies on both axes still emits point cells.
#
# Fixtures live in helper-parity-fixtures.R. `parity_toy_data()` moves both `a`
# and `b`, so an `alter(a):alter(b)` product changes over the sequence and its
# deltas actually reach a stream.

# The 0-based effect index of the interaction column: `augment_interactions()`
# appends the product after the function-effect columns, so with a single
# interaction it is the last column.
product_effect_index <- function(prep) {
  parity_n_effects(prep) - 1L
}

point_effects <- function(prep) {
  if (ncol(prep$stat_mat_update)) prep$stat_mat_update[3, ] else integer(0)
}

broadcast_effects <- function(prep) {
  if (ncol(prep$stat_mat_broadcast)) {
    prep$stat_mat_broadcast[3, ]
  } else {
    integer(0)
  }
}

test_that("an alter-by-alter product emits broadcast entries, not points", {
  data <- parity_toy_data()
  spec <- make_specification(
    choice = ~ inertia + alter(a):alter(b),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- suppressMessages(suppressWarnings(
    compute_statistics(spec, "DyNAM", "choice")
  ))
  product <- product_effect_index(prep)

  # The product moves over the sequence, so it must reach some stream at all.
  expect_true(product %in% c(point_effects(prep), broadcast_effects(prep)))

  # An alter-kind product belongs in the broadcast stream, like a plain alter
  # effect: this fails on the unfixed tree, where the product fans out to one
  # point cell per sender for every touched receiver.
  expect_true(product %in% broadcast_effects(prep))
  expect_false(product %in% point_effects(prep))
})

test_that("a genuinely dyadic product still emits point cells", {
  data <- parity_toy_data()
  # ego(a) varies on the sender axis, alter(b) on the receiver axis, so their
  # product varies on both and cannot be broadcast.
  spec <- make_specification(
    choice = ~ inertia + ego(a):alter(b),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- suppressMessages(suppressWarnings(
    compute_statistics(spec, "DyNAM", "choice")
  ))
  product <- product_effect_index(prep)

  expect_true(product %in% point_effects(prep))
  expect_false(product %in% broadcast_effects(prep))
})

test_that("the broadcast product agrees on both substrates", {
  data <- parity_toy_data()
  spec <- make_specification(
    choice = ~ inertia + alter(a):alter(b),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  merged <- suppressMessages(suppressWarnings(
    preprocess_joint(single_process_joint(spec))
  ))
  prep <- parity_prep_by(merged, "choice")
  product <- product_effect_index(prep)

  expect_true(product %in% broadcast_effects(prep))
  expect_false(product %in% point_effects(prep))
})

test_that("changing the emission kind leaves the product value unchanged", {
  # The value invariant the frozen baselines also guard: whatever stream carries
  # it, the product column equals the elementwise product of its operands at
  # every event.
  data <- parity_toy_data()
  spec <- make_specification(
    choice = ~ inertia + alter(a):alter(b),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  gathered <- suppressMessages(suppressWarnings(
    compute_statistics(spec, "DyNAM", "choice", output = "gather")
  ))
  m <- gathered$stat_all_events
  # Columns: inertia (1), alter(a) (2), alter(b) (3), product (4).
  expect_equal(ncol(m), 4L)
  expect_equal(m[, 4], m[, 2] * m[, 3], tolerance = 1e-6)
})

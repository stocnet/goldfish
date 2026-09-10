# Interaction operands are stored at their own broadcast kind on the dyad
# branch, as they already were on the sender branch. The parity toy
# specification carries one operand of every kind -- `ego(a):alter(b)` crosses
# the two axis kinds and `global(x):inertia` pairs a scalar with a genuinely
# dyadic operand -- so one preprocessing exercises all four.
#
# Byte-identity of the product and its update stream is carried by
# `test-preprocess_parity.R` (recipe loop against merged walk) and by the frozen
# coefficient baselines; what is asserted here is the storage those two cannot
# see.

# Record the shape of every buffer the walk writes, so storage-at-kind is
# observed directly rather than inferred from an allocation figure.
written_buffer_shapes <- function(expr) {
  shapes <- new.env(parent = emptyenv())
  shapes$seen <- list()
  original <- write_entries
  local_mocked_bindings(
    write_entries = function(buffer, entries, values) {
      shapes$seen <- c(
        shapes$seen,
        list(list(dim = dim(buffer), length = length(buffer)))
      )
      original(buffer, entries, values)
    }
  )
  force(expr)
  shapes$seen
}

test_that("a dyad-branch operand is stored at its kind, not as a dense grid", {
  data <- parity_toy_data()
  spec <- make_specification(
    choice = parity_toy_spec(data)$choice %||%
      ~ ego(a):alter(b) + global(x):inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  n <- 5L
  shapes <- suppressMessages(suppressWarnings(
    written_buffer_shapes(compute_statistics(spec, "DyNAM", "choice"))
  ))
  skip_if(length(shapes) == 0L, "no operand write reached the walk")

  lengths_seen <- vapply(shapes, function(s) s$length, integer(1))
  is_grid <- vapply(shapes, function(s) identical(s$dim, c(n, n)), logical(1))

  # Under the old dense storage every operand buffer was n x n, so a single
  # non-grid buffer is the whole claim.
  expect_true(any(!is_grid))
  expect_true(any(lengths_seen %in% c(1L, n)))
  # A genuinely dyadic operand still stores dense.
  expect_true(any(is_grid))
  # Nothing is stored at a size that is neither a kind nor the grid.
  expect_true(all(lengths_seen %in% c(1L, n, n * n)))
})

test_that("an interaction column is the product of its operands at seeding", {
  # The seeding path widens each kind-shaped operand back to point exactly once,
  # so the interaction's initial slice must still equal the elementwise product
  # of its operands' initial slices. `augment_interactions()` appends one
  # product column per interaction after the operands, so for
  # `ego(a):alter(b) + global(x):inertia` the six columns are the four operands
  # in formula order and then the two products.
  data <- parity_toy_data()
  spec <- make_specification(
    choice = ~ ego(a):alter(b) + global(x):inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  prep <- suppressMessages(suppressWarnings(
    compute_statistics(spec, "DyNAM", "choice")
  ))
  expect_identical(dim(prep$initial_stats)[[3L]], 6L)
  expect_equal(
    prep$initial_stats[,, 5L],
    prep$initial_stats[,, 1L] * prep$initial_stats[,, 2L]
  )
  expect_equal(
    prep$initial_stats[,, 6L],
    prep$initial_stats[,, 3L] * prep$initial_stats[,, 4L]
  )
  # And the product carries the diagonal rule its operands carry: a kind-shaped
  # ego operand holds a[i] at every entry, so without the rule the self-dyad
  # would come back as a[i] * b[i] instead of zero.
  expect_true(all(diag(prep$initial_stats[,, 5L]) == 0))
})

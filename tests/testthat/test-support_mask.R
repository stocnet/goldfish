# Tests for the support_constraint active-mask assembly (support_mask.R):
# active_1 (x) support (x) active_2 per model, storage-kind broadcasting, and
# the symmetric coordination / undirected-REM rule. Pure functions, tested on
# small fixtures without the recipe loop.

test_that("support tree evaluates to a mask at its atoms' shape", {
  # one point atom (n1 x n2 matrix) -> matrix mask
  m <- matrix(c(0, 2, 0, 5), 2, 2)
  out <- assemble_support_mask(list(.a1 = m), quote(.a1 != 0))
  expect_identical(out, matrix(c(FALSE, TRUE, FALSE, TRUE), 2, 2))

  # two atoms, boolean combination
  a <- matrix(c(1, 1, 0, 0), 2, 2)
  b <- matrix(c(0, 1, 0, 1), 2, 2)
  out2 <- assemble_support_mask(
    list(.a1 = a, .a2 = b),
    quote(.a1 != 0 & !(.a2 != 0))
  )
  expect_identical(out2, matrix(c(TRUE, FALSE, FALSE, FALSE), 2, 2))
})

test_that("support_to_grid broadcasts per storage kind", {
  # ego (kind 2): sender axis -> constant down columns
  expect_identical(
    support_to_grid(c(TRUE, FALSE), 2L, 2, 3),
    matrix(c(TRUE, FALSE), 2, 3)
  )
  # alter (kind 1): receiver axis -> constant across rows
  expect_identical(
    support_to_grid(c(TRUE, FALSE, TRUE), 1L, 2, 3),
    matrix(c(TRUE, FALSE, TRUE), 2, 3, byrow = TRUE)
  )
  # global (kind 3): scalar everywhere
  expect_identical(support_to_grid(TRUE, 3L, 2, 2), matrix(TRUE, 2, 2))
  # point (kind 0): already the grid
  pm <- matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2)
  expect_identical(support_to_grid(pm, 0L, 2, 2), pm)
  # NULL support -> all allowed
  expect_identical(support_to_grid(NULL, 0L, 2, 2), matrix(TRUE, 2, 2))
})

test_that("REM full mask is active_1 x support x active_2", {
  support <- matrix(
    c(
      TRUE,
      TRUE,
      FALSE,
      TRUE
    ),
    2,
    2,
    byrow = TRUE
  )
  active_1 <- c(TRUE, TRUE)
  active_2 <- c(TRUE, FALSE) # receiver 2 absent
  mask <- assemble_model_mask(support, active_1, active_2, 0L, "REM")
  # column 2 zeroed by absent receiver; (2,1) zeroed by support
  expect_identical(
    mask,
    matrix(c(TRUE, FALSE, FALSE, FALSE), 2, 2)
  )
})

test_that("absent sender/receiver is excluded regardless of the constraint", {
  support <- matrix(TRUE, 2, 2)
  mask <- assemble_model_mask(support, c(FALSE, TRUE), c(TRUE, TRUE), 0L, "REM")
  # sender 1 absent -> its whole row is FALSE even though support allows it
  expect_identical(mask[1, ], c(FALSE, FALSE))
  expect_identical(mask[2, ], c(TRUE, TRUE))
})

test_that("choice candidate matrix filters receivers per sender", {
  support <- matrix(
    c(
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE
    ),
    2,
    3,
    byrow = TRUE
  )
  mask <- assemble_model_mask(
    support,
    c(TRUE, TRUE),
    c(TRUE, TRUE, TRUE),
    0L,
    "choice"
  )
  expect_identical(mask[1, ], c(TRUE, FALSE, TRUE))
  expect_identical(mask[2, ], c(TRUE, TRUE, FALSE))
})

test_that("rate gate keeps senders with at least one allowed receiver", {
  support <- matrix(
    c(
      FALSE,
      FALSE, # sender 1: no allowed receiver -> gated out
      TRUE,
      FALSE # sender 2: one allowed
    ),
    2,
    2,
    byrow = TRUE
  )
  gate <- assemble_model_mask(
    support,
    c(TRUE, TRUE),
    c(TRUE, TRUE),
    0L,
    "rate"
  )
  expect_identical(gate, c(FALSE, TRUE))
})

test_that("rate gate respects receiver presence in the allowed set", {
  # sender 1 allows only receiver 2, but receiver 2 is absent -> gated out
  support <- matrix(
    c(
      FALSE,
      TRUE,
      TRUE,
      TRUE
    ),
    2,
    2,
    byrow = TRUE
  )
  gate <- assemble_model_mask(
    support,
    c(TRUE, TRUE),
    c(TRUE, FALSE),
    0L,
    "rate"
  )
  expect_identical(gate, c(FALSE, TRUE))
})

test_that("coordination / undirected mask is symmetric", {
  # support allows (1,2) but not (2,1): the mutual dyad must be excluded
  support <- matrix(
    c(
      TRUE,
      TRUE,
      FALSE,
      TRUE
    ),
    2,
    2,
    byrow = TRUE
  )
  mask <- assemble_model_mask(
    support,
    c(TRUE, TRUE),
    c(TRUE, TRUE),
    0L,
    "choice",
    symmetric = TRUE
  )
  expect_identical(mask[1, 2], FALSE)
  expect_identical(mask[2, 1], FALSE)
  expect_true(isSymmetric(mask))
})

test_that("no constraint degenerates to the separable presence product", {
  mask <- assemble_model_mask(NULL, c(TRUE, FALSE), c(TRUE, TRUE), 0L, "REM")
  expect_identical(mask, outer(c(TRUE, FALSE), c(TRUE, TRUE), "&"))
})

test_that("sender-axis (ego) constraint needs no dense support input", {
  # ego support as a length-n1 vector; gate is active_1 & support (receivers all
  # present), no n1 x n2 support matrix supplied.
  gate <- assemble_model_mask(
    c(TRUE, FALSE),
    c(TRUE, TRUE),
    c(TRUE, TRUE),
    2L,
    "rate"
  )
  expect_identical(gate, c(TRUE, FALSE))
})

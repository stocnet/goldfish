# End-to-end two-mode estimation across the well-defined DyNAM/REM sub-models,
# each with a time-varying nodal covariate on its own mode, plus the export
# lookup that resolves each side's local indices back to node labels.
# Coordination (DyNAM-MM) is the one sub-model rejected on two-mode data: it
# reads both directed dyads over a single node set, which a disjoint side pair
# cannot supply.

test_that("a two-mode rate model estimates with a sender-side covariate", {
  # `ego(x)` reads the sender's own value, updated mid-stream (P2 at t = 4.5).
  fit <- estimate_dynam(
    membership ~ 1 + ego(x),
    sub_model = "rate",
    data = as_goldfish(make_stocnet_fixture_twomode_estimable())
  )
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))
})

test_that("a two-mode choice model estimates with a receiver-side covariate", {
  # `alter(y)` reads the receiver's own value, updated mid-stream (O1 at t = 6.5).
  fit <- estimate_dynam(
    membership ~ inertia + alter(y),
    sub_model = "choice",
    data = as_goldfish(make_stocnet_fixture_twomode_estimable())
  )
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))
})

test_that("a two-mode REM estimates", {
  fit <- estimate_rem(
    membership ~ 1 + inertia,
    data = as_goldfish(make_stocnet_fixture_twomode_estimable())
  )
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))
})

test_that("two-mode coordination is rejected, naming the sub-model and layer", {
  expect_error(
    estimate_dynam(
      membership ~ inertia,
      sub_model = "choice_coordination",
      data = as_goldfish(make_stocnet_fixture_twomode_estimable())
    ),
    "choice_coordination.*cannot run on a two-mode layer"
  )
})

test_that("the two-mode export lookup joins each side's indices to labels", {
  x <- make_stocnet_fixture_twomode_estimable()
  out <- gather_model_data(
    membership ~ inertia + alter(y),
    model = "DyNAM",
    sub_model = "choice",
    data = x
  )

  lookup <- out$node_lookup
  expect_setequal(lookup$side, c(1L, 2L))
  # global indexes the original nodes tibble; local runs 1..n per side.
  expect_equal(lookup$label, x$nodes$label[lookup$global])
  side1 <- lookup[lookup$side == 1L, ]
  side2 <- lookup[lookup$side == 2L, ]
  expect_equal(side1$label, c("P1", "P2", "P3", "P4"))
  expect_equal(side2$label, c("O1", "O2", "O3"))

  # index_i decodes to a sender label, index_j to a receiver label.
  join_side <- function(index, side) {
    rows <- lookup[lookup$side == side, , drop = FALSE]
    rows$label[match(index, rows$local)]
  }
  expect_false(anyNA(join_side(out$index_i, 1L)))
  expect_false(anyNA(join_side(out$index_j, 2L)))
})

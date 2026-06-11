test_that("constructors return the documented class vectors", {
  expect_identical(
    class(dynam_rate_spec(nodes = "actors")),
    c("dynam_rate_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynam_rate_ordered_spec(nodes = "actors")),
    c("dynam_rate_ordered_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynam_choice_spec(nodes = "actors")),
    c("dynam_choice_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(dynam_choice_coord_spec(nodes = "actors")),
    c("dynam_choice_coord_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(dynami_rate_spec(nodes = "actors")),
    c("dynami_rate_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynami_rate_ordered_spec(nodes = "actors")),
    c("dynami_rate_ordered_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynami_choice_spec(nodes = "actors")),
    c("dynami_choice_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(rem_rate_spec(nodes = "actors")),
    c("rem_rate_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(rem_rate_ordered_spec(nodes = "actors")),
    c("rem_rate_ordered_spec", "dyad_spec", "model_spec")
  )
})

test_that("sender constructors are one-mode with nodes2 defaulting to nodes", {
  spec <- dynam_rate_spec(nodes = "actors")
  expect_false(spec$is_two_mode)
  expect_identical(spec$nodes2, "actors")
})

test_that("constructors store extra fields passed through dots", {
  spec <- rem_rate_spec(nodes = "actors", has_intercept = TRUE)
  expect_true(spec$has_intercept)
})

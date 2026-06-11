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

test_that("new_model_spec resolves the spec class from model and sub_model", {
  spec <- new_model_spec("DyNAM", "rate", nodes = "actors")
  expect_identical(class(spec)[1], "dynam_rate_spec")
})

test_that("new_model_spec sender-indexed spec is not dyad-indexed", {
  spec <- new_model_spec("DyNAM", "rate_ordered", nodes = "actors")
  expect_true(inherits(spec, "sender_spec"))
  expect_false(inherits(spec, "dyad_spec"))
})

test_that("new_model_spec dyad-indexed spec is not sender-indexed", {
  spec <- new_model_spec("REM", "rate", nodes = "actors")
  expect_true(inherits(spec, "dyad_spec"))
  expect_false(inherits(spec, "sender_spec"))
})

test_that("new_model_spec accepts all 9 valid variant combinations", {
  combinations <- list(
    c("DyNAM", "rate"), c("DyNAM", "rate_ordered"), c("DyNAM", "choice"),
    c("DyNAM", "choice_coordination"), c("DyNAMi", "rate"),
    c("DyNAMi", "rate_ordered"), c("DyNAMi", "choice"),
    c("REM", "rate"), c("REM", "rate_ordered")
  )
  for (combination in combinations) {
    spec <- new_model_spec(combination[1], combination[2], nodes = "actors")
    expect_s3_class(spec, "model_spec")
  }
})

test_that("new_model_spec rejects invalid model and sub_model values", {
  expect_error(
    new_model_spec("SAOM", "rate", nodes = "actors"),
    "not a valid model"
  )
  expect_error(
    new_model_spec("REM", "choice_coordination", nodes = "actors"),
    "not a valid sub model"
  )
})

test_that("new_model_spec two-mode requires both node sets", {
  expect_error(
    new_model_spec(
      "DyNAM", "choice",
      is_two_mode = TRUE, nodes = "actors", nodes2 = NULL
    ),
    "nodes2"
  )
  expect_error(
    new_model_spec(
      "REM", "rate",
      is_two_mode = TRUE, nodes = "actors", nodes2 = "actors"
    ),
    "distinct node sets"
  )
  spec <- new_model_spec(
    "DyNAM", "choice",
    is_two_mode = TRUE, nodes = "actors", nodes2 = "clubs"
  )
  expect_true(spec$is_two_mode)
  expect_identical(spec$nodes2, "clubs")
})

test_that("new_model_spec sender-indexed specs ignore is_two_mode", {
  expect_no_warning(
    spec <- new_model_spec(
      "DyNAM", "rate",
      is_two_mode = TRUE, nodes = "actors", nodes2 = "clubs"
    )
  )
  expect_false(spec$is_two_mode)
  expect_identical(spec$nodes2, "actors")
})

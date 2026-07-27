# The fixed-coefficient contract. `assemble_fixed_parameters()` is the single
# point where term names and coefficient positions are both known, so it is the
# single producer of the contract every estimation path consumes: `idx`
# (positions into the coefficient vector [intercept?, effects...,
# interactions...]), `values`, and `names` (the term label each position came
# from, so an error can name the term).

fixed_spec_envir <- function() {
  envir <- new.env()
  assign("actors", actors, envir = envir)
  assign("calls", calls, envir = envir)
  base::local(
    {
      call_network <- structure(
        matrix(
          0,
          nrow(actors),
          nrow(actors),
          dimnames = list(actors$label, actors$label)
        ),
        class = c("network.goldfish", "matrix", "array"),
        nodes = c("actors", "actors"),
        directed = TRUE,
        events = c("calls")
      )
      calls_dependent <- structure(
        calls,
        class = c("dependent.goldfish", "data.frame"),
        nodes = c("actors", "actors"),
        events = c("calls"),
        default_network = "call_network",
        type = "dyadic"
      )
    },
    envir = envir
  )
  envir
}

assemble_from_formula <- function(
  formula,
  model = "DyNAM",
  sub_model = "choice",
  fixed_parameters = NULL,
  offset_coef = NULL
) {
  parsed <- parse_formula(formula, envir = fixed_spec_envir())
  assemble_fixed_parameters(
    parsed,
    parsed$rhs_names,
    parsed$has_intercept,
    model,
    sub_model,
    fixed_parameters,
    offset_coef
  )
}

test_that("nothing fixed yields NULL, not an empty contract", {
  expect_null(assemble_from_formula(calls_dependent ~ inertia + recip))
})

test_that("an offset term fixes its coefficient by position", {
  spec <- assemble_from_formula(
    calls_dependent ~ inertia + offset(recip) + trans,
    offset_coef = 2
  )
  expect_s3_class(spec, "fixed_spec")
  expect_identical(spec$idx, 2L)
  expect_identical(spec$values, 2)
  expect_identical(spec$names, "recip(call_network)")
})

test_that("the intercept shifts every fixed position by one", {
  spec <- assemble_from_formula(
    calls_dependent ~ 1 + offset(indeg) + outdeg,
    sub_model = "rate",
    offset_coef = 0.5
  )
  expect_identical(spec$idx, 2L)
  expect_identical(spec$values, 0.5)
  expect_identical(spec$names, "indeg(call_network)")
})

test_that("multiple offsets align to offset_coef by formula order", {
  spec <- assemble_from_formula(
    calls_dependent ~ inertia + offset(recip) + offset(trans),
    offset_coef = c(2, -1)
  )
  expect_identical(spec$idx, c(2L, 3L))
  expect_identical(spec$values, c(2, -1))
})

test_that("operand-only interaction terms are fixed at zero", {
  # `recip:trans` without either operand as a main effect: both columns are kept
  # in the design but held out of estimation; the interaction column is
  # estimated, so it is absent from the contract.
  spec <- assemble_from_formula(calls_dependent ~ inertia + recip:trans)
  expect_identical(spec$idx, c(2L, 3L))
  expect_identical(spec$values, c(0, 0))
  expect_identical(spec$names, c("recip(call_network)", "trans(call_network)"))
})

test_that("the superseded positional vector is converted to a contract", {
  spec <- assemble_from_formula(
    calls_dependent ~ inertia + recip + trans,
    fixed_parameters = c(NA, 2, NA)
  )
  expect_identical(spec$idx, 2L)
  expect_identical(spec$values, 2)
  expect_identical(spec$names, "recip(call_network)")
})

test_that("an all-fixed model yields a contract covering every coefficient", {
  spec <- assemble_from_formula(
    calls_dependent ~ inertia + recip + trans,
    fixed_parameters = c(1, 2, 3)
  )
  expect_identical(spec$idx, 1:3)
  expect_identical(spec$values, c(1, 2, 3))
  # likelihood-only is a derivation on the contract, never an encoding of it
  expect_length(spec$idx, 3L)
})

test_that("fixed-coefficient errors name the offending term", {
  expect_snapshot(
    error = TRUE,
    assemble_from_formula(
      calls_dependent ~ inertia + offset(recip),
      offset_coef = c(1, 2)
    )
  )
  expect_snapshot(
    error = TRUE,
    assemble_from_formula(
      calls_dependent ~ inertia + recip,
      fixed_parameters = c(NA, 2, NA)
    )
  )
})

test_that("the contract constructor rejects malformed input", {
  expect_error(
    new_fixed_spec(c(2L, 2L), c(1, 2), c("recip", "recip")),
    "cannot be fixed twice"
  )
  expect_error(
    new_fixed_spec(2L, NA_real_, "recip"),
    "needs a value"
  )
  expect_error(new_fixed_spec(integer(0), numeric(0), character(0)))
})

test_that("a contract flattens to the positional encoding", {
  spec <- new_fixed_spec(c(1L, 3L), c(0.5, -1), c("Intercept", "trans"))
  expect_identical(fixed_spec_to_vector(spec, 4L), c(0.5, NA, -1, NA))
  expect_null(fixed_spec_to_vector(NULL, 4L))
})

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
    offset_coef,
    coef_labels = term_label(
      GetDetailPrint(get_objects_effects_link(parsed$rhs_names), parsed),
      ".coef_name",
      "coef"
    )
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

test_that("an offset term carries its own value in the formula", {
  parsed <- parse_formula(
    calls_dependent ~ inertia + offset(recip, coef = -1.2) + trans,
    envir = fixed_spec_envir()
  )
  # the inner call is still unwrapped and parsed like any effect
  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("inertia", "recip", "trans")
  )
  expect_identical(unlist(parsed$offset_parameter), c(FALSE, TRUE, FALSE))
  expect_identical(
    unlist(parsed$offset_coef_parameter),
    c(NA, -1.2, NA)
  )

  spec <- assemble_from_formula(
    calls_dependent ~ inertia + offset(recip, coef = -1.2) + trans
  )
  expect_identical(spec$idx, 2L)
  expect_identical(spec$values, -1.2)
})

test_that("the coef value is matched by name, not by position", {
  parsed <- parse_formula(
    calls_dependent ~ inertia + offset(coef = 2, recip),
    envir = fixed_spec_envir()
  )
  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("inertia", "recip")
  )
  expect_identical(unlist(parsed$offset_coef_parameter), c(NA, 2))
})

test_that("the intercept keeps the formula's offset values aligned", {
  spec <- assemble_from_formula(
    calls_dependent ~ 1 + offset(indeg, coef = 0.5) + outdeg,
    sub_model = "rate"
  )
  expect_identical(spec$idx, 2L)
  expect_identical(spec$values, 0.5)
})

test_that("per-term values distinguish two offsets in one formula", {
  spec <- assemble_from_formula(
    calls_dependent ~
      inertia + offset(recip, coef = 2) + offset(trans, coef = -1)
  )
  expect_identical(spec$idx, c(2L, 3L))
  expect_identical(spec$values, c(2, -1))
})

test_that("two value sources for one term abort", {
  expect_snapshot(
    error = TRUE,
    assemble_from_formula(
      calls_dependent ~ inertia + offset(recip, coef = 2),
      offset_coef = 2
    )
  )
})

test_that("an offset term with no value from either source aborts", {
  expect_snapshot(
    error = TRUE,
    assemble_from_formula(calls_dependent ~ inertia + offset(recip))
  )
})

test_that("a coef value must be a single finite number", {
  expect_error(
    parse_formula(
      calls_dependent ~ inertia + offset(recip, coef = c(1, 2)),
      envir = fixed_spec_envir()
    ),
    "single finite number"
  )
  expect_error(
    parse_formula(
      calls_dependent ~ inertia + offset(recip, wrong = 2),
      envir = fixed_spec_envir()
    ),
    "accepts a term and an optional"
  )
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

# Starting values resolve against the coefficient labels the fit renders, which
# `.coefTerms()` dedups when two terms share an effect name -- so the labels a
# user matches against are the ones `coef()` shows, collisions included.
labels_of <- function(formula) {
  envir <- fixed_spec_envir()
  parsed <- parse_formula(formula, envir = envir)
  term_label(
    GetDetailPrint(get_objects_effects_link(parsed$rhs_names), parsed),
    ".coef_name",
    "coef"
  )
}

test_that("an unnamed full-length vector seeds every coefficient", {
  labels <- c("Intercept", "ideg", "odeg")
  spec <- resolve_initial_parameters(c(-3, 0.1, 0.2), labels, 3L)
  expect_s3_class(spec, "initial_spec")
  expect_identical(spec$idx, 1:3)
  expect_identical(spec$values, c(-3, 0.1, 0.2))
  expect_identical(spec$names, labels)
})

test_that("a named vector seeds only the coefficients it names", {
  labels <- c("Intercept", "ideg", "odeg")
  spec <- resolve_initial_parameters(c(odeg = 0.5), labels, 3L)
  expect_identical(spec$idx, 3L)
  expect_identical(spec$values, 0.5)
  expect_identical(spec$names, "odeg")
})

test_that("nothing supplied resolves to no contract", {
  expect_null(resolve_initial_parameters(NULL, c("a", "b"), 2L))
})

test_that("names are matched against collision-deduped labels", {
  labels <- labels_of(
    calls_dependent ~
      1 + indeg + outdeg(call_network, window = "1 hour") + outdeg
  )
  # two `outdeg` terms, so the labels carry the object and window that tell
  # them apart
  expect_length(unique(labels), length(labels))
  spec <- resolve_initial_parameters(
    stats::setNames(0.5, labels[[3]]),
    labels,
    length(labels)
  )
  expect_identical(spec$idx, 3L)
  expect_identical(spec$names, labels[[3]])
})

test_that("an unnamed partial vector aborts naming the expected length", {
  expect_snapshot(
    error = TRUE,
    resolve_initial_parameters(c(0.5), c("Intercept", "ideg", "odeg"), 3L)
  )
})

test_that("an unknown name aborts listing the available labels", {
  expect_snapshot(
    error = TRUE,
    resolve_initial_parameters(c(inertai = 1.5), c("Intercept", "inrt"), 2L)
  )
})

test_that("a partly named vector aborts", {
  expect_error(
    resolve_initial_parameters(c(ideg = 1, 2), c("ideg", "odeg"), 2L),
    "fully named or fully unnamed"
  )
})

test_that("offset_coef can name the offset terms it values", {
  spec <- assemble_from_formula(
    calls_dependent ~ inertia + offset(recip) + offset(trans),
    offset_coef = c(trans = -1, rec = 2)
  )
  expect_identical(spec$idx, c(2L, 3L))
  expect_identical(spec$values, c(2, -1))
})

test_that("a named offset_coef entry must name an offset term", {
  expect_snapshot(
    error = TRUE,
    assemble_from_formula(
      calls_dependent ~ inertia + offset(recip),
      offset_coef = c(inrt = 2)
    )
  )
})

test_that("list-shaped starting values are accepted by the constructor", {
  expect_no_error(
    set_algorithm_newton(initial_parameters = c(inertia = 0.5))
  )
  expect_no_error(
    set_algorithm_newton(
      initial_parameters = list(creation = list(rate = c(inertia = 0.5)))
    )
  )
  expect_error(
    set_algorithm_newton(initial_parameters = list(c(inertia = 0.5))),
    "must be named"
  )
  expect_error(
    set_algorithm_newton(
      initial_parameters = list(creation = list(rate = list(c(1))))
    ),
    "nest one level only"
  )
  expect_error(
    set_algorithm_newton(initial_parameters = "a"),
    "must be a numeric vector"
  )
})

test_that("a contract flattens to the positional encoding", {
  spec <- new_fixed_spec(c(1L, 3L), c(0.5, -1), c("Intercept", "trans"))
  expect_identical(fixed_spec_to_vector(spec, 4L), c(0.5, NA, -1, NA))
  expect_null(fixed_spec_to_vector(NULL, 4L))
})

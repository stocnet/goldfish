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
  expect_s3_class(spec, "goldfishCoefFixed")
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
  expect_s3_class(spec, "goldfishCoefInit")
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

# The estimation-side decode. Every estimation path reads its masking from
# here, so the alignment between a term and its position is settled once.

test_that("nothing fixed or seeded leaves every coefficient free at zero", {
  mask <- resolve_coefficient_mask(NULL, NULL, 3L)
  expect_identical(mask$parameters, c(0, 0, 0))
  expect_null(mask$id_fixed)
  expect_identical(mask$id_unfixed, 1:3)
  expect_false(mask$likelihood_only)
  expect_false(mask$intercept_fixed)
  expect_false(mask$intercept_seeded)
})

test_that("fixing masks the fixed positions out of estimation", {
  fixed <- new_fixed_spec(2L, 1.5, "recip")
  mask <- resolve_coefficient_mask(fixed, NULL, 3L)
  expect_identical(mask$parameters, c(0, 1.5, 0))
  expect_identical(mask$id_fixed, 2L)
  expect_identical(mask$id_unfixed, c(1L, 3L))
  expect_false(mask$likelihood_only)
})

test_that("every coefficient fixed is likelihood-only evaluation", {
  fixed <- new_fixed_spec(1:3, c(1, 2, 3), c("a", "b", "c"))
  mask <- resolve_coefficient_mask(fixed, NULL, 3L)
  expect_true(mask$likelihood_only)
  expect_identical(mask$id_unfixed, integer(0))
  expect_true(mask$intercept_fixed)
})

test_that("seeding is applied first and fixing overwrites it", {
  initial <- new_initial_spec(c(1L, 2L), c(-3, 0.9), c("Intercept", "recip"))
  fixed <- new_fixed_spec(2L, 1.5, "recip")
  mask <- resolve_coefficient_mask(fixed, initial, 3L)
  expect_identical(mask$parameters, c(-3, 1.5, 0))
  expect_true(mask$intercept_seeded)
  expect_false(mask$intercept_fixed)
})

test_that("seeding a non-intercept term leaves the intercept unseeded", {
  initial <- new_initial_spec(3L, 0.5, "outdeg")
  mask <- resolve_coefficient_mask(NULL, initial, 3L)
  expect_identical(mask$parameters, c(0, 0, 0.5))
  expect_false(mask$intercept_seeded)
})

test_that("a position outside the coefficient vector aborts naming the term", {
  expect_snapshot(
    error = TRUE,
    resolve_coefficient_mask(new_fixed_spec(4L, 1, "trans"), NULL, 3L)
  )
  expect_error(
    resolve_coefficient_mask(NULL, new_initial_spec(9L, 1, "trans"), 3L),
    "seeded coefficient falls outside"
  )
})

test_that("a contract renders as one logical per coefficient", {
  spec <- new_fixed_spec(c(1L, 3L), c(0.5, -1), c("Intercept", "trans"))
  expect_identical(fixed_spec_mask(spec, 4L), c(TRUE, FALSE, TRUE, FALSE))
  expect_null(fixed_spec_mask(NULL, 4L))
})

# The fit's record of which coefficients were held. A typed table with a stable
# schema: every flag column exists on every fit, and `fixed` is a plain logical
# rather than the strings a character matrix forced it to be.

fixed_record_fit <- function(formula, ...) {
  d <- suppressWarnings({
    data("Social_Evolution", package = "goldfish", envir = environment())
    actors <- get("actors", environment())
    calls <- get("calls", environment())
    call_network <- make_network(nodes = actors, directed = TRUE)
    call_network <- link_events(
      x = call_network,
      change_event = calls,
      nodes = actors
    )
    calls_dependent <- make_dependent_events(
      events = calls,
      nodes = actors,
      default_network = call_network
    )
    calls_dependent <- calls_dependent[1:120, ]
    make_data(calls_dependent, call_network, calls, actors)
  })
  suppressWarnings(estimate_dynam(
    formula,
    sub_model = "choice",
    data = d,
    ...
  ))
}

test_that("the effect description is a typed table with a stable schema", {
  fit <- fixed_record_fit(calls_dependent ~ inertia + recip)
  expect_s3_class(fit$names, "data.frame")
  expect_true(all(
    c(
      "ignore_repetitions",
      "weighted",
      "type",
      "window",
      "fixed"
    ) %in%
      colnames(fit$names)
  ))
  # present on a fit that fixes nothing, and a logical there too
  expect_type(fit$names[, "fixed"], "logical")
  expect_false(any(fit$names[, "fixed"]))
  expect_identical(rownames(fit$names), c("inertia", "recip"))
})

test_that("fixedness is read from the logical, not parsed from a string", {
  fit <- fixed_record_fit(
    calls_dependent ~ inertia + offset(recip, coef = 2) + trans
  )
  expect_identical(fit$names[, "fixed"], c(FALSE, TRUE, FALSE))
  expect_equal(unname(GetFixed(fit)), c(FALSE, TRUE, FALSE))
  expect_named(GetFixed(fit), c("inertia", "recip", "trans"))
  # the fixed coefficient is omitted by default and kept with complete = TRUE
  expect_length(coef(fit), 2L)
  expect_equal(coef(fit, complete = TRUE)[["rec"]], 2)
})

# A fit old enough to store fixedness as strings also predates the snake_case
# component names, so it is the format guard it meets first.
old_string_column_fit <- function() {
  structure(
    list(
      parameters = c(1, 2),
      logLikelihood = -10,
      nParams = 2L,
      names = matrix(
        c("net", "net", "FALSE", "TRUE"),
        ncol = 2,
        dimnames = list(c("inertia", "recip"), c("Object", "fixed"))
      )
    ),
    class = "result.goldfish"
  )
}

test_that("a string-encoded fixed column is never evaluated", {
  fit <- fixed_record_fit(calls_dependent ~ inertia + offset(recip, coef = 2))
  fit$names[, "fixed"] <- as.character(fit$names[, "fixed"])
  # Not parsed back into logicals: an object recording fixedness this way
  # predates the layout, and the result-format guard is what diagnoses it --
  # refusing on the computing surfaces, saying so on the ones that only print.
  expect_false(any(GetFixed(fit)))
  expect_error(summary(old_string_column_fit()), "not fitted by this version")
})


test_that("an empty flag column is stored but not displayed", {
  fit <- fixed_record_fit(calls_dependent ~ inertia + recip)
  shown <- detail_display_table(fit$names)
  expect_identical(colnames(shown), "Object")
  # ... while a flag that applies is shown
  windowed <- fixed_record_fit(
    calls_dependent ~ inertia + recip(call_network, window = "1 hour")
  )
  expect_true("window" %in% colnames(detail_display_table(windowed$names)))
  expect_false("weighted" %in% colnames(detail_display_table(windowed$names)))
})

test_that("tidy() reports fixedness as a logical", {
  fit <- fixed_record_fit(
    calls_dependent ~ inertia + offset(recip, coef = 2) + trans
  )
  tidied <- generics::tidy(fit, complete = TRUE, compact = FALSE)
  expect_type(tidied$fixed, "logical")
  expect_identical(tidied$fixed, c(FALSE, TRUE, FALSE))
})

test_that("an effect used twice keeps both rows under its own name", {
  # The disambiguation belongs to the coefficient labels, which derive from the
  # row names; the row names themselves stay as the terms were written.
  fit <- fixed_record_fit(
    calls_dependent ~
      inertia + recip + recip(call_network, window = "1 hour")
  )
  expect_identical(rownames(fit$names), c("inertia", "recip", "recip"))
  expect_false(anyDuplicated(names(coef(fit))) > 0)
})

test_that("repeated effect names travel through the tables without warning", {
  fit <- fixed_record_fit(
    calls_dependent ~
      inertia + recip + recip(call_network, window = "1 hour")
  )
  expect_no_warning(tidied <- generics::tidy(fit, compact = FALSE))
  expect_identical(tidied$term, c("inertia", "recip", "recip"))
  expect_no_warning(
    utils::capture.output(print(summary(fit), compact = FALSE))
  )
})

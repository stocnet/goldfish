# One term vocabulary. goldfish renders a term three ways -- the compact
# string the summary prints, the export form, and the `coef()` abbreviation --
# and every argument that takes a term accepts all of them plus the position.
# Accepting more than one is correctness, not convenience: the compact string
# is neither guaranteed unique nor guaranteed to be what the user read.

terms_fixture <- function() {
  estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest
  )
}

test_that("model_terms reports every name a term answers to", {
  table <- model_terms(terms_fixture())

  expect_s3_class(table, "tbl_df")
  expect_contains(names(table), c("index", "term", "coefficient", "export"))
  expect_equal(table$index, 1:4)
  expect_equal(table$term[4], "indeg/networkExog")
  expect_equal(table$coefficient[4], "ideg_networ_1")
  expect_equal(table$export[4], "indeg_networkExog")
  # The effect details the non-compact summary prints travel with it, so a
  # search reaches the object an effect reads.
  expect_contains(names(table), "Object")
  expect_equal(table$Object[4], "networkExog")
})

test_that("model_terms returns the full string, not the console's", {
  fit <- terms_fixture()

  # The printed summary abbreviates to the console width, and the abbreviation
  # can COLLIDE without any ellipsis to signal it: at width 16 both `indeg`
  # terms render `ideg/networ`. This table must never abbreviate, or a string
  # read off a narrow console would select the wrong term or none.
  narrow <- compact_term_strings(fit$names, "console", width = 16L)
  expect_equal(narrow[[2]], narrow[[4]])
  full <- model_terms(fit)$term
  expect_false(anyDuplicated(full) > 0)
  expect_equal(full[[2]], "indeg/networkState")
  expect_equal(full[[4]], "indeg/networkExog")
})

test_that("model_terms filters by pattern across every column", {
  fit <- terms_fixture()

  expect_equal(nrow(model_terms(fit, pattern = "Exog")), 1L)
  expect_equal(model_terms(fit, pattern = "Exog")$index, 4L)
  # Case-insensitive, and reaching the effect details as well as the names.
  expect_equal(nrow(model_terms(fit, pattern = "networkstate")), 2L)
  expect_equal(nrow(model_terms(fit, pattern = "outdeg")), 1L)
  expect_equal(nrow(model_terms(fit, pattern = "nothing here")), 0L)
})

test_that("every spelling selects the same coefficient", {
  seeded <- function(name) {
    estimate_wrapper(
      depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
      model = "DyNAM",
      sub_model = "rate",
      data = dataTest,
      control_algo = set_algorithm_newton(
        initial_parameters = stats::setNames(0.5, name),
        max_iterations = 0
      )
    )$parameters
  }

  # The string the summary shows, the export form and the coef() label are the
  # same term, so they seed the same position.
  expect_equal(seeded("indeg/networkExog"), seeded("ideg_networ_1"))
  expect_equal(seeded("indeg_networkExog"), seeded("ideg_networ_1"))
  expect_equal(seeded("ideg_networ_1")[4], 0.5)
})

test_that("an unrecognized term names the helper", {
  expect_snapshot(
    error = TRUE,
    estimate_wrapper(
      depNetwork ~ 1 + indeg + outdeg,
      model = "DyNAM",
      sub_model = "rate",
      data = dataTest,
      control_algo = set_algorithm_newton(initial_parameters = c(bogus = 0.5))
    )
  )
})

test_that("the matcher resolves positions and refuses impossible ones", {
  fit <- terms_fixture()

  expect_equal(resolve_term_index(c(2L, 4L), fit$names, "effects"), c(2L, 4L))
  expect_snapshot(error = TRUE, resolve_term_index(9L, fit$names, "effects"))
  expect_snapshot(error = TRUE, resolve_term_index(0L, fit$names, "effects"))
})

test_that("an ambiguous term is offered a resolution", {
  fit <- terms_fixture()
  # Force the collision the compact string cannot rule out: two terms whose
  # rendered arguments are identical. The abort must offer a spelling that
  # separates them, since repeating the string they share resolves nothing.
  collided <- fit$names
  collided[4, "Object"] <- collided[2, "Object"]

  expect_equal(
    compact_term_strings(collided, "console", width = Inf)[2],
    compact_term_strings(collided, "console", width = Inf)[4]
  )
  expect_snapshot(
    error = TRUE,
    resolve_term_index("indeg/networkState", collided, "effect")
  )
})

test_that("model_terms refuses what is not a fit", {
  expect_snapshot(error = TRUE, model_terms(depNetwork))
})

# Family expansion (task 4.7). A model may carry an effect several times over
# different layers or arguments, and "test indeg" is then a question about the
# effect rather than about one of its variants. The bare effect name is
# therefore a fourth spelling -- but only where selecting a SET makes sense, so
# it is opt-in and the single-coefficient arguments do not get it.

test_that("a bare effect name selects every term of that effect", {
  fit <- terms_fixture()
  # indeg appears twice: over the dependent network and over the exogenous one.
  expect_equal(rownames(fit$names), c("Intercept", "indeg", "outdeg", "indeg"))

  expect_equal(
    resolve_term_index("indeg", fit$names, "effects", expand_family = TRUE),
    c(2L, 4L)
  )
  # An effect carried once expands to itself, so the caller need not know
  # which case they are in.
  expect_equal(
    resolve_term_index("outdeg", fit$names, "effects", expand_family = TRUE),
    3L
  )
})

test_that("a term spelling still beats the effect name that contains it", {
  fit <- terms_fixture()
  # The per-term spellings are tried first, so naming one variant selects that
  # variant and not its family.
  expect_equal(
    resolve_term_index(
      "indeg/networkExog",
      fit$names,
      "effects",
      expand_family = TRUE
    ),
    4L
  )
})

test_that("expansion keeps request order and selects each term once", {
  fit <- terms_fixture()
  # The family contributes its terms in model order at the position where it
  # was named; the rest of the request keeps the order it was written in.
  expect_equal(
    resolve_term_index(
      c("outdeg", "indeg"),
      fit$names,
      "effects",
      expand_family = TRUE
    ),
    c(3L, 2L, 4L)
  )
  # Naming an effect and one of its own variants is not an error and does not
  # select that variant twice.
  expect_equal(
    resolve_term_index(
      c("indeg", "indeg/networkExog"),
      fit$names,
      "effects",
      expand_family = TRUE
    ),
    c(2L, 4L)
  )
})

test_that("family expansion is opt-in, and its absence is the old error", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- terms_fixture()
  # Without the flag a bare effect name is simply not a term of the model,
  # which is what the single-coefficient arguments must keep saying.
  expect_snapshot(
    error = TRUE,
    resolve_term_index("indeg", fit$names, "effect")
  )
  # With it, an unknown name lists the effect names as a further route in.
  expect_snapshot(
    error = TRUE,
    resolve_term_index("nonesuch", fit$names, "effects", expand_family = TRUE)
  )
})

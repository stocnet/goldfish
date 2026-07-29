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

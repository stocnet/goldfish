# The non-tidy flavored surface: `residuals()`, `fitted()`, `predict()` and
# `evaluate_model()` on a container.
#
# These return vectors, matrices and lists, so nothing in the return can carry
# the process identity the way `augment()`'s appended columns do. They answer
# with a list keyed by process label instead, and `flavor =` narrows it.
#
# The bug this replaces is worth naming: `residuals()` and `fitted()` used to
# reach `stats`' own default on a container and return NULL silently, which is
# the failure mode -- a surface reporting nothing while looking like it worked
# -- that this whole change exists to remove.

nontidy_generics <- list(
  residuals = function(x, ...) stats::residuals(x, ...),
  fitted = function(x, ...) stats::fitted(x, ...),
  predict = function(x, ...) stats::predict(x, ...),
  evaluate_model = function(x, ...) evaluate_model(x, ...)
)

test_that("no method returns NULL on a container", {
  skip_on_cran()
  container <- flavored_container_fit(return_preprocessed = TRUE)

  for (name in names(nontidy_generics)) {
    value <- nontidy_generics[[name]](container)
    expect_false(is.null(value), info = name)
    expect_length(value, nrow(container$process_map))
  }
})

test_that("each entry equals the call on that process's own fit", {
  skip_on_cran()
  container <- flavored_container_fit(return_preprocessed = TRUE)
  labels <- goldfish:::flavored_component_labels(container)
  processes <- goldfish:::flavored_processes(container)

  for (name in names(nontidy_generics)) {
    generic <- nontidy_generics[[name]]
    joint <- generic(container)
    expect_named(joint, labels, info = name)
    for (i in seq_along(processes)) {
      expect_equal(
        joint[[i]],
        generic(processes[[i]]$fit),
        info = paste(name, labels[i])
      )
    }
  }
})

test_that("a flavor spanning several families narrows without disambiguating", {
  # `flavor =` selects; it does not ask which family was meant. Two processes
  # survive here, so the return stays a list -- restricted, and in the same
  # order the unselected call uses.
  skip_on_cran()
  container <- flavored_container_fit(return_preprocessed = TRUE)

  whole <- residuals(container)
  narrowed <- residuals(container, flavor = "creation")

  expect_named(
    narrowed,
    c("calls › creation › rate", "calls › creation › choice")
  )
  expect_equal(narrowed, whole[names(narrowed)])
})

test_that("a flavor resolving to one process gives the single-fit shape", {
  # The shape the Fisheries Treaties fits have, and the reason `flavor =` exists:
  # the familiar return is one argument away rather than an extraction.
  skip_on_cran()
  container <- flavored_single_family_fit(return_preprocessed = TRUE)
  expect_identical(nrow(container$process_map), 2L)

  for (name in names(nontidy_generics)) {
    generic <- nontidy_generics[[name]]
    selected <- generic(container, flavor = "creation")

    expect_false(is.list(selected) && identical(names(selected), character(0)))
    expect_equal(
      selected,
      generic(container)[["calls › creation › rate"]],
      info = name
    )
    expect_equal(
      selected,
      generic(fit_of(container, "creation", "rate")),
      info = name
    )
  }
  # Not wrapped: a numeric series comes back a numeric series.
  expect_type(residuals(container, flavor = "creation"), "double")
})

test_that("an unknown flavor aborts naming the ones the fit carries", {
  skip_on_cran()
  container <- flavored_container_fit(return_preprocessed = TRUE)

  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = TRUE
  )
  expect_snapshot(error = TRUE, residuals(container, flavor = "signing"))
  expect_snapshot(error = TRUE, fitted(container, flavor = c("creation", "x")))
})

test_that("a process that refuses is named", {
  # A container whose fits carry no preprocessed statistics: the per-process
  # method aborts, and the message has to say which of four processes it was.
  skip_on_cran()
  container <- flavored_container_fit()

  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = TRUE
  )
  expect_snapshot(error = TRUE, predict(container))
})

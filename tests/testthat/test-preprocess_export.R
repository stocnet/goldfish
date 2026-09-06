test_that("gather_model_data() is soft-deprecated onto compute_statistics()", {
  expect_snapshot(invisible(gather_model_data(
    depNetwork ~ inertia(networkState),
    data = dataTest
  )))
})

test_that("gather_model_data() returns what compute_statistics() returns", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_identical(
    gather_model_data(
      depNetwork ~ inertia(networkState) + recip,
      data = dataTest
    ),
    compute_statistics(
      depNetwork ~ inertia(networkState) + recip,
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      output = "gather"
    )
  )
})

test_that("Args check", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    gather_model_data(
      depNetwork ~ inertia(networkState, ignore_repetitions = TRUE),
      data = dataTest
    )
  )
  expect_warning(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      data = dataTest
    )
  )
  expect_error(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      model = "smh",
      data = dataTest
    )
  )
  expect_error(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      sub_model = "smh",
      data = dataTest
    )
  )
})
test_that("Printing", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_output(
    gather_model_data(
      depNetwork ~ inertia(networkState),
      ,
      data = dataTest,
      progress = TRUE
    ),
    "Preprocessing events."
  )
})
test_that("Output", {
  withr::local_options(lifecycle_verbosity = "quiet")
  out <- gather_model_data(
    depNetwork ~ inertia(networkState),
    data = dataTest
  )
  expect_type(out, "list")
  # +2 vs the legacy 8: the shared index vocabulary index_i / index_j; +1 for the
  # node_lookup resolving those indices to original node identity (stocnet path);
  # +1 for is_exact_time, reported alongside has_intercept on every output form.
  expect_length(out, 12)
  expect_contains(names(out), c("has_intercept", "is_exact_time"))
})
test_that("export names are valid, unique R names", {
  withr::local_options(lifecycle_verbosity = "quiet")
  out <- gather_model_data(
    depNetwork ~ inertia(networkState, weighted = TRUE) + outdeg(networkExog),
    data = dataTest
  )
  nm <- out$names_effects
  expect_equal(nm, colnames(out$stat_all_events))
  expect_true(all(make.names(nm) == nm))
  expect_false(any(grepl("[/·\\[\\] ]", nm, perl = TRUE)))
  expect_false(anyDuplicated(nm) > 0)
})
test_that("export name collisions are made unique", {
  m <- cbind(
    Object = c("net", "net"),
    weighted = c("", "")
  )
  rownames(m) <- c("inertia", "inertia")
  nm <- CreateNames(m)
  expect_false(anyDuplicated(nm) > 0)
  expect_length(nm, 2)
})
test_that("export max_length is enforced and keeps uniqueness", {
  m <- cbind(
    Object = c("alpha", "alpha"),
    weighted = c("W", "")
  )
  rownames(m) <- c("inertia", "inertia")
  nm <- CreateNames(m, max_length = 8L)
  expect_true(all(nchar(nm) <= 8L))
  expect_false(anyDuplicated(nm) > 0)
})

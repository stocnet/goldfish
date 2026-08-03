# The diagnostic test family: what the generics are, and what they dispatch
# on. The bodies arrive with each test's own implementation; what is fixed
# here is the shape of the entry points, which is part of the released API.

test_that("the test_* family are generics dispatching on the fit", {
  for (fn in c("test_gof", "test_parameter", "test_time")) {
    expect_true("UseMethod" %in% all.names(body(get(fn))))
    expect_false(is.null(getS3method(fn, "default", optional = TRUE)))
  }
})

test_that("the first argument matches the signature RSiena publishes", {
  # RSiena 1.6.6 publishes all three names as generics for the same three
  # questions asked of a different model class:
  #   test_gof(object, ...)  test_parameter(x, ...)  test_time(x, ...)
  # R CMD check enforces generic/method argument consistency, so registering
  # against those generics later is a two-line change only while these agree.
  # Hardcoded rather than read from RSiena: this must hold whether or not the
  # sibling is installed.
  expect_identical(names(formals(test_gof)), c("object", "..."))
  expect_identical(names(formals(test_parameter)), c("x", "..."))
  expect_identical(names(formals(test_time)), c("x", "..."))
})

test_that("the default methods name what they received", {
  expect_snapshot(test_gof(1:3), error = TRUE)
  expect_snapshot(test_parameter("a"), error = TRUE)
  expect_snapshot(test_time(list()), error = TRUE)
})

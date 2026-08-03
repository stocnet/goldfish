# `test_parameter()`: the score test of a coefficient the formula held at a
# value. The quantity it reads exists nowhere on the fit -- estimation zeroes
# the score at fixed coefficients before every Newton step -- so the first
# thing to hold is that it is recomputed, and the second that the recomputation
# is the efficient-score form and not something adjacent to it.

parameter_fixture <- function(
  formula = calls ~ inertia + offset(recip, coef = 0) + trans,
  return_preprocessed = TRUE,
  ...
) {
  data("social_evolution", envir = environment())
  suppressMessages(estimate_dynam(
    formula,
    sub_model = "choice",
    data = get("social_evolution", envir = environment()),
    return_preprocessed = return_preprocessed,
    ...
  ))
}

test_that("the tested score is the one the fit does not carry", {
  fit <- parameter_fixture()
  is_fixed <- GetFixed(fit)
  expect_identical(sum(is_fixed), 1L)

  # Estimation masks it before the Newton step, so the fit reports zero there
  # however wrong the imposed value is. That is why this test costs a pass.
  expect_identical(unname(fit$final_score[is_fixed]), 0)
  result <- test_parameter(fit)
  expect_false(result$score == 0)
  expect_identical(result$index, unname(which(is_fixed)))
  expect_identical(result$imposed, 0)
})

test_that("the statistic is the efficient score form", {
  fit <- parameter_fixture()
  result <- test_parameter(fit)
  joint <- attr(result, "context")$joint

  pass <- evaluate_model(
    fit,
    at = coef(fit, complete = TRUE),
    return = c("score", "information")
  )
  inverse <- solve(pass$information)

  # The specified equivalence: LM is the quadratic form of one scoring step.
  # `t(Delta) I Delta == t(U) I^-1 U` is an identity, so this pins the
  # arithmetic rather than the algebra.
  delta <- inverse %*% pass$score
  quadratic <- drop(t(delta) %*% pass$information %*% delta)
  expect_equal(quadratic, drop(t(pass$score) %*% inverse %*% pass$score))
  # And the reported statistic is that number. It is not identical: the free
  # block's score is zero only to the convergence tolerance, and the reported
  # form drops it where `t(U) I^-1 U` keeps it.
  expect_equal(joint$statistic, quadratic, tolerance = 1e-4)

  # The block of the INVERSE, used directly. Inverting it a second time is the
  # available mistake -- it reports the quadratic form in the efficient
  # information instead, which on this fixture is 1570 times larger.
  d <- which(GetFixed(fit))
  expect_equal(
    result$statistic,
    unname(pass$score[d]^2 * inverse[d, d])
  )
  expect_gt(
    unname(pass$score[d]^2 / inverse[d, d]),
    10 * result$statistic
  )
})

test_that("an offset held at a wrong value is detected", {
  # `recip` matters on these data, so holding it at zero is a misspecification
  # the test must see -- and holding it near its own estimate is one it must
  # not.
  wrong <- test_parameter(parameter_fixture())
  expect_lt(wrong$p_value, 0.05)

  estimated <- coef(parameter_fixture(calls ~ inertia + recip + trans))[["rec"]]
  right <- test_parameter(parameter_fixture(
    substitute(
      calls ~ inertia + offset(recip, coef = VALUE) + trans,
      list(VALUE = estimated)
    ) |>
      eval()
  ))
  expect_gt(right$p_value, 0.05)
  # Held at its own unconstrained estimate the leftover score is ~0, which is
  # the sense in which the test reads "would freeing this move the fit".
  expect_lt(abs(right$score), abs(wrong$score) / 100)
})

test_that("the chi-square degrees of freedom count the tested block", {
  fit <- parameter_fixture(
    calls ~ inertia + offset(recip, coef = 0) + offset(trans, coef = 0)
  )
  result <- test_parameter(fit)
  joint <- attr(result, "context")$joint

  expect_identical(nrow(result), 2L)
  expect_identical(result$df, c(1L, 1L))
  expect_identical(joint$df, 2L)
  expect_equal(
    joint$p_value,
    stats::pchisq(joint$statistic, df = 2L, lower.tail = FALSE)
  )
  # A selection tests fewer, and the untested offset leaves the parameter
  # space rather than becoming a nuisance parameter: it is a constant of the
  # model under both hypotheses.
  one <- test_parameter(fit, effects = "recip/calls [Fx]")
  expect_identical(nrow(one), 1L)
  expect_identical(attr(one, "context")$joint$df, 1L)
  expect_equal(
    one$statistic,
    result$statistic[result$term == "recip/calls [Fx]"]
  )
})

test_that("what cannot be tested says how to make it testable", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  expect_snapshot(
    test_parameter(parameter_fixture(calls ~ inertia + recip)),
    error = TRUE
  )
  expect_snapshot(
    test_parameter(parameter_fixture(), effects = "common_receiver"),
    error = TRUE
  )
  expect_snapshot(
    test_parameter(parameter_fixture(), effects = "inertia/calls"),
    error = TRUE
  )
  expect_snapshot(
    test_parameter(parameter_fixture(return_preprocessed = FALSE)),
    error = TRUE
  )
})

test_that("the statistics can be supplied instead of attached", {
  fit <- parameter_fixture(return_preprocessed = FALSE)
  expect_null(fit$preprocessed)
  data("social_evolution", envir = environment())
  prep <- compute_statistics(
    calls ~ inertia + offset(recip, coef = 0) + trans,
    sub_model = "choice",
    data = get("social_evolution", envir = environment()),
    output = "preprocessed"
  )
  supplied <- test_parameter(fit, preprocessed = prep)
  attached <- test_parameter(parameter_fixture())
  expect_equal(supplied$statistic, attached$statistic)
})

test_that("print reports the joint test and the per-term rows", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- parameter_fixture()
  header <- function(x) {
    out <- capture.output(print(x))
    cat(out[seq_len(which(startsWith(out, "# A tibble"))[1] - 1L)], sep = "\n")
  }
  expect_snapshot(header(test_parameter(fit)))
})

test_that("the table carries the diagnostic contract", {
  fit <- parameter_fixture()
  result <- test_parameter(fit)

  expect_s3_class(result, "test_parameter")
  expect_s3_class(result, "tbl_df")
  expect_identical(attr(result, "diagnostic"), "test_parameter")
  expect_identical(
    attr(result, "version"),
    as.character(utils::packageVersion("goldfish"))
  )
  # Row operations keep the object; dropping a defining column demotes it, so
  # no print can report a statistic from a column that is gone.
  expect_s3_class(result[1, ], "test_parameter")
  expect_false(inherits(result[, c("term", "imposed")], "test_parameter"))
})

test_that("a flavored fit is tested against each process's own offsets", {
  container <- suppressWarnings(suppressMessages(estimate_dynam(
    make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      choice = list(
        creation ~ trans + offset(recip, coef = 0),
        dissolution ~ trans
      ),
      model = "DyNAM",
      data = flavored_fixture_data()
    ),
    return_preprocessed = TRUE
  )))
  result <- test_parameter(container)

  # `inertia` would not do here: a creation event targets an absent tie, so
  # the mutually exclusive mask makes that statistic identically zero across
  # the risk set and the information singular. `recip` reads the reverse tie,
  # which the mask leaves alone.
  #
  # Only the process that declares an offset contributes rows, and no
  # candidate argument was supplied anywhere -- each formula names its own.
  expect_identical(nrow(result), 1L)
  expect_identical(result$flavor, "creation")
  expect_identical(result$family, "choice")
  expect_identical(
    result$statistic,
    test_parameter(fit_of(container, "creation", "choice"))$statistic
  )
  # One joint row per contributing process, and no combination across them.
  joint <- attr(result, "context")$joint
  expect_identical(nrow(joint), 1L)
  expect_true(all(c("flavor", "family") %in% names(joint)))
})

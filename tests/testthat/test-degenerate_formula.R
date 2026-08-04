# A formula carrying no effect term used to fail on an internal
# `nrow(integer(0))`: `terms()` drops the shape of its `factors` attribute along
# with the columns when there is no non-offset term. `~ 1`, `~ offset(x)`,
# `~ 1 + offset(x)` and multi-offset formulas all took that path.
#
# Parsing them is one question and judging them is another, and they are
# answered at different layers. Parsing has to reach the end so the model-level
# check runs on a parsed formula; the check itself runs before preprocessing,
# whose builders assume at least one effect throughout and fail on zero-length
# dimensions rather than on a stated rule.
#
# A term held at a fixed value still counts as a term. An all-fixed model
# estimates nothing and evaluates its likelihood, which is a supported use --
# the process-state evaluators take their reference from one.

# cli abort snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("degenerate formulas parse instead of failing on nrow()", {
  degenerate <- list(
    intercept = y ~ 1,
    offset = y ~ offset(x),
    both = y ~ 1 + offset(x),
    two_offsets = y ~ offset(x) + offset(z)
  )

  for (label in names(degenerate)) {
    parsed <- expect_no_error(get_rhs_names(degenerate[[label]]))
    # The per-term flags stay aligned with the terms, which is what the rest of
    # the parser reads.
    for (flag in c("offset", "is_main", "is_operand")) {
      expect_equal(
        length(attr(parsed, flag)),
        length(parsed),
        info = paste(label, flag)
      )
    }
  }

  expect_identical(attr(get_rhs_names(y ~ offset(x)), "offset"), TRUE)
  expect_identical(
    attr(get_rhs_names(y ~ offset(x) + offset(z)), "offset"),
    c(TRUE, TRUE)
  )
  # A formula that already parsed is unchanged, interactions included.
  expect_identical(attr(get_rhs_names(y ~ a * b), "is_main"), c(TRUE, TRUE))
})

test_that("an intercept-only model aborts on every sub-model", {
  # The two aborts share a headline and differ in the reason, because the
  # reasons differ. On the multinomial families the intercept identifies
  # nothing, which is a property of the likelihood. On the exact-time families
  # it is a well-defined baseline rate and goldfish is declining a model it
  # could fit, which is a property of goldfish -- calling that unidentified
  # would send the reader looking for a statistical error that is not there.
  local_cli_context()

  expect_snapshot(
    error = TRUE,
    estimate_dynam(depNetwork ~ 1, sub_model = "rate", data = dataTest)
  )
  expect_snapshot(
    error = TRUE,
    estimate_dynam(depNetwork ~ 1, sub_model = "choice", data = dataTest)
  )
  expect_snapshot(
    error = TRUE,
    estimate_dynam(
      depNetwork ~ 1,
      sub_model = "choice_coordination",
      data = dataTest
    )
  )
})

test_that("the abort arrives before preprocessing", {
  # The failure it replaces was an internal dimension error raised deep in the
  # builders, so the point is not only that it aborts but where.
  expect_error(
    estimate_dynam(depNetwork ~ 1, sub_model = "rate", data = dataTest),
    "at least one effect term"
  )
  err <- tryCatch(
    estimate_dynam(depNetwork ~ 1, sub_model = "rate", data = dataTest),
    error = function(e) e
  )
  expect_false(grepl("invalid 'length' argument", conditionMessage(err)))
  expect_false(grepl("must be of a vector type", conditionMessage(err)))
})

test_that("an all-fixed model is an evaluation, not an error", {
  # Every coefficient fixed leaves nothing to estimate but plenty to evaluate.
  # Fixedness means the same thing whether it came from `offset()` or from
  # `fixed_parameters`, so an offset-only formula is this case, not a
  # degenerate one.
  offset_only <- estimate_dynam(
    depNetwork ~ offset(inertia, coef = 0.5),
    sub_model = "choice",
    data = dataTest
  )
  expect_s3_class(offset_only, "result.goldfish")
  expect_length(coef(offset_only), 0L)
  expect_true(is.finite(as.numeric(logLik(offset_only))))

  # The rate sub-model adds its own intercept, so the same formula there has a
  # term and a free parameter, and estimates it.
  with_intercept <- estimate_dynam(
    depNetwork ~ offset(indeg, coef = 0.5),
    sub_model = "rate",
    data = dataTest
  )
  expect_named(coef(with_intercept), "Intercept")
})

test_that("the diagnostics defined without free parameters keep working", {
  # The boundary is whether the quantity is a function of the free parameters,
  # not whether it is advanced. `test_parameter()` is the case worth pinning:
  # it exists to test a coefficient held at an imposed value, so an all-fixed
  # fit is its pure case rather than its degenerate one.
  formula <- depNetwork ~
    offset(inertia, coef = 0.5) + offset(recip, coef = -0.2)
  prep <- compute_statistics(
    formula,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    output = "preprocessed"
  )
  fit <- estimate_dynam(formula, sub_model = "choice", data = dataTest)

  expect_true(is.finite(as.numeric(logLik(fit))))
  expect_true(is.finite(AIC(fit)))
  expect_true(is.finite(BIC(fit)))
  expect_length(fitted(fit), length(fit$intervals))
  expect_no_error(augment(fit))
  expect_no_error(residuals(fit, type = "deviance"))

  tested <- test_parameter(fit, preprocessed = prep)
  # One row per held coefficient, each carrying the value it was held at.
  expect_equal(nrow(tested), 2L)
  expect_equal(tested$imposed, c(0.5, -0.2))
  expect_true(all(is.finite(tested$statistic)))

  expect_no_error(evaluate_model(fit, return = "loglik", preprocessed = prep))
  expect_no_error(predict(fit, preprocessed = prep))
})

test_that("a covariance needs an estimated coefficient", {
  # The one surface that reached a numerical routine: the information over the
  # free parameters is zero-dimensional and `solve()` reported that in its own
  # vocabulary rather than in the model's.
  local_cli_context()
  fit <- estimate_dynam(
    depNetwork ~ offset(inertia, coef = 0.5),
    sub_model = "choice",
    data = dataTest
  )

  expect_snapshot(error = TRUE, vcov(fit))
  # The printer renders an empty coefficient table without complaint.
  expect_no_warning(capture.output(print(summary(fit))))
})

test_that("one free term beside an offset is unaffected", {
  fit <- estimate_dynam(
    depNetwork ~ 1 + offset(indeg, coef = 0.5) + outdeg,
    sub_model = "rate",
    data = dataTest
  )
  reference <- estimate_dynam(
    depNetwork ~ 1 + outdeg,
    sub_model = "rate",
    data = dataTest
  )

  expect_named(coef(fit), c("Intercept", "odeg"))
  # The offset is held, so it is not a free parameter, but it does enter the
  # linear predictor -- the fit is not the same as dropping the term.
  expect_false(isTRUE(all.equal(coef(fit), coef(reference))))
})

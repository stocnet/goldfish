local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("the contract table parses into the shape the readers assume", {
  contract <- fit_contract()

  expect_named(
    contract,
    c("generic", "class", "verdict", "reason", "alternative")
  )
  expect_true(all(contract$verdict %in% FIT_CONTRACT_VERDICTS))
  # One verdict per cell: a duplicated row is how a table starts disagreeing
  # with itself, and `fit_contract_cell()` would silently take the first.
  expect_false(any(duplicated(contract[c("generic", "class")])))
})

test_that("every override and refuse records why", {
  contract <- fit_contract()
  decided <- contract[contract$verdict != "inherit", ]

  expect_true(all(nzchar(decided$reason)))
  # An `inherit` cell has nothing to explain -- the parent's method is simply
  # correct -- so a reason there is a leftover from an edited verdict.
  expect_true(all(!nzchar(contract$reason[contract$verdict == "inherit"])))
})

test_that("a refuse cell names the class, the reason and the alternative", {
  local_cli_context()

  expect_error(
    refuse_fit_generic("coef_layout", "goldfishFit"),
    class = "goldfish_generic_refused"
  )
  expect_snapshot(
    refuse_fit_generic("coef_layout", "goldfishFit"),
    error = TRUE
  )
})

test_that("refusing a cell the table does not refuse is reported as a bug", {
  local_cli_context()

  # `coef` on a single-process fit is an `inherit` cell, so a method calling
  # the refusal helper for it would be code and table disagreeing.
  expect_snapshot(refuse_fit_generic("coef", "goldfishFit"), error = TRUE)
  expect_snapshot(refuse_fit_generic("coef", "goldfishNoSuchFit"), error = TRUE)
})

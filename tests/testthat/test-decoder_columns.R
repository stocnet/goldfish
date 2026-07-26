test_that("decoder columns persisted on result$names", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState) +
      outdeg(networkExog, weighted = TRUE),
    data = dataTest,
    sub_model = "choice"
  )
  cols <- colnames(mod$names)
  expect_true(all(
    c(".effect_short", ".object_short", ".term_export", ".coef_name") %in% cols
  ))
  expect_length(mod$names[, ".coef_name"], mod$n_params)
  expect_false(anyDuplicated(mod$names[, ".coef_name"]) > 0)
  expect_false(anyDuplicated(mod$names[, ".term_export"]) > 0)
})

test_that("decoder columns carried through gather_model_data", {
  withr::local_options(lifecycle_verbosity = "quiet")
  out <- gather_model_data(
    depNetwork ~ inertia(networkState) +
      outdeg(networkExog, weighted = TRUE),
    data = dataTest
  )
  expect_true(all(
    c(".effect_short", ".object_short", ".term_export", ".coef_name") %in%
      colnames(out$effect_description)
  ))
})

test_that("CreateNames ignores dot columns (compute) and reads .term_export", {
  m <- cbind(
    Object = c("bilatnet", "contignet"),
    weighted = c("W", ""),
    .coef_name = c("foo", "bar")
  )
  rownames(m) <- c("inertia", "tie")
  out <- CreateNames(m)
  expect_false(any(grepl("foo|bar", out)))
  expect_equal(out, c("inertia_bilatnet_W", "tie_contignet"))

  m2 <- cbind(m, .term_export = c("inertia_bilatnet_W", "tie_contignet"))
  expect_equal(CreateNames(m2), c("inertia_bilatnet_W", "tie_contignet"))
})

test_that("term_label reads dot-column when present, computes when absent", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState) +
      outdeg(networkExog, weighted = TRUE),
    data = dataTest,
    sub_model = "choice"
  )
  fromCache <- term_label(mod$names, ".coef_name", "coef")
  stripped <- mod$names[, !startsWith(colnames(mod$names), "."), drop = FALSE]
  computed <- term_label(stripped, ".coef_name", "coef")
  expect_identical(unname(fromCache), unname(computed))
  expect_false(anyDuplicated(computed) > 0)
})

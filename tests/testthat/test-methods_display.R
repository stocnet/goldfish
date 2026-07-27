test_that("summary goldfish", {
  objSum <- summary(resModObject)
  expect_s3_class(objSum, "summary.result.goldfish")
  # The fixture's own components plus the three `summary()` adds (`coef_mat`,
  # `AIC`, `BIC`), so this count moves whenever the fit's layout does -- 20 now
  # that the fixture records `fit_version`.
  expect_length(objSum, 20)
  expect_true(inherits(objSum$coef_mat, "array"))
  expect_type(objSum$coef_mat, "double")
  expect_length(objSum$coef_mat, resModObject$n_params * 4)
  expect_true(is.na(objSum$coef_mat[2, 2]))
  expect_type(objSum$AIC, "double")
  expect_type(objSum$BIC, "double")
  expect_length(objSum$AIC, 1)
  expect_length(objSum$BIC, 1)
})
test_that("summary goldfish print", {
  # objInv <- expect_invisible(print(summary(resModObject)))
  expect_output(print(summary(resModObject)), "AIC")
  expect_output(print(summary(resModObject)), "BIC")
  expect_output(print(summary(resModObject)), "Call:")
  expect_output(print(summary(resModObject)), "Coefficients:")
  expect_failure(expect_output(print(summary(resModObject)), "\nrecip"))
  expect_output(
    print(summary(resModObject), complete = TRUE),
    "\nrecip"
  )
  # compact = TRUE (default): single table, no "Effects details"
  expect_failure(expect_output(
    print(summary(resModObject)),
    "Effects details"
  ))
  expect_failure(expect_output(
    print(summary(resModObject), complete = TRUE),
    "Effects details"
  ))
  # compact = FALSE: details table restored (when isDetPrint applies)
  expect_output(
    print(summary(resModObject), compact = FALSE, complete = TRUE),
    "\nEffects details"
  )
})
test_that("result print", {
  expect_output(print(resModObject), "Call:")
  expect_output(print(resModObject), "Coefficients:")
  expect_failure(
    expect_output(print(resModObject), "\n   inrt      rec    trans")
  )
  expect_output(
    print(resModObject, complete = TRUE),
    "\n   inrt      rec    trans"
  )
})
test_that("compact summary print: single table + present-code legend", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      outdeg(networkExog) +
      recip,
    data = dataTest,
    sub_model = "choice"
  )
  out <- capture.output(print(summary(mod), width = 100))
  expect_false(any(grepl("Effects details", out)))
  expect_true(any(grepl("inertia/networkState \\[W\\]", out, fixed = FALSE)))
  expect_true(any(grepl("^W = weighted", out)))
  expect_false(any(grepl("compact = FALSE", out)))
})
test_that("compact = FALSE details table hides decoder columns", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState) + outdeg(networkExog),
    data = dataTest,
    sub_model = "choice"
  )
  out <- capture.output(print(summary(mod), compact = FALSE, width = 100))
  expect_true(any(grepl("Effects details", out)))
  expect_false(any(grepl(
    "\\.coef_name|\\.term_export|\\.effect_short|\\.object_short",
    out
  )))
})
test_that("compact summary: no opaque codes means no legend", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState) + outdeg(networkExog),
    data = dataTest,
    sub_model = "choice"
  )
  out <- capture.output(print(summary(mod), width = 100))
  expect_false(any(grepl(
    "= weighted|= window|user-defined|= fixed|= ignore_rep|transformer",
    out
  )))
  expect_false(any(grepl("compact = FALSE", out)))
})
test_that("compact legend still prints without significance stars", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState, weighted = TRUE) + recip,
    data = dataTest,
    sub_model = "choice"
  )
  op <- options(show.signif.stars = FALSE)
  on.exit(options(op), add = TRUE)
  out <- capture.output(print(summary(mod), width = 100))
  expect_false(any(grepl("Signif. codes", out)))
  expect_true(any(grepl("^W = weighted", out)))
})
test_that("nodes print", {
  expect_output(print(actors_ex), paste("Number of nodes:", nrow(actors_ex)))
  expect_output(
    print(actors_ex),
    paste("Number of present nodes:", sum(actors_ex$present))
  )
  expect_output(print(actors_ex), "Dynamic attribute")
  expect_failure(expect_output(print(actors_ex, full = TRUE), "First \\d rows"))
  expect_output(
    print(make_nodes(testAttr)),
    paste("Number of nodes:", nrow(testAttr))
  )
  expect_failure(
    expect_output(print(make_nodes(testAttr)), "Number of present nodes:")
  )
  expect_failure(
    expect_output(print(make_nodes(testAttr)), "Dynamic attribute")
  )
  expect_output(print(make_nodes(testAttr)), "First 6 rows")
  expect_failure(
    expect_output(print(make_nodes(testAttr), full = TRUE), "First 6 rows")
  )
})
test_that("network print", {
  expect_output(
    print(networkState),
    paste("Dimensions:", paste(dim(networkState), collapse = " "))
  )
  expect_output(
    print(networkState),
    paste("Number of ties \\(no weighted\\):", sum(networkState > 0))
  )
  expect_output(print(networkState), "Nodes set\\(s\\): actors_ex")
  expect_output(print(networkState), "It is a one-mode and directed network")
  expect_output(print(networkState), "Linked events: eventsIncrement")
  expect_output(print(networkState), "First \\d rows and columns")
  expect_failure(
    expect_output(
      print(networkState, full = TRUE),
      "First \\d rows and columns"
    )
  )

  netTest <- make_network(matrix = m, nodes = actors_ex)
  expect_output(
    print(netTest),
    paste("Dimensions:", paste(dim(netTest), collapse = " "))
  )
  expect_output(
    print(netTest),
    paste("Number of ties \\(no weighted\\):", sum(netTest > 0, na.rm = TRUE))
  )
  expect_output(print(netTest), "Nodes set\\(s\\): actors_ex")
  expect_output(print(netTest), "It is a one-mode and directed network")
  expect_failure(
    expect_output(print(netTest), "Linked events: eventsIncrement")
  )
  expect_output(print(netTest), "First \\d rows and columns")
  expect_failure(
    expect_output(
      print(netTest, full = TRUE),
      "First \\d rows and columns"
    )
  )

  expect_output(
    print(networkActorClub),
    paste("Dimensions:", paste(dim(networkActorClub), collapse = " "))
  )
  expect_output(
    print(networkActorClub),
    paste("Number of ties \\(no weighted\\):", sum(networkActorClub))
  )
  expect_output(print(networkActorClub), "Nodes set\\(s\\): actors_ex clubsEx")
  expect_output(
    print(networkActorClub),
    "It is a two-mode and directed network"
  )
  expect_output(print(networkActorClub), "Linked events: eventsActorClub")
  expect_output(print(networkActorClub), "First \\d rows and columns")
  expect_failure(
    expect_output(
      print(networkActorClub, full = TRUE),
      "First \\d rows and columns"
    )
  )
})
test_that("dependent events", {
  expect_output(print(depNetwork), paste("Number of events:", nrow(depNetwork)))
  expect_output(print(depNetwork), "Nodes set\\(s\\): actors_ex")
  expect_output(print(depNetwork), "Default network: networkState")
  expect_output(print(depNetwork), "First \\d rows")
  expect_failure(
    expect_output(print(depNetwork, full = TRUE), "First \\d rows")
  )
})
test_that("preprocessed", {
  preproData <- estimate_dynam(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      tie(networkExog, weighted = TRUE),
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_output(print(preproData), "Preprocess object for the model")
})
test_that("tidy results", {
  expect_s3_class(tidy(resModObject), "tbl_df")
  expect_length(tidy(resModObject), 5)
  expect_equal(nrow(tidy(resModObject)), 2L)
  expect_equal(
    tidy(resModObject)$term,
    c("inertia_call_network", "trans_call_network")
  )
  expect_length(tidy(resModObject, conf.int = TRUE), 7)
  expect_equal(
    tidy(resModObject, complete = TRUE)$term,
    c("inertia_call_network", "recip_call_network_Fx", "trans_call_network")
  )
  expect_true(
    anyNA(tidy(resModObject, complete = TRUE, conf.int = TRUE)$statistic)
  )
})
test_that("tidy compact term = builder export output; valid names", {
  compactTerm <- tidy(resModObject, complete = TRUE)$term
  builderTerm <- compact_term_strings(resModObject$names, "export")
  expect_equal(compactTerm, unname(builderTerm))
  expect_true(all(make.names(compactTerm) == compactTerm))
  expect_false(anyDuplicated(builderTerm) > 0)
})
test_that("tidy compact = FALSE keeps multi-column form, no dot columns", {
  out <- tidy(resModObject, compact = FALSE, complete = TRUE)
  expect_true(all(c("term", "Object", "fixed") %in% names(out)))
  expect_false(any(startsWith(names(out), ".")))
  expect_equal(out$term, c("inertia", "recip", "trans"))
})
test_that("glance results", {
  expect_s3_class(glance(resModObject), "tbl_df")
  expect_length(glance(resModObject), 5)
  expect_equal(nrow(glance(resModObject)), 1L)
  expect_equal(glance(resModObject)$logLik, resModObject$log_likelihood)
})

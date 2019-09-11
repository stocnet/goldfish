context("Diff preprocessingInit objects(Fisheries Treaties)")

test_that("Using DyNAM choice with data Fisheries Treaties", {
  prep_1 <- estimate(createBilat ~ inertia(bilatnet) +
    indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
    tie(contignet, ignoreRep = TRUE) +
    alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE) +
    alter(states$gdp, ignoreRep = TRUE) + diff(states$gdp, ignoreRep = TRUE),
  model = "DyNAM", subModel = "choice", preprocessingOnly = TRUE, silent = TRUE
  )
  prep_2 <- estimate(createBilat ~ inertia(bilatnet) +
    indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE)
    + diff(states$gdp, ignoreRep = TRUE),
  model = "DyNAM", subModel = "choice", preprocessingOnly = TRUE, silent = TRUE
  )
  time <- system.time(res <- estimate(createBilat ~ inertia(bilatnet) +
    indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
    tie(contignet, ignoreRep = TRUE) +
    alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE),
  model = "DyNAM", subModel = "choice", silent = TRUE
  ))

  time_1 <- system.time(res_1 <- estimate(createBilat ~ inertia(bilatnet) +
    indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
    tie(contignet, ignoreRep = TRUE) +
    alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE),
  model = "DyNAM", subModel = "choice", preprocessingInit = prep_1, silent = TRUE
  ))
  time_2 <- system.time(res_2 <- estimate(createBilat ~ inertia(bilatnet) +
    indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
    tie(contignet, ignoreRep = TRUE) +
    alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE),
  model = "DyNAM", subModel = "choice", preprocessingInit = prep_2, silent = TRUE
  ))

  expect_equal(res$parameters, res_1$parameters) # They should be the same
  expect_equal(res$parameters, res_2$parameters) # They should be the same
  # Test whether it's faster
  # expect_true(as.numeric(time_1[3]) <= as.numeric(time[3]) )
  # Using PreprocessingInit should not make the estimation slower
  # expect_true(as.numeric(time_2[3]) <= as.numeric(time[3]) )
  # Using PreprocessingInit should not make the estimation slower
})

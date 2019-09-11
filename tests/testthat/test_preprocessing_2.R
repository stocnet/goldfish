context("Preprocessing Fisheries Treaties")

test_that("DyNAM-choice_coordination", {
  prep <- estimate(createBilat ~ inertia(bilatnet) +
                     indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
                     tie(contignet, ignoreRep = TRUE) + node_trans(bilatnet, ignoreRep = TRUE) +
                     alter(states$regime, ignoreRep = TRUE) + same(states$regime, ignoreRep = TRUE) +
                     alter(states$gdp, ignoreRep = TRUE) + diff(states$gdp, ignoreRep = TRUE) +
                     tertius_diff(bilatnet, states$gdp, ignoreRep = TRUE) +
                     tertius(bilatnet, states$gdp, ignoreRep = TRUE),
                   model = "DyNAM", subModel = "choice_coordination", preprocessingOnly = TRUE, silent = TRUE)
  expect_match(class(prep), "preprocessed.goldfish")
})


test_that("DyNAM-rate", {
  prep <- estimate(createBilat ~
                     indeg(bilatnet, ignoreRep = TRUE) + outdeg(bilatnet, ignoreRep = TRUE) +
                     node_trans(bilatnet, ignoreRep = TRUE) +
                     ego(states$regime, ignoreRep = TRUE) +
                     ego(states$gdp, ignoreRep = TRUE) +
                     tertius(bilatnet, states$gdp, ignoreRep = TRUE),
                   model = "DyNAM", subModel = "rate", preprocessingOnly = TRUE, silent = TRUE)
  expect_match(class(prep), "preprocessed.goldfish")
})

test_that("REM-rate", {
  prep <- estimate(createBilat ~ 1 + inertia(bilatnet) +
                     indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
                     tie(contignet, ignoreRep = TRUE) + indeg(bilatnet, ignoreRep = TRUE, type = "ego") +
                     alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE) +
                     alter(states$gdp, ignoreRep = TRUE) + diff(states$gdp, ignoreRep = TRUE) +
                     tertius(bilatnet, states$gdp, ignoreRep = TRUE, type = "alter"),
                   model = "REM", subModel = "choice", preprocessingOnly = TRUE, silent = TRUE)
  expect_match(class(prep), "preprocessed.goldfish")
})

test_that("REM-rate-ordered", {
  prep <- estimate(createBilat ~ inertia(bilatnet) +
                     indeg(bilatnet, ignoreRep = TRUE) + trans(bilatnet, ignoreRep = TRUE) +
                     tie(contignet, ignoreRep = TRUE) +
                     alter(states$regime, ignoreRep = TRUE) + diff(states$regime, ignoreRep = TRUE) +
                     alter(states$gdp, ignoreRep = TRUE) + diff(states$gdp, ignoreRep = TRUE) +
                     tertius(bilatnet, states$gdp, ignoreRep = TRUE, type = "alter"),
                   model = "REM", subModel = "choice", preprocessingOnly = TRUE, silent = TRUE)
  expect_match(class(prep), "preprocessed.goldfish")
})

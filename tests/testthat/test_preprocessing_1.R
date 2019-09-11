context("Preprocessing Social Evolution")

test_that("DyNAM-choice Structural", {
  prep <- estimate(callsDependent ~ inertia + recip + trans + indeg + outdeg + four + node_trans,
    model = "DyNAM", subModel = "choice",
    preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("DyNAM-choice Attribute", {
  prep <- estimate(callsDependent ~ alter(actors$floor) + same(actors$floor) + sim(actors$floor),
                   model = "DyNAM", subModel = "choice",
                   preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("DyNAM-choice diff", {
  prep <- estimate(callsDependent ~ diff(actors$floor) + tertius_diff(callNetwork, actors$floor) +
                     tertius(callNetwork, actors$gradeType),
                   model = "DyNAM", subModel = "choice",
                   preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish", label = "diff")
})
test_that("DyNAM-rate-ordered", {
  prep <- estimate(callsDependent ~ node_trans + indeg + outdeg + ego(actors$floor) +
                     tertius(callNetwork, actors$gradeType),
    model = "DyNAM", subModel = "rate",
    preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("DyNAM-rate", {
  prep <- estimate(callsDependent ~ 1 + node_trans + indeg + outdeg + ego(actors$floor) +
                     tertius(callNetwork, actors$gradeType),
    model = "DyNAM", subModel = "rate",
    preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("REM-ordered structural", {
  prep <- estimate(callsDependent ~ inertia + recip + trans + indeg + outdeg + four + node_trans +
                     indeg(callNetwork, type = "ego") + outdeg(callNetwork, type = "ego"),
    model = "REM", subModel = "choice",
    preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("REM-ordered attribute", {
  prep <- estimate(callsDependent ~ ego(actors$floor) + alter(actors$floor) + same(actors$floor) + sim(actors$floor),
                   model = "REM", subModel = "choice",
                   preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("REM-ordered diff", {
  prep <- estimate(callsDependent ~ diff(actors$floor) + tertius_diff(callNetwork, actors$floor) +
                     tertius(callNetwork, actors$gradeType, type = "ego"),
                   model = "REM", subModel = "choice",
                   preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})
test_that("REM-rate", {
  prep <- estimate(callsDependent ~ 1 + inertia + recip + trans + indeg + outdeg + four + node_trans +
                     indeg(callNetwork, type = "ego") + outdeg(callNetwork, type = "ego") +
                     tertius(callNetwork, actors$gradeType, type = "alter"),
    model = "REM", subModel = "choice",
    preprocessingOnly = TRUE, silent = TRUE
  )
  expect_match(class(prep), "preprocessed.goldfish")
})

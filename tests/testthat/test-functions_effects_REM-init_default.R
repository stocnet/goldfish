test_that("REM default and ego init return the same result", {
  expect_equal(
    init_REM_choice.ego(effectFUN_REM_ego, attr$fishingComplete, 8, 8),
    init_DyNAM_choice.default(effectFUN_REM_ego, network = NULL, attribute = attr$fishingComplete,
                              window = NULL, n1 = 8, n2 = 8)
  )
  expect_equal(
    init_REM_choice.ego(effectFUN_REM_ego, attr$fishingComplete, 8, 8),
    init_DyNAM_choice.default(effectFUN_REM_ego, network = NULL, attribute = attr$fishingComplete,
                              window = 1, n1 = 8, n2 = 8),
    label = "when window is not NULL"
  )
})

test_that("REM default and diff init return the same result", {
  expect_equal(
    init_REM_choice.diff(effectFUN_REM_diff, attr$fishingComplete)$stat,
    init_DyNAM_choice.default(effectFUN_REM_diff, network = NULL, attribute = attr$fishingComplete,
                              window = NULL, n1 = 8, n2 = 8)$stat
  )
  expect_equal(
    init_REM_choice.diff(effectFUN_REM_diff, attr$fishingComplete),
    init_DyNAM_choice.default(effectFUN_REM_diff, network = NULL, attribute = attr$fishingComplete,
                              window = 1, n1 = 8, n2 = 8),
    label = "when window is not NULL"
  )
})

test_that("REM default and same init return the same result", {
  expect_equal(
    init_DyNAM_choice.same(effectFUN_REM_sim, attr$fishingComplete)$stat,
    init_DyNAM_choice.default(effectFUN_REM_sim, network = NULL, attribute = attr$fishingComplete,
                              window = NULL, n1 = 8, n2 = 8)$stat
  )
  expect_equal(
    init_DyNAM_choice.same(effectFUN_REM_sim, attr$fishingComplete),
    init_DyNAM_choice.default(effectFUN_REM_sim, network = NULL, attribute = attr$fishingComplete,
                              window = 1, n1 = 8, n2 = 8),
    label = "when window is not NULL"
  )
})

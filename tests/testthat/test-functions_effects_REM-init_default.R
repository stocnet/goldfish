test_that("REM default and ego init return the same result", {
  expect_equal(
    init_REM_choice.ego(effectFUN_REM_ego, testAttr$fishingComplete, 8, 8),
    init_DyNAM_choice.default(effectFUN_REM_ego,
      network = NULL, attribute = testAttr$fishingComplete,
      window = NULL, n1 = 8, n2 = 8
    )
  )
  expect_equal(
    init_REM_choice.ego(effectFUN_REM_ego, testAttr$fishingComplete, 8, 8),
    init_DyNAM_choice.default(effectFUN_REM_ego,
      network = NULL, attribute = testAttr$fishingComplete,
      window = 1, n1 = 8, n2 = 8
    ),
    label = "when window is not NULL"
  )
})

test_that("REM default and diff init return the same result", {
  expect_equal(
    init_REM_choice.diff(effectFUN_REM_diff, testAttr$fishingComplete)$stat,
    init_DyNAM_choice.default(effectFUN_REM_diff,
      network = NULL, attribute = testAttr$fishingComplete,
      window = NULL, n1 = 8, n2 = 8
    )$stat
  )
  expect_equal(
    init_REM_choice.diff(effectFUN_REM_diff, testAttr$fishingComplete),
    init_DyNAM_choice.default(effectFUN_REM_diff,
      network = NULL, attribute = testAttr$fishingComplete,
      window = 1, n1 = 8, n2 = 8
    ),
    label = "when window is not NULL"
  )
})

test_that("REM default and same init return the same result", {
  expect_equal(
    init_DyNAM_choice.same(effectFUN_REM_sim, testAttr$fishingComplete)$stat,
    init_DyNAM_choice.default(effectFUN_REM_sim,
      network = NULL, attribute = testAttr$fishingComplete,
      window = NULL, n1 = 8, n2 = 8
    )$stat
  )
  expect_equal(
    init_DyNAM_choice.same(effectFUN_REM_sim, testAttr$fishingComplete),
    init_DyNAM_choice.default(effectFUN_REM_sim,
      network = NULL, attribute = testAttr$fishingComplete,
      window = 1, n1 = 8, n2 = 8
    ),
    label = "when window is not NULL"
  )
})

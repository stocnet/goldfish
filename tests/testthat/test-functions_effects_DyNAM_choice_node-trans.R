test_that("init four initialises empty when it has a window or is empty", {
  expect_equal(init_DyNAM_choice.four(effectFUN, m0, NULL, 5, 5)$cache, m0)
  expect_equal(init_DyNAM_choice.four(effectFUN, m, 1, 5, 5)$cache, m0)
})

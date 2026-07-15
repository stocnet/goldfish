test_that("init tertius_diff initialises empty when it has a window or is empty", {
  expect_equal(
    init_DyNAM_choice.tertius_diff(
      effectFUN,
      m0,
      actors$floor,
      NULL,
      5,
      5
    )$cache,
    rep(0, 5)
  )
  expect_equal(
    init_DyNAM_choice.tertius_diff(effectFUN, m, actors$floor, 1, 5, 5)$cache,
    rep(0, 5)
  )
})

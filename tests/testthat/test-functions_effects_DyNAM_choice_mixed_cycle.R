test_that("mixed_cycle returns a valid object on update", {
  # Testing netUpdate = 1
  expect_type(update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 5, 1, m0), "list")
  # Testing netUpdate = 2
  expect_type(update_DyNAM_choice_mixed_cycle(list(m, m1), 1, 5, 5, 2, m0), "list")
})

test_that("mixed_cycle doesn't update when replace == oldValue", {
  # Testing netUpdate = 1
  expect_equal(update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 0, 1, m0)$cache, m0)
  # Testing netUpdate = 2
  expect_equal(update_DyNAM_choice_mixed_cycle(list(m, m1), 1, 5, 0, 2, m0)$cache, m0)
})

test_that("update_mixed_cycle doesn't update when sender == receiver", {
  expect_equal(update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 4, 5, 1, m0)$cache, m0)
})

test_that("update_mixed_cycle throws error for wrong netUpdate", {
  expect_error(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 5, 3, m0),
    "Check that you only declare two networks as argument."
  )
})

test_that("init mixed_cycle handles dimensions and windows", {
  # Successful init
  expect_type(init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), NULL, 5, 5), "list")
  # Dimension mismatch
  expect_error(init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), NULL, 4, 5), "Non conformable dimensions sizes")
  # Window init (should be empty)
  expect_type(init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), 1, 5, 5), "list")
})
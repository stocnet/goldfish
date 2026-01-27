test_that("mixed_common_sender returns a valid object on update", {
  # error when null
  # Testing netUpdate = 1
  expect_type(update_DyNAM_choice_mixed_common_sender(list(m, m1), 5, 3, 2, 1, m0), "list")
  # Testing netUpdate = 2
  expect_type(update_DyNAM_choice_mixed_common_sender(list(m, m1), 5, 3, 2, 2, m0), "list")
})

test_that("mixed_common_sender doesn't update when replace == oldValue", {
  # Testing netUpdate = 1
  expect_equal(update_DyNAM_choice_mixed_common_sender(list(m, m1), 5, 3, 0, 1, m0)$cache, m0)
  # Testing netUpdate = 2
  expect_equal(update_DyNAM_choice_mixed_common_sender(list(m, m1), 5, 3, 0, 2, m0)$cache, m0)
})

test_that("mixed_common_sender doesn't update when sender == sender", {
  expect_equal(update_DyNAM_choice_mixed_common_sender(list(m, m1), 5, 5, 0, 2, m0)$cache, m0)
})

test_that("update_mixed_common_sender throws error for wrong netUpdate", {
  expect_error(
    update_DyNAM_choice_mixed_common_sender(list(m, m1), 4, 3, 5, 3, m0),
    "Check that only two networks are declared in the 'network' argument"
  )
})

test_that("init mixed_common_sender checks two-mode and dimensions", {
  # Error on two-mode
  params_two_mode <- function(is_two_mode = TRUE, transformer_fn = identity) {
    NULL
  }
  expect_error(init_DyNAM_choice.mixed_common_sender(params_two_mode, list(m, m1), NULL, 5, 5), "must not use when is a two-mode network")
  # Successful init
  expect_type(init_DyNAM_choice.mixed_common_sender(effectFUN, list(m, m1), NULL, 5, 5), "list")
})

test_that("init mixed_common_sender initialises empty when it has a window or is empty", {
  expect_equal(init_DyNAM_choice.mixed_common_sender(effectFUN, list(m, m1), 3, 5, 5)$cache, matrix(0, nrow = 5, ncol = 5))
})

test_that("REM and DyNAM mixed_common_sender return the same result", {
  expect_equal(
    init_REM_choice.mixed_common_sender(effectFUN, list(m, m1), 1, 5, 5),
    init_DyNAM_choice.mixed_common_sender(effectFUN, list(m, m1), 1, 5, 5),
    label = "for init"
  )
  expect_equal(
    update_REM_choice_mixed_common_sender(list(m, m1), 5, 3, 2, 1, m0),
    update_DyNAM_choice_mixed_common_sender(list(m, m1), 5, 3, 2, 1, m0),
    label = "for update"
  )
})
test_that("alter returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_alter(
      attribute = testAttr$fishingSkill, node = 1, replace = 1, n1 = 8, n2 = 0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_alter(
        attribute = testAttr$fishingSkill, node = 1, replace = 1, n1 = 8, n2 = 0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
})

test_that("alter returns NULL if there is no change", {
  expect_null(
    update_DyNAM_choice_alter(
      testAttr$fishingSkill,
      node = 1, replace = 10, n1 = 8, n2 = 0
    )$changes
  )
  # expect_null(
  #   update_DyNAM_choice_alter(
  #     testAttr$fishingSkill,
  #     node = 2, replace = NA, n1 = 8, n2 = 0
  #   )$changes,
  #   label = "when previous value and replace are NA"
  # )
})

test_that("alter returns correct attributes on update", {
  expect_equal(
    update_DyNAM_choice_alter(
      testAttr$fishingSkill,
      node = 1, replace = 1, n1 = 8, n2 = 0
    )$changes,
    cbind(node1 = 2:8, node2 = rep(1, 7), replace = rep(1, 7))
  )
  expect_equal(
    update_DyNAM_choice_alter(
      testAttr$fishingSkill,
      node = 1, replace = 0, n1 = 8, n2 = 0
    )$changes,
    cbind(node1 = 2:8, node2 = rep(1, 7), replace = rep(0, 7)),
    label = "when replace is 0"
  )
  # expect_equal(
  #   update_DyNAM_choice_alter(
  #     testAttr$fishingSkill,
  #     node = 8, replace = 1, n1 = 8, n2 = 0
  #   )$changes,
  #   cbind(node1 = 1:7, node2 = rep(8, 7), replace = rep(1, 7)),
  #   label = "when previous value was NA"
  # )
})

test_that("alter init returns the correct result when TwoMode = FALSE", {
  expect_equal(
    diag(init_DyNAM_choice.alter(effectFUN, testAttr$fishingSkill, 8, 8)$stat),
    rep(0,8),
    label = "diagonal isn't the attribute")
})

test_that("alter init returns the correct result when TwoMode = TRUE", {
  check = formals(effectFUN)
  check$isTwoMode = TRUE
  formals(effectFUN) <- check
  expect_equal(
    diag(init_DyNAM_choice.alter(effectFUN, testAttr$fishingSkill, 8, 8)$stat),
    testAttr$fishingSkill,
    label = "diagonal isn't the attribute")
  expect_equal(
    init_DyNAM_choice.alter(effectFUN, testAttr$fishingSkill, 8, 8)$stat,
    matrix(testAttr$fishingSkill, nrow = 8, ncol = 8, byrow = TRUE)
           )
})

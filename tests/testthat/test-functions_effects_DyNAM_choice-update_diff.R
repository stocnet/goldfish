test_that("diff returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_diff(
      attribute = testAttr$fishingSkill, node = 1, replace = 1,
      n1 = 8, n2 = 0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_diff(
        attribute = testAttr$fishingSkill, node = 1, replace = 1,
        n1 = 8, n2 = 0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
})

test_that("diff equals NULL if there is no change", {
  expect_null(update_DyNAM_choice_diff(
    testAttr$fishingSkill,
    node = 7, replace = 3,
    n1 = 8, n2 = 0
  )$changes)
  # expect_null(
  #   update_DyNAM_choice_diff(testAttr$fishingSkill,
  #     node = 2, replace = NA,
  #     n1 = 8, n2 = 0
  #   )$changes,
  #   label = "when replace is NA"
  # )
})

test_that("diff returns correct attributes on update", {
  repVal0 <- testAttr$fishingSkill[-1]
  expect_equal(
    update_DyNAM_choice_diff(testAttr$fishingSkill,
      node = 5, replace = 10,
      n1 = 8, n2 = 0
    )$changes,
    rbind(
      cbind(
        node1 = rep(5, 7), node2 = c(1:4, 6:8),
        replace = c(0, NA, 5, 0, 2, 7, NA)
      ),
      cbind(
        node1 = c(1:4, 6:8), node2 = rep(5, 7),
        replace = c(0, NA, 5, 0, 2, 7, NA)
      )
    )
  )
  expect_equal(
    update_DyNAM_choice_diff(testAttr$fishingSkill,
      node = 1, replace = 0,
      n1 = 8, n2 = 0
    )$changes,
    rbind(
      cbind(node1 = rep(1, 7), node2 = 2:8, replace = repVal0),
      cbind(node1 = 2:8, node2 = rep(1, 7), replace = repVal0)
    ),
    label = "when replace is 0"
  )
  # expect_equal(
  #   update_DyNAM_choice_diff(testAttr$fishingSkill,
  #     node = 1, replace = NA,
  #     n1 = 8, n2 = 0
  #   )$changes,
  #   rbind(
  #     cbind(
  #       node1 = rep(1, 7), node2 = 2:8,
  #       replace = c(NA, 1.8, 3.2, 1.2, 1.2, 3.8, NA)
  #     ),
  #     cbind(
  #       node1 = 2:8, node2 = rep(1, 7),
  #       replace = c(NA, 1.8, 3.2, 1.2, 1.2, 3.8, NA)
  #     )
  #   ),
  #   label = "when replace is NA"
  # )
  # expect_equal(
  #   update_DyNAM_choice_diff(testAttr$fishingSkill,
  #     node = 2, replace = 10,
  #     n1 = 8, n2 = 0
  #   )$changes,
  #   rbind(
  #     cbind(
  #       node1 = rep(2, 7), node2 = c(1, 3:8),
  #       replace = c(0, 5, 0, 2, 2, 7, NA)
  #     ),
  #     cbind(
  #       node1 = c(1, 3:8), node2 = rep(2, 7),
  #       replace = c(0, 5, 0, 2, 2, 7, NA)
  #     )
  #   ),
  #   label = "when old value was NA"
  # )
})

test_that("diff init throws an error when two-mode network", {
  # effectFUN has is_two_mode=FALSE so we must change this prior to the test
  check <- formals(effectFUN)
  check$is_two_mode <- TRUE
  formals(effectFUN) <- check
  expect_error(init_DyNAM_choice.diff(effectFUN, m1, NULL, 5, 5),
               regexp = "doesn't work in two mode networks")
})
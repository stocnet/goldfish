test_that("sim returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_sim(
      attribute = testAttr$fishingSkill,
      node = 1,
      replace = 1,
      n1 = 8,
      n2 = 0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_sim(
        attribute = testAttr$fishingSkill,
        node = 1,
        replace = 1,
        n1 = 8,
        n2 = 0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
})

test_that("sim equals NULL if there is no change", {
  expect_null(
    update_DyNAM_choice_sim(
      testAttr$fishingSkill,
      node = 7,
      replace = 3,
      n1 = 8,
      n2 = 0
    )$changes
  )
  # expect_null(
  #   update_DyNAM_choice_sim(
  #     testAttr$fishingSkill,
  #     node = 2, replace = NA,
  #     n1 = 8, n2 = 0
  #   )$changes,
  #   label = "when replace is NA"
  # )
})

test_that("sim returns correct attributes on update", {
  expect_equal(
    update_DyNAM_choice_sim(
      testAttr$fishingSkill,
      node = 5,
      replace = 10,
      n1 = 8,
      n2 = 0
    )$changes,
    rbind(
      cbind(
        node1 = rep(5, 7),
        node2 = c(1:4, 6:8),
        replace = c(0, NA, -5, 0, -2, -7, NA)
      ),
      cbind(
        node1 = c(1:4, 6:8),
        node2 = rep(5, 7),
        replace = c(0, NA, -5, 0, -2, -7, NA)
      )
    )
  )
  expect_equal(
    update_DyNAM_choice_sim(
      testAttr$fishingSkill,
      node = 1,
      replace = 0,
      n1 = 8,
      n2 = 0
    )$changes,
    rbind(
      cbind(
        node1 = rep(1, 7),
        node2 = 2:8,
        replace = -testAttr$fishingSkill[-1]
      ),
      cbind(
        node1 = 2:8,
        node2 = rep(1, 7),
        replace = -testAttr$fishingSkill[-1]
      )
    ),
    label = "when replace is 0"
  )
  # expect_equal(
  #   update_DyNAM_choice_sim(
  #     testAttr$fishingSkill,
  #     node = 1, replace = NA,
  #     n1 = 8, n2 = 0
  #   )$changes,
  #   rbind(
  #     cbind(
  #       node1 = rep(1, 7), node2 = 2:8,
  #       replace = c(NA, -1.8, -3.2, -1.2, -1.2, -3.8, NA)
  #     ),
  #     cbind(
  #       node1 = 2:8, node2 = rep(1, 7),
  #       replace = c(NA, -1.8, -3.2, -1.2, -1.2, -3.8, NA)
  #     )
  #   ),
  #   label = "when replace is NA"
  # )
  # expect_equal(
  #   update_DyNAM_choice_sim(
  #     testAttr$fishingSkill,
  #     node = 2, replace = 10,
  #     n1 = 8, n2 = 0
  #   )$changes,
  #   rbind(
  #     cbind(
  #       node1 = rep(2, 7), node2 = c(1, 3:8),
  #       replace = c(0, -5, 0, -2, -2, -7, NA)
  #     ),
  #     cbind(
  #       ndoe1 = c(1, 3:8), node2 = rep(2, 7),
  #       replace = c(0, -5, 0, -2, -2, -7, NA)
  #     )
  #   ),
  #   label = "when old value was NA"
  # )
})

test_that("sim init negates the cross-side difference on a two-mode network", {
  # This used to abort as two-mode-incompatible; `sim` is the negated `diff`
  # and is as well defined across sides as `diff` is.
  check <- formals(effectFUN)
  check$is_two_mode <- TRUE
  formals(effectFUN) <- check

  ego <- c(1, 4, 9)
  alter <- c(2, 3)
  stat <- init_DyNAM_choice.sim(effectFUN, list(ego, alter), NULL, 3, 2)$stat

  expect_equal(dim(stat), c(3L, 2L))
  expect_equal(stat, (-1) * outer(ego, alter, "-"))
})

test_that("sim init returns the correct result", {
  expect_equal(
    init_DyNAM_choice.sim(effectFUN, testAttr$fishingSkill, NULL, 8, 8)$stat,
    (-1) * outer(testAttr$fishingSkill, testAttr$fishingSkill, "-")
  )
})

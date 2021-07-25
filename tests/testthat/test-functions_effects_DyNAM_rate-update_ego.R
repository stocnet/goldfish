
test_that("ego returns a valid matrix", {
  expect_type(
    update_DyNAM_rate_ego(
      testAttr$fishingSkill,
      node = 1, replace = 10,
      n1 = 8, n2 = 8, isTwoMode = FALSE
    ),
    "list"
  )
})
test_that("ego returns NULL if there is no change", {
  expect_null(
    update_DyNAM_rate_ego(
      testAttr$fishingSkill,
      node = 1, replace = 10,
      n1 = 8, n2 = 8, isTwoMode = FALSE
    )$changes
  )
})
test_that("ego returns correct attributes on update", {
  expect_equal(
    update_DyNAM_rate_ego(
      testAttr$fishingSkill,
      node = 1, replace = 9,
      n1 = 8, n2 = 8, isTwoMode = FALSE
    )$changes,
    cbind(node1 = rep(1, 7), node2 = 2:8, replace = rep(9, 7))
  )
  expect_equal(
    update_DyNAM_rate_ego(
      testAttr$fishingSkill,
      node = 1, replace = 0,
      n1 = 8, n2 = 8, isTwoMode = FALSE
    )$changes,
    cbind(node1 = rep(1, 7), node2 = 2:8, replace = rep(0, 7)),
    label = "when replace is 0"
  )
  expect_equal(
    update_DyNAM_rate_ego(
      testAttr$fishingSkill,
      node = 1, replace = NA,
      n1 = 8, n2 = 8, isTwoMode = FALSE
    )$changes,
    cbind(node1 = rep(1, 7), node2 = 2:8, replace = rep(6.8, 7)), # replace by average
    label = "when replace is NA"
  )
  expect_equal(
    update_DyNAM_rate_ego(
      testAttr$fishingSkill,
      node = 8, replace = 1,
      n1 = 8, n2 = 8, isTwoMode = FALSE
    )$changes,
    cbind(node1 = rep(8, 7), node2 = 1:7, replace = rep(1, 7)),
    label = "when previous value was NA"
  )
})

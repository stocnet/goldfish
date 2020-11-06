test_that("diff returns a valid object on update", {
  expect_is(update_DyNAM_choice_diff(attribute = testAttr$fishingSkill, node = 1, replace = 1,
                                      n1 = 8, n2 = 0),
            "list")
  expect_is(update_DyNAM_choice_diff(attribute = testAttr$fishingSkill, node = 1, replace = 1,
                                      n1 = 8, n2 = 0)$changes,
            "matrix")
})

test_that("diff equals NULL if there is no change", {
  expect_null(update_DyNAM_choice_diff(testAttr$fishingSkill, node = 7, replace = 3,
                                       n1 = 8, n2 = 0)$changes)
  expect_null(update_DyNAM_choice_diff(testAttr$fishingSkill, node = 2, replace = NA,
                                       n1 = 8, n2 = 0)$changes,
              label = "when replace is NA")
})

test_that("diff returns correct attributes on update", {
  expect_equivalent(update_DyNAM_choice_diff(testAttr$fishingSkill, node = 5, replace = 10,
                                             n1 = 8, n2 = 0)$changes,
                    rbind(cbind(rep(5, 7), c(1:4, 6:8), c(0, NA, 5, 0, 2, 7, NA)),
                          cbind(c(1:4, 6:8), rep(5 ,7), c(0, NA, 5, 0, 2, 7, NA))))
  expect_equivalent(update_DyNAM_choice_diff(testAttr$fishingSkill, node = 1, replace = 0,
                                             n1 = 8, n2 = 0)$changes,
                    rbind(cbind(rep(1, 7), 2:8, testAttr$fishingSkill[-1]),
                          cbind(2:8, rep(1, 7), testAttr$fishingSkill[-1])),
                    label = "when replace is 0")
  expect_equivalent(update_DyNAM_choice_diff(testAttr$fishingSkill, node = 1, replace = NA,
                                             n1 = 8, n2 = 0)$changes,
                    rbind(cbind(rep(1, 7), 2:8, c(NA, 1.8, 3.2, 1.2, 1.2, 3.8, NA)),
                          cbind(2:8, rep(1, 7), c(NA, 1.8, 3.2, 1.2, 1.2, 3.8, NA))),
                    label = "when replace is NA")
  expect_equivalent(update_DyNAM_choice_diff(testAttr$fishingSkill, node = 2, replace = 10,
                                             n1 = 8, n2 = 0)$changes,
                    rbind(cbind(rep(2, 7), c(1,3:8), c(0, 5, 0, 2, 2, 7, NA)),
                          cbind(c(1,3:8), rep(2, 7), c(0, 5, 0, 2, 2, 7, NA))),
                    label = "when old value was NA")
})

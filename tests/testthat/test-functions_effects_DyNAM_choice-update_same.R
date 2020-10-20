test_that("same returns a valid object on update", {
  expect_is(update_DyNAM_choice_same(attribute = testAttr$fishingSkill, node = 1, replace = 1),
            "list")
  expect_is(update_DyNAM_choice_same(attribute = testAttr$fishingSkill, node = 1, replace = 1)$changes,
            "matrix")
})

test_that("same returns NULL if there is no change", {
  expect_null(update_DyNAM_choice_same(testAttr$fishingSkill, node = 1, replace = 10)$changes)
  expect_null(update_DyNAM_choice_same(testAttr$fishSizeMean, node = 1, replace = 0.15)$changes,
              label = "when no match results from update")
  expect_null(update_DyNAM_choice_same(testAttr$fishingSkill, node = 7, replace = 2)$changes,
              label = "when new and old attribute have no match")
  expect_null(update_DyNAM_choice_same(testAttr$fishingSkill, node = 2, replace = NA)$changes,
              label = "when replace is NA")
})

test_that("same returns correct attributes on update", {
  expect_equivalent(update_DyNAM_choice_same(testAttr$fishingSkill, node = 2, replace = 10)$changes,
                    rbind(c(2, 1, 1),
                          c(2, 4, 1),
                          c(1, 2, 1),
                          c(4, 2, 1)),
                    label = "when new attribute creates additional matches")
  expect_equivalent(update_DyNAM_choice_same(testAttr$fishingSkill, node = 1, replace = 2)$changes,
                    rbind(c(1, 4, 0),
                          c(4, 1, 0)),
                    label = "when the new attribute removes a previous match")
  expect_equivalent(update_DyNAM_choice_same(testAttr$fishingSkill, node = 1, replace = NA)$changes,
                    rbind(c(1, 4, 0),
                          c(4, 1, 0)),
                    label = "when replace is NA and removes a previous match")
  expect_equivalent(update_DyNAM_choice_same(testAttr$fishingSkill, node = 8, replace = 10)$changes,
                    rbind(c(8, 1, 1),
                          c(8, 4, 1),
                          c(1, 8, 1),
                          c(4, 8, 1)),
                    label = "when previous value was NA")
})

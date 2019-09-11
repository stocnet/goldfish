context("Effect ego")

test_that("ego returns a valid matrix", {
  expect_is(ego(attr$fishingSkill, node = 1, replace = 10),
            "matrix")
})
test_that("ego returns NULL if there is no change", {
  expect_null(ego(attr$fishingSkill, node = 1, replace = 10))
})
test_that("ego returns correct attributes on update", {
  expect_equivalent(ego(attr$fishingSkill, node = 1, replace = 10),
                    cbind(rep(1,8), 1:8, rep(10,8)))
  expect_equivalent(ego(attr$fishingSkill, node = 1, replace = 0),
                    cbind(rep(1,8), 1:8, rep(0,8)),
                    label = "when replace is 0")
  expect_equivalent(ego(attr$fishingSkill, node = 1, replace = NA),
                    cbind(rep(1,8), 1:8, rep(NA,8)),
                    label = "when replace is NA")
  expect_equivalent(ego(attr$fishingSkill, node = 8, replace = 1),
                    cbind(rep(8,8), 1:8, rep(1,8)),
                    label = "when previous value was NA")
})

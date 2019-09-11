test_that("four returns a valid object on update", {
  expect_is(update_DyNAM_choice_four(m, sender = 1, receiver = 5, replace = 1,
                                      cache = m0),
            "list")
  expect_is(update_DyNAM_choice_four(m, sender = 1, receiver = 5, replace = 1,
                                      cache = m0)$changes,
            "matrix")
  expect_length(update_DyNAM_choice_four(m, sender = 1, receiver = 5, replace = 1,
                                          cache = m0)$changes[1, ],
                3)
})

test_that("four NULL if there is no change", {
  expect_null(update_DyNAM_choice_four(mTwoMode, sender = 1, receiver = 2, replace = 1,
                                       cache = m0)$changes)
  expect_null(update_DyNAM_choice_four(mTwoMode, sender = 1, receiver = 5, replace = NA,
                                       cache = m0)$changes,
              label = "when previous value and replace are NA")
})

test_that("four recognizes tie creation correctly", {
  expect_equivalent(update_DyNAM_choice_four(mTwoMode, sender = 1, receiver = 4, replace = 1,
                                             cache = m0)$changes,
                    rbind(c(1, 2, 1),
                          c(3, 2, 1),
                          c(3, 4, 1),
                          c(1, 5, 1)),
                    label = "when tie i -> l is created")
  expect_null(update_DyNAM_choice_four(mTwoMode, sender = 1, receiver = 1, replace = 1,
                                             cache = m0)$changes,
                    label = "when sender and receiver are the same node") # NULL, not self-loops
  expect_equivalent(update_DyNAM_choice_four(mTwoMode, sender = 1, receiver = 5, replace = 1,
                                             cache = m0)$changes,
                    rbind(c(1, 2, 1),
                          c(3, 2, 1),
                          c(1, 4, 1),
                          c(3, 5, 1)),
                    label = "when previous value was NA")
  expect_equivalent(update_DyNAM_choice_four(mTwoMode, sender = 1, receiver = 2, replace = NA,
                                             cache = m0)$changes,
                    rbind(c(1, 4, 0), c(1, 5, 0)),
                    label = "when replace is NA")
})

# test_that("four recognizes tie deletion correctly", {
# })

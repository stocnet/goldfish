test_that("trans returns a valid object on update", {
  expect_is(update_DyNAM_choice_trans(m, sender = 1, receiver = 5, replace = 1,
                                      cache = m0),
            "list")
  expect_is(update_DyNAM_choice_trans(m, sender = 1, receiver = 5, replace = 1,
                                      cache = m0)$changes,
            "matrix")
  expect_length(update_DyNAM_choice_trans(m, sender = 1, receiver = 5, replace = 1,
                                          cache = m0)$changes[1,],
                3)
})

test_that("trans returns NULL if there is no change", {
  expect_null(update_DyNAM_choice_trans(m, sender = 1, receiver = 2, replace = 1,
                                        cache = m0)$changes)
  expect_null(update_DyNAM_choice_trans(m, sender = 1, receiver = 1, replace = 0,
                                        cache = m0)$changes,
              label = "when sender and receiver are the same node")
  expect_null(update_DyNAM_choice_trans(m, sender = 5, receiver = 1, replace = NA,
                                        cache = m0)$changes,
              label = "when previous value and replace are NA")
  expect_null(update_DyNAM_choice_trans(m0, sender = 5, receiver = 4, replace = 1,
                                        cache = m0)$changes,
              label = "when change in tie composition has no effect")
  expect_null(update_DyNAM_choice_trans(m, sender = 1, receiver = 2, replace = 1.5,
                                        cache = m0)$changes,
              label = "when weighted is set to FALSE and an updated tie already exists")
})

test_that("trans recognizes tie creation correctly", {
  expect_equivalent(update_DyNAM_choice_trans(m, sender = 1, receiver = 4, replace = 1,
                                              cache = mCache)$changes,
                    rbind(c(1, 1, 1),
                          c(1, 5, 1),
                          c(2, 4, 1),
                          c(4, 4, 1)))
  expect_equivalent(update_DyNAM_choice_trans(m, sender = 5, receiver = 1, replace = 1,
                                              cache = mCache)$changes,
                    rbind(c(5, 2, 1),
                          c(5, 3, 1),
                          c(4, 1, 1)),
                    label = "when previous value was NA")
  expect_equivalent(update_DyNAM_choice_trans(m, sender = 1, receiver = 2, replace = NA,
                                              cache = mCache)$changes,
                    rbind(c(1, 1, -1),
                          c(1, 3,  0),
                          c(2, 2, -1),
                          c(4, 2, -1)),
                    label = "when replace is NA")
})

test_that("trans recognizes tie deletion correctly", {
  expect_equivalent(update_DyNAM_choice_trans(m, sender = 1, receiver = 2, replace = 0,
                                              cache = mCache)$changes,
                    rbind(c(1, 1, -1),
                          c(1, 3,  0),
                          c(2, 2, -1),
                          c(4, 2, -1)))
  expect_equivalent(update_DyNAM_choice_trans(m, sender = 5, receiver = 1, replace = 0,
                                              cache = mCache)$changes,
                    rbind(c(5, 2, 0),
                          c(5, 3, 0),
                          c(4, 1, 0)),
                    label = "when previous value was NA")
})

test_that("indeg returns a valid object on update", {
  expect_is(update_DyNAM_choice_indeg(m, sender = 1, receiver = 5, replace = 1,
                                      cache = vCache, n1 = 5, n2 = 0),
            "list")
  expect_is(update_DyNAM_choice_indeg(m, sender = 1, receiver = 5, replace = 1,
                                      cache = vCache, n1 = 5, n2 = 0)$changes,
            "matrix")
  expect_length(update_DyNAM_choice_indeg(m, sender = 1, receiver = 5, replace = 1,
                                          cache = vCache, n1 = 5, n2 = 0)$changes[1, ],
                3)
})

test_that("indeg returns NULL if there is no change", {
  expect_null(update_DyNAM_choice_indeg(m, sender = 1, receiver = 2, replace = 1,
                                        cache = vCache, n1 = 5, n2 = 0)$changes)
  expect_null(update_DyNAM_choice_indeg(m, sender = 1, receiver = 1, replace = 0,
                                        cache = vCache, n1 = 5, n2 = 0)$changes,
              label = "when sender and receiver are the same node")
  expect_null(update_DyNAM_choice_indeg(m, sender = 5, receiver = 1, replace = NA,
                                        cache = vCache, n1 = 5, n2 = 0)$changes,
              label = "when previous value and replace are NA")
  expect_null(update_DyNAM_choice_indeg(m, sender = 1, receiver = 2, replace = 2.5,
                                        weighted = FALSE, cache = vCache, n1 = 5, n2 = 0)$changes,
              label = "when weighted is set to FALSE and an updated tie already exists")
  expect_null(update_DyNAM_choice_indeg(m, sender = 4, receiver = 1, replace = 2,
                                        weighted = TRUE, cache = vCache, n1 = 5, n2 = 0)$changes,
              label = "when weighted is set to TRUE and the updated weight is identical")
})

test_that("indeg recognizes tie creation and updates correctly", {
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 4, replace = 1, weighted = TRUE,
                                              cache = vCache, n1 = 5, n2 = 0)$changes,
                    cbind(c(1, 2, 3, 5), rep(4, 4), rep(2, 4)))
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 5, receiver = 1, replace = 1,
                                              cache = vCache, n1 = 5, n2 = 0)$changes,
                    cbind(2:5, rep(1, 4), rep(1, 4)),
                    label = "when previous value was NA")
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 2, replace = NA,
                                              cache = vCache, n1 = 5, n2 = 0)$changes,
                    cbind(c(1, 3:5), rep(2, 4), rep(1, 4)),
                    label = "when replace is NA")
})

test_that("indeg recognizes tie deletion correctly", {
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 2, replace = 0,
                                              cache = vCache, n1 = 5, n2 = 0)$changes,
                    cbind(c(1, 3:5), rep(2, 4), rep(1, 4)))
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 5, receiver = 1, replace = 0,
                                              cache = vCache, n1 = 5, n2 = 0)$changes,
                    cbind(2:5, rep(1, 4), rep(0, 4)),
                    label = "when previous value was NA")
})

test_that("indeg recognizes updates to tie weights correctly", {
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 5, replace = 2,
                                              cache = vCache, n1 = 5, n2 = 0,
                                              weighted = TRUE)$changes,
                    cbind(1:4, rep(5, 4), rep(2, 4)),
                    label = "when a tie is created")
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 2, replace = 0.5,
                                              cache = vCache, n1 = 5, n2 = 0,
                                              weighted = TRUE)$changes,
                    cbind(c(1, 3:5), rep(2, 4), rep(1.5, 4)),
                    label = "when an existing tie is updated")
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 5, replace = -2,
                                              cache = vCache, n1 = 5, n2 = 0,
                                              weighted = TRUE)$changes,
                    cbind(1:4, rep(5, 4), rep(-2, 4)),
                    label = "when replace is negative")
  expect_equivalent(update_DyNAM_choice_indeg(m, sender = 1, receiver = 5, replace = 2,
                                              cache = vCache, n1 = 5, n2 = 0,
                                              weighted = TRUE, transformFun = sqrt)$changes,
                    cbind(1:4, rep(5, 4), rep(sqrt(2), 4)),
                    label = "when transformFun is specified")
})

test_that("indeg recognizes changes to two-mode networks correctly", {
  expect_equivalent(update_DyNAM_choice_indeg(mBipar, sender = 1, receiver = 4, replace = 1,
                                              cache = vCache, n1 = 2, n2 = 3,
                                              isTwoMode = TRUE)$changes,
                    cbind(1:2, rep(4, 2), rep(2, 2)),
                    label = "when a tie is created")
  expect_equivalent(update_DyNAM_choice_indeg(mBipar, sender = 1, receiver = 3, replace = 0,
                                              cache = vCache, n1 = 2, n2 = 3,
                                              isTwoMode = TRUE)$changes,
                    cbind(1:2, rep(3, 2), rep(2, 2)),
                    label = "when a tie is deleted")
  expect_equivalent(update_DyNAM_choice_indeg(mBipar, sender = 1, receiver = 3, replace = 1.5,
                                              cache = vCache, n1 = 2, n2 = 3,
                                              isTwoMode = TRUE, weighted = TRUE)$changes,
                    cbind(1:2, rep(3, 2), rep(3.5, 2)),
                    label = "when a weighted tie is updated")
})

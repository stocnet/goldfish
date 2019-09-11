test_that("recip returns a valid object on update", {
  expect_is(update_DyNAM_choice_recip(m, sender = 1, receiver = 5, replace = 1),
            "list")
  expect_is(update_DyNAM_choice_recip(m, sender = 1, receiver = 5, replace = 1)$changes,
            "matrix")
  expect_length(update_DyNAM_choice_recip(m, sender = 1, receiver = 5, replace = 1)$changes[1,], 3)
})

test_that("recip returns NULL if there is no change on update", {
  expect_null(update_DyNAM_choice_recip(m, sender = 1, receiver = 2, replace = 1)$changes)
  expect_null(update_DyNAM_choice_recip(m, sender = 1, receiver = 1, replace = 0)$changes,
              label = "when sender and receiver are the same node")
  expect_null(update_DyNAM_choice_recip(m, sender = 5, receiver = 1, replace = NA)$changes,
              label = "when previous value and replace are NA")
  expect_null(update_DyNAM_choice_recip(m, sender = 1, receiver = 2, replace = 2.5,
                                        weighted = FALSE)$changes,
              label = "when weighted is set to FALSE and an updated tie already exists")
})

test_that("recip recognizes tie creation and updates correctly", {
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 4, replace = 1)$changes,
                    cbind(4, 1, 1))
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 5, receiver = 1, replace = 1)$changes,
                    cbind(1, 5, 1),
                    label = "when previous value was NA")
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 2, replace = NA)$changes,
                    cbind(2, 1, 0),
                    label = "when replace is NA")
})

test_that("recip recognizes tie deletion correctly", {
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 2, replace = 0)$changes,
                    cbind(2, 1, 0))
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 5, receiver = 1, replace = 0)$changes,
                    cbind(1, 5, 0),
                    label = "when previous value was NA")
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 2, replace = NA)$changes,
                    cbind(2, 1, 0),
                    label = "when replace is NA")
})

test_that("recip recognizes updates to tie weights correctly", {
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 4, replace = 2,
                                              weighted = TRUE)$changes,
                    cbind(4, 1, 2),
                    label = "when a tie is created")
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 2, replace = 0.5,
                                              weighted = TRUE)$changes,
                    cbind(2, 1, 0.5),
                    label = "when an existing tie is updated")
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 4, replace = -2,
                                              weighted = TRUE)$changes,
                    cbind(4, 1, -2),
                    label = "when replace is negative")
  expect_equivalent(update_DyNAM_choice_recip(m, sender = 1, receiver = 4, replace = 2,
                                              weighted = TRUE, transformFun = function(x) x * x)$changes,
                    cbind(4, 1, 4),
                    label = "when transformFun is specified")
})

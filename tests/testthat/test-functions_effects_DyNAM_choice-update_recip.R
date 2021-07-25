test_that("recip returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_recip(m, sender = 1, receiver = 5, replace = 1),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_recip(
        m, sender = 1, receiver = 5, replace = 1)$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 5, replace = 1)$changes[1, ],
    3)
})

test_that("recip returns NULL if there is no change on update", {
  expect_null(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 2, replace = 1)$changes)
  expect_null(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 1, replace = 0)$changes,
    label = "when sender and receiver are the same node"
  )
  expect_null(
    update_DyNAM_choice_recip(
      m, sender = 5, receiver = 1, replace = NA)$changes,
    label = "when previous value and replace are NA"
  )
  expect_null(update_DyNAM_choice_recip(
    m,
    sender = 1, receiver = 2, replace = 2.5,
    weighted = FALSE
  )$changes,
  label = "when weighted is set to FALSE and an updated tie already exists"
  )
})

test_that("recip recognizes tie creation and updates correctly", {
  expect_equal(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 4, replace = 1)$changes,
    matrix(c(4, 1, 1), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace")))
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m, sender = 5, receiver = 1, replace = 1)$changes,
    matrix(c(1, 5, 1), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when previous value was NA"
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 2, replace = NA)$changes,
    matrix(c(2, 1, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when replace is NA"
  )
})

test_that("recip recognizes tie deletion correctly", {
  expect_equal(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 2, replace = 0)$changes,
    matrix(c(2, 1, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace")))
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m, sender = 5, receiver = 1, replace = 0)$changes,
    matrix(c(1, 5, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when previous value was NA"
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m, sender = 1, receiver = 2, replace = NA)$changes,
    matrix(c(2, 1, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when replace is NA"
  )
})

test_that("recip recognizes updates to tie weights correctly", {
  expect_equal(
    update_DyNAM_choice_recip(
      m,
      sender = 1, receiver = 4, replace = 2,
      weighted = TRUE
    )$changes,
    matrix(c(4, 1, 2), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when a tie is created"
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m,
      sender = 1, receiver = 2, replace = 0.5,
      weighted = TRUE
    )$changes,
    matrix(c(2, 1, 0.5), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when an existing tie is updated"
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m,
    sender = 1, receiver = 4, replace = -2,
    weighted = TRUE
  )$changes,
  matrix(c(4, 1, -2), 1, 3,
         dimnames = list(NULL, c("node1", "node2", "replace"))),
  label = "when replace is negative"
  )
  expect_equal(
    update_DyNAM_choice_recip(
      m,
    sender = 1, receiver = 4, replace = 2,
    weighted = TRUE, transformFun = function(x) x * x
  )$changes,
  matrix(c(4, 1, 4), 1, 3,
         dimnames = list(NULL, c("node1", "node2", "replace"))),
  label = "when transformFun is specified"
  )
})

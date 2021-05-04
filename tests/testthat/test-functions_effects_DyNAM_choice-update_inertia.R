test_that("inertia returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_inertia(m, sender = 1, receiver = 5, replace = 1),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_inertia(
        m, sender = 1, receiver = 5, replace = 1)$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 5, replace = 1)$changes,
    3)
})

test_that("inertia returns NULL if there is no change on update", {
  expect_null(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 2, replace = 1)$changes
    )
  expect_null(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 1, replace = 0)$changes,
    label = "when sender and receiver are the same node"
  )
  expect_null(
    update_DyNAM_choice_inertia(
      m, sender = 5, receiver = 1, replace = NA)$changes,
    label = "when previous value and replace are NA"
  )
  expect_null(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 2, replace = 2.5, weighted = FALSE)$changes,
    label = "when weighted is set to FALSE and an updated tie already exists"
  )
  expect_null(
    update_DyNAM_choice_inertia(
      m, sender = 4, receiver = 1, replace = 2, weighted = TRUE)$changes,
    label = "when weighted is set to TRUE and the updated weight is identical"
  )
})

test_that("inertia recognizes tie creation and updates correctly", {
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 5, replace = 1)$changes,
    matrix(c(1, 5, 1), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace")))
  )
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 5, receiver = 1, replace = 1)$changes,
    matrix(c(5, 1, 1), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when previous value was NA"
  )
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 3, replace = NA)$changes,
    matrix(c(1, 3, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when replace is NA"
  )
})

test_that("inertia recognizes tie deletion correctly", {
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 3, replace = 0)$changes,
    matrix(c(1, 3, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace")))
  )
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 5, receiver = 1, replace = 0)$changes,
    matrix(c(5, 1, 0), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when previous value was NA"
  )
})

test_that("inertia recognizes updates to tie weights correctly", {
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 5, replace = 2, weighted = TRUE)$changes,
    matrix(c(1, 5, 2), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when a tie is created"
  )
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 3, replace = 2, weighted = TRUE)$changes,
    matrix(c(1, 3, 2), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when an existing tie is updated"
  )
  expect_equal(
    update_DyNAM_choice_inertia(
      m, sender = 1, receiver = 5, replace = -1, weighted = TRUE)$changes,
    matrix(c(1, 5, -1), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when replace is negative"
  )
  expect_equal(
    update_DyNAM_choice_inertia(
      m,
      sender = 1, receiver = 3, replace = 2, weighted = TRUE,
      transformFun = function(x) `^`(x, 2)
    )$changes,
    matrix(c(1, 3, 4), 1, 3,
           dimnames = list(NULL, c("node1", "node2", "replace"))),
    label = "when transformFun is specified"
  )
})

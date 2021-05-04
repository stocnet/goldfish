test_that("four returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_four(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_four(
        m,
        sender = 1, receiver = 5, replace = 1,
        cache = m0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_four(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    )$changes[1, ],
    3
  )
})

test_that("four NULL if there is no change", {
  expect_null(update_DyNAM_choice_four(mTwoMode,
    sender = 1, receiver = 2, replace = 1,
    cache = m0
  )$changes)
  expect_null(update_DyNAM_choice_four(mTwoMode,
    sender = 1, receiver = 5, replace = NA,
    cache = m0
  )$changes,
  label = "when previous value and replace are NA"
  )
})

test_that("four recognizes tie creation correctly", {
  expect_equal(update_DyNAM_choice_four(mTwoMode,
    sender = 1, receiver = 4, replace = 1,
    cache = m0
  )$changes,
  rbind(
    c(node1 = 1, node2 = 2, replace = 1),
    c(node1 = 3, node2 = 2, replace = 1),
    c(node1 = 3, node2 = 4, replace = 1),
    c(node1 = 1, node2 = 5, replace = 1)
  ),
  label = "when tie i -> l is created"
  )
  expect_null(update_DyNAM_choice_four(mTwoMode,
    sender = 1, receiver = 1, replace = 1,
    cache = m0
  )$changes,
  label = "when sender and receiver are the same node"
  ) # NULL, not self-loops
  expect_equal(update_DyNAM_choice_four(mTwoMode,
    sender = 1, receiver = 5, replace = 1,
    cache = m0
  )$changes,
  rbind(
    c(node1 = 1, node2 = 2, replace = 1),
    c(node1 = 3, node2 = 2, replace = 1),
    c(node1 = 1, node2 = 4, replace = 1),
    c(node1 = 3, node2 = 5, replace = 1)
  ),
  label = "when previous value was NA"
  )
  expect_equal(update_DyNAM_choice_four(mTwoMode,
    sender = 1, receiver = 2, replace = NA,
    cache = m0
  )$changes,
  rbind(c(node1 = 1, node2 = 4, replace = 0),
        c(node1 = 1, node2 = 5, replace = 0)),
  label = "when replace is NA"
  )
})

# test_that("four recognizes tie deletion correctly", {
# })

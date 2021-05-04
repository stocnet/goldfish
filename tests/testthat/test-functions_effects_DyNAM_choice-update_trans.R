test_that("trans returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    ),
    "list"
  )
  expect_true(
    inherits(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    )$changes,
    "matrix"),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    )$changes[1, ],
    3
  )
})

test_that("trans returns NULL if there is no change", {
  expect_null(update_DyNAM_choice_trans(
    m,
    sender = 1, receiver = 2, replace = 1,
    cache = m0
  )$changes)
  expect_null(update_DyNAM_choice_trans(
    m,
    sender = 1, receiver = 1, replace = 0,
    cache = m0
  )$changes,
  label = "when sender and receiver are the same node"
  )
  expect_null(update_DyNAM_choice_trans(
    m,
    sender = 5, receiver = 1, replace = NA,
    cache = m0
  )$changes,
  label = "when previous value and replace are NA"
  )
  expect_null(update_DyNAM_choice_trans(
    m0,
    sender = 5, receiver = 4, replace = 1,
    cache = m0
  )$changes,
  label = "when change in tie composition has no effect"
  )
  expect_null(update_DyNAM_choice_trans(
    m,
    sender = 1, receiver = 2, replace = 1.5,
    cache = m0
  )$changes,
  label = "when weighted is set to FALSE and an updated tie already exists"
  )
})

test_that("trans recognizes tie creation correctly", {
  expect_equal(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 4, replace = 1,
      cache = mCache
    )$changes,
    rbind(
      "Actor 1" = c(node1 = 1, node2 = 1, replace = 1),
      "Actor 5" = c(node1 = 1, node2 = 5, replace = 1),
      "Actor 2" = c(node1 = 2, node2 = 4, replace = 1),
      "Actor 4" = c(node1 = 4, node2 = 4, replace = 1)
    )
  )
  expect_equal(
    update_DyNAM_choice_trans(
      m,
      sender = 5, receiver = 1, replace = 1,
      cache = mCache
    )$changes,
    rbind(
      "Actor 2" = c(node1 = 5, node2 = 2, replace = 1),
      "Actor 3" = c(node1 = 5, node2 = 3, replace = 1),
      "Actor 4" = c(node1 = 4, node2 = 1, replace = 1)
    ),
    label = "when previous value was NA"
  )
  expect_equal(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 2, replace = NA,
      cache = mCache
    )$changes,
    rbind(
      "Actor 1" = c(node1 = 1, node2 = 1, replace = -1),
      "Actor 3" = c(node1 = 1, node2 = 3, replace = 0),
      "Actor 2" = c(node1 = 2, node2 = 2, replace = -1),
      "Actor 4" = c(node1 = 4, node2 = 2, replace = -1)
    ),
    label = "when replace is NA"
  )
})

test_that("trans recognizes tie deletion correctly", {
  expect_equal(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 2, replace = 0,
      cache = mCache
    )$changes,
    rbind(
      "Actor 1" = c(node1 = 1, node2 = 1, replace = -1),
      "Actor 3" = c(node1 = 1, node2 = 3, replace = 0),
      "Actor 2" = c(node1 = 2, node2 = 2, replace = -1),
      "Actor 4" = c(node1 = 4, node2 = 2, replace = -1)
    )
  )
  expect_equal(
    update_DyNAM_choice_trans(
      m,
      sender = 5, receiver = 1, replace = 0,
      cache = mCache
    )$changes,
    rbind(
      "Actor 2" = c(node1 = 5, node2 = 2, replace = 0),
      "Actor 3" = c(node1 = 5, node2 = 3, replace = 0),
      "Actor 4" = c(node1 = 4, node2 = 1, replace = 0)
    ),
    label = "when previous value was NA"
  )
})

test_that("cycle returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_cycle(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_cycle(
        m,
        sender = 1, receiver = 5, replace = 1,
        cache = m0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_cycle(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    )$changes[1, ],
    3
  )
})

test_that("cycle returns NULL if there is no change", {
  expect_null(update_DyNAM_choice_cycle(
    m,
    sender = 1, receiver = 2, replace = 1,
    cache = m0
  )$changes)
  expect_null(
    update_DyNAM_choice_cycle(
      m,
      sender = 1, receiver = 1, replace = 0,
      cache = m0
    )$changes,
    label = "when sender and receiver are the same node"
  )
  # expect_null(
  #   update_DyNAM_choice_cycle(
  #     m,
  #     sender = 5, receiver = 1, replace = NA,
  #     cache = m0
  #   )$changes,
  #   label = "when previous value and replace are NA"
  # )
  expect_null(
    update_DyNAM_choice_cycle(
      m0,
      sender = 5, receiver = 4, replace = 1,
      cache = m0
    )$changes,
    label = "when change in tie composition has no effect"
  )
  expect_null(
    update_DyNAM_choice_cycle(
      m,
      sender = 1, receiver = 2, replace = 1,
      cache = m0
    )$changes,
    label = "when an updated tie already exists" 
  )
})

test_that("cycle recognises tie creation correctly", {
  expect_equal(
    update_DyNAM_choice_cycle(
      m,
      sender = 3, receiver = 1, replace = 1,
      cache = mCache
    )$changes,
    rbind(
      "Actor 2" = c(node1 = 2, node2 = 3, replace = 2),
      "Actor 2" = c(node1 = 1, node2 = 2, replace = 3)
    )
  )
  # expect_equal(
  #   update_DyNAM_choice_cycle(
  #     m,
  #     sender = 5, receiver = 1, replace = 1,
  #     cache = m0
  #   )$changes,
  #   rbind(
  #     "Actor 2" = c(node1 = 2, node2 = 5, replace = 1),
  #     "Actor 3" = c(node1 = 3, node2 = 5, replace = 1),
  #     "Actor 4" = c(node1 = 1, node2 = 4, replace = 1)
  #   ),
  #   label = "when previous value was NA"
  # )
  # expect_equal(
  #   update_DyNAM_choice_cycle(
  #     m,
  #     sender = 1, receiver = 2, replace = NA,
  #     cache = m0
  #   )$changes,
  #   rbind(
  #     "Actor 3" = c(node1 = 3, node2 = 1, replace = -1),
  #     "Actor 4" = c(node1 = 2, node2 = 4, replace = -1)
  #   ),
  #   label = "when replace is NA"
  # )
})

test_that("cycle recognizes tie deletion correctly", {
  expect_equal(
    update_DyNAM_choice_cycle(
      m,
      sender = 1, receiver = 2, replace = 0,
      cache = mCache
    )$changes,
    rbind(
      "Actor 3" = c(node1 = 3, node2 = 1, replace = -1),
      "Actor 4" = c(node1 = 2, node2 = 4, replace = -1)
    )
  )
  # expect_null(
  #   update_DyNAM_choice_cycle(
  #     m,
  #     sender = 5, receiver = 1, replace = 0,
  #     cache = mCache
  #   )$changes,
  #   label = "when previous value was NA"
  # )
})

test_that("cycle init throws an error when two-mode network", {
  # effectFUN_closure has isTwoMode=FALSE so we must change this prior to the test
  check = formals(effectFUN_closure)
  check$isTwoMode = TRUE
  formals(effectFUN_closure) <- check
  expect_error(init_DyNAM_choice.cycle(effectFUN_closure, m1, NULL, 5, 5),
               regexp = ".*\\Q effect must not use when is a two-mode network\\E.*")
})

test_that("cycle init returns the correct result", {
  expect_equal(
    init_DyNAM_choice.cycle(effectFUN_closure, m1, NULL, 5, 5)$cache,
    matrix(
      c(
      0, 0, 2, 0, 0,
      1, 0, 0, 1, 0,
      1, 1, 0, 1, 0,
      1, 2, 0, 0, 0,
      1, 0, 1, 0, 0
    ),
    nrow = 5, ncol = 5, byrow = TRUE
  ))
})

test_that("cycle init returns an empty cache", {
  expect_equal(
    init_DyNAM_choice.cycle(effectFUN_closure, m1, 1, 5, 5)$cache,
    matrix(0,
           nrow = 5, ncol = 5),
    label = "when windowed")
  expect_equal(
    init_DyNAM_choice.cycle(effectFUN_closure, m0, NULL, 5, 5)$cache,
    matrix(0,
           nrow = 5, ncol = 5),
    label = "when network is empty")
})

  
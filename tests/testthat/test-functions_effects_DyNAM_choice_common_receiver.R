test_that("common receiver returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_commonReceiver(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_commonReceiver(
        m,
        sender = 1, receiver = 5, replace = 1,
        cache = m0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_commonReceiver(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    )$changes[1, ],
    3
  )
})

test_that("commonReceiver returns NULL if there is no change", {
  expect_null(update_DyNAM_choice_commonReceiver(
    m,
    sender = 1, receiver = 2, replace = 1,
    cache = m0
  )$changes)
  expect_null(
    update_DyNAM_choice_commonReceiver(
      m,
      sender = 1, receiver = 1, replace = 0,
      cache = m0
    )$changes,
    label = "when sender and receiver are the same node"
  )
  # expect_null(
  #   update_DyNAM_choice_commonReceiver(
  #     `[<-`(m, 3, 1, NA),
  #     sender = 3, receiver = 1, replace = NA,
  #     cache = m0
  #   )$changes,
  #   label = "when previous value and replace are NA"
  # )
  expect_null(
    update_DyNAM_choice_cycle(
      m0,
      sender = 5, receiver = 1, replace = 1,
      cache = m0
    )$changes,
    label = "when change in tie composition has no effect"
  )
})

test_that("common receiver recognises tie creation correctly", {
  expect_equal(
    update_DyNAM_choice_commonReceiver(
      m,
      sender = 1, receiver = 5, replace = 1,
      cache = m0
    )$changes,
    rbind(
      "Actor 4" = c(node1 = 1, node2 = 4, replace = 1),
      "Actor 4" = c(node1 = 4, node2 = 1, replace = 1)
    )
  )
  # expect_equal(
  #   update_DyNAM_choice_commonReceiver(
  #     m,
  #     sender = 5, receiver = 1, replace = 1,
  #     cache = m0
  #   )$changes,
  #   rbind(
  #     "Actor 2" = c(node1 = 5, node2 = 2, replace = 1),
  #     "Actor 4" = c(node1 = 5, node2 = 4, replace = 1),
  #     "Actor 2" = c(node1 = 2, node2 = 5, replace = 1),
  #     "Actor 4" = c(node1 = 4, node2 = 5, replace = 1)
  #   ),
  #   label = "when previous value was NA"
  # )
  # expect_equal(
  #   update_DyNAM_choice_commonReceiver(
  #     m,
  #     sender = 1, receiver = 2, replace = NA,
  #     cache = m0
  #   )$changes,
  #   rbind(
  #     "Actor 3" = c(node1 = 1, node2 = 3, replace = -1),
  #     "Actor 3" = c(node1 = 3, node2 = 1, replace = -1)
  #   ),
  #   label = "when replace is NA"
  # )
})

test_that("common receiver recognizes tie deletion correctly", {
  expect_equal(
    update_DyNAM_choice_commonReceiver(
      m,
      sender = 1, receiver = 2, replace = 0,
      cache = mCache
    )$changes,
    rbind(
      "Actor 3" = c(node1 = 1, node2 = 3, replace = 0),
      "Actor 3" = c(node1 = 3, node2 = 1, replace = -1)
    )
  )
})

test_that("commonReceiver init returns an empty cache", {
  expect_equal(
    init_DyNAM_choice.commonReceiver(effectFUN_closure, m1, 1, 5, 5)$cache,
    matrix(0,
           nrow = 5, ncol = 5),
    label = "when windowed" )
  expect_equal(
    init_DyNAM_choice.commonReceiver(effectFUN_closure, m0, NULL, 5, 5)$cache,
    matrix(0,
           nrow = 5, ncol = 5),
    label = "when network is empty" )
})

test_that("commonReceiver init returns the correct result", {
  expect_equal(
    init_DyNAM_choice.commonReceiver(effectFUN_closure, m1, NULL, 5, 5)$cache,
    unname(tcrossprod(sign(m1))))
})

test_that("commonReceiver init returns an error when n1 != n2", {
  expect_error(
    init_DyNAM_choice.commonReceiver(effectFUN_closure, m1, NULL, 3, 5),
    regexp = "Dimensions of the two-mode network are not conformable")
})



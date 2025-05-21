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
      "matrix"
    ),
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
  expect_null(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 1, replace = 0,
      cache = m0
    )$changes,
    label = "when sender and receiver are the same node"
  )
  # expect_null(
  #   update_DyNAM_choice_trans(
  #     m,
  #     sender = 5, receiver = 1, replace = NA,
  #     cache = m0
  #   )$changes,
  #   label = "when previous value and replace are NA"
  # )
  expect_null(
    update_DyNAM_choice_trans(
      m0,
      sender = 5, receiver = 4, replace = 1,
      cache = m0
    )$changes,
    label = "when change in tie composition has no effect"
  )
  expect_null(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 2, replace = 1.5,
      cache = m0
    )$changes,
    label = "when weighted is set to FALSE and an updated tie already exists"
  )
  expect_null(
    update_DyNAM_choice_trans(
      `[<-`(m0, 4, 5, 0),
      sender = 4, receiver = 5, replace = 1,
      cache = m0, history = 'seq'
    )$changes,
    label = "when sequential and the only two paths the new tie forms are not sequential"
  )
  attr(m0,'lastUpdate') <- c(0,0,0)
  expect_null(
    update_DyNAM_choice_trans(
      m0,
      sender = 4, receiver = 5, replace = 1,
      cache = m0, history = 'cons', eventOrder = 2
    )$changes,
    label = "when consecutive and the only two paths the new tie forms are not consecutive"
  )
})

test_that("trans recognizes tie creation correctly ", {
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
      sender = 1, receiver = 4, replace = 1,
      cache = mCache, history='seq'
    )$changes,
    rbind(
      "Actor 2" = c(node1 = 2, node2 = 4, replace = 1),
      "Actor 4" = c(node1 = 4, node2 = 4, replace = 1)
    ),
    label = "when history = sequential"
  )
  attr(mCache,"lastUpdate") <- c(2,1,1)
  expect_equal(
    update_DyNAM_choice_trans(
      m,
      sender = 1, receiver = 4, replace = 1,
      cache = mCache, history='cons', eventOrder = 2
    )$changes,
    rbind(
      "Actor 2" = c(node1 = 2, node2 = 4, replace = 1)
    ),
    label = "when history = consecutive"
  )
  
  # expect_equal(
  #   update_DyNAM_choice_trans(
  #     m1,
  #     sender = 5, receiver = 1, replace = 1,
  #     cache = mCache
  #   )$changes,
  #   rbind(
  #     "Actor 2" = c(node1 = 5, node2 = 2, replace = 1),
  #     "Actor 3" = c(node1 = 5, node2 = 3, replace = 1),
  #     "Actor 4" = c(node1 = 4, node2 = 1, replace = 1)
  #   ),
  #   label = "when previous value was NA"
  # )
#   expect_equal(
#     update_DyNAM_choice_trans(
#       m,
#       sender = 1, receiver = 2, replace = NA,
#       cache = mCache
#     )$changes,
#     rbind(
#       "Actor 1" = c(node1 = 1, node2 = 1, replace = -1),
#       "Actor 3" = c(node1 = 1, node2 = 3, replace = 0),
#       "Actor 2" = c(node1 = 2, node2 = 2, replace = -1),
#       "Actor 4" = c(node1 = 4, node2 = 2, replace = -1)
#     ),
#     label = "when replace is NA"
#   )
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
        sender = 1, receiver = 2, replace = 0,
        cache = mCache, history='seq'
      )$changes,
      rbind(
        "Actor 1" = c(node1 = 1, node2 = 1, replace = -1),
        "Actor 3" = c(node1 = 1, node2 = 3, replace = 0),
        "Actor 2" = c(node1 = 2, node2 = 2, replace = -1),
        "Actor 4" = c(node1 = 4, node2 = 2, replace = -1)
      ),
      label = " when history = sequential"
    )
    expect_equal(
      update_DyNAM_choice_trans(
        m,
        sender = 1, receiver = 2, replace = 0,
        cache = mCache, history='cons', eventOrder = 2
      )$changes,
      rbind(
        "Actor 1" = c(node1 = 1, node2 = 1, replace = -1),
        "Actor 3" = c(node1 = 1, node2 = 3, replace = 0),
        "Actor 2" = c(node1 = 2, node2 = 2, replace = -1),
        "Actor 4" = c(node1 = 4, node2 = 2, replace = -1)
      ),
      label = " when history = consecutive"
    )
  
  # expect_null(
  #   update_DyNAM_choice_trans(
  #     m,
  #     sender = 5, receiver = 1, replace = 0,
  #     cache = mCache
  #   )$changes,
  #   label = "when previous value was NA"
  # )
})

test_that("trans init throws an error when two-mode network", {
  # effectFUN_closure has isTwoMode=FALSE so we must change this prior to the test
  check = formals(effectFUN_closure)
  check$isTwoMode = TRUE
  formals(effectFUN_closure) <- check
  expect_error(init_DyNAM_choice.trans(effectFUN_closure, m1, NULL, 5, 5),
               regexp = ".*\\Q effect must not use when is a two-mode network\\E.*")
})

test_that("DyNAM default and trans init return the same result", {
  expect_equal(
    init_DyNAM_choice.trans(effectFUN_closure, m1, NULL, 5, 5),
    init_DyNAM_choice.default(
      effectFUN_closure,
      network = m1, attribute = NULL, window = NULL,
      n1 = 5, n2 = 5
    )
  )
})

test_that("trans init is correctly performed for history = consecutive",{
  expect_equal(
    attr(init_DyNAM_choice.trans(effectFUN_closure, m1, NULL, 5, 5, history="cons")$cache,"lastUpdate"),
    c(0,0,0)
  )
})
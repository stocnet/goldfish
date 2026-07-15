test_that("mixed_trans returns a valid object on update when netupdate = 1", {
  expect_type(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_mixed_trans(
        list(m, m1),
        4,
        3,
        5,
        1,
        m0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0
    )$changes[1, ],
    3
  )
})

test_that("mixed_trans returns a valid object on update when netupdate = 2", {
  expect_type(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      1,
      5,
      5,
      2,
      m0
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_mixed_trans(
        list(m, m1),
        1,
        5,
        5,
        2,
        m0
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
  expect_length(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      1,
      5,
      5,
      2,
      m0
    )$changes[1, ],
    3
  )
})

test_that("update_mixed_trans doesn't update when sender == receiver", {
  expect_equal(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      4,
      5,
      1,
      m0
    )$cache,
    m0
  )
})

test_that("update_mixed_trans doesn't update when replace == old_value", {
  expect_equal(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      0,
      1,
      m0
    )$cache,
    m0,
    label = "When net_update = 1"
  )
  expect_equal(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      0,
      2,
      m0
    )$cache,
    m0,
    label = "When net_update = 2"
  )
})

test_that("update_mixed_trans throws an error when net_update is not 1 or 2. ", {
  expect_error(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      5,
      3,
      m0
    ),
    "Check you declare only two networks in network argument",
    label = "net_update is an unsuitable integer"
  )
  expect_error(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      5,
      list(1, 2),
      m0
    ),
    "Check you declare only two networks in network argument",
    label = "net_update is not an integer"
  )
})

test_that("init mixed_trans initialises successfully.", {
  expect_type(
    init_DyNAM_choice.mixed_trans(
      effectFUN,
      list(m, m1),
      NULL,
      5,
      5
    ),
    "list"
  )
})

test_that("init mixed_trans must have conformable dimensions", {
  expect_error(
    init_DyNAM_choice.mixed_trans(
      effectFUN,
      list(m, m1),
      NULL,
      4,
      5
    ),
    "Non conformable dimensions sizes for effect"
  )
})

test_that("init mixed_trans initialises empty when it has a window or is empty", {
  expect_equal(
    init_DyNAM_choice.mixed_trans(
      effectFUN,
      list(m, m1),
      1,
      5,
      5
    )$cache,
    matrix(0, nrow = 5, ncol = 5)
  )
})

test_that("REM and DyNAM mixed_trans return the same result", {
  expect_equal(
    init_REM_choice.mixed_trans(effectFUN, list(m, m1), 1, 5, 5),
    init_DyNAM_choice.mixed_trans(effectFUN, list(m, m1), 1, 5, 5),
    label = "for init"
  )
  expect_equal(
    update_REM_choice_mixed_trans(list(m, m1), 4, 3, 5, 1, m0),
    update_DyNAM_choice_mixed_trans(list(m, m1), 4, 3, 5, 1, m0),
    label = "for update"
  )
})

test_that("mixed_trans history = sequential: adding to net1 produces no new paths", {
  expect_null(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      3,
      5,
      1,
      m0,
      history = "sequential"
    )$changes,
    label = "sequential blocks new paths when adding to net1"
  )
})

test_that("mixed_trans history = sequential: adding to net2 same as pooled", {
  expect_equal(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      1,
      5,
      5,
      2,
      m0,
      history = "sequential"
    ),
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      1,
      5,
      5,
      2,
      m0
    ),
    label = "sequential does not filter net2 additions"
  )
})

test_that("mixed_trans history = sequential: removal from net1 same as pooled", {
  expect_equal(
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      1,
      0,
      1,
      m0,
      history = "sequential"
    ),
    update_DyNAM_choice_mixed_trans(
      list(m, m1),
      4,
      1,
      0,
      1,
      m0
    ),
    label = "sequential does not block removals"
  )
})

test_that("init mixed_trans returns empty cache when history = sequential", {
  effectFUN_seq <- function(
    network,
    sender,
    receiver,
    replace,
    cache,
    is_two_mode = FALSE,
    transformer_fn = identity,
    history = "sequential"
  ) {}
  expect_equal(
    init_DyNAM_choice.mixed_trans(effectFUN_seq, list(m, m1), NULL, 5, 5)$cache,
    matrix(0, nrow = 5, ncol = 5)
  )
})

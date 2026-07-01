test_that("mixed_cycle returns a valid object on update", {
  # Testing netUpdate = 1
  expect_type(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 5, 1, m0),
    "list"
  )
  # Testing netUpdate = 2
  expect_type(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 1, 5, 5, 2, m0),
    "list"
  )
})

test_that("mixed_cycle doesn't update when replace == oldValue", {
  # Testing netUpdate = 1
  expect_equal(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 0, 1, m0)$cache,
    m0
  )
  # Testing netUpdate = 2
  expect_equal(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 1, 5, 0, 2, m0)$cache,
    m0
  )
})

test_that("update_mixed_cycle doesn't update when sender == receiver", {
  expect_equal(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 4, 5, 1, m0)$cache,
    m0
  )
})

test_that("update_mixed_cycle throws error for wrong netUpdate", {
  expect_error(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 5, 3, m0),
    "Check that you only declare two networks as argument."
  )
})

test_that("init mixed_cycle handles dimensions and windows", {
  # Successful init
  expect_type(
    init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), NULL, 5, 5),
    "list"
  )
  # Dimension mismatch
  expect_error(
    init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), NULL, 4, 5),
    "Non conformable dimensions sizes"
  )
  # Window init (should be empty)
  expect_type(
    init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), 1, 5, 5),
    "list"
  )
})

test_that("mixed_cycle returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 5, 1, m0),
    "list"
  )
  expect_type(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 1, 5, 5, 2, m0),
    "list"
  )
})

test_that("mixed_cycle doesn't update when replace == oldValue", {
  expect_equal(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 0, 1, m0)$cache,
    m0
  )
  expect_equal(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 1, 5, 0, 2, m0)$cache,
    m0
  )
})

test_that("update_mixed_cycle doesn't update when sender == receiver", {
  expect_equal(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 4, 5, 1, m0)$cache,
    m0
  )
})

test_that("update_mixed_cycle throws error for wrong netUpdate", {
  expect_error(
    update_DyNAM_choice_mixed_cycle(list(m, m1), 4, 3, 5, 3, m0),
    "Check that you only declare two networks as argument."
  )
})

test_that("init mixed_cycle handles dimensions and windows", {
  expect_type(
    init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), NULL, 5, 5),
    "list"
  )
  expect_error(
    init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), NULL, 4, 5),
    "Non conformable dimensions sizes"
  )
  expect_type(
    init_DyNAM_choice.mixed_cycle(effectFUN, list(m, m1), 1, 5, 5),
    "list"
  )
})

test_that("REM and DyNAM common_receiver return the same result", {
  expect_equal(
    init_REM_choice.common_receiver(effectFUN_closure, m1, 1, 5, 5),
    init_DyNAM_choice.common_receiver(effectFUN_closure, m1, 1, 5, 5),
    label = "REM and DyNAM init return different results"
  )

  expect_equal(
    update_REM_choice_common_receiver(
      m,
      sender = 1,
      receiver = 5,
      replace = 1,
      cache = m0
    ),
    update_DyNAM_choice_common_receiver(
      m,
      sender = 1,
      receiver = 5,
      replace = 1,
      cache = m0
    ),
    label = "REM and DyNAM update return different results"
  )
})

test_that("mixed_cycle history = sequential: adding to net1 produces no new paths", {
  expect_null(
    update_DyNAM_choice_mixed_cycle(
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

test_that("mixed_cycle history = sequential: adding to net2 same as pooled", {
  expect_equal(
    update_DyNAM_choice_mixed_cycle(
      list(m, m1),
      1,
      5,
      5,
      2,
      m0,
      history = "sequential"
    ),
    update_DyNAM_choice_mixed_cycle(
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

test_that("mixed_cycle history = sequential: removal from net1 same as pooled", {
  expect_equal(
    update_DyNAM_choice_mixed_cycle(
      list(m, m1),
      4,
      1,
      0,
      1,
      m0,
      history = "sequential"
    ),
    update_DyNAM_choice_mixed_cycle(
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

test_that("init mixed_cycle returns empty cache when history = sequential", {
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
    init_DyNAM_choice.mixed_cycle(effectFUN_seq, list(m, m1), NULL, 5, 5)$cache,
    matrix(0, nrow = 5, ncol = 5)
  )
})

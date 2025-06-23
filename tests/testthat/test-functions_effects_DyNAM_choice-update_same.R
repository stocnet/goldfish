test_that("same returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_same(
      attribute = testAttr$fishingSkill, node = 1, replace = 1
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_same(
        attribute = testAttr$fishingSkill, node = 1, replace = 1
      )$changes,
      "matrix"
    ),
    label = "it doesn't return a matrix"
  )
})

test_that("same returns NULL if there is no change", {
  expect_null(
    update_DyNAM_choice_same(
      testAttr$fishingSkill,
      node = 1, replace = 10
    )$changes
  )
  expect_null(
    update_DyNAM_choice_same(
      testAttr$fishSizeMean,
      node = 1, replace = 0.15
    )$changes,
    label = "when no match results from update"
  )
  expect_null(
    update_DyNAM_choice_same(
      testAttr$fishingSkill,
      node = 7, replace = 2
    )$changes,
    label = "when new and old attribute have no match"
  )
  # expect_null(
  #   update_DyNAM_choice_same(
  #     testAttr$fishingSkill,
  #     node = 2, replace = NA
  #   )$changes,
  #   label = "when replace is NA"
  # )
})

test_that("same returns correct attributes on update", {
  expect_equal(
    update_DyNAM_choice_same(
      testAttr$fishingSkill,
      node = 7, replace = 10
    )$changes,
    rbind(
      c(node1 = 7, node2 = 1, replace = 1),
      c(node1 = 7, node2 = 4, replace = 1),
      c(node1 = 1, node2 = 7, replace = 1),
      c(node1 = 4, node2 = 7, replace = 1)
    ),
    label = "when new attribute creates additional matches"
  )
  expect_equal(
    update_DyNAM_choice_same(
      testAttr$fishingSkill,
      node = 1, replace = 2
    )$changes,
    rbind(
      c(node1 = 1, node2 = 4, replace = 0),
      c(node1 = 4, node2 = 1, replace = 0)
    ),
    label = "when the new attribute removes a previous match"
  )
  # expect_equal(
  #   update_DyNAM_choice_same(
  #     testAttr$fishingSkill,
  #     node = 1, replace = NA
  #   )$changes,
  #   rbind(
  #     c(node1 = 1, node2 = 4, replace = 0),
  #     c(node1 = 4, node2 = 1, replace = 0)
  #   ),
  #   label = "when replace is NA and removes a previous match"
  # )
  # expect_equal(
  #   update_DyNAM_choice_same(
  #     testAttr$fishingSkill,
  #     node = 8, replace = 10
  #   )$changes,
  #   rbind(
  #     c(node1 = 8, node2 = 1, replace = 1),
  #     c(node1 = 8, node2 = 4, replace = 1),
  #     c(node1 = 1, node2 = 8, replace = 1),
  #     c(node1 = 4, node2 = 8, replace = 1)
  #   ),
  #   label = "when previous value was NA"
  # )
})

test_that("same init throws an error when two-mode network", {
  check = formals(effectFUN)
  check$is_two_mode = TRUE
  formals(effectFUN) <- check
  expect_error(init_DyNAM_choice.same(effectFUN, m1, NULL, 5, 5),
               regexp = "doesn't work in two mode networks")
})


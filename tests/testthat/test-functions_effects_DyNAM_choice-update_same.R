test_that("same returns a valid object on update", {
  expect_type(
    update_DyNAM_choice_same(
      attribute = testAttr$fishingSkill,
      node = 1,
      replace = 1
    ),
    "list"
  )
  expect_true(
    inherits(
      update_DyNAM_choice_same(
        attribute = testAttr$fishingSkill,
        node = 1,
        replace = 1
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
      node = 1,
      replace = 10
    )$changes
  )
  expect_null(
    update_DyNAM_choice_same(
      testAttr$fishSizeMean,
      node = 1,
      replace = 0.15
    )$changes,
    label = "when no match results from update"
  )
  expect_null(
    update_DyNAM_choice_same(
      testAttr$fishingSkill,
      node = 7,
      replace = 2
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
      node = 7,
      replace = 10
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
      node = 1,
      replace = 2
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

test_that("same init compares the two sides on a two-mode network", {
  # This used to abort: `same` was declared two-mode-incompatible. It is not --
  # comparing a sender attribute to a receiver attribute is well defined, and
  # whether the two scales are comparable is the user's call. The parser
  # resolves the one written operand into one position per side, so the init
  # receives a list and the statistic is the cross-side outer comparison.
  check <- formals(effectFUN)
  check$is_two_mode <- TRUE
  formals(effectFUN) <- check

  ego <- c(1, 2, 3)
  alter <- c(2, 3)
  stat <- init_DyNAM_choice.same(effectFUN, list(ego, alter), NULL, 3, 2)$stat

  expect_equal(dim(stat), c(3L, 2L))
  expect_equal(stat, 1 * outer(ego, alter, "=="))
  # No diagonal is excluded: rows and columns index different node sets, so
  # [i, i] is an ordinary dyad rather than a self-tie.
  expect_equal(stat[2, 1], 1, label = "ego 2 equals alter 2")
})

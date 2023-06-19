test_that(
  "Reduce size updates",
  {
    depUpdates <- matrix(
      c(1, 2, 0, 2, 3, 1, 4, 5, 1), nrow = 3,
      dimnames = list(NULL, c("node1", "node2", "replace")),
      byrow = TRUE
    )
    updates <- matrix(
      c(1, 2, 1, 4, 5, 2), nrow = 2,
      dimnames = list(NULL, c("node1", "node2", "replace")),
      byrow = TRUE
    )

    check1 <- ReduceUpdateNonDuplicates(NULL, updates)
    expect_type(check1, "double")
    expect_true(inherits(check1, "array"))
    expect_length(check1, 2 * 3)
    expect_equal(check1, updates)

    check2 <- ReduceUpdateNonDuplicates(depUpdates, updates)
    outcome <- matrix(
      c(2, 3, 1, 1, 2, 1, 4, 5, 2), nrow = 3,
      dimnames = list(NULL, c("node1", "node2", "replace")),
      byrow = TRUE
    )
    expect_type(check2, "double")
    expect_true(inherits(check2, "array"))
    expect_length(check2, 3 * 3)
    expect_equal(check2, outcome)

    check3 <- ReduceUpdateNonDuplicates(depUpdates, NULL)
    expect_type(check3, "double")
    expect_true(inherits(check3, "array"))
    expect_length(check3, 3 * 3)
    expect_equal(check3, depUpdates)

    expect_null(ReduceUpdateNonDuplicates(NULL, NULL))
  }
)

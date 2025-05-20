test_that("compositional change", {
  compositionalEvents <- find_presence(actorsEx)
  expect_s3_class(compositionalEvents, "character")
  expect_equal(compositionalEvents, "compChange")
})

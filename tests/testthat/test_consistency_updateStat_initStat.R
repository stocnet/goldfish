context("Consistency updateStats-initStat functions")

test_that(
  "consistency between the updateStats function and the initStat function for all effects in formula", {
    expect_equal(finalStat, initStatList, label = "updatesStats consistency with initStat")
  }
)

test_that(
  "consistency between the updateStats function and the initStat function of inertia effect", {
    expect_true(
      any(abs(finalStat[[1]]) > 1e-5),
      label = "stats of the inertia effect should be nonzero"
    ) # They should be the same
    expect_equal(
      finalStat[[1]], initStatList[[1]],
      label = "the updateStats function and the initStat function of inertia effect should be consistent"
    )
  }
)



test_that(
  "consistency between the updateStats function and the initStat function of recip effect", {
    expect_true(any(abs(finalStat[[2]]) > 1e-5),
      label = "stats of the recip effect should be nonzero"
    ) # They should be the same
    expect_equal(finalStat[[2]], initStatList[[2]],
      label = "the updateStats function and the initStat function of recip effect should be consistent"
    )
  }
)

test_that(
  "consistency between the updateStats function and the initStat function of trans effect", {
    expect_true(
      any(abs(finalStat[[3]]) > 1e-5),
      label = "stats of the trans effect should be nonzero"
    ) # They should be the same
    expect_equal(finalStat[[3]], initStatList[[3]],
      label = "the updateStats function and the initStat function of trans effect should be consistent"
    )
  }
)

test_that(
  "consistency between the updateStats function and the initStat function of indeg effect", {
    expect_true(
      any(abs(finalStat[[4]]) > 1e-5),
      label = "stats of the indeg effect should be nonzero"
    ) # They should be the same
    expect_equal(finalStat[[4]], initStatList[[4]],
      label = "the updateStats function and the initStat function of indeg effect should be consistent"
    )
  }
)

test_that(
  "consistency between the updateStats function and the initStat function of tie effect", {
    expect_true(
      any(abs(finalStat[[5]]) > 1e-5),
      label = "stats of the tie effect should be nonzero"
    ) # They should be the same
    expect_equal(finalStat[[5]], initStatList[[5]],
      label = "the updateStats function and the initStat function of tie effect should be consistent"
    )
  }
)

test_that(
  "consistency between the updateStats function and the initStat function of four effect", {
    expect_true(
      any(abs(finalStat[[6]]) > 1e-5),
      label = "stats of the four effect should be nonzero"
    ) # They should be the same
    expect_equal(finalStat[[6]], initStatList[[6]],
      label = "the updateStats function and the initStat function of four effect should be consistent"
    )
  }
)

test_that("summary goldfish", {
  objSum <- summary(resModObject)
  expect_s3_class(objSum, "summary.result.goldfish")
  expect_length(objSum, 18)
  expect_true(inherits(objSum$coefMat, "array"))
  expect_type(objSum$coefMat, "double")
  expect_length(objSum$coefMat, resModObject$nParams * 4)
  expect_true(is.na(objSum$coefMat[2, 2]))
  expect_type(objSum$AIC, "double")
  expect_type(objSum$BIC, "double")
  expect_length(objSum$AIC, 1)
  expect_length(objSum$BIC, 1)
})
test_that("summary goldfish print", {
  # objInv <- expect_invisible(print(summary(resModObject)))
  expect_output(print(summary(resModObject)), "AIC")
  expect_output(print(summary(resModObject)), "BIC")
  expect_output(print(summary(resModObject)), "Call:")
  expect_output(print(summary(resModObject)), "Coefficients:")
  expect_failure(expect_output(print(summary(resModObject)), "\nrecip"))
  expect_output(
    print(summary(resModObject), complete = TRUE), "\nrecip"
  )
  expect_failure(expect_output(
    print(summary(resModObject)), "\nEffects details"
  ))
  expect_output(
    print(summary(resModObject), complete = TRUE), "\nEffects details"
  )
})
test_that("result print", {
  expect_output(print(resModObject), "Call:")
  expect_output(print(resModObject), "Coefficients:")
  expect_failure(
    expect_output(print(resModObject), "\ninertia    recip    trans")
  )
  expect_output(
    print(resModObject, complete = TRUE), "\ninertia    recip    trans"
  )
})
test_that("nodes print", {
  expect_output(print(actorsEx), paste("Number of nodes:", nrow(actorsEx)))
  expect_output(
    print(actorsEx),
    paste("Number of present nodes:", sum(actorsEx$present))
  )
  expect_output(print(actorsEx), "Dynamic attribute")
  expect_failure(expect_output(print(actorsEx, full = TRUE), "First \\d rows"))
  expect_output(
    print(defineNodes(testAttr)),
    paste("Number of nodes:", nrow(testAttr))
  )
  expect_failure(
    expect_output(print(defineNodes(testAttr)), "Number of present nodes:")
  )
  expect_failure(
    expect_output(print(defineNodes(testAttr)), "Dynamic attribute")
  )
  expect_output(print(defineNodes(testAttr)), "First 6 rows")
  expect_failure(
    expect_output(print(defineNodes(testAttr), full = TRUE), "First 6 rows")
  )
})
test_that("network print", {
  expect_output(
    print(networkState),
    paste("Dimensions:", paste(dim(networkState), collapse = " "))
  )
  expect_output(
    print(networkState),
    paste("Number of ties \\(no weighted\\):", sum(networkState > 0))
  )
  expect_output(print(networkState), "Nodes set\\(s\\): actorsEx")
  expect_output(print(networkState), "It is a one-mode and directed network")
  expect_output(print(networkState), "Linked events: eventsIncrement")
  expect_output(print(networkState), "First \\d rows and columns")
  expect_failure(
    expect_output(
      print(networkState, full = TRUE),
      "First \\d rows and columns"
    )
  )

  netTest <- defineNetwork(matrix = m, nodes = actorsEx)
  expect_output(
    print(netTest),
    paste("Dimensions:", paste(dim(netTest), collapse = " "))
  )
  expect_output(
    print(netTest),
    paste("Number of ties \\(no weighted\\):", sum(netTest > 0, na.rm = TRUE))
  )
  expect_output(print(netTest), "Nodes set\\(s\\): actorsEx")
  expect_output(print(netTest), "It is a one-mode and directed network")
  expect_failure(
    expect_output(print(netTest), "Linked events: eventsIncrement")
  )
  expect_output(print(netTest), "First \\d rows and columns")
  expect_failure(
    expect_output(
      print(netTest, full = TRUE),
      "First \\d rows and columns"
    )
  )

  expect_output(
    print(networkActorClub),
    paste("Dimensions:", paste(dim(networkActorClub), collapse = " "))
  )
  expect_output(
    print(networkActorClub),
    paste("Number of ties \\(no weighted\\):", sum(networkActorClub))
  )
  expect_output(print(networkActorClub), "Nodes set\\(s\\): actorsEx clubsEx")
  expect_output(
    print(networkActorClub), "It is a two-mode and directed network"
  )
  expect_output(print(networkActorClub), "Linked events: eventsActorClub")
  expect_output(print(networkActorClub), "First \\d rows and columns")
  expect_failure(
    expect_output(
      print(networkActorClub, full = TRUE),
      "First \\d rows and columns"
    )
  )
})
test_that("dependent events", {
  expect_output(print(depNetwork), paste("Number of events:", nrow(depNetwork)))
  expect_output(print(depNetwork), "Nodes set\\(s\\): actorsEx")
  expect_output(print(depNetwork), "Default network: networkState")
  expect_output(print(depNetwork), "First \\d rows")
  expect_failure(
    expect_output(print(depNetwork, full = TRUE), "First \\d rows")
  )
})
test_that("preprocessed", {
  preproData <- estimate(
    depNetwork ~ inertia(networkState, weighted = TRUE) +
      tie(networkExog, weighted = TRUE),
    model = "DyNAM", subModel = "choice",
    preprocessingOnly = TRUE
  )
  expect_output(print(preproData), "Preprocess object for the model")
})
test_that("tidy results", {
  expect_s3_class(tidy(resModObject), "tbl_df")
  expect_length(tidy(resModObject), 5)
  expect_equal(nrow(tidy(resModObject)), 2L)
  expect_equal(
    tidy(resModObject)$term,
    paste("callNetwork", c("inertia", "trans"), "FALSE")
  )
  expect_length(tidy(resModObject, conf.int = TRUE), 7)
  expect_equal(
    tidy(resModObject, complete = TRUE)$term,
    paste("callNetwork", c("inertia", "recip", "trans"), c(FALSE, TRUE, FALSE))
  )
  expect_true(
    anyNA(tidy(resModObject, complete = TRUE, conf.int = TRUE)$statistic)
  )
})
test_that("glance results", {
  expect_s3_class(glance(resModObject), "tbl_df")
  expect_length(glance(resModObject), 5)
  expect_equal(nrow(glance(resModObject)), 1L)
  expect_equal(glance(resModObject)$logLik, resModObject$logLikelihood)
})

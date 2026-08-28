# A process records the events IT modeled, not the whole layer.
#
# The same claim task 1.2 made for the observation window, on the other axis
# that narrows a fit's event set. There the window narrowed it in time; here the
# flavor narrows it by kind, and the recorded table has to follow both.
#
# Asserted as row IDENTITY rather than a row count, and for a reason the count
# makes visible: on an alternating creation/dissolution stream each flavor holds
# half the events and each process spans roughly the other half as censored
# intervals, so `nrow(dependent_events)` and the interval count agree to within
# one. A length check passes on the unfiltered table.

test_that("each process records only the events of its own flavor", {
  skip_on_cran()
  container <- flavored_container_fit()
  map <- container$process_map

  for (i in seq_len(nrow(map))) {
    fit <- container$results[[as.character(map$fid[i])]]
    label <- paste(map$flavor[i], map$family[i])

    expect_equal(
      fit$dependent_events$time,
      fit$event_time[!fit$right_censored_events],
      info = label
    )
  }
})

test_that("the flavors partition the layer rather than each holding it", {
  # The complementary statement, and the one that fails loudest on the
  # unfiltered table: two processes of the same family record disjoint event
  # sets whose union is the layer, not two copies of the layer.
  skip_on_cran()
  container <- flavored_container_fit()

  creation <- fit_of(container, "creation", "rate")$dependent_events$time
  dissolution <- fit_of(container, "dissolution", "rate")$dependent_events$time

  expect_length(intersect(creation, dissolution), 0L)
  expect_false(setequal(creation, dissolution))
})

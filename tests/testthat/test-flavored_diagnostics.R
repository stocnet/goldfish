# The flavored diagnostic surface: a container's tidy tables row-bind the
# per-process ones and carry the process identity as columns.
#
# The tidy and non-tidy surfaces take different routes on purpose -- a column
# can hold the identity, a vector cannot -- so the non-tidy methods return a
# list named by process label instead. This file covers the tidy side.

test_that("augment row-binds the processes and appends the identity", {
  skip_on_cran()
  container <- flavored_container_fit()

  tidy <- augment(container)
  parts <- lapply(container$results, augment)

  expect_equal(nrow(tidy), sum(vapply(parts, nrow, integer(1))))
  # Appended, not interleaved: the event columns stay positionally where a
  # single-process fit puts them, which is what lets one reading of the table
  # serve both shapes.
  expect_equal(tail(names(tidy), 2L), c("flavor", "family"))
  expect_equal(
    head(names(tidy), -2L),
    names(parts[[1]])
  )
})

test_that("the identity columns say which process each row came from", {
  skip_on_cran()
  container <- flavored_container_fit()

  tidy <- augment(container)
  map <- container$process_map

  # Flavor-major, the order every other view of the container presents -- which
  # is NOT the `process_map`'s own order, that one being family-major. The
  # distinction is the whole point of routing every flavored method through the
  # shared walk instead of iterating the map.
  presented <- map[goldfish:::flavored_row_order(container), ]
  expect_equal(
    unique(tidy[, c("flavor", "family")]),
    tibble::as_tibble(presented[, c("flavor", "family")])
  )
  # The same order `coef()` labels its components in, which is the assertion
  # that would catch the two surfaces drifting apart again.
  expect_equal(
    names(coef(container)),
    sprintf("calls › %s › %s", presented$flavor, presented$family)
  )

  for (i in seq_len(nrow(map))) {
    block <- tidy[
      tidy$flavor == map$flavor[i] & tidy$family == map$family[i],
    ]
    alone <- augment(container$results[[as.character(map$fid[i])]])
    expect_equal(
      block[, setdiff(names(block), c("flavor", "family"))],
      alone,
      ignore_attr = TRUE
    )
  }
})

test_that("a rate process contributes its censored remainder too", {
  # The per-event contract lets a rate fit end on a span that closes no waiting
  # time. Row-binding must carry that row rather than trimming the blocks to a
  # common length, so the flavored table is not silently one row short of the
  # per-process ones it claims to be made of.
  skip_on_cran()
  container <- flavored_container_fit()

  tidy <- augment(container)
  rate_rows <- tidy[tidy$flavor == "creation" & tidy$family == "rate", ]
  fit <- fit_of(container, "creation", "rate")

  expect_equal(nrow(rate_rows), fit$n_events + sum(rate_rows$censored))
  expect_equal(
    sum(tidy$censored),
    sum(vapply(
      container$results,
      function(f) sum(augment(f)$censored),
      integer(1)
    ))
  )
})

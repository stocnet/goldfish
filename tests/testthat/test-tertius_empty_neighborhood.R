# The tertius effects define a value for a node with no in-neighbors -- its
# aggregate is over an empty set and so undefined, and the effect's definition
# fills it with the mean of the defined entries. This is part of the statistic's
# definition, not attribute imputation. These regression tests pin the numeric
# behavior so the empty-neighborhood default cannot drift.
#
# Fixture: 4 nodes, attribute z = 1:4. In-neighbors by receiver column --
# node 1: {2, 3}; node 2: {1}; node 3: none (the empty neighborhood);
# node 4: {1, 2, 3}. Node 3's statistic is therefore the mean of the defined
# entries.

tertius_network <- function() {
  net <- matrix(0, 4, 4)
  net[2, 1] <- 1
  net[3, 1] <- 1
  net[1, 2] <- 1
  net[1, 4] <- 1
  net[2, 4] <- 1
  net[3, 4] <- 1
  net
}

test_that("choice tertius_diff fills an empty neighborhood with defined mean", {
  effect_fun <- function(
    network,
    attribute,
    is_two_mode = FALSE,
    transformer_fn = abs,
    summarizer_fn = function(x) mean(x, na.rm = TRUE)
  ) {}
  z <- c(1, 2, 3, 4)

  init <- init_DyNAM_choice.tertius_diff(
    effect_fun,
    tertius_network(),
    z,
    NULL,
    4,
    4
  )

  # Node 3 has no in-neighbors, so its cache aggregate is undefined (NA).
  expect_equal(init$cache, c(2.5, 1, NA, 2))
  # Its statistic column takes the empty-neighborhood default: the mean of the
  # defined entries (1.0555...), with the one-mode diagonal held at zero.
  expect_equal(
    init$stat,
    matrix(
      c(
        0,
        0.5,
        0.5,
        1.5,
        0,
        0,
        2,
        3,
        1.0555555556,
        1.0555555556,
        0,
        1.0555555556,
        1,
        0,
        1,
        0
      ),
      nrow = 4,
      byrow = FALSE
    ),
    tolerance = 1e-8
  )
})

test_that("rate tertius fills an empty in-neighborhood with the defined mean", {
  effect_fun <- function(
    network,
    attribute,
    is_two_mode = FALSE,
    transformer_fn = identity,
    summarizer_fn = function(x) mean(x, na.rm = TRUE)
  ) {}
  z <- c(1, 2, 3, 4)

  init <- init_DyNAM_rate.tertius(effect_fun, tertius_network(), z, NULL, 4, 4)

  expect_equal(init$cache, c(2.5, 1, NA, 2))
  # Node 3's rate statistic is the mean of the defined entries: (2.5+1+2)/3.
  expect_equal(
    as.vector(init$stat),
    c(2.5, 1, 1.8333333333, 2),
    tolerance = 1e-8
  )
})

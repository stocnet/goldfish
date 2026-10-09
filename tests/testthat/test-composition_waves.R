# Composition-to-waves: which actors an interval's draw reads, and (later) the
# rewrite of their presence. Fixtures are small joint specifications with one
# modeled panel layer; the specification is built on event-observed data and
# joined on the panel-observed copy, the way the estimate_dynes() fixtures do.

# Six one-mode actors, wave grid c(0, 2, 5). `friendship` is the modeled panel
# layer (flips 1 -> 2 in interval 1, 3 -> 4 in interval 2), `calls` a modeled
# relational layer, `emails` a relational layer no process models.
involvement_data <- function(observation) {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(
        friendship = "increment",
        calls = "increment",
        emails = "increment"
      ),
      directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
      observation = c(
        friendship = observation,
        calls = "event",
        emails = "event"
      )
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 3L),
        to = c(2L, 4L),
        time = c(2, 5),
        layer = "friendship"
      ),
      data.frame(
        from = c(5L, 2L),
        to = c(6L, 5L),
        time = c(2, 4),
        layer = "calls"
      ),
      data.frame(from = 3L, to = 4L, time = 1, layer = "emails")
    )
  )
}

involvement_spec <- function() {
  events <- involvement_data("event")
  make_joint_specification(
    make_specification(
      choice = ~inertia,
      layer = "friendship",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = events
    ),
    data = involvement_data("panel")
  )
}

# Two node sets (p -> o). The modeled panel layer `membership` flips P2 -> O1,
# whose local indices are (2, 1) but whose global ids are (2, 3).
involvement_twomode_spec <- function() {
  data <- function(observation) {
    list(
      info = list(
        name = "toy2",
        focal = "membership",
        update = c(membership = "increment", talks = "increment"),
        directed = c(membership = TRUE, talks = TRUE),
        observation = c(membership = observation, talks = "event"),
        sender = "p",
        receiver = "o"
      ),
      nodes = data.frame(
        label = c("P1", "P2", "O1", "O2"),
        mode = c("p", "p", "o", "o"),
        stringsAsFactors = FALSE
      ),
      ties = rbind(
        data.frame(from = 2L, to = 3L, time = 2, layer = "membership"),
        data.frame(from = 1L, to = 4L, time = 1, layer = "talks")
      )
    )
  }
  events <- data("event")
  make_joint_specification(
    make_specification(
      choice = ~inertia,
      layer = "membership",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "talks",
      model = "DyNAM",
      data = events
    ),
    data = data("panel")
  )
}

test_that("a modeled relational event involves its sender and receiver", {
  involved <- composition_involvement(involvement_spec(), c(0, 2, 5))
  expect_length(involved, 2L)
  # calls 5 -> 6 sits exactly at w_1 = 2, so it belongs to interval 1.
  expect_true(all(c(5L, 6L) %in% involved[[1L]]))
  # calls 2 -> 5 at t = 4 lies inside interval 2.
  expect_true(all(c(2L, 5L) %in% involved[[2L]]))
})

test_that("a flip involves its from and to nodes in its own interval", {
  involved <- composition_involvement(involvement_spec(), c(0, 2, 5))
  expect_true(all(c(1L, 2L) %in% involved[[1L]]))
  expect_true(all(c(3L, 4L) %in% involved[[2L]]))
  expect_false(any(c(3L, 4L) %in% involved[[1L]]))
})

test_that("an exogenous relational event involves nobody", {
  involved <- composition_involvement(involvement_spec(), c(0, 2, 5))
  # emails 3 -> 4 at t = 1 is replayed, not selected; 3 and 4 flip only in
  # interval 2.
  expect_identical(involved[[1L]], c(1L, 2L, 5L, 6L))
  expect_identical(involved[[2L]], c(2L, 3L, 4L, 5L))
})

test_that("a two-mode flip involves its receiver on the second node set", {
  involved <- composition_involvement(involvement_twomode_spec(), c(0, 2))
  # P2 -> O1 flips (global ids 2 and 3); the talks event P1 -> O2 is at t = 1
  # inside the same interval (global ids 1 and 4).
  expect_true(all(c(2L, 3L) %in% involved[[1L]]))
  expect_true(all(c(1L, 4L) %in% involved[[1L]]))
})

test_that("a two-mode receiver is not read as the same-index first-set node", {
  spec <- involvement_twomode_spec()
  spec$process_map <- spec$process_map[spec$process_map$layer != "talks", ]
  involved <- composition_involvement(spec, c(0, 2))
  expect_identical(involved[[1L]], c(2L, 3L))
})

# The legacy data constructors are soft-deprecated toward the stocnet workflow;
# the DyNAMi constructor is untouched. lifecycle::expect_deprecated() forces the
# warning past the per-session dedup.

test_that("each legacy data constructor emits a lifecycle deprecation", {
  actors <- data.frame(label = c("a", "b", "c"), present = TRUE)
  events <- data.frame(
    time = c(1, 2),
    sender = c("a", "b"),
    receiver = c("b", "c"),
    increment = 1
  )

  lifecycle::expect_deprecated(nodes <- make_nodes(actors))
  lifecycle::expect_deprecated(net <- make_network(nodes = actors))
  lifecycle::expect_deprecated(net <- link_events(net, events, nodes = actors))
  lifecycle::expect_deprecated(
    dep <- make_dependent_events(events, nodes = actors, default_network = net)
  )
  lifecycle::expect_deprecated(make_global_attributes(data.frame(winter = 0)))
  lifecycle::expect_deprecated(make_data(dep, net, events, actors))
})

test_that("make_network's deprecation shows the stocnet replacement code", {
  actors <- data.frame(label = c("a", "b"), present = TRUE)
  contact <- matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  lifecycle::expect_deprecated(
    net <- make_network(contact, nodes = actors),
    "join_nodes\\(actors\\)"
  )
})

test_that("the DyNAMi constructor emits no lifecycle deprecation", {
  withr::local_options(lifecycle_verbosity = "warning")
  records <- data.frame(
    nodeA = c(1, 3, 1, 4),
    nodeB = c(2, 4, 3, 2),
    Start = c(0, 0, 4, 5),
    End = c(3, 3, 5, 7)
  )
  actors <- data.frame(label = letters[1:4], present = TRUE)
  expect_no_condition(
    make_groups_interaction(records, actors, seed_randomization = 123),
    class = "lifecycle_warning_deprecated"
  )
})

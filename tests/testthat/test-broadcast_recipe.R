# Broadcast activation: the recipe emits compact broadcast entries for
# broadcast-eligible effects (alter -> kind 1, ego -> kind 2, global -> kind 3)
# instead of duplicate point columns.

test_that("alter() emits kind-1 broadcast entries, no point cols for it", {
  data_fish <- baselines_fisheries_data()
  # effects (0-indexed): inertia=0, tie=1, alter=2, diff=3
  prep <- compute_statistics(
    create_bilat ~ inertia +
      tie(contignet) +
      alter(states$regime) +
      diff(states$regime),
    data = data_fish,
    model = "DyNAM",
    sub_model = "choice"
  )
  bc <- prep$stat_mat_broadcast
  expect_gt(ncol(bc), 0)
  # alter is the only broadcast-eligible effect here -> all entries kind 1,
  # effect column 2 (0-indexed)
  expect_true(all(bc[1, ] == 1L))
  expect_true(all(bc[3, ] == 2L))
  # the point buffer carries no columns for the alter effect (col index 2)
  expect_false(any(prep$stat_mat_update[3, ] == 2L))
  # cell-specific effects stay on the point path
  expect_true(any(prep$stat_mat_update[3, ] == 0L)) # inertia
})

test_that("ego-type degree emits kind-2 broadcast entries", {
  se <- baselines_social_evolution_data()
  # effects (0-indexed, intercept excluded): indeg(ego)=0, inertia=1, recip=2
  prep <- compute_statistics(
    calls_dependent ~ 1 + indeg(call_network, type = "ego") + inertia + recip,
    data = se,
    model = "REM"
  )
  bc <- prep$stat_mat_broadcast
  expect_gt(ncol(bc), 0)
  expect_true(all(bc[1, ] == 2L))
  expect_true(all(bc[3, ] == 0L))
  expect_false(any(prep$stat_mat_update[3, ] == 0L))
})

test_that("global() emits kind-3 broadcast entries in a rate model", {
  gd <- baselines_global_data()
  # effects (intercept excluded): indeg=0, global=1
  prep <- compute_statistics(
    calls_dependent ~ 1 + indeg + global(seasons$winter),
    data = gd,
    model = "DyNAM",
    sub_model = "rate"
  )
  bc <- prep$stat_mat_broadcast
  expect_gt(ncol(bc), 0)
  expect_true(all(bc[1, ] == 3L))
  expect_true(all(bc[3, ] == 1L))
  # indeg (per-actor distinct values) stays a point update
  expect_true(any(prep$stat_mat_update[3, ] == 0L))
  expect_false(any(prep$stat_mat_update[3, ] == 1L))
})

test_that("models with only cell-specific effects emit no broadcasts", {
  se <- baselines_social_evolution_data()
  prep <- compute_statistics(
    calls_dependent ~ inertia + recip + trans,
    data = se,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_equal(ncol(prep$stat_mat_broadcast), 0L)
})

test_that("broadcast grouping collapses a fan-out to one column per held index", {
  # A kind-1 (alter) effect emits one row per sender for the single receiver
  # that moved; the grouping keeps one column at that held index, not one per
  # sender.
  updates <- cbind(
    node1 = c(1L, 2L, 4L),
    node2 = c(3L, 3L, 3L),
    replace = c(5, 5, 5)
  )
  block <- broadcast_entries_from_updates(updates, 1L, 2L)
  expect_equal(ncol(block), 1L)
  expect_equal(as.vector(block), c(1, 2, 1, 5))
})

test_that("broadcast grouping rejects a non-constant fan-out", {
  # The constant-value abort is broadcast-encoding business and stays at full
  # strength: one held index carrying two values is not a pure broadcast.
  updates <- cbind(node1 = c(1L, 2L), node2 = c(3L, 3L), replace = c(1, 2))
  expect_error(
    broadcast_entries_from_updates(updates, 1L, 2L),
    "non-constant fan-out"
  )
})

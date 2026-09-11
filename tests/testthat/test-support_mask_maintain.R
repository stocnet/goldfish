# Incremental support-mask maintenance + from-scratch equivalence.
# The constraint sub-plan is maintained in a self-contained recipe
# pass; at every dependent event its support mask must equal a from-scratch
# evaluation of the constraint on the atoms' current statistics. The pass is
# attached additively to the preprocessed object, so an unconstrained model is
# unaffected.

make_mask_fixture <- function(n_events = 80L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  calls_dependent <- calls_dependent[seq_len(n_events), ]
  # A legacy environment for the low-level pass that drives the constraint
  # builders directly with node-set names and the dependent object; make_data()
  # returns the stocnet the higher-level tests estimate against.
  env <- new.env()
  assign("call_network", call_network, envir = env)
  assign("calls_dependent", calls_dependent, envir = env)
  assign("calls", calls, envir = env)
  assign("actors", actors, envir = env)
  list(
    data = make_data(calls_dependent, call_network, calls, actors),
    env = env,
    actors = actors,
    calls = as.data.frame(calls),
    dep_times = calls_dependent$time,
    n_events = n_events
  )
}

# Independent from-scratch reference for `~ tie(net)`: the support at a dependent
# event is the cumulative call adjacency (increments) strictly before its time.
tie_support_from_scratch <- function(fx, event_time) {
  lab <- fx$actors$label
  n <- length(lab)
  adj <- matrix(0, n, n)
  prev <- fx$calls[fx$calls$time < event_time, , drop = FALSE]
  for (r in seq_len(nrow(prev))) {
    i <- match(prev$sender[r], lab)
    j <- match(prev$receiver[r], lab)
    adj[i, j] <- adj[i, j] + prev$increment[r]
  }
  adj != 0
}

run_mask_pass <- function(fx) {
  d <- fx$env
  cp <- parse_and_validate_constraint(
    ~ tie(call_network),
    has_dyad_part = TRUE,
    envir = d
  )
  sub <- compile_support_constraint(
    cp,
    model = "DyNAM",
    dep_name = "calls_dependent",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = d
  )
  preprocess_support_mask(
    sub,
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    symmetric = FALSE,
    snapshot_times = fx$dep_times,
    prep_envir = d
  )
}

test_that("the initial mask is empty for an initially-empty tie network", {
  fx <- make_mask_fixture()
  mt <- run_mask_pass(fx)
  expect_false(any(mt$initial))
  expect_identical(mt$n_stored, fx$n_events)
  expect_length(mt$update_pointer, fx$n_events)
})

test_that("incrementally maintained mask equals from-scratch at every event", {
  fx <- make_mask_fixture()
  mt <- run_mask_pass(fx)
  timeline <- mask_timeline(mt)
  for (e in seq_len(fx$n_events)) {
    ref <- tie_support_from_scratch(fx, fx$dep_times[e])
    expect_equal(unname(timeline[[e]]), unname(ref))
  }
})

test_that("the rate (sender) loop realizes the same dyad support at all events", {
  fx <- make_mask_fixture()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    rate_sub_model = "rate",
    choice_sub_model = "choice",
    layer = "calls_dependent",
    support_constraint = ~ tie(call_network),
    data = fx$data
  )
  prep_rate <- estimate_dynam(
    spec,
    sub_model = "rate",
    preprocessing_only = TRUE
  )
  expect_false(is.null(prep_rate$support_mask))
  # the mask timeline aligns with the preprocessed object's (right-censored +
  # dependent) events, and each equals the from-scratch reference at its time
  sm <- prep_rate$support_mask
  expect_identical(sm$n_stored, length(prep_rate$event_time))
  timeline <- mask_timeline(sm)
  for (e in seq_along(prep_rate$event_time)) {
    ref <- tie_support_from_scratch(fx, prep_rate$event_time[e])
    expect_equal(unname(timeline[[e]]), unname(ref))
  }
})

test_that("a constrained model attaches support_mask; unconstrained does not", {
  fx <- make_mask_fixture()
  prep_c <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(call_network)
  )
  prep_u <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE
  )
  expect_null(prep_u$support_mask)
  expect_false(is.null(prep_c$support_mask))
  expect_length(prep_c$support_mask$update_pointer, fx$n_events)
  # the statistics output is unchanged by attaching the mask
  expect_equal(prep_c$initial_stats, prep_u$initial_stats)
})

test_that("a separable constraint stores one vector per snapshot, not a grid", {
  # There is one stored mask per snapshot time, so the dense form is the memory
  # ceiling on any realistic node set: 1899 actors and 59,835 snapshots is about
  # 860 GB dense against 437 MB as receiver vectors. The classification already
  # existed; only the storage was missing.
  fx <- make_mask_fixture()
  prep <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) >= 0
  )
  mask <- prep$support_mask
  n1 <- length(mask$sender_presence_init %||% prep$active_sender_init)
  n2 <- length(mask$receiver_presence_init)

  first <- mask_timeline(mask)[[1L]]
  expect_equal(mask$stored_kind, 1L)
  expect_null(dim(first))
  expect_length(first, n2)
  expect_equal(
    dim(support_to_grid(first, mask$stored_kind, n1, n2)),
    c(n1, n2)
  )
})

test_that("a separable constraint stores each node's own value", {
  # Regression: the reduction to a separable kind used to read row or column 1
  # of the mask grid, which is the DIAGONAL entry for node 1. A dyad statistic
  # is a broadcast everywhere except on its diagonal, so node 1 came back with
  # the zeroed value and was silently excluded from every event's risk set.
  #
  # The existing constraint tests cannot see this: `indeg(...) >= 0` and
  # `indeg(...) > 0` agree on a zeroed diagonal and on a genuine zero. It needs
  # a nodal attribute whose first actor is on the allowed side of the threshold.
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  fx <- make_mask_fixture()
  expect_gt(actors$floor[1L], 2)

  for (side in c("alter", "ego")) {
    constraint <- if (side == "alter") {
      ~ alter(actors$floor) > 2
    } else {
      ~ ego(actors$floor) > 2
    }
    prep <- estimate_dynam(
      calls_dependent ~ inertia,
      sub_model = "choice",
      data = fx$data,
      preprocessing_only = TRUE,
      support_constraint = constraint
    )
    expect_identical(
      as.logical(prep$support_mask$initial),
      as.logical(actors$floor > 2),
      info = side
    )
  }
})

test_that("a dyadic constraint still stores the dense grid", {
  # The control: a point-kind mask is not separable and must stay a matrix.
  fx <- make_mask_fixture()
  prep <- estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(call_network)
  )
  mask <- prep$support_mask

  expect_equal(mask$stored_kind, 0L)
  expect_equal(length(dim(mask_timeline(mask)[[1L]])), 2L)
})

test_that("a grid round-trips through its axis-union kind", {
  # Reducing a separable grid to its kind and projecting it back is the
  # identity, which is what lets a mask be stored at a kind narrower than the
  # grid it stands for.
  ego_grid <- matrix(c(TRUE, FALSE, TRUE), 3L, 4L)
  alter_grid <- matrix(c(TRUE, FALSE, TRUE, TRUE), 3L, 4L, byrow = TRUE)

  expect_identical(
    support_to_grid(reduce_value(ego_grid, 0L, 2L), 2L, 3L, 4L),
    ego_grid
  )
  expect_identical(
    support_to_grid(reduce_value(alter_grid, 0L, 1L), 1L, 3L, 4L),
    alter_grid
  )
  expect_identical(
    support_to_grid(reduce_value(matrix(TRUE, 3L, 4L), 0L, 3L), 3L, 3L, 4L),
    matrix(TRUE, 3L, 4L)
  )
})

# --- atoms maintained at their kind, written in place ----------------------- #

# A node set large enough that a copied grid is unmistakable in an allocation
# figure: 400 actors is 1.28 MB a grid, so copying one per event over 300 events
# would be 384 MB.
make_wide_atom_fixture <- function(
  n_actors = 400L,
  n_events = 300L,
  seed = 5L
) {
  withr::local_seed(seed)
  actors <- data.frame(
    label = paste0("A", seq_len(n_actors)),
    present = TRUE,
    stringsAsFactors = FALSE
  )
  senders <- sample.int(n_actors, n_events, replace = TRUE)
  receivers <- (senders +
    sample.int(n_actors - 1L, n_events, replace = TRUE)) %%
    n_actors +
    1L
  calls <- data.frame(
    time = seq_len(n_events),
    sender = actors$label[senders],
    receiver = actors$label[receivers],
    increment = 1,
    stringsAsFactors = FALSE
  )
  actors <- make_nodes(actors)
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  env <- new.env()
  assign("call_network", call_network, envir = env)
  assign("calls_dependent", calls_dependent, envir = env)
  assign("calls", calls, envir = env)
  assign("actors", actors, envir = env)
  list(env = env, times = calls$time, n_events = n_events)
}

wide_atom_maintainer <- function(fx, constraint) {
  plan <- parse_and_validate_constraint(
    constraint,
    has_dyad_part = TRUE,
    envir = fx$env
  )
  sub <- compile_support_constraint(
    plan,
    model = "DyNAM",
    dep_name = "calls_dependent",
    nodes = "actors",
    nodes2 = "actors",
    window_derivations = NULL,
    envir = fx$env
  )
  build_atom_maintainer(
    sub,
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    prep_envir = fx$env
  )
}

test_that("an atom is stored at its own kind, not as a dense grid", {
  fx <- make_wide_atom_fixture()
  alter <- wide_atom_maintainer(fx, ~ indeg(call_network) >= 0)
  expect_identical(alter$atom_kind("indeg(call_network)"), 1L)
  expect_null(dim(alter$atom_value("indeg(call_network)")))
  expect_length(alter$atom_value("indeg(call_network)"), 400L)

  # The control: a genuinely dyadic atom is still dense.
  point <- wide_atom_maintainer(fx, ~ tie(call_network))
  expect_identical(point$atom_kind("tie(call_network)"), 0L)
  expect_identical(dim(point$atom_value("tie(call_network)")), c(400L, 400L))
})

test_that("the atom walk duplicates neither its buffers nor its state", {
  # ADR-0059's invariant applied to the atom pass. The atom templates receive
  # `state$networks[[key]]` as an argument, which marks it shared, so the state
  # write used to duplicate the adjacency matrix on every event; the atom
  # buffers were duplicated the same way. The assertion is on identity --
  # `tracemem` reports every duplication -- not on the values.
  skip_if_not(capabilities("profmem"), "R built without memory profiling")
  fx <- make_wide_atom_fixture()
  maintainer <- wide_atom_maintainer(fx, ~ tie(call_network))
  frame <- environment(maintainer$advance)

  buffer <- get("1", envir = frame$atom_state)
  network <- frame$state$networks[[1L]]
  invisible(tracemem(buffer))
  invisible(tracemem(network))
  # `tracemem` reports on stdout; capturing "message" would observe nothing.
  copies <- utils::capture.output(
    for (tt in fx$times) {
      maintainer$advance(tt)
    },
    type = "output"
  )
  untracemem(buffer)
  untracemem(network)

  expect_length(copies, 0L)
  # And the walk really ran, so the absence of copies is not an absence of work.
  expect_true(any(frame$state$networks[[1L]] != 0))
})

test_that("advancing the atom stream costs far less than a grid per event", {
  skip_on_cran()
  skip_if_not_installed("bench")
  # 400 actors is 1.28 MB a grid. Copying one per event over 300 events is
  # 384 MB. The bound is a tenth of that: comfortably under the defect and well
  # clear of what the effect closures themselves allocate, which is a few tens
  # of KB an event and is not this test's business.
  fx <- make_wide_atom_fixture()
  budget <- 300 * 400 * 400 * 8 / 2^20 / 10
  for (constraint in list(~ indeg(call_network) >= 0, ~ tie(call_network))) {
    maintainer <- wide_atom_maintainer(fx, constraint)
    used <- bench::bench_memory(
      for (tt in fx$times) {
        maintainer$advance(tt)
        # A live consumer drains the touched set every step; letting it grow
        # measures the accumulator rather than the walk.
        maintainer$take_touched()
      }
    )
    expect_lt(as.numeric(used$mem_alloc) / 2^20, budget)
  }
})

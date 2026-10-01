# The resident pool store behind the map seam: each pooled sequence's data and
# statistics reach the store once, on pool entry, through the two mutations
# the loop performs (append a chunk, replace the whole pool); evaluation tasks
# then carry only sequence ids, theta, and the pool generation. On daemons the
# store is broadcast to every worker; on the serial path it is a local
# environment behind the same API, so both paths run one code path.

store_fixture <- function() {
  candidates <- abem_toy_candidates()
  decomposition <- hand_decomposition(
    candidates[[1]],
    choice = ~ inertia + recip,
    theta_index = list(choice = 1:2)
  )
  entry <- function(d) {
    list(data = d, reference = new_reference_record(c(0, 0), -1, -1))
  }
  list(
    decomposition = decomposition,
    data = candidates[[1]],
    sequences = lapply(candidates, entry),
    theta = c(0.3, -0.2)
  )
}

open_fixture_store <- function(seam, fx) {
  open_pool_store(
    seam,
    pool_store_header(
      fx$decomposition,
      theta_length = 2L,
      control_prep = set_preprocessing(),
      data = fx$data
    )
  )
}

# Every name reachable inside an object, through nested lists and closure
# environments short of a namespace.
reachable_names <- function(x, seen = new.env()) {
  if (is.function(x)) {
    env <- environment(x)
    if (is.null(env) || isNamespace(env) || identical(env, globalenv())) {
      return(character(0))
    }
    key <- format(env)
    if (!is.null(seen[[key]])) {
      return(character(0))
    }
    seen[[key]] <- TRUE
    objects <- as.list(env, all.names = TRUE)
    return(c(
      names(objects),
      unlist(lapply(objects, reachable_names, seen = seen))
    ))
  }
  if (is.list(x)) {
    return(c(names(x), unlist(lapply(x, reachable_names, seen = seen))))
  }
  character(0)
}

# A task's serialized size as an installed goldfish ships it: load_all() keeps
# each function's source reference, which drags the whole source file along
# (and grows once the file's lines are first read), and an installed package
# has none.
payload_bytes <- function(x) {
  if (is.function(x)) {
    x <- utils::removeSource(x)
  }
  length(serialize(x, NULL))
}

# The evaluation tasks a recording backend saw: those mapped over ids.
evaluation_tasks <- function(record) {
  Filter(
    function(t) all(vapply(t$x, rlang::is_scalar_integer, logical(1))),
    record$tasks
  )
}

# The resident store's ids and generation as every worker reports them.
resident_states <- function(seam, store) {
  seam$broadcast(pool_store_state, list(key = store$key))
}

expect_store_in_sync <- function(seam, store, pool) {
  for (state in resident_states(seam, store)) {
    expect_identical(state$ids, pool$ids)
    expect_identical(state$generation, pool$generation)
  }
}

# --- Task payload -------------------------------------------------------------

test_that("an evaluation task's payload does not grow with the pool", {
  record <- local_recording_parallel_backend()
  fx <- store_fixture()
  seam <- dynes_map_seam(set_algorithm_em(n_cores = 2L))
  on.exit(seam$close(), add = TRUE)
  store <- open_fixture_store(seam, fx)
  on.exit(close_pool_store(store), add = TRUE, after = FALSE)

  small <- build_sequence_pool(
    fx$decomposition,
    fx$sequences[1:2],
    theta_length = 2L,
    store = store
  )
  evaluate_sequence_pool(small, fx$theta, what = "loglik")
  large <- append_sequence_pool(small, fx$sequences)
  evaluate_sequence_pool(large, fx$theta, what = "loglik")

  tasks <- evaluation_tasks(record)
  expect_length(tasks, 2L)
  expect_length(tasks[[1]]$x, 2L)
  expect_length(tasks[[2]]$x, 5L)
  # The task function and each input are the same size at K = 2 and K = 5:
  # ids, theta and the generation, never a sequence.
  expect_identical(payload_bytes(tasks[[2]]$f), payload_bytes(tasks[[1]]$f))
  expect_identical(
    unique(vapply(c(tasks[[1]]$x, tasks[[2]]$x), payload_bytes, integer(1))),
    payload_bytes(1L)
  )
  for (task in tasks) {
    expect_disjoint(reachable_names(task$f), c("stats", "data", "sequences"))
  }
})

test_that("the static header crosses the seam once, when the store opens", {
  record <- local_recording_parallel_backend()
  fx <- store_fixture()
  seam <- dynes_map_seam(set_algorithm_em(n_cores = 2L))
  on.exit(seam$close(), add = TRUE)
  store <- open_fixture_store(seam, fx)
  on.exit(close_pool_store(store), add = TRUE, after = FALSE)
  expect_length(record$broadcasts, 1L)

  pool <- build_sequence_pool(
    fx$decomposition,
    fx$sequences[1:2],
    theta_length = 2L,
    store = store
  )
  pool <- append_sequence_pool(pool, fx$sequences[3])
  evaluate_sequence_pool(pool, fx$theta, what = "loglik")

  # One broadcast per mutation after the header, and no mutation re-sends it.
  expect_length(record$broadcasts, 3L)
  for (b in record$broadcasts[-1]) {
    expect_disjoint(reachable_names(b$args), "sub_models")
  }
})

# --- Store in sync with the main pool ----------------------------------------

for (backend in c("serial", "daemons")) {
  test_that(
    paste0(
      "the ",
      backend,
      " store tracks the pool through append and replace"
    ),
    {
      n_cores <- 1L
      if (backend == "daemons") {
        local_recording_parallel_backend()
        n_cores <- 2L
      }
      fx <- store_fixture()
      seam <- dynes_map_seam(set_algorithm_em(n_cores = n_cores))
      on.exit(seam$close(), add = TRUE)
      store <- open_fixture_store(seam, fx)
      on.exit(close_pool_store(store), add = TRUE, after = FALSE)

      pool <- build_sequence_pool(
        fx$decomposition,
        fx$sequences[1:2],
        theta_length = 2L,
        store = store
      )
      expect_length(pool$ids, 2L)
      expect_store_in_sync(seam, store, pool)

      appended <- append_sequence_pool(pool, fx$sequences[3])
      expect_identical(appended$ids[1:2], pool$ids)
      expect_length(appended$ids, 3L)
      expect_gt(appended$generation, pool$generation)
      expect_store_in_sync(seam, store, appended)

      # A replace clears the old entries: the store holds the new pool only.
      replaced <- build_sequence_pool(
        fx$decomposition,
        fx$sequences[2:3],
        theta_length = 2L,
        store = store
      )
      expect_disjoint(replaced$ids, appended$ids)
      expect_gt(replaced$generation, appended$generation)
      expect_store_in_sync(seam, store, replaced)
      for (state in resident_states(seam, store)) {
        expect_identical(state$n_entries, 2L)
      }

      # The store answers for the pool it holds, like a fresh serial pool.
      fresh <- build_sequence_pool(
        fx$decomposition,
        fx$sequences[2:3],
        theta_length = 2L
      )
      expect_identical(
        evaluate_sequence_pool(replaced, fx$theta),
        evaluate_sequence_pool(fresh, fx$theta)
      )
    }
  )

  test_that(
    paste0("the ", backend, " store refuses a task from a stale pool"),
    {
      n_cores <- 1L
      if (backend == "daemons") {
        local_recording_parallel_backend()
        n_cores <- 2L
      }
      fx <- store_fixture()
      seam <- dynes_map_seam(set_algorithm_em(n_cores = n_cores))
      on.exit(seam$close(), add = TRUE)
      store <- open_fixture_store(seam, fx)
      on.exit(close_pool_store(store), add = TRUE, after = FALSE)

      stale <- build_sequence_pool(
        fx$decomposition,
        fx$sequences[1:2],
        theta_length = 2L,
        store = store
      )
      current <- append_sequence_pool(stale, fx$sequences[3])

      expect_error(
        evaluate_sequence_pool(stale, fx$theta, what = "loglik"),
        class = "goldfish_dynes_stale_pool"
      )
      expect_no_error(evaluate_sequence_pool(current, fx$theta))
    }
  )
}

test_that("a pool subset evaluates its own sequences from the store", {
  fx <- store_fixture()
  pool <- build_sequence_pool(
    fx$decomposition,
    fx$sequences,
    theta_length = 2L
  )
  whole <- evaluate_sequence_pool(pool, fx$theta, what = "loglik")$loglik

  part <- pool_subset(pool, c(3L, 1L))

  expect_identical(part$ids, pool$ids[c(3L, 1L)])
  expect_identical(
    evaluate_sequence_pool(part, fx$theta, what = "loglik")$loglik,
    whole[c(3L, 1L)]
  )
})

# --- Broadcast robustness -----------------------------------------------------

test_that("a broadcast waits for every daemon and aborts on any failure", {
  seen <- new.env()
  local_mocked_bindings(
    everywhere = function(
      .expr,
      ...,
      .args = list(),
      .min = 1L,
      .compute = NULL
    ) {
      seen$min <- .min
      seen$compute <- .compute
      "map"
    },
    collect_mirai = function(x, ...) {
      force(x)
      list(list(generation = 1L), structure(5L, class = "errorValue"))
    },
    .package = "mirai"
  )

  expect_error(
    dynes_broadcast(function() NULL, list(), "profile", n_workers = 2L),
    class = "goldfish_dynes_broadcast_failed"
  )
  expect_identical(seen$min, 2L)
  expect_identical(seen$compute, "profile")
})

test_that("a broadcast answered by fewer daemons than started aborts", {
  local_mocked_bindings(
    everywhere = function(...) "map",
    collect_mirai = function(x, ...) {
      force(x)
      list(list(generation = 1L))
    },
    .package = "mirai"
  )

  expect_error(
    dynes_broadcast(function() NULL, list(), "profile", n_workers = 2L),
    class = "goldfish_dynes_broadcast_failed"
  )
})

# --- End to end ---------------------------------------------------------------

test_that("an estimation run keeps its evaluation payload off the pool", {
  skip_on_cran()
  local_dynes_augmenter(new_wave_grid_augmenter())
  spec <- dynes_varying_raw_spec()
  run <- function(n_cores) {
    suppressWarnings(estimate_dynes(
      spec,
      set_algorithm_em(
        n_sequences = 4L,
        max_iterations = 2L,
        seed = 42L,
        n_cores = n_cores,
        augmenter = set_augmenter_options(routine = "random")
      )
    ))
  }

  serial <- run(1L)
  record <- local_recording_parallel_backend()
  parallel <- run(2L)

  expect_identical(parallel, serial)
  # One header, then only mutations: never a second header.
  opened <- vapply(
    record$broadcasts,
    function(b) identical(b$f, open_resident_pool_store),
    logical(1)
  )
  expect_identical(which(opened), 1L)
  sizes <- vapply(
    evaluation_tasks(record),
    function(t) payload_bytes(t$f),
    integer(1)
  )
  expect_gt(length(sizes), 1L)
  expect_lt(max(sizes), 20000L)
  # The fit carries no handle to the closed store.
  expect_null(parallel$pool$store)
})

# --- Real daemons -------------------------------------------------------------

test_that("real daemons hold the pool through append and replace", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  fx <- store_fixture()
  seam <- dynes_map_seam(set_algorithm_em(n_cores = 2L))
  on.exit(seam$close(), add = TRUE)
  store <- open_fixture_store(seam, fx)
  on.exit(close_pool_store(store), add = TRUE, after = FALSE)

  pool <- build_sequence_pool(
    fx$decomposition,
    fx$sequences[1:2],
    theta_length = 2L,
    store = store
  )
  stale <- pool
  pool <- append_sequence_pool(pool, fx$sequences[3])
  expect_store_in_sync(seam, store, pool)
  expect_length(resident_states(seam, store), 2L)

  serial <- build_sequence_pool(
    fx$decomposition,
    fx$sequences,
    theta_length = 2L
  )
  expect_identical(
    evaluate_sequence_pool(pool, fx$theta),
    evaluate_sequence_pool(serial, fx$theta)
  )
  expect_error(
    evaluate_sequence_pool(stale, fx$theta, what = "loglik"),
    "stale"
  )

  pool <- build_sequence_pool(
    fx$decomposition,
    fx$sequences[3],
    theta_length = 2L,
    store = store
  )
  expect_store_in_sync(seam, store, pool)
})

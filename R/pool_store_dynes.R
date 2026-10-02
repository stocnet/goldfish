# The resident pool store behind the map seam of estimate_dynes().
#
# A pooled sequence's data and statistics are the bulk of every evaluation,
# and one full-pool evaluation runs per SGD batch. Shipping them with each
# task would make per-iteration traffic grow with the pool (quadratically, when
# a task closes over the pool), so they are sent to wherever evaluation runs
# once, when the sequence enters the pool, and each evaluation task then
# carries only sequence ids, theta, and the pool generation.
#
# The store takes exactly the two mutations the ABEM loop makes: append a
# chunk (growth, a post-acceptance top-up) or replace the whole pool (the first
# pool, a refresh or an ESS-guard redraw). A replace clears the old entries, so
# memory stays at (1 + n_workers) x pool: the coordinator's own copy, which the
# weights, the reference records and the final covariance pass read, plus one
# resident copy per worker. On daemons every mutation is broadcast to all of
# them; on the serial path the resident copy is a local environment in this
# process, reached through the same functions, so both paths run one code
# path.
#
# Each mutation advances a pool generation. The coordinator's pool records the
# generation it was built at, every evaluation task carries it, and the
# resident store refuses a task whose generation is not its own: a pool object
# captured before an append or a replace can then never be evaluated against
# sequences it does not describe.

# The resident stores of this process, keyed by store key. On a daemon this is
# the daemon's own copy of the namespace, so the same functions reach it.
dynes_pool_stores <- new.env(parent = emptyenv())
dynes_pool_store_counter <- new.env(parent = emptyenv())

new_pool_store_key <- function() {
  n <- (dynes_pool_store_counter$n %||% 0L) + 1L
  dynes_pool_store_counter$n <- n
  paste0("pool_", Sys.getpid(), "_", n)
}

# The static part of every evaluation: the sub-models with their coefficient
# masks, the theta length, and the preprocessing control, plus the pool engine
# spec pool entry walks and the layout tag a precomputed attachment must carry
# to stand in for that walk. The coefficient layout depends on the formulas, not
# on the drawn events, so any data object of the specification resolves it.
pool_store_header <- function(
  decomposition,
  theta_length,
  control_prep,
  data,
  call = rlang::caller_env()
) {
  list(
    sub_models = with_coefficient_masks(
      decomposition$sub_models,
      data,
      call = call
    ),
    engine_spec = decomposition$engine_spec,
    theta_length = as.integer(theta_length),
    control_prep = control_prep,
    layout = pool_layout_tag(decomposition$sub_models, control_prep)
  )
}

# Open a store on `seam` and send it the header, once. The evaluation header
# leaves out the engine spec: only pool entry walks it, and entry runs through
# the map with the spec in its task.
open_pool_store <- function(seam, header) {
  store <- new.env(parent = emptyenv())
  store$key <- new_pool_store_key()
  store$seam <- seam
  store$header <- header
  store$generation <- 0L
  store$next_id <- 1L
  # The run-long count of drawn sequences and fixed-term losses
  # (tally_pool_entries()), and whether the loop stopped because no sequence
  # of a pool carried weight; a stop also records `pool_lost`, what that
  # pool's sequences were lost to (pool_lost_causes()).
  store$tally <- list(
    built = 0L,
    fixed_term = 0L,
    labels = character(0),
    pool_dropped = FALSE
  )
  # The coordinator's per-fid start state, which every entry refers to.
  store$start_state <- new.env(parent = emptyenv())
  class(store) <- "goldfishDynesPoolStore"
  # A serial store's resident copy lives in this process; drop it with the
  # handle, so pools built outside an estimation run do not accumulate.
  reg.finalizer(store, function(s) drop_resident_pool_store(s$key))
  pool_store_broadcast(
    store,
    open_resident_pool_store,
    list(
      key = store$key,
      header = header[c("sub_models", "theta_length", "control_prep")]
    )
  )
  store
}

# Release the resident copies. Called on the way out of an estimation run,
# possibly while an error unwinds, so a failure here is not raised over it.
close_pool_store <- function(store) {
  tryCatch(
    store$seam$broadcast(
      drop_resident_pool_store,
      list(key = store$key)
    ),
    error = function(e) NULL
  )
  invisible(NULL)
}

# Apply one mutation: `entries` become the new ids, appended to the store or
# replacing its contents, and the generation advances. Only what evaluation
# reads is sent; the reference records stay with the coordinator's copy.
pool_store_mutate <- function(store, entries, replace) {
  ids <- store$next_id + seq_along(entries) - 1L
  store$next_id <- store$next_id + length(entries)
  store$generation <- store$generation + 1L
  resident <- lapply(entries, function(e) {
    e[c("data", "stats", "fixed_loglik", "fixed_degenerate")]
  })
  pool_store_broadcast(
    store,
    if (replace) replace_resident_pool_store else append_resident_pool_store,
    list(
      key = store$key,
      ids = ids,
      entries = resident,
      generation = store$generation
    )
  )
  ids
}

# Broadcast through the seam and check that every resident copy reports the
# coordinator's generation, so a daemon that missed a mutation is caught here
# rather than by the first task it refuses.
pool_store_broadcast <- function(store, f, args) {
  states <- store$seam$broadcast(f, args)
  in_sync <- vapply(
    states,
    function(s) identical(s$generation, store$generation),
    logical(1)
  )
  if (!all(in_sync)) {
    cli::cli_abort(
      c(
        "The pool store is out of sync on {sum(!in_sync)} worker{?s}.",
        "i" = "Expected generation {.val {store$generation}}."
      ),
      class = "goldfish_dynes_broadcast_failed",
      .internal = TRUE
    )
  }
  invisible(states)
}

# Resident side ----
# These run where the store lives: in this process on the serial path, on
# every daemon otherwise. Each returns pool_store_state() for the check above.
# A broadcast delivers every entry as its own serialized copy, so each
# resident copy points the entries it receives at its own per-fid start state
# (share_start_state()); on the serial path they already arrive shared.

open_resident_pool_store <- function(key, header) {
  resident <- new.env(parent = emptyenv())
  resident$header <- header
  resident$entries <- list()
  resident$ids <- integer(0)
  resident$generation <- 0L
  resident$start_state <- new.env(parent = emptyenv())
  assign(key, resident, envir = dynes_pool_stores)
  pool_store_state(key)
}

append_resident_pool_store <- function(key, ids, entries, generation) {
  resident <- resident_pool_store(key)
  entries <- share_start_state(entries, resident$start_state)
  names(entries) <- as.character(ids)
  resident$entries <- c(resident$entries, entries)
  resident$ids <- c(resident$ids, ids)
  resident$generation <- generation
  pool_store_state(key)
}

replace_resident_pool_store <- function(key, ids, entries, generation) {
  resident <- resident_pool_store(key)
  entries <- share_start_state(entries, resident$start_state)
  names(entries) <- as.character(ids)
  resident$entries <- entries
  resident$ids <- ids
  resident$generation <- generation
  pool_store_state(key)
}

drop_resident_pool_store <- function(key) {
  if (exists(key, envir = dynes_pool_stores, inherits = FALSE)) {
    rm(list = key, envir = dynes_pool_stores)
  }
  NULL
}

pool_store_state <- function(key) {
  resident <- resident_pool_store(key)
  list(
    ids = resident$ids,
    generation = resident$generation,
    n_entries = length(resident$entries)
  )
}

resident_pool_store <- function(key) {
  resident <- get0(key, envir = dynes_pool_stores, inherits = FALSE)
  if (is.null(resident)) {
    cli::cli_abort(
      "The pool store {.val {key}} is not open here.",
      class = "goldfish_dynes_store_closed"
    )
  }
  resident
}

# The per-sequence evaluation task: ids in, results out, the sequences read
# from the resident store. Built in its own frame so a worker receives the
# store key, the generation, and theta, and nothing that grows with the pool.
# The arguments are rebound as plain values: a forced promise can still
# serialize with the environment it was created in, which here is a caller
# holding the whole pool.
store_evaluation_task <- function(key, generation, theta) {
  key <- key
  generation <- generation
  theta <- theta
  function(id) {
    resident <- resident_pool_store(key)
    if (!identical(resident$generation, generation)) {
      cli::cli_abort(
        c(
          "A stale pool was evaluated.",
          "x" = "The pool is at generation {.val {generation}}; the store
                 holds generation {.val {resident$generation}}.",
          "i" = "Evaluate the pool returned by the latest append or replace."
        ),
        class = "goldfish_dynes_stale_pool"
      )
    }
    evaluate_pool_sequence(
      resident$header,
      resident$entries[[as.character(id)]],
      theta
    )
  }
}

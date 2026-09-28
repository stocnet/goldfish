# The per-sequence map seam and the RNG keying behind estimate_dynes().
#
# Every per-sequence loop -- augmentation draws, pool preprocessing on entry,
# pool evaluation -- runs through one map function: lapply() by default, or
# mirai daemons when more than one core is explicitly requested and mirai is
# installed. Reproducibility across worker counts rests on two rules:
#
# * each augmentation draw runs on its own L'Ecuyer-CMRG substream, keyed by
#   (iteration, draw_index) and injected into the task, so which worker runs a
#   draw never enters its random numbers;
# * results are reduced in sequence-index order, never arrival order, so the
#   sums downstream do not re-round when the worker count changes.
#
# The single `seed` seeds one L'Ecuyer-CMRG root with three stream families:
# the root stream itself is the coordinator's control stream (SGD batch
# selection, resampling); the next stream is the MCMC chain, advanced serially
# on the coordinator and never sent to a worker; the streams after it hold one
# stream per EM iteration, whose substreams are the iteration's draws.
# Iteration 0 is the warm-start draw, so it can never collide with the first
# pool. L'Ecuyer streams are 2^127 draws apart and substreams 2^76, so no two
# keys overlap in any run of practical length.

# Seed the L'Ecuyer-CMRG root from `seed` and return a function restoring the
# caller's RNG kind and state, so estimation leaves the caller's RNG as it
# found it.
set_dynes_rng <- function(seed) {
  kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  function() {
    RNGkind(kind[[1]], kind[[2]], kind[[3]])
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv()) # nolint
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }
}

get_rng_state <- function() get(".Random.seed", envir = globalenv())

set_rng_state <- function(state) {
  assign(".Random.seed", state, envir = globalenv()) # nolint
}

# The substream keyer over a root `.Random.seed`. Draws within an iteration are
# requested in increasing draw_index order, so the keyer walks the substreams
# forward from a cursor; any other request recomputes from the iteration's
# stream, giving the same state either way.
new_draw_keyer <- function(root) {
  keyer <- new.env(parent = emptyenv())
  keyer$chain <- parallel::nextRNGStream(root)
  keyer$augmentation <- parallel::nextRNGStream(keyer$chain)
  keyer$iteration <- NA_integer_
  keyer$cursor <- 0L
  keyer$state <- NULL
  keyer
}

draw_seed <- function(keyer, iteration, draw_index) {
  iteration <- as.integer(iteration)
  draw_index <- as.integer(draw_index)
  if (!identical(keyer$iteration, iteration) || draw_index < keyer$cursor) {
    stream <- keyer$augmentation
    for (i in seq_len(iteration)) {
      stream <- parallel::nextRNGStream(stream)
    }
    keyer$iteration <- iteration
    keyer$cursor <- 1L
    keyer$state <- stream
  }
  while (keyer$cursor < draw_index) {
    keyer$state <- parallel::nextRNGSubStream(keyer$state)
    keyer$cursor <- keyer$cursor + 1L
  }
  keyer$state
}

# Evaluate `f()` on the MCMC chain stream, keeping the chain's advanced state
# and handing the coordinator back its control stream.
with_chain_rng <- function(keyer, f) {
  control <- get_rng_state()
  on.exit(set_rng_state(control))
  set_rng_state(keyer$chain)
  value <- f()
  keyer$chain <- get_rng_state()
  value
}

# Wrap an augmenter so every draw runs on its keyed substream. Independent
# draws (`random`, `sim`) are mapped one task per draw_index through the seam;
# an MCMC chain is inherently serial and stays on the coordinator's chain
# stream, as does the augmenter's init().
keyed_augmenter <- function(augmenter, keyer, map_fn, routine) {
  inner_draw <- augmenter$draw
  inner_init <- augmenter$init
  wrapped <- augmenter
  wrapped$draw <- function(iteration, draw_index, theta) {
    if (identical(routine, "mcmc")) {
      return(with_chain_rng(keyer, function() {
        inner_draw(iteration, draw_index, theta)
      }))
    }
    tasks <- lapply(draw_index, function(k) {
      list(draw_index = k, seed = draw_seed(keyer, iteration, k))
    })
    control <- get_rng_state()
    on.exit(set_rng_state(control))
    map_fn(tasks, keyed_draw_task(inner_draw, iteration, theta))
  }
  if (is.function(inner_init)) {
    wrapped$init <- function(proposal_evaluator, theta) {
      with_chain_rng(keyer, function() inner_init(proposal_evaluator, theta))
    }
  }
  wrapped
}

# One draw task. Built in its own small frame so a worker receives the draw
# function, the iteration, and theta, and nothing else from the caller.
keyed_draw_task <- function(draw, iteration, theta) {
  function(task) {
    set_rng_state(task$seed)
    draw(iteration, task$draw_index, theta)[[1L]]
  }
}

# The seam for one estimation run: `map(x, f)` with the lapply() contract, and
# `close()` to release any daemons. Serial is the default (n_cores = 1L);
# only n_cores > 1 asks for parallelism, and without mirai that request warns
# once and runs serially, which gives the same result. Workers times BLAS
# threads never exceeds `n_cores`, so the two levels of parallelism do not
# oversubscribe.
dynes_map_seam <- function(control, call = rlang::caller_env()) {
  serial <- list(
    map = lapply,
    n_workers = 1L,
    close = function() invisible(NULL)
  )
  if (control$n_cores <= 1L) {
    return(serial)
  }
  if (!dynes_mirai_available()) {
    cli::cli_warn(
      c(
        "!" = "{.arg n_cores} = {.val {control$n_cores}} was requested, but
               {.pkg mirai} is not installed; running serially.",
        "i" = "Install {.pkg mirai} to run the per-sequence work in parallel.
               The result is the same either way."
      ),
      class = "goldfish_dynes_serial_fallback",
      call = call
    )
    return(serial)
  }
  n_workers <- control$n_cores
  blas_threads <- max(1L, control$n_cores %/% n_workers)
  compute <- dynes_daemons_start(n_workers, blas_threads)
  list(
    map = function(x, f) {
      reduce_in_index_order(dynes_parallel_map(x, f, compute))
    },
    n_workers = n_workers,
    close = function() dynes_daemons_stop(compute)
  )
}

# Per-element results tagged `list(index, value)`, in whatever order they
# arrived, back in sequence-index order.
reduce_in_index_order <- function(results) {
  index <- vapply(results, function(r) r$index, integer(1))
  lapply(results[order(index)], function(r) r$value)
}

dynes_mirai_available <- function() {
  requireNamespace("mirai", quietly = TRUE)
}

# Launch `n_workers` daemons under their own compute profile, each limited to
# `blas_threads` BLAS threads through the environment it inherits, with
# goldfish loaded. Returns the profile name.
#
# The daemons load the goldfish the coordinator runs: from a load_all() tree
# they load that same tree, since the installed version may be stale or lack
# the internals a task calls. The path is the loaded namespace's, not the
# working directory, so a run from tests/testthat or a second checkout loads
# the same code on both sides. compile = FALSE reuses the coordinator's built
# library, so concurrent daemons never race a compile of the shared src/.
dynes_daemons_start <- function(n_workers, blas_threads) {
  compute <- paste0("goldfish_dynes_", Sys.getpid())
  thread_vars <- c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS"
  )
  old <- Sys.getenv(thread_vars, unset = NA_character_)
  on.exit({
    unset <- is.na(old)
    Sys.unsetenv(thread_vars[unset])
    if (any(!unset)) {
      do.call(Sys.setenv, as.list(old[!unset]))
    }
  })
  do.call(
    Sys.setenv,
    stats::setNames(as.list(rep(as.character(blas_threads), 4L)), thread_vars)
  )
  # daemons() draws the daemons' own streams from the host RNG; the seam opens
  # after the control stream is seeded, so that draw would shift the SGD batch
  # selection and break identity with the serial run.
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  rng <- if (had_seed) get_rng_state()
  mirai::daemons(n_workers, .compute = compute)
  if (had_seed) {
    set_rng_state(rng)
  }
  if (dynes_dev_package()) {
    mirai::everywhere(
      pkgload::load_all(path, compile = FALSE, helpers = FALSE, quiet = TRUE),
      .args = list(path = getNamespaceInfo("goldfish", "path")),
      .compute = compute
    )
  } else {
    mirai::everywhere(loadNamespace("goldfish"), .compute = compute)
  }
  compute
}

dynes_dev_package <- function() {
  requireNamespace("pkgload", quietly = TRUE) &&
    pkgload::is_dev_package("goldfish")
}

dynes_daemons_stop <- function(compute) {
  mirai::daemons(0, .compute = compute)
  invisible(NULL)
}

# mirai_map() returns its results in input order; they are tagged with their
# index anyway, so the reduction never depends on that.
dynes_parallel_map <- function(x, f, compute) {
  values <- mirai::mirai_map(x, f, .compute = compute)[mirai::.stop]
  lapply(seq_along(values), function(i) list(index = i, value = values[[i]]))
}

# What a sim draw evaluates and opens, counted on the walk-alignment
# reference fixtures (helper-walk_alignment.R). Counts, not timings: they are
# deterministic for a seeded draw and separate on a small fixture.
#
# * The observed events and earlier flips before an interval are applied, not
#   drawn, so they need no rate: an attempt evaluates at most the driver's one
#   rate round at the walk's opening state before its interval starts.
# * At one walk state the augmenter reads each rate fid once, and each choice
#   fid's row once per sender.
# * Without a user support constraint no step can strand, so one walk carries
#   the whole draw.

# Count walk opens, attempts and every evaluate_fid() call of the draws made
# in `env`, each call with the attempt and the interval it ran in, the walk
# time, a version that changes whenever the walk moves or takes an event, and
# whether the driver's rate round made it. The interval is read from the
# attempt's state when the call is made, since one walk may carry several.
local_sim_counts <- function(env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$opens <- 0L
  log$attempt <- 0L
  log$state <- NULL
  log$version <- 0L
  log$in_round <- FALSE
  log$rows <- list()
  open <- walk_open
  attempt <- new_sim_attempt
  advance <- walk_advance
  inject <- walk_inject
  round <- rate_values
  evaluate <- evaluate_fid
  local_mocked_bindings(
    walk_open = function(...) {
      log$opens <- log$opens + 1L
      open(...)
    },
    new_sim_attempt = function(...) {
      log$attempt <- log$attempt + 1L
      log$state <- attempt(...)
      log$state
    },
    walk_advance = function(handle, ...) {
      log$version <- log$version + 1L
      advance(handle, ...)
    },
    walk_inject = function(handle, ...) {
      log$version <- log$version + 1L
      inject(handle, ...)
    },
    rate_values = function(...) {
      log$in_round <- TRUE
      on.exit(log$in_round <- FALSE)
      round(...)
    },
    evaluate_fid = function(handle, fid, theta, evaluate_step, sender = NULL) {
      map <- handle$process_map
      log$rows[[length(log$rows) + 1L]] <- data.frame(
        attempt = log$attempt,
        k = log$state$k %||% NA_integer_,
        fid = fid,
        family = map$family[match(fid, map$fid)],
        time = handle$current_time,
        version = log$version,
        sender = sender %||% NA_integer_,
        driver = log$in_round
      )
      evaluate(handle, fid, theta, evaluate_step, sender = sender)
    },
    .env = env
  )
  log
}

sim_evaluations <- function(log) {
  do.call(rbind, log$rows)
}

# The evaluations each attempt made before its interval opened, per fid.
pre_interval_counts <- function(evaluations, waves) {
  before <- evaluations[evaluations$time < waves[evaluations$k], ]
  stats::aggregate(
    list(n = rep(1L, nrow(before))),
    by = before[c("attempt", "k", "fid", "family")],
    FUN = sum
  )
}

for (name in c("uncoupled", "coupled")) {
  test_that(
    paste0(
      "a sim attempt evaluates at most one rate round before its interval",
      " (",
      name,
      ")"
    ),
    {
      skip_on_cran()
      fixture <- wa_sim_fixture(name)
      log <- local_sim_counts()
      wa_seeded_draw(fixture, 1L)
      counts <- pre_interval_counts(sim_evaluations(log), fixture$waves)
      # Intervals after the first have a prefix to apply.
      expect_true(any(counts$k > 1L))
      expect_false(any(counts$family == "choice"))
      expect_lte(max(counts$n), 1L)
    }
  )

  test_that(
    paste0(
      "a sim step reads each rate fid once and each choice row once per",
      " sender (",
      name,
      ")"
    ),
    {
      skip_on_cran()
      fixture <- wa_sim_fixture(name)
      log <- local_sim_counts()
      wa_seeded_draw(fixture, 1L)
      reads <- sim_evaluations(log)
      reads <- reads[!reads$driver, ]
      expect_gt(nrow(reads), 0L)
      key <- paste(reads$attempt, reads$version, reads$fid, reads$sender)
      expect_false(anyDuplicated(key) > 0L)
    }
  )
}

# --- The forced prefix --------------------------------------------------------

test_that("a later interval's prefix costs one round whatever its length", {
  skip_on_cran()
  # Coupled, so every interval has attempts of its own, each over a prefix.
  fixture <- wa_sim_fixture("coupled")
  log <- local_sim_counts()
  wa_seeded_draw(fixture, 1L)
  counts <- pre_interval_counts(sim_evaluations(log), fixture$waves)
  rate_fids <- with(fixture$spec$process_map, fid[family == "rate"])
  # Interval 3 replays more events than interval 2 (every observed call and
  # flip before it), yet each attempt at either pays one evaluation of each
  # rate fid, the driver's round at the walk's opening state.
  calls <- as.data.frame(fixture$spec$data$ties)
  calls <- calls$time[calls$layer == "calls" & !is.na(calls$time)]
  expect_gt(sum(calls < fixture$waves[[3L]]), sum(calls < fixture$waves[[2L]]))
  for (k in 2:3) {
    at_k <- counts[counts$k == k, ]
    expect_setequal(at_k$fid, rate_fids)
    expect_true(all(at_k$n == 1L))
  }
})

test_that("seeded sim draws equal the pre-change references", {
  skip_on_cran()
  # The prefix draws no random number however it is applied, so on data with
  # no stamp tie the draws are the ones recorded before the change. The
  # coupled draws restart, and the restart tail of seeds 2 to 5 is slow.
  for (name in c("uncoupled", "coupled")) {
    fixture <- wa_sim_fixture(name)
    seeds <- if (name == "uncoupled") 1:5 else 1L
    for (seed in seeds) {
      expect_identical(
        wa_draw_record(wa_seeded_draw(fixture, seed)),
        fixture$draws[[as.character(seed)]],
        label = paste(name, "seed", seed)
      )
    }
  }
})

# --- One rate vector per fid per step -----------------------------------------

test_that("a step evaluates a rate fid once over several of its pairs", {
  skip_on_cran()
  fixture <- wa_sim_fixture("uncoupled")
  log <- local_sim_counts()
  flip_options <- sim_flip_options
  steps <- list()
  local_mocked_bindings(
    sim_flip_options = function(state, t, handle) {
      before <- length(log$rows)
      options <- flip_options(state, t, handle)
      reads <- do.call(rbind, log$rows[-seq_len(before)])
      reads <- reads[reads$family == "rate", , drop = FALSE]
      steps[[length(steps) + 1L]] <<- list(
        pairs = table(options$rate_fid),
        reads = table(reads$fid)
      )
      options
    }
  )
  wa_seeded_draw(fixture, 1L)
  crowded <- Filter(function(step) any(step$pairs > 1L), steps)
  expect_gt(length(crowded), 0L)
  most <- vapply(crowded, function(step) max(c(0L, step$reads)), integer(1))
  expect_lte(max(most), 1L)
})

# --- Column reads -------------------------------------------------------------

# Count the data-frame row extractions and unique() calls the augmenter's own
# step code makes, through a trace on the base methods. A call is the step's
# when the nearest named goldfish frame above it is a `sim_*` function, so
# the walk's and the evaluator's own data frames are not counted.
local_sim_frame_reads <- function(env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$reads <- character()
  namespace <- asNamespace("goldfish")
  record <- function(method) {
    frames <- sys.calls()
    for (i in rev(seq_along(frames))) {
      head <- frames[[i]][[1L]]
      if (!is.name(head) || identical(as.character(head), "FUN")) {
        next
      }
      fn <- sys.function(i)
      if (!identical(topenv(environment(fn) %||% baseenv()), namespace)) {
        next
      }
      if (startsWith(as.character(head), "sim_")) {
        log$reads <- c(log$reads, paste(method, as.character(head)))
      }
      return(invisible())
    }
  }
  for (method in c("[.data.frame", "unique.data.frame")) {
    suppressMessages(trace(
      method,
      tracer = bquote(.(record)(.(method))),
      print = FALSE,
      where = baseenv()
    ))
  }
  withr::defer(
    {
      suppressMessages(untrace("[.data.frame", where = baseenv()))
      suppressMessages(untrace("unique.data.frame", where = baseenv()))
    },
    envir = env
  )
  log
}

test_that("a sim step reads no data-frame row and makes no unique() over one", {
  skip_on_cran()
  fixture <- wa_sim_fixture("uncoupled")
  steps <- 0L
  clock <- sim_clock
  local_mocked_bindings(
    sim_clock = function(...) {
      steps <<- steps + 1L
      clock(...)
    }
  )
  log <- local_sim_frame_reads()
  wa_seeded_draw(fixture, 1L)
  expect_gt(steps, 0L)
  # The sites, so a failure names where the step still reads a row.
  expect_identical(sort(unique(log$reads)), character())
})

# --- One walk -----------------------------------------------------------------

test_that("an uncoupled sim draw opens one walk", {
  skip_on_cran()
  fixture <- wa_sim_fixture("uncoupled")
  # Every interval carries flips, so a per-interval draw opens three walks.
  expect_true(all(tabulate(fixture$recipe$events$interval, 3L) > 0L))
  log <- local_sim_counts()
  draw <- wa_seeded_draw(fixture, 1L)
  expect_identical(log$opens, 1L)
  # Carried on one walk, the draw is still the one the intervals drew apart.
  expect_identical(wa_draw_record(draw), fixture$draws[["1"]])
})

test_that("a coupled sim draw opens one walk per interval attempt", {
  skip_on_cran()
  fixture <- wa_sim_fixture("coupled")
  log <- local_sim_counts()
  draw <- wa_seeded_draw(fixture, 1L)
  # A stranded interval is redrawn from its starting state, which the walk,
  # moving only forward, cannot return to: every attempt opens its own.
  expect_true(draw$restarted)
  expect_gt(log$attempt, 3L)
  expect_identical(log$opens, log$attempt)
})

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
# in `env`, each call with the attempt and interval it ran in, the walk time,
# a version that changes whenever the walk moves or takes an event, and
# whether the driver's rate round made it.
local_sim_counts <- function(env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$opens <- 0L
  log$attempt <- 0L
  log$k <- NA_integer_
  log$version <- 0L
  log$in_round <- FALSE
  log$rows <- list()
  open <- walk_open
  attempt <- sim_interval_attempt
  advance <- walk_advance
  inject <- walk_inject
  round <- rate_values
  evaluate <- evaluate_fid
  local_mocked_bindings(
    walk_open = function(...) {
      log$opens <- log$opens + 1L
      open(...)
    },
    sim_interval_attempt = function(plan, k, ...) {
      log$attempt <- log$attempt + 1L
      log$k <- k
      attempt(plan, k, ...)
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
        k = log$k,
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

test_that("an uncoupled sim draw opens one walk", {
  skip_on_cran()
  fixture <- wa_sim_fixture("uncoupled")
  # Every interval carries flips, so a per-interval draw opens three walks.
  expect_true(all(tabulate(fixture$recipe$events$interval, 3L) > 0L))
  log <- local_sim_counts()
  wa_seeded_draw(fixture, 1L)
  expect_identical(log$opens, 1L)
})

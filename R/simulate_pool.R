# =========================================================================== #
# The replicate pool: what simulate() returns for nsim > 1.
#
# A classed list of `goldfishSim` replicates, so it indexes and iterates as a
# list while printing and filtering as one object. Everything a replicate
# shares with the others comes from the completed specification and is said
# once; what differs across replicates -- their length and why they stopped --
# is read from a per-replicate summary built from each replicate's own fields,
# never stored beside them, so a subset can never disagree with its summary.
# =========================================================================== #

new_goldfish_sim_pool <- function(replicates) {
  structure(replicates, class = c("goldfishSimPool", "list"))
}

#' @export
`[.goldfishSimPool` <- function(x, i) {
  new_goldfish_sim_pool(unclass(x)[i])
}

#' Filter a pool of simulated replicates
#'
#' Keeps the replicates of a [simulate()] pool whose per-replicate summary
#' meets every condition. Nothing is excluded from a pool by default: a
#' replicate that stopped at a guard stays in it, and in anything computed
#' from it, until a filter removes it, so the criterion is written in the
#' call that applies it.
#'
#' @param pool a `goldfishSimPool`, from [simulate()] with `nsim > 1`.
#' @param ... conditions evaluated against `summary(pool)`, each giving one
#'   logical per replicate. Several conditions are combined with AND, and a
#'   replicate whose condition is `NA` is dropped. Variables that are not
#'   summary columns are read from the calling environment.
#'
#' @return A `goldfishSimPool` of the kept replicates, keeping their original
#'   replicate numbers. `pool` itself is unchanged.
#'
#'   `summary()` on a pool returns the data frame the conditions read, one row
#'   per replicate: `replicate`, `n_events`, `end_time` (the clock when the
#'   run stopped), `stop_reason`, `capped`, `n_proposals` and
#'   `acceptance_rate`.
#'
#' @family simulation
#' @export
#' @examples
#' \donttest{
#' # pool <- simulate(spec, nsim = 100, coef = coef)
#' # filter_simulation(pool, stop_reason == "horizon")
#' # filter_simulation(pool, !capped, n_events > 100)
#' }
filter_simulation <- function(pool, ...) {
  call <- rlang::current_env()
  if (!inherits(pool, "goldfishSimPool")) {
    cli::cli_abort(
      c(
        "{.arg pool} must be a {.cls goldfishSimPool}.",
        "x" = "A {.cls {class(pool)[1]}} was supplied.",
        "i" = "{.fn simulate} returns a pool when {.arg nsim} is above 1."
      ),
      call = call,
      class = "goldfish_sim_bad_filter"
    )
  }
  replicates <- summary(pool)
  keep <- rep(TRUE, nrow(replicates))
  for (condition in rlang::enquos(...)) {
    keep <- keep & evaluate_filter_condition(condition, replicates, call)
  }
  new_goldfish_sim_pool(unclass(pool)[keep])
}

# One condition over the summary. A name that is neither a column nor visible
# from where the condition was written is a column the summary does not have,
# and is reported as one rather than as R's "object not found".
evaluate_filter_condition <- function(condition, replicates, call) {
  expression <- rlang::quo_get_expr(condition)
  label <- rlang::expr_deparse(expression)
  env <- rlang::quo_get_env(condition)
  names_used <- all.vars(expression)
  unknown <- names_used[
    !names_used %in% names(replicates) &
      !vapply(names_used, exists, logical(1), envir = env)
  ]
  if (length(unknown) > 0L) {
    columns <- names(replicates)
    cli::cli_abort(
      c(
        "Cannot filter on {.field {unknown}}: not a column of the pool's
         summary.",
        "i" = "The summary has {.field {columns}}."
      ),
      call = call,
      class = "goldfish_sim_bad_filter"
    )
  }
  value <- rlang::eval_tidy(condition, data = replicates)
  if (!is.logical(value) || length(value) != nrow(replicates)) {
    n_replicates <- nrow(replicates)
    cli::cli_abort(
      c(
        "{.code {label}} must give one logical per replicate.",
        "x" = "It gave a {.cls {class(value)[1]}} of length {length(value)}
               for {n_replicates} replicate{?s}."
      ),
      call = call,
      class = "goldfish_sim_bad_filter"
    )
  }
  !is.na(value) & value
}

#' @param object a `goldfishSimPool`.
#' @rdname filter_simulation
#' @export
summary.goldfishSimPool <- function(object, ...) {
  diagnostics <- lapply(object, `[[`, "diagnostics")
  data.frame(
    replicate = vapply(object, function(run) as.integer(run$replicate), 1L),
    n_events = vapply(diagnostics, function(d) as.integer(d$n_events), 1L),
    end_time = vapply(diagnostics, function(d) as.numeric(d$end_time), 1),
    stop_reason = vapply(diagnostics, `[[`, "", "stop_reason"),
    capped = vapply(object, function(run) isTRUE(run$capped), TRUE),
    n_proposals = vapply(
      diagnostics,
      function(d) as.integer(d$n_proposals),
      1L
    ),
    acceptance_rate = vapply(
      diagnostics,
      function(d) as.numeric(d$acceptance_rate),
      1
    )
  )
}

#' @param x a `goldfishSimPool` from [simulate()].
#' @rdname print-method
#' @export
print.goldfishSimPool <- function(x, ...) {
  replicates <- summary(x)
  n_replicates <- nrow(replicates)
  cli::cli_rule(left = "{.cls goldfishSimPool}")
  if (n_replicates == 0L) {
    cli::cli_text("0 replicates")
    cli::cli_alert_info(
      "Select replicates by their summary with {.fn filter_simulation}."
    )
    return(invisible(x))
  }

  # The processes and the variant belong to the completed specification, so
  # the first replicate speaks for all of them.
  first <- x[[1L]]
  map <- first$process_map
  n_processes <- nrow(map)
  times <- first$times
  source <- if (identical(first$times_source, "specification")) {
    "from the specification"
  } else {
    "requested"
  }
  cli::cli_text(
    "{n_replicates} replicate{?s} · times {.val {times}} ({source}) ·
     {n_processes} process{?es}"
  )
  regime <- map$regime %||% rep("modeled", n_processes)
  labels <- render_process_label(map, map$fid)
  fids <- map$fid
  cli::cli_ul()
  for (i in seq_along(labels)) {
    cli::cli_li("{.strong {labels[i]}} [fid {fids[i]}] ({regime[i]})")
  }
  cli::cli_end()

  counts <- replicates$n_events
  low <- min(counts)
  middle <- stats::median(counts)
  average <- format(round(mean(counts), 1), nsmall = 1)
  high <- max(counts)
  cli::cli_text(
    "Events per replicate: min {low} · median {middle} · mean {average} ·
     max {high}"
  )
  reasons <- sort(table(replicates$stop_reason), decreasing = TRUE)
  by_reason <- paste(names(reasons), as.integer(reasons), collapse = " · ")
  cli::cli_text("Stop reasons: {by_reason}")
  n_guarded <- sum(replicates$stop_reason %in% SIM_GUARD_STOPS)
  if (n_guarded > 0L) {
    cli::cli_alert_warning("{n_guarded} replicate{?s} stopped at a guard.")
  }
  cli::cli_alert_info(
    "Select replicates by their summary with {.fn filter_simulation}."
  )
  invisible(x)
}

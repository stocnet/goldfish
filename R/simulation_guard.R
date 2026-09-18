# =========================================================================== #
# The simulation guard: when a free-running replicate gives up.
#
# Guards are not targets. A target (`n_events`, `horizon`) is what the user
# asks a run to produce; a guard is the condition under which a run stops
# short of it. Every guard stop ends only its replicate and flags it, so the
# object holds thresholds and nothing that could end a call.
# =========================================================================== #

#' Set the guards that stop a runaway simulation
#'
#' A free-running replicate stops at its target, `n_events` or `horizon`, or
#' earlier at a guard. The guards are set here and passed to [simulate()] as
#' `control_sim`. A guard stop ends only its own replicate: the events drawn
#' so far are kept, the replicate is flagged `capped`, and it stays in the
#' result.
#'
#' By default a replicate whose total rate grows without bound runs on to
#' `max_events`, so the explosion shows in the run itself. Two guards are on
#' by default. `max_events` caps the proposed events, and `clock_resolution`
#' stops a replicate once a drawn waiting time no longer moves the clock,
#' after which every event would share one timestamp. The two
#' rate-trajectory triggers, `rate_multiple` and `wait_collapse`, are off
#' until set, for a user who wants a runaway to stop earlier.
#'
#' The window length that `clock_resolution` is measured against runs from
#' the start to the end of the observation window: the `start_time` and
#' `end_time` of [set_preprocessing()] when set, otherwise the first and last
#' observed row on the schedule, not counting a windowed effect's expiries.
#' The observed mean waiting time is that length over the number of observed
#' dependent events.
#'
#' @param max_events the count guard, over proposed events: `NULL` for ten
#'   times the number of observed dependent events, or one positive whole
#'   number.
#' @param rate_multiple stop when the total rate exceeds this multiple of its
#'   value at the first drawn event. One number of at least 1; `Inf`, the
#'   default, never fires.
#' @param wait_collapse stop when the median of the last `wait_window` waiting
#'   times falls below the observed mean waiting time divided by this factor.
#'   One number of at least 1; `Inf`, the default, never fires.
#' @param wait_window the number of most recent waiting times whose median
#'   `wait_collapse` reads. One positive whole number.
#' @param clock_resolution stop when a step moves the clock by no more than
#'   this fraction of the window length. `0`, the default, stops only when a
#'   wait leaves the clock unchanged; `NULL` turns the stop off, so collapsed
#'   timestamps run on to `max_events`.
#'
#' @return An object of class `goldfishSimGuard`, a list with components:
#'   \item{max_events}{Value from the `max_events` argument, as an integer.}
#'   \item{rate_multiple}{Value from the `rate_multiple` argument.}
#'   \item{wait_collapse}{Value from the `wait_collapse` argument.}
#'   \item{wait_window}{Value from the `wait_window` argument, as an integer.}
#'   \item{clock_resolution}{Value from the `clock_resolution` argument.}
#'
#' @seealso [simulate()], [filter_simulation()].
#' @family simulation
#' @export
#' @examples
#' # Stop a replicate once its total rate is a thousand times its start.
#' set_simulation_guard(rate_multiple = 1e3)
set_simulation_guard <- function(
  max_events = NULL,
  rate_multiple = Inf,
  wait_collapse = Inf,
  wait_window = 50L,
  clock_resolution = 0
) {
  call <- rlang::current_env()
  if (!is.null(max_events)) {
    max_events <- guard_whole_number(max_events, "max_events", call)
  }
  rate_multiple <- guard_factor(rate_multiple, "rate_multiple", call)
  wait_collapse <- guard_factor(wait_collapse, "wait_collapse", call)
  wait_window <- guard_whole_number(wait_window, "wait_window", call)
  if (!is.null(clock_resolution) && !is_guard_number(clock_resolution, 0)) {
    abort_bad_guard(
      clock_resolution,
      "clock_resolution",
      "{.code NULL} or one finite number of at least 0",
      call
    )
  }

  structure(
    list(
      max_events = max_events,
      rate_multiple = rate_multiple,
      wait_collapse = wait_collapse,
      wait_window = wait_window,
      clock_resolution = clock_resolution
    ),
    class = "goldfishSimGuard"
  )
}

is_simulation_guard <- function(x) inherits(x, "goldfishSimGuard")

is_guard_number <- function(x, lower, finite = TRUE) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    (!finite || is.finite(x)) &&
    x >= lower
}

guard_whole_number <- function(x, arg, call) {
  if (!is_guard_number(x, 1) || x != trunc(x)) {
    abort_bad_guard(x, arg, "one positive whole number", call)
  }
  as.integer(x)
}

guard_factor <- function(x, arg, call) {
  if (!is_guard_number(x, 1, finite = FALSE)) {
    abort_bad_guard(x, arg, "one number of at least 1, or {.code Inf}", call)
  }
  x
}

abort_bad_guard <- function(x, arg, expected, call) {
  cli::cli_abort(
    c(
      paste0("{.arg {arg}} must be ", expected, "."),
      "x" = "You supplied {.val {x}}."
    ),
    call = call,
    class = "goldfish_sim_bad_guard"
  )
}

resolve_simulation_guard <- function(control_sim, call) {
  if (!is_simulation_guard(control_sim)) {
    cli::cli_abort(
      c(
        "{.arg control_sim} must be a {.cls goldfishSimGuard}.",
        "x" = "A {.cls {class(control_sim)[1]}} was supplied.",
        "i" = "Build one with {.fn set_simulation_guard}."
      ),
      call = call,
      class = "goldfish_sim_bad_guard"
    )
  }
  control_sim
}

#' @rdname print-method
#' @export
print.goldfishSimGuard <- function(x, ...) {
  on <- c(
    "max_events",
    if (is.finite(x$rate_multiple)) "rate_multiple",
    if (is.finite(x$wait_collapse)) "wait_collapse",
    if (!is.null(x$clock_resolution)) "clock_resolution"
  )
  cli::cli_rule(left = "{.cls goldfishSimGuard}")
  cli::cli_text("Stops on: {on}")
  max_events <- if (is.null(x$max_events)) {
    "10 x observed dependent events"
  } else {
    x$max_events
  }
  rate_multiple <- if (is.finite(x$rate_multiple)) {
    cli::format_inline("{x$rate_multiple} x total rate at the first event")
  } else {
    "off"
  }
  wait_collapse <- if (is.finite(x$wait_collapse)) {
    cli::format_inline("observed mean wait / {x$wait_collapse}")
  } else {
    "off"
  }
  clock_resolution <- if (is.null(x$clock_resolution)) {
    "off"
  } else {
    cli::format_inline("{x$clock_resolution} x window length")
  }
  cli::cli_dl(c(
    max_events = max_events,
    rate_multiple = rate_multiple,
    wait_collapse = wait_collapse,
    wait_window = x$wait_window,
    clock_resolution = clock_resolution
  ))
  invisible(x)
}

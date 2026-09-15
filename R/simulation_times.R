# =========================================================================== #
# The `times` a simulation runs in, read from what the model can honor.
#
# A free-running run draws waiting times, so it needs a clock the model
# estimated. A timed rate is one. An ordered rate estimates which event comes
# next and leaves the baseline hazard unestimated. A coordination process is
# estimated under the same partial likelihood, and the rate of the proposals
# behind its realized events is not identified from them. Neither has a clock
# to draw from, and none is invented for them.
#
# A choice-only DyNAM has no rate at all, and is the one case a clock can be
# supplied for: a constant exponential rate beside a choice model is a
# complete Poisson process, the construction of RSiena's default constant rate
# function. So it defaults to the observed stamps, and asking for generated
# times completes that rate rather than being refused.
# =========================================================================== #

#' The simulation variant a model supports by default
#'
#' `times_of()` reads which `times` variant [simulate()] uses when none is
#' given: `"generated"` (free-running, the waiting times drawn from the model)
#' when the model carries a timed rate, and `"observed"` (time-anchored, the
#' observed event times held) otherwise.
#'
#' The rule, by what the model's processes carry:
#' \describe{
#'   \item{a timed rate}{`"generated"`. The rate is a clock, so the waiting
#'     times are drawn from it.}
#'   \item{an ordered rate}{`"observed"`. An ordered rate models which event
#'     comes next, not when, so there is no clock to draw from, and asking for
#'     `"generated"` is an error.}
#'   \item{a coordination process}{`"observed"`, for the same reason: its
#'     timing is estimated through the order of events, and the rate of the
#'     proposals behind them is not identified. Asking for `"generated"` is an
#'     error.}
#'   \item{no rate}{`"observed"`. A choice-only DyNAM has no clock. Asking for
#'     `"generated"` completes a constant exponential rate pinned at the crude
#'     rate of the observed events, with a warning.}
#' }
#'
#' A process of a joint specification whose rate is missing while another
#' process carries a timed rate is completed with a pinned rate at
#' [simulate()]'s entry, so such a specification reads as `"generated"`. A
#' flavored fit reads across all of its processes the same way.
#'
#' @param object a specification ([make_specification()] or
#'   [make_joint_specification()]) or a fitted model.
#'
#' @return `"generated"` or `"observed"`, with a `reason` attribute naming the
#'   rule that decided it: `"timed rate"`, `"ordered rate"`, `"coordination"`
#'   or `"no rate"`.
#'
#' @seealso [simulate()]
#' @family simulation
#' @export
#' @examples
#' data("social_evolution")
#' # A choice-only specification has no clock, so it is time-anchored:
#' choice_only <- make_specification(
#'   choice = ~ inertia + recip,
#'   model = "DyNAM",
#'   layer = "calls",
#'   data = social_evolution
#' )
#' times_of(choice_only)
#'
#' # A timed rate is a clock, so the waiting times are generated:
#' timed <- make_specification(
#'   rate = ~ 1 + indeg,
#'   choice = ~ inertia + recip,
#'   model = "DyNAM",
#'   layer = "calls",
#'   data = social_evolution
#' )
#' times_of(timed)
times_of <- function(object) {
  UseMethod("times_of")
}

#' @rdname times_of
#' @export
times_of.goldfishJointSpec <- function(object) {
  # A specification is not a model spec yet: its bundles hold the sub-model
  # the user asked for, and there is no descriptor to read instead.
  reasons <- vapply(
    joint_fid_bundles(object),
    function(entry) times_reason_of_sub_model(entry$sub_model),
    character(1)
  )
  times_from_reasons(reasons)
}

#' @rdname times_of
#' @export
times_of.goldfishSpec <- function(object) {
  times_of(single_process_joint(object))
}

#' @rdname times_of
#' @export
times_of.goldfishFit <- function(object) {
  times_from_reasons(times_reason_of_model_spec(object$model_spec))
}

#' @rdname times_of
#' @export
times_of.goldfishFlavFit <- function(object) {
  reasons <- vapply(
    object$results,
    function(fit) times_reason_of_model_spec(fit$model_spec),
    character(1)
  )
  times_from_reasons(reasons)
}

#' @rdname times_of
#' @export
times_of.default <- function(object) {
  cli::cli_abort(
    c(
      "{.fn times_of} needs a specification or a fitted model.",
      "x" = "A {.cls {class(object)[1]}} was supplied."
    ),
    class = "goldfish_sim_bad_object"
  )
}

# What one fitted process says about the clock, read off its descriptor.
times_reason_of_model_spec <- function(model_spec) {
  if (identical(behavior_likelihood(model_spec), "coordination")) {
    return("coordination")
  }
  if (identical(behavior_timing(model_spec), "timed")) {
    return("timed rate")
  }
  if (is_choice_family(model_spec)) "no rate" else "ordered rate"
}

# What one specified sub-model says about the clock, before a descriptor
# exists.
times_reason_of_sub_model <- function(sub_model) {
  switch(
    sub_model,
    choice_coordination = "coordination",
    rate_ordered = "ordered rate",
    rate = "timed rate",
    "no rate"
  )
}

# The order of the checks is the rule: coordination and an ordered rate rule
# a clock out, so they decide before a timed rate does, and a model with no
# rate anywhere decides last.
times_from_reasons <- function(reasons) {
  precedence <- c("coordination", "ordered rate", "timed rate", "no rate")
  reason <- precedence[min(match(reasons, precedence))]
  value <- if (identical(reason, "timed rate")) "generated" else "observed"
  structure(value, reason = reason)
}

# Settle the `times` a run uses. Unrequested, it is the model's default.
# Requested, it is checked against what the model can honor, and what an
# override costs is said at entry: nothing when it equals the default, a
# message when a timed model is anchored, an abort when a clock is asked of a
# model that estimates none. A choice-only model asked for generated times is
# let through here, because completion installs its rate and warns there.
#
# Returns a list: `times`, `source` ("specification" or "requested"),
# `default` and `reason`.
resolve_simulation_times <- function(object, times, requested, call) {
  default <- times_of(object)
  reason <- attr(default, "reason")
  default <- as.character(default)
  resolved <- list(
    times = default,
    source = "specification",
    default = default,
    reason = reason
  )
  if (!requested) {
    return(resolved)
  }
  times <- rlang::arg_match(
    times,
    c("generated", "observed"),
    error_arg = "times",
    error_call = call
  )
  resolved$times <- times
  resolved$source <- "requested"
  if (identical(times, default)) {
    return(resolved)
  }
  if (identical(times, "observed")) {
    cli::cli_inform(
      c(
        "Simulating time-anchored, as requested.",
        "i" = "The specification's default is {.val generated}: it carries a
               timed rate. The observed event times are held and the marks
               redrawn at each."
      ),
      class = "goldfish_sim_times_override"
    )
    return(resolved)
  }
  switch(
    reason,
    "ordered rate" = cli::cli_abort(
      c(
        "Cannot generate event times for an ordered rate.",
        "x" = "An ordered rate models which event comes next, not when, so it
               estimates no clock to draw waiting times from.",
        "i" = "Simulate it time-anchored with {.code times = \"observed\"}."
      ),
      call = call,
      class = "goldfish_sim_no_clock"
    ),
    "coordination" = cli::cli_abort(
      c(
        "Cannot generate event times for a coordination process.",
        "x" = "Coordination timing is estimated under Cox, and the rate of the
               proposals behind its realized events is not identified from
               them.",
        "i" = "Simulate it time-anchored with {.code times = \"observed\"}."
      ),
      call = call,
      class = "goldfish_sim_no_clock"
    )
  )
  resolved
}

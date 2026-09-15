# =========================================================================== #
# simulate() — the entry points.
#
# Four methods, one driver. A specification carries its own data and takes the
# parameters explicitly; a fitted result supplies its estimates as the
# parameters but carries no data, so it takes the stocnet it was fitted on.
# The flavored container is the one generic that does NOT fan out over its
# components: its flavors are competing processes of one specification and
# simulating them independently would simulate a different model.
# =========================================================================== #

#' Simulate event sequences from a specification or a fitted model
#'
#' Generates event sequences from a goldfish model: the waiting time and the
#' marks are drawn from the model at the state the previous event left behind,
#' so the simulated sequence feeds back into its own statistics exactly as an
#' observed one does.
#'
#' Two variants, named by `times`. **Free-running** (`"generated"`, the
#' default) draws the clock and the marks from the model, holding nothing from
#' the observed stream but the initial state — this is what tests the clock.
#' **Time-anchored** (`"observed"`) holds the observed event times and redraws
#' only the marks.
#'
#' Observed exogenous streams — covariate changes and node composition — march
#' along with the simulated clock. A free-running run that passes the last
#' observed change holds that state and warns once rather than inventing its
#' continuation.
#'
#' A run stops at whichever target binds first, `n_events` or `horizon`. The
#' `max_events` guard is not a target: it is the explosion backstop, and a run
#' that reaches it is flagged `capped` so a pool can exclude it.
#'
#' Every step of the loop is replaceable — see [set_simulation_steps()] for the
#' plug points and [set_parameter_provider()] for parameters that change during
#' a run.
#'
#' @param object a specification ([make_specification()] or
#'   [make_joint_specification()]) or a fitted model.
#' @param nsim number of replicates to simulate.
#' @param seed passed to [set.seed()] before the first replicate, for a
#'   reproducible run.
#' @param coef the parameters to simulate at: a numeric vector for a
#'   single-process specification, a [set_parameters()] object for a joint one,
#'   or a [set_parameter_provider()]. A fitted model supplies its own estimates
#'   and needs none.
#' @param data the stocnet to simulate on. Required on a fitted model, which
#'   stores its formula and estimates but not the data it was fitted on.
#' @param times `"generated"` for a free-running run (default) or `"observed"`
#'   for a time-anchored one.
#' @param n_events stop after this many events.
#' @param horizon stop at this time.
#' @param max_events the explosion guard; defaults to ten times the number of
#'   observed dependent events. A run reaching it stops and is flagged.
#' @param steps a [set_simulation_steps()] object replacing any of the driver's
#'   own steps.
#' @param control_prep preprocessing options, from [set_preprocessing()].
#' @param ... passed between methods.
#'
#' @return For `nsim = 1` a `goldfishSim`: the simulated `events`, the
#'   `process_map` with each process's regime, the `times` variant, the
#'   `capped` flag and run `diagnostics`. For `nsim > 1` a list of them.
#'
#' @seealso [set_simulation_steps()], [set_parameter_provider()],
#'   [simulation-handle].
#' @family simulation
#' @importFrom stats simulate
#' @name simulate
#' @examples
#' \donttest{
#' data("Social_Evolution", package = "goldfish")
#' # A specification simulates at parameters you choose.
#' # simulate(spec, nsim = 1, coef = c(-8, 0.1), n_events = 50)
#' }
NULL

#' @rdname simulate
#' @method simulate goldfishJointSpec
#' @export
simulate.goldfishJointSpec <- function(
  object,
  nsim = 1,
  seed = NULL,
  coef = NULL,
  times = c("generated", "observed"),
  n_events = NULL,
  horizon = NULL,
  max_events = NULL,
  steps = NULL,
  control_prep = set_preprocessing(),
  ...
) {
  call <- rlang::current_env()
  times <- match.arg(times)
  if (identical(times, "observed")) {
    cli::cli_abort(
      c(
        "Time-anchored simulation is not available yet.",
        "i" = "Run free-running with {.code times = \"generated\"}."
      ),
      call = call,
      class = "goldfish_sim_unsupported"
    )
  }
  abort_on_effect_free_process(object, call)
  steps <- resolve_simulation_steps(steps, call)
  if (is.null(coef)) {
    coef <- steps$parameters
  }
  if (is.null(coef)) {
    cli::cli_abort(
      c(
        "{.arg coef} is required when simulating from a specification.",
        "i" = "A specification carries no estimates; supply the parameters to
               simulate at."
      ),
      call = call,
      class = "goldfish_sim_bad_coef"
    )
  }

  # Completion runs ONCE here, before any walk opens: a half-specified flavor
  # gains its zero-parameter default and the parameters are reconciled against
  # the completed fid set, so every replicate walks the same processes.
  completed <- complete_generative_spec(
    object,
    consumer = "simulate",
    call = call
  )
  # A pinned rate reads no coefficient and carries no standard error, and the
  # pin's provenance differs per consumer -- an observed event count here, a
  # wave Hamming diff for joint estimation -- so the warning is the consumer's
  # to fire, at its own entry, in its own wording.
  warn_pinned_rates(completed, consumer = "simulate", call = call)
  # A choice-only DyNAM is deliberately NOT rate-completed -- its timing is the
  # ordered strategy, not a fabricated baseline hazard -- so it has no clock to
  # run free. Said here, where the specification is still in view, rather than
  # letting the walk report a missing sub-model.
  if (!any(completed$process_map$family == "rate")) {
    cli::cli_abort(
      c(
        "Free-running simulation needs a process with a rate.",
        "i" = "A choice-only specification has no clock of its own: no
               baseline hazard is fabricated for it.",
        "i" = "Simulate its marks at the observed stamps with
               {.code times = \"observed\"}."
      ),
      call = call,
      class = "goldfish_sim_no_clock"
    )
  }
  provider <- resolve_coef_provider(coef, completed, call)
  if (is.null(n_events) && is.null(horizon)) {
    n_events <- observed_dependent_count(object)
  }
  if (is.null(max_events)) {
    max_events <- 10L * observed_dependent_count(object)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  runs <- lapply(seq_len(nsim), function(replicate) {
    simulate_replicate(
      spec = completed,
      provider = provider,
      steps = steps,
      replicate = replicate,
      times = times,
      horizon = horizon,
      n_events = n_events,
      max_events = max_events,
      control_prep = control_prep,
      call = call
    )
  })
  if (identical(as.integer(nsim), 1L)) runs[[1L]] else runs
}

#' @rdname simulate
#' @method simulate goldfishSpec
#' @export
simulate.goldfishSpec <- function(object, nsim = 1, seed = NULL, ...) {
  simulate(single_process_joint(object), nsim = nsim, seed = seed, ...)
}

#' @rdname simulate
#' @method simulate goldfishFit
#' @export
simulate.goldfishFit <- function(
  object,
  nsim = 1,
  seed = NULL,
  data = NULL,
  coef = NULL,
  ...
) {
  call <- rlang::current_env()
  abort_if_stale_result(object, "a simulation")
  spec <- specification_from_fit(object, data, call)
  if (is.null(coef)) {
    coef <- object$parameters
  }
  simulate(spec, nsim = nsim, seed = seed, coef = coef, ...)
}

#' @rdname simulate
#' @method simulate goldfishFlavFit
#' @export
simulate.goldfishFlavFit <- function(
  object,
  nsim = 1,
  seed = NULL,
  data = NULL,
  coef = NULL,
  ...
) {
  call <- rlang::current_env()
  # Deliberately NOT the container fan-out every other generic uses: the
  # flavors compete for one clock, so one run draws across all of them. A
  # per-flavor list of independent runs would be a different model.
  cli::cli_abort(
    c(
      "Simulating a flavored fit is not available yet.",
      "i" = "Simulate the flavored specification with
             {.fn make_joint_specification} and {.fn set_parameters}."
    ),
    call = call,
    class = "goldfish_sim_unsupported"
  )
}

# --------------------------------------------------------------------------- #
# Helpers
# --------------------------------------------------------------------------- #

resolve_simulation_steps <- function(steps, call) {
  if (is.null(steps)) {
    return(default_simulation_steps())
  }
  if (!is_simulation_steps(steps)) {
    cli::cli_abort(
      c(
        "{.arg steps} must be a {.cls goldfishSimSteps}.",
        "x" = "A {.cls {class(steps)[1]}} was supplied.",
        "i" = "Build one with {.fn set_simulation_steps}."
      ),
      call = call,
      class = "goldfish_sim_bad_step"
    )
  }
  steps
}

# Refuse a DyNAM process whose authored rate is intercept-only and which has
# no choice. Completion would add a uniform choice, and the process would then
# carry no effect anywhere: every sender at one constant rate, every receiver
# equally likely. That is a specification with nothing to simulate from, so it
# is refused before completion rather than walked.
abort_on_effect_free_process <- function(joint_spec, call) {
  rows <- list()
  for (spec in joint_spec$specifications) {
    if (!identical(spec$model, "DyNAM")) {
      next
    }
    for (proc in spec_processes(spec)) {
      rate <- proc$submodels$rate
      if (
        is.null(proc$submodels$choice) &&
          !isTRUE(rate$completed) &&
          is_intercept_only_rate_bundle(rate)
      ) {
        rows[[length(rows) + 1L]] <- data.frame(
          layer = spec$focal,
          flavor = proc$flavor,
          family = "rate",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  refused <- do.call(rbind, rows)
  refused$fid <- seq_len(nrow(refused))
  labels <- render_process_label(refused, refused$fid)
  cli::cli_abort(
    c(
      "Cannot simulate a process with no effect to draw from.",
      "x" = "{.val {labels}} {?is an/are} intercept-only rate{?s} with no
             choice.",
      "i" = "Completion would add a uniform choice, so every draw would be a
             constant rate and an equiprobable receiver.",
      "i" = "Add an effect to the rate, or model a choice."
    ),
    call = call,
    class = "goldfish_sim_no_effect"
  )
}

# Rebuild the specification a fit was estimated from. A `goldfishFit` stores
# its formula (two-sided, the response naming the focal layer), its model and
# its sub-model, but NOT the specification object and NOT the data -- so the
# data comes from the caller, who has it, rather than from a copy of the whole
# stocnet carried on every fitted model.
specification_from_fit <- function(fit, data, call) {
  if (is.null(data)) {
    cli::cli_abort(
      c(
        "{.arg data} is required to simulate from a fitted model.",
        "i" = "A {.cls goldfishFit} stores its formula and estimates, not the
               data it was fitted on; pass the same stocnet."
      ),
      call = call,
      class = "goldfish_sim_no_data"
    )
  }
  formula <- fit$formula
  if (length(formula) < 3L) {
    cli::cli_abort(
      "Cannot recover the focal layer from the fit's formula.",
      call = call,
      class = "goldfish_sim_no_data"
    )
  }
  layer <- deparse1(formula[[2L]])
  rhs <- stats::reformulate(
    attr(stats::terms(formula), "term.labels"),
    intercept = attr(stats::terms(formula), "intercept") == 1L
  )
  args <- list(layer = layer, model = fit$model, data = data)
  args[[if (identical(fit$sub_model, "choice")) "choice" else "rate"]] <- rhs
  do.call(make_specification, args)
}

# The observed dependent-event count, which sets the default stopping target
# and the explosion guard. Read off the schedule the merged walk would step,
# so it counts the same events the run generates.
observed_dependent_count <- function(joint_spec) {
  merged <- build_merged_blocks(joint_spec, set_preprocessing())
  sum(merged$schedule$dependent)
}

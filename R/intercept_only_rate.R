# The intercept-only rate sub-model: a constant baseline hazard whose single
# intercept is *pinned* (a deterministic function of supplied counts/exposures),
# never estimated. It is a `goldfish:::` building block for the timed generative
# consumers -- the D9 completion transform, a future `simulate()` method, and
# DyNES augmentation -- deliberately NOT exported and committing to no
# user-facing signature (the user surface is `rate = ~ 1` in the generative
# context, wired in a later session). Lifecycle: experimental.
#
# The representation is the degenerate DyNAM-rate case: a rate carrying NO
# covariate columns and a single fixed intercept, so the timed hazard
# exp(beta^T s_i) collapses to the constant per-actor hazard exp(intercept) = λ.
# Evaluation reuses the existing constant-hazard/timed-rate evaluator
# (`.pse_eval_rate` via `evaluate_process_state`) rather than adding a second
# evaluator: an intercept-only state is materialized as a single intercept
# column of ones over the active senders, exactly the `has_intercept = TRUE`,
# zero-effect state.
#
# The object records that its intercept is *fixed* (`fixed_intercept`) and that
# it carries zero free parameters (`n_free_parameters`), so the downstream
# θ-layout / optimizer code can exclude it from the score and Hessian. The
# per-period pin `intercept_w = log(count_w / (T_w * |R_w|))` (one plateau per
# inter-wave period), its compute-once-and-freeze contract, and the half-open
# period membership used to select the applicable plateau also live here; the
# uniform-support-legal-sender semantics arrive in a later session.

# The per-period pin -- the pure function every consumer calls at setup. Given a
# per-period event count `count`, the period duration `duration` (the exposure
# denominator T_w), and the period's average risk-set size `risk_set_size`
# (|R_w|), the pinned *per-actor* hazard is
#
#   lambda_w = count_w / (T_w * |R_w|),
#
# recorded as intercept_w = log(count_w / (T_w * |R_w|)). Each argument is a
# numeric vector with one entry per inter-wave period, so the result is a
# piecewise-constant per-actor baseline log-hazard: one plateau per period.
#
# Dividing by |R_w| places the pin at the *per-actor* layer (the analogue of
# RSiena's division by n_actors), so exp(intercept_w) is a per-actor constant
# hazard -- commensurable on the shared clock with a competing flavor's
# per-actor rate in the superposition Sum_i exp(.). The aggregate flavor rate is
# then |R(t)| * exp(intercept_w), reproducing count_w over the period. The pin
# is exactly the per-period frozen form of goldfish's estimated intercept-only
# MLE `log(n_dep_events / total_time / avg_active_entity)`.
#
# The function is deliberately thin (a pure map). It does NOT diff waves, infer
# windows, or compute |R_w| -- those are the consumer's: the augmenter's wave
# Hamming diff or simulate()'s observed count for `count`, and the relational
# time-weighted `avg_active_entity` vs the panel wave-endpoint average
# (|R(w_{k-1})| + |R(w_k)|)/2 for `risk_set_size`. It is a deterministic
# function of the *supplied* counts/durations/risk-set sizes and is never
# recomputed from generated, sampled, or augmented events.
#
# A zero-count period pins to intercept_w = log(0) = -Inf: the per-actor hazard
# exp(-Inf) = 0, so the flavor cannot fire in that period -- correct under the
# net-change (Hamming-diff) count.
pin_intercept_only_rate <- function(
  count,
  duration,
  risk_set_size,
  call = rlang::caller_env()
) {
  if (
    !is.numeric(count) ||
      !is.numeric(duration) ||
      !is.numeric(risk_set_size)
  ) {
    cli::cli_abort(
      "{.arg count}, {.arg duration}, and {.arg risk_set_size} must be numeric.",
      call = call
    )
  }
  n <- length(count)
  if (n < 1L || length(duration) != n || length(risk_set_size) != n) {
    cli::cli_abort(
      c(
        "{.arg count}, {.arg duration}, and {.arg risk_set_size} must be
         non-empty vectors of the same length (one entry per period).",
        "x" = "Got lengths {length(count)}, {length(duration)}, and
               {length(risk_set_size)}."
      ),
      call = call
    )
  }
  if (anyNA(count) || anyNA(duration) || anyNA(risk_set_size)) {
    cli::cli_abort(
      "{.arg count}, {.arg duration}, and {.arg risk_set_size} must not
       contain missing values.",
      call = call
    )
  }
  if (any(count < 0)) {
    cli::cli_abort(
      c(
        "{.arg count} must be a non-negative per-period count.",
        "x" = "Got {.val {count}}."
      ),
      call = call
    )
  }
  if (any(duration <= 0)) {
    cli::cli_abort(
      c(
        "{.arg duration} (the exposure denominator T_w) must be positive.",
        "x" = "Got {.val {duration}}."
      ),
      call = call
    )
  }
  if (any(risk_set_size <= 0)) {
    cli::cli_abort(
      c(
        "{.arg risk_set_size} (|R_w|) must be a positive average risk-set size.",
        "i" = "An empty support set is the consuming routine's guard, not a pin.",
        "x" = "Got {.val {risk_set_size}}."
      ),
      call = call
    )
  }
  log(count / (duration * risk_set_size))
}

# Construct the *frozen* intercept-only rate object from an already-pinned,
# per-period intercept vector (typically `pin_intercept_only_rate()`'s output)
# and the period partition. `intercept` carries one pinned log-hazard per
# inter-wave period (length K >= 1; a length-1 vector is the single-window /
# no-wave-grid case). `wave_times` is the K+1 period boundaries used for the
# half-open membership lookup (`intercept_only_rate_period()`): required when
# there is more than one period, optional for a single plateau (every event then
# falls in the sole period). `model_type` selects the timed evaluator the flavor
# routes through: "DyNAM-M-Rate" for an actor-oriented flavor (per-actor hazard,
# `.pse_eval_rate`), "REM" for a tie-oriented one (per-dyad hazard,
# `.pse_eval_rem`).
#
# "Compute once and freeze" (D7): the intercept is computed at consumer setup
# (the pin is a deterministic function of supplied counts/exposures) and stored
# here as a fixed vector under `frozen = TRUE`. There is deliberately NO
# recompute hook -- the object is read unchanged across every EM / MCMC /
# simulation iteration and never re-derived from generated events. (A future
# latent-count development would replace the frozen value; out of scope here.)
make_intercept_only_rate <- function(
  intercept,
  model_type = c("DyNAM-M-Rate", "REM"),
  wave_times = NULL,
  call = rlang::caller_env()
) {
  model_type <- match.arg(model_type)
  if (!is.numeric(intercept) || length(intercept) < 1L) {
    cli::cli_abort(
      "{.arg intercept} must be a non-empty numeric vector of pinned
       log-hazards (one plateau per period).",
      call = call
    )
  }
  # Each pinned intercept is a finite log-hazard, or -Inf for a zero-count period
  # (per-actor hazard exp(-Inf) = 0: the flavor cannot fire). +Inf and NA are
  # not valid pins.
  if (anyNA(intercept) || any(intercept == Inf)) {
    cli::cli_abort(
      c(
        "Each {.arg intercept} entry must be a finite log-hazard or
         {.val -Inf}.",
        "x" = "Got {.val {intercept}}."
      ),
      call = call
    )
  }
  n_periods <- length(intercept)
  wave_times <- validate_wave_times(wave_times, n_periods, call = call)

  structure(
    list(
      sub_model = "rate",
      model_type = model_type,
      # The degenerate `has_intercept = TRUE`, zero-effect case: a single
      # intercept column and no covariate columns.
      has_intercept = TRUE,
      effects = character(0),
      # The θ-exclusion markers: the intercept is pinned, not estimated, so the
      # rate contributes no free parameter to any joint fit's θ layout.
      fixed_intercept = TRUE,
      n_free_parameters = 0L,
      # The frozen per-period pin: one plateau per inter-wave period, computed
      # once at setup and never recomputed from generated events.
      intercept = intercept,
      wave_times = wave_times,
      n_periods = n_periods,
      frozen = TRUE
    ),
    class = "intercept_only_rate"
  )
}

# Validate the K+1 period boundaries for a K-period pin. Required when K > 1
# (there is no way to place an event in one of several plateaus without the
# grid); optional for a single plateau (K == 1), where every event falls in the
# sole period. When supplied, `wave_times` must be a strictly increasing numeric
# vector of length K+1 (w_0 < w_1 < ... < w_K).
validate_wave_times <- function(
  wave_times,
  n_periods,
  call = rlang::caller_env()
) {
  if (is.null(wave_times)) {
    if (n_periods > 1L) {
      cli::cli_abort(
        c(
          "{.arg wave_times} is required for a multi-period pin.",
          "i" = "Supply the {n_periods + 1L} period boundaries for the
                 {n_periods} plateau{?s}."
        ),
        call = call
      )
    }
    return(NULL)
  }
  if (!is.numeric(wave_times) || length(wave_times) != n_periods + 1L) {
    cli::cli_abort(
      c(
        "{.arg wave_times} must be a numeric vector of {n_periods + 1L} period
         boundaries for a {n_periods}-period pin.",
        "x" = "Got {length(wave_times)} value{?s}."
      ),
      call = call
    )
  }
  if (anyNA(wave_times) || is.unsorted(wave_times, strictly = TRUE)) {
    cli::cli_abort(
      "{.arg wave_times} must be strictly increasing
       ({.field w_0 < w_1 < ... < w_K}).",
      call = call
    )
  }
  wave_times
}

# TRUE for an intercept-only rate object -- the flag θ-layout / optimizer code
# reads to exclude the pinned intercept from the score and Hessian.
is_intercept_only_rate <- function(x) {
  inherits(x, "intercept_only_rate")
}

# The half-open period membership convention (design-resolved): interior wave
# boundaries are left-closed / right-open and the *final* period is
# right-closed, so an event landing exactly on an interior boundary belongs to
# the *next* period and a terminal-time event (the common last-wave observation
# in panel data) is never dropped into a nonexistent period. The partition is
#
#   [w_0, w_1)  [w_1, w_2)  ...  [w_{K-1}, w_K]
#
# which is exactly findInterval(time, wave_times, rightmost.closed = TRUE). It
# returns the 1-based period index for each event time -- the index into the
# frozen per-period `intercept`. A single-plateau rate (no wave grid) places
# every event in period 1. (The final period's *duration* past the last wave, if
# a simulate() window extends beyond w_K, is the consumer-supplied T_w used in
# the pin -- membership here still buckets on the observed wave_times.)
intercept_only_rate_period <- function(rate, time, call = rlang::caller_env()) {
  if (is.null(rate$wave_times)) {
    return(rep(1L, length(time)))
  }
  period <- findInterval(time, rate$wave_times, rightmost.closed = TRUE)
  # findInterval returns 0 below w_0 and K+1 above w_K: an event outside the
  # supplied partition has no pinned plateau. The consumer supplies wave_times
  # covering the observation window, so flag the gap rather than silently
  # returning an out-of-range index.
  if (any(period < 1L | period > rate$n_periods)) {
    last <- rate$wave_times[[rate$n_periods + 1L]]
    cli::cli_abort(
      c(
        "{.arg time} falls outside the pinned period partition
         {.field [{rate$wave_times[[1]]}, {last}]}.",
        "i" = "The consuming routine must supply {.arg wave_times} covering
               every event time."
      ),
      call = call
    )
  }
  period
}

# The constant per-actor intensity exp(intercept) the rate exposes, constant
# across every support-legal actor by construction (equal per-actor hazards).
# With no `time`, returns every period's plateau intensity (length K; a
# single-plateau rate returns one value). With a `time`, returns the intensity
# of the period containing each event time (via the half-open membership above).
intercept_only_rate_intensity <- function(rate, time = NULL) {
  if (is.null(time)) {
    return(exp(rate$intercept))
  }
  exp(rate$intercept[intercept_only_rate_period(rate, time)])
}

# Materialize the degenerate process state for an intercept-only rate over the
# given active-sender mask: a single intercept column of ones and no covariate
# columns, so the shared `.pse_eval_rate` hazard exp(beta^T s_i) collapses to
# the constant exp(intercept) for every active sender, exact-zero for the rest.
# This is the same materialized-state shape the estimation path produces for a
# `has_intercept = TRUE`, zero-effect rate; reusing it is what lets the timed
# evaluator score the pinned rate with no second evaluator.
intercept_only_rate_state <- function(
  rate,
  active_sender,
  timespan = NA_real_,
  event_sender = NA_integer_,
  is_dependent = FALSE
) {
  n <- length(active_sender)
  list(
    model_type = rate$model_type,
    stat_mat = matrix(1, nrow = n, ncol = 1L),
    active_sender = active_sender,
    n_actors1 = n,
    n_actors2 = 1L,
    is_rate = TRUE,
    event_sender = event_sender,
    is_dependent = is_dependent,
    timespan = timespan
  )
}

# Evaluate the intercept-only rate through the existing timed-rate path at one
# instant: select the applicable per-period plateau for `time` (via the
# half-open membership), build the degenerate intercept-only state, and dispatch
# it through `evaluate_process_state()` (which routes a "DyNAM-M-Rate" state to
# `.pse_eval_rate`). Returns that evaluator's per-actor hazard vector unchanged
# -- exp(intercept_w) for every active sender, exact-zero for excluded ones --
# so the pinned flavor slots into the shared-clock superposition Σ_i exp(·)
# alongside a competing flavor's per-actor rates. No new evaluator is introduced.
# `time` may be omitted only for a single-plateau rate; a multi-period rate needs
# the event time to pick the plateau.
evaluate_intercept_only_rate <- function(
  rate,
  active_sender,
  time = NULL,
  timespan = NA_real_,
  event_sender = NA_integer_,
  is_dependent = FALSE,
  call = rlang::caller_env()
) {
  this_intercept <- if (is.null(time)) {
    if (rate$n_periods > 1L) {
      cli::cli_abort(
        c(
          "{.arg time} is required to evaluate a multi-period pinned rate.",
          "i" = "Supply the event time so the applicable plateau is selected."
        ),
        call = call
      )
    }
    rate$intercept
  } else {
    rate$intercept[intercept_only_rate_period(rate, time)]
  }
  state <- intercept_only_rate_state(
    rate,
    active_sender = active_sender,
    timespan = timespan,
    event_sender = event_sender,
    is_dependent = is_dependent
  )
  evaluate_process_state(state, parameters = this_intercept)
}

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
# per-period pin `intercept_w = log(count_w / (T_w * |R_w|))` and the
# uniform-support-legal-sender semantics live in later sessions; this file is
# only the representation.

# Construct an intercept-only rate from an already-pinned intercept (the
# constant log-hazard the consumer / pin function supplies). `model_type`
# selects the timed evaluator the flavor routes through: "DyNAM-M-Rate" for an
# actor-oriented flavor (per-actor hazard, `.pse_eval_rate`), "REM" for a
# tie-oriented one (per-dyad hazard, `.pse_eval_rem`). The intercept is a single
# plateau here; the per-period (piecewise-constant) pin arrives with the pin
# function in a later session.
make_intercept_only_rate <- function(
  intercept,
  model_type = c("DyNAM-M-Rate", "REM"),
  call = rlang::caller_env()
) {
  model_type <- match.arg(model_type)
  if (!is.numeric(intercept) || length(intercept) != 1L) {
    cli::cli_abort(
      "{.arg intercept} must be a single numeric value (a pinned log-hazard).",
      call = call
    )
  }
  # A pinned intercept is a finite log-hazard, or -Inf for a zero-count period
  # (per-actor hazard exp(-Inf) = 0: the flavor cannot fire). +Inf and NA are
  # not valid pins.
  if (is.na(intercept) || intercept == Inf) {
    cli::cli_abort(
      c(
        "{.arg intercept} must be a finite log-hazard or {.val -Inf}.",
        "x" = "Got {.val {intercept}}."
      ),
      call = call
    )
  }

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
      intercept = intercept
    ),
    class = "intercept_only_rate"
  )
}

# TRUE for an intercept-only rate object -- the flag θ-layout / optimizer code
# reads to exclude the pinned intercept from the score and Hessian.
is_intercept_only_rate <- function(x) {
  inherits(x, "intercept_only_rate")
}

# The constant per-actor intensity λ = exp(intercept) the rate exposes. Constant
# across every support-legal actor by construction (equal per-actor hazards).
intercept_only_rate_intensity <- function(rate) {
  exp(rate$intercept)
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

# Evaluate the intercept-only rate through the existing timed-rate path: build
# the degenerate intercept-only state and dispatch it through
# `evaluate_process_state()` (which routes a "DyNAM-M-Rate" state to
# `.pse_eval_rate`). Returns that evaluator's per-actor hazard vector unchanged
# -- exp(intercept) for every active sender, exact-zero for excluded ones -- so
# the pinned flavor slots into the shared-clock superposition Σ_i exp(·)
# alongside a competing flavor's per-actor rates. No new evaluator is introduced.
evaluate_intercept_only_rate <- function(
  rate,
  active_sender,
  timespan = NA_real_,
  event_sender = NA_integer_,
  is_dependent = FALSE
) {
  state <- intercept_only_rate_state(
    rate,
    active_sender = active_sender,
    timespan = timespan,
    event_sender = event_sender,
    is_dependent = is_dependent
  )
  evaluate_process_state(state, parameters = rate$intercept)
}

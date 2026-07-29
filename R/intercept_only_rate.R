# The intercept-only rate sub-model: a constant baseline hazard whose single
# intercept is *pinned* (a deterministic function of supplied counts/exposures),
# never estimated. It is a `goldfish:::` building block for the timed generative
# consumers -- the D9 completion transform, a future `simulate()` method, and
# DyNES augmentation -- deliberately NOT exported and committing to no
# user-facing signature (the user surface is `rate = ~ 1` in the generative
# context, wired in a later session; confirmed still true by the
# "not exported" test at the bottom of this file's test suite). Lifecycle:
# experimental.
#
# ---- The public contract this file provides (design.md D1-D9) --------------
#
# 1. Constant per-actor hazard (D1). The representation is the degenerate
#    DyNAM-rate case: a rate carrying NO covariate columns and a single fixed
#    intercept, so the timed hazard exp(beta^T s_i) collapses to the constant
#    per-actor hazard exp(intercept) = lambda. Evaluation reuses the existing
#    constant-hazard/timed-rate evaluator (`.pse_eval_rate` via
#    `evaluate_process_state`) rather than adding a second evaluator: an
#    intercept-only state is materialized as a single intercept column of ones
#    over the active senders, exactly the `has_intercept = TRUE`, zero-effect
#    state. `make_intercept_only_rate()` / `evaluate_intercept_only_rate()`.
#
# 2. Pinned per-period intercept (D2/D8/D9). Given a per-period count
#    `count_w`, duration `T_w`, and average risk-set size `|R_w|` -- all
#    consumer-supplied, never derived here -- the pin is the PURE function
#    `intercept_w = log(count_w / (T_w * |R_w|))`, one piecewise-constant
#    plateau per inter-wave period, selected by the half-open membership
#    convention `[w_0,w_1) ... [w_{K-1},w_K]`
#    (`findInterval(t, wave_times, rightmost.closed = TRUE)`) so a
#    terminal-time event is never dropped. `|R_w|`'s SOURCE tracks the regime
#    (D9): the RELATIONAL case supplies goldfish's time-weighted
#    `avg_active_entity` restricted to period w (exact, unchanged); the PANEL
#    / DyNES case supplies the wave-endpoint average
#    `(|R_g(w_{k-1})| + |R_g(w_k)|)/2` of the flavor's post-constraint entity
#    count at the two observed wave states (the between-wave risk-set
#    trajectory is latent). Computed once and FROZEN -- read unchanged across
#    every EM/MCMC/simulation iteration, never recomputed from generated
#    events (D7). `pin_intercept_only_rate()`, `make_intercept_only_rate()`.
#
# 3. Uniform support-legal-sender semantics, draw owned by the consumer
#    (D4/D5/D8). `exp(intercept_w)` is a per-actor CONSTANT hazard -- identical
#    for every support-legal actor, not an aggregate flavor scalar -- so it
#    slots into the shared-clock superposition Sum_i exp(.) commensurably with
#    a competing flavor's per-actor rate. The equal hazards make the sender
#    uniform over the support-legal set AS A CONSEQUENCE (self-loops the only
#    automatic restriction; support inherited from the flavor, never
#    fabricated, never borrowed from a sibling). This file exposes the
#    semantics only -- the actual draw and the empty/saturated-support guard
#    are the CONSUMING ROUTINE's, not built here.
#    `intercept_only_rate_sender_semantics()`.
#
# 4. Zero-free-parameters contract (D4/D7). The pinned intercept is
#    THETA-INDEPENDENT (not merely iteration-constant): it never enters the
#    fid / theta layout and is excluded from the optimizer's score and Hessian
#    by dimension. It MAY be added as a constant offset to a *reported* total
#    log-likelihood, never to the optimization objective.
#    `intercept_only_rate_theta_block()`, `joint_theta_layout()`,
#    `intercept_only_rate_loglik_offset()`.
#
# 5. Intercept-only <=> pinned in the generative context (D6). `rate = ~ 1`
#    (no other rate effects) and a completion-supplied rate for a choice-only
#    flavor produce the SAME pinned object -- source-agnostic, read only from
#    bundle shape. A rate carrying any effect keeps its estimated baseline. The
#    single-process path (`estimate_dynam()` / `estimate_rem()`) is untouched:
#    scoped by TYPE to `joint_specification.goldfish`.
#    `is_intercept_only_rate_bundle()`, `pinned_rate_descriptor()`,
#    `mark_pinned_rates()`. Each consumer warns at its own entry, worded for
#    its count source, never suppressed on re-entry: `warn_pinned_rate()`,
#    `warn_pinned_rates()`.
#
# 6. Timed-regime scope only (D3). The primitive applies only where a shared
#    continuous clock exists. `mark_pinned_rates()` aborts (cli) if handed an
#    ORDERED-only (choice-only) joint specification -- a missing rate's timing
#    there is `process-simulation`'s pseudo-time / fixed-template modes, out of
#    scope here. `is_timed_joint_specification()`,
#    `assert_timed_joint_specification()`.
#
# ---- Developer note: what each of the three consumers supplies --------------
#
# Written from this design's contract (D2/D5/D8/D9), NOT from reading consumer
# code -- none of the three exists yet as of this note (2026-07-29). Revisit
# and correct against the real call sites once each lands.
#
#   * `make-multivariate-spec` D9 completion transform (`complete_generative_
#     spec()`, task 1c.2): for each choice-only flavor on the timed branch,
#     supplies the completion bundle `mark_pinned_rates()` classifies as
#     intercept-only (an intercept, no effects), then pins from the flavor's
#     wave Hamming diff (`count_w`, a net-change floor) over each inter-wave
#     `T_w`, with `|R_w|` as the PANEL wave-endpoint average
#     (D9). It does NOT diff waves, infer windows, draw a sender, or guard an
#     empty support set here -- see D5.
#   * `process-simulation`'s `simulate()`: for a mixed rate-modeled/missing
#     timed composition, supplies the observed event count over the wave grid
#     (or a single user-provided/min-max-inferred window absent waves) as
#     `count_w`, the window as `T_w`, and `|R_w|` as either source depending on
#     whether the flavor's timeline is fully observed (RELATIONAL,
#     `avg_active_entity`) or wave-only (PANEL, wave-endpoint average). It
#     performs the sender draw against `intercept_only_rate_sender_semantics()`
#     and guards the empty/saturated-support case.
#   * `dynes-augmentation`'s augmenters / pool evaluator: supplies the wave
#     Hamming diff as `count_w` (same net-change-floor count `estimate_dynes()`
#     warns about), the inter-wave `T_w`, and the PANEL wave-endpoint-average
#     `|R_w|` -- placing a pinned-rate flavor's events on the shared clock by
#     drawing against the exposed semantics and guarding empty support, exactly
#     as `simulate()` does for its own timed composition.
#
# All three read `|R_w|` as the flavor's RATE ENTITY (active senders for an
# actor-oriented flavor, active dyads for a tie-oriented one, D9) -- never a
# flat actor/dyad count, which would badly over-state a sparse tie flavor's
# risk set (D9).

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

# The sender-selection *semantics* the pinned rate exposes -- a description the
# consuming routine draws against, NOT a draw. Two properties, both consequences
# of the per-actor framing (D4/D8):
#
#   1. Commensurability. `exp(intercept_w)` is a per-actor *constant* hazard --
#      the same value for every support-legal actor -- so on the shared clock the
#      pinned flavor enters the competing-risks superposition Sum_i exp(.)
#      alongside any competing flavor's per-actor rates. `hazard` below is that
#      per-actor vector (exp(intercept_w) for every legal actor, exact-zero for
#      the rest), living in the same hazard space `.pse_eval_rate` returns.
#
#   2. Uniform support-legal sender, as a *consequence*. Because the per-actor
#      hazards are equal, the competing-risks attribution of a firing to an actor
#      is proportional to those equal hazards -- i.e. uniform over the
#      support-legal set. `probability` below is that selection distribution
#      (uniform over the legal set, exact-zero elsewhere). It is not a separately
#      imposed uniform draw; it falls out of the equal hazards.
#
# The support-legal set is `support_legal`, the flavor's post-constraint mask
# **supplied by the consumer** -- support is *inherited* from the flavor, never
# fabricated here and never borrowed from a sibling flavor (the function reads
# only the mask it is given). The *only* restriction the primitive applies on its
# own is that self-loops are disallowed: pass `self_loop` (a logical mask marking
# entities that would be self-loops, e.g. the diagonal of a tie-oriented flavor's
# entity enumeration) and those are removed even if `support_legal` marked them
# legal. Actor-oriented flavors have no self-loop at the sender layer and pass
# none.
#
# This function performs **no draw** and holds **no empty/saturated-support
# guard** -- both belong to the consuming simulation/augmentation routine (D5).
# An empty support-legal set yields an all-zero `hazard` and therefore an all-NaN
# `probability` (0 / 0), surfacing rather than silently absorbing the condition
# the consumer must detect. A zero-count period (intercept_w = -Inf) likewise
# yields a zero hazard: the flavor cannot fire, again the consumer's to handle.
intercept_only_rate_sender_semantics <- function(
  rate,
  support_legal,
  self_loop = NULL,
  time = NULL,
  call = rlang::caller_env()
) {
  legal <- as.logical(support_legal)
  n <- length(legal)
  if (!is.null(self_loop)) {
    self_loop <- as.logical(self_loop)
    if (length(self_loop) != n) {
      cli::cli_abort(
        c(
          "{.arg self_loop} must be the same length as {.arg support_legal}.",
          "x" = "Got {length(self_loop)} and {n}."
        ),
        call = call
      )
    }
    # The only automatic restriction: a self-loop is never selectable, even if
    # the inherited support mask marked it legal.
    legal <- legal & !self_loop
  }
  if (is.null(time) && rate$n_periods > 1L) {
    cli::cli_abort(
      c(
        "{.arg time} is required to select the plateau of a multi-period pinned
         rate.",
        "i" = "Supply the firing instant so the applicable per-period hazard is
               used."
      ),
      call = call
    )
  }
  # The equal per-actor hazard exp(intercept_w) for the applicable plateau. The
  # magnitude does not change the uniform selection (any positive constant gives
  # the same distribution); it is carried so `hazard` is the genuine per-actor
  # rate that slots into the shared-clock superposition, and so a zero-count
  # period's zero hazard is faithful.
  intensity <- intercept_only_rate_intensity(rate, time = time)
  hazard <- ifelse(legal, intensity, 0)
  list(
    hazard = hazard,
    probability = hazard / sum(hazard),
    uniform = TRUE,
    support = "inherited",
    self_loops = "disallowed"
  )
}

# The free-parameter block a pinned rate contributes to a joint fit's θ layout:
# the *empty* block. This is the enforcement of the zero-free-parameters
# contract at the layout layer -- a pinned intercept-only rate occupies NO θ
# slot at all.
#
# This is stronger than, and distinct from, an offset() / `fixedParameters`
# term: a fixed parameter in the Newton-Raphson sense still *occupies* a θ slot
# (it is counted in `nParams` and merely held constant across iterations --
# iteration-constancy). The pinned intercept instead never enters the fid / θ
# layout, because it is **θ-independent**: `intercept_w` is a deterministic
# function of the supplied counts/exposures, so its timing likelihood is an
# additive constant w.r.t. θ and is excluded from the score and Hessian by
# dimension, not merely pinned within them. The contract is asserted defensively
# (a pinned rate that reported a free parameter would be a construction bug).
intercept_only_rate_theta_block <- function(rate, call = rlang::caller_env()) {
  if (!is_intercept_only_rate(rate)) {
    cli::cli_abort(
      "{.arg rate} must be an {.cls intercept_only_rate}.",
      call = call
    )
  }
  if (!isTRUE(rate$fixed_intercept) || !identical(rate$n_free_parameters, 0L)) {
    cli::cli_abort(
      c(
        "A pinned intercept-only rate must carry zero free parameters.",
        "x" = "Got {.field fixed_intercept} = {rate$fixed_intercept},
               {.field n_free_parameters} = {rate$n_free_parameters}."
      ),
      call = call
    )
  }
  numeric(0)
}

# Assemble a joint fit's θ layout from an ordered list of per-flavor blocks,
# concatenating each flavor's free-parameter block into one flat θ vector and
# recording the slice of θ each flavor owns. A block is either a numeric vector
# (an estimated flavor's free parameters) or an `intercept_only_rate` object (a
# pinned flavor, contributing the empty block via
# `intercept_only_rate_theta_block()`). This is the shared layout primitive the
# generative consumers build θ with -- not an estimator: it computes no score or
# Hessian and fits nothing.
#
# Because a pinned flavor contributes `numeric(0)`, adding one to a set of
# estimated flavors leaves the flat θ vector and every estimated flavor's index
# slice **unchanged** -- it takes up no dimension, so the optimizer's
# `nParams`-sized score (`rep(0, nParams)`) and Hessian
# (`matrix(0, nParams, nParams)`) are unchanged too. The pinned flavor's slice
# is the empty range `integer(0)`.
joint_theta_layout <- function(blocks, call = rlang::caller_env()) {
  if (!is.list(blocks)) {
    cli::cli_abort("{.arg blocks} must be a list.", call = call)
  }
  values <- lapply(blocks, function(block) {
    if (is_intercept_only_rate(block)) {
      intercept_only_rate_theta_block(block, call = call)
    } else if (is.numeric(block)) {
      block
    } else {
      cli::cli_abort(
        c(
          "Each element of {.arg blocks} must be a numeric free-parameter block
           or an {.cls intercept_only_rate}.",
          "x" = "Got a block of class {.cls {class(block)}}."
        ),
        call = call
      )
    }
  })
  lengths <- vapply(values, length, integer(1))
  ends <- cumsum(lengths)
  starts <- ends - lengths + 1L
  index <- Map(
    function(start, len) {
      if (len == 0L) integer(0) else seq.int(start, len = len)
    },
    starts,
    lengths
  )
  names(index) <- names(blocks)
  list(
    theta = unlist(values, use.names = FALSE) %||% numeric(0),
    index = index,
    n_free = sum(lengths)
  )
}

# The pinned rate's constant contribution to a *reported* total log-likelihood
# (spec: it MAY appear as a constant offset, never in the optimization
# objective). For a constant-hazard flavor over period `w` the timing
# log-likelihood is
#
#   count_w * intercept_w - lambda_w * (T_w * |R_w|) = count_w * (intercept_w - 1)
#
# since lambda_w = exp(intercept_w) = count_w / (T_w * |R_w|). It depends only on
# the frozen `intercept_w` and the consumer-supplied per-period `count` -- and
# crucially on **no θ**: the function takes no parameter vector, so it is
# θ-independent by construction and can never enter the score or Hessian. A
# zero-count period contributes 0 (its `intercept_w = -Inf` never multiplies a
# nonzero count).
intercept_only_rate_loglik_offset <- function(
  rate,
  count,
  call = rlang::caller_env()
) {
  if (!is_intercept_only_rate(rate)) {
    cli::cli_abort(
      "{.arg rate} must be an {.cls intercept_only_rate}.",
      call = call
    )
  }
  if (!is.numeric(count) || length(count) != rate$n_periods) {
    cli::cli_abort(
      c(
        "{.arg count} must be a numeric vector with one entry per period.",
        "x" = "Got {length(count)} value{?s} for {rate$n_periods}
               period{?s}."
      ),
      call = call
    )
  }
  if (anyNA(count) || any(count < 0)) {
    cli::cli_abort(
      "{.arg count} must be non-negative and free of missing values.",
      call = call
    )
  }
  # A zero-count period pins to intercept_w = -Inf; 0 * (-Inf - 1) is NaN, but
  # its likelihood contribution is 0 (no event, zero hazard integrated).
  terms <- ifelse(count == 0, 0, count * (rate$intercept - 1))
  sum(terms)
}

# ---- User surface: intercept-only <=> pinned in the generative context -------
#
# In the generative/joint context an intercept-only rate -- `rate = ~ 1` with no
# other rate effects, or a rate a completion transform supplies for a
# choice-only flavor -- is understood as *pinned* (zero free parameters), not
# estimated. The re-interpretation is safe because goldfish's leading `1` is
# otherwise an *estimated* baseline log-hazard used pervasively as
# `~ 1 + effects`; a *bare* intercept-only rate is the one case with nothing
# worth estimating (its MLE is the degenerate log(N / T / |R|), exactly the
# per-period frozen pin this primitive computes). A rate carrying ANY effect
# keeps its estimated baseline intercept unchanged -- only the effect-free rate
# is pinned. This treatment is scoped to the generative context and lives here;
# the single-process estimation path is untouched.

# Classify a *rate* submodel bundle as intercept-only. This is the single,
# source-agnostic test both a user-written `~ 1` and a completion-supplied rate
# route through: it reads only the bundle's shape -- a rate sub_model carrying an
# intercept and NO other effects -- never how the bundle was produced, so the two
# sources are indistinguishable here and reinterpret to the same pinned object. A
# rate carrying any effect (`~ 1 + inertia`) has a non-empty effect list and is
# NOT intercept-only, so it keeps its estimated baseline. A non-rate family
# (choice / rate_ordered -- whose `has_intercept` is forced FALSE upstream) is
# never intercept-only.
is_intercept_only_rate_bundle <- function(bundle) {
  is.list(bundle) &&
    identical(bundle$sub_model, "rate") &&
    isTRUE(bundle$has_intercept) &&
    length(bundle$parsed$rhs_names) == 0L
}

# Map a specification's `model` ("DyNAM" / "REM") to the timed-rate evaluator's
# `model_type`, so a pinned actor-oriented flavor routes through the per-actor
# hazard and a tie-oriented (REM) flavor through the per-dyad hazard.
pinned_rate_model_type <- function(model, call = rlang::caller_env()) {
  switch(
    model,
    "DyNAM" = "DyNAM-M-Rate",
    "REM" = "REM",
    cli::cli_abort(
      "A pinned intercept-only rate supports only {.val DyNAM} and {.val REM}
       models, not {.val {model}}.",
      call = call
    )
  )
}

# The canonical zero-parameter pinned descriptor an intercept-only rate becomes
# in the generative context -- the SAME object for a user-written `~ 1` and a
# completion-supplied rate, because it depends on nothing that distinguishes the
# two (only the intercept-only shape and the flavor's `model`). It carries the
# θ-exclusion markers (`fixed_intercept`, `n_free_parameters = 0`) so a joint
# fit's θ layout excludes it, and the timed evaluator's `model_type` -- but NOT
# yet the per-period `intercept` vector: that is pinned by the consumer at setup
# from the counts / exposures it supplies (`pin_intercept_only_rate()` ->
# `make_intercept_only_rate()`), never from the bundle.
pinned_rate_descriptor <- function(bundle, model, call = rlang::caller_env()) {
  if (!is_intercept_only_rate_bundle(bundle)) {
    cli::cli_abort(
      c(
        "{.arg bundle} must be an intercept-only rate ({.code rate = ~ 1}).",
        "i" = "A rate carrying any effect keeps its estimated baseline
               intercept; only the effect-free rate is pinned."
      ),
      call = call
    )
  }
  list(
    sub_model = "rate",
    model_type = pinned_rate_model_type(model, call = call),
    has_intercept = TRUE,
    effects = character(0),
    fixed_intercept = TRUE,
    n_free_parameters = 0L,
    pinned = TRUE
  )
}

# Reinterpret every intercept-only rate in a joint specification as PINNED -- the
# generative-context rule, and ONLY there. Walks the composed specifications'
# submodel bundles in `process_map` (fid) order, sets a `pinned` logical column
# on the map (TRUE for a rate fid whose bundle is intercept-only, FALSE for every
# estimated rate, every rate carrying effects, and every non-rate family), and
# attaches the per-fid pinned descriptors under `pinned_rates`. A user-written
# `~ 1` and a completion-supplied rate for a choice-only flavor produce the
# identical bundle shape, so both are marked pinned identically -- one concept,
# one treatment.
#
# The reinterpretation is scoped to the generative context by TYPE: it accepts
# ONLY a `joint_specification.goldfish`. The single-process estimation path
# operates on a plain `specification.goldfish` and never reaches this function,
# so a bare `rate = ~ 1` under `estimate_dynam()` / `estimate_rem()` keeps its
# existing estimated-intercept meaning untouched.
mark_pinned_rates <- function(joint_spec, call = rlang::caller_env()) {
  if (!inherits(joint_spec, "joint_specification.goldfish")) {
    cli::cli_abort(
      c(
        "{.arg joint_spec} must be a {.cls joint_specification.goldfish}.",
        "i" = "Pinning an intercept-only rate is scoped to the generative
               context; the single-process path ({.fn estimate_dynam} /
               {.fn estimate_rem}) keeps its estimated-intercept meaning."
      ),
      call = call
    )
  }
  # Timed-regime scope guard (D3): the primitive is meaningful only where a
  # shared continuous clock exists. An ordered-only composition is rejected
  # here rather than silently marking (and finding) nothing to pin.
  assert_timed_joint_specification(joint_spec, call = call)
  # Keyed by the character fid, in the same order `build_joint_process_map()`
  # assigns fids, so `fid_bundles[[as.character(fid)]]` is that row's bundle.
  fid_bundles <- joint_fid_bundles(joint_spec)
  map <- joint_spec$process_map
  pinned <- vapply(
    map$fid,
    function(fid) {
      entry <- fid_bundles[[as.character(fid)]]
      identical(entry$family, "rate") &&
        is_intercept_only_rate_bundle(entry$bundle)
    },
    logical(1)
  )
  map$pinned <- pinned
  descriptors <- lapply(map$fid[pinned], function(fid) {
    entry <- fid_bundles[[as.character(fid)]]
    pinned_rate_descriptor(entry$bundle, entry$model, call = call)
  })
  names(descriptors) <- as.character(map$fid[pinned])
  joint_spec$process_map <- map
  joint_spec$pinned_rates <- descriptors
  joint_spec
}

# ---- Timed-regime scope guard (D3) -------------------------------------------
#
# The intercept-only rate primitive is meaningful only where a shared continuous
# clock exists to place events on: the TIMED regime, i.e. a joint composition in
# which at least one process carries a genuine waiting-time/intensity rate
# (`sub_model == "rate"`, as opposed to the ordered regime's `"rate_ordered"`
# stand-in, which has no continuous intensity to pin against). This is D9's
# regime rule ("timed iff any process carries a waiting-time/intensity rate"),
# read directly off the joint specification's own submodel bundles
# (`joint_fid_bundles()`) rather than duplicated as a second classifier.
#
# In the ORDERED regime a missing rate has no clock to pin against; its timing
# is `process-simulation`'s pseudo-time / fixed-template modes (D3), out of
# scope here. This guard only rejects that case at the primitive's own
# joint-specification-level entry points (`mark_pinned_rates()`,
# `warn_pinned_rates()`) -- it does NOT confirm that any particular *caller*
# (e.g. `make-multivariate-spec`'s D9 completion transform, task 1c.2) reaches
# this primitive only on the timed branch. That confirmation needs 1c.2 itself,
# which does not exist yet (this change lands before it, per the land order);
# it is deferred to mv-spec's own tasks 1c.5/1c.6 to assert against the real
# caller once it lands, not stubbed here.

# TRUE iff `joint_spec` carries at least one genuine waiting-time rate
# (`sub_model == "rate"`) anywhere in its process map -- the composition is
# TIMED. A composition built only from `"rate_ordered"` / `"choice"` /
# `"choice_coordination"` submodels (no continuous intensity anywhere) is
# ORDERED. Not exported; used only by this file's own guard.
is_timed_joint_specification <- function(joint_spec) {
  fid_bundles <- joint_fid_bundles(joint_spec)
  any(vapply(
    fid_bundles,
    function(entry) identical(entry$sub_model, "rate"),
    logical(1)
  ))
}

# Abort with a clear cli error unless `joint_spec` is in the TIMED regime. The
# defensive check the intercept-only rate primitive applies at its own
# joint-specification-level entry points -- it needs no consumer to exist, since
# it reads the regime straight off the supplied `joint_spec`'s own submodel
# bundles.
assert_timed_joint_specification <- function(
  joint_spec,
  call = rlang::caller_env()
) {
  if (!inherits(joint_spec, "joint_specification.goldfish")) {
    cli::cli_abort(
      "{.arg joint_spec} must be a {.cls joint_specification.goldfish}.",
      call = call
    )
  }
  if (!is_timed_joint_specification(joint_spec)) {
    cli::cli_abort(
      c(
        "The intercept-only rate primitive applies only in the {.strong timed}
         regime.",
        "x" = "This joint specification carries no waiting-time/intensity rate
               ({.field sub_model} = {.val rate}) anywhere in its process map
               -- it is {.strong ordered}.",
        "i" = "A missing rate's timing in the ordered regime is handled by
               {.pkg process-simulation}'s pseudo-time / fixed-template modes,
               not by this primitive."
      ),
      class = "goldfish_intercept_only_rate_ordered_regime_error",
      call = call
    )
  }
  invisible(joint_spec)
}

# ---- Context-aware pinned-rate warning (D6) ----------------------------------
#
# Each generative consumer -- estimate_dynes() and the future simulate() method
# -- warns at its OWN entry that a rate is pinned, worded for its count source
# (D6/D2): estimate_dynes() reads the wave Hamming diff (a net-change floor),
# has no standard error, and excludes the rate from estimation; simulate()
# reads the observed event count and has no standard-error language (SE is not
# a simulation concept). Neither consumer exists yet (grep confirms no
# `estimate_dynes()` in R/model_estimate.R and no `simulate.joint_specification.
# goldfish` method) -- this defines the wording/logic every consumer will call
# at its entry point, ahead of the wiring.
#
# The warning is NOT suppressed on re-entry: `cli::cli_warn()` fires every call
# by default (no `.frequency = "once"`/"regularly" is set here, matching every
# other `cli_warn()` call in this package), so the SAME specification routed
# through a second consumer warns again -- deliberately, since the two
# consumers' wording differs and a user landing in the second consumer needs
# its own no-SE/exclusion notice.
warn_pinned_rate <- function(
  fid,
  consumer = c("estimate_dynes", "simulate"),
  call = rlang::caller_env()
) {
  consumer <- match.arg(consumer)
  switch(
    consumer,
    estimate_dynes = cli::cli_warn(
      c(
        "!" = "Rate {.field {fid}} is an intercept-only rate ({.code ~ 1}):
               it is {.strong pinned}, not estimated.",
        "i" = "The pin comes from the wave {.strong Hamming diff} between
               observed states -- a net-change floor.",
        "i" = "It carries no standard error and is excluded from estimation."
      ),
      class = "goldfish_pinned_rate_warning",
      call = call
    ),
    simulate = cli::cli_warn(
      c(
        "!" = "Rate {.field {fid}} is an intercept-only rate ({.code ~ 1}):
               it is {.strong pinned}, not estimated.",
        "i" = "The pin comes from the {.strong observed event count} over the
               relevant period."
      ),
      class = "goldfish_pinned_rate_warning",
      call = call
    )
  )
  invisible(NULL)
}

# Warn once per pinned fid, in `process_map` fid order, for a joint
# specification already passed through `mark_pinned_rates()`. This is the
# entry-point call each consumer makes: `estimate_dynes()` calls it with
# `consumer = "estimate_dynes"`, the future `simulate()` method with
# `consumer = "simulate"` -- against the SAME `pinned_rates` attached by
# `mark_pinned_rates()`, so a joint specification routed through both consumers
# warns independently at each entry (never suppressed, D6). A specification
# with no pinned rate warns about nothing.
warn_pinned_rates <- function(
  joint_spec,
  consumer = c("estimate_dynes", "simulate"),
  call = rlang::caller_env()
) {
  consumer <- match.arg(consumer)
  if (!inherits(joint_spec, "joint_specification.goldfish")) {
    cli::cli_abort(
      "{.arg joint_spec} must be a {.cls joint_specification.goldfish}.",
      call = call
    )
  }
  pinned_fid <- names(joint_spec$pinned_rates)
  for (fid in pinned_fid) {
    warn_pinned_rate(fid, consumer = consumer, call = call)
  }
  invisible(joint_spec)
}

# Run-level ESS diagnostics for the ascent-based Monte Carlo EM (ABMCEM) loop
# driven by estimate_dynes(). Distinct from the between-iteration ESS redraw
# guard in weights_dynes.R (`ess_guard()`, which regenerates a stale persistent
# pool): these surfaces only inform. The loop owns the run-level dedup state and
# threads it through every pass so a diagnostic fires at most once per estimation
# run, never per iteration or per draw.

# The cold-start pathology floor as a fraction of the pool size: one fifth of the
# ESS redraw-guard threshold, so it is always strictly below it. The floor is
# reserved for a genuinely pathological startup, not the ordinary staleness the
# between-iteration guard handles; a fraction of `ess_threshold` (rather than a
# fixed constant) keeps it strictly under any user-set guard threshold.
cold_start_pathology_fraction <- function(ess_threshold = 0.5) {
  ess_threshold / 5
}

# The one-shot cold-start ESS diagnostic. The ESS redraw guard is silenced at
# startup (a redraw of the first pool is statistically pointless -- a uniform
# redraw is identical), but that silence must not hide a PATHOLOGICAL cold start:
# when the first-iteration ESS falls below the pathology floor (a fraction of the
# pool size strictly below the 0.5 redraw guard), this diagnostic -- separate
# from the guard warning -- names the proposal/target mismatch and its levers.
# It only informs: it never redraws or grows the pool. It stays quiet on the
# default theta0 = 0 path, where the ESS of a near-uniform pool sits near K, and
# earns its keep on the `random` + non-zero-theta0 corner where the weights
# collapse at iteration 1. `weights` are the normalized startup E-step weights;
# the loop calls this once, at iteration 1.
cold_start_ess_diagnostic <- function(weights, ess_threshold = 0.5) {
  ess <- effective_sample_size(weights)
  pool_size <- length(weights)
  floor_value <- cold_start_pathology_fraction(ess_threshold) * pool_size
  decision <- list(ess = ess, diagnosed = FALSE, redraw = FALSE, grow = FALSE)
  if (ess >= floor_value) {
    return(decision)
  }
  cli::cli_warn(c(
    "!" = "Startup effective sample size {.val {round(ess, 2)}} is pathologically
           low (below {.val {round(floor_value, 2)}} for a pool of
           {.val {pool_size}}); the proposal sits far from the target at the
           starting parameters.",
    "i" = "This is a proposal/target mismatch, not ordinary staleness: a redraw
           at the same parameters would be statistically identical.",
    "i" = "Consider a model-driven {.arg routine} ({.val sim} or {.val mcmc})
           with {.arg initialize} = {.val sim}, or start from
           {.arg initial_parameters} = {.code NULL} (theta0 = 0)."
  ))
  decision$diagnosed <- TRUE
  decision
}

# The single seam for the mid-run IS-ESS collapse threshold, as a fraction of the
# pool size. This change owns only the message surface that consumes it; the
# numeric floor -- where importance-weighted augmentation degenerates faster than
# pool growth can recover, the flips-per-interval cliff -- is owned by
# dynes-augmentation's 6.2/6.3 degeneracy study. Reading it through this one
# accessor (rather than a literal at the call site) is what keeps the runtime
# recommendation and the documented operating envelope from drifting: when the
# study lands, dynes-augmentation calibrates the value here, in one place. Until
# then the threshold is NA and the recommendation stays dormant -- the surface
# ships without a guessed number baked in.
importance_ess_collapse_threshold <- function() {
  NA_real_
}

# Run-level diagnostic state, owned by the estimation loop and threaded through
# every pass. The dedup flags keep the mid-run ESS-collapse recommendation and
# the sim restart-coupling warning firing at most once per estimation run (never
# per iteration or per draw). The loop creates one at entry.
new_run_warning_state <- function() {
  state <- new.env(parent = emptyenv())
  state$ess_collapse_warned <- FALSE
  state$restart_coupling_warned <- FALSE
  state
}

# The mid-run IS-ESS collapse recommendation (design D5, dynes-augmentation
# D14/#5). Distinct from BOTH the ESS redraw guard (which regenerates the pool)
# and the cold-start diagnostic (a startup bad-theta0 corner): when the pool's
# effective sample size collapses mid-run under an importance-weighted augmenter
# (`routine` "random"/"sim") because flips-per-interval is high, the IS-weight
# variance grows ~exponentially in the sequence-space dimension, so no redraw at
# any theta recovers it. The loop then emits a cli recommendation to switch to
# `augment_seq_mcmc()`, which does not pay the IS-dimension tax. The
# recommendation only informs -- it never switches augmenters or redraws -- and
# fires at most once per run. It does not apply to MCMC (its uniform weighting
# makes the same statistic trivially the pool size) or at startup (the cold-start
# diagnostic owns iteration 1). The numeric floor comes from
# importance_ess_collapse_threshold(); a dormant (NA) threshold never fires.
recommend_mcmc_on_ess_collapse <- function(
  state,
  weights,
  routine,
  at_startup = FALSE
) {
  if (
    at_startup ||
      !routine %in% c("random", "sim") ||
      isTRUE(state$ess_collapse_warned)
  ) {
    return(invisible(FALSE))
  }
  threshold <- importance_ess_collapse_threshold()
  ess <- effective_sample_size(weights)
  pool_size <- length(weights)
  if (is.na(threshold) || ess >= threshold * pool_size) {
    return(invisible(FALSE))
  }
  state$ess_collapse_warned <- TRUE
  cli::cli_warn(c(
    "!" = "Effective sample size {.val {round(ess, 2)}} has collapsed mid-run
           under the importance-weighted {.arg routine} = {.val {routine}} (pool
           size {.val {pool_size}}); redrawing cannot recover it.",
    "i" = "With many changes per interval the importance-weight variance grows
           with the sequence-space dimension, a cost no redraw at any parameters
           removes.",
    "i" = "Switch to {.fn augment_seq_mcmc}, which does not pay the
           importance-sampling dimension tax."
  ))
  invisible(TRUE)
}

# Restart-coupling warning dedup (design D5, dynes-augmentation D20). The sim
# augmenter's reject-and-restart fires inside augmenter draws the loop drives;
# the augmenter reports that a restart occurred, but the LOOP owns the run-level
# dedup so exactly one warning fires per estimation run (the first restart at any
# iteration or draw), naming the coupled constraint and its plug-in-normalizer
# bias -- never one warning per iteration or per draw. A coupling-free run (no
# restart ever fires) emits none, and neither does `random`: its redraws are
# normalized by a theta-free constant that cancels in the normalized weights,
# so they carry no plug-in bias.
warn_restart_coupling_once <- function(
  state,
  restarted,
  routine = "sim",
  constraint = NULL
) {
  if (
    !identical(routine, "sim") ||
      !isTRUE(restarted) ||
      isTRUE(state$restart_coupling_warned)
  ) {
    return(invisible(FALSE))
  }
  state$restart_coupling_warned <- TRUE
  coupled <- if (length(constraint) > 0L) {
    c(
      "i" = "Coupled support {cli::qty(length(constraint))}
             constraint{?s}: {.val {constraint}}."
    )
  }
  cli::cli_warn(c(
    "!" = "A coupled constraint forced the {.val sim} augmenter to reject and
           restart at least one interval during this run.",
    coupled,
    "i" = "Restart-conditioning normalizes the proposal density by an estimated
           completion probability, a plug-in normalizer that biases the
           importance weights.",
    "i" = "Consider {.fn augment_seq_mcmc}, or loosening the coupled constraint,
           if the coupling is severe."
  ))
  invisible(TRUE)
}

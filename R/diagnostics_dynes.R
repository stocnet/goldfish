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

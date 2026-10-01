---
depends-on: abmcem
---

## Why

`pool_fixed_loglik()` marks a pooled sequence degenerate only when scoring a
fixed-only sub-model *errors*. When the engine *returns* a non-finite
log-likelihood instead, the sequence comes back as `loglik = −Inf,
degenerate = FALSE`. The known trigger is a pinned rate whose plateau is
`−Inf` with events in its period: `pinned_rate_loglik()` sums `−Inf` event
terms. That breaks the evaluator contract that `abmcem` specifies. The free
sub-models are still evaluated, and the sequence reports a finite score and
Fisher information next to a `−Inf` log-likelihood, with `degenerate = FALSE`.
Weights still vanish (softmax sends `−Inf` to zero), so today's estimates are
not corrupted. But every consumer that filters on `degenerate` instead of on
the weight (the sandwich covariance, the degenerate count) sees a healthy
sequence, and a `+Inf`/`NaN` term (an infinite plateau gives `Inf − Inf`)
would not vanish at all. Fix it now because `dynes-augmentation` task 6.1
wires `wave_times` into `estimate_dynes()`, which is the first path that can
produce a per-period `−Inf` pin. Its task 2.2f removes the one known cause,
but the guard must not depend on that.

## What Changes

- `pool_fixed_loglik()` treats a non-finite fixed-only term (`−Inf`, `+Inf`,
  `NaN`, `NA`) exactly as it already treats an engine error: the sequence is
  degenerate, with `loglik = −Inf`.
- Both callers inherit the fix with no change of their own, because they share
  `pool_fixed_loglik()`: pool entry (`new_pool_entry()`, the stored
  `fixed_degenerate`) and the proposal evaluator (`evaluate_candidate()`, once
  per candidate).
- A degenerate pooled sequence then gets the full degenerate result from
  `evaluate_pool_sequence()`: free sub-models skipped, score and Fisher `NA`,
  `degenerate = TRUE`.
- No user-facing API change. One `NEWS.d/` fragment records the fix (this is
  a feature branch, so no `DESCRIPTION`/`NEWS.md` edit).

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `abmcem-optimization`: the "Pool evaluation through the zero-iteration
  engine path" requirement extends its degenerate-sequence clause from a
  singular information matrix to any non-finite fixed-only term, with a new
  scenario. The requirement exists today only in `abmcem`'s `## ADDED` delta,
  so this delta is written against that wording, and the placement pre-flight
  is expected to fail until `abmcem` folds at the `feature/dynes` merge.

## Impact

- **Code:** `R/evaluator_dynes.R`, `pool_fixed_loglik()` (and its comment).
- **Tests:** `tests/testthat/test-evaluator_dynes.R`, next to the existing
  fixed-only and pinned-rate tests, reusing the `fixed_only_*` fixtures.
- **Unblocks:** `dynes-augmentation` task 6.1 (wiring `wave_times`).
- **Baselines:** none touched. Finite fixed-only terms take the same path as
  before, so no frozen coefficient baseline can move.

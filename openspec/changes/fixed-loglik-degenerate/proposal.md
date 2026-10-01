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
sequence, and a `+Inf`/`NaN` term would not vanish at all. Fix it now because
`dynes-augmentation` task 6.1 wires `wave_times` into `estimate_dynes()`,
which is the first path that can produce a per-period `−Inf` pin. Its task
2.2f removes the one known cause, but the guard must not depend on that.

A `−Inf` pin with events in its period is not bad luck: the pin is derived
from per-period counts (a zero count pins `−Inf`), and a sampler under that
pin cannot fire there. It only arises when the pin and the sequences disagree
on periods, i.e. a specification bug. Because the pin belongs to the
specification, such a bug degenerates the whole pool, and today the user sees
only a generic ESS or fully-degenerate-pool message. The fix therefore also
reports the cause.

## What Changes

- `pool_fixed_loglik()` treats a non-finite fixed-only term (`−Inf`, `+Inf`,
  `NaN`, `NA`) exactly as it already treats an engine error: the sequence is
  degenerate, with `loglik = −Inf`.
- Both callers inherit the fix with no change of their own, because they share
  `pool_fixed_loglik()` through `new_pool_entry()`: pool entry and the
  proposal evaluator (`evaluate_candidate()`, once per candidate).
- A degenerate pooled sequence then gets the full degenerate result from
  `evaluate_pool_sequence()`: free sub-models skipped, score and Fisher `NA`,
  `degenerate = TRUE`.
- **User-visible through `compute_lik_seq()`**: a sequence whose fixed-only
  term is non-finite now returns `degenerate = TRUE` with `NA` score and
  Fisher information, where it returned `degenerate = FALSE` with a finite
  score. Its `@details` list of degenerate causes gains this case.
- Each pool entry records why it is degenerate (`"walk"` or `"fixed_term"`)
  and, for a fixed term, the label of the sub-model whose term failed.
- `estimate_dynes()` emits one end-of-run warning of class
  `goldfish_dynes_fixed_term_degenerate` when any drawn sequence was dropped
  for a non-finite fixed-only term: the cumulative count over all drawn
  sequences, the failing sub-models' labels, and a request to report it as a
  goldfish bug (every fixed-only term is derived by goldfish, so the user
  cannot cause it).
- When every sequence in the current pool is dropped for a fixed term, the
  ABEM loop stops at once with the last accepted parameters and
  `converged = FALSE`, on both weighting paths. Today importance weighting
  reports such a run as converged at θ it never moved, and uniform weighting
  aborts with retries exhausted.
- The fit's `em_trace` gains a per-pass integer column `fixed_degenerate`: the
  number of sequences in the current pool dropped for a non-finite fixed-only
  term.
- One `NEWS.d/` fragment records the fix (this is a feature branch, so no
  `DESCRIPTION`/`NEWS.md` edit).

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `abmcem-optimization`: the "Pool evaluation through the zero-iteration
  engine path" requirement extends its degenerate-sequence clause from a
  singular information matrix to any non-finite fixed-only term, with
  scenarios for the pooled and the single-candidate path. A new requirement,
  "Fixed-term degeneracy is reported", covers the warning, the `em_trace`
  column, and the early stop of a fully fixed-term-degenerate pool. The modified requirement exists today only in `abmcem`'s `## ADDED`
  delta, so the placement pre-flight is expected to fail on the `## MODIFIED`
  header until `abmcem` folds at the `feature/dynes` merge.

## Impact

- **Code:** `R/evaluator_dynes.R` (`pool_fixed_loglik()`, `new_pool_entry()`,
  the pool builders' tally, `compute_lik_seq()` roxygen),
  `R/em_loop_dynes.R` (`abem_trace_tibble()` column, the early stop in
  `run_abem_loop()`), `R/estimate_dynes.R` (end-of-run warning helper and its
  call).
- **Docs:** `devtools::document()` for the `compute_lik_seq()` `@details`
  change.
- **Tests:** `tests/testthat/test-evaluator_dynes.R`, next to the existing
  fixed-only and pinned-rate tests, reusing the `fixed_only_*` fixtures;
  `test-em_loop_dynes.R` / `test-estimate_dynes.R` for the column and the
  warning. The `em_trace` snapshots in `_snaps/em_loop_dynes.md` and
  `_snaps/fit_dynes.md` change intentionally with the new column.
- **Unblocks:** `dynes-augmentation` task 6.1 (wiring `wave_times`) after task
  group 1; group 2 does not gate it.
- **Baselines:** none touched. Finite fixed-only terms take the same path as
  before, so no frozen coefficient baseline can move.

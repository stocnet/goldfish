## Why

Every exact-time fit has been paying a **second full risk-set `exp` pass per
event** since 1.9.14, for a diagnostic component the caller never asked for by
name. `set_algorithm_newton()` defaults to `diagnostics = c("loglik", "scores")`;
`"loglik"` sets `return_total_rate`; and `return_total_rate` is one of the flags
guarding the shifted log-sum-exp block that `950f21b` (in `backend-parity`) added
to the exact-time kernels. The guard reads as opt-in and is in practice always on.

Measured on the Social Evolution REM fixture (439 events, 6 972 dyads):

| | seconds |
|---|--:|
| `diagnostics = "scores"` (block skipped) | 4.72 |
| `diagnostics = c("loglik", "scores")` (default) | 6.01 |

A **27 % tax on the default path**. A version sweep across all 44 installable
releases ≥ 1.7.1 places it exactly at 1.9.13 → 1.9.14, and a commit-level bisect
inside that range isolates it to `950f21b`. It is confined to the two exact-time
kernels and scales with risk-set size — REM **+23.2 %** (6 972 dyads),
DyNAM-rate **+3.6 %** (84 senders) — while the four ordinal/multinomial kernels,
which already guard their probability vector, moved 0.0–1.1 %.

It shipped invisibly because it moved no number. The frozen baselines protect the
coefficients; nothing was watching the cost.

## What Changes

- **Do not recompute what the iteration already has.** The exact-time kernels
  compute the raw rate vector and its sum (`normalizer`) for the likelihood, then
  compute a *second*, max-shifted `exp` pass to obtain `log_normalizer`. Where the
  raw pass is finite and strictly positive, `log_normalizer` SHALL be derived from
  it (`log(normalizer)`) and the probability vector from the raw weights, with the
  shifted pass retained as the fallback for under/overflow. This preserves the
  stabilization guarantee `950f21b` was written for while paying for it only when
  it is needed.
- **Stop materializing an unread vector.** `arma::vec probabilities = weights /
  shifted_total` is unconditional inside the guarded block in
  `REM_default.cpp` and `DyNAM_rate_default.cpp`, though only
  `margins` / `probabilities` / `conditional_scores` read it. It moves under its
  own guard, matching what the ordinal and multinomial kernels already do.
  (Patched and measured in isolation: ~0.2 s of the 1.3 s.)
- **A standing cost gate.** The existing timing requirement is a one-off record
  of a past refactor's speedup. It becomes a **standing** obligation: a small,
  fixed cell set is timed and its cost recorded, so a regression of this shape is
  caught by a test rather than by a version sweep run for another purpose a year
  later.
- No user-visible behavior changes. `"loglik"` keeps mapping to `intervalLogL`,
  `total_rate` and the conditional component on exact-time submodels; the
  diagnostics vocabulary, its default, and every stored component are untouched.

## Capabilities

### New Capabilities

*(none — this is a cost defect in a shipped contract, not new behavior)*

### Modified Capabilities

- `likelihood-computation`: adds a requirement that the exact-time kernels derive
  the diagnostic normalizer from the pass the likelihood already computes rather
  than running a second `exp` over the risk set, and that the per-event
  probability vector is materialized only for the primitives that read it;
  promotes the one-off "Timing evidence for the refactor goal" requirement into a
  standing per-event cost gate.

## Impact

- `src/REM_default.cpp`, `src/DyNAM_rate_default.cpp` — the two exact-time
  kernels. The four ordinal/multinomial kernels are already correct and are
  **not** touched.
- `tests/testthat/` — a cost gate, plus a test that the conditional component is
  unchanged on both the fast and the fallback branch.
- **Constraint (ADR-0021):** no frozen coefficient baseline may move. Coefficients
  and the aggregate log-likelihood do not depend on `conditional_logl`; the
  surviving invariants are every coefficient, every standard error, and the
  log-likelihood, all bit-comparable at the existing 1e-6 floor. The conditional
  component itself may move in its last bits where the fast branch is taken, since
  `log(sum(e))` and a max-shifted log-sum-exp are not bit-identical — that is the
  one numerical consequence, and it is stated here **before** the tests are run.
- Cross-checks: `backend-primitive-parity` requires cpp/r/gather agreement to
  1e-10 at a fixed parameter vector, which the fast branch must continue to meet.

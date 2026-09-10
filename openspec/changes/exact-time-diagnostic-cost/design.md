## Context

The two exact-time kernels (`REM_default.cpp`, `DyNAM_rate_default.cpp`) run one
pass per event to build the raw rate vector `e` and its sum `normalizer`. The
likelihood needs both on the **absolute** scale: the compensator enters as
`-Δt · normalizer`, so a max-shifted normalizer would be a different model, not a
stabilization. `950f21b` needed a numerically safe `log_normalizer` for the
conditional component, and rather than reuse the raw pass it added a **second**,
max-shifted `exp` pass beside it. The commit message is explicit that leaving the
raw pass untouched was what kept every frozen coefficient in place — a correct and
deliberate choice about *numbers*, taken without a measurement of *cost*.

The guard on that block includes `return_total_rate`, which
`R/model_estimate.R` sets from `"loglik" %in% control_algo$diagnostics`, and
`"loglik"` is in the default `diagnostics`. So the second pass runs on every fit
by default.

Measured decomposition on Social Evolution REM (439 events, 6 972 dyads, median
of 3):

| build | default | `"scores"` only |
|---|--:|--:|
| current | 6.01 s | 4.72 s |
| probability vector guarded | 5.80 s | 4.74 s |

The whole block costs ~1.3 s; the unread probability vector is ~0.2 s of it, and
the second `exp` pass is the remaining ~1.1 s.

## Goals / Non-Goals

**Goals:**
- Remove the second `exp` pass from the default exact-time path without changing
  what `"loglik"` stores.
- Keep the overflow/underflow guarantee that motivated the shifted pass.
- Leave a standing gate so a cost regression of this shape fails a test.

**Non-Goals:**
- Changing the `diagnostics` vocabulary, its default, or the `"loglik"` →
  `{intervalLogL, total_rate, conditional}` mapping. The contract is documented
  and specified; this change makes it cheap, not different.
- Touching the ordinal/multinomial kernels. They already guard correctly and
  measured 0.0–1.1 % across the regression.
- Optimizing the raw likelihood pass itself.

## Decisions

### D1 — Derive the log-normalizer from the raw pass, with a guarded fallback

`log_normalizer = log(normalizer)` and `probabilities = e / normalizer` whenever
the raw pass is well-scaled; otherwise fall back to the existing
`log_sum_exp_masked`. "Well-scaled" is `std::isfinite(normalizer) && normalizer >
0` — the two conditions under which the raw ratio is exactly the quantity the
shifted pass computes.

*Alternatives considered.* **(a) Always use the shifted pass** — the status quo;
correct, and the thing being paid for. **(b) Always use the raw pass** —
reintroduces exactly the subnormal-underflow/overflow failure the shifted pass was
added to fix. **(c) Make `"loglik"` stop implying the conditional component** —
cheaper still, but it breaks a specified mapping and would be a user-visible
behavior change to fix an implementation defect. D1 is the only option that keeps
the contract, keeps the numerical guarantee, and removes the cost from the common
path.

### D2 — The probability vector moves under its own guard

`arma::vec probabilities` is declared empty and filled only under
`return_probabilities || return_margins || return_conditional_scores`. This is
what `REM_ordered_default.cpp`, `DyNAM_rate_ordered_default.cpp`,
`DyNAM_choice_default.cpp` and `DyNAM_MM_default.cpp` already do; the two
exact-time kernels are the outliers. Independently measured at ~0.2 s, and it
stands on its own even after D1 — under D1's fallback branch the same waste would
otherwise return.

### D3 — The fast branch is verified against the fallback, not against itself

A test fixes a parameter vector and asserts the conditional component agrees
between the two branches to 1e-10, and that `backend-primitive-parity`'s
cpp/r/gather agreement still holds. Forcing the fallback needs a seam — an
internal switch or a fixture scaled into the subnormal regime. **Open question
below.**

### D4 — The cost gate is a ratio, not a wall-clock threshold

Absolute timings are machine-dependent and would make the suite flaky. The gate
measures the **default path against the same fit with the block disabled**
(`diagnostics = "scores"`) on one small fixture, and fails if the ratio exceeds a
recorded ceiling. That is the quantity this change is about, it is
self-normalizing across machines, and it would have caught `950f21b` on the day.

*Alternative considered.* Recording absolute seconds in a ledger, as the existing
"Timing evidence" requirement does — it documents a moment but gates nothing, which
is precisely how this shipped.

### D5 — ADR-0021 compliance is a prediction, written before the run

Stated up front, per ADR-0021's rule that a baseline may only move when the new
value was derived beforehand: **no frozen coefficient baseline moves.** The
kernels' returned `derivative`, `fisher`, `logLikelihood` and `intervalLogL` are
computed from the raw pass and are untouched. The one quantity that may move in
its last bits is `conditional_logl` on the fast branch, since `log(Σe)` and a
max-shifted log-sum-exp are not bit-identical; it is a diagnostic, enters no
baseline, and is bounded by the 1e-10 parity floor.

## Risks / Trade-offs

- **The fast branch is taken on a fixture where the shifted pass mattered, and a
  diagnostic silently degrades.** → The guard is on `normalizer` itself, so the
  fallback triggers precisely when the raw value is not usable. D3's test pins
  both branches.
- **`conditional_logl` moves in its last bits and a downstream snapshot test
  fails.** → Predicted here rather than explained afterwards; if a snapshot moves,
  the 1e-10 parity assertion decides whether it is the expected drift or a defect.
- **The cost-gate ratio is noisy on a loaded CI machine.** → One small fixture,
  median of repeats, and a ceiling with headroom. It is there to catch a 25 %
  structural regression, not a 2 % drift.
- **The gate hides behind `skip_on_cran()` and never runs.** → It must run in the
  `NOT_CRAN=true` suite and be confirmed PASS, not SKIP, like the other frozen
  gates.

## Migration Plan

No user-facing migration. The kernels change, `compileAttributes()` reports no
interface delta (the edits are body-only), and the `NOT_CRAN=true` suite with the
frozen baselines PASS is the acceptance gate. Rollback is a revert of the two
kernel files.

## Open Questions

1. **How is the fallback branch exercised in a test?** An internal
   force-fallback argument on the kernels is the direct route but widens an
   exported signature for a test; a fixture scaled into the subnormal regime keeps
   the signature but is fragile. Decide before task 3.
2. **What ceiling does the cost gate use?** Needs the post-D1 ratio measured
   first; the number is recorded in the change log when task 4 runs, not guessed
   now.
3. **Should DyNAM-rate's `reduce_stat_mat * parameters` also be hoisted?** It
   recomputes a linear predictor inside the guarded block that the accumulation
   loop above has already formed as per-row dot products. Same family of defect,
   but a separate measurement — out of scope unless it shows up in task 1's
   profile.

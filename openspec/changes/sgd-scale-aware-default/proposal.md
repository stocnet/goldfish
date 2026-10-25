---
depends-on: abmcem
status: landed (feature/dynes, awaiting fold)
---

## Why

The DyNES M-step's SGD defaults (`set_sgd_options()`: `step_size = 0.001`,
`step_schedule = "constant"`, `max_iterations = 100L`) take a fixed raw step on a summed
score whose scale grows with the event count and the information. The step therefore
under-steps on realistic pools:
- On the abmcem toy (K = 30, 6–11 free θ), every M-step hit the 100-iteration cap with
  gradient norm 0.9–14. Q decayed geometrically and no 15-iteration run met the stop
  rule (abmcem D17 follow-up, `abmcem/progress.md`).
- On the `dynes-augmentation` 6.2b count-panel oracle, every routine stopped at the cap
  with gradient norm 0.009, above `sgd_tolerance = 1e-3`. That left a 0.02-SE gap to the
  Poisson MLE that was first misread as Monte-Carlo error
  (`.plan/dynes/recovery/PILOT_findings.md`).
- On the `dynes-augmentation` cluster sweep (2026-10-09, `.plan/dynes/recovery/FINDINGS.md`
  B3–B5), three more readings came in:
  - At the top flips level, the constant step instead **overshoots** the large summed
    score. `random`, `sim` and `mcmc` all abort at EM iteration 1 with Q < 0 on every
    pass (`goldfish_abem_retry_exhausted`), before the mid-run ESS check is ever
    reached.
  - `adagrad` at 0.1 overshoots the other way: from a warm start already at the
    optimum (ASE = 0), its steps do not vanish, so Q < 0 is certain and the run
    aborts. `adagrad` at 0.01 avoids both failures and is the study's setting.
  - The M-step's own stop rule cannot certify convergence. `sgd_tolerance = 1e-3` on
    the raw summed score sits below the minibatch noise, so the convergence flag was
    FALSE in nearly every coupled cell and every cell from f15 up, while the median
    exact Newton step to the pool optimum there was 0.01–0.05 SE. The study therefore
    gates on that Newton step (< 0.1 SE, `dynes-augmentation` design D25, amended
    2026-10-09) and records the flag without using it.

So the problem is not only the step size. The stop rule is in raw score units too,
and a fixed tolerance there means a different thing on every pool.

A user who calls `estimate_dynes()` with the defaults gets estimates between θ₀ and the
optimum and no sign that the M-step stopped short. `dynes-augmentation` works around
this for its validation study with one tuned setting and a per-replicate convergence
gate (its design D25), but the default itself should reach the optimum.

## What Changes

- Choose a scale-aware default step for the M-step. Candidates: one of the existing
  `"adagrad"`/`"adam"` schedules (the study's `adagrad` 0.01 is the incumbent), or a
  step normalized by the event count or by the pooled information. The design picks
  one on evidence from the toy, the oracle fixture and the study's failure cells. On
  the low-flip cells the default has to reach the optimum. At the top flips level it
  must not overshoot (B3), and from a warm start at the optimum it must not overshoot
  either (B4).
- Make the M-step's stop rule scale-aware, so that its convergence flag means "θ is at
  this pool's optimum" in standard-error units, as D25's Newton-step gate does. The
  candidates are a tolerance on the information-scaled step, `I⁻¹ s̄` relative to SE,
  or a final Newton step on the pool. A tolerance on the raw summed score is not one
  of them. **Success criterion:** on those fixtures, a default `estimate_dynes()`
  reports converged exactly when the Newton step to its final pool's optimum is under
  the design's SE threshold. Meeting the raw `sgd_tolerance` is not the criterion.
- Surface an M-step that ends unconverged under that rule: record it in the `em_trace`
  and say so in the fit's convergence report, rather than leaving it silent. Under
  the raw-score rule this report would fire on nearly every coupled fit (B5), so it
  ships with the new rule, never before it.
- **BREAKING (numerical):** default DyNES estimates move toward the optimum. DyNES is
  experimental and unreleased on this branch, so no lifecycle deprecation applies. No
  frozen coefficient baseline covers `estimate_dynes()`, so the `tests/testthat/_baselines/`
  floor is untouched.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `abmcem-optimization`: "SGD M-step over the concatenated parameter vector" — the
  default step rule and the stop rule become scale-aware, and an unconverged M-step is
  reported.
  The requirement is introduced by the unarchived `abmcem` change, so this delta is
  written against it and archived after `abmcem`.
- `dynes-estimation` (also introduced by `abmcem`): "set_algorithm_em() and its
  nested constructors control the ABEM algorithm" lists the new default schedule
  and the standard-error units of `sgd_tolerance`. An ADDED requirement makes the
  fit report whether its estimate is at the final pool's optimum (`newton_se`,
  `at_optimum`).

## Impact

- `R/algorithm_controls.R` (`set_sgd_options()` defaults and docs), the SGD M-step
  optimizer behind `estimate_dynes()`, the `em_trace` and convergence reporting of
  `goldfishDynesFit`.
- Tests and cli snapshots that encode default-SGD trajectories in the DyNES suites.
- **Sequencing on `feature/dynes` (amended 2026-10-14):** this change does not gate the
  `dynes-augmentation` study itself (6.2.1–6.2.5 and the 6.2.8 write-up), which fixes
  its own SGD setting (design D25). It **does** run before that change's remaining code
  tasks (6.2.6, 6.2.7, 6.2.9, 6.2.10, 6.2.11). Those tasks write the oracle and recovery
  tests, the ESS-floor recommendation (reached only once the M-step survives to the
  mid-run check) and the user-facing vignette. Written before this change, they would
  encode the under-stepping default, or an explicit setting the default then replaces,
  and the vignette would need re-rendering. It goes **after** `focal-stream-dedup`, so
  each moved seeded expectation has one cause. Design work may start at once. Fold
  order at the merge: after `abmcem`, whose requirement the delta modifies. Once this
  lands, compare the study's tuned setting with the new default.
  (Was: "Does not gate `dynes-augmentation` D15", written 2026-10-05, before the cluster
  sweep's B3–B5 findings.)

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

A user who calls `estimate_dynes()` with the defaults gets estimates between θ₀ and the
optimum and no sign that the M-step stopped short. `dynes-augmentation` works around
this for its validation study with one tuned setting and a per-replicate convergence
gate (its design D25), but the default itself should reach the optimum.

## What Changes

- Choose a scale-aware default for the M-step so that a default `estimate_dynes()` meets
  `sgd_tolerance` on the toy and oracle fixtures. Candidates: one of the existing
  `"adagrad"`/`"adam"` schedules, or a step normalized by the event count or by the
  pooled information. The design picks one, on evidence from both fixtures.
- Surface an M-step that ends at its iteration cap above `sgd_tolerance`: record it in
  the `em_trace` and say so in the fit's convergence report, rather than leaving it
  silent.
- **BREAKING (numerical):** default DyNES estimates move toward the optimum. DyNES is
  experimental and unreleased on this branch, so no lifecycle deprecation applies. No
  frozen coefficient baseline covers `estimate_dynes()`, so the `tests/testthat/_baselines/`
  floor is untouched.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `abmcem-optimization`: "SGD M-step over the concatenated parameter vector" — the
  default step rule becomes scale-aware, and a capped, unconverged M-step is reported.
  The requirement is introduced by the unarchived `abmcem` change, so this delta is
  written against it and archived after `abmcem`.

## Impact

- `R/algorithm_controls.R` (`set_sgd_options()` defaults and docs), the SGD M-step
  optimizer behind `estimate_dynes()`, the `em_trace` and convergence reporting of
  `goldfishDynesFit`.
- Tests and cli snapshots that encode default-SGD trajectories in the DyNES suites.
- Does not gate `dynes-augmentation` D15, which fixes its own SGD setting (design D25).
  Once this lands, that study's tuned setting can be compared with the new default.

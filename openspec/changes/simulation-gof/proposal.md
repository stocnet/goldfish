> **Stub (2026-09-09).** Proposal and design only; tasks are written when the
> change is scheduled, after `process-simulation` lands. Created to give the
> simulation-based goodness of fit for an ordinary fit a home: `residuals-gof`
> deferred it as its phase 3 on 2026-07-31 and no successor existed.

## Why

`test_gof()` ships one discrepancy for a DyNAM or REM fit: the Boschi–Wit
cumulative-score bridge test, analytic, over the effects in the model. The
other classical family, auxiliary statistics compared against their
model-implied distribution, has no general asymptotic null and is
simulation-based, so it waited for `simulate()`. `gof-dynes` D14 settles the
surface for the DyNES fit (a `type` axis under `test_gof()`, the reference
distribution chosen by the fit class, not the user) and explicitly says the
same pair of options exists for a `goldfishFit` once `process-simulation`
lands. This change gives the plain fit its predictive discrepancy, on the
same axis, from the same simulator.

## What Changes

- **`test_gof(fit, type = "auxiliary")` on `goldfishFit` and
  `goldfishFlavFit`**: simulate `nsim` sequences from the fit through
  `simulate()`, compute the auxiliary statistics on the observed and the
  simulated sequences, and compare them (Mahalanobis with the singularity
  guard `gof-dynes` D9 defines, or per-statistic quantile envelopes).
- **The `times` variant per statistic** (`process-simulation` D6): order and
  mark statistics (degree distributions, dyad and triad census,
  reciprocation and closure shares, p-shift distributions) use the
  time-anchored variant, which is the cheap and conditionally exact default;
  timing statistics (inter-event distributions, waiting-time deciles,
  intercept calibration) require the free-running variant with a horizon.
  The result records which variant produced each statistic. Capped
  replicates and completed or replayed components are excluded per
  `process-simulation` D3 and D9.
- **Statistics as a small registry**, shared with `gof-dynes` D8's built-ins
  where the definition is the same, with the plain-fit statistics living
  here; user-supplied statistic functions accepted with the same signature.
- **Plot data contract, not a plot**: the result carries the observed value,
  the simulated distribution and the envelope per statistic in the shape
  `autograph` plots (ADR-0039); no `plot()` method here.
- **The omnibus stays gated**: any combined p-value across statistics is
  reported only after the simulation study ADR-0003 requires.

## Capabilities

### New Capabilities

- `simulation-gof`: the predictive auxiliary-statistic discrepancy for an
  ordinary fit under `test_gof(type = )`, its statistic registry, the
  per-statistic `times` variant, the exclusion rules, and the plot-data
  contract.

### Modified Capabilities

- `diagnostic-tests`: `test_gof()` gains the `type` axis for `goldfishFit`
  (the same delta `gof-dynes` D14 makes for the DyNES fit; whichever lands
  first writes it, the other inherits).

## Impact

- **Consumes** `process-simulation` (`simulate()` with both `times`
  variants, the capped flag and the regime record) and `gof-dynes` D14's
  `type` axis and control-object shape.
- **Sequencing**: after `process-simulation`; alongside or after `gof-dynes`
  phase 3, sharing its statistic definitions and reporting classes
  (`goldfishGOF` extended, not a new class).
- **Theory**: `.plan/sp/dynes_gof_score.md` Construction 2 (the predictive
  reference for the score discrepancy) is the second `type` this surface
  can carry later; out of scope for the first version.
- **Code**: `R/test_gof.R`, a new statistics file, tests on seeded fixtures;
  no C++ expected.

## Context

Stub, 2026-09-09. The shipped `test_gof()` is the analytic score
discrepancy (ADR-0017 fixed its basis; ADR-0003 gates its omnibus on a
simulation study). `gof-dynes` D14 places a second, predictive discrepancy
under the same generic on a `type` axis and states that the plain
`goldfishFit` will carry the same pair once `simulate()` exists. `residuals-gof`
recorded this as its deferred phase 3. `process-simulation` now defines the
simulator this change drives: one loop, the `times` axis (free-running /
time-anchored), capped-replicate flagging, and the per-component regime
record.

## Goals / Non-Goals

**Goals:**
- One `type` axis on `test_gof()` for every fit class, with the reference
  distribution determined by the fit and reported, never chosen by the user.
- The auxiliary discrepancy for DyNAM and REM fits on both `times` variants,
  each statistic declaring which variant it needs.
- A plot-data contract autograph consumes.

**Non-Goals:**
- The DyNES half (`gof-dynes` owns it).
- The predictive score discrepancy (Construction 2 of the DyNES score note),
  a later `type`.
- A `plot()` method (ADR-0039).
- Any omnibus combination before ADR-0003's study.

## Decisions

_To be written when scheduled._ Decisions the proposal already implies, to
be numbered then:

- The statistic registry's shape and which built-ins ship first (degree
  distributions, reciprocation and closure shares, p-shifts anchored; waiting
  time deciles free-running).
- Per-statistic `times` variant declared in the registry entry, with the
  result recording it; a statistic requesting free-running on a Cox-family
  fit reports pseudo-time as such (`process-simulation` D2).
- Exclusion of capped replicates and completed/replayed components, read
  from the simulation result rather than recomputed.
- Comparison: Mahalanobis with `gof-dynes` D9's guard for the joint
  statistic, quantile envelopes per statistic for the plot data.
- Result: extend `goldfishGOF` with a `type` field rather than a new class,
  so autograph's dispatch stays one.

## Risks / Trade-offs

- [The statistic set diverges between the plain and DyNES halves] → one
  registry, statistics defined once, each half selecting.
- [Free-running simulation of a Cox-family fit yields pseudo-time] → timing
  statistics refuse or label on such fits; anchored is the default for
  everything else.

## Open Questions

- Which built-in statistics ship in the first version, and whether the
  p-shift census belongs here or in `gof-dynes` D8.
- Whether `nsim` defaults follow `gof-dynes` D6's "at least ten grown
  sequences per seed" or a plain count.

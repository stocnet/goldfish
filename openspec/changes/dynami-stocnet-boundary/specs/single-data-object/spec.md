# single-data-object Delta Specification

## Deferred (not delivered by this change)

The planned public **legacy-environment abort** — `make_specification()` /
`estimate_*()` (including `estimate_dynami()`) aborting on `is.environment(data)`
and naming a migration path, plus `as_goldfish()` converting a saved environment
— is **deferred** out of this change (decision 2026-07-23).

Its precondition (design D4) was that `make_data()` never returns an environment
for any family. Apply falsified that: `make_data()` still returns an environment
for a valid, common input — a dependent-events object subset against a
fuller-event covariate network is not stocnet-assemblable and falls back to the
environment. A blanket public abort would therefore break real subset workflows,
and its migration advice ("rebuild via the constructors") is circular for a
subset. `as_goldfish()` likewise still defers environment conversion.

The abort and the `as_goldfish()` environment conversion move to the change that
guarantees `make_data()` assembles every input to a `stocnet` (the
`single-data-object` family). No requirement in the living spec changes here.

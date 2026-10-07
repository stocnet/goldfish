---
status: proposal stub (open questions; ADR-0096, proposed)
---

## Why

A flavored layer is the package's way of modeling competing processes on one
state (creation and dissolution of a tie), and it works today for exactly two
flavors with the net state as the only thing an effect can read. Settling the
simulation's replay and anchored counts (ADR-0094, ADR-0095) showed that
several flavor questions cut across specification, effects, estimation and
simulation, and that none of them belongs to one of those changes alone.
This change collects them so each is settled once, with its own decision.

## What Changes

Scope, to be settled question by question before any design (ADR-0096):

- **Counts per layer and per flavor.** A simulation reports replay and
  anchored-stamp counts as replicate totals. Decide whether they are also
  kept per flavor, on the result's `process_map` rows beside the regime, as
  pool summary columns, or both.
- **Effects that read one flavor's past events.** An effect reads a layer's
  net state. Decide how an effect restricts itself to the past events of one
  flavor (for example an inertia over past creations only): an argument on
  the existing effects, or a derived per-flavor layer they already read.
- **More than two flavors on a `mutually_exclusive` layer.** Under
  `mutually_exclusive` a dyad holds exactly one state and changes only by
  jumping to another state, so a flavor is a jump and its support is the
  dyads in the state it leaves. Today only two states are expressible, with
  two complementary masks. Decide how more states, or more jumps, are
  declared so each flavor's support mask is derived.
- **`add_flavor()` for that case.** It validates a dichotomous
  `values_equivalence` (`+1`/`-1` or `1`/`0`). Decide whether it declares
  the many-state case, how, and how an existing two-flavor call keeps
  working.

Nothing is implemented until these are settled; this file is a stub.

## Capabilities

### New Capabilities

_To be decided once the questions settle._

### Modified Capabilities

Likely, to be confirmed against the living specs:

- `flavored-processes`: flavors beyond two states; per-flavor support.
- `model-specification`: the mask derivation for a many-state
  `mutually_exclusive` layer.
- `multivariate-specification`: per-flavor reporting.

## Impact

Candidates: `add_flavor()` and `validate_flavor_mapping()`
(`R/add_flavor.R`), the support-mask derivation in `make_specification()`,
the effect argument surface, the simulation result's `process_map` and
`summary()` on a pool (`R/simulate_driver.R`, `R/simulate_pool.R`).
Related changes: `process-simulation` (the counts, which stay totals there),
`per-family-flavor-modeling` (the estimation side of half-specified flavors,
a different question), `effect-term-registry` (the effect argument surface).

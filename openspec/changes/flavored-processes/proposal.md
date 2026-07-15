## Why

Relational *states* (friendship, alliances, treaties) evolve through competing processes —
creation and dissolution — each deserving its own rate/choice specification and
parameters, with the tie state itself inducing support constraints (create only where no
tie exists; dissolve only where one does). `refactor-single-data-object` landed the
minimal seam (reserved `ties$flavor` column, flavor-keyed formula list with **exactly
one** modeled flavor); this change activates the full mechanism for **fully observed**
flavored event streams: K modeled flavors as K parallel processes over one layer, each
with derived support constraints, preprocessed in one pass and estimated as separate
factorized likelihoods. No MCMC, no EM — Stage A of the DyNES roadmap, estimable with
existing machinery (the panel-augmentation estimation core is the follow-up
`dynes-augmentation` change).

## What Changes

- **Multi-flavor modeled specifications**: the one-modeled-flavor abort in
  `make_specification()` is lifted — flavor-keyed `rate`/`choice` lists accept K > 1 keys
  (e.g. `list(creation ~ ..., dissolution ~ ...)`), each flavor a parallel process on the
  same focal layer. A flavor present in the data but absent from the formulas updates
  state but is not modeled.
- **Flavor metadata on layer info**: `flavor_style ∈ {mutually_exclusive, redundant}` and
  a `values_equivalence` named-vector mapping live in layer info metadata, never on ties.
- **`add_flavor()` helper** (goldfish verb): thin — populates `ties$flavor` from update
  values and records the mapping/style in layer info. It does NOT precompute constraints.
  Limited to dichotomous states (creation/dissolution-style named vector); other
  encodings warn about accumulate/replace semantics and point to `weighted = FALSE`.
- **Derived support constraints at specification time**: for `mutually_exclusive` flavors,
  `creation → ~ !tie(L)` and `dissolution → ~ tie(L)` masks are derived per flavor and
  combined (AND) with any user `support_constraint`; K flavors produce K derived masks in
  the specification plan's derivations.
- **Sensible defaults with a cli message**: an unflavored layer under a flavored model
  infers `increment ±1 → creation/dissolution` (and `replace 1/0` for replace layers);
  ambiguous encodings (non-±1 weights, no mapping) abort with guidance.
- **One preprocessing pass, per-flavor outputs**: all formulas preprocess together —
  an effect appearing in multiple formulas is computed once; output is a list of
  per-flavor preprocessed objects. For timed rate sub-models (DyNAM-rate, REM),
  other-flavor events enter each flavor's stream as **right-censored** events; for
  ordered/choice sub-models they only update process state.
- **Per-flavor estimation, sectioned results**: each flavor's model is estimated
  separately from its preprocessed data (the joint likelihood factorizes per flavor —
  verified by the gating math note, task 1); the result is a list of goldfish results
  printed by flavor sections (and rate/choice within each for DyNAM).
- **Deferred**: flavor-filtered effect arguments (`outdeg(L, flavor = "dissolution")`) —
  usage study first, own change; a joint multivariate C++ estimation pass — time gains
  unclear, revisit after this change; `flavor` as a manynet reserved ties name — upstream
  proposal, non-blocking.

## Capabilities

### New Capabilities
- `flavored-processes`: the competing-process contract — flavor metadata
  (`flavor_style`, `values_equivalence`) on layer info, `add_flavor()`, default flavor
  inference, per-flavor derived support constraints, single-pass multi-formula
  preprocessing with per-flavor right-censoring, per-flavor factorized estimation, and
  the sectioned multi-process result object.

### Modified Capabilities
- `model-specification`: flavor-keyed `rate`/`choice` lists accept multiple keys (the
  single-flavor abort is replaced by multi-process validation: same key set across
  rate/choice, keys resolve against layer flavors/`values_equivalence`); specification
  print nests all modeled flavors under the dependent layer with per-flavor formulas and
  derived constraints.
- `flat-preprocess-output`: preprocessing of a flavored specification returns per-flavor
  `preprocessed.goldfish` objects whose intercept scalars (`n_dep_events`,
  `avg_active_actors`) and dependent/right-censored partition are per-flavor (each
  flavor's mask and dependent subset; other-flavor events right-censored on timed
  sub-models).
- `support-constraint`: specification-derived flavor masks enter the derivations
  alongside user constraints (`tie(L)` atoms reading the modeled layer's state are legal;
  mask-reading atoms remain forbidden); the per-flavor combined constraint drives risk
  sets and `avg_active_actors` exactly as a user constraint does.

## Impact

- **R**: `R/make_specification.R` (multi-key validation, derived constraints, print),
  new `add_flavor()` + layer-info metadata helpers, `R/model_preprocess.R` /
  `R/preprocess_builders.R` (per-flavor plans, shared-effect dedup, per-flavor RC
  routing), `R/model_estimate.R` + results classes (per-flavor estimation loop, sectioned
  print via cli).
- **C++ (`src/`)**: none expected — per-flavor estimation reuses the existing engines on
  per-flavor preprocessed objects.
- **Dependencies**: none new (manynet already in Imports from
  `refactor-single-data-object`).
- **Sequencing**: requires `refactor-single-data-object` (stocnet input, flavor seam,
  layer-info metadata) implemented first; `support-constraint` derivations machinery
  already on the branch. The `dynes-augmentation` change consumes this surface unchanged.
- **Docs**: Fisheries-style creation/dissolution example becomes the flagship
  two-process model; vignette section on competing processes.

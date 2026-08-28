## Why

Two consumers need to supply parameter values against a
`joint_specification.goldfish` and neither has a defined surface for it.
`estimate_dynes()` (`abmcem`) needs `initial_parameters` — a starting θ₀ over
the joint spec's concatenated, fid-indexed parameter vector; `simulate()`
(`process-simulation`) needs `coef` — a fully specified θ to drive the walk.
Both face the identical problem: the same effect name recurs across fids
(`inertia` in three processes), so a bare effect name cannot identify a
coefficient, and a flat positional vector over the concatenated θ is fragile
(a silently mis-aligned block gives a wrong-but-runnable result). The joint
spec already carries everything needed to solve this — the `process_map` makes
the integer `fid` the canonical consumer identity and renders human-readable
labels from it, and `formula_parser` already emits the per-effect offset
(fixed) mask. What is missing is one small, shared surface that projects those
into a user-writable parameter object.

## What Changes

This change adds a **shared parameter-layout surface** to the
`multivariate-specification` capability, consumed by both `estimate_dynes()`
and `simulate()`:

- **`set_parameters(spec, ...)` → `parameters.goldfish`**: a builder taking a
  `joint_specification.goldfish` and one **full-length per-fid vector** per
  process sub-model, keyed by a **readable composite label**
  (`layer[:flavor]:sub_model`, the focal layer being the process id). Each
  per-fid vector is the fid's effects in formula order; per-effect **names are
  optional** (validated if given, positional otherwise). `NA` marks a slot the
  builder does not pin. The returned object validates itself once against the
  spec (label resolution, per-fid length, name agreement) and carries the flat
  θ projection plus layout metadata, so both consumers trust it without
  re-validating.
- **`NA` disambiguation by the spec's offset mask** (the one semantic rule):
  **offset always prevails**. A slot the formula marks `offset()` is fixed at
  the spec's value regardless of what `set_parameters()` receives there — `NA`
  is accepted silently, and a **non-NA value at an offset slot warns and is
  ignored** (the offset wins). A `NA` at a **non-offset** slot means **free** —
  to be estimated by `estimate_dynes()`, and **incomplete** for `simulate()`.
- **Complete-vs-partial contract**: `parameters.goldfish` tracks whether every
  non-offset slot is filled. `estimate_dynes()` treats the free (non-offset
  `NA`) slots as the parameters to estimate; `simulate()` **asserts
  completeness** and aborts on any remaining free `NA`, naming the free
  effects — the parallel to the walk handle asserting generative completeness.
- **`coef_layout()` generic**: one row per effect — `fid`, process label,
  `sub_model`, `flavor`, effect name, `fixed` (logical), the **offset value**
  for fixed rows, and the `index` into the flat free-parameter θ. Methods for a
  `joint_specification.goldfish` (empty layout, so users can author
  `set_parameters()` before they have values), for a `parameters.goldfish`
  (layout + supplied values), and for a fitted result (layout + θ̂/SE, used by
  `summary()`/`print()`).

## Capabilities

### Modified Capabilities

- `multivariate-specification`: gains the parameter-layout surface —
  `set_parameters()` → `parameters.goldfish`, the readable composite label
  grammar over the `process_map`, the offset-prevails `NA` disambiguation, the
  complete-vs-partial contract, and the `coef_layout()` generic. The
  `joint_specification.goldfish` object is unchanged; this is additive surface
  over the existing `process_map` and offset mask.

## Impact

- **R**: new `R/joint_parameters.R` (`set_parameters()`,
  `parameters.goldfish` class + validator/`print()`, `coef_layout()` generic
  and its methods). Projections reuse the `process_map` (canonical fid order,
  labels) and the parsed `offset_coef_parameter` mask — no new bookkeeping in
  the spec object.
- **Consumers (joint specs only)**: `abmcem`'s
  `set_algorithm_em(initial_parameters=)` (always a joint spec) accepts **only** a
  `parameters.goldfish`; `process-simulation`'s `simulate(coef=)` accepts it
  **when the input is a `joint_specification.goldfish`** and asserts completeness.
  This forces the object **nowhere else**: the single-process estimators
  (`estimate_dynam()` / `estimate_rem()`) keep their plain numeric
  `initial_parameters` and `offset_coef`, and single-spec `simulate()` keeps a
  numeric `coef` — a one-fid spec is already unambiguous. Both changes carry a
  one-line pointer to this surface.
- **Sequencing**: lands **before** `abmcem`'s `estimate_dynes()` surface and
  before `process-simulation` wires `coef`. It has no dependency on either — it
  is a pure projection over the archived `make-multivariate-spec` object, so it
  is implementable now.
- **No baseline impact**: additive surface only; the frozen coefficient
  baselines and every event-stream estimator are untouched.

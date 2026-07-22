## ADDED Requirements

### Requirement: The modeled layer drives focal resolution and info focal is optional

The resolved dependent layer SHALL be the focal layer for every side, mode, and layer-map lookup during preprocessing and estimation, not `info$focal` read independently by each data source. The dependent is resolved from the formula's left-hand side, `make_specification`'s `layer`, or `info$focal` as the fallback default. Consequently `info$focal` SHALL be an optional default: a `stocnet` that
names its dependent through the formula or `layer` SHALL estimate whether or not
`info$focal` is set, for one-mode and two-mode objects alike. Where the resolved
dependent and `info$focal` disagree, side and mode resolution SHALL follow the
**resolved dependent** (the layer being modeled). An absent or empty focal name
reaching a layer-map lookup SHALL resolve to "no layer" rather than aborting.
When no dependent can be resolved at all — empty LHS, no `layer`, and no
`info$focal` — estimation SHALL abort with a clear message naming the missing
declaration, never with an internal indexing error.

#### Scenario: two-mode object estimates with `info$focal` unset

- **WHEN** a hand-built two-mode `stocnet` omits `info$focal` and a rate model
  names the dependent through the formula LHS (`support ~ 1 + outdeg(support)`)
- **THEN** the model estimates without error, resolving the focal sides against
  the `support` layer

#### Scenario: one-mode object estimates with `info$focal` unset

- **WHEN** a hand-built one-mode `stocnet` omits `info$focal` and the formula
  names the dependent
- **THEN** the model estimates without error

#### Scenario: modeling a layer other than `info$focal`

- **WHEN** `info$focal` is set to one layer but the formula/`layer` names a
  different layer as the dependent
- **THEN** every side and mode lookup resolves against the modeled layer, and a
  two-mode effect's side-validity is checked against the modeled layer's mode
  pair

#### Scenario: no dependent resolvable is a clear error

- **WHEN** the formula LHS is empty, no `layer` is given, and `info$focal` is
  unset
- **THEN** estimation aborts with a message directing the user to name the
  dependent, not with an internal `get1index` / indexing error

#### Scenario: frozen baselines unaffected

- **WHEN** an object already sets `info$focal` to the layer it models (every
  prebuilt dataset and frozen baseline)
- **THEN** focal resolution is unchanged and the 1e-6 coefficient baselines
  remain PASS under `NOT_CRAN=true`

## ADDED Requirements

### Requirement: estimate_dynes estimates flavored models from panel waves

The package SHALL provide `estimate_dynes()` accepting a flavored
`specification.goldfish` (per the `flavored-processes` capability) whose focal layer
carries the panel-semantics flag, and an `algorithm` control object from
`set_algorithm_abem()`. It SHALL estimate the per-flavor parameters by ascent-based
Monte Carlo EM: per iteration, augment/refresh a pool of endpoint-hitting sequences for
every between-wave interval, evaluate E-step quantities on the pool at the current
parameters, and take an ascent step, until the convergence criterion or `max_iter` is
reached. `estimate_dynes()` SHALL be marked experimental
(`lifecycle::badge("experimental")`).

#### Scenario: two-wave friendship model estimates
- **WHEN** `estimate_dynes(spec, algorithm = set_algorithm_abem(augmentation =
  "random", n_sequences = 100))` runs on a two-wave panel friendship layer with
  creation/dissolution formulas
- **THEN** estimation returns per-flavor parameter estimates with convergence
  diagnostics, without requiring observed event times.

#### Scenario: event-stream specification rejected
- **WHEN** `estimate_dynes()` receives a specification whose focal layer is an
  event-stream layer
- **THEN** it aborts pointing to `estimate_dynam()`/`estimate_rem()` for fully observed
  sequences.

### Requirement: set_algorithm_abem controls the ABEM variants

The package SHALL provide `set_algorithm_abem()` — aligned with the `set_*_opt()`
control-object family — selecting the augmentation variant (`random`, `model`,
`mcmc`), pool size (`n_sequences`), importance sampling and resampling toggles, batch
size, `max_iter`, `tolerance`, and `seed`. Invalid combinations SHALL abort with cli
errors naming the valid options. The returned control object SHALL select the
augmenter, evaluator, and optimizer step implementations through their contracts, so
new variants extend the options without modifying `estimate_dynes()`.

#### Scenario: control object selects the steps
- **WHEN** `set_algorithm_abem(augmentation = "mcmc", importance_sampling = TRUE)` is
  passed to `estimate_dynes()`
- **THEN** the ABEM loop runs the mutation augmenter with importance-weighted E-steps,
  with no branching inside `estimate_dynes()` beyond contract dispatch.

#### Scenario: invalid option aborts
- **WHEN** `set_algorithm_abem(augmentation = "exact")` is called
- **THEN** a cli error lists the valid augmentation variants.

### Requirement: ABEM results carry Fisher-based vcov and Monte-Carlo error

The `estimate_dynes()` result SHALL contain the parameter estimates, the
Fisher-information approximation accumulated at convergence (inverted by `vcov()`,
with NA rows/columns for fixed parameters), Monte-Carlo standard errors, and
convergence diagnostics (iterations run, ascent trajectory, and pool effective sample
size under importance weights). `summary()` SHALL display asymptotic and Monte-Carlo
error side by side, and `print()`/`summary()` SHALL render via cli semantic elements,
stable under a pinned cli context for snapshot tests.

#### Scenario: vcov from the Fisher approximation
- **WHEN** `vcov()` is called on a converged `estimate_dynes()` result
- **THEN** it returns the inverse of the accumulated Fisher approximation with
  parameter names, NA-padded for fixed parameters.

#### Scenario: MC error is visible
- **WHEN** `summary()` renders a DyNES result estimated with a small pool
- **THEN** the Monte-Carlo standard errors appear alongside the asymptotic standard
  errors, making pool-limited precision visible.

### Requirement: Parameter recovery on simulated panels

The change SHALL include a simulation-based validation: data simulated from known
creation/dissolution parameters, observed only at wave snapshots, re-estimated by
`estimate_dynes()`, with recovery within pre-registered tolerance bounds. A reduced
deterministic-seed variant SHALL run in the test suite (skip_on_cran), and the full
study — including the identifiability assessment of separate creation/dissolution
parameters from waves — SHALL be recorded with the change.

#### Scenario: known parameters recovered
- **WHEN** the seeded recovery test runs on a small simulated two-wave panel
- **THEN** estimates fall within the pre-registered bounds of the generating
  parameters.

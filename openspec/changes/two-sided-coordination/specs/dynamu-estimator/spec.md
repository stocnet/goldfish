# dynamu-estimator Delta Specification

## ADDED Requirements

### Requirement: estimate_dynamu is the two-sided estimator surface
`estimate_dynamu()` SHALL estimate undirected two-sided relational event
models from a positional choice formula, an optional named `rate` formula, an
optional named `acceptance` formula, a `mechanism` argument with values
`c("conjunctive", "forcing", "confirmation", "disjunctive", "compensatory")`
(default `"conjunctive"`), and a `data` object. The function SHALL have no
`sub_model` and no `distribution` argument: timing is the Cox partial
likelihood over marks, always.

#### Scenario: constant-rate one-liner
- **WHEN** `estimate_dynamu(events ~ shared_partners, data = d)` is called
- **THEN** the conjunctive model is estimated under constant rates from the
  mark likelihood, with only choice parameters in the fit

#### Scenario: mechanism selects the variant
- **WHEN** `estimate_dynamu(events ~ shared_partners, mechanism =
  "disjunctive", data = d)` is called
- **THEN** the disjunctive mark likelihood is maximized and the fit records
  the mechanism

#### Scenario: no distribution argument exists
- **WHEN** `estimate_dynamu(events ~ shared_partners, distribution = "weibull",
  data = d)` is called
- **THEN** the call fails as an unused/unknown argument (the parameter is not
  part of the signature)

### Requirement: The acceptance formula is gated to the confirmation mechanism
`estimate_dynamu()` SHALL abort with a cli error when `acceptance` is supplied
with any mechanism other than `"confirmation"` (inert arguments are signaled,
not dropped), and SHALL abort when `mechanism = "confirmation"` is requested
without an `acceptance` formula.

#### Scenario: acceptance with a non-confirmation mechanism aborts
- **WHEN** `estimate_dynamu(events ~ inertia, acceptance = ~ ego(status),
  mechanism = "forcing", data = d)` is called
- **THEN** a cli error states the acceptance model belongs to the
  confirmation mechanism only

#### Scenario: confirmation without acceptance aborts
- **WHEN** `estimate_dynamu(events ~ inertia, mechanism = "confirmation",
  data = d)` is called
- **THEN** a cli error asks for the `acceptance` formula

### Requirement: The conjunctive default reproduces choice_coordination
The conjunctive constant-rate fit SHALL reproduce the coefficients, standard
errors, and log-likelihood of the pre-change `estimate_dynam(sub_model =
"choice_coordination")` fit to within 1e-6 on the same data and effects,
making today's published model the default of `estimate_dynamu()`.

#### Scenario: bridge to the published model
- **WHEN** a model previously fitted as `choice_coordination` is fitted with
  `estimate_dynamu(events ~ <same effects>, data = d)`
- **THEN** coefficients agree to within 1e-6

### Requirement: The fit is one joint object with labeled parameter blocks
The `estimate_dynamu()` result SHALL be a single fitted object whose
coefficients are labeled by part (rate / choice / acceptance), with `coef()`,
`vcov()` (full joint information), `logLik()`, and a cli-rendered
`summary()`/`print()` presenting the blocks separately, and the mechanism
stated in the printed header.

#### Scenario: joint fit exposes all blocks
- **WHEN** `estimate_dynamu(events ~ alter(status), rate = ~ 1 + indeg,
  acceptance = ~ ego(status), mechanism = "confirmation", data = d)` completes
- **THEN** `coef(fit)` contains rate, choice, and acceptance parameters with
  block labels and `vcov(fit)` is the inverse joint information across all
  blocks

#### Scenario: summary output is deterministic for snapshots
- **WHEN** `summary(fit)` prints under a pinned cli context
- **THEN** the output snapshot shows the mechanism and per-block coefficient
  tables

### Requirement: estimate_dynamu accepts a DyNAMu specification
`estimate_dynamu()` SHALL accept a `make_specification(model = "DyNAMu")`
object in place of its formula surface — the direct formula path stays simple
(single formulas per part), and flavored coordination goes through the
specification. `estimate_dynam()` SHALL redirect a DyNAMu specification to
`estimate_dynamu()` with a `cli` error naming it, the same pattern as the
event-stream estimators rejecting a joint specification by naming
`estimate_dynes()`.

#### Scenario: estimate from a DyNAMu specification equals estimate from a formula
- **WHEN** the same coordination model is fitted via
  `estimate_dynamu(make_specification(choice = ~ inertia, model = "DyNAMu",
  mechanism = "forcing", layer = "collab", data = d))` and via the direct
  formula surface
- **THEN** the two fits agree exactly

#### Scenario: the direct path refuses flavored lists
- **WHEN** `estimate_dynamu(list(creation ~ inertia), data = d)` is called
  with a flavor-keyed list on the direct surface
- **THEN** a `cli` error directs the caller to
  `make_specification(model = "DyNAMu")` for flavored coordination

#### Scenario: estimate_dynam redirects a DyNAMu specification
- **WHEN** `estimate_dynam(spec)` is called with a
  `make_specification(model = "DyNAMu")` object
- **THEN** a `cli` error names `estimate_dynamu()` as the estimator for it

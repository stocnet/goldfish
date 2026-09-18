## ADDED Requirements

### Requirement: Every multi-process surface renders through one shared renderer

The package SHALL print a flavored `goldfishSpec`, a `goldfishJointSpec`,
a `goldfishFlavFit` and its `goldfishSummFlavFit` summary through one
internal renderer that takes a header, an ordered list of processes and a
footer. Each process SHALL carry its `fid`, `layer`, `flavor`, `family`, the label
rendered by the same function every data method uses
(`layer › flavor › family`), and a caller-supplied content block. The
renderer SHALL emit the object header, one section per layer when more
than one layer is present, one per flavor beneath it and one per family
beneath that, and SHALL print the process label with its fid on every
surface. The callers SHALL differ only in their content blocks: formulas
for a specification, formulas plus the separability annotation for a joint
specification, the estimate table for a fitted container, the coefficient
table with its convergence line for a summary.

#### Scenario: the printed label is the data label
- **WHEN** a flavored fit is printed and `coef(fit)` is called
- **THEN** every section label in the printout equals a name of
  `coef(fit)`, and the fid beside it equals the `fid` column of
  `coef_layout(fit)` for that block

#### Scenario: one renderer, three contents
- **WHEN** the flavored specification, the joint specification composed
  from it and the container fitted from it are printed
- **THEN** the three outputs share the header idiom, section order and
  process labels, and differ only in each section's content

### Requirement: One process order on every surface

Processes SHALL be ordered flavor-major in fid order — specification, then
flavor, then family — on every surface that lists them: the printers,
`coef()`, `vcov()`, `confint()`, `nobs()`, `summary()`, `tidy()`,
`glance()`, `coef_layout()`, `goldfishParams`,
`margin_table()` and the `test_*`/`diagnose_*` fan-outs. The flavored
container SHALL expose the planner's fids without re-keying them, so
position and key agree everywhere.

#### Scenario: position and key agree
- **WHEN** `coef(fit)` (flat) and `coef_layout(fit)` are compared for a
  two-flavor DyNAM
- **THEN** the i-th free coefficient of the flat vector is the i-th
  `index` row of the layout, and the layout's fids run 1, 2, 3, 4 for
  creation-rate, creation-choice, dissolution-rate, dissolution-choice

### Requirement: The joint specification shows a Dependent block per layer

A `goldfishJointSpec` print SHALL render, inside each layer section, the
Dependent block the single specification shows — events, time span,
sender and receiver node sets, and the network when present — using the
same block renderer, and SHALL NOT render a join-level Dependent block.

#### Scenario: two composed layers
- **WHEN** a joint specification over layers `calls` and `friendship` is
  printed
- **THEN** each layer section opens with that layer's event count, time
  span and node sets before its flavor sections

### Requirement: The container summary prints once as one model

`summary()` on a `goldfishFlavFit` SHALL return a named list of
`goldfishSummFit` objects classed `goldfishSummFlavFit`, unwrapped to the
single object when `flavor =` selects one process. Its print SHALL render
the joint header once, one coefficient table per process with that
process's convergence line as its content, and the joint log-likelihood,
free-parameter count and AIC once in the footer, worded as taken over the
separable processes; BIC, AICc and per-process information criteria SHALL
NOT be printed by the container (they remain available through the
generics and through `glance()`). `glance()` on the container
SHALL return one row per process in the canonical order with the identity
columns appended, and SHALL NOT return a pooled row.

#### Scenario: one model, four tables
- **WHEN** `summary(fit)` on a two-flavor DyNAM is printed
- **THEN** the output has one header, four coefficient tables labeled by
  process, and one footer with one log-likelihood, one parameter count and
  one AIC, and no BIC

#### Scenario: the list contract is unchanged
- **WHEN** `summary(fit)` is inspected
- **THEN** it is a list named by process label whose elements are
  `goldfishSummFit`, and `summary(fit, flavor = "creation")` on a
  one-family flavor is the bare `goldfishSummFit`

### Requirement: A printed section spells the flat coefficient name

Every rendered process section SHALL print the fid that prefixes that
block's flat `coef()` names, and the rows inside the block SHALL be labeled
by the bare short names, so a reader derives `f<fid>_<short>` from the
printout without consulting a lookup. The summary's coefficient table SHALL
label rows by the compact term strings, as on a single fit.

#### Scenario: the name is derivable
- **WHEN** a flavored container prints a section headed by fid 2 with a row
  `inertia`
- **THEN** `coef(fit)[["f2_inertia"]]` is that row's estimate and
  `coef(fit, process = <that section's label>)[["inertia"]]` is the same
  number

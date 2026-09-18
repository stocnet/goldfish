## ADDED Requirements

### Requirement: The joint specification prints through the shared renderer with a Dependent block per layer

`print()` for a `goldfishJointSpec` SHALL render through the shared
multi-process renderer: one section per composed layer opening with that
layer's Dependent block (events, time span, sender and receiver node sets,
network when present), then one section per flavor and one per family
beneath it, each labeled with the process label every data method uses and
its fid, in the map's flavor-major fid order. Each process's content SHALL be
its formulas plus the separability annotation (`separable`, `coupled`, or
`completed` for an autocompleted default). There SHALL be no join-level
Dependent block. `coef_layout()` on a joint specification, a
`goldfishParams` and a fitted result SHALL list rows in that same fid
order, so a printed section and a layout block are found by the same fid.

#### Scenario: layer sections carry their dependent data
- **WHEN** a joint specification composed from two single-layer
  specifications is printed
- **THEN** each layer section shows the layer's event count, time span and
  node sets before its flavor sections, and no Dependent block appears above
  the layers

#### Scenario: the layout order is the printed order
- **WHEN** `coef_layout(joint_spec)` is compared with `print(joint_spec)`
- **THEN** the sequence of distinct fids in the layout equals the sequence
  of fids printed

## MODIFIED Requirements

### Requirement: coef_layout renders the parameter layout for spec, parameters, and result

The package SHALL provide a `coef_layout()` generic returning **one row per
coefficient-space slot** (length `n_params` — the intercept when present, the
effects, and the interaction columns; not merely one row per effect) with the
`fid`, the process **label** (the rendered `layer › flavor › family`
form, keyed on `family`), the `sub_model` (the finer estimation variant, e.g.
`choice_coordination` / `rate_ordered`, as a descriptive column), the `flavor`,
the effect **name** (mirroring what `coef()` surfaces via
`coefficient_term_labels()`: `"Intercept"` for a rate intercept, the interaction's
label for an interaction row; `"1"` as the placeholder for an autocompleted-default
slot with no `coef()` name; and no intercept row where `estimate_dynam()` surfaces
none), a `fixed` logical (true for `offset()`, operand-only, and
autocompleted-default rows), the **fixed value** for fixed rows (the offset value,
`0` for an operand-only term, or the frozen value for an autocompleted default;
`NA` for free rows), and the `index`
of free effects in the flat parameter vector (`NA` for fixed rows). It SHALL
dispatch on a `goldfishJointSpec` (the empty layout, all values `NA`,
for authoring `set_parameters()`), on a flavored or single-process
`goldfishSpec` (the same empty layout over that specification's own
`process_map`), on a `goldfishParams` (layout plus
supplied values and free/fixed classification), and on every fitted
result — a single-process `goldfishFit` (one block), a flavored container,
and a joint/DyNES result (layout plus estimates and standard errors). The
layout SHALL carry a `coef_name` column holding the flat `coef()` name of
the slot (`f<fid>_<short>` on a multi-process object, the bare short name
on a single-process one), so a row joins the flat vector by name as well
as by `index`; the term's compact string, export form and effect-detail
columns, so it is also the term lookup; and a `pattern` argument filtering
rows by any of those spellings. A single-process fit's `process` label is
`<layer> › <family>`, read from the dependent layer name the fit stores at
estimation. The `joint_specification`
method SHALL be **completion-aware**: on a **raw** (authored) spec it spans the
authored fids only; on a **completed** spec (the output of
`complete_generative_spec()`, still a `goldfishJointSpec`, distinguished
by its populated `completed` column) it SHALL additionally render each
**autocompleted-default** fid's rows as `fixed = TRUE` with the `"1"` placeholder
name and the frozen value — the full pre-fit walked layout. The
`goldfishParams` layout SHALL span the **authored** fids only (it is built
from the raw spec). Autocompleted-default rows therefore appear on the **completed
joint spec** and **fitted-result** layouts, never on the raw-spec authoring layout
nor the `goldfishParams` layout. `coef()` and `vcov()` on
every result SHALL be flat — a named vector and matrix over the free
parameters in fid order, named `f<fid>_<short>` on a multi-process result
and by the bare short name on a single-process one — with the
multivariate grouping supplied by `coef_layout()` for rendering rather than
by changing the generics, and a `process =` selector returning one block
with bare short names.

`set_parameters()` SHALL accept a `goldfishJointSpec`, a flavored
`goldfishSpec` and a single-process `goldfishSpec`, each carrying its
`process_map` from construction. `goldfishParams` SHALL be required by
`estimate_dynes()` and accepted by `simulate()` on any specification kind
and by the `initial_parameters` argument of every estimator, single or
flavored, where a numeric vector or a flavor-keyed list remains accepted as
a convenience that builds one; a value at a free slot is a warm start,
`NA` is a free slot with no start. Fixing a coefficient SHALL remain a
specification concern (`offset(term, coef = )`, `offset_coef`), never an
argument of `set_parameters()`.

#### Scenario: empty layout guides authoring
- **WHEN** `coef_layout()` is called on a raw `goldfishJointSpec`
- **THEN** it returns one row per authored-fid coefficient slot with labels,
  names, formula order, and the fixed flag, values `NA` for free slots, and no
  autocompleted-default rows, so a user can author `set_parameters()`.

#### Scenario: completed-spec layout previews the full walked layout
- **WHEN** `coef_layout()` is called on a **completed** spec (e.g.
  `coef_layout(complete_generative_spec(spec))`), whose `process_map` carries the
  autocompleted fids and a populated `completed` column
- **THEN** it returns the authored rows **plus** one row per autocompleted-default
  slot, each marked `fixed = TRUE` with its frozen value, so a user can preview the
  full layout `simulate()` will walk before fitting.

#### Scenario: result layout groups the flat coefficients
- **WHEN** `summary()` renders a joint/DyNES fitted result
- **THEN** it uses `coef_layout()` to group the flat coefficient vector into
  per-process blocks, while `coef()` still returns the flat named vector.

#### Scenario: numeric inputs remain accepted
- **WHEN** `estimate_dynam()` is called with a numeric `initial_parameters`, or
  `simulate()` is called on a single `goldfishSpec` with a numeric
  `coef`
- **THEN** both proceed, building the equivalent `goldfishParams` internally.

#### Scenario: a single-process specification takes a parameter object
- **WHEN** `set_parameters(spec, `calls › rate` = c(-8, 0.3))` is called on a
  single-process `goldfishSpec` and the result is passed as
  `initial_parameters` to `estimate_dynam()` or as the coefficients to
  `simulate()`
- **THEN** the estimator warm-starts from those values and the simulation
  walks them, with the same value gate the joint path applies.

#### Scenario: a single-process fit has a one-block layout
- **WHEN** `coef_layout()` is called on a `goldfishFit`
- **THEN** it returns one block whose `fid` is the fit's single process, one
  row per coefficient slot, labeled `<layer> › <family>` from the layer the
  fit stores, with `coef_name` equal to the bare `coef()` name.

#### Scenario: fit values round-trip back into a parameter object
- **WHEN** a joint/DyNES fitted result is passed to `set_parameters()` (the
  from-result form)
- **THEN** the per-fid vectors are reconstructed from the result's `coef_layout()`
  (which keeps the `fid` grouping the flat `coef()` vector discards), the values
  land at the same fids and slots they were estimated at — the shared canonical
  order (`process_map` fid order, then coefficient order within each fid) — and a
  complete `goldfishParams` a `simulate()` can drive is returned. A flat,
  free-only `coef()` vector is also accepted, matched by its `f<fid>_<short>`
  names, since those no longer collide across fids.

## RENAMED Requirements

- FROM: `### Requirement: make_joint_specification composes process specifications`
- TO: `### Requirement: make_joint_specification composes process specifications into a goldfishJointSpec object`

## MODIFIED Requirements

### Requirement: make_joint_specification composes process specifications into a goldfishJointSpec object

The package SHALL export `make_joint_specification(...)` accepting two or more
`make_specification()` objects over one shared data object and returning a
`goldfishJointSpec` object — a multivariate specification that portrays their
co-evolution (design D17 — renamed from `joint_specification.goldfish` under
the retired `<noun>.goldfish` house convention). Construction SHALL NOT
require a panel-observed layer to be referenced — all viability is consumer-owned. A
combination that references no panel-observed layer SHALL compose: it is
estimation-separable (the factorized likelihood) yet generatively coupled through the
shared clock, hence a valid `simulate()` input with no other constructor;
`estimate_dynes()` (not construction) SHALL abort such an all-separable spec, naming
`estimate_dynam()`. Construction MAY emit a separability note but SHALL NOT abort on
it. DyNAM-i processes SHALL be rejected. All processes
MUST reference one shared mode-map object; one- and two-mode processes MAY be
composed, and dependent processes over distinct mode-pairs MAY be joined
provided every cross-process read conforms by mode-set identity (see the
node-space conformance requirement below). DyNAM (rate, choice,
choice_coordination) and REM processes, timed or ordered, MAY be freely mixed,
flavored or plain.

#### Scenario: panel plus relational processes compose
- **WHEN** `make_joint_specification(friendship_spec, calls_spec, data = x)` runs
  with friendship panel-observed (flavored creation/dissolution) and calls a
  fully observed relational-event process
- **THEN** a `goldfishJointSpec` is returned covering both processes'
  formulas.

#### Scenario: exogenous-only panel reference composes but is not DyNES-viable
- **WHEN** no composed process's focal layer is panel-observed, but a
  relational-event process reads a panel-observed layer as an exogenous covariate
  (e.g. `calls ~ ... + tie(friendship)` with friendship panel-observed)
- **THEN** a `goldfishJointSpec` is returned — the panel layer enters as a
  static exogenous step-covariate — but no fid is coupled, so `estimate_dynes()`
  will abort on it, naming `estimate_dynam()` (nothing is latent).

#### Scenario: no panel reference composes (estimation-separable, generatively simulable)
- **WHEN** no composed process references any panel-observed layer (focal or
  exogenous)
- **THEN** a `goldfishJointSpec` is returned — the processes are
  estimation-separable but generatively coupled through the shared clock, so it is a
  valid `simulate()` input — and `estimate_dynes()` (not construction) aborts it,
  naming `estimate_dynam()` for per-process estimation.

## ADDED Requirements

### Requirement: set_init_param builds a validated parameter object over the joint spec

The package SHALL export `set_init_param(spec, ...)` taking a
`goldfishJointSpec` and returning a `goldfishParams` object that
carries parameter values over the specification's fid-indexed parameter vector.
Each `...` argument SHALL be a **full-length per-fid vector** — one entry per
coefficient of that fid, in the fid's **coefficient order**: the intercept
(when the sub-model carries one), then the effects in formula order, then the
interaction columns — the same positions, and the same count, that `coef()`
names for that fid. It SHALL include slots for fixed coefficients (both
`offset()` terms and operand-only interaction terms) — keyed by the **rendered
process label**: the `layer › flavor › family`
form the package already renders for reading (the labels `coef()` / `print()`
surface for flavored results), where `family` names the fid within the process
(`rate` / `choice`) and `flavor` is present only when the process carries one.
Keys SHALL be resolved by **membership** against the set of rendered labels — the
package SHALL NOT split a key back into `(layer, flavor, family)` — so a key that
matches no rendered label, or matches more than one (a name carrying the separator,
or a collision), SHALL abort naming the valid labels. A per-fid vector whose length
does not equal the fid's **coefficient count** (`n_params` — the intercept when
present, plus the effects, plus the interaction columns) SHALL abort naming the
fid and the expected length. Per-effect names SHALL be
**optional but all-or-nothing**: a per-fid vector SHALL be either fully
positional (no names) or fully named. When named, every name SHALL be validated
against the fid's effect labels (a disagreement aborts); when unnamed the vector
SHALL be read positionally; a **partially named** vector (some slots named, some
bare) SHALL be rejected. A process whose key is **omitted** SHALL be read as all-`NA` — every
non-fixed slot of that fid free. The returned object SHALL validate itself once at
construction so consumers accept it without re-validating, and SHALL carry **two
projections** — the flat **free-parameter** vector (fixed slots skipped; what
`estimate_dynes()` estimates) and the **full per-fid coefficient vectors** (every
slot resolved to a value: user pins plus spec-resolved fixed values; what
`simulate()` consumes) — plus the layout metadata. Both projections and `coef()`
on a fitted result SHALL concatenate in one **canonical order**: `process_map` fid
order, then coefficient order within each fid (the intercept when present, then
effects in formula order, then interactions).

`set_init_param()` SHALL also accept, as `set_init_param(spec, result)`, a
**fitted joint/DyNES result** in place of the per-fid vectors (the fit →
re-simulate round-trip), reconstructing the per-fid values from the result's
`coef_layout()` — which retains the `fid` grouping the flat `coef()` vector
discards. It SHALL first assert the result was fit against **that same `spec`**
(matching fid vocabulary / `coef_layout()`) and abort on a mismatch. A flat,
free-only `coef()` vector SHALL NOT be accepted directly, because its
per-parameter names collide across fids.

#### Scenario: per-fid vectors keyed by the rendered label
- **WHEN** `set_init_param(spec, "friendship › rate" = c(0.2, -0.1),
  "friendship › choice" = c(inertia = 0.5))` is called on a spec whose
  `friendship` rate fid has two effects and whose choice fid has an `inertia`
  effect
- **THEN** a `goldfishParams` is returned with those values placed at the
  resolved fids, the named choice entry validated against the effect label.

#### Scenario: flavor is included only when the process carries one
- **WHEN** a layer `calls` carries two flavors and a layer `friendship` carries
  none
- **THEN** the accepted labels are `calls › creation › rate` (flavor present) and
  `friendship › rate` (flavor elided), and a key matching no rendered label (e.g.
  `friendship › create › rate`) aborts naming the valid labels.

#### Scenario: wrong-length per-fid vector aborts
- **WHEN** a fid whose coefficient count (`n_params`) is three receives a
  length-two vector
- **THEN** `set_init_param()` aborts naming the fid and the expected length.

#### Scenario: an omitted process key is all-free
- **WHEN** `set_init_param()` is called without a key for one of the spec's
  processes
- **THEN** every non-fixed slot of that fid is read as free (`NA`); the object is
  partial (usable by `estimate_dynes()`, rejected by `simulate()` unless that
  fid is entirely fixed).

### Requirement: NA disambiguation resolves from the fixed mask with the fixed value prevailing

`set_init_param()` SHALL classify each slot from the **authored** specification's
per-effect **fixed mask** in coefficient order, where a coefficient is fixed if it
is an `offset()` term or an operand-only interaction term (the latter held out of
estimation at `0`). (Autocompleted sub-models — the zero-free-parameter defaults
`complete_generative_spec()` synthesizes at **consumer entry** — are **not present**
in the authored spec `set_init_param()` sees, so they never reach this
classification; they are resolved trivially at the consumer, and surface as fixed
rows only on `coef_layout()`'s completed-spec / fitted-result methods.) For both
fixed kinds the **fixed value SHALL always prevail**: a fixed slot SHALL take the
specification's value (an `offset()` term's value; `0` for an operand-only term)
regardless of the value supplied — an `NA` there is accepted silently, and a
non-`NA` value there SHALL warn once and be ignored. A `NA` at a
**free** slot SHALL denote a **free** parameter (one to be estimated); a non-`NA`
value at a free slot SHALL pin that free parameter. The user SHALL NOT be
required to distinguish fixed from free — the classification is derived entirely
from the specification.

#### Scenario: value at a fixed slot warns and is ignored
- **WHEN** a per-fid vector supplies a non-`NA` value at a slot the formula marks
  `offset(effect, coef = 2)`
- **THEN** `set_init_param()` warns that the fixed value prevails and the object
  carries the offset value, not the supplied one.

#### Scenario: NA at a free slot is a free parameter
- **WHEN** a per-fid vector has `NA` at a free (non-fixed) slot
- **THEN** that slot is classified free and appears in the object's
  free-parameter projection.

### Requirement: a joint specification carries every offset's fixed value at build

For the fixed value that "offset prevails" and `coef_layout()` report to be
available, `make_joint_specification()` SHALL require every `offset()` term of a
joined process to carry an inline `coef = value` and SHALL reject the join
otherwise — failing at **build**, not deferring to estimation. The rejection
message SHALL direct the user to write the value inline
(`offset(term, coef = value)`) and SHALL NOT offer the algorithm-control route
(`offset_coef`), which does not apply to a multi-process specification.

#### Scenario: a bare offset on a joined process aborts at build
- **WHEN** `make_joint_specification()` joins a process whose formula contains a
  bare `offset(term)` with no `coef =`
- **THEN** the build aborts naming the term, directing the user to
  `offset(term, coef = value)`.

#### Scenario: an inline coef on every offset builds
- **WHEN** every `offset()` term of every joined process carries an inline
  `coef = value`
- **THEN** the join succeeds and each fixed value is available to
  `coef_layout()` and to `set_init_param()`'s offset-prevails handling.

### Requirement: goldfishParams carries a complete-vs-partial contract

A `goldfishParams` object is defined over the specification's **authored**
fid set (autocompleted sub-models do not yet exist at authoring time — they are
synthesized at consumer entry — so they are not part of the object). It SHALL
expose whether it is **complete** — complete iff every **free** slot of the
authored fids carries a non-`NA` value (its fixed slots — `offset()` and
operand-only — are always resolved by the specification). `estimate_dynes()` SHALL
treat a partial object's free (`NA`) slots as the parameters to estimate, reading
the fixed values from the joint spec rather than from a full vector. `simulate()`
SHALL require a **full coefficient vector** as its initial parameters — it SHALL
**assert completeness** and abort on any remaining free `NA`, naming the free
effects, mirroring the walk handle's generative-completeness assertion. At consumer
entry, after the spec is completed, any fid **absent from the object** — an
autocompleted zero-free-parameter default the user never keyed — SHALL be treated
as **trivially resolved**: it requires no user value and SHALL NOT render the
object incomplete.

#### Scenario: simulate rejects an incomplete parameter object
- **WHEN** a `goldfishParams` with a free (non-offset `NA`) slot is passed
  to `simulate()`
- **THEN** simulation aborts naming the unfilled free effect(s).

#### Scenario: estimate accepts a partial parameter object
- **WHEN** the same partial object is passed to `estimate_dynes()` as
  `initial_parameters`
- **THEN** estimation proceeds, treating the free slots as parameters to estimate.

### Requirement: coef_layout renders the parameter layout for spec, parameters, and result

The package SHALL provide a `coef_layout()` generic returning **one row per
coefficient-space slot** (length `n_params` — the intercept when present, the
effects, and the interaction columns; not merely one row per effect) with the
`fid`, the process **label** (the rendered `layer › flavor › family`
form, keyed on `family`), the `sub_model` (the finer estimation variant, e.g.
`choice_coordination` / `rate_ordered`, as a descriptive column), the `flavor`,
the effect **name** (the self-describing **console form**
`summary()` renders — every attribute (window, weight, transformer) appears
inline, e.g. `inertia [1h,W]` — so a name authored via `set_init_param()` and a
name read off a fit are the same string; `"Intercept"` for a rate intercept and
an interaction row's `label` render identically whether read as the console
form or the deparse form, so neither is affected by the vocabulary; `"1"` as
the placeholder for an autocompleted-default slot with no such name; and no
intercept row where `estimate_dynam()` surfaces none), a `fixed` logical (true
for `offset()`, operand-only, and
autocompleted-default rows), the **fixed value** for fixed rows (the offset value,
`0` for an operand-only term, or the frozen value for an autocompleted default;
`NA` for free rows), and the `index`
of free effects in the flat parameter vector (`NA` for fixed rows). It SHALL
dispatch on a `goldfishJointSpec` (the empty layout, all values `NA`,
for authoring `set_init_param()`), on a `goldfishParams` (layout plus
supplied values and free/fixed classification), and on a joint/DyNES fitted
result (layout plus estimates and standard errors). The `joint_specification`
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
those results SHALL remain flat — a named vector and matrix over the free
parameters, named by the composite labels — with the multivariate grouping
supplied by `coef_layout()` for rendering rather than by changing the generics.

This surface SHALL be scoped to joint specifications. `set_init_param()` SHALL
require a `goldfishJointSpec`, and `goldfishParams` SHALL be
required only at the joint consumer surfaces (`estimate_dynes()` always;
`simulate()` when its input is a `goldfishJointSpec`). The
single-process estimators and single-specification `simulate()` SHALL be
unchanged, keeping their numeric `initial_parameters` / `coef` / `offset_coef`;
this change SHALL NOT force the object on them nor alter their signatures.

#### Scenario: empty layout guides authoring
- **WHEN** `coef_layout()` is called on a raw `goldfishJointSpec`
- **THEN** it returns one row per authored-fid coefficient slot with labels,
  names, formula order, and the fixed flag, values `NA` for free slots, and no
  autocompleted-default rows, so a user can author `set_init_param()`.

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

#### Scenario: single-process surfaces are unaffected
- **WHEN** `estimate_dynam()` is called with a numeric `initial_parameters`, or
  `simulate()` is called on a single `specification.goldfish` with a numeric
  `coef`
- **THEN** both proceed unchanged, requiring no `goldfishParams`.

#### Scenario: fit values round-trip back into a parameter object
- **WHEN** a joint/DyNES fitted result is passed to `set_init_param()` (the
  from-result form)
- **THEN** the per-fid vectors are reconstructed from the result's `coef_layout()`
  (which keeps the `fid` grouping the flat `coef()` vector discards), the values
  land at the same fids and slots they were estimated at — the shared canonical
  order (`process_map` fid order, then coefficient order within each fid) — and a
  complete `goldfishParams` a `simulate()` can drive is returned. A flat,
  free-only `coef()` vector is **not** accepted directly (its per-parameter names
  collide across fids).

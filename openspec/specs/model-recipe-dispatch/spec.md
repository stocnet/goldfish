# model-recipe-dispatch Specification

## Purpose
Define the internal S3 `goldfishKind` class hierarchy (constructed by `new_model_spec()`) that drives recipe and effect dispatch across all model variants.
## Requirements
### Requirement: S3 goldfishKind class hierarchy
The package SHALL define internal S3 classes for the model variants, and the
class vector SHALL carry only what dispatches. Each spec SHALL be constructed
by the non-exported spec constructor, which SHALL also compute the behavioral
descriptor. The class vector SHALL name the likelihood class and the risk-set
axis; it SHALL NOT encode the `model` and `sub_model` pairing for its own
sake, because preprocessing and the estimation entry point read the descriptor
and the axis rather than a per-variant class. Variants whose likelihood
implementation is identical SHALL share one likelihood class rather than being
given a class each and an alias method.

#### Scenario: Spec class resolves from model and sub_model
- **WHEN** a spec is constructed for a given model and sub-model
- **THEN** it carries the likelihood class its implementation requires and the
  class for its risk-set axis, and its descriptor records the model and
  sub-model as provenance

#### Scenario: two variants sharing an implementation share a class
- **WHEN** specs are constructed for two variants whose likelihood code is the
  same
- **THEN** both carry the same likelihood class, and only one method is
  registered for them

### Requirement: is_two_mode requires both node sets
When `is_two_mode = TRUE`, `new_model_spec()` SHALL require both `nodes` and `nodes2` to be non-NULL and non-identical. `goldfishAxisSender` variants (dynam_rate, dynam_rate_ordered, dynami_rate, dynami_rate_ordered) SHALL force `is_two_mode = FALSE` regardless of input.

#### Scenario: Two-mode spec requires nodes2
- **WHEN** `new_model_spec("DyNAM", "choice", is_two_mode = TRUE, nodes = "actors", nodes2 = NULL, ...)` is called
- **THEN** an informative error is thrown indicating `nodes2` is required for two-mode models

#### Scenario: Sender-indexed specs ignore is_two_mode
- **WHEN** `new_model_spec("DyNAM", "rate", is_two_mode = TRUE, ...)` is called
- **THEN** `spec$is_two_mode` is FALSE and no error or warning is emitted

### Requirement: compute_stats exported preprocessing entry point
The package SHALL export `compute_stats(formula, data, model, sub_model, ...)` which constructs a `goldfishKind` internally and returns a `goldfishStat` object. `new_model_spec()` and `preprocess()` SHALL NOT be exported.

#### Scenario: compute_stats returns goldfishStat
- **WHEN** `compute_stats(formula, data, model = "DyNAM", sub_model = "rate")` is called with valid inputs
- **THEN** the returned object inherits from `"goldfishStat"`

#### Scenario: compute_stats result is usable by estimate_dynam
- **WHEN** the result of `compute_stats(...)` is passed as `preprocessing` argument to `estimate_dynam()`
- **THEN** estimation completes successfully and produces the same coefficients as without pre-computing

### Requirement: preprocess dispatches per recipe with no model-type branches
Preprocessing SHALL select its recipe and that recipe's parameters by reading
the behavioral descriptor, not by dispatching on a per-variant S3 class and
not by branching on `model` or `sub_model`. The recipe selection SHALL follow
the descriptor's risk-set axis, and the parameters that distinguish a timed
rate from an ordinal sub-model SHALL follow the descriptor's `timing` field.

#### Scenario: recipe selection reads the descriptor
- **WHEN** preprocessing runs for any supported model variant
- **THEN** the recipe and its parameters are determined by the descriptor's
  axis and timing fields, and no per-variant preprocess method participates

#### Scenario: variants with identical preprocessing share one path
- **WHEN** preprocessing runs for a choice sub-model and for a coordination
  sub-model
- **THEN** both take the same recipe path with the same parameters, because
  their descriptors agree on axis and timing

### Requirement: global() rejected in choice sub-models
A formula containing a `global()` effect SHALL cause `estimate_dynam()` with `sub_model = "choice"` or `sub_model = "choice_coordination"` (and `compute_stats()` for those sub-models) to call `cli::cli_abort()` before preprocessing begins. The error message SHALL state that global covariates in choice sub-models will be supported only through interaction effects in a future release. Rate sub-models (`DyNAM` rate / rate_ordered, `REM` rate / rate_ordered) SHALL continue to accept `global()`.

#### Scenario: global() in DyNAM-choice aborts
- **WHEN** `estimate_dynam(dep ~ inertia(net) + global(seasons$winter), data, sub_model = "choice")` is called
- **THEN** a `cli::cli_abort()` error mentioning interaction-only future support is raised before any preprocessing occurs

#### Scenario: global() in rate sub-models still works
- **WHEN** `estimate_dynam(dep ~ global(seasons$winter), data, sub_model = "rate")` is called
- **THEN** estimation completes without error

### Requirement: estimate_int dispatches per recipe
`estimate_int()` SHALL be an S3 generic dispatching on the `goldfishKind`
class. The internal model-type strings (`"DyNAM-M"`, `"DyNAM-MM"`,
`"REM"`, `"REM-ordered"`, `"DyNAM-M-Rate"`, `"DyNAM-M-Rate-ordered"`) SHALL
NOT drive branching anywhere in the R pipeline — `R/estimation_core.R`,
`R/cpp_interface.R`, `R/model_estimate.R`, `R/model_preprocess.R`, and
`R/preprocess_writers.R` — including the compiled-interface entry
(`estimate_c_int`) and the gather routines, which SHALL select estimators
and shape arguments via spec-class dispatch or risk-set-descriptor reads.
The `legacy_model_type()` spec-to-string converter SHALL be removed.
Model-type strings MAY appear as values inside exported output or
user-facing documentation, never as branch keys.

#### Scenario: No modelType string in dispatched estimation methods
- **WHEN** `grep -rn '"DyNAM-M"\|"DyNAM-MM"\|"REM-ordered"\|"DyNAM-M-Rate"' R/estimation_core.R` is run after the change
- **THEN** zero matches are returned

#### Scenario: No model-type branching in the compiled interface or writers
- **WHEN** the model-type string literals are searched in
  `R/cpp_interface.R`, `R/model_estimate.R`, and `R/preprocess_writers.R`
  after the change
- **THEN** no match is a branching condition (`if`/`switch`/`%in%` guard);
  any remaining literal is a value in exported output or documentation

#### Scenario: legacy converter removed
- **WHEN** the package sources are searched for `legacy_model_type` after
  the change
- **THEN** the function definition and all call sites are gone

### Requirement: S3 dispatch only at stage boundaries
S3 dispatch on the model spec SHALL occur at most once per pipeline stage: at entry into `preprocess()`, `estimate_int()`, and writer selection in `compute_stats()`. Methods SHALL bind per-event helper functions (the `compute_event_contribution()` and `compute_step()` methods) to local function variables before entering any per-event loop. No generic call dispatched via `UseMethod` SHALL execute inside a per-event loop body in `R/model_preprocess.R`, `R/estimation_core.R`, or `R/cpp_interface.R`.

#### Scenario: Per-event helpers resolved before the loop
- **WHEN** any `estimate_int` method is inspected after the change
- **THEN** the event loop calls a locally bound function variable (resolved once before the loop), and `grep -n "compute_step(spec\|compute_event_contribution(spec" R/estimation_core.R` inside loop bodies returns zero matches

#### Scenario: Engine variants still override behavior
- **WHEN** a future spec class overrides `compute_step()`
- **THEN** the pre-loop binding resolves to the overriding method without any change to the loop body

### Requirement: Recipes maintain data objects in a state container
Recipe methods SHALL maintain all evolving data objects in a named-list state container owned by the recipe function: networks as individual matrices in a named sub-list, nodal attributes as one data frame per node set, and global attributes as a one-row data frame. Object updates SHALL write into this container directly. No `assign()`, `get()`, or `eval(parse(...))` call SHALL execute inside a recipe event loop, and recipe loops SHALL NOT keep a second live reference to a network matrix across an update (single-owner discipline, so writes stay in place under R ≥ 4 reference counting).

#### Scenario: No environment manipulation in recipe loops
- **WHEN** `grep -n "eval(parse\|assign(" R/model_preprocess.R` is run after the recipes replace the monolith
- **THEN** zero matches fall inside recipe event-loop bodies

#### Scenario: Network update is observed by subsequent effect calls
- **WHEN** a network event updates `state$networks[[k]]` and a later event's effect reads the same network
- **THEN** the effect receives the updated matrix, and preprocessing output equals the pre-refactor monolith's output for the same model

### Requirement: rate_ordered as explicit sub_model for estimate_dynam
`estimate_dynam()` SHALL accept `sub_model = "rate_ordered"` as a valid value meaning the partial likelihood / CoxPH model (no time intercept). `sub_model = "rate"` with a no-intercept formula SHALL emit a `cli::cli_warn()` deprecation warning and behave as `"rate_ordered"`.

#### Scenario: rate_ordered sub_model is accepted
- **WHEN** `estimate_dynam(formula, data, sub_model = "rate_ordered")` is called
- **THEN** estimation completes and the result class includes `"dynam_rate_ordered"`

#### Scenario: Old no-intercept rate pattern warns
- **WHEN** `estimate_dynam(~ 0 + inertia(net), data, sub_model = "rate")` is called
- **THEN** a deprecation warning is emitted suggesting `sub_model = "rate_ordered"`

### Requirement: estimate_rem sub_model values
`estimate_rem()` SHALL accept `sub_model = "rate"` (full dyadic hazard model with intercept) and `sub_model = "rate_ordered"` (CoxPH / partial likelihood). `sub_model = "choice"` SHALL remain valid but emit a deprecation warning suggesting `"rate"`.

#### Scenario: REM rate sub_model is accepted
- **WHEN** `estimate_rem(formula, data, sub_model = "rate")` is called
- **THEN** estimation completes and the result corresponds to a full dyadic rate model

#### Scenario: REM choice alias warns
- **WHEN** `estimate_rem(formula, data, sub_model = "choice")` is called
- **THEN** a deprecation warning is emitted suggesting `sub_model = "rate"`

### Requirement: DyNAMi spec participates in dispatch, delegates to existing loop
A DyNAM-i spec SHALL participate in dispatch through the same descriptor as
every other spec, and its difference SHALL be carried by the descriptor's
`input_shape` field rather than by a per-variant class. Its likelihood SHALL
be the same class as the corresponding DyNAM variant, since the
implementations are identical, and no alias method SHALL be registered for it.
Its preprocessing SHALL continue to delegate to the group-interaction loop for
as long as that difference is real.

#### Scenario: a DyNAM-i spec shares the DyNAM likelihood
- **WHEN** the likelihood is computed for a DyNAM-i rate spec
- **THEN** it dispatches to the same method a DyNAM rate spec dispatches to,
  with no alias registered

#### Scenario: the grouped input shape is visible on the descriptor
- **WHEN** a DyNAM-i spec is constructed
- **THEN** its descriptor records the grouped input shape, and preprocessing
  reads that field to reach the group-interaction loop


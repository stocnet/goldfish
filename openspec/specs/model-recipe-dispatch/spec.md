# model-recipe-dispatch Specification

## Purpose
Define the internal S3 `model_spec` class hierarchy (constructed by `new_model_spec()`) that drives recipe and effect dispatch across all model variants.

## Requirements

### Requirement: S3 model_spec class hierarchy
The package SHALL define 9 internal S3 classes covering all model variants. Each class SHALL be constructed by `new_model_spec(model, sub_model, is_two_mode, nodes, nodes2, ...)` which is NOT exported. The class vector SHALL follow the pattern `c("<variant>_spec", "<indexing>_spec", "model_spec")`.

#### Scenario: Spec class resolves from model and sub_model
- **WHEN** `new_model_spec("DyNAM", "rate", is_two_mode = FALSE, ...)` is called
- **THEN** the returned object has `class(spec)[1] == "dynam_rate_spec"`

#### Scenario: Sender-indexed spec class is correct
- **WHEN** `new_model_spec("DyNAM", "rate_ordered", ...)` is called
- **THEN** `inherits(spec, "sender_spec")` is TRUE and `inherits(spec, "dyad_spec")` is FALSE

#### Scenario: Dyad-indexed spec class is correct
- **WHEN** `new_model_spec("REM", "rate", ...)` is called
- **THEN** `inherits(spec, "dyad_spec")` is TRUE and `inherits(spec, "sender_spec")` is FALSE

#### Scenario: All 9 valid variant combinations are accepted
- **WHEN** `new_model_spec()` is called with each of the 9 valid (model, sub_model) combinations
- **THEN** a `model_spec` object is returned without error for each

### Requirement: is_two_mode requires both node sets
When `is_two_mode = TRUE`, `new_model_spec()` SHALL require both `nodes` and `nodes2` to be non-NULL and non-identical. `sender_spec` variants (dynam_rate, dynam_rate_ordered, dynami_rate, dynami_rate_ordered) SHALL force `is_two_mode = FALSE` regardless of input.

#### Scenario: Two-mode spec requires nodes2
- **WHEN** `new_model_spec("DyNAM", "choice", is_two_mode = TRUE, nodes = "actors", nodes2 = NULL, ...)` is called
- **THEN** an informative error is thrown indicating `nodes2` is required for two-mode models

#### Scenario: Sender-indexed specs ignore is_two_mode
- **WHEN** `new_model_spec("DyNAM", "rate", is_two_mode = TRUE, ...)` is called
- **THEN** `spec$is_two_mode` is FALSE and no error or warning is emitted

### Requirement: compute_stats exported preprocessing entry point
The package SHALL export `compute_stats(formula, data, model, sub_model, ...)` which constructs a `model_spec` internally and returns a `preprocessed.goldfish` object. `new_model_spec()` and `preprocess()` SHALL NOT be exported.

#### Scenario: compute_stats returns preprocessed.goldfish
- **WHEN** `compute_stats(formula, data, model = "DyNAM", sub_model = "rate")` is called with valid inputs
- **THEN** the returned object inherits from `"preprocessed.goldfish"`

#### Scenario: compute_stats result is usable by estimate_dynam
- **WHEN** the result of `compute_stats(...)` is passed as `preprocessing` argument to `estimate_dynam()`
- **THEN** estimation completes successfully and produces the same coefficients as without pre-computing

### Requirement: preprocess dispatches per recipe with no model-type branches
`preprocess()` SHALL be an S3 generic. Each concrete `model_spec` class SHALL have a dedicated `preprocess.<class>()` method. Concrete methods MAY be thin configurations delegating to shared unexported family kernels (`run_sender_recipe_loop()`, `run_dyad_recipe_loop()`); no `preprocess.sender_spec()` / `preprocess.dyad_spec()` S3 fallback methods SHALL be defined. No recipe method or kernel SHALL contain a runtime branch on `model`, `sub_model`, or `modelType` strings inside its event loop; constant boolean configuration knobs (e.g. `right_censored`) evaluated on loop-invariant values are permitted.

#### Scenario: Dispatched recipe produces correct initialStats shape for sender model
- **WHEN** `preprocess(spec)` is called with a `dynam_rate_spec` where `n1=10`, `nEffects=3`
- **THEN** `dim(result$initialStats)` is `c(10L, 3L)`

#### Scenario: Dispatched recipe produces correct initialStats shape for dyad model
- **WHEN** `preprocess(spec)` is called with a `dynam_choice_spec` where `n1=10`, `n2=10`, `nEffects=3`
- **THEN** `dim(result$initialStats)` is `c(100L, 3L)`

#### Scenario: Rate recipe computes intercept scalars; ordered recipe does not
- **WHEN** `preprocess(spec)` is called with a `dynam_rate_spec`
- **THEN** `result$avg_active_actors` is a positive numeric

- **WHEN** `preprocess(spec)` is called with a `dynam_rate_ordered_spec`
- **THEN** `result$avg_active_actors` is NULL

### Requirement: global() rejected in choice sub-models
A formula containing a `global()` effect SHALL cause `estimate_dynam()` with `sub_model = "choice"` or `sub_model = "choice_coordination"` (and `compute_stats()` for those sub-models) to call `cli::cli_abort()` before preprocessing begins. The error message SHALL state that global covariates in choice sub-models will be supported only through interaction effects in a future release. Rate sub-models (`DyNAM` rate / rate_ordered, `REM` rate / rate_ordered) SHALL continue to accept `global()`.

#### Scenario: global() in DyNAM-choice aborts
- **WHEN** `estimate_dynam(dep ~ inertia(net) + global(seasons$winter), data, sub_model = "choice")` is called
- **THEN** a `cli::cli_abort()` error mentioning interaction-only future support is raised before any preprocessing occurs

#### Scenario: global() in rate sub-models still works
- **WHEN** `estimate_dynam(dep ~ global(seasons$winter), data, sub_model = "rate")` is called
- **THEN** estimation completes without error

### Requirement: estimate_int dispatches per recipe
`estimate_int()` SHALL be an S3 generic dispatching on the `model_spec` class. The internal `modelType` string (`"DyNAM-M"`, `"DyNAM-MM"`, etc.) SHALL NOT appear in any method after this change.

#### Scenario: No modelType string in dispatched estimation methods
- **WHEN** `grep -rn '"DyNAM-M"\|"DyNAM-MM"\|"REM-ordered"\|"DyNAM-M-Rate"' R/estimation_core.R` is run after the change
- **THEN** zero matches are returned

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
`dynami_rate_spec` and `dynami_choice_spec` SHALL exist as valid spec classes. Their `preprocess()` methods SHALL delegate to the existing DyNAMi monolithic preprocessing loop without change. The dedicated DyNAMi recipe (post-event update order) is out of scope.

#### Scenario: estimate_dynami produces a typed spec
- **WHEN** `estimate_dynami(formula, data, sub_model = "rate")` is called
- **THEN** the internally constructed spec satisfies `inherits(spec, "dynami_rate_spec")`

#### Scenario: DyNAMi coefficients are unchanged after dispatch wiring
- **WHEN** DyNAMi model coefficients are compared before and after the dispatch refactor
- **THEN** all coefficients agree to within 1e-6

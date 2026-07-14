## MODIFIED Requirements

### Requirement: estimate_int dispatches per recipe
`estimate_int()` SHALL be an S3 generic dispatching on the `model_spec`
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

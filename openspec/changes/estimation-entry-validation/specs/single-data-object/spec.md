## MODIFIED Requirements

### Requirement: Dual input path with one validate-and-stamp boundary
The package SHALL accept a `stocnet` object (list of `info`, `nodes`, `ties`, `changes`,
`global` components per `manynet::make_stocnet()`) as the data input for DyNAM and REM
estimation through two entries sharing one internal validator: directly via the `data`
argument of `estimate_dynam()` / `estimate_rem()` / `make_specification()`, and via an
exported `as_goldfish(x, ...)` that validates early and stamps the object (subclass
marker) without restructuring it. Validation SHALL run unconditionally at
specification/estimation time — the stamp SHALL NOT bypass it (manynet verbs and plain
list assignment mutate the object while preserving the class vector, so a stamp is not
evidence of validity); the stamp serves provenance and print dispatch only.

Validation SHALL run on both estimation entries, with `validate_goldfish_data()`'s checks
and nothing weaker:

- **Specification path.** `make_specification()` SHALL validate its `data` and abort on
  an invalid object.
- **Formula path.** `estimate_dynam()`, `estimate_rem()` and `estimate_dynami()` given a
  formula and `data`, and `compute_statistics()` and `gather_model_data()` given a
  formula and `data`, SHALL validate a stocnet `data` before any part of the package
  reads it, including the DyNAM-i conversion to the legacy environment. During a
  deprecation period an object that fails validation on this path SHALL raise a
  lifecycle deprecation warning (class `lifecycle_warning_deprecated`), raised on every
  such call, whose message carries the validator's message, and estimation SHALL then
  continue exactly as it would without the check. A later release SHALL replace the
  warning with the validator's abort.
- **A specification estimated against data.** When `estimate_*()` or
  `compute_statistics()` receives a specification and no `data`, or the identical object
  the specification was built on, that object SHALL NOT be validated again, so no
  validation message is raised twice. When it receives a different `data` object, that
  object SHALL be validated as on the formula path.

A valid object SHALL produce the same results with the check as without it. The check
SHALL run once per call, before preprocessing, and SHALL NOT run per event.

goldfish SHALL read stocnet components structurally (accepting plain data.frames — tibble
not required) while manynet SHALL be listed in Imports pinned `>= 2.1.0`; manynet
functions are called only where delegation is DRY (wrapper assembly, legacy conversion),
via `@importFrom` or `manynet::` — never `manynet:::`.

#### Scenario: Raw stocnet accepted directly
- **WHEN** `estimate_dynam(formula, data = social_evolution, ...)` is called with a valid
  unstamped stocnet object
- **THEN** the object is validated on the fly, no warning is raised by the check, and
  estimation completes.

#### Scenario: as_goldfish validates early and is reused
- **WHEN** `d <- as_goldfish(social_evolution)` succeeds and `d` is passed to two
  `estimate_*()` calls
- **THEN** validation fails early at `as_goldfish()` if the data is invalid; both
  estimations re-validate the stamped object and produce results identical to the
  raw-stocnet path.

#### Scenario: Post-stamp mutation is caught at specification
- **WHEN** a stamped object is modified after `as_goldfish()` (e.g. piped through
  `bind_ties()` adding rows with character `time`, or edited by list assignment) and then
  passed to `make_specification()`
- **THEN** the unconditional validation at specification time catches the invalid data
  and aborts — the surviving stamp does not bypass any check.

#### Scenario: Post-stamp mutation is caught on the formula path
- **WHEN** a stamped object is edited by list assignment into one that
  `validate_goldfish_data()` rejects (for example, its `info$directed` entry removed)
  and then passed with a formula to `estimate_rem()`, `estimate_dynam()` or
  `compute_statistics()`
- **THEN** a `lifecycle_warning_deprecated` warning carrying the validator's message is
  raised, and the result equals the one the same call returns without the check.

#### Scenario: An invalid object warns on every call
- **WHEN** the same invalid object is passed with a formula to two estimation calls in one
  session
- **THEN** each call raises the deprecation warning; neither is silenced by the first.

#### Scenario: A specification estimated against its own data validates once
- **WHEN** `spec <- make_specification(formula, data = x)` succeeds and `spec` is
  estimated with no `data`, or with `data = x`
- **THEN** `x` is not validated a second time, and the estimation raises no validation
  warning or message.

#### Scenario: A specification estimated against substitute data validates that data
- **WHEN** a valid specification is estimated with `data = y`, where `y` is a different
  object that fails validation
- **THEN** `y` is validated and the deprecation warning is raised, as on the formula
  path.

#### Scenario: Hand-built structural input accepted
- **WHEN** a hand-built list with the stocnet structure (plain data.frames, no manynet
  constructor involved) is passed to `as_goldfish()`
- **THEN** validation and estimation work — the boundary reads structure, not class
  provenance, keeping the test fixtures independent of manynet's release cadence.

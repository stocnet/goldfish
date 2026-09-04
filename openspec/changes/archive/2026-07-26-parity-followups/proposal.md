## Why

`backend-parity` made every per-event primitive available on every backend and
proved them equal to 1e-10. Getting there surfaced eight further problems that
were each **verified, then deliberately not fixed**, because fixing them inside
that change would have meant either changing numbers a refactor promised not to
change, or widening a scope that was already large. The reasons are recorded in
its `design.md` and `progress.md`; the problems are still there.

They share a shape: something currently *works*, but nothing *guards* it, or the
contract stops short of what a consumer actually needs. That is exactly the
class of issue that gets rediscovered — expensively — by the next change. One
(the summation-order fragility) is a latent numerical hazard; three are missing
guards on controls that already exist; one is a contract this package's own
diagnostics surface is about to depend on (`residuals-gof`); one is a
consistency cleanup whose original precision justification did not survive
measurement and is kept on narrower grounds; two are open questions worth
answering while the measurements are fresh.

## What Changes

- **A fitted model declares what its per-event indices mean.** Today a
  length-5 vector indexes senders on DyNAM-rate and receivers on DyNAM-choice,
  and the axis that distinguishes them is readable only through
  `goldfish:::risk_set_axis()`. It becomes documented, exported surface, and the
  node lookup is specified as the way any index resolves to an actor. Additive,
  not breaking. (Reshaping or labelling `margins` itself is **not** here —
  `residuals-gof` owns that object and is already opening it; see design D1 as
  revised.)
- **The six `*_default.cpp` engines produce `event_scores` through the shared
  reduction** instead of six private copies of the arithmetic — the same
  one-implementation argument that carried the ranks and margins folds. It
  shifts stored values slightly, which is why `backend-parity` D7 could not do
  it inside a behavior-preserving refactor. It is *not* justified as a precision
  fix: the conditioning penalty of the current form was measured at 1.2e-14 to
  9.2e-13 relative, far inside the 1e-10 tolerance (design D4 as revised), so
  this is a consistency cleanup and is prioritized as one.
- **A regression guard for the summation-order hazard.** `DyNAM_rate_default`
  deliberately stores the doubles its loop already computes rather than
  rebuilding the rate vector as a GEMV, because the two summation orders differ
  in the last bit and can move a frozen coefficient. Nothing currently fails if
  someone "simplifies" it back.
- **A content guard on the frozen baselines.** The PreToolUse hook matches
  `Edit|Write|MultiEdit` on `file_path`, so any shell write bypasses it — by
  construction, since the sanctioned generator writes through `Rscript`. A
  checksum assertion catches modification regardless of which tool made it.
- **The baseline generator scripts get exercised.** Both were broken —
  referencing `baselines_engines`, renamed by `backend-parity` task 1.2 — and
  nothing noticed, because no test runs them. They are the documented
  provenance of the regression floor.
- **`parallel` is declared.** `helper-baselines.R` calls
  `parallel::detectCores()` / `mclapply()` while the package declares it in
  neither `Imports` nor `Suggests` (recorded in `backend-parity` D18 as a real,
  out-of-scope gap).
- **An archive pre-flight for spec-delta placement.** `openspec validate`
  checks SHALL wording and scenario structure but not `##` section placement, so
  a `## MODIFIED` block naming a requirement that does not exist in the living
  spec silently becomes an ADD at archive, leaving the old wording in place.
  This found a real instance in `revise-gather-output`.
- **Two open questions answered:** the ~2.4e-07 relative drift of `fish_rem` /
  `fish_dynam_rate` against baselines frozen at `b890cd0` (within tolerance,
  identical on both backends, unexplained); and whether coordination's
  irreducible rank tie-fragility should get a deterministic tie-break rule or
  stay documented as implementation-dependent.

## Capabilities

### New Capabilities

- `diagnostic-object-contract`: what a position in a per-event diagnostic
  component *means*, and how a consumer resolves it to an actor without
  re-deriving the model family — the recorded index axis and the node-lookup
  resolution contract.

### Modified Capabilities

- `likelihood-computation`: adds the requirement that the stored per-event
  score comes from the one shared reduction rather than a per-kernel copy.

## Impact

- **Code:** the six `src/*_default.cpp` engines (`event_scores` block only);
  the results assembly in `R/model_estimate.R` plus an accessor for the axis;
  `DESCRIPTION` (`parallel`); `tests/testthat/helper-baselines.R` and the
  `_baselines/` generator scripts. **`margins` is untouched here.**
- **Consumers:** `residuals-gof` reads these components directly and is the
  reason the contract matters now; its `diagnose_*()` and `residuals()` surfaces
  are written against them, and its task 1.10 owns the margins labels and the
  uniform accessor this change deliberately leaves to it. `autograph` consumes
  fitted objects downstream.
- **Sequencing:** depends on `backend-parity` being **archived first**. Its
  deltas rename `optimizer-selection` :: "User-facing return_event_scores
  option" to "Per-event scores primitive" and add the parity requirements; a
  delta here written against the post-archive living spec would otherwise hit
  the very placement hazard this change adds a pre-flight for.
- **Not in scope:** changing any coefficient, the `diagnostics` vocabulary
  (owned by `residuals-gof`), or the backend capability table itself.

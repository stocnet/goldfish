## Why

`backend-parity` made every per-event primitive available on every backend and
proved them equal to 1e-10. Getting there surfaced eight further problems that
were each **verified, then deliberately not fixed**, because fixing them inside
that change would have meant either changing numbers a refactor promised not to
change, or widening a scope that was already large. The reasons are recorded in
its `design.md` and `progress.md`; the problems are still there.

They share a shape: something currently *works*, but nothing *guards* it, or the
contract stops short of what a consumer actually needs. That is exactly the
class of issue that gets rediscovered — expensively — by the next change. Two of
them (the score conditioning, the summation-order fragility) are latent
numerical hazards; three are missing guards on controls that already exist; one
is a contract this package's own diagnostics surface is about to depend on
(`residuals-gof`); two are open questions worth answering while the measurements
are fresh.

## What Changes

- **The per-event diagnostic object describes itself.** `margins` uses one
  component vocabulary across every risk-set geometry instead of two, the fit
  records which axis a per-event index refers to, per-event vectors carry actor
  labels, and coordination's doubled totals are marked rather than implied.
  Today a length-5 margin vector means senders on DyNAM-rate and receivers on
  DyNAM-choice, with nothing on the object to say which. **BREAKING** for code
  reading the two-sided `*_sender` / `*_receiver` component names.
- **The six `*_default.cpp` engines compute `event_scores` directly**
  (`X_obs − c·w'X`) rather than as a before/after difference of the running
  derivative. The two are algebraically identical; the difference form is the
  worse-conditioned one, subtracting two large partial sums late in a sequence.
  This changes stored `event_scores` values slightly, which is why
  `backend-parity` D7 could not do it inside a behavior-preserving refactor.
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

- `diagnostic-object-contract`: what a fitted model's per-event diagnostic
  components mean and how a consumer reads them without re-deriving the model
  family — uniform component naming across risk-set geometries, the recorded
  index axis, actor labels, and the marked accumulation set for each stored
  scale.

### Modified Capabilities

- `likelihood-computation`: adds the per-event score conditioning requirement —
  the stored score is computed directly from the event's own quantities rather
  than differenced out of an accumulating total.

## Impact

- **Code:** the six `src/*_default.cpp` engines (`event_scores` block only);
  `R/cpp_interface.R` and `R/estimation_core.R` result assembly (margin naming,
  axis marker, labels); `DESCRIPTION` (`parallel`); `tests/testthat/helper-baselines.R`
  and the `_baselines/` generator scripts.
- **Consumers:** `residuals-gof` reads these components directly and is the
  reason the contract matters now; its `diagnose_*()` and `residuals()` surfaces
  are written against them. `autograph` consumes fitted objects downstream.
- **Sequencing:** depends on `backend-parity` being **archived first**. Its
  deltas rename `optimizer-selection` :: "User-facing return_event_scores
  option" to "Per-event scores primitive" and add the parity requirements; a
  delta here written against the post-archive living spec would otherwise hit
  the very placement hazard this change adds a pre-flight for.
- **Not in scope:** changing any coefficient, the `diagnostics` vocabulary
  (owned by `residuals-gof`), or the backend capability table itself.

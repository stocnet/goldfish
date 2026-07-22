## Context

Audit of the preprocess → estimation boundary (2026-07-10, post
`support-constraint-as-stat`):

- `model_estimate.R:1550-1733`: ~180 lines of family × fold-state × engine
  case analysis. Recomputes `is_choice_family` / `is_coord_family` /
  `is_rate_family` / `is_rem_family` / `is_rem_ordered_family` from
  model/sub_model strings; hand-maintains the `native_compiled` boolean; emits
  four side channels into `argsEstimation` (`opportunitiesList`, `senderGate`,
  `remMask`, `supportMask`).
- `senderGate` is DEAD: its only origin (`:1571`) assigns `NULL`; threaded
  through `cpp_interface.R:46,63` and `estimation_core.R:67,268,1439,1504`.
- `remMask` is UNREACHABLE: it fires only when a REM/coordination constraint
  exists unfolded (`:1720-1726`), but `fold_active_dyad_support` folds those
  families unconditionally (`model_preprocess.R:1152-1161`); the
  preprocessed-version assertion rules out stale objects.
- The OUTER (ego-kind) choice fold is pending (`model_preprocess.R:1148-1151`
  comment) — the one case that keeps `mask_to_opportunities()`
  (`model_estimate.R:569`), `support_gather`/`supportMask`, and
  `cpp_interface.R:61`'s `default_c_support_ok <- identical(modelTypeCall,
  "DyNAM-M")` alive. The main `active-availability-stat` spec ALREADY requires
  this fold ("ego-support (as row flips)" + its scenario) — a spec-conformance
  gap.
- Legacy vocabulary: `legacy_model_type()` (`model_spec.R:311`, roxygen: "to
  be removed") converts spec → strings for `model_estimate.R:1750`,
  `model_preprocess.R:2006`, `preprocess_writers.R:396,552`;
  `cpp_interface.R` branches on `modelTypeCall` strings ~20× (`:61,77,199,
  242,246,349,591-731`); contribution signatures take `riskMask`
  (`estimation_core.R:687-1702`) while upstream speaks `active_dyad`; the
  rate family is dimension-sniffed (`length(dim(initialStats)) == 2L`,
  `estimation_core.R:1443`; `modelTypeCall %in% c(...)`,
  `cpp_interface.R:77`).
- The `model-recipe-dispatch` spec already bans model-type strings — scoped to
  `R/estimation_core.R` only, which is exactly where they no longer are; they
  survive in the files the requirement does not name.

Constraints: frozen coefficient baselines (1e-6, PASS not SKIP,
`NOT_CRAN=true`, never regenerate) and the C++ golden suite are the floor;
this change is behavior-preserving (no coefficient changes, no `src/` edits);
DyNAMi still runs the legacy monolith until `refactor-dynami-engine`.

Revisions (2026-07-21, pre-apply review):

- **The audit's line anchors are stale**: `refactor-likelihood-compute`,
  `refactor-single-data-object`, and `flavored-processes` all archived since
  2026-07-10 and rewrote the audited files — task 0.1 re-verifies the claims
  and refreshes every anchor before any edit.
- **Flavored processes landed**: `estimate_from_specification()` branches to
  `estimate_flavored()`, which loops `estimate_wrapper()` per fid with
  per-consumer compiled constraints. The descriptor lives on each submodel
  bundle's typed spec, so the per-fid loop reads it unchanged — but the D5
  no-strings widening must also cover `estimate_flavored.R` /
  `preprocess_flavored.R` if model-type strings appear there (task 0.1
  inventories).
- **Descriptor geometry is mode-map-aware**: `multimode-network-support`
  lands first (release step 2) — on a two-mode focal, mask symmetrization is
  skipped and `directed` is noted-and-ignored (its task 3.3), and
  nodes/nodes2 resolve through the mode map (its task 4.1). The parse-time
  descriptor (axis, symmetrize) must be derived WITH the mode map, and the
  descriptor tests re-grounded against the landed surface.
- **No DyNAMi opportunities carve-out needed**: `dynami-stocnet-boundary`'s
  corrected D6 (2026-07-21) compiles/folds its derived availability through
  the STANDARD support-constraint machinery, not the internal
  `opportunitiesList` channel — this change's ban on opportunity-list
  reductions of constraints holds cleanly; the only surviving
  `opportunitiesList` use is the deprecated constraint-free user list.

## Goals / Non-Goals

**Goals:**
- One decision point: risk-set geometry, fold policy, and engine capability
  become DATA on the typed model spec, attached at parse time
  (`R/model_spec.R` constructors, invoked from formula parsing); every
  downstream consumer reads the descriptor, none re-derives it (D1).
- Close the spec-conformance gap: the outer/ego-kind choice constraint folds
  like every other kind (D2); estimation reads maintained availability ONLY.
- Delete the vestigial channels (`senderGate`, `remMask`, `supportMask`,
  `mask_to_opportunities()`) and collapse the consumption block to
  validate-then-dispatch (D3).
- One engine-capability map consulted by every guard, abort text generated
  from it (D4).
- Retire the legacy vocabulary from the R pipeline: `legacy_model_type()`
  deleted, string branches → spec/descriptor dispatch, `riskMask` →
  `active_dyad` naming, dimension-sniffing → descriptor read (D5).

**Non-Goals:**
- No numerical changes and no `src/` changes (the C++ estimators are separate
  functions selected in R; only the R-side selection changes).
- No DyNAMi engine rework: `dynami_*` specs GET descriptor values, but the
  monolith keeps consuming its own path until `refactor-dynami-engine`.
- No user-facing API change: `estimate_*()` signatures, results, and messages
  keep their meaning (abort texts may be regenerated from the map; snapshots
  updated deliberately, not silently).
- No new constraint semantics (undirected-REM symmetrization stays with the
  `gather-rem-coordination-format` §1.4 discussion; the descriptor reserves
  the `dyad_symmetric` value it will need).

## Decisions

### D1 — Risk-set descriptor: spec data attached at parse time
Each `model_spec` constructor attaches a descriptor with (working shape):
`risk_set_axis` ("sender" | "receiver_given_sender" | "dyad" |
"dyad_symmetric"), `fold` (availability object + encoding policy the fold
uses: active_sender; active_dyad at alter/point; dense point both-presences),
`symmetrize` (TRUE only for `dynam_choice_coord_spec` today; the value the
undirected-REM discussion will reuse), and the engine-capability entries D4
consumes. The descriptor is decided ONCE, where the formula/spec is parsed —
`model_spec_structure()` and its nine constructors — and read via small
accessors everywhere else: `active_dyad_encoding_decide()` (currently keyed
on `legacy_model_type` strings, `preprocess_writers.R:552`), the fold
selection in `fold_active_dyad_support()` (currently `model_type %in%
c("REM", "REM-ordered", "DyNAM-MM")`), the validation family
(`model_estimate.R:1631`), and the estimation guards. Rationale: the recipes
already prove the pattern — preprocessing dispatches on the spec class with
no model-type branches (`model-recipe-dispatch`); this extends the same
principle to the risk-set/dispatch metadata, which today is re-derived from
strings at each site. *Alternative rejected:* S3 methods per property
(`risk_set_axis.dynam_choice_spec` etc.) — nine classes × several properties
of pure data is a table, not behavior; methods would scatter the single
source this change exists to create. S3 stays for STAGE dispatch (D5).

### D2 — Complete the outer fold (spec conformance)
`fold_active_dyad_support()` gains the pending branch: an outer-encoded
(ego-kind) choice constraint folds into the dense point `active_dyad` as row
flips — receiver presence ∩ the sender-gated support rows — exactly the
"ego-support (as row flips)" the `active-availability-stat` spec already
mandates. Consequences: `prep$active_dyad_folded` is TRUE for EVERY
constrained model; the standalone `support_mask$support` per-event list stops
reaching estimation. Verification: a fixture with an ego-kind choice
constraint estimated on the OLD standalone-mask path vs the folded path
agrees to 1e-10 per event (harness run before the old path is deleted),
plus the existing observed-event-excluded validation still fails fast.
*Alternative rejected:* keep the standalone path for outer only — it is the
single case forcing three side channels and one string guard to exist.

### D3 — Delete the vestigial channels; collapse the consumption block
With D2 in place: `senderGate` (dead — only-NULL origin) is removed from
`model_estimate.R`, `estimate_c_int`, and the four `estimation_core.R`
threading sites; `remMask` (unreachable — unconditional fold) is removed
after a confirming test that a REM/coordination constraint always arrives
folded; `supportMask`/`support_gather` and `mask_to_opportunities()` are
removed with D2. The `model_estimate.R:1550-1733` block reduces to:
fail-fast validation (family read from the descriptor) + capability-map
check (D4) + engine call — no fold-state branches, no side-channel
assembly. *Alternative rejected:* keep the channels as defensive fallbacks —
they are the patch this change removes; the preprocessed-version assertion
already guards stale inputs, and an unreachable branch is untestable.

### D4 — One engine-capability map, abort text generated from it
A single internal table — rows keyed by spec class/descriptor, columns by
engine — answers "does this engine run this (constrained) family natively?".
Consumers: the `model_estimate` guard (replacing the hand-built
`native_compiled` boolean and the hand-enumerated `cli_abort` family list,
`:1591-1605`) and `estimate_c_int`'s defensive stops (replacing
`default_c_support_ok` and the `senderGate`/`remMask` checks,
`cpp_interface.R:51-72`). The abort message enumerates supported
combinations FROM the map, so adding a family/engine cell is one table edit.
The map is data next to the descriptor (D1), not scattered predicates.
*Alternative rejected:* keep two independent guards that must agree — the
audit found they already encode the same knowledge in different vocabularies
(family booleans vs `modelTypeCall` strings).

### D5 — Legacy vocabulary retired from the R pipeline
Full scope (user decision 2026-07-10, not stretch): (a)
`legacy_model_type()` DELETED; its call sites (`model_estimate.R:1750`,
`model_preprocess.R:2006`, `preprocess_writers.R:396,552`) read the spec /
descriptor; (b) `cpp_interface.R` stops branching on `modelTypeCall` strings
— `estimate_c_int` selects the C++ estimator and shapes its arguments via
stage-boundary dispatch on the spec (consistent with the existing "S3
dispatch only at stage boundaries" requirement), and `gather_`'s family
branches (`:591-731`) key on the descriptor/indexing class; (c) contribution
signatures rename `riskMask` → `active_dyad`-vocabulary (pure rename,
byte-identical results; roxygen updated); (d) rate detection reads the
descriptor (`risk_set_axis == "sender"`), not array dimensionality —
`estimation_core.R:1443`, `cpp_interface.R:77`. The existing
`model-recipe-dispatch` requirement widens accordingly (delta): model-type
strings banned from the full R pipeline, not only `estimation_core.R`.
Boundary note: `modelTypeCall` may survive ONLY as a value inside exported
legacy OUTPUT (`gather_model_data()` docs name model strings for users) —
never as a branching key. *Alternative rejected:* keep `legacy_model_type()`
as a shim — its own roxygen has said "to be removed" since the recipe
refactor; every kept caller is a site where the string vocabulary re-enters.

### D6 — Sequencing after refactor-likelihood-compute
That change edits the same `model_estimate.R` region (its 5.8 lifts the
coordination redirect at `:1666-1673`) and rewrites the gather emit this
change's `gather_` dispatch touches. Landing second avoids same-block churn
and lets the D3 collapse delete the redirect-adjacent scaffolding in one
pass. `refactor-dynami-engine` is independent (the monolith keeps its own
path either way); `gather-rem-coordination-format` lands after both and is
unaffected (storage-only, behind the index contract).

## Risks / Trade-offs

- **A rename/collapse this wide can silently change guard behavior** → abort
  and inform messages snapshot-tested (pinned cli context) BEFORE the
  collapse; the constrained suites (`test-support_constraint_*.R`) already
  cover every family × engine cell and must pass unchanged.
- **The outer fold changes the preprocessed representation for ego-kind
  constraints** (standalone mask list → folded point buffer) → per-event
  1e-10 equivalence harness against the old path before deletion; frozen
  baselines are unaffected (unconstrained models byte-identical).
- **`remMask` might be reachable via a path the audit missed** → a
  confirming test (constrained REM/coordination always arrives folded) gates
  the deletion; if it fires, the fold gap found is fixed rather than the
  fallback kept.
- **Descriptor drift vs recipe declarations** (the recipes also declare fold
  behavior) → the recipes READ the descriptor (D1) instead of declaring in
  parallel; one source.
- **DyNAMi monolith untouched** → descriptor values defined but not
  load-bearing there; asserted by the existing DyNAMi suites staying green.

## Migration Plan

1. Descriptor + accessors on the spec constructors; `active_dyad_encoding_
   decide()` and the fold selection read it; preprocessing byte-identical.
2. Outer fold (D2) + equivalence harness vs the standalone path; then delete
   `mask_to_opportunities()` / `supportMask`.
3. Dead channels out (`senderGate`, `remMask` after the confirming test);
   capability map in (D4); consumption block collapsed (D3); guard-message
   snapshots.
4. Legacy vocabulary retirement (D5): `legacy_model_type()` deleted,
   `cpp_interface`/`gather_` dispatch on spec, `riskMask` rename,
   descriptor-based rate detection.
5. Full `NOT_CRAN=true` suite green (baselines PASS not SKIP), lintr clean on
   touched files, milestone bump, spec-conformance agent.

Rollback: each step is an independent commit gated on the unchanged
constrained suites + baselines.

## Open Questions

*(all resolved 2026-07-19 explore session)*

- ~~Descriptor shape~~ — **one `risk_set` named-list field** on the spec
  (`spec$risk_set <- list(axis =, fold_target =, encoding =, symmetrize =,
  …capability entries)`) with small internal accessors: consumers read one
  field, the whole geometry prints/debugs at once, and future fields never
  touch the constructor signatures.
- ~~`gather_` dispatch style~~ — **S3 methods on the spec class** (generic +
  per-indexing-family methods), consistent with the "S3 at stage boundaries"
  principle; the descriptor is data consumed inside the method, never a
  second dispatch idiom.
- ~~`modelTypeCall` user surface~~ — user confirmed **nothing user-facing
  relies on the values**: D5 retires the vocabulary outright with no
  compatibility shim; the audit reduces to a confirming grep over exported
  surfaces in task 4.4's grep-clean check.

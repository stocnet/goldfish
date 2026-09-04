## Why

The preprocess side of `support-constraint-as-stat` landed well-knit (one
availability representation, per-family folds, fail-fast validation), but the
preprocess → estimation BOUNDARY still carries its scaffolding, audited
2026-07-10:

- **One fold is incomplete**: an outer-encoded (ego-kind) DyNAM-choice
  constraint is NOT folded (`model_preprocess.R:1148-1151` says "still
  pending") — the acknowledged gap that keeps THREE estimation-time side
  channels alive (`mask_to_opportunities()`, `support_gather`/`supportMask`,
  and the `default_c_support_ok` string check in `cpp_interface.R:61`). The
  main `active-availability-stat` spec ALREADY requires this fold ("ego-support
  (as row flips)"), so the code is out of conformance.
- **Dead/vestigial channels**: `senderGate` is assigned only `NULL` at its one
  origin (`model_estimate.R:1571`) yet threads through `estimate_c_int` and
  four `estimation_core.R` sites; `remMask` survives only as a
  fold-did-not-apply fallback that is unreachable (the REM/coordination fold
  is unconditional, `model_preprocess.R:1152-1161`).
- **Family knowledge is re-derived in three places, three ways**: a 180-line
  block (`model_estimate.R:1550-1733`) recomputes `is_*_family` booleans from
  model/sub_model strings and hand-maintains the `native_compiled` table;
  `cpp_interface.R:51-72` re-checks nativeness independently by
  `modelTypeCall` string; `estimation_core.R` uses spec classes. Three sites
  must agree about which engine × family combination is native.
- **Legacy vocabulary at the seam**: `legacy_model_type(spec)` converts the
  typed spec BACK to strings right at the engine boundary (its own roxygen
  says "to be removed"); contribution signatures still speak `riskMask` while
  the pipeline upstream renamed to `active_dyad`; the rate family is detected
  by array-dimension sniffing (`length(dim(initialStats)) == 2L`,
  `estimation_core.R:1443`, `cpp_interface.R:77`).

The fix follows the principle the recipes already embody: **the typed model
spec, constructed once at formula-parsing time, is the single decision point**
— risk-set geometry, fold policy, and engine capability are spec DATA that
every downstream consumer reads, never re-derives.

## What Changes

- **Risk-set descriptor on the model spec (decided at parse time)**: the spec
  constructors (`R/model_spec.R`) attach the risk-set geometry as data —
  axis (`sender` / `receiver_given_sender` / `dyad` / `dyad_symmetric`), fold
  target + encoding policy, symmetrization — and every downstream site
  (encoding decision, fold selection, validation family, engine guards) reads
  it. No `is_*_family` boolean is recomputed from model/sub_model strings.
- **Complete the outer fold** (conformance with `active-availability-stat`):
  an ego-kind/outer DyNAM-choice constraint folds into the dense point
  `active_dyad` as row flips, like every other kind. With that, ALL
  constrained models read maintained availability buffers at estimation time.
- **Delete the estimation-time side channels**: `mask_to_opportunities()`,
  `supportMask`/`support_gather`, the unreachable `remMask` fallback, and the
  dead `senderGate` parameter (all origins and threads); the
  `model_estimate.R:1550-1733` consumption block collapses to
  validate-then-dispatch.
- **Single engine-capability map**: one spec × engine table (derived from the
  descriptor) answers "native or abort" for constrained estimation;
  `model_estimate` guards AND the `estimate_c_int` defensive stops consult it;
  abort messages are generated from it (no hand-enumerated family lists).
- **Retire the legacy model-type vocabulary from the R pipeline** (user
  decision 2026-07-10: full scope, not stretch): `legacy_model_type()` is
  deleted; `estimate_c_int` / `gather_` / writer sites branch on the spec
  class or descriptor instead of `modelTypeCall` strings — widening the
  existing `model-recipe-dispatch` requirement (strings banned in
  `estimation_core.R`) to the whole pipeline; contribution signatures rename
  `riskMask` → the `active_dyad` vocabulary; dimension-sniffing rate
  detection is replaced by a descriptor read. Pure R-side: the C++
  estimators are separate functions selected in R, so no `src/` change.
- **Behavior-preserving**: no coefficient changes; frozen baselines PASS at
  1e-6; the outer-fold equivalence is tested against the standalone-mask path
  BEFORE that path is deleted; guard/abort texts snapshot-tested.

## Capabilities

### New Capabilities
- `risk-set-dispatch`: the risk-set descriptor as parse-time spec data; the
  single engine-capability map; no estimation-time standalone constraint
  channels (all kinds folded, estimation reads maintained availability only).

### Modified Capabilities
- `model-recipe-dispatch`: the no-model-type-strings requirement widens from
  `R/estimation_core.R` to the full R pipeline (`R/cpp_interface.R`,
  `R/model_estimate.R`, `R/preprocess_writers.R`); `legacy_model_type()` is
  removed.

## Impact

- **Code**: `R/model_spec.R` (descriptor fields + accessors;
  `legacy_model_type()` deleted), `R/model_estimate.R` (consumption block
  collapse, capability map, side-channel removal, `mask_to_opportunities()`
  deleted), `R/model_preprocess.R` (outer fold in `fold_active_dyad_support`),
  `R/preprocess_writers.R` (`active_dyad_encoding_decide()` reads the
  descriptor), `R/cpp_interface.R` (`estimate_c_int`/`gather_` dispatch via
  spec; defensive stops via the map; `senderGate`/`supportMask` params
  removed), `R/estimation_core.R` (`riskMask` → `active_dyad` naming,
  descriptor-based rate detection, `senderGate`/`remMask` threading removed).
  No `src/` change.
- **DyNAMi caveat**: the `dynami_*` spec classes get descriptor values, but
  their consumption stays inside the legacy monolith path until
  `refactor-dynami-engine` lands — the descriptor is defined for them, not yet
  load-bearing there.
- **Tests**: outer-fold equivalence (folded vs standalone-mask path, before
  deletion); constrained estimates unchanged across engines (existing
  support_constraint suites); guard-message snapshots (pinned cli context);
  frozen baselines + C++ golden PASS not SKIP; renames verified by the
  unchanged suites.
- **Sequencing**: AFTER `refactor-likelihood-compute` — its tasks 5.7–5.9
  edit the same `model_estimate.R` region (redirect lift) and the gather emit;
  landing this second avoids same-block churn and lets the collapse delete
  the redirect-adjacent scaffolding in one pass. *(Satisfied — it archived
  2026-07-12.)* In the 2.0.0 sequence this change follows
  `multimode-network-support` (descriptor geometry reads the mode map) and
  `dynami-stocnet-boundary` (whose corrected D6 routes the derived DyNAMi
  availability through the standard support-constraint machinery — no
  opportunities carve-out here), and precedes `residuals-gof`. Task 0.1
  re-grounds the 2026-07-10 audit against everything archived since.

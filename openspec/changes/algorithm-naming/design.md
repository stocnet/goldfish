# Design — algorithm-naming

## Context

The estimation control surface predates the stocnet workshop naming
conventions. Grounding facts (survey 2026-07-24, full record in
`.plan/Naming_guidelines.md`):

- RSiena 1.6.0 shipped its rename ("Overhaul of naming system"): split
  `sienaAlgorithmCreate` into `set_model_saom()` / `set_algorithm_saom()`
  / `set_output_saom()`, consumed as `siena(data, effects,
  control_model =, control_algo =, control_out =)`. Suffixes are
  semantic (`_saom` control, `_rsiena` data), analysis verbs unsuffixed
  (`test_gof`, `test_parameter`). Backward compatibility there is
  parallel aliases with zero warnings.
- `feature/dynes` has no `set_algorithm_*()` in code; the abmcem /
  dynes-augmentation designs specify a provisional `set_alg_em()`
  (nesting `set_alg_augment/weights/sgd`) consumed via
  `estimate_dynes(spec, algorithm =)`, explicitly pending alignment with
  the house convention.
- residuals-gof (in progress, phase 1 done) introduced `diagnostics =`
  on `set_estimation_opt()` with fresh `deprecate_soft` messages naming
  `set_estimation_opt(diagnostics)`, and its phase-2 specs name
  `keep_preprocessed`, `evaluate_engine()`, `examine_onset()` — none
  implemented yet.
- The dynes branch carries a doc/code drift: roxygen documents
  `control_estimation.goldfish` / `control_preprocessing.goldfish`
  while constructors assign `estimation_opt.goldfish` /
  `preprocessing_opt.goldfish`.

Constraint: 2.0.0 (CRAN ~mid-Aug) is the window; this change must land
before residuals-gof phase 2 so new exports are born under final names.
Frozen coefficient baselines must stay PASS — renames must not touch
numerical paths.

## Goals / Non-Goals

**Goals:**

- Rename the two control constructors, the estimator control arguments,
  and the preprocessed-object plumbing to the settled names, with a
  complete lifecycle alias layer (soft at 2.0.0).
- Rename `examine_outliers()` / `examine_changepoints()` to
  `diagnose_*()`.
- Re-point every live deprecation message to final names (no two-hop
  chains).
- Align the in-flight residuals-gof / abmcem / dynes-augmentation
  artifacts to the final vocabulary.

**Non-Goals:**

- No behavior or numerical changes; no C++ changes.
- No removal of the 1.7.0 defunct layer (schedule note only).
- No effect renaming (owned by effect-term-registry successors); the
  `gather_model_data()`/`compute_stats()` consolidation is owned by
  revise-gather-output (implemented after this change); no
  `to_ego()`/`to_alter()` export audit — backlog items in
  `.plan/Naming_guidelines.md` §5.
- No result-class renames (`outliers.goldfish` etc. stay; autograph
  contract untouched).

## Decisions

### D1 — `set_algorithm_newton()`: suffix by algorithm family, not package

Workshop table says `set_algorithm_pkg()`, but one package will hold two
algorithm objects (Newton-type direct maximization now, DyNES
ascent-based Monte Carlo EM next), so the package suffix cannot
discriminate; RSiena's shipped `set_algorithm_saom()` already broke the
pkg-suffix reading in favor of a semantic one. Alternatives rejected:
`set_algorithm_gbm()` — collides with the {gbm} CRAN package, the DyNES
inner SGD optimizer is also gradient-based (leaky contrast), and
Nelder–Mead is derivative-free; `set_algorithm_direct()` — accurate but
opaque; `set_algorithm_goldfish()` — fails at the second algorithm.
"Newton" is the docs' existing vocabulary (damped Newton–Raphson
default; BFGS/BHHH are Newton-type), accepting Nelder–Mead as the
documented odd one out.

### D2 — Class hierarchy `c("algorithm_newton.goldfish", "algorithm.goldfish", "list")`

The `algorithm.goldfish` superclass gives estimators one `inherits()`
check and a shared print/validation hook that `set_algorithm_em()` joins
later; it also resolves the dynes-branch doc drift by making the roxygen
class references true. `set_preprocessing()` returns
`c("preprocessing.goldfish", "list")`. The old class strings are NOT
kept in the class vector: classes are internal plumbing; grep shows no
user-facing contract on them, and internal consumers are updated in this
change. Print methods migrate to the new classes.

### D3 — Estimator arguments `control_algo =` / `control_prep =`

Follows RSiena's shipped `control_*` argument family (not the workshop
example's `control_alg`/`control_out` spellings), both clipped for
symmetry. This change implements BEFORE revise-gather-output (2026-07-24
alignment session), so `compute_statistics()` is born there fully final
(`control_prep = set_preprocessing()`, superseding its interim
`control_preprocess` sketch) with no code task here;
`gather_model_data()` is deprecated wholesale there and keeps its old
argument. Applies to `estimate_dynam/dynami/rem` and internal
`estimate_wrapper()` / `estimate_from_specification()` plumbing. The
abmcem spec's
`estimate_dynes(spec, algorithm =)` is edited to `control_algo =` so the
two estimator generations agree. Old argument names are kept as
`deprecated()`-sentinel formals that forward with a soft warning.

### D4 — `preprocessed =` is the single supply argument; the standalone route lives in revise-gather-output

`preprocessing_init =` (estimators) and the residuals-gof consumers'
`preprocessed =` were two names for "here is a `preprocessed.goldfish`";
`preprocessed =` wins everywhere (renamed here as a sentinel).

The standalone "give me only the preprocessed object" route is
`compute_statistics(output = "preprocessed")` — owned by
revise-gather-output, which implements after this change and also owns
the `preprocessing_only` soft-deprecation (its warning must name a
function that exists). An earlier sketch of this change exported a
dedicated `make_preprocessed()`; it was dropped (2026-07-24 alignment
session) because the two same-day changes would have shipped two exported
routes to the same object — the duplicate-route interplay the
`return_preprocessed` / `preprocessing_only` split exists to remove. The
`output` value `"preprocessed"` was chosen over `"default"` (engine
jargon, says nothing about the return) and `"compact"` (accurate but
requires knowing the delta/broadcast internals): it names the returned
class and mirrors the `preprocessed =` argument it feeds. The
return-changing route and residuals-gof's attach-to-fit flag
(`return_preprocessed`, born final there) still can never interact.

### D5 — Lifecycle policy: soft at 2.0.0, no two-hop messages

Every old surface stays working through 2.x: renamed functions as thin
exported wrappers with `lifecycle::deprecate_soft(when = "2.0.0")` (from
the wrapper frame so attribution hits the user's call), renamed
arguments as `deprecated()` sentinels folded into the new argument.
Deliberately not RSiena's silent permanent aliases — goldfish already
has a lifecycle discipline and silent doubling contradicts it. All live
deprecation texts that point at renamed surfaces are re-pointed in the
same release: `return_interval_loglik`/`return_probabilities`/
`return_event_scores` (`with = "set_algorithm_newton(diagnostics)"`),
`convergence_criterion`, `fixed_parameters`, `opportunities_list`
(names `estimate_dynam()` — unaffected — but verify), and cli error
strings naming `set_estimation_opt`. Removal horizon for this alias
layer AND the 1.7.0 camelCase defunct layer: ≥3.0.0 (NEWS note, no code
removal here).

### D6 — `diagnose_*` replaces `examine_*`

Workshop reserves `diagnose_[mode]()`; no sibling ships it yet, goldfish
is first mover. `examine_outliers()` → `diagnose_outliers()`,
`examine_changepoints()` → `diagnose_changepoints()` with soft aliases
(the already-deprecated `examineOutliers`/`examineChangepoints` shims in
`goldfish-defunct.R` re-point their `with =` to `diagnose_*()` directly
— the no-two-hop rule). While the signature is open,
`diagnose_outliers(parameter =)` is renamed `threshold =` (vague name;
backlog item promoted since residuals-gof already breaks these
functions' return classes — one break, one release). Return classes
(`diagnostic.goldfish` today, changing in residuals-gof) are residuals-
gof's concern, not this change's.

### D7 — Cross-change artifact alignment happens here, as tasks

residuals-gof phase 2 must be born final: its design (D3/D12/D13) and
specs swap `keep_preprocessed` → `return_preprocessed`,
`evaluate_engine()` → `evaluate_model()`, `examine_*` → `diagnose_*`.
abmcem + dynes-augmentation designs/specs swap `set_alg_em()` →
`set_algorithm_em()`, note component naming for the nested trio
(decided there, not here), and `estimate_dynes(algorithm =)` →
`control_algo =`. These are artifact edits only — no dynes code exists.

### D8 — Living-spec wording deltas ride the change

`offset-fixed-terms` (2× `set_estimation_opt`), `active-availability-
stat` (3× `preprocessing_init`), `flat-preprocess-output` (1×
`preprocessing_init`) get wording-only MODIFIED deltas so the archive
fold leaves the living spec consistent. `optimizer-selection` gets the
substantive delta (constructor name, superclass, `control_algo`).

## Risks / Trade-offs

- [Alias layer widens the exported surface through 2.x] → aliases are
  one-line wrappers in a single file section; removal is a scheduled
  3.0.0 sweep.
- [`deprecate_soft` is silent in non-direct calls, so downstream code
  paths keep old names unnoticed] → grep-based task verifies no internal
  caller uses a deprecated name after the rename.
- [Snapshot/coefficient tests calling old names emit new warnings] →
  testthat 3e treats `deprecate_soft` as quiet inside tests of other
  functions; tests that *target* the deprecations use
  `expect_snapshot()`/`lifecycle::expect_deprecated()`; the suite is
  migrated to new names anyway (same grep task).
- [Renaming `parameter =` inside D6 is a hard break for named-argument
  callers] → soft-deprecated sentinel like the others; only the default
  positional use is unaffected.
- [Editing another active change's artifacts (residuals-gof) mid-flight]
  → residuals-gof phase 2 has not started (9/40, all phase 1); edits are
  pure string renames; its progress.md logs the alignment.
- [`preprocessing_only` stays non-deprecated through this change's
  release window] → deliberate sequencing (its replacement lands next in
  revise-gather-output); the NEWS migration table already lists the
  coming route so users are not pointed at a moving target.

## Migration Plan

1. Land constructors + classes + aliases (`set_opt.R`), then estimator
   arguments, then `diagnose_*` — one task group each, tests green and
   `NOT_CRAN=true` baselines PASS at every commit.
2. Cross-change artifact edits last (no code).
3. NEWS.md gains a 2.0.0 "naming migration" section with the old→new
   table; DESCRIPTION bump at the milestone.
4. Rollback: each task group is a focused commit; aliases mean user code
   never breaks mid-migration.

## Open Questions

- None blocking. Component names for the nested DyNES constructors
  (`set_alg_augment/weights/sgd` →) are explicitly deferred to the
  abmcem change (D7).

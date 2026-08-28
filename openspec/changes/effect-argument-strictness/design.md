## Context

`effect-term-registry` already specifies exact argument-name matching (D25), but
it is a Layer-1 rewrite sitting in the post-2.0.0 effects track. The defect it
would fix is live now and has already reached a frozen baseline, so the
name-matching slice is extracted to land before the release.

Two decisions govern this change: ADR-0032 (the pre-2.0.0 rename policy covers
argument names, and a rename must fail loudly) and ADR-0021 (a frozen baseline
moves only when the move was computed first).

## Goals / Non-Goals

**Goals**
- An effect argument goldfish cannot honor is an error, at parse time.
- The five retired 1.7.0 names report their replacement.
- The corpus and the vignette stop using a retired name; the one baseline that
  depended on the drop is corrected with its derivation recorded.

**Non-Goals**
- The registry. This changes the matching rule, not where argument metadata
  lives; `effect-term-registry` still owns that move.
- **Argument *values*.** `resolve_effect_args()` validates only arguments whose
  closure default is a character vector of length >= 2, so of 65 (function,
  argument) pairs across `type`/`history`/`sub_type`/`joining`, **16 are
  validated and 49 are not** — all 31 `sub_type` and all 18 `joining`, every one
  a DyNAMi effect with a length-1 default. `sub_type = "bogus"` therefore fails
  mid-walk with `comparison (!=) is possible only for atomic and list types`.
  Real, and deliberately out of scope: fixing it means writing down every
  DyNAMi choice set, which today exists only as the `if` branches of the update
  bodies. That is `effect-term-registry` D25's `allowed` schema. This change
  fixes the *name* side only, and D6 below says why the boundary sits there.
- Inert-but-accepted arguments. ADR-0007 (abort) is implemented with the
  registry's encoding step, not here.

## Decisions

### D1: Scope is effect arguments, not exported-function arguments
The living `naming-deprecations` spec already covers renamed arguments of
exported functions (`control_estimation` -> `control_algo`, `preprocessing_init`
-> `preprocessed`, …) as `lifecycle::deprecated()` sentinels that warn and
forward. This change does **not** touch that and does not contradict it.

The split is grounded in mechanism, not preference. A sentinel works because the
function's own signature receives the argument and can inspect it. An effect
argument never reaches a signature: the parser matches names against `formals()`
and discards what does not match, before anything with a lifecycle opinion is
called. A sentinel for `subType` would have to be added to every effect's
formals and then be re-dropped by the same `pmatch`.

So: **exported-function arguments warn and forward; effect arguments abort.**
Both are stated in ADR-0032's terms — the user is told when goldfish cannot
honor what they wrote — and the difference in loudness follows from where the
argument is resolved.

*This answers ADR-0032's first open question.*

### D2: Exact matching; prefix acceptance is retired
Names match exactly. `pmatch` also binds unambiguous prefixes, so
`transformer = sqrt` and `weight = TRUE` work today and will error.

Accepted deliberately: a prefix that is unambiguous now becomes ambiguous the
moment a sibling argument is added, so today's behaviour is a latent
compatibility hazard rather than a feature — a future effect gaining a
`transform_window` argument would silently change what `transformer =` means.
The corpus sweep found no prefix use in the repo; user code is not surveyed.

### D3: The retired-name map is data
A lookup table of retired spelling -> current spelling, read by the error
message. Prose in a vignette does not reach the person writing a formula, and
the sixth rename must extend a table rather than repeat this change.

Seeded with the five from the 1.7.0 retirement: `transformFun` ->
`transformer_fn`, `isTwoMode` -> `is_two_mode`, `aggregateFun` ->
`summarizer_fn`, `ignoreRep` -> `ignore_repetitions`, `subType` -> `sub_type`.

Shape it so `effect-term-registry` can absorb it into the per-argument
`deprecated` field (its task 2.1) without a second migration — a flat named
character vector keyed by retired name, not logic embedded in the error path.

### D4: An unknown name that is not retired gets a suggestion
Error names the argument, lists the accepted ones, and suggests the closest
accepted name when one is within a small edit distance. `transform_fn` ->
`transformer_fn` is the motivating near-miss and is *not* a retired name, so
the map alone does not cover it.

### D5: The baseline correction is one commit with its derivation
Per ADR-0021 the two DyNAM-i intercepts may be re-frozen because both are
computable from the old baseline's own numbers:

```
m = mean(age) = 322/11 = 29.2727...
d(Intercept)      = b_leave * m            = 0.81839842   measured 0.81839846
d(intercept_join) = (b_join - b_leave) * m = 0.98735328   measured 0.98735312
```

Surviving invariants: all seven slopes bit-identical, `logLik` unchanged at
`-1306.3199849182` to fourteen figures. The derivation goes in
`tests/testthat/_baselines/README.md`, and the correction lands in the same
commit as the check that forces it so no commit in history holds a baseline its
own formula does not produce.

**Blocked on**: the `PreToolUse` guard in `.claude/settings.json` denies
Edit/Write anywhere under `_baselines/`, README included. Narrowing it is
ADR-0018's
standing open question. The re-freeze itself is *not* blocked — those numbers are
inline literals in `test-dynami_baselines.R`, not files under `_baselines/`.

### D6: Name side now, value side with the registry
The name defect is closable in isolation: the matching rule changes, and the set
of valid names already exists in `formals()`. The value defect is not — it needs
a declared choice set per effect, which does not exist anywhere yet and is the
registry's `allowed` schema.

Splitting them keeps this change small enough to land pre-2.0.0, which is the
whole reason it was extracted. The cost is that `sub_type = "bogus"` keeps
failing with a confusing mid-walk error until the registry lands; that is a bad
message for a genuine mistake, not a silent wrong answer, and it is the lesser
of the two defects.

*Rejected:* widening `resolve_effect_args()`'s length >= 2 guard as a stopgap.
It would need a choice set to widen *to*, so it is the registry's work wearing a
smaller name — and a second transitional reader is exactly what D24 of the
registry exists to remove.

## Risks / Trade-offs

- [A break lands on user code we cannot survey] → the error carries the
  replacement name for retired spellings and a suggestion otherwise, so the fix
  is in the message. `NEWS.md` names all five renames. Pre-2.0.0 is the cheap
  window (ADR-0016/0032) and it is closing.
- [Prefix retirement is invisible until it bites] → no corpus use, but a user
  who wrote `transformer =` gets an error naming `transformer_fn`, which the
  edit-distance suggestion (D4) covers.
- [Re-freezing a baseline sets a precedent] → ADR-0021 bounds it: derived
  before the run, derivation recorded, invariants stated. This is the only
  baseline movement in scope.
- [Divergence from effect-term-registry] → that change's D25 and tasks 1.2b /
  4.2 / 4.2b describe this same work at registry scope. When this lands first,
  those tasks reduce to absorbing the map and the matcher into `term_def`.
  Re-ground them at that point rather than deleting them now.

## Migration Plan

1. Inventory: retired names + AST sweep (the sweep script is reusable).
2. Exact matching + retired map + suggestions, with tests.
3. Migrate the 21 corpus sites; re-knit the vignette.
4. Correct and re-freeze the two intercepts with the derivation recorded.
5. `NEWS.md` retrospective entry.

## Open Questions

- [ ] Does the sweep become a permanent regression check, or retire with the
      migration? It catches the general class and would have caught this years
      earlier.
- [ ] `NEWS.md` placement for a retrospective correction covering a released
      version — under the development version with a note, or an amended 1.7.0
      heading? ADR-0032 leaves this open.
- [ ] Are there retired names beyond the five? The `v1.6.12..HEAD` `@param` diff
      is the evidence, but arguments removed rather than renamed would not show
      up as a rename and would land in the generic unknown-argument path.

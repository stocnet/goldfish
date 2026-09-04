# Proposal — fixed-parameter-contract

## Why

Fixedness (which coefficients are held at supplied values) currently has
three internal encodings, none of which carries names:

1. **The wire**: a positional NA-vector (`NA` = estimate, value = fix)
   assembled by `assemble_fixed_parameters()` (formula_validate.R) and
   passed to the estimators. Its alignment with the coefficient order
   (intercept prepended, interaction columns, excluded parameters) is
   checkable only by length — an off-by-one silently fixes the *wrong*
   coefficient, the exact failure mode `offset()` was introduced to end at
   the user surface.
2. **The decode**: duplicated blocks in `estimate_c_int()`
   (cpp_interface.R:84-108) and `estimate_int()` (estimation_core.R:84-108)
   — same logic, same typo (`idUnfixedCompnents`) — plus the maxLik
   adapter's masking, plus a hidden coupling at cpp_interface.R:194 where
   `is.na(fixedParameters[1])` hardcodes "position 1 is the intercept" into
   an intercept warm-start gate.
3. **The fit record**: `names[, "fixed"]` stores `"TRUE"`/`"FALSE"`
   *strings* that `GetFixed()` (utils.R:815-823) decodes with
   `eval(parse(text = x))` per coefficient, consumed by five-plus
   print/postestimate methods.

The user surface is already right: `offset()` aligns by term name and
`assemble_fixed_parameters()` is the single point that knows names,
positions, the intercept rule, and the interaction columns. Everything
needed for a structured contract exists at exactly one place — it is then
flattened into a sentinel vector and re-derived downstream. And the next
consumer is imminent: `residuals-gof` task 2.1 generalizes the evaluator
into `evaluate_model()`, which would otherwise re-implement the decode a
fourth time.

**The disease has a sibling** (scope expanded 2026-07-27):
`set_algorithm_newton(initial_parameters =)` is a full-length positional
vector — seeding one term means counting coefficient positions for all of
them, and supplying the vector at all silently disables the Poisson rate
intercept's data-derived warm start (cpp_interface.R gates it on
`is.null(initialParameters)`), even when the user only wanted to seed a
non-intercept term. The flavored path shows why positional value channels
cannot survive: a multi-flavor specification estimates per fid by
re-entering the wrapper with each fid's *own formula* but **one shared
control object**, so (a) per-flavor offset values are inexpressible, (b) a
specification where only one flavor carries an `offset()` term aborts
outright at the offset-less fid, and (c) a shared positional initial
vector that happens to match another flavor's length seeds the *wrong
coefficients with no error*. Names travel correctly through per-fid
re-entry; positions do not. Since `initial_parameters` is public surface
(v1.7.0), an additive named form costs nothing at 2.0.0 and would owe a
deprecation cycle after it.

## What Changes

- **One structured contract replaces the NA-vector wire.**
  `assemble_fixed_parameters()` emits a `fixed_spec` object — `idx`
  (integer positions into the final coefficient vector), `values`
  (numeric, same length), `names` (term labels for messages) — or `NULL`
  when nothing is fixed. `likelihood_only` (all coefficients fixed) and
  intercept-fixedness (`1L %in% idx`) become derivations, not overloads:
  the `is.na(fixedParameters[1])` gate is replaced by the explicit
  predicate.
- **One shared decode.** A single masking helper consumed by
  `estimate_c_int()`, `estimate_int()`, and the maxLik adapter replaces the
  duplicated blocks (and retires the `Compnents` typo); indices are
  validated against `nParams` once, with cli errors that can name the term.
- **The fit records fixedness as a plain logical**, read by `GetFixed()`
  without `eval(parse())`. A fit carrying only the string column is a
  pre-change fit and is refused through the result-format guard with re-fit
  guidance — the same no-window policy `snake-case-result-components`
  shipped (decision 2026-07-27: any pre-rename fit already trips that guard
  first, so a tolerant string reader would be dead code; dev-line fits
  produced between the rename and this change re-fit rather than being
  tolerated).
- **Named-partial initial values** (additive). `initial_parameters` also
  accepts a named numeric vector matched against the fit's deduped
  coefficient labels (the ones `tidy()` renders), seeding only the named
  terms; the full-length unnamed positional form keeps its exact current
  behavior, so nothing breaks and no lifecycle is owed. Seeding a
  non-intercept term no longer disables the rate intercept's warm start —
  the gate reads the explicit `intercept_seeded` predicate. On a flavored
  specification a flat named vector **broadcasts** (each fid seeds the
  labels it has — sound, because initials never change the optimum), and
  a nested list keyed by flavor (optionally family) targets one process;
  the positional form aborts there.
- **Offset values ride the formula: `offset(term, coef = value)`.** The
  value sits in the only fid-local object a flavored specification has —
  its formulas — so per-flavor and per-family values are exact by
  construction (base-R precedent: `offset(log(exposure))` carries its
  values in the formula). A flavored specification takes offset values
  from formulas only (`offset_coef` there aborts with guidance); the
  mixed-presence abort that made a one-flavor-offset specification
  inestimable disappears. Single-process models keep `offset_coef`
  (dev-line surface, positional plus a named form) alongside `coef =`,
  with a both-sources conflict aborting on the named term.
- **The effect description becomes a typed per-term table.** The fit's
  `names` component — today a character matrix with *conditional* columns,
  which is why fixedness is a `"TRUE"`/`"FALSE"` string — becomes a
  data.frame with a stable schema (every column always present, `fixed` a
  plain logical). `tidy()` and the summary details table read typed
  columns. No stamp bump: the format-version machinery is unreleased, so
  the current epoch is the 2.0.0 epoch and unstamped legacy fits are
  already refused by the guard's missing-stamp branch.
- **Dead plumbing retired.** The internal `excludeParameters` argument is
  never passed by the estimation front end and would silently shift fixed
  positions if it ever were (the assembler is exclusion-unaware); it is
  removed from the internal signatures outright (dev-only, no lifecycle).
- **No breaking user-surface change.** `offset()` / `offset_coef` stay the
  authoritative interface for fixing (the named forms are additive); the
  superseded `fixed_parameters` argument keeps its lifecycle path exactly
  as the deprecation-scope audit (backend-parity design D11) settled — it
  is public surface (CRAN + v1.7.0) and folds into the same `fixed_spec`
  at the same assembler.
- **No numerics.** The masking and seeding arithmetic is unchanged — the
  frozen 1e-6 coefficient baselines PASS (not SKIP) under `NOT_CRAN=true`
  is the proof on every commit.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `offset-fixed-terms`: the "offset coefficients supplied via offset_coef"
  requirement currently mandates assembling "the existing positional
  `fixedParameters` vector"; it is restated around the structured contract
  (assembled once, consumed everywhere through one helper, errors naming
  terms), and extended with the formula-side `offset(term, coef =)` value
  carrier — required for multi-flavor specifications, coexisting with
  `offset_coef` (conflict aborts) for single-process models. A new
  requirement covers the fit-side record: fixedness stored as a logical in
  the typed per-term effect-description table; a fit carrying only the
  string-encoded column is refused through the result-format guard with
  re-fit guidance (aligned with the snake-case-result-components no-window
  policy).
- `optimizer-selection`: a new requirement — initial parameter values
  align to terms by name: `initial_parameters` accepts a named partial
  vector matched against coefficient labels alongside the unchanged
  full-length positional form; seeding non-intercept terms preserves the
  rate intercept's warm start; on a multi-flavor specification a flat
  named vector broadcasts to every process whose labels match, and a
  nested flavor/family list targets one process.

## Impact

- **goldfish R**: `formula_validate.R` (`assemble_fixed_parameters()`
  return shape + the name resolver), `set_opt.R` (named-form input
  validation for `initial_parameters` / `offset_coef`), `cpp_interface.R`
  (decode block, the `[1]` intercept gate and the `is.null(initial)` gate,
  maxLik masking wiring), `estimation_core.R` (duplicate decode block),
  `model_estimate.R` (pass-through and the fit's fixed record),
  `utils.R` (`GetDetailPrint()` returns the typed table; `GetFixed()`;
  `term_label()` unchanged as the label authority), `methods_display.R` /
  `methods_postestimate.R` (table consumers: summary details, `tidy()`,
  `vcov()`), `estimate_flavored.R` only through the per-fid resolver.
- **Tests**: existing offset/fixed suites migrate mechanically; new unit
  tests for the contract objects (index validation, likelihood-only,
  intercept-fixed/intercept-seeded predicates, term-named errors, the
  name resolver on collision-deduped labels), flavored value-channel
  tests on the two-flavor fixture, and a format-guard test on a
  legacy-shaped fit.
- **Sequencing**: strictly after `backend-parity` (whose vocabulary push
  and shared-helper pattern this follows) and before `residuals-gof` task
  2.1, so `evaluate_model()` is born consuming `fixed_spec`. Coordination
  is one-way: residuals-gof's evaluator task reads this change's contract;
  no spec requirement is modified by both changes.

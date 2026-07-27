# Tasks — fixed-parameter-contract

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline whenever roxygen /
exports / signatures change, `air format` the touched R files before `lintr`,
`NOT_CRAN=true` with the frozen 1e-6 baselines PASS (not SKIP) before each
commit — this change is pure R with no numerics, so the baselines are the
standing proof. r-lib skills (r-package-development, testing-r-packages, cli)
before the work they cover. **Do not start before `backend-parity`'s task list
is complete** (design D4: same estimator files, sequential edits only) —
satisfied, it archived 2026-07-26; the operative spine gate is now
**revise-gather-output completing** (one change at a time on the spine),
and the hard downstream deadline is unchanged: land before residuals-gof
task 2.1. Scope expanded 2026-07-27 (D5–D7): the change owns fixing AND
seeding.

## 1. The contracts

- [ ] 1.1 `fixed_spec` constructor + validation in `formula_validate.R`:
      `assemble_fixed_parameters()` returns `NULL` or the structured object
      (`idx` / `values` / `names`), converting the superseded
      `fixed_parameters` input at the same site; cli errors name terms (the
      offset_coef-length spec scenario). Unit tests: idx/values against
      formula fixtures with and without intercept and with interactions;
      the no-offset and all-fixed shapes; term-named error snapshots.
- [ ] 1.2 `offset(term, coef =)` in the parser + assembler (design D7):
      ground first at formula_parser.R:1115-1118 — the offset marker is
      recorded as indices into the terms' variables with the inner call
      unwrapped, so verify how that unwrap treats a two-argument call
      today before extending it; then the parser accepts and threads the
      per-term value; the
      assembler folds it into `fixed_spec` at the same site as
      `offset_coef` values, with the both-sources conflict aborting on
      the named term; an `offset()` term with a value from neither source
      aborts as today. Unit tests: coef-carrying formulas parse and fix
      correctly with/without intercept; conflict abort snapshot;
      `offset_coef`-only path byte-identical to HEAD.
- [ ] 1.3 The name resolver + `initial_spec` (design D6): one internal
      resolver at the assembler's site matches named `initial_parameters`
      entries (and single-process named `offset_coef`) against the fit's
      deduped coefficient labels (`term_label()` authority — note the
      front-end order wrinkle in Context: the label build at
      model_estimate.R:2074 currently runs AFTER the assembly at :2051,
      so move it above or derive labels in the resolver) and emits an
      `initial_spec` with the D1 shape; unnamed full-length positional
      `initial_parameters` passes through with its exact current behavior;
      unnamed partial length aborts naming the expected length; an unknown
      name aborts listing the available labels. `set_opt.R` input
      validation admits the named and nested-list forms. Unit tests
      include collision-deduped labels and the positional pass-through.
- [ ] 1.4 The shared masking helper (estimation-side, design D2): takes
      `(fixed_spec, initial_spec, n_params)`, validates both `idx` sets
      once; yields the unfixed index vector, the seeded parameter vector
      (seed first, fixed overwrites — order stated in its docs),
      `likelihood_only`, `intercept_fixed`, `intercept_seeded`. Unit tests
      at helper level, including the out-of-range index abort naming the
      term and the seed-then-fix overwrite order.

## 2. The consumers

- [ ] 2.1 `estimate_c_int()` consumes the helper: duplicated decode block
      (cpp_interface.R:84-114) deleted; the Poisson intercept warm-start
      gate reads `!intercept_fixed && !intercept_seeded` instead of
      `is.na(fixedParameters[1])` / `is.null(initialParameters)` — a named
      seed of a non-intercept term keeps the warm start, a full positional
      vector disables it exactly as today; maxLik adapter wiring
      (`id_fixed`) fed from the same decode. Existing offset/optimizer
      suites green; `NOT_CRAN=true` baselines PASS.
- [ ] 2.2 `estimate_int()` consumes the helper: duplicate block
      (estimation_core.R:84-114) deleted. Cross-backend offset fixture
      agreement unchanged (the every-backend spec scenario).
- [ ] 2.3 `model_estimate.R` pass-through: the estimator front-end hands
      `fixed_spec` + `initial_spec` down in place of the two positional
      vectors end to end; no remaining internal producer or consumer of
      the sentinel encoding (`git grep` gate on the decode idioms). Retire
      `excludeParameters` from every internal signature that carries it —
      the two estimators plus preprocess_writers.R:433 and
      process_state_evaluators.R:45 (design D2: never passed by the front
      end, exclusion-unaware alignment; dev-only, no lifecycle) —
      `git grep` gate on the name.
- [ ] 2.4 The flavored value channel (designs D6/D7): offsets come from
      each fid's own formula (`coef =`) — `offset_coef` and the
      superseded positional `fixed_parameters` supplied alongside
      a flavored specification abort with guidance, and the
      mixed-presence abort disappears (a fid without `offset()` simply
      fixes nothing); initials resolve per fid — flat named vector
      broadcasts to matching labels, the nested flavor/family list targets
      one process (keys validated against the process map), positional
      aborts. Tests on the four-fid fixture (two flavors × rate/choice,
      the same windowed term in every formula): four distinct `coef =`
      values land on the right fids; a one-flavor-offset specification
      estimates; broadcast initials seed every fid's matching labels while
      rate intercepts keep the warm start; nested-list initials touch only
      the targeted fid; the `offset_coef`-with-flavored and
      positional-with-flavored abort messages.

## 3. The fit record

- [ ] 3.1 `GetDetailPrint()` returns the typed per-term data.frame (design
      D5): stable schema (all columns always present, `""`/`NA` when
      unused), `fixed` a plain logical; `GetFixed()` reads it and the
      string-column `eval(parse())` path is removed outright; NO stamp
      bump — a string-column-only fit is unstamped by definition and the
      guard's missing-stamp branch already refuses it with re-fit
      guidance (design D3 as corrected 2026-07-27; epochs frozen until
      2.0.0). Consumer sweep gated by `git grep` on `$names` /
      `names[, `: `compact_term_strings()` including its token builder's
      `Fx` badge (`.isFixedToken(row[["fixed"]])`, utils.R:957 — becomes
      a plain logical test), `term_label()`, the
      `compact = FALSE` summary details table, `tidy()`, `vcov()`, and
      the `effect_description` sites (preprocess_export.R:214,
      formula_parser.R:454). Tests: printed fixed coefficient marked from the
      logical on a fresh fit; a legacy-shaped fit aborts through the
      format guard, not `GetFixed()`; `GetFixed()` return type unchanged
      for all five consumers; `tidy()` output identical modulo types.

## 4. Closure

- [ ] 4.1 Coordination note into `residuals-gof` task 2.1 (its
      `evaluate_model()` consumes `fixed_spec` — one-line task-text
      amendment there, per design D4); roxygen for `offset(term, coef =)`
      where `offset()` is documented and for the named/nested forms on
      `set_opt.R` (`initial_parameters`, `offset_coef`) and touch-ups
      where the internal contract is mentioned; NEWS entry naming the
      additive named surfaces, the flavored value-channel fix, and the
      term-named errors; DESCRIPTION bump per milestone convention. Full
      `NOT_CRAN=true` suite green, baselines PASS.

## 0. Working notes

- [ ] 0.1 Create `findings.md` in this change directory; append a short note
      after each completed task (what was done, surprises, decisions, test
      state). Keep it updated through the whole change (local-only, gitignored).

## 1. Effect inventory (all three families)

- [ ] 1.1 Enumerate every implemented effect across `R/functions_effects_*.R`
      (DyNAM rate/choice/choice_coordination, REM rate/choice/rate_ordered,
      DyNAMi rate/choice): canonical name, `(model, sub_model)` variants, the
      `init_*`/`update_*` symbols, and `stat_kind`.
- [ ] 1.2 For each effect, capture its **current** argument set (from `formals()`
      and the formula parser), including which arguments gate runtime `if/else`
      (e.g. `weighted`, `history`, `subType`, `joining`, `type`).
- [ ] 1.3 For each effect, capture its **current effective validity** (allowed
      directionality/mode/model/sub_model/interaction) derived from existing
      checks, the test suite, and the baselines — this seeds the initial strict
      validity declarations (D12). Per D17 this matrix is a **transitional
      migration artifact only**: it verifies the seeded `term_def` entries and
      is not maintained afterwards.
- [ ] 1.4 For each effect, capture current metadata: short name (from
      `.goldfishEffectShort`), `@aliases`, endogenous/exogenous twin and object
      defaulting (e.g. `inertia`↔`tie`, `trans`↔`closure_ff`), and its `family`
      per the settled taxonomy (closure/dyadic/degree/attribute/
      attribute+structural). Record the catalogue in `findings.md` and
      cross-check it against `.plan/effect_refactor_proposal.tex` (update the
      tex where the inventory contradicts it).
- [ ] 1.5 **Pre-implementation review gate.** Validate the remaining
      code-grounded facts before writing any code: confirm where `stat_kind` is
      assigned and whether per-effect; the exact broadcast-eligibility
      predicate; the full `stop()`/`cli_abort()`/`warning()` inventory feeding
      the strict validity declarations (incl. `opportunities_list` in
      `estimation_core.R`); and the `refactor-single-data-object` sequencing
      decision (object-by-name / focal-layer / panel-window validity). Record
      confirmations/corrections in `findings.md` and update `design.md`
      (D2/D3/D12) before starting group 2.

## 2. Registry data structure and public API (D1, D2, D9, D11, D18)

- [ ] 2.1 Define the `term_def` schema (D2 + D18): identity/taxonomy,
      presentation, endogenous/exogenous pairing, recipes, validity, argument
      schema, **plus the reserved Layer-2/3 surface**: `wraps` (wrapper-of:
      target term + pinned/mapped args), `cache_spec` (field catalogue with
      argument-value activation predicates, D14), the `after_event_stream`
      recipe hook slot (D22), and the reserved argument vocabulary (`open`,
      `retain`, `combiner_fn`, `normalizer_fn`, `summarizer_fn`, `levels`) —
      documented and validated but unused by Layer-1 code. Choose the
      representation (list + validator vs class) per the open question.
- [ ] 2.2 Implement the internal registry store (frozen environment after load)
      and `get_term_def()` resolving canonical name and `aliases`, with a
      "did you mean" error for unknown names.
- [ ] 2.3 Implement and `@export` the public `register_term()` with a validator
      that rejects malformed `term_def`s naming the offending field; add and
      `@export` listing/search accessors (by name, alias, family) — the
      `search_effects()` surface (D21).
- [ ] 2.4 Add unit tests: schema validation (accept/reject, incl. reserved
      fields), lookup by name/alias, listing/search, unknown-name suggestions.
- [ ] 2.5 Run `devtools::document()` inline (new exports); verify
      `NOT_CRAN=true` tests for the new file PASS; update `findings.md`; commit.

## 3. Term constructor with adapter recipes (D3, D8, D16)

- [ ] 3.1 Implement `construct_term(term_def, formula_args, context)` returning a
      constructed-term object; seed every registry entry with **adapter recipes
      that reference the existing `init_*`/`update_*` functions unchanged** so
      numerics are identical.
- [ ] 3.2 Implement strict validation (D12) in the constructor: unknown argument,
      out-of-range value, and invalid context (model/sub_model/mode/direction/
      interaction) each raise a consistent `cli` error.
- [ ] 3.3 Implement endogenous `object_default` injection and the exogenous
      missing-object error (D7); set `is_two_mode`/`directed` authoritatively
      from object attributes (replacing the parser inference warning).
- [ ] 3.4 Implement the constructed term's `build_plan` field (D16): move the
      parser's ad-hoc window-object creation into build-time promises the
      preprocessing phase fulfills (window now; `retain`/categorical slots
      reserved for Layer 3). Resolve `cache_spec` activation predicates into
      the active cache-field set.
- [ ] 3.5 Add unit tests for construction output, strict-validation errors,
      endogenous/exogenous object handling, and build-plan/window promises.
- [ ] 3.6 Verification: `NOT_CRAN=true` tests PASS; update `findings.md`; commit.

## 4. Route the parser through the registry/constructor (D8 phase 1)

- [ ] 4.1 Replace string-built `update_*`/`init_*` discovery in
      `parse_multiple_effects()` (`R/formula_parser.R`) with
      `get_term_def()` + `construct_term()`; feed the resolved references into
      the existing parser outputs / `build_update_plan()` `effects` registry.
- [ ] 4.2 Ensure the strict validity declarations (1.3) reproduce current accept/
      reject behaviour: every formula that estimated still constructs, every one
      that errored still errors.
- [ ] 4.3 Verification: full `NOT_CRAN=true Rscript -e 'devtools::test()'` —
      coefficient + global baselines and cpp golden report PASS (not SKIP) and
      nothing regressed. Update `findings.md`; commit.

## 5. Registry-driven metadata; retire the hard-coded map (D6)

- [ ] 5.1 Point `GetDetailPrint()`/`.decoderColumns()` and the compact-term
      builder (`compact_term_strings`, `.shortEffect`) at `term_def`
      `short`/`abbrev`/`family`; seed identical strings for existing effects.
- [ ] 5.2 Remove `.goldfishEffectShort` once unused; confirm no references
      remain.
- [ ] 5.3 Add/adjust tests: short/abbrev/family come from the registry; reuse the
      `compact-term-summary` tests as the regression gate.
- [ ] 5.4 Verification: `NOT_CRAN=true` tests PASS; update `findings.md`; commit.

## 6. Encoding benchmark spike (D15 — gates group 7)

- [ ] 6.1 Co-write the benchmark scripts in `.plan/bench/` (never committed):
      synthetic event-stream generator parameterized by actors × events, with
      **prefix reuse** (one max-length stream per actor size, sliced for
      smaller event counts).
- [ ] 6.2 Implement the four variants for the two cases — `tie(weighted=)` and
      `trans(history=)` — plus a deliberately flattened all-if-else `trans`:
      (a) current if/else, (b) stored-fn-per-arg, (c) construction-time
      specialized closure, (d) pre-transformed/binarized cache. Include both
      cache containers (single-reference named list vs environment-backed) in
      the (c)/(d) variants.
- [ ] 6.3 Run the grid with `bench::mark()` (time + `mem_alloc`): actors
      {10, 100, 500, 1000, 3000} × events {1e2, 1e3, 1e4, 1e5}; skip
      ≥3000 × ≥1e4 except capability probes; replicates tiered 10/5/3/1 by
      runtime; 5h wall-clock cap; auto-skip variant×combo over ~30 min.
      **User runs on HPC** and adds real-dataset end-to-end checks (local
      paths).
- [ ] 6.4 Analyze results together (judgment call weighted toward large
      combos): pick the default mechanism, the per-argument (d) exceptions,
      and the cache container. Record the decision + curves summary in
      `findings.md` and update `design.md` (D14/D15) before starting group 7.

## 7. Migrate argument encoding effect-by-effect (D4, D14)

- [ ] 7.1 Wrap each effect's cache as `list(stat = <old cache>)` while
      migrating it (same pass, numerics-neutral); route access through the
      resolved `cache_spec` active-field set.
- [ ] 7.2 Encode `weighted` at construction per the benchmark decision
      (factory and/or binarized cache); refactor the affected
      `init_*`/`update_*` bodies to read the encoded transformer instead of
      branching; re-run baselines.
- [ ] 7.3 Encode `history` to the selected subroutine; refactor the closure
      effects to invoke it without branching; re-run baselines.
- [ ] 7.4 Encode the remaining gating arguments (`type`, `subType`, `joining`,
      DyNAMi-specific) per effect, re-running baselines after each effect.
- [ ] 7.5 Add tests asserting the update path no longer branches on the encoded
      arguments (e.g. construction stores the transformer/subroutine) and that
      only active cache fields are created/updated.
- [ ] 7.6 Verification: full `NOT_CRAN=true` run after the encoding migration;
      baselines PASS to 1e-6 on both engines. Update `findings.md`; commit
      (per-effect commits where practical).

## 8. Family coverage parity: REM and DyNAMi (D10)

- [ ] 8.1 Confirm every REM (rate/choice/rate_ordered) effect has a registry
      entry with correct validity/metadata and routes through the constructor;
      baselines PASS.
- [ ] 8.2 Confirm every DyNAMi (rate/choice) effect — including the bespoke
      joining/subType/history handling — has an entry and routes through the
      constructor; declare the `wraps` mapping (tertius + summarizer/
      normalizer/transformer vocabulary, per the tex mapping table) as
      *metadata only* (implementation belongs to `refactor-dynami-engine`);
      add targeted tests; baselines/tests PASS.
- [ ] 8.3 Add a coverage test asserting no implemented effect lacks a registry
      entry (enumerate `init_*`/`update_*` vs registered names).
- [ ] 8.4 Reorganize effect files by family taxonomy (D13/tex):
      `effects_closure.R`, `effects_dyadic.R`, `effects_degree.R`,
      `effects_attribute.R`, `effects_attribute_structural.R`, registration
      co-located; delete the per-model files once empty. Pure moves, no logic
      changes; baselines PASS.
- [ ] 8.5 Verification: full `NOT_CRAN=true` run; update `findings.md`; commit.

## 9. Public API documentation

- [ ] 9.1 Roxygen for `register_term()` and the accessors (the `term_def` schema
      as a documented, versioned stability surface, incl. reserved fields);
      run `devtools::document()`.
- [ ] 9.2 Add a vignette/section showing how to register a custom effect
      end-to-end (definition → formula use), built via the precompile flow.
- [ ] 9.3 Verification: `R CMD check`/`devtools::check()` doc checks clean;
      update `findings.md`; commit.

## 10. Finalize

- [ ] 10.1 Full `NOT_CRAN=true Rscript -e 'devtools::test()'`; confirm baselines +
      cpp golden PASS (not SKIP) and nothing regressed across all three families.
- [ ] 10.2 Run `lintr::lint_package()` on touched files and resolve issues.
- [ ] 10.3 Bump the version in `DESCRIPTION` and add a `NEWS.md` entry (new public
      registration API, registry-driven metadata, constructor-time argument
      encoding). Commit the version/NEWS bump.
- [ ] 10.4 Final `findings.md` summary entry (commit hashes, test state,
      follow-ups, any deferred open questions); hand-off notes for the
      successor changes `effect-naming-scheme` and
      `effect-statistic-extensions`.

## 11. Window argument validation and units UX (added 2026-07-13, from refactor-single-data-object review)

- [ ] 11.1 Validate character windows against the time axis: the "number units" parser
      (`utils.R:866`) converts units to seconds, which is only correct when
      `as.numeric(time)` is in seconds (POSIXct axis) — abort with a cli error when a
      character window is used on a plain-numeric or Date time axis, explaining the
      supported combinations
- [ ] 11.2 Accept `difftime` (base R, exact unit conversion via its units attribute) and
      lubridate `duration` (numeric seconds) as window values alongside numeric (raw
      axis units) and character; document that POSIXct fractional seconds cover
      sub-second resolution (no new time class; nanosecond-scale data enters as a
      numeric axis the user owns)
- [ ] 11.3 Tests: character window on numeric axis aborts; difftime/duration windows
      equal their numeric-seconds equivalents to machine precision; snapshot the cli
      errors

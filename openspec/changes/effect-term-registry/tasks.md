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
- [ ] 1.2b **Inventory the argument names the parser silently discards**
      (added 2026-08-19; ADR-0021). Two independent paths drop an unrecognised
      named argument without counting or reporting it: `pmatch()` in
      `parse_multiple_effects()` (`R/formula_parser.R`) returns `NA` for an
      unknown name and `na.omit()` discards it, and `get_data_objects()`
      (`R/utils.R`) keeps only unnamed or reserved-named arguments. Capture
      both so the strict check (3.2) knows what it is newly rejecting.
      - **Retired names**, from `git diff v1.6.12..HEAD` over the `@param`
        tags: `transformFun` -> `transformer_fn`, `isTwoMode` ->
        `is_two_mode`, `aggregateFun` -> `summarizer_fn`, `ignoreRep` ->
        `ignore_repetitions`, `subType` -> `sub_type`. All five are dropped in
        silence today; none is named in `NEWS.md`.
      - **Corpus sweep** of every formula in `tests/`, `R/` and the
        `.Rmd.orig` vignettes, comparing each term's supplied named arguments
        against the union of `formals()` for its `update_*` symbols (excluding
        the parser-level `ignore_repetitions` and `window`). Measured
        2026-08-19: **21 distinct sites, 72 occurrences, all `subType`** — no
        `transformFun` and no prefix-matched abbreviation anywhere in the
        repo. Sites: `vignettes/dynami-example.Rmd.orig` (`ego`, `diff`,
        `same`, `tie`, `size`, `egopop`, `alter`, `alterpop`, `inertia`),
        `test-dynami_baselines.R`, `test-dynami_surface.R`,
        `test-compute_statistics.R`, `test-dynami_bridge.R`,
        `test-residuals_recompute.R`. This list is the enumeration 4.2's
        carve-out refers to; keep it in `findings.md`.
      - Note the sweep also has to consider `pmatch`'s *accepting* half: it
        matches unambiguous prefixes, so `transformer = sqrt` and
        `weight = TRUE` bind today and would newly error under exact matching.
        The corpus contains none, but user code may. Feeds D25.
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
      The argument schema's `allowed` field is the choice-set source of truth
      the constructor resolves against (D24) — the `type` / `history` / `joining`
      enumerations currently living as `match.arg` defaults in the recipe bodies.
      Reserve two further per-argument fields: `aliases` (accepted spellings
      that resolve to the canonical name) and `deprecated` (a retired spelling
      that resolves *and* signals). The 1.7.0 camelCase renames left five
      retired argument names with no home — 1.2b lists them — and Layer 2's
      `effect-naming-scheme` only plans lifecycle handling for the renames it
      will itself make. Declared and validated here; whether the retired five
      are wired to `deprecated` or simply rejected is D25's call.
- [ ] 2.2 Implement the internal registry store (frozen environment after load)
      and `get_term_def()` resolving canonical name and `aliases`, with a
      "did you mean" error for unknown names.
- [ ] 2.3 Implement and `@export` the public `register_term()` with a validator
      that rejects malformed `term_def`s naming the offending field; add and
      `@export` listing/search accessors (by name, alias, family) — the
      `search_effects()` surface (D21).
- [ ] 2.4 Add unit tests: schema validation (accept/reject, incl. reserved
      fields), lookup by name/alias, listing/search, unknown-name suggestions.
- [ ] 2.6 Implement the D23 mode-signature interpreter: evaluate a term's
      `mode_signature`/`attr_reads` against the mode map (mode-set
      conformability, type-resolved non-degeneracy, per-slice attribute
      definedness) as the single validity check; derive validity groups from
      signature shapes for docs/tests/error text; seed and verify against the
      `multimode-network-support` corrected taxonomy and its table-driven
      boundary test (that change's D4/D10).
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
      Cover the argument-name paths specifically (D25): an unknown name, a
      retired name from 1.2b, a near-miss that should suggest a correction, and
      an unambiguous prefix (`transformer` for `transformer_fn`) — the last
      asserting whatever D25 settles. Snapshot the messages.
- [ ] 3.6 Verification: `NOT_CRAN=true` tests PASS; update `findings.md`; commit.

## 4. Route the parser through the registry/constructor (D8 phase 1)

- [ ] 4.1 Replace string-built `update_*`/`init_*` discovery in
      `parse_multiple_effects()` (`R/formula_parser.R`) with
      `get_term_def()` + `construct_term()`; feed the resolved references into
      the existing parser outputs / `build_update_plan()` `effects` registry.
- [ ] 4.2 Ensure the strict validity declarations (1.3) reproduce current accept/
      reject behaviour: every formula that estimated still constructs, every one
      that errored still errors — **except for the one intended break** below.
      - **Carve-out (added 2026-08-19).** An argument the parser currently
        discards in silence is *meant* to start erroring; that is the point of
        D12, and read without this clause the no-newly-broken gate forbids it.
        The break is bounded, not open-ended: the newly-rejected set is
        enumerated in advance by 1.2b, and any formula rejected by the
        constructor that is **not** on that list is a regression, not an
        intended tightening. Verify the gate that way — diff the reject set
        against 1.2b's inventory — rather than by asserting nothing new errors.
      - The same carve-out does **not** extend to values or contexts. A value
        or context that is accepted today must still be accepted, since those
        were genuinely bound and genuinely used.
- [ ] 4.2b **Migrate the corpus and re-freeze the two DyNAM-i intercepts**
      (ADR-0021). Correct all 21 `subType` sites from 1.2b to `sub_type`, in
      the same commit as the strict check that forces them, so no commit in
      history has a baseline whose formula does not produce it.
      - **20 of the 21 move no number**: eight of the nine affected effects
        request their own default (`diff` -> `averaged_sum`, `same` ->
        `proportion`, `tie` -> `proportion`, `egopop` -> `normalized`, …), so
        binding the argument changes nothing.
      - **`ego` is the exception.** `test-dynami_baselines.R` asks for
        `"centered"` against a default of `"identity"`, and both are real
        branches of `update_DyNAMi_rate_ego()`. The frozen M1 rate baseline was
        therefore computed on *uncentered* age. Correcting it moves
        `Intercept` and `intercept` and nothing else: seven slopes stay
        bit-identical and `logLik` stays `-1306.3199849182` to all fourteen
        figures.
      - **The new numbers are derived, not observed.** With `m = mean(age) =
        322/11`, `d(Intercept) = b_leave * m = 0.81839842` and
        `d(intercept_join) = (b_join - b_leave) * m = 0.98735328`, matching the
        measured `0.81839846` / `0.98735312` to the rounding of the
        eight-decimal coefficients. Commit that derivation beside the diff —
        ADR-0021 permits the re-freeze *because* the value was computed first,
        and a baseline diff with no derivation beside it is what it forbids.
      - Re-knit `vignettes/dynami-example.Rmd.orig`; its printed intercepts
        move by the same amounts.
      - Leaves the global/coefficient baselines and the cpp golden untouched;
        confirm with `git diff --stat` over `_baselines/`.
- [ ] 4.3 Verification: full `NOT_CRAN=true Rscript -e 'devtools::test()'` —
      coefficient + global baselines and cpp golden report PASS (not SKIP) and
      nothing regressed. Update `findings.md`; commit.

## 5. Registry-driven metadata; retire the hard-coded map (D6)

- [ ] 5.1 Point `GetDetailPrint()`/`.decoderColumns()` and the compact-term
      builder (`compact_term_strings`, `.shortEffect`) at `term_def`
      `short`/`abbrev`/`family`; seed identical strings for existing effects.
      **Downstream consumer added 2026-07-29 (`residuals-gof` D25):**
      `residuals-gof` exports `model_terms(fit, pattern = NULL)`, a
      **fit-scoped** helper returning a fit's terms as a tibble (effect-detail
      columns + compact string + `coef()` label + export form + coefficient
      index; `flavor`/`family` appended on a flavored fit), and routes every
      user-facing term argument — `initial_parameters`,
      `diagnose_*(effect =)`, `test_*(effects =)` — through one matcher that
      renders candidates as compact strings. Repointing the builder here MUST
      keep those strings byte-identical, since they are now a selection key and
      not only display: `compact-term-summary`'s tests are the regression gate
      for the display side, and `residuals-gof`'s term-vocabulary scenarios for
      the selection side. Do NOT confuse the two lookups —
      `search_effects()` / `register_term()` (task 2.3) are **package-scoped**,
      answering which effects goldfish provides; `model_terms()` answers which
      terms a given fit has. They meet only at this builder, so no consolidation
      is owed in either direction.
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
- [ ] 7.2b **Signal an argument the encoding renders inert** (ADR-0007, user,
      2026-07-31). Encoding `weighted` is the moment a supplied
      `transformer_fn` is thrown away: `weighted = FALSE` resolves to the
      binarising transformer, and the user's function never reaches the recipe.
      Today that is silent — `inertia(net, transformer_fn = log1p)` fits
      **identically** to `inertia(net)`, same log-likelihood and same
      coefficients (verified on `social_evolution`; the coefficient equality is
      the tell, an applied-then-absorbed transform would have scaled it by
      `1/log(2)`). Warn at construction, naming the term, the inert argument
      and the remedy (`weighted = TRUE`).
      - **Not a numerics fix**: the short-circuit is correct. Any monotone
        transform of a 0/1 indicator is an affine rescaling the coefficient
        absorbs, so applying it would change no fit and only muddle the
        coefficient's units. What is wrong is that the user is not told their
        request was dropped.
      - **Why it matters downstream**: `test_gof()` / `test_time()` read
        whether a contribution is spread evenly over the sequence, so a binary
        statistic gives them no functional form to detect. Measured in
        `residuals-gof` 4.8: the same misspecified reciprocity effect is caught
        at 0.72 on a weighted statistic and at 0.04 — the null — on a binary
        one. The silent drop and the blind test compound.
      - **Implement as a schema property, not a per-effect check** (D2f), so it
        generalises: this is one instance of "an argument accepted by the
        schema that another argument's value renders inert", and `history`,
        `subType` and `joining` are the places to look for more. Per ADR-0007's
        open question, consider deriving it by comparing the encoded recipe
        against the supplied arguments — anything that did not survive encoding
        — which needs no new schema vocabulary and catches undeclared cases.
      - Tests: the warning fires on `weighted = FALSE` with a
        `transformer_fn`, is silent on `weighted = TRUE`, and is silent when
        `transformer_fn` is not supplied; snapshot the message.
      - Pairs with `residuals-gof` task 6.0b, the vignette section explaining
        the power consequence, which can then point at this warning.
- [ ] 7.2c **`effect(named_arg = value)` with no object dies in the parser**
      (found 2026-07-31 while writing the residuals-gof diagnostics vignette).
      `calls ~ inertia(weighted = TRUE)` aborts with
      `strsplit(): non-character argument`;
      `calls ~ inertia(calls, weighted = TRUE)` and `calls ~ inertia` both work.
      Mechanism: in `get_data_objects()` (`R/utils.R`) the named-parameter
      filter drops every argument, leaving `objNames` zero-length, so
      `ifelse(areList, ...)` returns `logical(0)` and `strsplit()` rejects it.
      - The right fix is **not** the one-line guard. An endogenous effect
        already defaults its object to the dependent layer when written bare,
        so `inertia(weighted = TRUE)` should resolve that default and then
        apply the argument — which is exactly D2(c) `object_default` resolved
        at construction (D3 step 2), before any argument parsing.
      - Until then the message is the worst part: it names `strsplit`, points
        at no term, and suggests nothing. The registry's argument schema (D2f)
        owns making it one actionable error.
      - Tests: the bare, object-named and named-argument-only forms all parse
        to the same term when the object is the dependent layer -- the equality
        is the point, not merely that the third stops erroring.
      - **Written up as ADR-0009** (proposed), which records the breadth
        measurement (five formulas across `inertia`/`recip`/`trans`/`indeg` and
        both `weighted` and `window`, all failing identically) and rejects the
        one-line guard explicitly: it would convert a loud type error into a
        silent divergence between `inertia` and `inertia(weighted = TRUE)`.

- [ ] 7.3 Encode `history` to the selected subroutine; refactor the closure
      effects to invoke it without branching; re-run baselines.
- [ ] 7.4 Encode the remaining gating arguments (`type`, `subType`, `joining`,
      DyNAMi-specific) per effect, re-running baselines after each effect.
      **Fold in the argument *resolution* (D24), not only the encoding**: the
      constructor runs `arg_match` against each argument's `allowed`, so the
      `update_*`/`init_*` bodies stop calling `match.arg(type)` /
      `match.arg(history)` and stop reading a raw choice-set default. This
      absorbs `multimode-network-support`'s transitional `resolve_effect_args()`
      (its D14) — the parser-seam resolver is superseded by the registry's
      `allowed`, so the two-mode length-2 init crash cannot recur. Assert an
      out-of-set value (`type = "bogus"`) aborts from the constructor with a cli
      error naming the effect, argument, and allowed set.
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

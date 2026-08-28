## 0. Working notes

- [ ] 0.1 Create `progress.md` in this change directory; append one section per
      session (tasks done, commit hashes, findings, test state, what is next).
      Local-only, gitignored.

## 1. Inventory (D3)

- [ ] 1.1 Capture the retired argument names by diffing `@param` tags across
      `v1.6.12..HEAD`: `transformFun` -> `transformer_fn`, `isTwoMode` ->
      `is_two_mode`, `aggregateFun` -> `summarizer_fn`, `ignoreRep` ->
      `ignore_repetitions`, `subType` -> `sub_type`. Confirm none is named in
      `NEWS.md`, and check for retired names beyond the five (open question).
- [ ] 1.2 Re-run the AST corpus sweep over every formula in `tests/`, `R/` and
      the `.Rmd.orig` vignettes, comparing supplied named arguments against the
      union of `formals()` for each effect's `update_*` symbols (excluding the
      parser-level `ignore_repetitions` and `window`). Baseline measurement
      2026-08-19: **21 distinct sites, 72 occurrences, all `subType`**; no
      `transformFun` and no prefix use. Record the site list in `progress.md` —
      it is the enumeration the migration and the tests check against.

## 2. Exact matching and reporting (D2, D3, D4)

- [ ] 2.1 Replace `pmatch()` in the signature binder (`R/formula_parser.R`) with
      exact matching; an unmatched name raises a `cli` error naming the term,
      the argument, and the accepted names.
- [ ] 2.2 Close the second drop path in `get_data_objects()` (`R/utils.R`), so an
      unrecognized *named* argument cannot vanish on the object-resolution side
      either.
- [ ] 2.3 Add the retired-name map as a flat named character vector keyed by the
      retired spelling, shaped so `effect-term-registry` task 2.1 can absorb it
      into the per-argument `deprecated` field without a second migration. The
      error names the replacement.
- [ ] 2.4 Add the edit-distance suggestion for an unmatched name that is not
      retired (`transform_fn` -> `transformer_fn`).
- [ ] 2.5 Tests: unknown name, each of the five retired names, a near-miss
      suggestion, an unambiguous prefix, and an accepted-name control that must
      keep parsing. Snapshot the messages under a pinned cli context.
- [ ] 2.6 `air format` the touched R files; `NOT_CRAN=true` tests PASS; update
      `progress.md`; commit.

## 3. Corpus migration (ADR-0032)

- [ ] 3.1 Migrate the 20 `subType` sites that request their effect's own default
      — `test-compute_statistics.R`, `test-dynami_bridge.R`,
      `test-dynami_surface.R`, `test-residuals_recompute.R`, and the eight
      non-`ego` terms in `vignettes/dynami-example.Rmd.orig`. No number moves;
      confirm that.
- [ ] 3.2 Re-knit `vignettes/dynami-example.Rmd.orig` (install first — vignettes
      knit against the *installed* goldfish; `vignettes/precompile.R` only
      defines helpers).
- [ ] 3.3 Verification: `NOT_CRAN=true` tests PASS with every baseline reporting
      PASS not SKIP; update `progress.md`; commit.

## 4. The one baseline correction (D5, ADR-0021)

- [ ] 4.1 Correct `ego(age, joining = ±1, subType = "centered")` to `sub_type` in
      `test-dynami_baselines.R` (lines ~26-27 and ~126-127) and re-freeze the two
      inline intercepts. **These are inline numeric literals in the test file,
      not entries under `_baselines/`** — the `regen-baselines` skill governs the
      `.rds` floor and is not the route here.
      - Expected: `Intercept` -6.76131450 -> -5.94291604, `intercept`
        1.52230722 -> 2.50966034. All seven slopes and
        `logLik = -1306.3199849182` unchanged.
      - Derivation, computed from the *old* baseline before running anything:
        `m = mean(age) = 322/11`, `d(Intercept) = b_leave * m = 0.81839842`,
        `d(intercept_join) = (b_join - b_leave) * m = 0.98735328`.
      - Land in the same commit as the check that forces it, so no commit in
        history holds a baseline its own formula does not produce.
- [ ] 4.2 Record the derivation, the surviving invariants, and the reason in
      `tests/testthat/_baselines/README.md`, plus a note that the README is the
      provenance record for the inline test baselines too.
      **Blocked**: the `PreToolUse` guard denies Edit/Write anywhere under
      `_baselines/`. Narrowing it is ADR-0018's standing open question and must
      land first; editing through the shell is the circumvention ADR-0018
      already rejected.
- [ ] 4.3 Verification: full `NOT_CRAN=true` run; coefficient, global and C++
      golden baselines unchanged and PASS; update `progress.md`; commit.

## 5. Announcement (ADR-0032)

- [ ] 5.1 `NEWS.md` entry naming both spellings for all five 1.7.0 renames — the
      announcement that release skipped. Decide placement (development version
      with a note vs an amended 1.7.0 heading; design open question).
- [ ] 5.2 Effect documentation: confirm no `@param` or vignette prose still uses
      a retired spelling; `devtools::document()` if roxygen changed.

## 6. Finalize

- [ ] 6.1 Re-ground `effect-term-registry` tasks 1.2b / 4.2 / 4.2b and D25 —
      with this landed they reduce to absorbing the map and the matcher into
      `term_def`, not doing the work again.
- [ ] 6.2 `lintr::lint_package()` clean on touched files; `DESCRIPTION` +
      `NEWS.md` version bump per the phase-milestone rule.
- [ ] 6.3 Full `NOT_CRAN=true` suite green; update `progress.md`; commit.

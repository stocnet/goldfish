# Tasks — model-spec-descriptor

Disciplines (`openspec/config.yaml`): one focused conventional commit per
task, full `NOT_CRAN=true` suite green at every commit with the frozen 1e-6
baselines PASS not SKIP, `air format` then `lintr` on touched files,
`devtools::document()` inline when roxygen changes. No `src/` edit, so no
recompile. **Sequencing with `class-naming-scheme` must be settled before
task 1.1 — see design D8.**

## 1. Descriptor construction (additive; nothing reads it yet)

- [ ] 1.1 Confirm `class-naming-scheme` has landed before starting (design
      D8, settled 2026-09-05: the rename goes first, this change follows).
      If that change chose to exclude the `model_spec` hierarchy from its
      table, note which class strings arrive unrenamed, since this change
      then names them as it reshapes them.
- [ ] 1.2 Inventory: for each of the 9 variants record its current class
      vector, its `preprocess.*` parameters, its `compute_event_contribution`
      method (noting the three aliases), and the descriptor values it will
      carry. Record the table in `progress.md` — it is the migration's
      reference and the reviewer's checklist.
- [ ] 1.3 Build the descriptor in the spec constructor, subsuming the existing
      `risk_set` list (design D1): `axis`, `timing`, `likelihood`,
      `input_shape`, `distribution`, `fold_target`, `encoding`. Abort on a
      combination with no mapping. Nothing reads it yet, so the suite must be
      green with zero behavior change.
- [ ] 1.4 Tests: construct one spec per variant and assert every descriptor
      field against the 1.2 table; assert an unmapped combination aborts;
      assert no second behavioral object remains on the spec.
- [ ] 1.5 Verification: `air format` → `lintr` → **not-cran-test**. Baselines
      unchanged — this task adds a field and changes no arithmetic.

## 2. Preprocessing reads the descriptor

- [ ] 2.1 Replace the six non-DyNAMi `preprocess.*` methods with descriptor
      reads: the recipe follows `axis`, its parameters follow `timing`.
      `dynam_choice` and `dynam_choice_coord` are byte-identical today and
      MUST collapse to one path, not two identical ones.
- [ ] 2.2 Route the three DyNAM-i `preprocess.*` methods through
      `input_shape == "grouped"`, keeping the group-interaction loop they
      delegate to unchanged — that difference is real and stays (design,
      Non-Goals).
- [ ] 2.3 Tests: preprocessing output is byte-identical to before for every
      variant. Compare against stored output, not against a re-run of the new
      code.
- [ ] 2.4 Retire the `dyad_symmetric` axis string (design D1a): one-mode
      coordination carries `axis = "dyad"` like every other dyadic model, and
      the sites testing `dyad_symmetric` test `likelihood == "coordination"`
      instead. Check the C++ boundary first — the branch comment names
      `DyNAM_MM_default.cpp`, so confirm the reduction path is R-side before
      changing the string.
- [ ] 2.5 Verification: `air format` → `lintr` → **not-cran-test**. The
      coordination baselines are the detector here: the unordered-pair
      reduction must produce identical numbers.

## 3. One name per behavior — retire the flag pair

- [ ] 3.1 Replace `right_censored` / `intercept_scalars` with `timing`
      (design D2) across the 130 sites in 18 files. Recipe helpers may keep
      booleans in their own signatures, derived from `timing` at the call
      site; no other site names either retired flag.
- [ ] 3.2 **Rewrite, do not swap, every comment and roxygen line that
      explains the old flags.** They describe a consequence
      ("right-censored") rather than the property ("timed rate"), so an
      identifier substitution leaves the prose asserting the wrong thing.
      Hand-edit one site at a time; a scripted pass over comments is
      forbidden.
- [ ] 3.3 Grep for both retired names and require an empty result outside the
      recipe-helper signatures documented in 3.1.
- [ ] 3.4 Verification: `air format` → `lintr` → `devtools::document()` →
      **not-cran-test**.

## 4. Likelihood dispatch and alias deletion

- [ ] 4.1 Carry a likelihood class on the spec (design D3) and re-point
      `compute_event_contribution` dispatch at it; variants sharing an
      implementation share the class.
- [ ] 4.2 Delete the three DyNAM-i alias assignments (design D4). Assert that
      a DyNAM-i rate spec dispatches to the DyNAM rate method and that no
      alias is registered.
- [ ] 4.3 Confirm `estimate_int` still dispatches on the axis and that
      preprocessing no longer dispatches per variant.
- [ ] 4.4 Verification: `air format` → `lintr` → **not-cran-test**. This is
      the task most able to move a coefficient; a moved baseline stops it
      (ADR-0021), it is not re-frozen.

## 5. Re-derivation sweep

- [ ] 5.1 Replace the 43 `model`/`sub_model` behavior branches outside the
      constructor with descriptor reads. Where a site's rule turns out to
      disagree with the descriptor, that is a latent bug — record it in
      `progress.md` and fix it deliberately rather than preserving it.
- [ ] 5.2 Keep `model` / `sub_model` as provenance (design D5): print
      methods and error messages still report what the user asked for.
- [ ] 5.3 Add the guard test (design D6) failing on any `model`/`sub_model`
      equality or membership test outside an explicit exception list
      (the constructor, `class_checks.R`, `formula_validate.R`). List the
      exceptions by path; do not pattern-match them.
- [ ] 5.4 Verification: `air format` → `lintr` → **not-cran-test**.

## 6. Close

- [ ] 6.1 The guard test passes and the descriptor covers every variant in
      the 1.2 table.
- [ ] 6.2 `devtools::check()` clean; `openspec validate model-spec-descriptor
      --strict`; `bash .plan/opsx-spec-placement-check.sh model-spec-descriptor`.
- [ ] 6.3 Write a `NEWS.d/` fragment (internal-refactor note; no user-visible
      behavior change). **Do not** bump `DESCRIPTION` or edit `NEWS.md` — this
      runs on a feature branch and those are merge-time folds (ADR-0040).
- [ ] 6.4 Final **not-cran-test** on the full suite; frozen 1e-6 and C++
      goldens PASS not SKIP.
- [ ] 6.5 Re-run the placement pre-flight for `parametric-rates` and
      `two-sided-coordination`: both add axes to this surface, and this
      change moves the living-spec wording their deltas target.

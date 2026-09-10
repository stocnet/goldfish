## 0. Gate and baseline

- [ ] 0.1 Record the go/no-go rule before measuring (design D6): the recipe
      loops stay as the batch path if the merged walk exceeds 1.10x their wall
      time on CollegeMsg with the full model. Then run `process-simulation`
      task 1.2 (Social Evolution full model with windows, an interaction term
      and a support constraint; CollegeMsg from `.plan/datasets/CollegeMsg.txt`;
      one `walk_open()` replay) and record wall time, peak memory and the
      merged/oracle ratio in `.plan/` and `progress.md`. Decide: groups 1–2
      only, or 1–4.
- [ ] 0.2 Branch `refactor/preprocess-one-walk` off `develop`; confirm no
      incremental `preprocessed=` walk path exists on the recipe loops (a
      supplied `preprocessed` skips the walk); inventory every caller of
      `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the helpers
      only they use, and the `writer`/`new_writer` selection in
      `estimate_wrapper()`; note it in `progress.md`.
- [ ] 0.3 Verification: `NOT_CRAN=true` suite green on the branch start;
      frozen baselines and C++ goldens PASS not SKIP.

## 1. Merged-walk parity with the recipe loops

- [ ] 1.1 Writers (design D5): `build_walk_engine()` takes the `writer` /
      `new_writer` pair and hands it to `init_consumers()`; `preprocess_joint()`
      and `run_merged_walk()` thread it from the preprocessing controls. Tests:
      `output = "gather"`, `"data.frame"`, `"db"` on a single-process fixture
      through `preprocess_joint(single_process_joint(spec))` equal the recipe
      loops' rendered output.
- [ ] 1.2 Observation window (design D4): `start_time` / `end_time` bound the
      shared schedule; rows before the start update state unwritten, rows after
      the end are dropped, every timed engine writes the closing right-censored
      row at the end time; lift the `run_merged_walk()` abort. Tests: bounded
      fixtures (start only, end only, both, end past the last event) equal the
      recipe loops byte-for-byte, including `total_time` and
      `avg_active_entity`.
- [ ] 1.3 Window effects (design D3): each unit's `plan$derivations` realize
      the derived windowed network into the shared object registry
      (deduplicated by derived identity) and its expiry streams into
      `build_joint_schedule()` with the stream index the recipe context assigns;
      lift the `build_walk_engine()` abort. Tests: the `window-list-*`
      windowed fixtures and a tied-time expiry fixture through the merged walk
      equal the recipe loops byte-for-byte; a two-process join windowing the
      same source shares one derived object.
- [ ] 1.4 Restricted opportunity sets and user support constraints on a
      single-process unit through the merged walk: parity tests against the
      recipe loops on the `active_dyad_fold` and support-constraint fixtures
      (both already exist on the joint side; this task proves the single-unit
      entry reaches them).
- [ ] 1.5 Verification: `NOT_CRAN=true` suite green; baselines PASS; the
      joint test file's byte-identity assertions now cover windowed, bounded,
      writer-varied and constrained single-process fixtures.

## 2. One compile stage

- [ ] 2.1 Extract the compile stage of `estimate_wrapper()` (parse, effects,
      links, `new_model_spec()`, `build_spec_map()`, imputation policy stamp)
      into one internal `compile_spec_map()`; `compile_recipe_spec_map()`
      becomes a call to it with the joint path's per-focal working copy as a
      parameter; `parsed_formula` reuse stays a parameter (design D2).
      Roxygen for the internal function; `devtools::document()`.
- [ ] 2.2 `build_merged_blocks()` accepts pre-compiled units: the joint path
      compiles per process with `compile_spec_map()` and hands units in; a
      one-unit entry wraps a single compiled `spec_map` (with its consumer
      specs for a flavored plan) for the single/flavored callers.
- [ ] 2.3 Verification: `NOT_CRAN=true` suite green; baselines PASS; the joint
      tests' compile assertions (one spec_map per process, grouped by block)
      unchanged.

## 3. Dispatch flip and deletion (only if task 0.1 says go)

- [ ] 3.1 `preprocess.goldfishKind()` routes every `input_shape = "standard"`
      spec to the merged walk through the one-unit entry (design D1): a
      single-process call unwraps fid 1; a flavored plan returns the fid-keyed
      list with `process_map`. `preprocess_recipe()` passes the compiled
      `spec_map` and consumer specs through unchanged. The grouped branch is
      untouched. Full `NOT_CRAN=true` suite: baselines PASS. This is the
      bisectable commit.
- [ ] 3.2 `preprocess_flavored()` drops its per-family loop and calls the
      merged walk once for all families; its union planning, consumer specs,
      `validate_prep_support()` stamping and `process_map` contract stay.
      Tests: the flavored fixtures and the `estimate_flavored()` container
      tests unchanged.
- [ ] 3.3 Delete `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the
      helpers only they called (inventory from task 0.2); keep the finalizer
      helpers the merged walk shares. `air format` the touched files, then
      `lintr::lint()` on them. The descriptor spec's guard test and
      `grep -n "eval(parse\|assign(" R/model_preprocess.R` stay clean.
- [ ] 3.4 Re-run the task 0.1 measurement on the flipped tree and record it
      beside the pre-flip numbers; confirm the ratio is within the recorded
      rule.
- [ ] 3.5 Verification: full `NOT_CRAN=true` suite green; frozen baselines and
      C++ goldens PASS not SKIP; `devtools::document()` if any roxygen changed.

## 4. Close

- [ ] 4.1 NEWS.d fragment (Internal): one batch preprocessing loop, the recipe
      loops deleted (or, on a no-go, the merged walk's new parity and the
      recorded reason the loops stay). No `NEWS.md` or Version edit on the
      branch (ADR-0040).
- [ ] 4.2 `bash .plan/opsx-spec-placement-check.sh preprocess-one-walk`
      clean; `openspec validate preprocess-one-walk --strict` clean; on a
      no-go, trim the three MODIFIED deltas to what landed.
- [ ] 4.3 Note the consequences on the neighbors in their `progress.md` when
      next claimed: `process-simulation` task 2.0c shrinks to the breakpoint
      API; ADR-0045's inspection records this as its first closed finding
      and `compile_spec_map()` as its second.
- [ ] 4.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; `devtools::document()`; ready for the trunk merge to
      fold.

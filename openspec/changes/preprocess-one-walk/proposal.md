## Why

goldfish carries three batch preprocessing substrates for the same per-event
statistics update: `run_sender_recipe_loop()` and `run_dyad_recipe_loop()` in
`R/model_preprocess.R` (541 and 585 lines, 88 percent byte-identical to each
other, differing only in the kernel shape), and the merged single-clock walk
in `R/preprocess_joint.R`, whose `merged_covariate_step()` is a port of the
same body with the shape branch inside one function. The merged walk already
reproduces the flavored two-walk and the single-process standalone output
byte-for-byte in its own tests, but no user path reaches it: every
`estimate_*()` and `compute_statistics()` call still runs the recipe loops,
so the substrate `simulate()` and the DyNES augmenters will stand on is
exercised by its own test file only, while the loops it duplicates are
hardened by the full suite and the frozen 1e-6 baselines. ADR-0045 asked for
exactly this duplication to be measured and closed once the class and
descriptor changes landed; they landed at 1.9.31. Closing it now puts the
whole suite behind the one walk before `process-simulation` builds on it.

## What Changes

- **The merged single-clock walk becomes the only batch preprocessing loop
  for DyNAM and REM.** Single-process, flavored, and joint specifications all
  preprocess through `build_merged_blocks()` + `run_merged_walk()`; the two
  recipe loops are deleted. DyNAM-i stays on its monolith
  (`refactor-dynami-engine` owns that conversion).
- **The merged walk gains what the recipe loops have and it lacks:** window
  effects (the derived windowed networks and their expiry streams enter the
  shared object registry and the merged schedule; the `build_walk_engine()`
  abort is lifted), an explicit observation window (`start_time` /
  `end_time`, including the closing right-censored row at the end time), and
  the writer choice (`default` / `gather` / `db`) that `build_walk_engine()`
  currently hardcodes to the default writer.
- **One compile stage.** `compile_recipe_spec_map()`, which mirrors the parse
  and `build_spec_map()` portion of `estimate_wrapper()` by its own comment,
  and that portion converge into one function both callers use;
  `build_merged_blocks()` accepts pre-compiled units so the wrapper compiles
  once and hands the unit over.
- **The stepping handle inherits every lift for free**: `walk_open()` builds
  on the same `build_merged_blocks()` and `build_walk_engine()`, so
  `process-simulation` task 2.0c shrinks to the driver-inserted breakpoint
  API.
- **Gated on measurement, not on optimism.** `process-simulation` task 1.2
  (Social Evolution full model, CollegeMsg stress case, one handle replay)
  runs first; a merged walk slower than the recipe loops beyond the recorded
  threshold stops this change at the substrate lifts and keeps the loops as
  the batch fast path, with the reason recorded.
- No user-facing surface changes: same functions, same arguments, same
  outputs, same numbers. Internal only; a NEWS.d fragment records it under
  Internal.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `model-recipe-dispatch`: preprocessing no longer selects one of two
  per-axis recipe loops; the descriptor's axis becomes an engine property
  inside the one merged walk, and the recipe-selection requirement is
  reworded accordingly.
- `multi-process-walk`: the merged walk is the batch preprocessing path for
  every non-grouped specification and supports window effects, an explicit
  observation window, and every writer; the frozen-baseline gate is stated
  against the deleted loops' outputs.
- `flavored-processes`: single-pass preprocessing of a flavored
  specification is one walk over both families, not one walk per family; the
  output contract (fid-keyed `goldfishStat` list with `process_map`) is
  unchanged.

## Impact

- **Code**: `R/model_preprocess.R` loses `run_sender_recipe_loop()`,
  `run_dyad_recipe_loop()` and whatever helpers only they call;
  `preprocess.goldfishKind()` routes non-grouped specs to the merged walk;
  `R/preprocess_flavored.R` loses its per-family loop (its union planning,
  consumers and finalizers stay, the merged walk uses them);
  `R/preprocess_joint.R` gains window realization, the observation window,
  writer plumbing, and the pre-compiled-unit entry; `R/model_estimate.R`'s
  compile stage becomes the shared compile function; `R/walk_handle.R`
  changes nothing and gains windows and bounds through the substrate.
- **Numbers**: none move. The frozen 1e-6 coefficient baselines and the C++
  goldens are the gate at every commit (ADR-0021); the joint test file's
  byte-identity assertions extend to windowed, bounded and writer-varied
  fixtures.
- **Performance**: decided by `process-simulation` task 1.2 before any
  dispatch flips; the joint test's 0.92x ratio on its two-flavor fixture is
  the only number today and is too small to decide on.
- **Sequencing**: after `process-simulation` task 1.2; before
  `process-simulation` task 2.1. Independent of `refactor-dynami-engine`,
  which keeps the monolith until its own conversion. Developed on a feature
  branch with a NEWS.d fragment; the trunk merge folds (ADR-0040).
- **Successors it simplifies**: `process-simulation` 2.0c (windows in the
  merged walk, done here), ADR-0045's duplication inspection (this is its
  first closed finding; `compile_recipe_spec_map()` is its second).

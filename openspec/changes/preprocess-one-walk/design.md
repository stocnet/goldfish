## Context

Measured on `develop` at 1.9.31 (2026-09-07/08, `process-simulation` explore
sessions; design D10 there records the three substrates):

| Loop | File | Lines | Lines unique to it |
| --- | --- | --- | --- |
| `run_sender_recipe_loop()` | `R/model_preprocess.R` | 541 | 63 |
| `run_dyad_recipe_loop()` | `R/model_preprocess.R` | 585 | 107 |
| `run_merged_walk()` + `build_walk_engine()` + `merged_covariate_step()` | `R/preprocess_joint.R` | 184 + 167 + 168 | shape branch on `engine$is_sender` |
| `preprocess_monolith()` (DyNAM-i) | `R/model_preprocess.R` | 695 | its own setup |

Every difference between the two recipe loops is the kernel shape: a
per-sender vector versus a dyad array, `cbind(node1, gid)` versus
`cbind(node1, node2, gid)`, operand expansion for interactions, the
constraint-mask realization and the intercept scalar. The merged walk already
parameterizes that shape and hosts N engines over one state and one schedule.
Its tests assert byte-identity against the flavored two-walk and against
single-process standalone output, and a hot-path guard reports merged/oracle
0.92x on the two-flavor fixture.

What the merged walk lacks today, each an abort or a hardcode: window effects
(`build_walk_engine()` aborts; the compile passes `realize_windows = FALSE`
and the merged schedule never realizes the derived networks' expiry streams),
an explicit observation window (`run_merged_walk()` aborts on
`start_time`/`end_time`), and the writer (`build_walk_engine()` calls
`init_consumers(writer = writer_default())` while the wrapper selects
`default`/`gather`/`db` per output). Everything else the recipe loops do, the
merged walk does through the same helpers: `prepare_recipe_context()` for
setup, the consumer/writer layer for routing and emission, the constraint atom
pass for masks (compiled once per `constraint_id`, snapshot per fid),
`finalize_walk_engine()` mirroring `finalize_consumers()`.

Constraints: the frozen 1e-6 baselines and the C++ goldens never move
(ADR-0021); `src/` is untouched; the change runs on a feature branch and
writes a NEWS.d fragment (ADR-0040); DyNAM-i is out of scope
(`refactor-dynami-engine`).

## Goals / Non-Goals

**Goals:**
- One batch preprocessing loop for every non-grouped specification, with the
  recipe loops deleted rather than kept as a parallel path.
- The merged walk reaches feature parity with the recipe loops: windows,
  observation window, writers, restricted opportunity sets.
- One compile function feeding both the wrapper and the joint path.
- The whole suite and the baselines exercise the substrate `simulate()` will
  drive.

**Non-Goals:**
- DyNAM-i (stays on the monolith; its recompute-style effects join through the
  adapter `refactor-dynami-engine` D1 describes).
- Any change to the walk handle's API or to `simulate()`.
- Routing estimation of a flavored spec through anything other than the
  per-fid Newton–Raphson it uses today; the likelihood stays per block.
- Performance work beyond not regressing; the measurement gate decides
  go/no-go, it does not set an optimization target.
- The per-family completion-gap abort (ADR-0053, a separate follow-up).

## Decisions

### D1 — The flip point is `preprocess_recipe()`, and `preprocess.goldfishKind()` keeps dispatching

`preprocess_recipe()` in `R/model_estimate.R` is where the wrapper compiles a
`spec_map` and calls `preprocess(spec_map, ...)`, which today selects a loop by
axis. After this change `preprocess.goldfishKind()` routes every non-grouped
spec to the merged walk through a one-unit entry: the compiled `spec_map`
becomes a unit, `build_merged_blocks()` accepts pre-compiled units, and
`run_merged_walk()` runs it. The grouped branch (DyNAM-i) is untouched. A
single-process call unwraps fid 1 from the fid-keyed return so the wrapper's
downstream (`decorate()`, output rendering, estimation) sees exactly the object
it sees today; a flavored call returns the fid-keyed list with `process_map`
as `preprocess_flavored()` does now. *Rejected:* flipping inside
`estimate_wrapper()` by calling `preprocess_joint(single_process_joint(spec))`
directly — that re-parses and re-compiles what the wrapper just compiled, and
it bypasses the `preprocess()` generic the descriptor spec requires to be the
one dispatch point.

### D2 — One compile function; `build_merged_blocks()` takes units

`compile_recipe_spec_map()` (`R/preprocess_joint.R`) and the compile stage of
`estimate_wrapper()` (parse → effects → links → `new_model_spec()` →
`build_spec_map()`) are the same sequence written twice; the joint one says so
in its comment. They converge into a single internal `compile_spec_map()` that
both call, with the joint path's per-focal working copy and the wrapper's
`parsed_formula` reuse as parameters rather than as two bodies.
`build_merged_blocks()` gains an entry that accepts already-compiled units
(the joint path compiles per process with the shared function and hands them
in; the single path hands in one). *Rejected:* keeping two compiles and
asserting equality in a test — that is the shape ADR-0045 exists to remove.

### D3 — Windows enter the merged walk the way they enter the recipe loops: as derived objects with event streams

The recipe path parses with `realize_windows = FALSE`, records one derivation
per windowed network, and realizes at state creation the derived network plus
its dissolve-event streams (the source events shifted by the window length
with negated increments) from `plan$derivations`. The merged walk does the
same at `build_shared_objects()` / `build_shared_state()`: each unit's
derivations add the derived object to the shared registry and its expiry
streams to `build_joint_schedule()`. From then on an expiry is an ordinary
covariate row that `merged_covariate_step()` routes like any other. The
`build_walk_engine()` abort goes. Deduplication across units keys on the
derived object's identity, so two processes windowing the same source network
by the same length share one derived object. *Rejected:* a window-specific
branch inside the merged loop (the recipe loops have none either; the pre-start
branch noted in `merged_covariate_step()`'s comment is the observation-window
concern of D4, not a window-effect concern).

### D4 — The observation window is applied on the shared clock

`start_time` / `end_time` bound the merged schedule exactly as
`prepare_recipe_context()` bounds a recipe schedule: rows before the start
update state without being written, rows after the end are dropped, and every
timed engine writes the closing right-censored row at the end time when the
schedule runs out earlier (`run_sender_recipe_loop()`'s "the window closes at
the end time" block). The bound is one per walk, applied to the shared
schedule, so a joint specification with an explicit window applies it to every
process; per-process windows are not a thing the recipe loops offer either.
The `run_merged_walk()` abort goes.

### D5 — Writers are a per-consumer parameter of the engine, not a hardcode

`build_walk_engine()` takes the `writer` / `new_writer` pair the wrapper
selects (`default`, `gather`, `db`) and passes it to `init_consumers()`, as
`preprocess_recipe()` does today through `build_consumer_specs()`. The
`output = "gather"`, `"data.frame"` and `"db"` paths then run on the merged
walk with no rendering change. *Rejected:* rendering gather/db from the
default writer's output after the walk — it doubles memory on the stack
writer's own use case.

### D6 — Go/no-go is the task 1.2 measurement, with the threshold recorded before the run

`process-simulation` task 1.2 measures recipe loops versus merged walk on
Social Evolution (full model with windows, an interaction and a constraint) and
on CollegeMsg (about 60k events), plus one handle replay. This change's task
0.1 records, before reading the numbers, the threshold above which the loops
stay: proposed **1.10x** wall time on CollegeMsg with the full model, since a
ten percent loss on the largest realistic case is the price of one loop and
anything larger is a regression a user notices. Above the threshold the change
completes groups 1 and 2 (substrate lifts, compile convergence), skips group
3, and records why in `progress.md` and NEWS.d; the deletion then waits on an
optimization pass with a re-measurement. *Rejected:* flipping first and
measuring after — the measurement is cheap and the flip touches every test.

### D7 — Deletion, not deprecation

The recipe loops are internal, unexported, and reached by no user surface, so
they are deleted in the same commit their last caller leaves. `preprocess()`
remains the generic; the lifecycle skill is not involved. Helpers that only
the loops used (`walk_presence_buffer()`, the interaction operand accumulators
if the merged kernel owns its own) go with them; helpers the finalizers share
(`fold_active_*`, `crossings_from_vectors()`) stay.

### D8 — The handle changes nothing and gains everything

`walk_open()` builds on `build_merged_blocks()` and `build_walk_engine()`, so
windows, bounds and pre-compiled units reach it with no edit in
`R/walk_handle.R`. Its remaining refusals (user support constraints,
composition changes, effect-free sub-models) are `process-simulation`'s task
2.0a/2.0b and driver concerns, not this change's; its windowed case stays
refused only where the *live* maintenance of a derived object matters, which
is the breakpoint API of `process-simulation` 2.0c.

## Risks / Trade-offs

- [A coefficient moves] → it cannot if the port is exact; the baselines are
  the detector and a move stops the task (ADR-0021). The flip lands as its own
  commit after the parity fixtures pass, so a move is bisectable to one
  commit.
- [The merged walk is slower on large data] → D6 measures first and records
  the stop rule; the substrate lifts are valuable on their own for
  `simulate()` even if the deletion waits.
- [Windowed byte-identity is subtle: expiry ordering at tied times] → the
  merged schedule orders `(time, stream_index)`; the parity fixture uses the
  existing windowed baselines (`window-list-*` tests) and a tied-time fixture,
  and the derivation streams take the same stream index the recipe context
  assigns.
- [The `preprocessed=` incremental path] → none exists on the recipe loops
  today (`preprocessing_init` is deprecated and a supplied `preprocessed`
  skips the walk entirely), so nothing to port; verified in task 0.2.
- [`prepare_recipe_context()` builds a schedule the merged walk discards] →
  kept in this change (its state creation is what realizes derivations);
  trimming its schedule build is a follow-up measured by task 1.2's setup
  share, not a correctness item.

## Migration Plan

Internal only. Feature branch `refactor/preprocess-one-walk` off `develop`;
one commit per task; NEWS.d fragment under Internal; the trunk merge folds
(ADR-0040). Rollback is per commit; the flip commit (task 3.1) is the one to
revert if a baseline moves, and it leaves the substrate lifts in place.

## Open Questions

- Whether `prepare_recipe_context()` keeps building its own schedule after
  the flip, or state creation splits out so the merged walk builds the only
  schedule — decide from task 1.2's setup-share numbers (about 0.14 s of a
  0.54 s Social Evolution rate call is setup today).
- Whether the one-unit wrapper for a single-process spec lives in
  `preprocess.goldfishKind()` or in `preprocess_recipe()`; D1 says the
  generic, task 3.1 confirms against the descriptor spec's "one dispatch
  point" requirement.

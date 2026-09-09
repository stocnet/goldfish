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

**Deletion needs Alvaro's explicit approval, separately from the measurement**
(added 2026-09-09). A ratio inside the threshold is a necessary condition, not
the decision. The recipe loops are what the frozen 1e-6 baselines have always
run through and what the whole suite exercises, so retiring them is a judgment
about confidence in the replacement, not an inference from a number. Task 3.3
does not start on a green re-run alone.

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

### D9 — The 1.10x measurement came back 0.31x and is still a no-go (added 2026-09-09)

`process-simulation` task 1.2 ran on 2026-09-09. CollegeMsg, base model,
median of three warm runs: two recipe loops 219.6 s, merged walk 67.2 s, ratio
**0.31** against a 1.10 threshold. Social Evolution reversed it: merged 1.31x
(windowless) and 1.78x (plain), i.e. slower.

The reversal is explained by allocation. One 1899 x 1899 double matrix is
27.51 MB; the recipe loops allocate 27.53 MB per event (rate) and 36.56 MB
(choice), the merged walk 27.67 MB. Two loops copy the shared adjacency matrix
once each per event because each holds its own state container; one merged walk
copies it once. The 2-to-1 **is** the 0.31.

So the measurement does not answer the question the rule asks, and this change
takes D6's above-threshold branch: complete groups 1 and 2, **skip group 3**,
re-run the gate once D10 and D11 land. Recorded as ADR-0057.

The pre-fix numbers above are the recorded contrast and are **not** re-measured;
task 0.9 re-runs the same script on the fixed tree and tabulates before against
after. It adds a **10k-event** CollegeMsg subset, because Social Evolution is
setup-dominated and the full 59,835 events are copy-dominated, so neither
isolates the architecture alone. *Rejected:* a
"conditional go" that completes groups 1 and 2 and defers only the deletion —
identical work, but the word recorded in this change's history is what a later
reader quotes, and "conditional go" becomes "go".

Two conditions stand beside the ratio. Parity does not hold: the merged walk
computes weighted degree where the recipe loops compute unweighted degree on an
accumulating layer, and every fixture in `test-preprocess_joint.R` gives each
dyad one event, which is why the byte-identity assertions pass. And no
full-model CollegeMsg cell exists, because a support constraint exhausts 24 GB
on every substrate (D11).

### D10 — The per-event state copy is this change's defect to fix, not the merged walk's advantage (added 2026-09-09)

`call_effect_template()` hands the state's adjacency matrix to the effect
closures through `c(list(network = state$networks[[key]], ...), event_args)` and
`do.call()`, which marks it shared; the state write that follows,
`state$networks[[key]][sender, receiver] <- replace`, duplicates the whole
matrix. The cycle repeats every event, in the recipe loops and in
`merged_apply_state_update()` alike. The sharing is transient rather than a
saturated reference count — writes with no closure call in between are in place,
and one call in between costs exactly one copy — so the fix is to break the
call-then-write cycle, not to avoid saturating.

An `Rcpp::NumericMatrix` in-place setter, benchmarked against the R
subassignment on a 1899 x 1899 matrix with the same closure call between writes:
3.67 ms per event versus 0.02 ms, and `tracemem` reports no copy at all for the
C++ path. Against the measured 1.99 ms per event of the CollegeMsg rate loop,
the copy is close to all of it.

The same pattern sits in four places: the two recipe loops' state write,
`merged_apply_state_update()`, the R estimation backend's per-event fold
(`stat_mat <- .gather_apply_stat(stat_mat, ...)` in `R/cpp_interface.R`), and
`walk_fold_engine()`'s `live_stats` fold. The C++ likelihood engines already
mutate in place through `src/broadcast_updates.cpp`, so the fold exists twice,
once free and once expensive.

**The four sites fall in two risk tiers, and only one of them is hard.** What
marks an object shared is *binding it to a name*, so where the binding comes
from decides the fix:

| tier | sites | what binds the buffer | fix |
| --- | --- | --- | --- |
| fold | `stat_mat` in the R backend, `live_stats` on the handle | the helper call itself (`.gather_apply_stat(stat_mat, ...)`) | remove the binding, or write through C++ |
| state | both recipe loops, `merged_apply_state_update()` | the effect closure receiving the matrix | C++ in place, or slice-passing |

The fold tier is the easy one. Those buffers are engine-local and never reach an
effect closure, so nothing else can alias them, and the copy exists only because
the helper takes the buffer as an argument. Doing the same subassignment inline
removes it in pure R: measured 0.430 ms/event through the helper against 0.003
ms/event inline, identical result. C++ is equally available there and keeps the
helper shared across its callers; either way the verification burden is small
because there is no aliasing question.

The state tier is where the care goes. Inlining does not help, because the
sharing comes from the closure call rather than from where the write is written,
so the write is already inline and still copies.

**Two ways to stop the state copy, measured.** The mechanism is *binding the
whole object to a name*, not reading from it, which narrows the options:

| approach | per event | copies |
| --- | ---: | ---: |
| pass the matrix to the closure (today), 1200^2 | 0.48 ms | yes |
| pass the closure the state environment and slice inside it | 0.39 ms | yes |
| pass the closure only the slices it needs | 0.02 ms | none |
| keep passing the matrix, write through C++ in place, 1899^2 | 0.02 ms | none |

Handing the closure an environment does **not** help: `state$net[i, j]` inside
the closure binds the matrix just as an argument does. Only two things work, and
they trade differently. The C++ in-place write changes one function and no
contract, but it steps outside R's copy semantics, so any other live reference
to that matrix would see the mutation, and that has to be argued rather than
assumed. Slice-passing stays in pure R and is safe by construction, but it
changes what every effect update closure receives, across every effect in three
model families, on the code path the frozen 1e-6 baselines cover.

Task 0.4 takes the C++ route for that reason: it is the one that can land
without touching the effect contract. Slice-passing is recorded here as the
pure-R alternative, to be reached for only if the in-place write cannot be made
safe.

**The safety argument is about aliasing, and two sites already alias.** An
in-place write is correct exactly when nothing else holds a live reference to
the matrix expecting it not to change. Two places in the tree do:

- `ds_impute_missing()` stores an imputed matrix in `src$net_override[[name]]`,
  and `ds_network()` then returns *that same object* on every later call. Two
  state containers built from one source would share the SEXP, so a write
  through one would be visible in the other and would corrupt the override
  itself. It triggers only when a network carries `NA`, which is why neither
  Social Evolution nor CollegeMsg would surface it.
- `init_DyNAM_choice.four()` returns `list(cache = network, stat = network)` on
  an edgeless network, so the effect cache aliases the state matrix. Today the
  cache holds a frozen snapshot; under an in-place write it would follow the
  state.

Neither is exercised by the frozen baselines' models, so the baselines are not
the detector here. The ordinary path (`ds_network()` on a stocnet without `NA`)
materializes a fresh matrix per call and is safe.

Both aliases are cheap to remove, and removing them is a prerequisite rather
than part of the write. `ds_network()` can copy when it serves an override,
which costs one copy per container build rather than one per event; and
`init_DyNAM_choice.four()`'s edgeless branch can give the cache its own matrix
instead of the state's. Clear the aliases first, land the write second, so a
baseline move is attributable to one step.

The legacy `ds_network.goldfishSourceEnvir()` path is deliberately left alone:
the environment surface retires with `refactor-dynami-engine`, so hardening it
is work with no future.

Why `four()` and not `trans()` or `cycle()`: all three special-case an edgeless
network, but they do it in different places. `trans()` and `cycle()` fold the
empty case into the same branch as the windowed and non-pooled-history cases,
which allocates a fresh `matrix(0, n1, n2)` for the cache. `four()` returns
earlier and separately, handing back the input network as both cache and stat
because it is already all zeros. The values are identical; the difference is
purely that `four()` reuses the object instead of allocating, which saves two
allocations and creates the alias. `four()`'s own windowed branch allocates
fresh matrices, so it is inconsistent with itself as well as with its siblings.

Fixing it is group 0 work because the gate cannot be re-read until it is done.
*Rejected as the fix direction:* maintaining the linear predictor incrementally
instead of the statistics. It is lossy — the statistics cannot be recovered from
the projection — and it dies at estimation anyway, because Newton changes the
parameter vector every iteration and the predictor would be rebuilt per
iteration. It survives only as a simulation-side accelerator carried *in
addition to* the statistics, and only for the families where every dyad competes
(REM, REM-ordered, coordination). That belongs to `process-simulation`, not
here.

### D11 — The support mask stores by its axis-union kind, and that is a prerequisite (added 2026-09-09)

`R/support_mask_maintain.R` stores one dense n1 x n2 logical per snapshot time.
Its own header calls the axis-union storage "a memory optimization deferred to a
later slice". At 59,835 snapshots on 1899 actors that is roughly 860 GB, and
CollegeMsg with a support constraint exhausts the 24 GB vector limit on **every**
substrate, the recipe loops included. It is a shipped-behavior defect on any
realistic node set, not a merged-walk limitation.

The classification already exists: `active_dyad_encoding_decide()` returns point,
alter, ego or scalar, and `test-active_dyad_fold.R` already asserts that a
receiver-axis constraint folds to a length-n2 vector with no dense allocation.
Only the storage is missing — a separable mask is a vector or a scalar per
snapshot, and its timeline is a sparse update stream like any other statistic.

This lands in group 0, before `process-simulation` task 2.0a wires the mask onto
the stepping handle, because 2.0a would otherwise inherit the dense form.

### D12 — Open: the statistics array is not the shape utilities are computed in (added 2026-09-09)

Not decided; recorded because the answer changes what task 0.4 touches.

`initial_stats` for a dyad family is an `n1 x n2 x p` array, and the instinct is
that this is already the right shape because a choice utility takes all
receivers for one sender. It is not the shape either consumer uses.
`walk_seed_live_stats()` flattens it to `(n1*n2) x p` with dyad `(i, j)` at row
`(i - 1) * n2 + j`, and `R/cpp_interface.R` does the same at the C++ boundary
with `stat_mat_init[, i] <- t(initial_stats[,, i])`. Both flatten before any
product is formed.

R is column-major, so the flattening is not cosmetic:

| layout | sender `i`'s block, one effect | |
| --- | --- | --- |
| `(n1*n2) x p` flattened | `n2` contiguous doubles | what both consumers build |
| `n1 x n2 x p` array | `n2` doubles at stride `n1` | what is stored |

So the array is the worse layout for the utility slice, and the code already
compensates. That leaves a real question with two live answers: one
`(n1*n2) x p` matrix, or a length-`p` list of `n2 x n1` matrices. The list gives
the same contiguous per-sender block and localizes copy-on-modify to the one
effect that changed, which is why it interacts with D10. The matrix keeps the
product as a single BLAS call, where the list needs `p` passes; that favours the
matrix as `p` grows and the list when `p` is small, since it avoids the slice
allocation entirely.

Settle it with a measurement on the complex fixture of task 0.6, which has a
realistic `p`, before task 0.4 chooses where the in-place write lands.

### D13 — A windowed constraint atom registers its derived object in the plan, like any other windowed term (added 2026-09-09, corrected same day)

Found while specifying task 0.6's fixtures. A `window =` inside a
`support_constraint` is silently ignored:
`~ !tie(call_network, window = 5)` and
`~ !tie(call_network, window = "365 days")` produce masks byte-identical to
`~ !tie(call_network)`, while the same window in the estimated formula does move
the statistics.

A window reaches the walk through three stages, and the middle one is the
registry the current code is organized around:

```
  parse            parse_formula() records the window as metadata on
                   parsed_formula$window_derivations, and the effect's object
                   reference is rewired to the derived name
     |
  MAP              build_spec_map() -> build_derivations() puts one
                   { kind = "window", derived_name = "call_network_5" } entry
                   in plan$derivations, one row per derived object.
                   A constraint separately appends { kind = "support_mask" }.
     |
  REALIZE          ds_realize_derivations(src, plan$derivations) at state
                   creation builds the derived network and its expiry stream,
                   and the object enters the shared registry
```

Measured on a real spec: a windowed formula term yields
`plan$derivations` = one entry, `kind = window`, `derived_name =
call_network_5`. A windowed *constraint* atom yields **no window entry at all** —
the merged units report zero derivations and the shared object registry holds
only `call_network`. Nothing is registered, so nothing is realized, and the
atom's reference still names the source layer, which it reads undecayed.

The failure is uniform: because the reference rewrite is what binds an effect to
its derived object, a matching window in the estimated formula does **not**
rescue the constraint. Verified with
`choice = ~ inertia(call_network, window = 5)` and
`support_constraint = ~ !tie(call_network, window = 5)` — the mask is still the
unwindowed one. There is no accidental-pass path.

The constraint's atoms are plain effects, which is the premise the whole
constraint compiler rests on, so they belong in the same registry: a windowed
atom contributes its own `kind = "window"` entry to `plan$derivations` beside
the `kind = "support_mask"` one, and its reference is rewired to the derived
name. Entries deduplicate by derived identity, so a window shared between
formula and constraint is one derived object with one expiry stream, not two.
The realize step then needs no change, which is the point of registering rather
than special-casing.

*Do not reach for `parse_time_windows()` directly.* It still runs inside
`parse_formula()`, but on the recipe path it is metadata-only
(`realize_windows = FALSE`), and treating it as the entry point is the
pre-registry framing. The work lands on what enters `plan$derivations`.

This is task 0.5b, sequenced beside the mask-storage work because both are
constraint defects, and after task 0.6 has written the failing test. It belongs
in group 0 rather than group 1: a constraint decides the risk set, so ignoring
its window does not crash, it silently estimates a different model, and every
constrained fixture the later groups rely on would be measuring the wrong risk
set. *Rejected:* aborting on a windowed constraint atom instead of supporting
it — the grammar already accepts the argument, so an abort would be a new
restriction on documented behavior rather than a fix.

### D14 — The merged walk must compute the statistic the recipe loops compute, before any ratio between them means anything (added 2026-09-09)

The divergence that made ADR-0057 a no-go on more than timing grounds. On a
layer whose events accumulate, `preprocess_joint()` tracks weighted degree where
the recipe loops track unweighted. Five nodes, four calls, the dyad `1 -> 2`
repeating: the recipe loop leaves `indeg(N2)` at 1 and emits no update, the
merged walk emits one carrying 2. On Social Evolution the recipe loop
reconstructs `colSums(A > 0)` and the merged walk `colSums(A)`.

The recipe loops are the correct side: `indeg` and `outdeg` default to
`weighted = FALSE`, and the frozen 1e-6 baselines run through them. So this is a
merged-walk defect, and fixing it cannot move a baseline.

Every fixture in `test-preprocess_joint.R` gives each dyad exactly one event, so
weighted and unweighted coincide and the byte-identity assertions pass. The
divergence needs a repeated dyad, which is the normal case in real event data.
That is why task 0.6 exists before this one.

*The cause is deliberately not asserted here.* One diagnostic clue is recorded:
writing the effects as `indeg(calls, weighted = TRUE)` makes the rate block
agree byte-for-byte, which points at how the `weighted` flag reaches the merged
engine rather than at the schedule, the shared state, or the event-argument
resolution — `merged_build_event_args()` resolves an increment against the live
state exactly as the recipe loop does, so both hand the closure the same
`replace`. The choice side diverges differently: for unweighted `inertia` the
merged walk emits a redundant update carrying the same value, harmless
numerically but not byte-identical and not free.

Sequenced as task 0.4d, after 0.6 writes the fixtures and before 0.9 re-runs the
gate, because a ratio between substrates computing different statistics is not a
decision input.

### D15 — The merged walk imputes a missing network cell, because its state creation skips the step the recipe path takes (added 2026-09-09)

Found while writing task 0.6's fixture (e), which the fixture list expected to
exercise `src$net_override` for 0.4b's aliasing regression and nothing more. It
does more: a network carrying `NA` does not preprocess on the merged walk at
all.

```
Error in if (replace_value < 0) : missing value where TRUE/FALSE needed
  merged_build_event_args()   preprocess_joint.R#1509
```

The cause is one missing call, and it is not subtle. `prepare_recipe_context()`
runs `ds_impute_missing()` before it builds its state container
(`model_preprocess.R:328`), which replaces every `NA` in a network with 0 and
caches the result in `src$net_override`; `build_state_container()` then reads
the imputed matrix back through `ds_network()`. `build_merged_blocks()` builds
its own `new_data_source()` and hands it straight to `build_shared_state()`
(`preprocess_joint.R:625-635`), with nothing in between. The `NA` survives into
the shared state, and `merged_build_event_args()` resolves an increment against
it and tests its sign.

**Networks only.** Nodal imputation is already right on the merged side: the
per-object policy rides on `build_shared_object_props()` and the recode happens
at walk time in `impute_nodal_value()`. Measured on a fixture with `NA` in a
nodal covariate and a complete network, the rate block is byte-identical across
substrates. So this is one call on one object kind, not a missing subsystem.

**The crash is the benign case.** A `replace` layer never reaches the sign test,
so there the `NA` would pass through the effect closures into the statistics
instead of stopping the walk. That is the reason this is group 0 work rather
than a group 1 parity item: like the constraint window (D13), it is a silent
wrong answer wearing a crash on one code path.

Sequenced as task 0.4e, after 0.6 writes the fixture and before 0.9 re-runs the
gate, for D14's reason: a substrate that cannot preprocess the data is not a
ratio input. It is independent of 0.4b — that task removes the alias so an
in-place write is safe, this one makes the merged walk reach the override at
all — but they touch the same two functions, so 0.4b lands first.

*Rejected:* imputing inside `build_shared_state()`. The step belongs to the
source, not the state: `ds_impute_missing()` returns a modified source and the
merged walk already threads one, so calling it where the recipe path calls it
keeps one imputation contract rather than two. Full reproduction in
`.plan/bug-merged-walk-skips-network-imputation.md`.


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
- [The `preprocessed=` incremental path] → **corrected in task 0.2**: this
  said "none exists ... so nothing to port", and that is only half true. No
  incremental *within-walk* path exists — nothing steps a partially built
  state, and `preprocessing_init` is deprecated. But a supplied `preprocessed`
  skips the walk only when every effect matches. When `compare_formulas()`
  leaves a zero in `effects_indexes`, the wrapper runs a **second full recipe
  walk over the added effects alone** and column-merges it into the stored
  object. So there is a second entry into the loops, with a reduced effect set,
  that group 3's flip must keep working — and the merge asserts the layouts
  agree, so the merged walk owes the same `stat_mat_update` /
  `stat_mat_pointer` shape rather than merely the same values. Separately, that
  call passes positionally and so drops `support_constraint`, `writer`,
  `work_data`, `modeled_flavor` and `flavor_plan`: adding an effect to a stored
  object from a constrained or flavored model walks the new effects
  unconstrained. Recorded, not fixed here.
- [`prepare_recipe_context()` builds a schedule the merged walk discards] →
  kept in this change (its state creation is what realizes derivations);
  trimming its schedule build is a follow-up measured by task 1.2's setup
  share, not a correctness item.

## Migration Plan

Internal only. Work lands on **`feature_simulation`** alongside
`process-simulation` (decided 2026-09-09) rather than on a branch of its own:
that branch already carries both changes' artifacts, and `process-simulation`
task 2.0a waits on task 0.5a here, so separate branches would mean folding one
before the other could start. ADR-0041 covers two changes on one variant branch
— each flips to `status: landed (feature_simulation, awaiting fold)` when
complete instead of archiving there, and the trunk merge folds both `NEWS.d/`
fragments once. The two changes' spec deltas do not overlap, so neither declares
`depends-on`; the dependency is code sequencing and lives in the tasks.

One commit per task; NEWS.d fragment under Internal; the trunk merge folds
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

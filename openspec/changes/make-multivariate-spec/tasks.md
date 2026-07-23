> Sequencing: post-2.0.0, DyNES track. Blocked until `flavored-processes` is
> complete (its D9 fid vocabulary and D10 consumer seam are this change's
> foundation). Precedes `dynes-augmentation`; re-ground that proposal's
> simulation-hook/evaluator seams against `multi-process-walk` before starting.
> Parallel-development ordering (see `.plan/mv_branch.md`): sections 1, 2, and 4
> may run in parallel with `residuals-gof`; section 3 (merged walk) starts only
> after `spec-driven-dispatch` lands — it edits the same loop/writer files.
> `multimode-network-support` has landed; its mode map is the substrate for the
> per-mode-pair block keying (D8, section 1b/3). Section 1b additionally blocks on
> `formula-drives-focal` (D8, Option A): a join models N dependent layers over one
> object, so per-process focal resolution is a hard dependency, not stamped locally.
> Task 3.0 is exempt: it is a measurement
> spike that runs the profiler against already-landed code and edits no source,
> so it can and should run early — its number is an input to D3b, not a
> consequence of the merge.

## 1. Multivariate specification surface

- [x] 1.1 `make_joint_specification(...)` (source `R/make_joint_specification.R`,
      returning a new S3 class `joint_specification.goldfish` that does NOT inherit
      `specification.goldfish`): accept ≥2 specification objects over one data
      object; validate the shared node set, reject DyNAM-i processes, require ≥1
      panel-observed layer *referenced* (focal/dependent or exogenous covariate) —
      abort only when no panel layer is referenced at all, with
      per-process-estimation guidance; do NOT require a modeled panel process here
      (that DyNES-viability check is estimation-time in `estimate_dynes()`); abort
      when two joined specifications share the same focal/dependent layer (focal
      uniqueness — covariate reuse allowed; all of a layer's flavors in one
      specification), naming the duplicated layer; assemble the extended
      process_map (fid rows across processes, shared-constraint ids)
- [x] 1.2 Coupling detection: mark each fid coupled iff its parsed effects or
      constraint atoms directly reference a **modeled** panel-observed layer (a
      panel layer that is itself a process); reading a static exogenous panel
      covariate does not couple; store the `coupled` column; no transitivity
- [x] 1.3 Print method: processes sectioned per layer (flavors nested), derived
      and combined constraints shown, separable fids marked (cli semantic
      elements; labels rendered from the process_map)
- [x] 1.4 Tests: composition validation matrix (panel-reference requirement,
      node-set mismatch, DyNAM-i rejection, exogenous-only-panel spec composes),
      process_map row/constraint-id correctness, coupling via effect vs constraint
      atom vs windowed effect and modeled-panel vs static-exogenous-panel reference,
      print snapshots under a pinned cli context
- [x] 1.5 Verification: full `NOT_CRAN=true` run (baselines PASS not SKIP);
      `devtools::document()`; commit
- [x] 1.6 Estimator guards: `estimate_dynam()` / `estimate_rem()` abort on a
      `joint_specification.goldfish` object (class check BEFORE the
      `specification.goldfish` dispatch branch), directing to `estimate_dynes()`;
      confirm the existing `check_dependent_panel()` PE-focal guard fires on the
      single-specification path for `estimate_dynam()`/`estimate_rem()`/
      `estimate_dynami()` (message retargeted to `estimate_dynes()` lives in the
      `single-data-object` delta — do not duplicate it here); `estimate_dynami()`
      joint-object rejection deferred (future development, noted). Tests: joint
      object into dynam/rem aborts with the dynes pointer; PE-focal spec into each
      event-stream estimator aborts (cli snapshots under a pinned context)
- [x] 1.7 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP);
      `devtools::document()`; commit

## 1b. Node-space generality (D8 — mode-pair-keyed composition)

> Supersedes the "shared node set" validation in 1.1/1.4, which was written
> against the pre-multimode single-node-set restriction. The rule is now
> mode-set-identity conformance over a shared mode-map object.

- [x] 1b.1 Replace the shared-node-set check in `make_joint_specification()`
      with the mode-map conformance rule: accept processes over one shared
      mode-map object, one- or two-mode, over distinct mode-pairs; for each
      cross-process read (effect argument / constraint atom that reads another
      process's layer) require **mode-set-identity** conformance reusing
      `multimode-network-support`'s conformance predicate; abort on a
      subset/nested read (a mode ⊂ a union containing it) with a `cli` error
      naming the two layers and offending modes and noting it as future
      development ("Gap B")
- [x] 1b.2 Coupling detection (extends 1.2) resolves the coupled read's node
      space and confirms identity conformance; a coupling read that is
      subset/nested is rejected by 1b.1 before it is marked, never silently
      coupled on a wrong projection
- [x] 1b.3 Consume `formula-drives-focal` (hard dependency, D8 Option A): rely on
      its "modeled layer drives side/mode resolution" contract so each composed
      process resolves against its own dependent layer, not the object-level
      `info$focal`; do NOT replicate focal resolution in `make_joint_specification()`.
      Add a regression test that a join over an object whose single `info$focal`
      names only one of the modeled layers still resolves every other process's
      sides/modes correctly (the two-mode side-validity check in particular)
- [x] 1b.4 Tests: whole-shared-mode multilevel composes (advice `{staff}×{dir}`
      + nominations `{dir}×{proj}`, director-side read conforms); two-mode
      multiplex composes; subset/nested read aborts with the future-development
      message (cli snapshot under a pinned context); mixed one/two-mode join
- [x] 1b.5 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP);
      `devtools::document()`; commit

## 2. Cross-process union planning

- [x] 2.1 Generalize the union planner across processes: per statistic block,
      deduplicate effect terms across all fids sharing that block's dispatch
      family; per-fid effect maps against the block's union columns; never
      dedup across families
- [x] 2.2 Generalize consumer routing to the `(layer, flavor) → fid` lookup:
      the schedule carries layer and flavor for every dependent stream (one per
      process), cross-process events right-censor other processes' timed rate
      consumers
- [x] 2.3 Tests: two-process fixtures with hand-computed dependent/RC/state-only
      partitions per fid, cross-process shared-effect single computation,
      per-fid intercept scalars over per-fid masks
- [x] 2.4 Verification: `NOT_CRAN=true` run (single-process and flavored paths
      byte-unchanged, baselines PASS); commit

## 3. Merged single-clock walk

- [ ] 3.0 Spike (runnable NOW, before the merge — `preprocess_support_mask()`
      runs today on any constrained model, so this needs neither flavors nor
      the merged walk): measure what fraction of `t_preprocess_sec` the separate
      constraint-mask pass costs, using `.plan/profile_goldfish.R` (it already
      reports the preprocess/estimate split and `object_size_bytes`). Compare
      constrained vs unconstrained runs of the SAME single-process model on a
      mid-size and a large dataset, and record the rows. Then repeat on a
      two-flavor rate+choice spec: mask passes scale with output count (4 today
      for 2 flavors), so the flavored case shows the share the multivariate case
      will amplify — flavors change the magnitude, not the measurability.
      Split the pass's own cost into atom MAINTENANCE (`apply_atom_event()`,
      which shared-atom pooling removes N-1 copies of) and mask EVALUATION
      (`eval_mask()`, which cannot collapse — every fid needs its own mask at
      its own times); `profvis` or a counter around the two closures suffices,
      the ratio matters more than the absolute. The two numbers decide two
      different questions and must not be conflated:
      - the pass's SHARE of `t_preprocess_sec` decides D3b's fold (is the
        separate pass worth optimizing at all);
      - the MAINTENANCE share WITHIN the pass decides D3c / task 3.0b (is
        atom pooling worth doing) — if evaluation dominates, pooling buys
        little no matter how large the pass is.
      Take D3b's trade only on a measured number: it spends the separate pass's
      isolation (the property that keeps the unconstrained statistics path, and
      therefore the frozen 1e-6 baselines, provably untouched) for one shared
      atom-maintenance pass. If the mask pass is a few percent of preprocessing
      the isolation is nearly free and the refinement is dropped; if it is a
      large share, do 3.0b FIRST (it captures the atom-sharing win without
      touching the hot path) and re-measure before considering the fold at all.
      Write both numbers and the outcome into D3b and D3c.
- [ ] 3.0b Verify and then implement the shared-atom mask pass (design D3c).
      `preprocess_support_mask()` already separates atom MAINTENANCE
      (`apply_atom_event()`) from mask EVALUATION (`eval_mask()` projecting the
      atoms through the boolean tree), but is instantiated once per output, so
      the atoms are re-walked per fid. Verify first, on the landed two-flavor
      fixture, that (a) evaluating a fid's `expr` at a superset of snapshot
      times and slicing its own subset is exact, and (b) the per-output fold
      steps consume the sliced support unchanged. Then maintain the UNION of all
      constraints' atoms once and evaluate each fid's own `expr` at its own
      snapshot times over that shared atom state. Independent of the merged
      walk — extractable to its own change if section 3 stalls.
- [ ] 3.1 Merge the sender and dyad recipe walks into one clock hosting the
      statistic blocks keyed by mode-pair (`stat_block = (model, family,
      mode-pair)`, D8): the single-mode-pair join has the two blocks, a
      multi-mode-pair join has one sender block per distinct sender mode and one
      dyad block per distinct mode-pair; consumers attached per fid;
      cross-mode-pair events right-censor other processes' timed rate fids;
      single-process and flavored specifications route through the merged walk
      byte-identically (frozen-baseline gate at every commit)
- [ ] 3.2 Per-fid preprocessing driver over the merged walk: fid-indexed list
      with the process_map attached, each element passing engine-readiness
      checks; empty-risk-set aborts name the fid label rendered from the
      process_map; compile each `(layer, flavor)` constraint ONCE into the
      merged plan's `plan$support_constraints` indexed directly by
      `constraint_id` (the compile is family-invariant, see design D3b),
      and snapshot the mask PER FID against that fid's own stored `event_time`
- [ ] 3.3 Tests: merged-walk equivalence against the two-walk outputs on
      flavored fixtures; multivariate fixtures per fid; two fids sharing one
      `constraint_id` but with different stored-event timelines (a timed rate
      fid carrying right-censored rows and a choice fid without them) get
      DIFFERENT snapshot sequences from the SAME compiled sub-plan — the
      compile-once/snapshot-per-fid contract, whose violation is a silent
      wrong-mask bug rather than a crash; timing comparison recorded (merge
      must not regress the single-process hot path)
- [ ] 3.4 Verification: full `NOT_CRAN=true` run (baselines PASS not SKIP);
      version bump in DESCRIPTION + NEWS.md entry (merged-walk milestone);
      commit

## 4. Walk handle (stepping + injection)

- [ ] 4.1 `walk_open()` / `walk_advance()` / `walk_evaluate()` /
      `walk_inject()` over the merged walk's stepper; evaluation applies the
      fid's compiled mask; injection updates shared state for all consumers;
      roxygen with lifecycle experimental badges; `devtools::document()`
- [ ] 4.2 Batch-vs-replay equality: replaying observed fixtures through the
      handle reproduces the batch driver's per-fid quantities
- [ ] 4.3 Tests: advance/evaluate between events, injection visibility across
      fids, handle misuse aborts (evaluate before open, inject out of order)
      with cli errors
- [ ] 4.4 Verification: full `NOT_CRAN=true` run (baselines PASS not SKIP);
      commit

## 5. Documentation and milestone

- [ ] 5.1 Vignette section: specifying co-evolving processes (panel + events),
      coupling and separability, what estimation requires (`estimate_dynes()`
      in the DyNES change); developer documentation of the walk handle for the
      augmenter/simulate consumers
- [ ] 5.2 Verification: full `NOT_CRAN=true` run (PASS not SKIP); version bump
      in DESCRIPTION + NEWS.md entry (multivariate substrate milestone); commit

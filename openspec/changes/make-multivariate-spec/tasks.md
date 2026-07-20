> Sequencing: post-2.0.0, DyNES track. Blocked until `flavored-processes` is
> complete (its D9 fid vocabulary and D10 consumer seam are this change's
> foundation). Precedes `dynes-augmentation`; re-ground that proposal's
> simulation-hook/evaluator seams against `multi-process-walk` before starting.
> Parallel-development ordering (see `.plan/mv_branch.md`): sections 1, 2, and 4
> may run in parallel with `residuals-gof`; section 3 (merged walk) starts only
> after `spec-driven-dispatch` and `multimode-network-support` land — they edit
> the same loop/writer/surface files. Task 3.0 is exempt: it is a measurement
> spike that runs the profiler against already-landed code and edits no source,
> so it can and should run early — its number is an input to D3b, not a
> consequence of the merge.

## 1. Multivariate specification surface

- [ ] 1.1 `make_multivariate_spec(...)`: accept ≥2 specification objects over
      one data object; validate the shared node set, reject DyNAM-i processes,
      require ≥1 panel-observed focal layer (abort for fully observed
      combinations with per-process-estimation guidance); assemble the extended
      process_map (fid rows across processes, shared-constraint ids)
- [ ] 1.2 Coupling detection: mark each fid coupled iff its parsed effects or
      constraint atoms directly reference a panel-observed layer; store the
      `coupled` column; no transitivity
- [ ] 1.3 Print method: processes sectioned per layer (flavors nested), derived
      and combined constraints shown, separable fids marked (cli semantic
      elements; labels rendered from the process_map)
- [ ] 1.4 Tests: composition validation matrix (panel requirement, node-set
      mismatch, DyNAM-i rejection), process_map row/constraint-id correctness,
      coupling via effect vs constraint atom vs windowed effect, print
      snapshots under a pinned cli context
- [ ] 1.5 Verification: full `NOT_CRAN=true` run (baselines PASS not SKIP);
      `devtools::document()`; commit

## 2. Cross-process union planning

- [ ] 2.1 Generalize the union planner across processes: per statistic block,
      deduplicate effect terms across all fids sharing that block's dispatch
      family; per-fid effect maps against the block's union columns; never
      dedup across families
- [ ] 2.2 Generalize consumer routing to the `(layer, flavor) → fid` lookup:
      the schedule carries layer and flavor for every dependent stream (one per
      process), cross-process events right-censor other processes' timed rate
      consumers
- [ ] 2.3 Tests: two-process fixtures with hand-computed dependent/RC/state-only
      partitions per fid, cross-process shared-effect single computation,
      per-fid intercept scalars over per-fid masks
- [ ] 2.4 Verification: `NOT_CRAN=true` run (single-process and flavored paths
      byte-unchanged, baselines PASS); commit

## 3. Merged single-clock walk

- [ ] 3.0 Spike (runnable NOW, before the merge — the flavored driver already
      realizes one mask pass per output): measure what fraction of
      `t_preprocess_sec` the separate constraint-mask pass costs, using
      `.plan/profile_goldfish.R` (it already reports the preprocess/estimate
      split and `object_size_bytes`). Compare constrained vs unconstrained runs
      of the same model on a mid-size and a large dataset, and record the rows.
      This decides design D3b's optional refinement — folding constraint-atom
      maintenance into the merged walk — which trades the separate pass's
      isolation (the property that keeps the unconstrained statistics path, and
      therefore the frozen 1e-6 baselines, provably untouched) for one shared
      atom-maintenance pass. Take the trade only on a measured number: if the
      mask pass is a few percent of preprocessing the isolation is nearly free
      and the refinement is dropped; if it is a large share at realistic sizes,
      schedule it with an explicit baseline-safety plan. Write the outcome into
      D3b either way.
- [ ] 3.1 Merge the sender and dyad recipe walks into one clock hosting both
      statistic blocks, consumers attached per fid; single-process and flavored
      specifications route through the merged walk byte-identically
      (frozen-baseline gate at every commit)
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

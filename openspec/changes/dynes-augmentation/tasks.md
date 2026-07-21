## 1. Spikes and studies (gate design D3/D4 and the mutation move set)

- [ ] 1.1 B1 benchmark spike (scratch branch, results in `.plan/`): pool likelihood
      evaluation with K ∈ {10, 100, 1000, 10000} sequences on the packaged
      `social_evolution` dataset — matrix (a) R loop calling gather + likelihood per
      sequence vs (b) batched evaluation over a list of flat preprocessed objects,
      crossed with BLAS threads default vs pinned to 1 and serial vs per-sequence
      sharded workers, plus one large-n synthetic cell (dyad-indexed model,
      n ≥ 1000) to measure the bandwidth ceiling and the BLAS-thread crossover;
      record wall time, peak RSS, and the BLAS backend
      (`extSoftVersion()["BLAS"]`)
- [ ] 1.2 B3 memory/profiling spike: naive in-memory pool over a (n × events × K)
      grid anchored on the packaged `social_evolution` dataset; acceptance bound
      ~5 GB per machine (divided across parallel workers) for K = 100–1000; record
      measurements and the implied storage decision (in-memory vs broadcast-aware
      on-disk vs DBI writer)
- [ ] 1.3 RSiena ML-estimator study: read the MLE sequence-sampling code
      (`~/Documents/repos/rsiena`), write `.plan/DyNES/rsiena_mle_notes.md` mapping
      proposal moves (permutations, excursion insert/delete), endpoint handling,
      acceptance ratios, identifiability findings, and MC-error/convergence
      diagnostics onto the augmenter/evaluator contracts
- [ ] 1.4 End-to-end toy prototype: small n, 2 waves, random augmenter, existing
      likelihood evaluation, one ascent loop — shake the
      augmenter/evaluator/optimizer contract signatures before implementation; keep
      the toy as the future recovery-test fixture blueprint
- [ ] 1.5 Design revision: fold 1.1–1.4 outcomes back into design D3/D4/D6 and the
      `sequence-augmentation` spec (batching shape, pool storage, mutation move set);
      record the revision in progress.md — later phases MUST NOT start before this
      task closes

## 2. Panel augmentation trigger and wave diffing

- [ ] 2.1 Panel augmentation triggered by formula reference (no separate flag): a
      panel-observed layer referenced in the multivariate spec is an augmentation
      target; scope the panel-focal rule per estimator (event-stream estimators
      abort pointing to `estimate_dynes()`; DyNES accepts a modeled panel process);
      exogenous-only panel references carry a per-layer static-vs-random-augmenter
      choice on the estimation surface
- [ ] 2.2 Wave diffing into candidate flip sets per between-wave interval
      (standalone-usable on a validated data object) plus the endpoint-hitting
      sequence validator
- [ ] 2.3 Tests (testthat 3e: diff fixtures, flag validation, focal-rule scoping,
      cli message snapshots); `devtools::document()`
- [ ] 2.4 Verification: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      version bump in DESCRIPTION + NEWS.md entry (panel seam milestone); commit

## 3. Augmenters (walk-handle drivers)

- [ ] 3.1 Augmenter contract as an external driver of the `multi-process-walk`
      handle (`walk_open`/`advance`/`evaluate`/`inject`, `make-multivariate-spec`);
      no per-event hook is added to the recipe loop and `preprocess-output-writers`
      is not modified by this change — baselines gate the commit
- [ ] 3.2 `augment_seq_random()` (iid-uniform times with within-chain sorting over
      the flip set, closed-form proposal density reported, design D20)
- [ ] 3.3 `augment_seq_sim()`: constrained sequential model-driven draw at `theta`
      driving the walk handle and reusing the `process-simulation` per-step drawing
      core under wave-endpoint conditioning (R-side risk-set restriction over
      support-applicable remaining events plus the globally next relational event;
      truncated-exponential waiting times; full-path proposal density, design D20)
- [ ] 3.4 `augment_seq_mcmc()`: permute + shift move set with rate-based
      truncated-exponential time redraws, unified pred/succ windows, upfront
      exclusion of chain-order-violating swaps, and the injected
      proposal-evaluator closure (design D20; insert/delete excursions recorded
      as future extensions, design D16) with forward/reverse proposal densities
      and the D16 chain lifecycle: warm start into each EM iteration's new
      target, burn-in at every chain restart, same-chain continuation for
      within-iteration growth
- [ ] 3.5 Tests: endpoint-hitting asserted on every draw, proposal-density
      correctness on hand-computed fixtures, walk-handle batch-vs-replay
      consistency; verification run `NOT_CRAN=true` (PASS not SKIP); version bump +
      NEWS (augmentation milestone); commit

## 4. Batched pool evaluator

- [ ] 4.1 C++ batched pool evaluation per the task-1.5 revised contract:
      per-sequence logLik/score/Fisher at `theta` behind the `what` request flag
      (score-only for the SGD loop; Fisher only where consumed; zero optimizer
      iterations) over flat preprocessed objects — cpp-recompile after every
      `src/` edit, before testing
- [ ] 4.2 Per-sequence full re-preprocess pipeline: interval start state via the
      initial-state materializer, recipe run per drawn sequence, pool assembly per
      the task-1.2 storage decision
- [ ] 4.3 Importance weights in the evaluator per design D14: permanent per-sequence
      (θ_ref, log-likelihood at θ_ref, log proposal density) records kept on the log
      scale, model/proposal ratio normalized, effective sample size — and
      `compute_lik_seq()` per-sequence sugar; swap the batched evaluator in behind
      the evaluator contract shipped by the `abmcem` change (its prototype-path
      adapter retires here); `devtools::document()`
- [ ] 4.4 Tests: batched vs zero-iteration-engine equivalence within 1e-10, weight
      fixtures, pool memory within the accepted bound; verification `NOT_CRAN=true`
      (PASS not SKIP); version bump + NEWS (evaluator milestone); commit

## 5. Process simulation — moved out

- [ ] 5.1 (moved) The general `simulate()` surface is implemented by the standalone
      `process-simulation` change. This change consumes it: `augment_seq_sim()`
      (task 3.3) reuses that change's per-step drawing core, and the recovery study
      (task 6.2) simulates panels through it. No simulation tasks remain here.

## 6. Validation study and documentation

- [ ] 6.1 Multi-layer specification validation per design D19 (all-or-nothing
      panel-layer modeling; separability read from the multivariate spec's `coupled`
      column — all-separable aborts, mixed proceeds with a cli message;
      history-span trimming) wired into the `estimate_dynes()` surface shipped by
      the `abmcem` change; cli message snapshots; `devtools::document()`
- [ ] 6.2 Seeded parameter-recovery test (skip_on_cran) from the toy fixture;
      full simulation study incl. creation/dissolution identifiability from waves,
      recorded with the change (progress.md + `.plan/`)
- [ ] 6.3 Vignette on panel-state estimation (wave diffing, algorithm variants,
      reading MC vs asymptotic error); dataset example; `devtools::document()`
- [ ] 6.4 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (DyNES milestone); commit

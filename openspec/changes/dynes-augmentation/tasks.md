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
      ~2 GB per machine (divided across parallel workers) for K = 100–1000; record
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

## 2. Panel-semantics flag and wave diffing

- [ ] 2.1 Implement the per-layer panel-semantics flag as readable metadata
      (validator: only on `observation = "panel"` layers); scope the panel-focal
      rule per estimator (event-stream estimators abort pointing to
      `estimate_dynes()`; DyNES accepts)
- [ ] 2.2 Wave diffing into candidate flip sets per between-wave interval
      (standalone-usable on a validated data object) plus the endpoint-hitting
      sequence validator
- [ ] 2.3 Tests (testthat 3e: diff fixtures, flag validation, focal-rule scoping,
      cli message snapshots); `devtools::document()`
- [ ] 2.4 Verification: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      version bump in DESCRIPTION + NEWS.md entry (panel seam milestone); commit

## 3. Simulation hook and augmenters

- [ ] 3.1 Implement the per-event simulation hook on the recipe loop per the
      documented contract (visible state after event i, event-stream append);
      no-registered-hook path byte-equivalent — baselines gate the commit
- [ ] 3.2 Augmenter contract + `augment_sequence_random()` (uniform ordering/times
      over the flip set, proposal density reported)
- [ ] 3.3 `augment_sequence_model()`: sequential model-driven draw at `theta`
      consuming the simulation hook (rates/probabilities from the estimation kernels
      at the current state)
- [ ] 3.4 `augment_sequence_mutate()`: MCMC move set per the task-1.3 note with
      forward/reverse proposal densities
- [ ] 3.5 Tests: endpoint-hitting asserted on every draw, proposal-density
      correctness on hand-computed fixtures, hook state-visibility; verification run
      `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS (augmentation milestone);
      commit

## 4. Batched pool evaluator

- [ ] 4.1 C++ batched pool evaluation per the task-1.5 revised contract:
      per-sequence logLik/score/Fisher at `theta` (zero optimizer iterations) over
      flat preprocessed objects — cpp-recompile after every `src/` edit, before
      testing
- [ ] 4.2 Per-sequence full re-preprocess pipeline: interval start state via the
      initial-state materializer, recipe run per drawn sequence, pool assembly per
      the task-1.2 storage decision
- [ ] 4.3 Importance weights in the evaluator (model density / proposal density,
      normalized; effective sample size) and `compute_lik_seq()` per-sequence sugar;
      `devtools::document()`
- [ ] 4.4 Tests: batched vs zero-iteration-engine equivalence within 1e-10, weight
      fixtures, pool memory within the accepted bound; verification `NOT_CRAN=true`
      (PASS not SKIP); version bump + NEWS (evaluator milestone); commit

## 5. Optimizers and the estimation surface

- [ ] 5.1 Optimizer contract + `optimize_abem_sgd()` (damped stochastic gradient
      over sampled batches, convergence bookkeeping)
- [ ] 5.2 `optimize_abem_resampling()` and `optimize_abem_is()`
      (importance-sampling update) variants
- [ ] 5.3 `set_algorithm_abem()` control object (cli validation of variants,
      `n_cores` defaulting within CRAN's 2-core check cap) and `estimate_dynes()`
      running the ABEM loop purely through the three contracts; the sequence-map
      seam (serial default; mirai daemons per design D10 when installed, with
      parallel RNG streams and the non-nested BLAS thread budget); mirai added to
      Suggests; lifecycle experimental badge; `devtools::document()`
- [ ] 5.4 Tests: contract dispatch (no branching inside the loop), toy-fixture
      ascent behavior, control-object validation snapshots; verification
      `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS (estimation-surface
      milestone); commit

## 6. Process simulation

- [ ] 6.1 Resolve the simulation-surface open questions in design.md before
      implementing (entry points and naming, ordered-family default mode, legal
      writer sinks and output class, exogenous horizon behavior, `max_events`
      default, coordination rejection bookkeeping, θ-uncertainty in/out); record
      the decisions as design amendments
- [ ] 6.2 Generative draw machinery over the walk: total rate + exponential waiting
      time (timed sub-models), sender/receiver draws from the Phase-4 rates-at-state
      kernels, stopping by horizon or fixed event count, `max_events` explosion
      guard with total-rate diagnostic
- [ ] 6.3 Ordered/Cox-like timing modes: fixed-template-times-redraw-dyads and
      crude-rate pseudo-time (reusing the intercept scalars); scale-caveat
      documentation on pseudo-time output
- [ ] 6.4 Coordination mutual-choice rejection scheme (uniform sender, crude-rate
      waiting time, accept iff reciprocated) with acceptance-rate diagnostic
- [ ] 6.5 Simulation surface per the task-6.1 decisions: entry points, flavored
      competing-flavor draws under derived masks, evaluator-compatible pool output,
      optional writer-sink statistics recording; `devtools::document()`
- [ ] 6.6 Tests: seeded intensity/event-count sanity on timed fixtures,
      template-times preservation, acceptance-rate fixtures, explosion abort
      snapshot, simulate→evaluate round trip at generating vs perturbed parameters;
      verification `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS (simulation
      milestone); commit

## 7. Results, validation study, and documentation

- [ ] 7.1 Result class with Fisher approximation, MC standard errors, convergence
      diagnostics; `vcov()` (NA-padded fixed parameters), `summary()`/`print()` via
      cli with pinned-context snapshots
- [ ] 7.2 Seeded parameter-recovery test (skip_on_cran) from the toy fixture;
      full simulation study incl. creation/dissolution identifiability from waves,
      recorded with the change (progress.md + `.plan/`)
- [ ] 7.3 Vignette on panel-state estimation (wave diffing, algorithm variants,
      reading MC vs asymptotic error); dataset example; `devtools::document()`
- [ ] 7.4 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (DyNES milestone); commit

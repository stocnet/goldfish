> **Merge order (cross-change).** This change is coupled to two others that are not
> yet started: `abmcem` (the `estimate_dynes()` surface, EM loop, controls, result
> contract) and `process-simulation` (the per-step drawing core `augment_seq_sim()`
> reuses). The coupling to `abmcem` is **bidirectional** — this change's task 4.3 swaps
> the real batched evaluator in *behind* abmcem's evaluator contract, and abmcem's own
> surface task wires `complete_generative_spec(wave_times = …)` *from* this change's
> wave grid (task 6.1). **Landing order:** `abmcem` lands first against its
> **prototype-path (zero-iteration) evaluator** — that adapter is the integration seam;
> this change then retires the adapter and swaps in the real augmenters + batched
> evaluator. `process-simulation` must land before task 3.3 (`augment_seq_sim()`).
> Keep this order in sync with the mirror note in `abmcem/tasks.md`.
>
> **Prerequisite (cross-change).** The joint/multivariate parameter surface —
> `set_parameters()` → `goldfishParams` and `coef_layout()` — lives in the
> `joint-parameters` change (extending the `multivariate-specification`
> capability). `estimate_dynes()` (`abmcem`) takes it as `initial_parameters` and
> `simulate()` (`process-simulation`) as `coef`, so `joint-parameters` lands
> **before** the abmcem surface. It is a pure projection over
> `make_joint_specification()`'s `process_map` + the parsed offset mask and has no
> dependency on this change. This change does not consume `goldfishParams`
> directly (its augmenters receive θ from the loop as a flat vector), so the note
> is a landing-order reminder only.

## 1. Spikes and studies (gate design D3/D4 and the mutation move set)

- [ ] 1.1 B1 benchmark spike (scratch branch, results in `.plan/`): pool likelihood
      evaluation with K ∈ {10, 100, 1000, 10000} sequences on the packaged
      `social_evolution` dataset — matrix (a) R loop calling gather + likelihood per
      sequence vs (b) batched evaluation over a list of flat preprocessed objects,
      crossed with BLAS threads default vs pinned to 1 and serial vs per-sequence
      sharded workers, plus one large-n synthetic cell (dyad-indexed model,
      n ≥ 1000) to measure the bandwidth ceiling and the BLAS-thread crossover;
      record wall time, peak RSS, and the BLAS backend
      (`extSoftVersion()["BLAS"]`).
      **Load-bearing cell (design D3):** the DyNES estimand is DyNAM rate × choice —
      both O(events · n · p) light families — so the **actor-oriented rate+choice
      cells gate the v1 engine decision**, which is expected to resolve to Option A
      (cache reduced/thin θ-free stacks, GEMV reweight; no new kernel). The
      dyad-indexed large-n cell measures a family DyNES does not estimate (O(n²)
      stack): keep it as a **bandwidth-ceiling / future-proofing** probe for a
      possible REM-flavored panel layer, but it MUST NOT gate the v1 engine or
      justify building the Option C decode-reduce fusion.
- [ ] 1.2 B3 memory/profiling spike: naive in-memory pool over a (n × events × K)
      grid anchored on the packaged `social_evolution` dataset on the broadcast
      representation; acceptance bound set **empirically from the measurements** (no
      fixed byte threshold; the old ~2/5 GB figures referred to a superseded heavier
      representation), divided across parallel workers, for K = 100–1000; record
      measurements and the implied storage decision (in-memory vs broadcast-aware
      on-disk vs DBI writer)
- [ ] 1.3 Move-set justification note (design D6; **de-scoped from the RSiena study**):
      write `.plan/DyNES/rsiena_mle_notes.md` recording (a) the **fixed-cardinality
      ergodicity argument** — the wave diff fixes the PE event count per interval
      (no net-zero excursions, D8), so permute + shift span the endpoint-conditioned
      space and RSiena's insert/delete (increment/reduce) moves draw paths outside v1's
      estimand, deferred as D16 future extensions — and (b) the accepted no-excursion
      limitation (minimal-flip interpretation). Endpoint handling, the
      `α = [f(Ω′)/f(Ω)]·[q_rev/q_fwd]` acceptance ratio, and MC-error/thinning
      diagnostics are already settled in D20/D15/D18 — cite, don't re-derive.
      Identifiability of separate creation/dissolution parameters is **task 6.2's** job
      (answered on DyNES's estimand), not this note's. **Optional de-risk** (only if
      `~/Documents/repos/rsiena` is on hand): cross-check the acceptance-ratio math and
      move-mapping against RSiena's MLE sampling code — not a blocker, concepts
      re-implemented, no code copied.
- [ ] 1.4 End-to-end toy prototype: small n, 2 waves, random augmenter, existing
      likelihood evaluation, one ascent loop — shake the
      augmenter/evaluator/optimizer contract signatures before implementation; keep
      the toy as the future recovery-test fixture blueprint
- [ ] 1.5 Design revision: fold 1.1–1.4 outcomes back into design D3/D4/D6 and the
      `sequence-augmentation` spec (batching shape, pool storage, mutation move set);
      record the revision in progress.md — later phases MUST NOT start before this
      task closes. When folding B1 back, weigh the **actor-oriented rate+choice cells**
      as decisive (the DyNES estimand) and read the dyad-indexed cell as
      future-proofing only (design D3): do not let the O(n²) dyad numbers argue for the
      Option C fusion that the rate+choice estimand never exercises.

## 2. Panel augmentation trigger and wave diffing

- [ ] 2.1 Panel augmentation triggered by modeled-process reference (no separate
      flag): a panel-observed layer that is a modeled process is an augmentation
      target; scope the panel-focal rule per estimator (event-stream estimators
      abort pointing to `estimate_dynes()`; DyNES accepts a modeled panel process);
      an exogenous-only panel reference stays a static step-covariate (no
      augmentation, no random sampling); `estimate_dynes()` aborts (naming
      `estimate_dynam()`, static-exogenous message) when no panel layer is a modeled
      dependent process
- [ ] 2.2 Wave diffing into candidate flip sets per between-wave interval
      (standalone-usable on a validated data object) plus the endpoint-hitting
      sequence validator
- [ ] 2.3 Compile the diff output once at estimation entry into the shared
      `augmentation_recipe` (θ-free, sequence-free: flip sets, per-dyad chains,
      injective flavor map validated exactly once here, risk-set **membership**
      universe — chain membership only; live support from the walk handle — same-wave
      pair/constraint graph, and the per-wave start states materialized once); define
      the mutable per-draw cursor interface the three augmenters hold over it
      (static-plan / mutable-state split, full contract in design D22)
- [ ] 2.4 Tests (testthat 3e: diff fixtures, flag validation, focal-rule scoping,
      `augmentation_recipe` compiled-once + injective-map-validated-once assertions,
      cli message snapshots); `devtools::document()`
- [ ] 2.5 Verification: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      version bump in DESCRIPTION + NEWS.md entry (panel seam milestone); commit

## 3. Augmenters (walk-handle drivers)

- [ ] 3.1 Augmenter contract as an external driver of the `multi-process-walk`
      handle (`walk_open`/`advance`/`evaluate`/`inject`, `make-multivariate-spec`),
      reading the shared static `augmentation_recipe` (task 2.3) through a mutable
      per-draw cursor; no per-event hook is added to the recipe loop and
      `preprocess-output-writers` is not modified by this change — baselines gate the
      commit
- [ ] 3.2 `augment_seq_random()` (iid-uniform times with within-chain sorting over
      the flip set, closed-form proposal density reported, design D20)
- [ ] 3.3 `augment_seq_sim()`: constrained sequential model-driven draw at `theta`
      driving the walk handle and reusing the `process-simulation` per-step drawing
      core under wave-endpoint conditioning (R-side risk-set restriction over
      support-applicable remaining events plus the globally next relational event;
      truncated-exponential waiting times; full-path proposal density, design D20).
      **Time-domain invariants (design D20):** draw waiting times from the walk
      handle's `Λ` (the same rate the rate kernel's `Δt·Λ` compensator uses — the
      time-domain fid-for-fid pin), record `q` as a density over times, and carry the
      trailing censored interval `[t_last, t_{m+1}]` in the proposal density.
- [ ] 3.4 `augment_seq_mcmc()`: permute + shift move set with rate-based
      truncated-exponential time redraws, unified pred/succ windows, upfront
      exclusion of chain-order-violating swaps, and the injected
      proposal-evaluator closure (design D20; insert/delete excursions recorded
      as future extensions, design D16) with forward/reverse proposal densities
      and the D16 chain lifecycle: warm start into each EM iteration's new
      target, burn-in at every chain restart, same-chain continuation for
      within-iteration growth.
      **Time-domain invariants (design D20):** rate-based time redraws use the walk
      handle's `Λ`; `q_fwd`/`q_rev` are products of truncated-exponential *time*
      densities; the trailing censored interval to the wave endpoint is a
      non-cancelling term in `f` and `q` alike.
      **Pool-reuse guard (design D23):** the chain already holds `current_loglik` every
      step (the MH denominator `f(Ω)`). v1 discards it and re-preprocesses at pool entry
      (A0), so no stats retention is built here — but the accepted-vs-last-proposal
      invariant is the contract the D23 reuse fast-follow (A3) depends on: retain the
      **current accepted** `(current_stats, current_loglik)`, adopted together on accept
      and held across rejects, never the last (possibly rejected) proposal's pair.
- [ ] 3.5 Tests: endpoint-hitting asserted on every draw in tests and an opt-in debug
      path (trust-by-construction in production, no per-draw hot-path check),
      proposal-density correctness on hand-computed fixtures, walk-handle
      batch-vs-replay consistency; verification run `NOT_CRAN=true` (PASS not SKIP);
      version bump + NEWS (augmentation milestone); commit.
      **Restriction-relationship fixture (design D20):** on a hand-built two-wave case,
      assert the proposal normalizer runs over the endpoint-restricted **observed-flip**
      set while the evaluator's likelihood normalizer runs over the **full structural
      support** (they differ by the unobserved-but-creatable alternatives), that the
      placed flip's numerator rate matches on both sides (the density pin), and that the
      resulting `f/q ≠ 1` equals the hand-computed `Σ_observed rate / Σ_full rate` ratio.
      **Injected-substream determinism (design D20, resolved RNG open question):** assert
      each augmenter draw is a pure function of its injected `L'Ecuyer-CMRG` substream
      state — the same injected state reproduces the same sequence and proposal density
      bit-for-bit (`random`/`sim`/`mcmc`), and the augmenter reads no global RNG state.
      This is the augmenter-side half of the cross-`n_cores` promise; the seam-side keying
      (`(iteration, draw_index)`) and index-ordered reduction are tested in abmcem.

- [ ] 3.6 Cold-start near-target property (design D14/D1 resolution, augmenter side):
      roxygen caveat on `augment_seq_random()` that it is the sanctioned cold start
      **only near θ₀ = 0** (uniform draws = the near-uniform model there — uniform
      order-statistic times, uniform ordering), and away from 0 the model-driven
      `sim`/`mcmc` routines are preferred; test on a toy two-wave fixture that at
      θ₀ = 0 the `random` proposal's importance weights are non-degenerate
      (ESS ≈ K), and that a θ₀ far from 0 drives ESS toward 1 — the fixture the
      abmcem-side diagnostic + construction guard are tested against. `initialize =
      "sim"` is the MCMC-routine mitigation (task 3.4), inert for a standalone
      `random` run.

## 4. Batched pool evaluator

- [ ] 4.1 C++ batched pool evaluation per the task-1.5 revised contract:
      per-sequence logLik/score/Fisher at `theta` behind the `what` request flag
      (score-only for the SGD loop; Fisher only where consumed; zero optimizer
      iterations) over flat preprocessed objects — cpp-recompile after every
      `src/` edit, before testing.
      **Reuse boundary (design D3):** the evaluator's inner call is the compiled
      `compute_*_selection()` kernels + `event_reductions.{cpp,h}` primitives over the
      whole event stream — **not** `evaluate_process_state()` / `materialize_process_state()`.
      Those two are the augmenter's R-mirror substrate: the former returns the full
      candidate space for a *single* event, the latter replays from event 0 (O(events)
      per call, O(n²) if looped over a sequence). Reaching for either inside the
      evaluator is the anti-pattern. The per-event math is pure reuse (pinned to the R
      mirror at 1e-10 — the reused correctness contract to *extend*, per task 3.5, not
      re-derive); the only new C++ is the K-loop batching wrapper, plus the conditional
      broadcast-decode-reduce fusion gated on B1 (task 1.1).
      **Compensator emits the censored tail (design D20):** the rate fid's
      `intervalLogL = −Δt·Λ` fires on every interval including `is_dependent = 0`, so the
      evaluator MUST receive the trailing `[t_last, t_{m+1}]` interval per between-wave
      path with `Δt = t_{m+1} − t_last`; if the wave boundary truncates it, `f` silently
      drops a term that does not cancel against `q`.
- [ ] 4.2 Per-sequence full re-preprocess pipeline: per-wave start states
      materialized **once** and cached with the static plan (identical across
      endpoint-hitting sequences, θ-free — task 2.3), recipe run per drawn sequence
      over the shared start states, pool assembly per the task-1.2 storage decision.
      **Preprocess per between-wave window (design D5/D20):** anchor each path at its
      D5 per-wave start state and treat `t_{m+1}` as the window end, so the ordinary
      end-of-observation right-censoring emits the trailing `−Δt·Λ` compensator with no
      new RC logic (the merged-walk RC contract covers modeled/covariate-event
      boundaries, **not** panel wave times). If instead the multi-wave sequence is
      preprocessed as one stream, inject an interior-wave RC boundary for every timed
      rate fid at every wave time — and test it.
      **Attachment-optional pool entry (design D23):** shape the pool-write path to
      accept an *optional* precomputed `(stats, loglik@θ_ref)` attachment — absent in v1
      ⇒ from-scratch recompute (A0, the baseline above); present-and-consistent
      (`θ_ref == θ_k`, layout tag matches) ⇒ adopt. This keeps the D23 MCMC-reuse
      fast-follow (A3) a drop-in producer (only the abmcem-side `eval_fn`-return widening
      remains) rather than a rewrite of the pool contract; `random`/`sim` carry no
      attachment and fall through to recompute.
- [ ] 4.3 Importance weights in the evaluator per design D14: permanent per-sequence
      (θ_ref, log-likelihood at θ_ref, log proposal density) records kept on the log
      scale, model/proposal ratio normalized, effective sample size — and
      `compute_lik_seq()` per-sequence sugar; the evaluator returns the **classed
      E-step object** (`goldfishEstepIS`/`goldfishEstepResampling`/
      `goldfishEstepUniform`, a `goldfishDynesEstep`)
      that abmcem's `compute_q()`/`compute_ase()` generics dispatch on (design D15
      seam: this change constructs the object, abmcem owns the generics); swap the
      batched evaluator in behind the evaluator contract shipped by the `abmcem` change
      (its prototype-path adapter retires here); `devtools::document()`
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
      the `abmcem` change; **plus the D21 wiring**: `estimate_dynes()` calls
      `complete_generative_spec(spec, consumer = "estimate_dynes", wave_times = <panel
      wave grid>)` once at entry, sourcing `wave_times` from this change's wave-diffing
      grid (task 2.2/2.3) — omitting it silently degrades a multi-wave timed-rate pin
      to one flat plateau (design D21). The completed spec feeds all four paths
      (three augmenters + evaluator) so they agree fid-for-fid. Seam: the
      `complete_generative_spec()` primitive is `make-multivariate-spec`'s and the
      `estimate_dynes()` surface is `abmcem`'s; the wave grid it must be called with is
      this change's. cli message snapshots; `devtools::document()`
- [ ] 6.2 Seeded parameter-recovery test (skip_on_cran) from the toy fixture;
      full simulation study incl. creation/dissolution identifiability from waves,
      recorded with the change (progress.md + `.plan/`)
- [ ] 6.3 Vignette on panel-state estimation (wave diffing, algorithm variants,
      reading MC vs asymptotic error); dataset example; `devtools::document()`
- [ ] 6.4 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (DyNES milestone); commit

> **Merge order (cross-change).** This change is coupled to two others: `abmcem` (the
> `estimate_dynes()` surface, EM loop, controls, result contract) and
> `process-simulation` (the per-step drawing core `augment_seq_sim()` reuses).
> **Status 2026-09-29:** `abmcem` has landed (46/46) against its stub augmenter and
> zero-iteration adapter — the seam this change replaces. The coupling to `abmcem` is **bidirectional** — this change's task 4.3 swaps
> the real batched evaluator in *behind* abmcem's evaluator contract, and abmcem's own
> surface task wires `complete_generative_spec(wave_times = …)` *from* this change's
> wave grid (task 6.1). **Landing order:** `abmcem` lands first against its
> **prototype-path (zero-iteration) evaluator** — that adapter is the integration seam;
> this change then retires the adapter and swaps in the real augmenters + batched
> evaluator. `process-simulation` must land before task 3.4 (`augment_seq_sim()`).
> **Status 2026-09-18 (`process-simulation`):** the part task 3.4 needs has landed on `feature_simulation`
> and is merged here (`8c8275d`) — the driver, its five plug points, the
> exponential free-running clock, the guard/pool surface, and the walk-handle lifts
> (support constraints, composition change, windowed effects) of its tasks 2.0–2.2m.
> Its open tasks (2.3–2.11: anchored variant, parametric and ordinal clocks,
> coordination, flavored competing draws, the exposure integral) are not on task
> 3.4's path. Keep this order in sync with the mirror note in `abmcem/tasks.md`.
>
> **Export handoff (cross-change, `abmcem` D2/Q1).** `abmcem` implements and tests
> `estimate_dynes()` through a stub augmenter but ships it **unexported** (no
> `@export`, absent from `NAMESPACE`) — its four control constructors export, the
> driver does not — so no user can reach an exported surface that would only abort
> for lack of a real augmenter. **This change owns adding the `@export`**: once the
> real augmenters (Phase 3) and batched evaluator (Phase 4) exist, task 6.1 adds
> `@export` to `estimate_dynes()` and re-runs `devtools::document()`. Keep this in
> sync with `abmcem`'s task 5.2 and its `dynes-estimation` spec note.
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

> **TDD.** Implementation tasks below run as red/green pairs — an "(a)" task
> writes a failing test and commits it RED, the paired "(b)" task makes it pass and
> commits GREEN (per `openspec/config.yaml` `rules.tasks`). Spikes, studies, vignettes,
> and characterization tests over already-implemented behavior are not paired — there
> is no new implementation for them to drive red-then-green — and stay single tasks.

## 1. Spikes and studies (gate design D3/D4 and the mutation move set)

- [x] 1.1 B1 benchmark spike (scratch branch, results in `.plan/`): pool likelihood
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
      **Done 2026-09-29** — results in `.plan/dynes/b1/B1_results.md` (local): the
      existing `cpp` backend is already the Option C path and wins on rate+choice
      (faster than A, ~7× lighter; A infeasible at K = 10000); BLAS irrelevant;
      the large-n dyad cell was not run (non-gating). Task 1.5 folds this into D3/D4/D10.
- [x] 1.2 B3 memory/profiling spike: naive in-memory pool over a (n × events × K)
      grid anchored on the packaged `social_evolution` dataset on the broadcast
      representation; acceptance bound set **empirically from the measurements** (no
      fixed byte threshold; the old ~2/5 GB figures referred to a superseded heavier
      representation), divided across parallel workers, for K = 100–1000; record
      measurements and the implied storage decision (in-memory vs broadcast-aware
      on-disk vs DBI writer)
      **Done 2026-09-29** — results in `.plan/dynes/b3/B3_results.md` (local),
      measured on abmcem's real `pool_entry_task()` entry: **in-memory, no on-disk
      or DBI**, provided the per-window start state is held once per process (D5).
      Shared, a sequence costs ≤ ~0.45 KB per path event, independent of n
      (0.55 MB at n = 1000); unshared, today's entry is O(n²) (59 MB/seq at
      n = 1000). The cpp closure's `stat_mat_init` re-duplicates it, and the
      replicated seam is the binding bound at W > 1. Acceptance bound for 4.2a:
      ≤ 1 KB/path event marginal + one start state per window per process. Task
      1.5 folds this into D4/D5 and 4.2.
- [x] 1.3 Move-set justification note (design D6; **de-scoped from the RSiena study**):
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
      **Done 2026-09-29** — `.plan/dynes/rsiena_mle_notes.md` (local; `.plan/DyNES/`
      is the same directory on this case-insensitive FS). RSiena cross-check not
      done (repo not on hand). Two findings for 1.5: D6's "ergodic" claim holds on
      coupling-free specs and on cross-dyad coupling that is a precedence order or
      a one-sided counter bound, **not** for general state-dependent constraints
      (possible reducibility, not just slow mixing). And 6.2b's optional
      state-panel CTMC sibling measures the minimal-flip bias rather than passing
      to MC error.
- [x] 1.4 End-to-end toy prototype: small n, 2 waves, random augmenter, existing
      likelihood evaluation, one ascent loop — shake the
      augmenter/evaluator/optimizer contract signatures before implementation; keep
      the toy as the future recovery-test fixture blueprint
      **Done 2026-09-30** — `.plan/dynes/toy/TOY_results.md` (local): four spec
      variants (base, cross-layer effects both ways, windows over the observed
      layer, a window over the modeled panel layer) through `estimate_dynes()`
      with a prototype random augmenter. Three ran end to end; the panel-window
      variant is unconstructible (refused twice). Findings for 1.5: the landed
      augmenter/evaluator/optimizer signatures differ from D2; the loop, not the
      evaluator, builds the classed E-step; one-stream preprocessing with
      `end_time = t_M` emits the trailing compensator (D5's per-window route is
      not load-bearing); the completed pin reads wave snapshots strictly before
      the wave, so it is one period late and goes −Inf once 6.1 wires
      `wave_times`.
- [x] 1.5 Design revision: fold 1.1–1.4 outcomes back into design D3/D4/D6 and the
      `sequence-augmentation` spec (batching shape, pool storage, mutation move set);
      record the revision in progress.md — later phases MUST NOT start before this
      task closes. When folding B1 back, weigh the **actor-oriented rate+choice cells**
      as decisive (the DyNES estimand) and read the dyad-indexed cell as
      future-proofing only (design D3): do not let the O(n²) dyad numbers argue for the
      Option C fusion that the rate+choice estimand never exercises.
      **Done 2026-09-30.** The rate+choice cells themselves reversed D3: the existing
      `cpp` backend is Option C and wins on time and memory, with no new kernel and no
      batching wrapper. Dated revision blocks were added to D2, D3, D4, D5, D6, D10,
      D15 and D19, plus Risks and Open Questions. The `sequence-augmentation` spec was
      amended: augmenter contract, scheme-agnostic evaluator, window to the last
      wave, shared start state, and the inclusive-at-wave snapshot. So was
      `dynes-estimation`: a window over a modeled panel layer is out of v1. User
      decisions: the pin bug is fixed in 2.2 (new 2.2e/2.2f), panel windows are out
      of v1, MCMC reducibility under general coupling is a recorded gap, and the
      abmcem defects are flagged there. Tasks 4.1–4.2, 6.1 and 6.2b were rescoped
      to match.

## 2. Panel augmentation trigger and wave diffing

- [x] 2.1a Write failing tests for the panel-augmentation trigger: modeled-process
      reference detection with no separate flag; panel-focal rule scoping per
      estimator (event-stream estimators abort pointing to `estimate_dynam()`; DyNES
      accepts a modeled panel process); `estimate_dynes()` abort (naming
      `estimate_dynam()`, static-exogenous message) when no panel layer is a modeled
      dependent process, with a cli message snapshot on the abort. Run to confirm RED;
      commit the tests alone.
- [x] 2.1b Implement: panel augmentation triggered by modeled-process reference (no
      separate flag) — a panel-observed layer that is a modeled process is an
      augmentation target; scope the panel-focal rule per estimator; an
      exogenous-only panel reference stays a static step-covariate (no augmentation,
      no random sampling). Re-run the 2.1a tests to confirm GREEN; commit.
- [x] 2.2a Write failing tests for wave diffing: candidate-flip-set fixtures per
      between-wave interval on a standalone validated data object, the
      endpoint-hitting sequence validator, and the incomplete-snapshot (`NA` tie
      values) cli abort. Run to confirm RED; commit the tests alone.
- [x] 2.2b Implement: wave diffing into candidate flip sets per between-wave interval
      (standalone-usable on a validated data object) plus the endpoint-hitting
      sequence validator; reject **incomplete** snapshots (`NA` tie values) with an
      informative abort before diffing rather than coercing them to tie-absence
      (missing-data waves are a recorded future goal, not a v1 capability). Re-run
      the 2.2a tests to confirm GREEN; commit.
- [x] 2.2c Write failing tests for **count-panel** wave diffing (design D24): a
      cumulative-count layer diffing to a per-dyad multiset of `Δ` indistinguishable
      events per interval; `Δ < 0` aborting as a data error (cli snapshot); no flavor
      map and no derived support constraint on the resulting recipe; and the
      observation-start anchoring — the interval from the resolved `start_time` to the
      first wave is diffed and augmented like any other, with no wave treated as
      history. Run to confirm RED; commit the tests alone.
- [x] 2.2d Implement: count-panel diffing to per-dyad multiplicities alongside the
      state-panel flip sets, the monotonicity abort, and the observation-start left
      anchor (M waves → M modeled intervals over M+1 boundaries), the entering state of
      the first interval being the initial condition. Re-run the 2.2c tests to confirm
      GREEN; commit.
- [x] 2.2e Write failing tests for the **shared wave snapshot** (added 2026-09-30,
      task 1.5; design Risks "pin one period late"): a panel change-list row stamped
      exactly at a wave time is in that wave's snapshot and in the interval ending
      there; `complete_generative_spec(wave_times = c(t0, w1, …))` on the task-1.4 toy
      shape (`.plan/dynes/toy/`: creations stamped at waves 5 and 10, t0 = 0) pins a
      **finite** first plateau whose count equals the wave diff's first-interval flip
      count; and the per-period counts equal the 2.2b diff's per-interval counts. Run to
      confirm RED (today `panel_wave_risk_set()` reads strictly before the wave and
      pins period 1 to `−Inf`); commit the tests alone.
- [x] 2.2f Implement: one inclusive-at-wave snapshot definition, used by the 2.2b wave
      diff and by `panel_wave_risk_set()`'s per-period counts and endpoint risk sets,
      so a pinned period and its interval always count the same flips. This MUST land
      before task 6.1 wires `wave_times` (with the wiring, every pooled path would
      otherwise score `−Inf`). Re-run the 2.2e tests and the completion suite to
      confirm GREEN; commit.
- [x] 2.3a Write failing tests for the `augmentation_recipe` compiler: assert it is
      compiled exactly once at estimation entry, that the injective flavor map is
      validated exactly once there (not per draw), and the contract of the mutable
      per-draw cursor the three augmenters hold over the static recipe. Run to
      confirm RED; commit the tests alone.
- [x] 2.3b Implement: compile the diff output once at estimation entry into the
      shared `augmentation_recipe` (θ-free, sequence-free: flip sets, per-dyad
      chains, injective flavor map validated exactly once here, risk-set
      **membership** universe — chain membership only; live support from the walk
      handle — same-wave pair/constraint graph, and the per-wave start states
      materialized once); define the mutable per-draw cursor interface the three
      augmenters hold over it (static-plan / mutable-state split, full contract in
      design D22). Re-run the 2.3a tests to confirm GREEN; commit.
- [x] 2.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      `devtools::document()` if this group touched roxygen/exports; commit. **No
      DESCRIPTION version bump and no root `NEWS.md` edit on this branch** — record
      the panel seam milestone as a bullet in the change-local
      `openspec/changes/dynes-augmentation/NEWS.md` (see progress.md "Version /
      NEWS / archival")

## 3. Augmenters (walk-handle drivers)

- [x] 3.1 Augmenter contract as an external driver of the `multi-process-walk`
      handle (`walk_open`/`advance`/`evaluate`/`inject` plus the
      `process-simulation` additions `walk_replay()`, `walk_schedule()`,
      `walk_next_breakpoint()` and `walk_evaluate(sender =)`, design D9
      amendment), reading the shared static
      `augmentation_recipe` (task 2.3) through a mutable
      per-draw cursor; no per-event hook is added to the recipe loop and
      `preprocess-output-writers` is not modified by this change — baselines gate the
      commit (contract plumbing; no bespoke unit test of its own — it is exercised
      through tasks 3.3–3.5's augmenters). **Flavor keys (added 2026-09-28,
      `abmcem` task 6.3):** every augmenter emits sequences whose flavored layers
      carry the flavor keys `add_flavor()` stamps from the data, because `abmcem`'s
      pool entry routes each event to its flavor by those keys; asserted in 3.6.
- [x] 3.1a Verify the substrate on a real DyNES fixture before building on it
      (added 2026-09-18; replaces the design's former "[open — blocks task 3.1]"
      question, which `process-simulation` tasks 2.0a/2.0b/2.0c/2.0f resolved).
      On a mutually-exclusive flavored two-wave fixture with a user support
      constraint, a windowed term and an actor joining mid-sequence, assert:
      `walk_open()` opens (no `goldfish_walk_unsupported`), `walk_evaluate()`
      returns constraint-masked output whose risk set excludes at least one
      alternative, `walk_evaluate(sender =)` agrees row-for-row with the full
      matrix, and `walk_next_breakpoint()` reports the window expiry. A
      characterization test over already-landed behavior, so not a red/green
      pair. **If it fails on a constraint reading a changing object no formula
      term reads, that is `constraint-objects-on-shared-walk`'s open refusal —
      record it and narrow the fixture, do not fix it here.**
      **Done 2026-09-30** — the refusal did not fire; the fixture needed no
      narrowing (`tests/testthat/test-augmenters_dynes.R`).
- [x] 3.2a Write failing tests for the shared restart-ceiling helper: the bounded
      redraw loop terminates on success, the per-`θ_ref` acceptance-rate estimator
      (successes ÷ attempts) is correct on a hand-computed fixture, and the helper
      aborts with a `cli` diagnostic naming the coupled support constraint when the
      restart-count ceiling is exhausted. Run to confirm RED; commit the tests alone.
- [x] 3.2b Implement: shared restart-ceiling helper the reject-and-restart /
      reject-and-redraw driver both `augment_seq_sim()` (task 3.4) and
      `augment_seq_random()` (task 3.3) consume — a bounded loop that redraws an
      interval until it succeeds (sim: the greedy draw completes; random: the
      ordering is support-legal), estimates the per-`θ_ref` success probability as
      the acceptance rate (successes ÷ attempts) for the caller's proposal-density
      normalizer, and **aborts with a `cli` diagnostic naming the coupled support
      constraint** when a restart-count ceiling is exhausted (completion vanishingly
      likely). This helper is the single home for the ceiling constant, the
      acceptance-rate estimator, and the abort message. The one-per-run deduplicated
      coupling **bias warning** is NOT the helper's — it is `sim`-only (task 3.4),
      because `random`'s normalizer cancels — so it lives with the caller, not here.
      Re-run the 3.2a tests to confirm GREEN; commit.
- [x] 3.3a Write failing tests for `augment_seq_random()`: closed-form proposal
      density on hand-computed fixtures; endpoint-hitting asserted on every draw.
      **Support-legality reject-and-redraw fixture (design D20):** on a hand-built
      two-wave fixture with a cross-dyad support coupling, assert
      `augment_seq_random()` never delivers a support-illegal ordering, its recorded
      `q` carries the `− log P(legal)` normalizer whose value equals the observed
      redraw acceptance rate, a coupling-free fixture records `P(legal) ≡ 1` (no
      redraw), and **no** coupling bias warning is emitted (the normalizer cancels).
      Run to confirm RED; commit the tests alone.
- [x] 3.3b Implement: `augment_seq_random()` (iid-uniform times with within-chain
      sorting over the flip set, closed-form proposal density reported, design D20).
      **Support-legality reject-and-redraw (design D20):** `random` is
      support-blind, so under cross-dyad support coupling the sorted ordering can
      fire a flip without applicable support — replay the drawn ordering against the
      support constraints and **reject-and-redraw** until legal (a coupling-free
      spec never redraws) via the shared restart-ceiling helper (task 3.2). Record
      the proposal density as `Σ_j log(c_j!) − n·log L − log P(legal)` with
      `P(legal)` the θ-free legal-ordering fraction estimated by the helper's
      redraw acceptance rate. Because that normalizer is a θ-free pool-wide
      **constant that cancels** in the normalized weights, `random` incurs **no**
      plug-in-normalizer bias and MUST NOT emit the coupling bias warning (contrast
      `sim`, task 3.4); the redraw's purpose is pool hygiene — keeping `f = 0` /
      walk-erroring draws out of the pool. Re-run the 3.3a tests to confirm GREEN;
      commit.
- [x] 3.4a Write failing tests for `augment_seq_sim()`: endpoint-hitting asserted on
      every draw, proposal-density correctness on hand-computed fixtures,
      walk-handle batch-vs-replay consistency.
      **Breakpoint-truncation fixture (design D9/D20 amendments, 2026-09-18):** on
      a two-wave fixture carrying a windowed term (or an exogenous covariate row)
      whose next change falls strictly inside an interval, assert the truncated
      exponential is bounded at `walk_next_breakpoint()` rather than at the next
      relational event / wave end, that a draw reaching the bound advances and
      redraws at the post-breakpoint rate, and that the accumulated `q` equals the
      hand-computed product of per-segment densities (a breakpoint-free fixture is
      unchanged, the one-segment case).
      **Restriction-relationship fixture (design D20):** on a hand-built two-wave
      case, assert the proposal normalizer runs over the endpoint-restricted
      **observed-flip** set while the evaluator's likelihood normalizer runs over
      the **full structural support** (they differ by the unobserved-but-creatable
      alternatives), that the placed flip's numerator rate matches on both sides
      (the density pin), and that the resulting `f/q ≠ 1` equals the hand-computed
      `Σ_observed rate / Σ_full rate` ratio.
      **Restart-normalization fixture (design D20, v2 bias note):** on a hand-built
      two-wave case with cross-dyad support coupling that forces
      `augment_seq_sim()` to strand-and-restart, assert the recorded proposal
      density is the **success-conditioned** (restart-normalized) density, not the
      raw accumulated one, that its completion-probability normalizer equals the
      observed restart acceptance rate (completed ÷ attempted) on the fixture, that
      a coupling-free fixture records normalizer 1 (no restart), and that a whole
      run emits **exactly one** deduplicated coupling warning regardless of how many
      draws restart.
      **Pinned-fid time pin (added 2026-09-25, design D20 invariant 2 amendment):** on
      a two-wave fixture whose modeled panel layer is choice-only in the timed regime
      (so its rate is completed and pinned), assert that the recorded `q`'s
      time density for that layer's flips uses the pinned `intercept_w` of the period
      containing the draw, read from `spec$completed_rates` and not from θ. Also
      assert that the wave-end truncation keeps each draw within one plateau.
      Run to confirm RED; commit the tests alone.
- [x] 3.4b Implement: `augment_seq_sim()`: constrained sequential model-driven draw
      at `theta` as **supplied steps on the `process-simulation` driver**, not a
      bespoke loop — `set_simulation_steps(mark =, clock =, accept =)` on a
      `goldfishSimSteps`, with `parameters` the plain θ_k vector and **`evaluate`
      left at the package default** (design D12; supplying it would break the
      `q`/`f` rate pin): `mark` restricts the sender–flavor set in R to
      support-applicable remaining events plus the globally next relational
      event and draws the receiver among that pair's remaining observed ones;
      `clock` is the truncated exponential; `accept` is the completion check
      feeding the restart normalizer (design D20). **Note (design D12):**
      `process-simulation` D11's table files the DyNES augmenter under `accept`
      alone — that shorthand is narrower than D20's mechanics; implement the
      three plugs and flag the table entry to `process-simulation` rather than
      shrinking the augmenter to fit it. **Time-domain invariants (design D20):**
      draw waiting
      times from the walk handle's `Λ` (the same rate the rate kernel's `Δt·Λ`
      compensator uses — the time-domain fid-for-fid pin; for a completed pinned
      rate that `Λ` is the per-period plateau from `spec$completed_rates` of the
      spec completed at entry, never θ_k, per the D20 invariant 2 amendment of
      2026-09-25), bound the truncation
      at `min(next unplaced RE time, wave end, walk_next_breakpoint(handle))` and
      redraw past a breakpoint the draw reaches (the frozen-rate draw must not
      span a rate change — design D9/D20 amendments), record `q` as a density
      over times, and carry the trailing censored interval `[t_last, t_{m+1}]` in
      the proposal density at the **observation extent**, not at a window expiry
      past it (`observation-tail-right-censoring`). **Completion / reject-and-restart (design D20):** the
      greedy forward draw can strand under cross-dyad support coupling (a step
      disables another latent flip's support); v1 **rejects and restarts** the
      interval draw until it completes when the risk set empties with flips
      unplaced, so the delivered sequence is success-conditioned and the recorded
      `q` is restart-normalized — divided by the completion probability,
      **estimated as the restart acceptance rate** (completed ÷ attempted draws,
      recorded per draw with its reference parameters; exactly 1 on any
      coupling-free spec) — and the importance weight must use that
      success-conditioned `q`. The restart loop, its acceptance-rate estimator, and
      the restart-count-ceiling abort (naming the coupling) are the shared helper's
      (task 3.2); `sim` additionally emits **exactly one** deduplicated `cli`
      warning per estimation run (never per iteration/draw) the first time any
      interval restarts, flagging the plug-in-normalizer bias and naming the
      coupled constraint — this bias warning is `sim`-only (it does not fire for
      `random`, whose normalizer cancels). Cycle-structured latent processes (join →
      produce → close) are a v2 direction needing differently-determined risk sets,
      out of v1 scope. Re-run the 3.4a tests to confirm GREEN; commit.
- [x] 3.5a Write failing tests for `augment_seq_mcmc()`: forward/reverse proposal
      densities and the D16 chain lifecycle (warm start into each EM iteration's new
      target, burn-in at every chain restart, same-chain continuation for
      within-iteration growth) on hand-computed fixtures; upfront exclusion of
      same-dyad chain-order-violating swaps.
      **Cross-dyad support-legality fixture (design D20):** on the same hand-built
      two-wave fixture with a cross-dyad support coupling (e.g. a
      max-outdegree-1 constraint forcing `dissolve` before `create`), assert
      `augment_seq_mcmc()` proposes the cross-dyad-illegal permute (it is not
      pre-excluded, so pick-probability cancellation holds) but rejects it via
      `f(Ω′) = 0 ⇒ α = 0` — the chain stays put, the step is counted as an
      ordinary rejection, no redraw normalizer is applied, and the doomed
      proposal's preprocess is skipped.
      **Real-closure rate case (added 2026-09-24, `rate-queries`):** alongside the
      stub-`eval_fn` tests, one case drives the chain with a real
      `make_proposal_evaluator(sub_models, theta, spec = <completed spec>)` on a
      small completed fixture (the `rq_*` helpers in
      `tests/testthat/helper-rate_queries.R` are the starting point) and asserts
      the `q_rev` of a permute and of a shift reads the rate
      `query_rate_at_position()` returns for the moved pair at `after = ` the
      preceding event's ordinal in Ω′ — so the reverse density is proven to use
      the walk's `Λ`, which the stub cannot show. **Pinned term in `f′/f` (added
      2026-09-25):** on the same completed fixture, two candidates that differ only
      in a pinned fid's event time get closure `loglik` values that differ by exactly
      the pinned compensator difference `e^{c_w} Δ∫|R(t)| dt`. So the acceptance
      ratio sees the fixed-only term (design D21 and its resolved open question).
      Run to confirm RED; commit the
      tests alone.
- [x] 3.5b Implement: `augment_seq_mcmc()`: permute + shift move set with
      rate-based truncated-exponential time redraws, unified pred/succ windows,
      upfront exclusion of chain-order-violating swaps, and the injected
      proposal-evaluator closure (design D20; insert/delete excursions recorded as
      future extensions, design D16) with forward/reverse proposal densities and the
      D16 chain lifecycle: warm start into each EM iteration's new target, burn-in
      at every chain restart, same-chain continuation for within-iteration growth.
      **Cross-dyad support legality — check, no redraw normalizer (design D20):**
      only *same-dyad* chain-order violations are excluded upfront (above).
      *Cross-dyad* support-illegal proposals MUST NOT be excluded from the proposal
      set — that would make the valid-move set state-dependent and break the
      pick-probability cancellation the acceptance ratio relies on — but MUST be
      detected by a cheap support lookup and rejected via the MH ratio as
      `f(Ω′) = 0 ⇒ α = 0`, an ordinary rejected step in the sweep needing **no**
      redraw normalizer (`q_fwd`/`q_rev` untouched; the target genuinely has zero
      mass there). The lookup spares the doomed proposal's preprocess and dodges the
      walk's not-guaranteed-−Inf-on-illegal-transition hazard. **Time-domain
      invariants (design D20):** rate-based time redraws use the walk handle's `Λ`;
      `q_fwd`/`q_rev` are products of truncated-exponential *time* densities; the
      trailing censored interval to the wave endpoint is a non-cancelling term in
      `f` and `q` alike. **Pool-reuse guard (design D23):** the chain already holds
      `current_loglik` every step (the MH denominator `f(Ω)`). v1 discards it and
      re-preprocesses at pool entry (A0), so no stats retention is built here — but
      the accepted-vs-last-proposal invariant is the contract the D23 reuse
      fast-follow (A3) depends on: retain the **current accepted**
      `(current_stats, current_loglik)`, adopted together on accept and held across
      rejects, never the last (possibly rejected) proposal's pair.
      **Rate queries (added 2026-09-24):** `q_rev` comes from the injected
      closure's `rate_queries`, landed by the `rate-queries` change — this task
      builds no rate primitive. Query shape: `list(after = k, layer, flavor,
      sender, receiver)`, where D20's "state after the preceding panel event"
      is `after = ` that event's ordinal among Ω′'s dependent events (`after = 0`
      is the wave-start state; no later row is applied, even one sharing the
      stamp). Request rates only on proposals that need `q_rev`: each request
      opens a walk. Caching a live handle across the chain's proposals is an
      optional optimization for this driver (`rate-queries` D5), not required
      for v1. Re-run the 3.5a tests to confirm GREEN; commit.
- [x] 3.6 Cross-augmenter characterization test: injected-substream determinism
      (design D20, resolved RNG open question) — assert each augmenter draw is a
      pure function of its injected `L'Ecuyer-CMRG` substream state — the same
      injected state reproduces the same sequence and proposal density bit-for-bit
      (`random`/`sim`/`mcmc`), and the augmenter reads no global RNG state. This
      property already holds over the augmenters implemented in 3.3b/3.4b/3.5b (no
      new implementation to drive), so it runs GREEN on first commit rather than
      red-then-green. This is the augmenter-side half of the cross-`n_cores`
      promise; the seam-side keying (`(iteration, draw_index)`) and index-ordered
      reduction are tested in abmcem. **Flavor keys (added 2026-09-28):** on a
      two-flavor fixture, each augmenter's emitted sequence carries the
      `add_flavor()` keys, and `abmcem`'s pool entry routes every event to its own
      flavor (the augmenter-side counterpart of `abmcem` test 6.3a(iii), which
      pins it on a hand-built sequence). Unlike the determinism property this
      one is not known to hold yet; if it goes red, fix the augmenter's
      emission, not the pool entry.
- [x] 3.7 Cold-start near-target property (design D14/D1 resolution, augmenter side):
      roxygen caveat on `augment_seq_random()` that it is the sanctioned cold start
      **only near θ₀ = 0** (uniform draws = the near-uniform model there — uniform
      order-statistic times, uniform ordering), and away from 0 the model-driven
      `sim`/`mcmc` routines are preferred; test on a toy two-wave fixture that at
      θ₀ = 0 the `random` proposal's importance weights are non-degenerate
      (ESS ≈ K), and that a θ₀ far from 0 drives ESS toward 1 — the fixture the
      abmcem-side diagnostic + construction guard are tested against. `initialize =
      "sim"` is the MCMC-routine mitigation (task 3.5), inert for a standalone
      `random` run.
- [x] 3.8 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      commit. **No DESCRIPTION version bump and no root `NEWS.md` edit on this
      branch** — record the augmentation milestone as a bullet in the change-local
      `openspec/changes/dynes-augmentation/NEWS.md` (see progress.md "Version /
      NEWS / archival").

## 4. Batched pool evaluator

- [x] 4.1a Write failing tests for the batched pool evaluator: equivalence against
      the zero-iteration engine within 1e-10 (per-sequence logLik/score/Fisher, the
      score-only path for the SGD loop, the Fisher-only-where-consumed path) over
      flat preprocessed objects; assert the evaluator receives and uses the trailing
      `[t_last, t_{m+1}]` interval per between-wave path with
      `Δt = t_{m+1} − t_last`. **Fixed-only sub-models (added 2026-09-25):** the
      batched evaluator reproduces the `abmcem` 4.1b-i fixed-only contract at 1e-10.
      That covers the log-likelihood contribution with no score or Fisher block, the
      multi-plateau pinned rate's per-period offset column, and the completed
      uniform choice with no statistic columns. **Joint likelihood (added
      2026-09-28, `abmcem` task 6.3):** the equivalence runs against the adapter's
      joint single-clock likelihood, so it covers `abmcem`'s "a multi-process
      likelihood carries cross-process censoring" and "flavored processes are
      scored on their own events" scenarios at 1e-10. Run to confirm RED; commit
      the tests alone.
- [x] 4.1b Implement: pool evaluation per the task-1.5 revised contract (design D3
      "Phase-1 revision"): a **per-sequence loop over `cpp`-backend engine closures**
      (`make_engine_evaluator(backend = "cpp")`), built at evaluation time by default
      (design D4 revision: a closure held per sequence re-duplicates the start state
      through `stat_mat_init`), harvesting per-sequence logLik/score/Fisher at `theta`
      behind the `what` request flag (score-only for the SGD loop; Fisher only where
      consumed; zero optimizer iterations) over flat preprocessed objects. No batching
      wrapper and no new kernel (R glue is ~2% of the kernel call, B1); a
      loglik/score-only kernel flag is optional. If any `src/` edit is made,
      cpp-recompile after every one, before testing. **Reuse boundary (design D3):** the evaluator's inner
      call is the compiled `compute_*_selection()` kernels +
      `event_reductions.{cpp,h}` primitives over the whole event stream — **not**
      `evaluate_process_state()` / `materialize_process_state()`. Those two are the
      augmenter's R-mirror substrate: the former returns the full candidate space
      for a *single* event, the latter replays from event 0 (O(events) per call,
      O(n²) if looped over a sequence). Reaching for either inside the evaluator is
      the anti-pattern. The per-event math is pure reuse (pinned to the R mirror at
      1e-10 — the reused correctness contract to *extend*, per task 3.6, not
      re-derive); the broadcast decode-reduce is the existing `cpp` backend (B1), so
      no new C++ is expected.
      **Compensator emits the censored tail (design D20):** the rate fid's
      `intervalLogL = −Δt·Λ` fires on every interval including `is_dependent = 0`,
      so the evaluator MUST receive the trailing `[t_last, t_{m+1}]` interval per
      between-wave path with `Δt = t_{m+1} − t_last`; if the wave boundary
      truncates it, `f` silently drops a term that does not cancel against `q`.
      **Fixed-only sub-models (added 2026-09-25):** carry `abmcem` 4.1b-ii's
      contract into the batched kernel (log-likelihood only, per-period offset
      column read from `spec$completed_rates`, the empty-statistic uniform choice)
      so the swap behind the evaluator contract (4.3b) keeps `f` complete.
      **Joint likelihood (added 2026-09-28, `abmcem` task 6.3):** the pooled
      per-fid objects come from one merged `preprocess_joint()` walk per sequence
      over `abmcem`'s pool engine spec, not from per-process preprocessing. Each
      timed rate fid carries right-censoring rows at every other process's events
      (other layers and other flavors) and is exposed to the end of the
      observation window; each flavor is scored on its own events.
      Re-run the 4.1a tests to confirm GREEN; commit.
> **What `abmcem` already provides for Group 4 (2026-09-29).** Do not rebuild it:
> the weight state (`R/weights_dynes.R`: `new_reference_record()`, LR reweighting,
> ESS, the three resampling schemes, the ESS guard), the classed E-step objects
> (`goldfishEstepIS`/`goldfishEstepResampling`/`goldfishEstepUniform`, parent
> `goldfishDynesEstep`, with the `compute_q()`/`compute_ase()` generics), and the
> pool structure, entry path and resident store (`R/evaluator_dynes.R`:
> `build_sequence_pool()`, `preprocess_pool_sequence()`, `pool_entry_task()`,
> `make_proposal_evaluator()`; `R/pool_store_dynes.R`). What this change adds is the
> **batched C++ kernel** behind that contract, `compute_lik_seq()` (absent from `R/`
> and `src/`), the per-wave start-state cache, and any change the B1/B3 spikes force
> on the pool representation. Tasks 4.2 and 4.3 below are scoped to that remainder.

- [x] 4.2a Write failing tests for the per-sequence re-preprocess pipeline: pool
      memory stays within the empirically-set accepted bound (task 1.2, design D4
      revision: **≤ 1 KB marginal per path event**); per-wave start states are
      materialized once and reused across endpoint-hitting sequences rather than
      recomputed per draw, asserted by **object identity** (the pool holds one start
      state per window per process, not one per sequence); the trailing interval of
      every timed rate ends at the last wave `t_M`; and a pinned rate's interval
      spanning an interior wave is scored at each period's plateau. Run to confirm
      RED; commit the tests alone.
- [x] 4.2b Implement: per-sequence full re-preprocess pipeline: **extend
      `abmcem`'s existing entry path (`preprocess_pool_sequence()`/`pool_entry_task()`)
      rather than writing a second one**, and reconcile the task-1.2 storage decision
      with its resident pool store (`0af5df0`). The decision (design D4 revision):
      in memory, start state shared per window per process, `data` dropped from the
      resident entry once the adapter retires (4.3b); sharding the resident store is
      flagged to `abmcem`, not built here. **Route (design D5 revision):** keep
      abmcem's one-stream entry; the trailing compensator comes from pinning the
      preprocessing window to `[t0, t_M]` (task 6.1b), and a pinned plateau's
      straddle is already split by `pinned_rate_compensator()`. The per-window
      alternative below is superseded; the text stays for the record. Per-wave start
      states materialized **once** and cached with the static plan (identical
      across endpoint-hitting sequences, θ-free — task 2.3), recipe run per drawn
      sequence over the shared start states, pool assembly per the task-1.2 storage
      decision. **Preprocess per between-wave window (design D5/D20):** anchor each
      path at its D5 per-wave start state and treat `t_{m+1}` as the window end, so
      the ordinary end-of-observation right-censoring emits the trailing `−Δt·Λ`
      compensator with no new RC logic (the merged-walk RC contract covers
      modeled/covariate-event boundaries, **not** panel wave times). If instead the
      multi-wave sequence is preprocessed as one stream, inject an interior-wave RC
      boundary for every timed rate fid at every wave time — and test it. That
      boundary has **two** jobs (added 2026-09-25): it emits the trailing
      compensator (D20 invariant 3), and it keeps every interval inside one wave
      period so a pinned rate's per-period offset column reads one `intercept_w`
      (D20 invariant 2 amendment). Either preprocessing route needs a test that
      no interval straddles a wave time.
      **Attachment-optional pool entry (design D23):** shape the pool-write path to
      accept an *optional* precomputed `(stats, loglik@θ_ref)` attachment — absent
      in v1 ⇒ from-scratch recompute (A0, the baseline above); present-and-consistent
      (`θ_ref == θ_k`, layout tag matches) ⇒ adopt. This keeps the D23 MCMC-reuse
      fast-follow (A3) a drop-in producer (only the abmcem-side `eval_fn`-return
      widening remains) rather than a rewrite of the pool contract; `random`/`sim`
      carry no attachment and fall through to recompute. Re-run the 4.2a tests to
      confirm GREEN; commit.
- [x] 4.3a Write failing tests for the importance-weight assembly **as wired to the
      batched evaluator** (the weight arithmetic and the classed E-step objects
      already exist and are tested in `abmcem`; do not re-test them — assert the
      batched path feeds them identically to the adapter): weight fixtures
      covering the permanent per-sequence `(θ_ref, log-likelihood at θ_ref, log
      proposal density)` records on the log scale, the model/proposal ratio
      normalization, effective sample size, and `compute_lik_seq()` per-sequence
      sugar; assert the returned classed E-step object carries both the per-sequence
      Fisher information and the per-sequence score. **Fixed-only term in the
      reference record (added 2026-09-25):** on a fixture with a completed pinned
      rate, the stored `log-likelihood at θ_ref` includes the fixed-only fid's
      θ-free per-path term. It is computed once at pool entry and not recomputed
      at a new θ. It changes the normalized weights across paths but cancels in
      the cross-iteration reweight on a fixed path. Run to confirm RED; commit the
      tests alone.
- [x] 4.3b Implement: the batched evaluator supplies the per-sequence
      log-likelihood/score/Fisher that `abmcem`'s **existing** weight state
      (`R/weights_dynes.R`, design D14: permanent per-sequence (θ_ref, log-likelihood
      at θ_ref, log proposal density) records on the log scale, ratio normalization,
      ESS) and classed E-step object (`goldfishEstepIS`/`goldfishEstepResampling`/
      `goldfishEstepUniform`, a `goldfishDynesEstep`, dispatched on by
      `compute_q()`/`compute_ase()`) consume — plus the new `compute_lik_seq()`
      per-sequence sugar. The E-step object must keep carrying both the
      per-sequence score and Fisher information; extend the existing constructor,
      do not add a parallel one. Swap the batched
      evaluator in behind the evaluator contract shipped by the `abmcem` change (its
      prototype-path adapter retires here); `devtools::document()`. **Sandwich
      ingredients (design D7 / abmcem D9):** the Fisher path emits the per-sequence
      **complete-data information** `Jᵢ`, and the E-step object MUST also carry the
      per-sequence **score** `Sᵢ` (not information alone), because `abmcem` forms
      the Ruth-2024 sandwich `vcov = I_c⁻¹ (Σ w̄ᵢ SᵢSᵢᵀ) I_c⁻¹` (`I_c = Σ w̄ᵢ Jᵢ`) —
      the meat needs the per-sequence scores. Omitting the score forces the
      anti-conservative complete-data inverse in `vcov()`. Re-run the 4.3a tests to
      confirm GREEN; commit.
- [x] 4.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      commit. **No DESCRIPTION version bump and no root `NEWS.md` edit on this
      branch** — record the evaluator milestone as a bullet in the change-local
      `openspec/changes/dynes-augmentation/NEWS.md` (see progress.md "Version /
      NEWS / archival")

## 5. Process simulation — moved out

- [ ] 5.1 (moved) The general `simulate()` surface is implemented by the standalone
      `process-simulation` change. This change consumes it: `augment_seq_sim()`
      (task 3.4) runs as supplied steps on that change's driver, and the recovery
      study (task 6.2) simulates panels through it. No simulation tasks remain
      here. **Consumed surface as landed (2026-09-18, for reference when writing
      3.4b and 6.2):** `simulate()` with `nsim`, `times`, `horizon`/`n_events` and
      `control_sim = set_simulation_guard()`; results `goldfishSim` /
      `goldfishSimPool` with `filter_simulation()`; `set_simulation_steps(
      parameters, evaluate, clock, mark, accept)` and
      `set_parameter_provider(init, at)`; the handle accessors `n_actors()`,
      `process_map()`, `current_time()`, `regime_of()`, `times_of()`,
      `risk_set_axis()`. Its own tasks 2.3–2.11 (anchored variant, parametric and
      ordinal clocks, coordination, windowed free-running, flavored competing
      draws, the exposure integral) are still open there; task 3.4 needs only the
      exponential free-running core, which has landed.

## 6. Validation study and documentation

- [x] 6.1a Write failing tests for multi-layer specification validation: the
      all-or-nothing panel-layer modeling rule; the **gate on a modeled panel dependent
      process** rather than on the `coupled` column (design D19) — a modeled count panel
      whose fids are all separable must estimate, an exogenous-only panel reference must
      abort to `estimate_dynam()`, and a mutually dependent calls/emails specification
      must proceed; the mixed-specification cli message still naming the separable fids
      off the `coupled` column; observation-start anchoring (no wave is history, and
      modeled REs between `start_time` and the first wave contribute likelihood terms);
      and the D21 wiring — asserting
      `estimate_dynes()` calls `complete_generative_spec(spec, consumer =
      "estimate_dynes", wave_times = <panel wave grid>)` once at entry sourced from
      this change's wave-diffing grid (task 2.2/2.3), and that omitting it degrades
      a multi-wave timed-rate pin to one flat plateau; **the estimation window is
      pinned to `[t0, t_M]`** (added 2026-09-30, design D5 revision) for pool entry,
      the proposal evaluator and the warm start, whatever `control_prep` the user
      passed; **a window over a modeled panel layer aborts** naming the layer
      (design D19 narrowing, cli snapshot), while a window over an observed layer
      estimates; **completed fids contribute no
      θ** (added 2026-09-25, design D21): on the completed spec, a completed timed
      rate's `coef_layout()` row is `fixed = TRUE` at its pinned value with `index`
      `NA`, and the free-slot indices skip it; **every DyNES consumer
      resolves the grid the way `estimate_dynes()` does** (added 2026-10-01,
      from `completion-idempotency` design Non-Goals): `compute_lik_seq(
      fit$authored_spec, ...)` at the fit's sequence and θ matches the
      log-likelihood computed on the fit's completed `spec` (today it pins
      the authored spec on the single `default_window()` and disagrees), and
      `simulate()` on a DyNES fit runs on per-wave pins, not one plateau;
      **pins and draws share one grid** (added 2026-10-01, from
      `fixed-loglik-degenerate` Q13): every pinned rate of the completed spec
      has a stored grid (`stored_pin_grid()`) identical to the augmentation
      recipe's `wave_times`, and a completed spec whose pin grid is altered
      to differ aborts at entry with an internal-error class; and a property
      test over each augmenter's draws on a fixture with a zero-count period
      (pinned `-Inf`): no drawn sequence holds an event of that pinned
      process in that period, so its fixed-only term is finite and no pooled
      sequence is dropped as fixed-term degenerate;
      cli message snapshots. Run to confirm RED; commit the tests alone.
- [x] 6.1b Implement: multi-layer specification validation per design D19
      (all-or-nothing panel-layer modeling; the DyNES gate testing for a modeled panel
      dependent process, with the `coupled` column consumed only to name the separable
      fids in the mixed-specification cli message; history anchored on the resolved
      `start_time`, so no wave is trimmed as history) wired into the `estimate_dynes()` surface
      shipped by the `abmcem` change; **plus the D21 wiring**: `estimate_dynes()`
      calls `complete_generative_spec(spec, consumer = "estimate_dynes", wave_times
      = <panel wave grid>)` once at entry, sourcing `wave_times` from this change's
      wave-diffing grid (task 2.2/2.3) — omitting it silently degrades a multi-wave
      timed-rate pin to one flat plateau (design D21). The completed spec feeds all
      four paths (three augmenters + evaluator) so they agree fid-for-fid.
      **Pin-grid invariant (added 2026-10-01):** the one `wave_times` object
      feeds both `complete_generative_spec()` and
      `compile_augmentation_recipe()`, and after completion `estimate_dynes()`
      asserts that every pinned rate's `stored_pin_grid()` equals it,
      aborting with an internal-error class otherwise. A pin and a sampler on
      different grids is the only route to a `-Inf` pin with events in its
      period, which `fixed-loglik-degenerate` drops and reports as a goldfish
      bug; this keeps that bug from arising.
      **Consumer grid resolution (added 2026-10-01):** `compute_lik_seq()` and
      `simulate.goldfishDynesFit()` resolve their grid through
      `panel_wave_grid()`, as `estimate_dynes()` does, and pass it to
      `complete_generative_spec()`. The fit method may instead simulate the
      fit's completed `spec`. Either way, no DyNES consumer completes an
      authored spec on `default_window()`. `completion-idempotency` already
      makes a completed spec keep its pins whatever grid is omitted, so this
      only concerns authored specs. Completion owning the grid instead is
      deferred to v2 (design, Open Questions).
      **Gates (2026-09-30, revised 2026-10-01):** 2.2f (shared snapshot) and
      task group 1 of `fixed-loglik-degenerate` (`pool_fixed_loglik()` flags
      any non-finite fixed-only term as degenerate; `abmcem` follow-up 1) MUST
      both have landed on `feature/dynes` first. **Both met (2026-10-01):**
      2.2f landed earlier; `fixed-loglik-degenerate` group 1 landed as
      `8bc67c11` (tests) → `c032a6f9` (fix). That change's group 2 (the
      `goldfish_dynes_fixed_term_degenerate` warning and the `em_trace`
      `fixed_degenerate` column) does not gate this task. The whole change
      has since landed (2026-10-01): group 2 `5782bc90` → `1044580c`, group 3
      (early stop of a fully dropped pool) `e36962e1` → `65d6427f`, NEWS.d
      fragment `fd922b76`, `status: landed` `7e58130c`. Neither gate needs
      `abmcem` archived: archiving happens at the merge, in the order `abmcem`,
      `fixed-loglik-degenerate`, then this change. Also pin the preprocessing window to `[t0, t_M]` and
      sharpen the panel-window refusal message for a modeled panel layer
      (message only, design D19). Seam: the
      `complete_generative_spec()` primitive is `make-multivariate-spec`'s and the
      `estimate_dynes()` surface is `abmcem`'s; the wave grid it must be called with
      is this change's. `devtools::document()`. **Export handoff (`abmcem` D2/Q1):**
      `abmcem` ships `estimate_dynes()` unexported (stub-tested only); now that the
      real augmenters (Phase 3) and batched evaluator (Phase 4) exist, **add
      `@export` to `estimate_dynes()`** (keeping the
      `lifecycle::badge("experimental")`) and re-run `devtools::document()` so the
      surface becomes user-reachable with a working augmenter behind it. The four
      control constructors are already exported by `abmcem`; only the driver's
      `@export` is this change's to add. Re-run the 6.1a tests to confirm GREEN;
      commit.
- [ ] 6.2 Seeded parameter-recovery test (skip_on_cran) from the toy fixture; full
      **Gate met (2026-10-02):** `singular-sequence-information` landed on
      `feature/dynes` (full NOT_CRAN run: PASS 11748, FAIL 0, SKIP 2, baselines PASS).
      simulation study incl. creation/dissolution identifiability from waves,
      recorded with the change (progress.md + `.plan/`). This validates the
      already-implemented pipeline end to end rather than driving new production
      code, so it is not a red/green pair. **The DGP is `simulate()`**, not a
      study-local simulator (spec `dynes-estimation`): draw from the same
      `goldfishJointSpec` the estimation consumes, with the known parameters as a
      `goldfishParams` from `set_parameters()`, replicates as one `nsim > 1` pool,
      and capped replicates removed with `filter_simulation()` before estimating
      (report how many were dropped). Observe the drawn sequences only at the wave
      snapshots — the panel is a projection of the simulated run, so the same
      object feeds both the truth and the estimand. **Degeneracy envelope (spec
      `dynes-estimation`):** the full study sweeps flips-per-interval
      (~2 → 5 → 15 → 40) at fixed pool size to locate the ESS cliff where
      importance-weighted augmentation degenerates. The located cliff **sets the
      numeric IS-ESS collapse threshold** the runtime switch-to-`augment_seq_mcmc()`
      recommendation fires at — this change owns that threshold; `abmcem` owns the
      message surface that emits it (design Open Questions, "runtime ESS collapse";
      abmcem D14) — and it is recorded as applying to `random`/`sim` only, MCMC's
      uniform weighting making the same statistic trivially the pool size. The
      **operating envelope** the vignette reports (task 6.3) is the **MC standard error
      against the asymptotic SE** at each cell, not the ESS cliff: ESS measures the
      proposal, so a coupled specification can score worse on it while being better
      identified (design D14, "What ESS measures").
      **Optional pinned-rate cell (added 2026-09-25):** one specification whose
      modeled panel layer is choice-only, so its rate is completed and pinned. It
      checks that the plug-in pin (endpoint-averaged risk set, not estimated, design
      D21) does not bias recovery of the authored θ beyond MC error. Report the gap
      between the pin and the DGP's true baseline alongside it.
- [ ] 6.2b Closed-form oracle test (skip_on_cran, design D24): a **count panel** with
      exogenous-only effects and covariates constant between waves has an analytic
      likelihood — dyad counts Poisson in `λ_ij · Δt`, times uniform order statistics
      given the counts — so `estimate_dynes()` must reproduce the Poisson-regression MLE
      to Monte-Carlo error. Unlike 6.2 this target is analytic rather than another draw
      from the same machinery, so it tests the augmenter, the weights and the M-step
      together against something none of them produced. Record the achieved MC error
      alongside the tolerance. (Sibling check, optional: the state-panel analogue against
      the two-state CTMC's closed-form transition probabilities — **a bias measurement,
      not a pass/fail oracle** (task 1.3 finding, design D6): the CTMC's transition
      probabilities include excursions, which v1's minimal-flip estimand excludes, so the
      gap measures the downward minimal-flip bias rather than passing to MC error.)
- [ ] 6.2c Extend the 6.2 degeneracy sweep with a **coupling axis**: repeat the
      flips-per-interval sweep with the panel layer coupled to a densely observed
      event-stream layer (the calls/emails fixture) and with it uncoupled, so the
      envelope separates dimension-driven weight degeneracy from proposal/posterior
      mismatch. A forward draw cannot see the observed events that come *after* the
      placement it is making, which is where the information about latent timing sits, so
      the coupled cell is expected to degrade `sim` specifically — record whether the
      ESS floor the runtime recommendation fires at must differ by coupling.
- [ ] 6.3 Vignette on panel-state estimation (wave diffing, both panel kinds — state and
      count — algorithm variants, reading MC vs asymptotic error; the operating envelope
      stated as the MC-to-asymptotic error ratio, and the runtime
      switch-to-`augment_seq_mcmc()` recommendation with the IS-ESS floor it fires at).
      The vignette MUST keep the two apart: ESS is the **trigger** (a property of the
      proposal — a coupled specification can score low on it precisely because its
      posterior is sharp) and the error ratio is the **verdict**; and it MUST say that
      IS-ESS is uninformative once the user has switched to MCMC, where acceptance rate
      and autocorrelation are the diagnostics. Dataset example; `devtools::document()`
- [ ] 6.4 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); commit. **No DESCRIPTION version bump, no root `NEWS.md` edit, and no
      `/opsx:archive` on this branch** — record the DyNES milestone as a bullet in
      the change-local `openspec/changes/dynes-augmentation/NEWS.md`. The version
      bump, root-NEWS fold, and archival happen once at branch merge (see
      progress.md "Version / NEWS / archival")

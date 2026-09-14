## 1. Simulation surface decisions

- [x] 1.1 Resolve the design.md open questions before implementing (entry points
      and generic naming, ordered-family default mode, legal writer sinks and
      output class, exogenous-horizon behavior, `max_events` default, coordination
      rejection bookkeeping); record the decisions as design amendments
      — done 2026-08-19 (explore session): D2/D3 revised, D6–D9 added,
      ADR-0033/ADR-0034; residual surface items (writer sinks, output class
      name, threshold constants) settle at implementation
- [x] 1.2 Measure the preprocessing substrates before any dispatch flips (added
      2026-09-08): time `preprocess_flavored()` / the recipe loops against
      `preprocess_joint()` on (a) Social Evolution with a full model (rate +
      choice, windowed and interaction terms, a support constraint) and (b)
      CollegeMsg (`.plan/datasets/CollegeMsg.txt`, ~60k events) as the stress
      case; also time one `walk_open()` replay of the observed sequence
      against the batch merged walk, since that is what one `simulate()`
      replicate costs. Record wall time, peak memory and the merged/oracle
      ratio in `.plan/`; the joint test's 0.92x on the two-flavor fixture is
      the only number so far and the fixture is tiny. The result decides
      whether the recipe loops stay as the batch fast path or the merged walk
      becomes the single loop (design D10's ADR-0045 note); no code moves on
      this task
      — done 2026-09-09 (session `goldfish-50`), full write-up in
      `.plan/sp/preprocess_timing_2026-09.md`, scripts beside it. CollegeMsg
      base model: recipe loops 219.6 s, merged 67.2 s, ratio **0.31**; Social
      Evolution the other way, merged 1.31x-1.78x. The reversal is a per-event
      adjacency-matrix copy present in both substrates, so the gate is recorded
      as a **no-go** and the fixes move to `preprocess-one-walk` group 0
      (ADR-0057). Four findings land on THIS change instead: one replay
      replicate is 74% index `data.frame()` construction that
      `walk_evaluate_choice_matrix()` discards; a DyNAM step needs one sender's
      row, not the matrix (D12); `walk_open()` refuses user support constraints
      and both merged entries crash on a windowed term before the intended
      abort; one CollegeMsg replicate extrapolates to about 1.7 hours at 104 ms
      per step, peaking at 4.2 GB over 500 steps

- [x] 2.0 Walk-handle lifts the driver depends on (added 2026-09-07; each with
      **(a) waits on `preprocess-one-walk` task 0.5a**, which stores the support
      mask by its axis-union kind — wiring the mask onto the handle before that
      would inherit one dense n1 x n2 logical per snapshot, which exhausts 24 GB
      on a realistic node set. Both changes run on `feature_simulation`, so this
      is task ordering on one branch, not a cross-branch fold (decided
      2026-09-09; the two changes' spec deltas do not overlap, so neither
      declares `depends-on`).
      Each lift ships with
      the batch-vs-replay equality test in `test-walk_handle.R` extended to
      the lifted case): (a) user support constraints and the derived flavor
      masks maintained live on the handle, replacing the all-active
      `active_sender`/`active_dyad` in `walk_build_state()` (D4); (b)
      node-composition changes advancing through `walk_advance()` (D6); (c)
      window effects in the merged walk (`build_walk_engine()` abort lifted,
      eager expiry pseudo-events per engine) plus a driver-inserted breakpoint
      API on the schedule, since `exo_rows` is precomputed at open (D8); (d)
      per-ego parameters on `evaluate_process_state()` and `walk_evaluate()`:
      `parameters` may be an `n_ego × p` matrix (row-wise product), the
      vector case byte-identical, a coordination fid taking one matrix per
      side (D11). Land (a), (b) and (d) before 2.1; (c) before 2.7. Frozen
      baselines untouched: the lifts are on the stepping handle and the
      evaluators' matrix branch, not the batch loops
      — done 2026-09-13 (session `goldfish-23`), frozen baselines PASS in
      every full run. (d) `9a56835`: vector or `n_ego x p` matrix; a
      coordination fid takes ONE per-actor matrix (each directed half uses its
      sender's row), the per-side shape left to `two-sided-coordination`.
      **Reverted 2026-09-15 (ADR-0074, task 2.0e):** the matrix branch is not
      goldfish's; the variant is the `evaluate` plug point.
      (a) `8641656`: the handle advances the batch walk's own atom stores and
      recomputes masks with `build_mask_maintainer()`; flavored derived masks
      ride the same path; replay matches batch exactly on dyadic,
      receiver-axis and flavored fixtures. **Gap:** a constraint on a changing
      object no formula term reads is not stepped by the shared schedule, so
      the handle aborts on it (`goldfish_walk_unsupported`). (b)+(c)
      `a21b20c`: presence cursors, `walk_schedule()`,
      `walk_next_breakpoint()`; windows already ran on the merged walk
      (`preprocess-one-walk`), so (c) added the breakpoint API and the windowed
      replay tests
- [ ] 2.0e Substrate corrections before 2.1 (added 2026-09-15): (i)
      `git revert 9a56835` — the evaluators and `walk_evaluate()` take a
      vector only; the reverted diff saved as `.plan/per_actor_evaluate.patch`
      and handed to goldfish.latent as its evaluate step (its
      `randomEffects-actor.R`); the three matrix tests go with it. (ii)
      `.pse_eval_choice()` stops building the index `data.frame` its only
      caller discards (parallel integer vectors, as the R estimation backend
      does) and `walk_evaluate()` grows the `sender` argument (D12; absent =
      today's full matrix, so `test-walk_handle.R`'s oracle is unchanged).
      (iii) Re-measure D12's three numbers on the current tree and date them
      in the design. (iv) Correct `preprocess_flavored()`'s header comment
      (two walks per family) or retire the function if
      `printing-homogenization` task 1.1 has landed. Verification:
      `NOT_CRAN=true` green, baselines PASS not SKIP.
- [ ] 2.1 `simulate()` S3 generic + methods (fitted result → θ̂; specification →
      explicit `coef`) with the `times = c("generated", "observed")` axis,
      driving `walk_open`/`walk_advance`/`walk_evaluate`/`walk_inject`; the
      per-step draw-next-mark core factored for reuse by `augment_seq_sim()`.
      One `walk_open()` per replicate, every fid evaluated from its compiled
      engine, no per-sub-model pass (D10). The driver evaluates the completed
      effect-free fids the handle refuses (`assert_walkable_submodels()`):
      uniform choice as an equiprobable draw over the receiver support, pinned
      rate as the period's constant `exp(intercept_w)` (D5). Fit methods
      register per the `fit-class-hierarchy` delta (`goldfishFit` override,
      `goldfishFlavFit` override as one competing run, not a fan-out); the
      result class is `goldfishSim` (confirm against the class-naming guard
      test). The loop is written against the four plug points of D11 from
      the start: exported `set_simulation_steps(parameters, evaluate, clock, mark,
      accept)` → `goldfishSimSteps` and `set_parameter_provider(init, at)` →
      `goldfishParamProvider`, each slot defaulting to the descriptor-keyed
      step, the `evaluate` step receiving the narrow (stats rows, θ, risk
      set, meta) contract and returning the fid's values (D11, ADR-0074);
      `coef` resolves (numeric / `goldfishParams` / provider) to whatever the
      evaluate step accepts — a per-fid vector for the default — before the
      loop; the handle reaches the closures as an opaque object
      through the exported accessors only; construction validates arity and
      dry-runs the closures on a one-actor toy handle; the provider's latent
      path is written per event. Tests: a constant provider reproduces the
      plain `coef` path byte-for-byte; a toy two-regime provider (P1 only)
      and a toy evaluate step forming a per-actor row-wise product (PE, the
      goldfish.latent shape, defined inside the test) drive the loop on a
      seeded fixture, proving both hooks without goldfish carrying either
      variant. Evaluation path
      per D12: `walk_evaluate()` grows a `sender` argument (absent = today's
      full matrix, so the oracle tests are unchanged; supplied = that sender's
      row), and `.pse_eval_choice()` stops building the index `data.frame` its
      only caller discards, using parallel integer vectors as the R estimation
      backend already does. Retirement per
      ADR-0054: this task lands the consumer `materialize_process_state()`
      was reserved for and does not call it, so the function moves from
      `R/process_state_evaluators.R` to a `tests/testthat/helper-*.R` file
      as the batch-vs-replay oracle (header rewritten to say so); confirm
      `joint_simulation_parameters()` / `reconcile_joint_parameters()` are
      called here or retire them the same way
- [ ] 2.2 Exponential free-running clock: total rate + exact exponential waiting
      time with breakpoint redraws (events, exogenous changes, expiries);
      stopping targets `horizon =`/`n_events =` (whichever binds first);
      `max_events` guard (default `10 * n_dep`) with the rate-trajectory early
      trigger and total-rate diagnostic; capped-replicate flagging; exogenous
      state frozen-and-warned past the last observed change
- [ ] 2.3 Time-anchored variant on every family (`times = "observed"`): marks
      redrawn at observed stamps from the fitted conditionals; per-flavor
      anchoring through the regime record
- [ ] 2.4 ~~Weibull/Gompertz free-running clocks~~ **Moved 2026-09-15 to
      `parametric-rates` task 3.4a** (ADR-0074): the clocks are `clock`
      steps owned by the change that owns the distribution axis; this
      change ships the exponential clock and the `clock` plug point they
      plug into, and 2.2's breakpoint contract is what they honor.
- [ ] 2.5 Cox/ordered strategies, keyed on `behavior$timing == "ordinal"`:
      crude-rate pseudo-time reusing the `goldfishStat` scalars
      (`n_dep_events`, `total_time`, `avg_active_entity`) with the up-to-scale
      labeling; anchored documented as the clean variant
- [ ] 2.6 Coordination: the conjunctive mutual-choice draw only (anchored
      mark-multinomial and free-running rejection), through the `mark` plug
      point. ~~The other four mechanisms~~ **Moved 2026-09-15 to
      `two-sided-coordination` task 2.5a** (ADR-0074): per-mechanism mark
      kernels are `mark` steps owned by the change that defines the
      mechanisms; its 2.5 fixtures seed them.
- [ ] 2.7 Windowed effects in free-running simulation (after 2.0c; revised
      2026-09-15, D8): the handle fans an injected source-layer event out to
      each derived `layer_ω` (entry row now, expiry scheduled through
      `walk_schedule()` at `t + ω`, lengths from the unit's plan
      derivations), so a driver injects once; no FIFO structure. Tests:
      eager-recompute agreement on a windowed fixture; two window lengths on
      one layer expire in time order regardless of entry order; an expiry is
      a breakpoint for the clock.
- [ ] 2.8 Per-component regime record (modeled / completed / anchored-replay) on
      `process_map` and print; replay coherence guard (skip-and-count, never
      clamp; incoherence flag past the documented threshold). Unmodeled
      flavors of a relational layer take `anchored-replay` by default
      (ADR-0055): their observed events enter the walk schedule as rows the
      driver never draws, right-censoring the timed engines; the per-flavor
      override keeps the explicit flag. Test: the Fisheries shape (creation
      modeled, dissolution in neither list) simulates with no completion
      warning, replays every dissolution, and reports the skip count
- [ ] 2.9 Flavored/multivariate competing-flavor draws under the live derived
      masks from 2.0a; evaluator-compatible pool output carrying capped flags +
      regime record (a plain list of `goldfishSim` until the
      `dynes-augmentation` pool format lands); optional writer-sink statistics
      recording; `devtools::document()`
- [ ] 2.10 Multi-period relational `|R_w|` slicing (owned here per
      `make-multivariate-spec` D9a — the one genuinely new piece the panel/DyNES
      path did not need): for a **relationally-observed** flavor completed with a
      pinned intercept-only timed rate inside a **wave-gridded** join, slice the
      preprocessed presence walk per inter-wave period to a per-period
      time-weighted `avg_active_entity = (1/T_w)·∫|R_g(t)| dt`, so each plateau's pin
      `log(count_w / (T_w · |R_w|))` uses its own period's risk-set size (the single
      window K=1 case reuses the preprocessed `avg_active_entity` unchanged, already
      served upstream). Period membership follows the half-open
      `findInterval(t, wave_times, rightmost.closed = TRUE)` convention. Tests: a
      multi-period relational fixture reproduces each period's `count_w`; single-window
      reduction matches the upstream value byte-for-byte

- [ ] 2.11 Exposure integral on the merged walk (added 2026-09-15, D5): the
      walk accumulates, per timed fid, `∫|R_w(t)| dt` — the sum over
      intervals of `dt × |active ∩ gated senders|`, presence changes applied
      at their stamps — and stores it beside `total_time` on the
      `goldfishStat` (`exposure_actor_time`); `avg_active_entity` for an
      exact-time family becomes `exposure_actor_time / total_time` (the
      time-weighted value the `intercept-only-rate` and
      `multivariate-specification` living specs already require, in place of
      `assemble_default_output()`'s event-averaged count); the pinned
      completion `log(count_w / exposure_w)` and the intercept seed read it;
      the panel path keeps its period average. Spec delta: MODIFIED
      `active-availability-stat` "avg_active_entity declared by the recipe
      constructor". Frozen baselines PASS (the scalar seeds the intercept
      only); the completion-warning snapshots on composition-changing
      fixtures re-recorded and reviewed.

## 3. Tests and documentation

- [ ] 3.1 Tests (testthat 3e): seeded intensity/event-count sanity on timed
      fixtures, anchored-times preservation, parametric-shape recovery,
      acceptance-rate fixtures per mechanism, trajectory-trigger and explosion
      abort snapshots, capped-replicate exclusion, skip-and-count on a
      misfitting replay fixture, simulate→evaluate round trip at generating vs
      perturbed parameters, batch-vs-replay consistency against the walk handle
      (including a windowed fixture)
- [ ] 3.2 `simulate()` reference + a simulation section in the model-usage
      vignette (the `times` axis, per-distribution clocks, per-mechanism
      coordination, which GOF statistics need which variant);
      `devtools::document()`
- [ ] 3.3 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (simulation milestone);
      commit

## 1. Simulation surface decisions

- [x] 1.1 Resolve the design.md open questions before implementing (entry points
      and generic naming, ordered-family default mode, legal writer sinks and
      output class, exogenous-horizon behavior, `max_events` default, coordination
      rejection bookkeeping); record the decisions as design amendments
      — done 2026-08-19 (explore session): D2/D3 revised, D6–D9 added,
      ADR-0033/ADR-0034; residual surface items (writer sinks, output class
      name, threshold constants) settle at implementation
- [ ] 1.2 Measure the preprocessing substrates before any dispatch flips (added
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

- [ ] 2.0 Walk-handle lifts the driver depends on (added 2026-09-07; each with
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
      the start: exported `set_simulation_steps()` → `goldfishSimSteps` and
      `set_parameter_provider(init, at)` → `goldfishParamProvider`, each
      slot defaulting to the descriptor-keyed step; `coef` resolves (numeric
      / `goldfishParams` / provider) to the per-fid vector-or-matrix shape
      before the loop; the handle reaches the closures as an opaque object
      through the exported accessors only; construction validates arity and
      dry-runs the closures on a one-actor toy handle; the provider's latent
      path is written per event. Tests: a constant provider reproduces the
      plain `coef` path byte-for-byte; a toy per-actor provider and a toy
      two-regime provider drive the loop on a seeded fixture. Retirement per
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
- [ ] 2.4 Weibull/Gompertz free-running clocks by analytic inversion of
      Σλ_i·[G(t+w) − G(t)] per constant-rate segment (common-shape closure);
      seeded shape-recovery fixtures (the `parametric-rates` task 3.4 DGP)
- [ ] 2.5 Cox/ordered strategies, keyed on `behavior$timing == "ordinal"`:
      crude-rate pseudo-time reusing the `goldfishStat` scalars
      (`n_dep_events`, `total_time`, `avg_active_entity`) with the up-to-scale
      labeling; anchored documented as the clean variant
- [ ] 2.6 Coordination per mechanism: anchored mark-multinomial draws for all
      five mechanisms; free-running generative thinning per mechanism
      (conjunctive mutual-choice rejection first, then forcing / confirmation /
      disjunctive / compensatory), rejected proposals consuming clock time,
      acceptance-rate diagnostic, max-proposals bound; seeded from
      `two-sided-coordination` task 2.5 fixtures
- [ ] 2.7 FIFO self-scheduled window expiry (after 2.0c): per-window
      insertion-order queue, expiries as schedule-visible breakpoint events
      replayable from the simulated stream through the handle's breakpoint
      API; eager-recompute agreement test on a windowed fixture
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

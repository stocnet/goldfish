## 1. Simulation surface decisions

- [x] 1.1 Resolve the design.md open questions before implementing (entry points
      and generic naming, ordered-family default mode, legal writer sinks and
      output class, exogenous-horizon behavior, `max_events` default, coordination
      rejection bookkeeping); record the decisions as design amendments
      — done 2026-08-19 (explore session): D2/D3 revised, D6–D9 added,
      ADR-0033/ADR-0034; residual surface items (writer sinks, output class
      name, threshold constants) settle at implementation

## 2. Generative draw machinery over the walk handle

- [ ] 2.1 `simulate()` S3 generic + methods (fitted result → θ̂; specification →
      explicit `coef`) with the `times = c("generated", "observed")` axis,
      driving `walk_open`/`walk_advance`/`walk_evaluate`/`walk_inject`; the
      per-step draw-next-mark core factored for reuse by `augment_seq_sim()`
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
- [ ] 2.5 Cox/ordered strategies: crude-rate pseudo-time (reusing the intercept
      scalars) with the up-to-scale labeling; anchored documented as the clean
      variant
- [ ] 2.6 Coordination per mechanism: anchored mark-multinomial draws for all
      five mechanisms; free-running generative thinning per mechanism
      (conjunctive mutual-choice rejection first, then forcing / confirmation /
      disjunctive / compensatory), rejected proposals consuming clock time,
      acceptance-rate diagnostic, max-proposals bound; seeded from
      `two-sided-coordination` task 2.5 fixtures
- [ ] 2.7 FIFO self-scheduled window expiry: per-window insertion-order queue,
      expiries as schedule-visible breakpoint events replayable from the
      simulated stream; eager-recompute agreement test on a windowed fixture
- [ ] 2.8 Per-component regime record (modeled / completed / anchored-replay) on
      `process_map` and print; replay coherence guard (skip-and-count, never
      clamp; incoherence flag past the documented threshold)
- [ ] 2.9 Flavored/multivariate competing-flavor draws under derived masks;
      evaluator-compatible pool output carrying capped flags + regime record;
      optional writer-sink statistics recording; `devtools::document()`
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

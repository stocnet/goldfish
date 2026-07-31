## 1. Simulation surface decisions

- [ ] 1.1 Resolve the design.md open questions before implementing (entry points
      and generic naming, ordered-family default mode, legal writer sinks and
      output class, exogenous-horizon behavior, `max_events` default, coordination
      rejection bookkeeping); record the decisions as design amendments

## 2. Generative draw machinery over the walk handle

- [ ] 2.1 `simulate()` S3 generic + methods (fitted result → θ̂; specification →
      explicit `coef`) driving `walk_open`/`walk_advance`/`walk_evaluate`/
      `walk_inject`; the per-step draw-next-mark core factored for reuse by
      `augment_seq_sim()`
- [ ] 2.2 Timed sub-models: total rate + exponential waiting time, stopping by
      horizon or fixed event count, `max_events` explosion guard with the
      total-rate diagnostic
- [ ] 2.3 Ordered/Cox-like timing modes: fixed-template-times-redraw-marks and
      crude-rate pseudo-time (reusing the intercept scalars); scale-caveat
      documentation on pseudo-time output
- [ ] 2.4 Coordination mutual-choice rejection scheme (uniform sender, crude-rate
      waiting time, accept iff reciprocated) with acceptance-rate diagnostic
- [ ] 2.5 Flavored/multivariate competing-flavor draws under derived masks;
      evaluator-compatible pool output; optional writer-sink statistics recording;
      `devtools::document()`
- [ ] 2.6 Multi-period relational `|R_w|` slicing (owned here per
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
      fixtures, template-times preservation, acceptance-rate fixtures, explosion
      abort snapshot, simulate→evaluate round trip at generating vs perturbed
      parameters, batch-vs-replay consistency against the walk handle
- [ ] 3.2 `simulate()` reference + a simulation section in the model-usage
      vignette (per-family timing modes); `devtools::document()`
- [ ] 3.3 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (simulation milestone);
      commit

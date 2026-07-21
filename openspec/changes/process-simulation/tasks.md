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

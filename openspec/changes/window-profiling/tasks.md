# Tasks — window-profiling

Post-2.0.0. The ω legs (groups 2–4) need only existing windowed
effects; group 5 is gated on `recency-effects`; group 6's bootstrap on
`process-simulation`. Group 1 happens at proposal time.

## 1. Homogenization edits (proposal-time)

- [x] 1.1 `recency-effects` design: resolve the geometric-ρ open
      question to "fixed-only in v1; profiled by `window-profiling`'s
      shared-rank-walk grid", and note the refit-at-k-and-2k
      diagnostic generalizes to the profile engine
- [x] 1.2 ADR-0028 written (profile engine exempt from replay; winner
      refit eagerly) and cited from this change's design

## 2. Profile engine core (ω)

- [ ] 2.1 Per-key sorted event-time arrays built in one pass (hash
      map: packed key → dynamic array; appends arrive time-sorted)
- [ ] 2.2 Two-pointer annulus sweep updating per-candidate statistics;
      hash-keyed multiset helpers with decrement-and-test-for-zero for
      indicator/threshold statistics; document max/min-type statistics
      as sweep-unsupported (per-candidate recompute fallback)
- [ ] 2.3 Lag-quantile grid defaults + `candidates =` override; grid
      recorded on the profile object
- [ ] 2.4 Agreement tests: sweep statistics vs eager path at every
      candidate on a small fixture (bit-identical)

## 3. Fit-along-grid and selection

- [ ] 3.1 Warm-started fits along the grid; cold-start fallback with
      per-candidate record; cold restart at the argmax
- [ ] 3.2 Winner refit through the eager build-plan path; returned fit
      indistinguishable from a directly specified fit (test: residuals
      and replay on the refit)
- [ ] 3.3 `profile_memory()` surface (name flagged for the
      estimation-naming review) + `goldfishProfile` class,
      print/plot (step curve, selection, interval + irregularity
      caveat) via cli/ggplot conventions
- [ ] 3.4 Brute-force validation: profile curve equals per-candidate
      full refits on a small fixture

## 4. Two-window variant and inference (ω legs complete)

- [ ] 4.1 Constrained 2-D sweep (ω_s < ω_l), warm starts along both
      axes; disjoint-band reparameterization option
- [ ] 4.2 Davies-bounded sup-LR vs the unwindowed baseline; output
      labels the bound and the grid size
- [ ] 4.3 Docs: selection workflow, when to prefer bands, the
      Davies caveat

## 5. Event-index grids (gated on `recency-effects`)

- [ ] 5.1 Integer `k`/`last_k` grid: one buffer walk emits all k ≤
      k_max; default k_max decision
- [ ] 5.2 Geometric-ρ grid: shared rank walk, per-candidate ρ^r
      transform; wire to the recency `"geometric"` kernel
- [ ] 5.3 Composition profiling (one axis fixed) + docs on the joint
      grid's identification cost

## 6. Bootstrap calibration (gated on `process-simulation`)

- [ ] 6.1 Parametric bootstrap: simulate from the fitted baseline,
      sup-LR per replicate over the recorded grid, empirical p;
      parallel over replicates; informative error while the
      simulation surface is absent
- [ ] 6.2 Calibration exercise on a simulated null (documented, not a
      CI test)

## 7. Close-out

- [ ] 7.1 Benchmarks: sweep vs per-candidate replays on
      small/medium/large histories; report in docs
- [ ] 7.2 pkgdown/reference entries; NOT_CRAN suite green;
      DESCRIPTION/NEWS milestone bumps per phase discipline

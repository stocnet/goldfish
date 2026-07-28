> Sequencing: standalone primitive on the DyNES track. **Hard dependent**:
> `make-multivariate-spec` D9 timed-regime completion (tasks 1c.2/1c.3) — this change
> MUST land before that timed branch. Also consumed by `process-simulation`
> (`simulate()` mixed rate-modeled/missing processes) and `dynes-augmentation` (the
> augmenters and pool evaluator place a pinned-rate flavor's events on the shared
> clock). Reuses the landed timed DyNAM-rate hazard path (`exp(intercept)`, no
> covariate columns — `R/process_state_evaluators.R`) rather than a new evaluator
> (D1). Thin boundary (D5): this change pins given consumer-supplied
> `(count_w, T_w, |R_w|)` and defines the per-actor/uniform-sender *semantics*; the
> routines derive counts (wave Hamming diff / observed count), partition periods, supply
> the average risk-set size (relational `avg_active_entity` / panel wave-endpoint average,
> D9), perform the sender draw, and guard empty support. Additive: the single-process / flavored
> estimation path and the frozen 1e-6 baselines are untouched — every verification
> task confirms baselines PASS not SKIP.

## 1. Pinned intercept-only rate representation

- [x] 1.1 Add the intercept-only rate sub-model representation (new source file,
      snake_case, American English): a rate carrying **no covariate columns** and a
      **fixed intercept**, evaluated through the existing constant-hazard/timed-rate
      path `exp(intercept)` (degenerate `has_intercept = TRUE`, zero-effect case, D1).
      Mark the intercept **fixed** on the object so θ-layout / optimizer code can
      exclude it (D4). Lifecycle **experimental** badge on any exported surface;
      roxygen + `devtools::document()`
- [x] 1.2 Tests: the representation exposes a constant intensity and no effect
      columns; it reports zero free parameters; evaluation through the timed-rate path
      yields the same constant intensity for every support-legal actor and introduces
      no second evaluator (testthat 3e, self-contained)
- [x] 1.3 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP);
      `devtools::document()`; commit

## 2. Per-period pin from supplied counts over exposure

- [x] 2.1 Per-period pin `(count_w, T_w, |R_w|) → intercept_w = log(count_w / (T_w · |R_w|))`
      as a **pure function** (D2/D5/D8/D9): consume per-period counts, period durations, and
      the average size of the flavor's rate entity `|R_w|` the caller supplies and produce a
      **piecewise-constant per-actor** hazard, **one plateau per inter-wave period**.
      Dividing by `|R_w|` places the pin at the per-actor layer (RSiena's `÷ n_actors`
      analogue) so it is commensurable with a competing per-actor rate on the shared
      clock. Do NOT diff waves, infer windows, or compute `|R_w|` here — those are the
      consumer's (D5/D9): the caller supplies `avg_active_entity` in the relational case
      and the **wave-endpoint average** `(|R_g(w_{k-1})| + |R_g(w_k)|)/2` in the panel
      case. The pin depends **only** on the supplied counts/durations/risk-set sizes and is
      **never** recomputed from generated/sampled/augmented events
- [x] 2.2 Compute `intercept_w` **once and freeze it** on the completed spec / `process_map`
      fid (D7): read unchanged across every EM / MCMC / simulation iteration; leave no
      per-iteration recompute hook (the latent-count recompute is future development,
      design Open Questions)
- [x] 2.3 Apply the resolved half-open period convention: interior boundaries
      left-closed / right-open, the **final** period right-closed
      (`findInterval(t, wave_times, rightmost.closed = TRUE)`), so a terminal-time event
      is never dropped; the final-period duration past the last wave is the
      consumer-supplied `simulate()` window (D5). Document the chosen convention in roxygen
- [x] 2.4 Tests: `intercept_w` matches hand-computed per-period
      counts / durations / average risk-set sizes on a multi-wave fixture, exercising both
      a relational `|R_w|` (time-weighted `avg_active_entity`) and a panel `|R_w|`
      (wave-endpoint average of the two observed states, D9); distinct plateaus per period
      (a global `count / T_total` would differ when per-period rates differ); `intercept_w`
      is unchanged after a round of generated events is added (frozen, not recomputed)
- [x] 2.5 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP); commit

## 3. Per-actor hazard semantics (draw owned by the routine)

- [x] 3.1 Define the per-actor-hazard + uniform-support-legal-sender **semantics**
      (D4/D5/D8): `exp(intercept_w)` is a per-actor constant hazard — identical for every
      support-legal actor, so it enters the shared-clock superposition `Σ_i exp(·)`
      commensurably with a competing per-actor rate. The sender is uniform over the
      support-legal actors **as a consequence** of the equal per-actor hazards — inherit
      the flavor's support constraints (never fabricate, never borrow a sibling
      flavor's), disallow self-loops as the only automatic restriction. Expose these
      semantics so `estimate_dynes()`, `simulate()`, and the augmenters draw
      consistently; **do NOT** implement the draw or the empty/saturated-support guard
      here (the consuming routine owns them, D5)
- [x] 3.2 Tests: the semantics specify a uniform support-legal sender with self-loops
      excluded and a defined support constraint inherited (a sibling flavor's NOT
      borrowed); the per-actor hazard is commensurable with a competing effect-driven
      per-actor rate on the shared clock; a routine driving the semantics reproduces
      `count_w` in expectation (aggregate `|R(t)| · exp(intercept_w)`) under a changing
      but non-empty risk set; confirm the primitive itself performs no draw and holds no
      empty-support handling
- [x] 3.3 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP); commit

## 4. Zero-free-parameters contract into a joint fit

- [x] 4.1 Enforce the contract (D4): the pinned intercept never enters the fid / θ
      layout and is **excluded from the optimizer's score and Hessian** by
      **θ-independence** (not by iteration-constancy); it MAY be added as a **constant
      offset** to a *reported* total log-likelihood only
- [x] 4.2 Tests: adding a pinned intercept-only rate leaves the fit's θ layout, score,
      and Hessian dimensions unchanged (only a reported log-likelihood offset differs);
      the pinned value is identical across iterations
- [x] 4.3 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP); commit

## 5. User surface: intercept-only ⟺ pinned, and the context-aware warning

- [x] 5.1 In the generative/joint context (`estimate_dynes()` / `simulate()` / D9
      completion) treat an **intercept-only rate** — `rate = ~ 1` with no other rate
      effects, or a completion-supplied rate — as **pinned** (D6): a user-written `~ 1`
      and a completion-supplied rate produce the **same** pinned object; a rate carrying
      **any** effect keeps its estimated baseline intercept unchanged
- [x] 5.2 Leave the single-process path untouched (D6): a bare `rate = ~ 1` in
      `estimate_dynam()` / `estimate_rem()` keeps its existing estimated-intercept
      meaning; add a regression test that this is byte-identical to the pre-change path
      (frozen baselines PASS) and that a pinned rate never appears in θ
- [x] 5.3 Context-aware warning at each consumer entry (D6, `cli`, not suppressed on
      re-entry): `estimate_dynes()` — pinned from the wave Hamming diff (net-change
      floor), no standard error, excluded from estimation; `simulate()` — pinned from
      the observed event count (no SE language). Snapshot both under a pinned cli context
- [ ] 5.4 Tests: user `~ 1` under `estimate_dynes()`/`simulate()` pins identically to
      completion; `~ 1 + effects` keeps an estimated baseline; warning wording per
      consumer (snapshots); warning re-fires when the same spec is routed through a
      second consumer
- [ ] 5.5 Verification: `NOT_CRAN=true` run (baselines PASS not SKIP);
      `devtools::document()`; commit

## 6. Timed-regime scope guard and documentation

- [ ] 6.1 Guard/contract that the primitive is invoked only in the **timed** regime
      (D3): the ordered-regime missing-rate path is `process-simulation`'s and is out
      of scope here. Confirm the consuming completion transform (D9) reaches this
      primitive only on the timed branch; add a defensive check if a non-timed context
      is passed
- [ ] 6.2 Tests: the primitive pins a missing timed rate so events land on the shared
      clock; it is not applied (and, if invoked, guards) in an ordered context
- [ ] 6.3 Documentation: roxygen for the exported surface (constant per-actor hazard,
      pinned per-period intercept `log(count_w / (T_w · |R_w|))`, uniform-sender
      semantics, zero-free-parameter contract, intercept-only ⟺ pinned rule); brief
      developer note for the three consumers (`make-multivariate-spec` D9 completion,
      `process-simulation`, `dynes-augmentation`) stating what they supply (counts,
      periods, `|R_w|` — relational `avg_active_entity` vs panel wave-endpoint average
      (D9) — the draw, the empty-support guard)
- [ ] 6.4 Verification: full `NOT_CRAN=true` run (baselines PASS not SKIP); version
      bump in DESCRIPTION + NEWS.md entry (intercept-only-rate primitive milestone);
      `devtools::document()`; commit

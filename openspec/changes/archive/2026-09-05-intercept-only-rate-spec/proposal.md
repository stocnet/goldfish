## Why

In a **timed** multivariate composition every modeled flavor must place its events
on one shared continuous clock. A flavor that is choice-only (or otherwise missing
its rate) has no waiting-time model, so it cannot fire on that clock — yet adding a
free rate coefficient would silently enlarge θ and change what a joint DyNES fit
estimates. `make-multivariate-spec`'s D9 generative-readiness completion resolves
this with a **constant-hazard rate whose intercept is pinned from observed event
counts, never estimated** — but that primitive has no home. This change defines it
as a standalone rate sub-model so D9's timed-regime branch, `process-simulation`,
and `dynes-augmentation` can all consume one shared, zero-free-parameter
representation instead of re-deriving it three times.

## What Changes

- **An intercept-only rate sub-model primitive**: a constant baseline hazard whose
  single intercept is **pinned**, not estimated. There are no effects and no free
  coefficient — the only "parameter" is the pinned constant, derived entirely from
  supplied counts/exposures. It **reuses** the existing constant-hazard evaluation path
  rather than adding a second evaluator.
- **Per-period pin from supplied counts over exposure**: given a per-period count
  `count_w` inside wave-period `w` of duration `T_w`, the **per-actor** hazard is
  `λ_w = count_w / (T_w · |R_w|)`, stored as `intercept_w = log(count_w / (T_w · |R_w|))`,
  where `|R_w|` is the period's average size of the flavor's rate entity (active senders
  for an actor-oriented flavor, active dyads for a tie-oriented one) — a
  **piecewise-constant baseline hazard**, one plateau per inter-wave period (mirroring
  RSiena's period-specific basic-rate parameters, which likewise divide by `n_actors`,
  against pooled evaluation effects). The **counts, period partition, and `|R_w|` are
  supplied by the consuming routine**, not derived here, and `|R_w|`'s form tracks the
  regime: in the **relational** case it is goldfish's time-weighted `avg_active_entity`
  (unchanged); in the **panel / DyNES** case it is the **wave-endpoint average**
  `(|R_g(w_{k-1})| + |R_g(w_k)|)/2` over the two observed wave states — likewise the count
  is the observed count for `simulate()` and the deterministic net **Hamming diff** for a
  panel flavor (a net-change floor). This change consumes `(count_w, T_w, |R_w|)` and pins.
- **Per-actor constant hazard with uniform support-legal-sender semantics**:
  `exp(intercept_w)` is a **per-actor constant hazard** — identical for every
  support-legal actor — **not** an aggregate flavor scalar, so on the shared clock it
  slots into the competing-risks superposition `Σ_i exp(·)` alongside any competing
  flavor's per-actor rates (commensurability). The placement semantics follow **as a
  consequence**: equal per-actor hazards make the competing-risks minimum select the
  sender **uniformly among the support-legal actors** (constraints inherited; self-loops
  disallowed) — while the **actual draw and the empty/saturated-support guard are
  performed by the consuming routine**. In the relational case, because `|R_w|` is the
  period's time-weighted average risk set, the aggregate `|R(t)| · exp(intercept_w)`
  reproduces `count_w` over the period regardless of how the risk set changes; in the
  panel case the wave-endpoint average reproduces it against the two observed wave states.
- **A zero-free-parameters contract**: the intercept is **θ-independent**, so its timing
  likelihood is an **additive constant** w.r.t. the estimated parameters and is
  **excluded** from the optimizer's score/Hessian; it never enters the θ layout. `intercept_w`
  is computed **once and frozen** (the count is the deterministic Hamming diff for now),
  read unchanged across every EM / MCMC / simulation iteration. The fixed contribution
  MAY be reported as a constant offset in a total log-likelihood.
- **User surface (pinned in the generative context)**: an **intercept-only rate**
  (`rate = ~ 1`, no other effects) is understood as **pinned** under `estimate_dynes()`
  / `simulate()` / D9 completion — a user-written `~ 1` and a completion-supplied rate
  yield the same pinned object. A rate carrying **any** effect keeps its estimated
  baseline. The **single-process path is unchanged**: `estimate_dynam()` /
  `estimate_rem()` retain the existing estimated-intercept meaning (frozen baselines
  gate it). Each consumer **warns** at entry, worded for its source — `estimate_dynes()`
  states the pin is from the Hamming diff with no standard error and excluded from
  estimation; `simulate()` states the pin is from the observed count (no SE concept).
- **Timed-regime scope only**: the primitive is meaningful only in a timed composition
  (a shared continuous clock). In the **ordered** regime a missing rate is out of scope
  here — `process-simulation`'s pseudo-time modes handle that instead. A `simulate()`
  mixed process (one flavor rate-modeled, one missing) pins the missing flavor's
  constant this way; a **fully** rate-less `simulate()` draws its budget from the
  user's event-count / time-window request (`process-simulation`, not this change).

## Capabilities

### New Capabilities

- `intercept-only-rate`: the rate sub-model representation of a constant baseline
  hazard with a pinned, non-free intercept; the per-period pin
  `intercept_w = log(count_w / (T_w · |R_w|))` from consumer-supplied counts, exposures,
  and average risk-set size (one plateau per inter-wave period); the per-actor constant
  hazard plus uniform-support-legal-sender *semantics* (self-loops disallowed,
  constraints inherited — the draw itself owned by the consuming routine);
  the zero-free-parameters contract (θ-independent, frozen, excluded from the
  score/Hessian, optionally a reported constant offset); and the user surface
  (intercept-only ⟺ pinned in the generative context, single-process path unchanged,
  a context-aware warning per consumer).

### Modified Capabilities

<!-- None. D9's completion transform in make-multivariate-spec's
     multivariate-specification capability consumes this primitive, but its
     requirements already reference it by name (make-multivariate-spec owns that
     delta). This change adds no new requirement to an existing spec. -->

## Impact

- **Sequencing / dependents (hard)**: `make-multivariate-spec` — its D9 timed-regime
  completion (`complete_generative_spec()`, tasks 1c.2/1c.3) consumes this primitive to
  fill a missing timed rate; this change MUST land before that timed branch. Also
  consumed by `process-simulation` (mixed rate-modeled/missing processes) and
  `dynes-augmentation` (the augmenters and pool evaluator place events of a
  pinned-rate flavor on the shared clock).
- **R**: a new source file for the pinned-rate representation and its per-period pin
  `(count_w, T_w, |R_w|) → intercept_w` (counts/periods received, not derived); the intercept-only ⟺
  pinned reinterpretation in the generative context plus each consumer's context-aware
  warning. The rate sub-model surface (constant hazard, pinned intercept) is usable by
  the completion transform and by the walk handle's evaluator, reusing the existing
  timed-rate hazard path (`exp(intercept)` with no covariate columns) rather than adding
  a new evaluator. The sender draw and empty-support guard live in the consuming
  simulation/augmentation routines, not here.
- **Estimation θ / optimizer**: the pinned intercept carries **zero** free parameters
  into any joint fit — it never enters the fid / θ layout, score, or Hessian; its fixed
  contribution MAY surface only as a reported constant offset.
- **Frozen baselines**: untouched — this is additive and does not run on the
  single-process / flavored estimation path that the 1e-6 baselines gate. A
  `NOT_CRAN=true` run MUST report the frozen baselines as PASS, not SKIP.
- **Conventions**: snake_case, American English, `cli` for any user-facing output, a
  lifecycle **experimental** badge on any exported surface, roxygen +
  `devtools::document()`.

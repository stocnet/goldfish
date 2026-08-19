# Proposal: two-sided-coordination

## Why

goldfish implements exactly one mechanism by which an undirected coordination
tie arises from actor decisions — the conjunctive (mutual-choice) model,
reachable as `estimate_dynam(sub_model = "choice_coordination")`. Snijders &
Pickup (2017) catalogue five two-sided mechanisms; translated to relational
events (`.plan/sp/undirected_coordination_marked_ph.md`) all five are marked
point processes with proportional hazards, estimable by the Cox partial
likelihood over marks. The additional four (forcing, initiative+confirmation,
disjunctive, compensatory) do not fit the `sub_model` rate/choice split: with
actor-varying rates the parameters (β, θ, α) must be estimated **jointly** —
the fit is neither "a rate sub-model" nor "a choice sub-model" — so the model
gets its own estimator instead of more `sub_model` tokens.

## What Changes

- New estimator `estimate_dynamu()` (u = undirected two-sided DyNAM): the
  container of the three model parts — positional **choice** formula, optional
  named `rate =` and `acceptance =` formulas — plus `mechanism =
  c("conjunctive", "forcing", "confirmation", "disjunctive", "compensatory")`.
  No `sub_model` argument; the default (`mechanism = "conjunctive"`, no rate
  formula) reproduces today's `choice_coordination` fit exactly.
- Timing is **Cox-only**: partial likelihood over marks, which estimates all
  of (β, θ, α); there is deliberately **no** `distribution` argument (the
  thinning of unobserved rejected proposals under a parametric clock is an
  unresolved theoretical question, recorded in ADR-0023).
- `make_specification()` gains `model = "DyNAMu"` with `mechanism =` and
  `acceptance =` arguments (cli abort outside DyNAMu) — one constructor, one
  more model; flavored coordination (`choice = list(creation ~ ...,
  dissolution ~ ...)`) inherits the flavored machinery, and
  `make_joint_specification()` composes it with no new acceptance path. Any
  mechanism composes; generative consumers gate to conjunctive initially.
  `estimate_dynamu()` accepts the spec, keeps simple formulas on its direct
  surface, and `estimate_dynam()` redirects DyNAMu specs to it (ADR-0029,
  superseding ADR-0024's dedicated constructor).
- Full joint estimation for all five mechanisms when `rate =` is supplied
  (additive rate factor for conjunctive/disjunctive/compensatory; mixture
  weights for forcing/confirmation); constant rates otherwise. Two-stage
  estimation is not offered (inconsistent).
- Damped Newton–Raphson with the generic score/Hessian assembly
  (∂g_obs − E_D[∂g]; mixture rule), outer-product information fallback, and
  identifiability/divergence monitoring.
- **BREAKING (deprecation)**: `estimate_dynam(sub_model =
  "choice_coordination")` (released at v1.7.0) keeps working through a
  lifecycle `deprecate_warn()` redirect to `estimate_dynamu()`; removal
  ≥3.0.0. `make_specification()`'s `"choice_coordination"` token (dev-line
  only) is deleted outright — coordination models live on `estimate_dynamu()`,
  not the specification object.
- r-backend reference implementation plus cpp port, parity-gated; conjunctive
  under constant rates must reproduce current coefficients to 1e-6.

## Capabilities

### New Capabilities
- `dynamu-estimator`: the `estimate_dynamu()` surface — signature, defaults,
  mechanism/acceptance validation, Cox-only timing, backward-compatible
  conjunctive default, result-object blocks and methods.
- `coordination-mechanisms`: the five mechanisms' relative risks and mark
  likelihoods, the joint-estimation regime, predictability (t⁻) discipline,
  estimation safeguards, and backend parity.

### Modified Capabilities
- `model-recipe-dispatch`: dynamu recipes join the S3 dispatch; the
  `choice_coordination` recipe is reached only via the deprecation redirect.
- `model-specification`: `choice_sub_model` loses `"choice_coordination"`;
  coordination models are out of `make_specification()`'s scope.
- `naming-deprecations`: the `sub_model = "choice_coordination"` value
  redirect joins the deprecation surface (single-hop message, NEWS horizon).
- `multivariate-specification`: coordination processes join
  `make_joint_specification()` as `make_specification(model = "DyNAMu")`
  objects (any mechanism; generative consumers gate to conjunctive
  initially), and the completion default is restated as the uniform
  conjunctive draw (design D14–D16, ADR-0029/0030).

## Impact

- **R**: new `R/estimate_dynamu.R` (surface + assembly), `R/model_estimate.R`
  (redirect), `R/make_specification.R` (token removal), preprocessing reuse of
  the existing undirected/coordination risk-set machinery, summary/print for
  the three parameter blocks (cli).
- **C++** (`src/`): mechanism-specific g-derivative assembly in the
  coordination engine (`cpp-recompile` after every edit; frozen baselines must
  stay PASS — the conjunctive path's numbers are the bridge).
- **Tests**: simulation fixtures for the five mechanisms (extending
  `.plan/datasets/Simulation.R`'s approach), recovery tests, parity tests,
  deprecation-path tests.
- **Sequencing**: after `parametric-rates` (which settles the distribution
  vocabulary this change deliberately does not adopt); both pre-2.0.0.
- **Merged DyNES substrate (1.9.29)**: resolved (design D14–D16, ADR-0029) —
  coordination is a model of the one constructor
  (`make_specification(model = "DyNAMu")`); `estimate_dynamu()` fits it and
  `make_joint_specification()` composes it with no new acceptance path, while
  the estimator remains the only fitting surface. Composition accepts any
  mechanism;
  the walk/`simulate()`/`estimate_dynes()` consumers gate to conjunctive
  until the other mechanisms' generative constructions land. The internal
  `choice_coordination` consumers (`complete_generative_spec()`,
  `walk_handle`, `make_joint_specification()`) re-key to the dynamu
  family + mechanism vocabulary. Also coordinates with `process-simulation`
  (conjunctive-only coordination DGP → per-mechanism),
  `coordination-tie-consistency`, `review-rem-directed` (symmetric mask),
  `gather-rem-coordination-format`, and `effect-term-registry`.

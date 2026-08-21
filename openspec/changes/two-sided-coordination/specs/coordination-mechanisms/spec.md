# coordination-mechanisms Delta Specification

## ADDED Requirements

### Requirement: Five mechanisms define the dyadic relative risk
The estimator SHALL implement the five Snijders–Pickup mechanisms as dyadic
relative risks φ_kl in a common-baseline marked point process, with the mark
probability P_kl = φ_kl / Σ_{a<b} φ_ab: conjunctive (τ̃_k + τ̃_l)·p_kl·p_lk;
forcing τ̃_k·p_kl + τ̃_l·p_lk; confirmation τ̃_k·p_kl·π_lk + τ̃_l·p_lk·π_kl;
disjunctive (τ̃_k + τ̃_l)·(p_kl + p_lk − p_kl·p_lk); compensatory
(τ̃_k + τ̃_l)·exp(β′(s_kl + s_lk)). Under constant rates the rate factor
SHALL cancel from the mark probability, recovering the published special cases.

#### Scenario: constant-rate special cases
- **WHEN** each mechanism is fitted with no rate formula on data simulated
  from its own constant-rate mark probability
- **THEN** the estimated choice (and acceptance) parameters recover the
  simulation truth within tolerance, and the conjunctive case equals the
  published choice-coordination likelihood

#### Scenario: mechanisms are distinguishable by fit
- **WHEN** the five mechanisms are fitted to the same simulated event stream
- **THEN** log-likelihoods are comparable (equal parameter count for the
  non-acceptance variants) and the generating mechanism attains the best fit
  on average over replications

### Requirement: Joint estimation when rates vary by actor
The estimator SHALL maximize the mark (partial) likelihood jointly over all
parameter blocks when a rate formula is supplied: θ enters additively through
log(τ̃_k + τ̃_l) for conjunctive, disjunctive, and compensatory, and as
mixture weights for forcing and confirmation. A two-stage (rate-then-choice)
route SHALL NOT be offered.

#### Scenario: joint recovery with additive rates
- **WHEN** data are simulated from a disjunctive model with actor-varying
  rates and mixed dyads, and the model is refitted with a rate formula
- **THEN** (θ, β) recover the truth within simulation tolerance from the mark
  likelihood alone

#### Scenario: mixture-weight rates are estimated jointly
- **WHEN** a forcing model with actor-varying rates is fitted
- **THEN** θ and β are updated in one joint optimization (mixture rule), not
  in alternating stages

### Requirement: Estimation is damped with an information fallback
The optimizer SHALL use damped Newton–Raphson on the generic assembly
(score = ∂η_obs − E_𝒟[∂η]; Hessian = ∂²η_obs − E_𝒟[∂²η] − Cov_𝒟(∂η)) with
step-halving, substituting the outer-product information when the observed
information loses negative definiteness, and SHALL report non-convergence
with the suspected cause (no mixed dyads; mixture identification; likelihood
ridge) rather than iterating indefinitely.

#### Scenario: indefinite Hessian does not stop estimation
- **WHEN** a mixture-variant fit passes through a region where the observed
  information is not negative definite
- **THEN** the update proceeds on the outer-product information and the fit
  converges or reports a diagnosed non-convergence

#### Scenario: unidentified rate block is reported
- **WHEN** a rate formula uses a covariate constant across all actors (no
  mixed dyads)
- **THEN** the fit reports the identification failure naming the rate block
  instead of returning a spurious estimate

### Requirement: Statistics evaluate at the left limit
Every statistic SHALL be computed from the state at t⁻ (before the event
being evaluated) — including choice probabilities, acceptance probabilities,
and mixture weights — with simultaneous events strictly ordered, so
intensities remain predictable and the score is a martingale at the truth.

#### Scenario: an event cannot feed its own evaluation
- **WHEN** an event updates a statistic that appears in its own mechanism's
  relative risk
- **THEN** the likelihood contribution of that event uses the pre-update
  value, verified by a test constructing consecutive events on the same dyad

### Requirement: Backends agree on every mechanism
The r backend SHALL be the reference implementation of the five mechanisms
and the cpp backend SHALL reproduce it: per-event likelihood contributions
and fitted parameters agree within the parity tolerances used by the existing
backend-parity tests, for each mechanism and each estimation regime.

#### Scenario: parity per mechanism
- **WHEN** the parity suite runs a fixture through both backends for each of
  the five mechanisms
- **THEN** per-event contributions and final coefficients agree within the
  established parity tolerance

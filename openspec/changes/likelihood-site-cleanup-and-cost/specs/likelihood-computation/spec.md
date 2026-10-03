## ADDED Requirements

### Requirement: Likelihood work buffers are allocated per evaluation and the risk set is maintained in place
Every compiled likelihood evaluation (one call of a compiled kernel, one
gather build) SHALL allocate its event-sized work buffers once, before its
event loop, and reuse them across events: the linear predictor, the weights
or exponentials, the probability vector when requested, the weighted
statistics copy the information matrix is formed from, and any reduced
statistics view. The risk-set mask SHALL be held as one buffer initialized
once from the availability's initial value with the structural self-tie rule
applied, and SHALL be updated in place by the same per-event walk that
updates the availability, so an event costs the cells that flipped. Masked
entries SHALL be zeroed by a pass over the mask buffer, never by building an
index vector of the masked cells per event. Nothing of size `n1`, `n2`,
`n1 x n2` or `n1 x n2 x p` SHALL be allocated per event in a compiled kernel
or a gather build, and no per-event index vector SHALL grow by
concatenation in any backend.

The r backend SHALL stay pure R, the readable reference the compiled
kernels are checked against. Its per-event state SHALL be written in place
through the shared in-place writers, never by subassignment of a binding a
caller also holds, and its per-event accumulators SHALL be preallocated at
their final size and written in place. Its statistics array SHALL NOT be
copied to apply an update, to zero the diagonal, or to reshape. A REM event
(exact-time or ordinal) on the r backend SHALL allocate exactly one
weighted statistics array, the `S * p` of the information's
`crossprod(S, S * p)`, and otherwise only vectors of the risk set's size,
at most `1 + 3/q` statistics arrays in all, where `q` is the statistics
array's column count (`p + 1` on the exact-time REM with its intercept, `p`
on the ordinal REM); it SHALL NOT copy, reshape or subset the statistics
array, SHALL apply an axis filter as a mask on the whole grid rather than
by reducing the array, and SHALL build the probability vector and the
risk-set membership only when a per-event output that reads them is
requested. The mask SHALL be maintained in place: an availability flip
(a point cell, or a sender or receiver of the outer encoding) SHALL write
the cells it reaches when it happens, and no event SHALL rebuild the
excluded cells, so nothing allocated per event grows with the number of
actors excluded. The evaluation's time is then that of the full grid
however many actors are absent, which is accepted: under heavy composition
a REM evaluation is slower than the reduced-grid one it replaced. In pure R
one statistics-sized allocation per REM event is the information itself
and cannot be avoided.

#### Scenario: a compiled kernel allocates nothing event-sized inside its loop
- **WHEN** a REM, REM-ordered, coordination, choice or rate kernel evaluates
  a sequence of `E` events at fixed parameters
- **THEN** its wall-clock on an optimized build on a fixture whose dyad grid
  dominates (1899 actors) falls against the pre-change kernel by the share
  the per-event allocations took, and a count of event-sized allocations in
  its loop is zero (the `allowed`, `lin_pred`, `weights`, `e`,
  `probabilities` and Fisher-copy buffers are declared before the loop).

#### Scenario: the risk set follows the availability's flips
- **WHEN** an event's availability slice flips `k` cells (or `k` factor
  entries at the outer encoding)
- **THEN** the mask buffer changes in exactly the cells those flips reach
  (a factor flip reaches its row or column), the self-tie cells stay
  excluded, and the masked exponential vector equals the one a from-scratch
  mask would give, so per-event log-likelihood, score and information agree
  with the pre-change kernel within 1e-10.

#### Scenario: an r-backend REM event allocates one weighted statistics array
- **WHEN** one r-backend evaluation runs on the 300-actor generated REM
  fixtures of the cost guard, exact-time (unconstrained, under a point
  constraint, or with 200 of the 300 actors absent through the outer
  encoding, `q = 3`) or ordinal (unconstrained or 200 absent, `q = 2`)
- **THEN** it allocates at most `1 + 3/q` statistics arrays per event, 2 on
  the exact-time and 2.5 on the ordinal fixtures, plus 0.1 for the
  evaluation's own fixed allocations and one-off flip writes spread over
  its events, which is the limit the guard asserts; no event copies,
  reshapes or subsets the statistics array or rebuilds its excluded cells,
  an outer-encoded filter included, so the allocation per event is the
  same with 10 or 200 of 300 actors absent; and a whole r fit of
  the Social Evolution REM allocates under about 2100 MB (3290 MB before,
  at `237c8dd`; 7325 MB at `7b56821`).

#### Scenario: the gather builder preallocates its candidate index
- **WHEN** the gather stack is built for a dyad family
- **THEN** each event's candidate index is written into a preallocated
  vector and the point presence grid is updated in place, with the stack
  byte-identical to the pre-change build.

### Requirement: Diagnostics are evaluated once at the converged parameters
During the Newton iterations the likelihood evaluation SHALL compute only
the quantities the optimizer reads: the log-likelihood, the score and the
information matrix (and the interval log-likelihoods when the step
acceptance reads them). Every requested diagnostic component (per-event
scores, total rate and its conditional component, ranks, margins,
availability, per-event probabilities, the event information trace) SHALL
be produced by one evaluation at the returned parameters after convergence,
and SHALL equal the component the per-iteration evaluation at those
parameters produces. The convergence check SHALL inspect the estimation
components only, never the diagnostic lists.

#### Scenario: diagnostics are computed once
- **WHEN** a fit converges in `k` iterations with the default diagnostics
- **THEN** the diagnostic components were computed in exactly one
  evaluation, every stored component equals the one obtained by evaluating
  the per-iteration path at the returned parameters, and the fit's
  wall-clock falls by the diagnostics' share of `k - 1` evaluations.

#### Scenario: a non-converged fit still carries its diagnostics
- **WHEN** a fit stops at the iteration limit or is interrupted by a
  non-finite step
- **THEN** the diagnostics are evaluated once at the parameters the fit
  returns, as for a converged fit, and the fit's convergence status is
  unchanged.

### Requirement: Exact-time kernels derive diagnostic normalizers from the likelihood pass
The exact-time C++ kernels (`estimate_REM`, `estimate_DyNAM_rate`) SHALL obtain
the per-event log-normalizer required by the conditional (Cox
partial-likelihood) component from the raw rate pass the likelihood has already
computed in the same iteration, rather than from a second `exp` pass over the
risk set. Where the raw normalizer is a normal positive double (finite,
strictly positive and not subnormal), the
log-normalizer SHALL be `log(normalizer)` and the per-event probability vector
SHALL be the raw weights divided by that normalizer. Where it is not, the kernel
SHALL fall back to the max-shifted log-sum-exp, so the overflow and
subnormal-underflow guarantee is preserved. The raw pass SHALL remain on the
absolute scale, because the compensator enters the likelihood as
`-Δt · normalizer`.

#### Scenario: the default path runs one exp pass per event
- **WHEN** an exact-time sub-model is fitted with the default
  `diagnostics = c("loglik", "scores")`
- **THEN** the kernel evaluates the risk-set exponential once per event, and the
  stored `total_rate` and conditional component are unchanged from a fit that
  evaluates it twice

#### Scenario: a degenerate normalizer falls back to the shifted pass
- **WHEN** the raw per-event normalizer is not finite, is not strictly
  positive, or is subnormal (where the raw ratio keeps only a few significant
  bits)
- **THEN** the kernel computes the log-normalizer by max-shifted log-sum-exp, and
  the conditional component is finite wherever the shifted pass makes it finite

#### Scenario: coefficients and the log-likelihood do not move
- **WHEN** any frozen coefficient baseline is re-run against the changed kernels
- **THEN** every coefficient, every standard error and the aggregate
  log-likelihood match the frozen values at the established 1e-6 floor, because
  none of them reads the conditional component

#### Scenario: exact-time backends agree under extreme predictors
- **WHEN** the same exact-time sub-model is evaluated at a parameter vector
  that makes the raw normalizer overflow, or underflow to zero, on the `r`
  and `cpp` backends
- **THEN** both return a finite conditional component through their
  fallback paths and agree within the cross-backend tolerance, and both
  return the same non-finite likelihood where the raw sum overflows, so the
  step rejection is identical

#### Scenario: partial underflow moves only the last bits of a diagnostic
- **WHEN** a few linear predictors lie far below a representable maximum at
  one event
- **THEN** the log-normalizer from the raw pass agrees with the shifted pass
  within 1e-10, the probability vector sums to one, and the difference in the
  smallest probabilities is bounded by the cross-backend tolerance and
  recorded with the change

### Requirement: The per-event probability vector is materialized only for the primitives that read it
Every C++ estimation kernel SHALL allocate and fill the per-event probability
vector only when at least one primitive that consumes it — `probabilities`,
`margins`, or `conditional_scores` — has been requested. Requesting `loglik`
alone SHALL NOT cause a risk-set-sized probability vector to be formed, on any
sub-model family.

#### Scenario: loglik alone forms no probability vector
- **WHEN** a fit requests `diagnostics = "loglik"` on any sub-model
- **THEN** no per-event probability vector is allocated, and the cost per event
  is independent of the risk-set size beyond the passes the likelihood itself
  requires

#### Scenario: the consuming primitives still receive it
- **WHEN** a fit requests any of `probabilities`, `margins` or
  `conditional_scores`
- **THEN** the probability vector is formed and the stored primitives are
  numerically identical to those the unguarded implementation produced

## MODIFIED Requirements

### Requirement: Timing evidence for the refactor goal
The estimation path SHALL carry a standing, recorded cost budget, not only a
one-off record of a past speedup. The budget SHALL be checked under
`NOT_CRAN=true` and SHALL report PASS, not SKIP, alongside the frozen
coefficient baselines, on a fixed set of cells on fixtures where the dyad grid
dominates the per-event cost: the memory allocated by one fit on the r and
gather backends, the median seconds of a warm fit per backend, the number of
Newton iterations, and the diagnostics ratio: one likelihood evaluation with
the requested diagnostics against one evaluation without them, timed in the
same session, so the ratio is self-normalizing across machines and SHALL never
be an absolute threshold. The budget SHALL fail above the recorded value times
a limit and, for the allocation cells, below the recorded value times the
mirror of that limit, so an improvement lowers the record in the same commit;
seconds SHALL be compared only where an environment variable says the record
was made on this machine. Raising a recorded value or the ratio's ceiling SHALL
require the maintainer's approval, and the commit SHALL record the new value
and the reason beside it, in the same way a re-frozen coefficient baseline
records its derivation. The compiled backend's allocation, invisible to R, is
covered by its time cells, the ratio and the kernel allocation count of the
per-evaluation allocation requirement.

#### Scenario: speedup is documented
- **WHEN** a change claims an estimation speedup
- **THEN** the change log contains BEFORE and AFTER timings measured with the
  same procedure, and `NEWS.md` notes the default-engine estimation speedup

#### Scenario: a per-evaluation diagnostics cost regression fails a test
- **WHEN** a change makes an evaluation with the default diagnostics materially
  more expensive than the same evaluation with the diagnostics off
- **THEN** the budget test fails, naming the measured ratio and the ceiling,
  and it does so on a machine slower or faster than the one that recorded the
  ceiling, because both evaluations are timed in the same session

#### Scenario: a regression in estimation allocation fails a test
- **WHEN** a change makes a fit on the r or gather backend allocate more
  than the recorded value times its limit on a budget cell
- **THEN** the budget test fails, naming the cell, the measured value and
  the ceiling.

#### Scenario: an improvement lowers the record
- **WHEN** a change makes a fit allocate less than the recorded value times
  the floor on a budget cell
- **THEN** the budget test fails until the recorded value is lowered in the
  same commit, so the gain cannot be given back unnoticed.

### Requirement: C++ multinomial normalizers use the shared stable softmax
Every compiled kernel SHALL obtain its per-event normalizer from one shared
max-shift log-sum-exp helper (plain C++/Armadillo, no R-level callbacks), so all
compiled paths have identical overflow behavior for the same model. The helper
SHALL be named for what it computes — it returns a log-sum-exp and the shifted
weights, never a softmax — and the scale on which each quantity uses it is
determined **per quantity, not per kernel**:

- Quantities that are ratios or logs of the normalizer — multinomial
  log-likelihood contributions, per-event probabilities, ranks, and the
  exact-time conditional log-probability — SHALL be computed from the
  log-sum-exp and the shifted weights, since the max-shift cancels exactly and
  keeps them finite where the linear scale does not, with one exception: the
  exact-time kernels (`estimate_REM`, `estimate_DyNAM_rate`) SHALL take the
  conditional log-probability's normalizer as `log(normalizer)` and the
  per-event probabilities as the raw rates divided by `normalizer`, from the
  raw pass the likelihood already computed, wherever that normalizer is a
  normal positive double (finite, strictly positive and not subnormal), and
  SHALL fall back to the shared log-sum-exp helper wherever it is not. On a
  normal normalizer the raw ratio is what the shifted pass computes, to the
  last bits; the fallback keeps the overflow and subnormal-underflow guarantee
  where it does not.
- The exact-time likelihood's total rate SHALL keep the raw linear scale: it
  enters the log-likelihood as `−Δt · T` rather than as a ratio, so shifting it
  would change the model rather than stabilize it. Overflow there is handled by
  the estimation loop, which rejects a step whose log-likelihood is not finite.

This covers the `cpp` backend's event-loop kernels and the `gather` backend's
compute kernels. Any FURTHER `src/` micro-optimization SHALL be justified by a
recorded profile showing a measurable gain on the baseline fixtures. Every
`src/` diff SHALL pass the cpp-reviewer before commit and keep the
cross-backend agreement tests passing at their existing tolerance.
If profiling shows no further gain, the finding SHALL be recorded and no
additional change made.

#### Scenario: backends agree under extreme predictors
- **WHEN** the same extreme-parameter choice model is evaluated on the `r`,
  `cpp` and `gather` backends
- **THEN** all three return finite, agreeing results through their
  log-sum-exp paths.

#### Scenario: gather adoption leaves well-conditioned fixtures unmoved
- **WHEN** the gather kernels adopt the max-shift log-sum-exp and the
  cross-backend agreement tests are run on the well-conditioned baseline
  fixtures
- **THEN** the results are unchanged within the existing tolerance, since the
  shift is a no-op where plain `exp()` does not overflow.

#### Scenario: the exact-time likelihood is unchanged by the adoption
- **WHEN** an exact-time sub-model is evaluated before and after its kernel
  takes its weights from the shared helper
- **THEN** the per-event log-likelihood, score and information are unchanged,
  because the total rate is recovered on the linear scale from the
  log-normalizer rather than being shifted.

#### Scenario: no further measurable C++ gain
- **WHEN** profiling the compiled likelihood loops beyond the stable-softmax
  adoption shows no hot spot with a measurable improvement
- **THEN** no additional `src/` change is made and the profile result is
  recorded in the change log.

#### Scenario: adopted C++ change is reviewed
- **WHEN** any `src/` change lands (the stable-softmax helper, the shared
  reduction helper, or a profiled micro-optimization)
- **THEN** the diff passed the cpp-reviewer and the cross-backend agreement
  tests pass.

#### Scenario: an exact-time diagnostic takes the raw normalizer when it is normal
- **WHEN** an exact-time kernel forms the conditional log-probability or the
  per-event probabilities at an event whose raw normalizer is a normal
  positive double
- **THEN** it uses `log(normalizer)` and the raw ratio without a second
  `exp` pass, agreeing with the shifted log-sum-exp within 1e-10, and at an
  event whose normalizer overflows, underflows to zero or is subnormal it
  takes the shared log-sum-exp helper instead.

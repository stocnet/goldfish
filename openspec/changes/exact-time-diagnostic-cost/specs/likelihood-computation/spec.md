## ADDED Requirements

### Requirement: Exact-time kernels derive diagnostic normalizers from the likelihood pass
The exact-time C++ kernels (`estimate_REM`, `estimate_DyNAM_rate`) SHALL obtain
the per-event log-normalizer required by the conditional (Cox
partial-likelihood) component from the raw rate pass the likelihood has already
computed in the same iteration, rather than from a second `exp` pass over the
risk set. Where the raw normalizer is finite and strictly positive, the
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

- **WHEN** the raw per-event normalizer is not finite, or is not strictly
  positive
- **THEN** the kernel computes the log-normalizer by max-shifted log-sum-exp, and
  the conditional component is finite wherever the shifted pass makes it finite

#### Scenario: coefficients and the log-likelihood do not move

- **WHEN** any frozen coefficient baseline is re-run against the changed kernels
- **THEN** every coefficient, every standard error and the aggregate
  log-likelihood match the frozen values at the established 1e-6 floor, because
  none of them reads the conditional component

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
The estimation path SHALL carry a standing per-event cost gate, not only a
one-off record of a past speedup. The gate SHALL measure a fixed small fixture
fitted with the default `diagnostics` against the same fixture fitted with the
diagnostic block disabled, and SHALL fail when that ratio exceeds a recorded
ceiling. Because absolute wall-clock is machine-dependent, the gate SHALL be
expressed as a ratio between two fits in the same session, never as an absolute
threshold. A change that alters the ceiling SHALL record the new value and the
reason beside it, in the same way a re-frozen coefficient baseline records its
derivation. The gate SHALL run in the `NOT_CRAN=true` suite and report PASS, not
SKIP.

#### Scenario: a per-event cost regression fails a test

- **WHEN** a change makes the default diagnostics path materially more expensive
  per event than the same fit with the diagnostic block disabled
- **THEN** the cost gate fails, naming the measured ratio and the ceiling

#### Scenario: the gate is self-normalizing across machines

- **WHEN** the gate runs on a machine slower or faster than the one that recorded
  the ceiling
- **THEN** it still passes, because both fits it compares are timed in the same
  session on the same machine

#### Scenario: speedup is documented

- **WHEN** a change claims an estimation speedup
- **THEN** the change log contains BEFORE and AFTER timings measured with the
  same procedure, and `NEWS.md` notes the default-engine estimation speedup

# likelihood-computation (delta)

## MODIFIED Requirements

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
  keeps them finite where the linear scale does not.
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

# likelihood-computation (delta)

## MODIFIED Requirements

### Requirement: C++ multinomial normalizers use the shared stable softmax
Every C++ multinomial normalizer loop SHALL adopt the same max-shift stable
softmax via a shared C++ helper (plain C++/Armadillo, no R-level callbacks), so
all compiled paths have identical overflow behavior for the same model. This
covers the `cpp` backend's event-loop kernels **and the `gather` backend's
multinomial and Poisson compute kernels**, whose weights feed the shared
per-event reductions; leaving one backend on plain `exp()` would make the
per-event primitives agree only within that backend's overflow behavior. The
timed hazard path keeps plain `exp()` in every backend — its scale is absolute
(Non-Goal). Any FURTHER `src/` micro-optimization SHALL be justified by a
recorded profile showing a measurable gain on the baseline fixtures. Every
`src/` diff SHALL pass the cpp-reviewer before commit and keep the
cross-backend agreement tests passing at their existing tolerance.
If profiling shows no further gain, the finding SHALL be recorded and no
additional change made.

#### Scenario: backends agree under extreme predictors
- **WHEN** the same extreme-parameter choice model is evaluated on the `r`,
  `cpp` and `gather` backends
- **THEN** all three return finite, agreeing results through their
  stable-softmax paths.

#### Scenario: gather adoption leaves well-conditioned fixtures unmoved
- **WHEN** the gather kernels adopt the max-shift softmax and the
  cross-backend agreement tests are run on the well-conditioned baseline
  fixtures
- **THEN** the results are unchanged within the existing tolerance, since the
  shift is a no-op where plain `exp()` does not overflow.

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

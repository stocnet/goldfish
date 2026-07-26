# likelihood-computation Specification

## Purpose
TBD - created by archiving change refactor-likelihood-compute. Update Purpose after archive.
## Requirements
### Requirement: Numerical equivalence floor for the refactor
The refactor SHALL satisfy two floors: (a) each refactored helper agrees with
the pre-refactor implementation on per-event outputs (logLikelihood, score,
informationMatrix, pMatrix) within 1e-10 on deterministic small fixtures
covering all six sub-models and both baseline datasets, verified by tests that
run both implementations BEFORE the old code is deleted; and (b) the frozen
coefficient baselines and C++ golden baselines PASS (not SKIP) at their
existing 1e-6 tolerance under `NOT_CRAN=true`. Byte-identity is explicitly NOT
required (BLAS reorders floating-point summation).

#### Scenario: per-helper equivalence before deletion
- **WHEN** a contribution helper is refactored
- **THEN** an old-vs-new test asserts ≤ 1e-10 disagreement on every per-event
  output for the fixture models, and only after it passes is the original
  implementation removed.

#### Scenario: frozen baselines remain the end-to-end floor
- **WHEN** the full test suite runs with `NOT_CRAN=true` after the refactor
- **THEN** the coefficient baselines report PASS (not SKIP) at 1e-6 with no
  baseline file regenerated.

### Requirement: In-house single-pass stable softmax for multinomial contributions
The system SHALL compute the multinomial likelihood contributions
(DyNAM-choice, DyNAM-choice-coordination, REM-ordered, DyNAM-rate-ordered) —
their probabilities and log-normalizer — through an in-house max-shift stable
softmax in ONE exponential pass: `m = max(x)`, `e = exp(x − m)`, yielding both
the probabilities `e / Σe` and the log-normalizer `m + log(Σe)` from the same
`e` — never a standalone log-sum-exp evaluated next to a separate `exp()`.
The observed alternative's log-likelihood SHALL be computed from the shifted
linear predictor (`(x_sel − m) − log(Σe)`), not as `log()` of a stored
probability. The timed DyNAM-rate/REM hazard contribution SHALL NOT be
max-shifted (its exponential enters the likelihood absolutely as
`−timespan · Σ exp(xᵢ)`). No external package SHALL be added for this
functionality (`matrixStats` closed by inspection: no `LinkingTo` headers, no
`R_RegisterCCallable` surface).

#### Scenario: one exponential pass serves both outputs
- **WHEN** a multinomial contribution is evaluated
- **THEN** exactly one elementwise exponential over the alternatives produces
  both the probability vector and the log-normalizer (no duplicate exp pass).

#### Scenario: extreme linear predictors stay finite
- **WHEN** parameters push the maximum linear predictor beyond the double
  `exp()` overflow threshold in a choice/coordination/ordered model
- **THEN** the log-likelihood, score, and information matrix are finite and
  match a high-precision reference (no `Inf`/`NaN`).

#### Scenario: underflowing observed event keeps a finite logL
- **WHEN** the observed alternative's probability underflows to zero
- **THEN** its log-likelihood is computed from the shifted predictor and is
  finite (not `log(0) = -Inf`).

#### Scenario: timed hazard path is not shifted
- **WHEN** the timed DyNAM-rate/REM contribution is evaluated
- **THEN** the rates are plain `exp(x)` (absolute hazard scale preserved) and
  agree with the pre-change implementation within 1e-10 on benign fixtures.

#### Scenario: no new dependency
- **WHEN** the refactor is complete
- **THEN** `DESCRIPTION` gains no new Imports entry.

### Requirement: C++ REM and coordination estimators use staged BLAS form
The `default_c` REM estimators SHALL compute each event's normalizer, weighted
statistics sum, and Fisher contribution through staged vectorized operations —
one linear-predictor product, one masked exponential vector (presence,
reflexive exclusion, and risk-set restrictions entering as zeros), one
weighted-sum product, and one weighted cross-product for the Fisher matrix —
with no per-dyad scalar dot products or per-dyad rank-1 outer-product
accumulation. The coordination estimators SHALL use a dyad-triangle
representation: a length-d weight vector (d = number of distinct dyads) built
in one triangle pass and normalized in log space via the stable softmax, a
compact d×p deviation buffer allocated once and reused across events (no
per-event copy of the full statistics matrix, no full n×n pairwise-probability
matrix), and the Fisher matrix as one weighted cross-product over that buffer.

#### Scenario: REM event uses the staged pipeline
- **WHEN** `estimate_REM` (or the ordered variant) evaluates an event
- **THEN** the linear predictor, masked exponentials, weighted sum, and Fisher
  contribution come from staged matrix/vector operations over the statistics
  matrix, and excluded dyads (absent, reflexive, restricted) contribute exact
  zeros — reproducing the pre-change results within 1e-10 on benign fixtures.

#### Scenario: coordination event allocates no n²-sized temporaries
- **WHEN** the coordination estimator evaluates an event
- **THEN** no full statistics-matrix copy and no full pairwise-probability
  matrix is allocated for the event; the dyad weights live in a length-d
  vector, the deviations in the reused d×p buffer, and the event logL is
  computed in log space (finite even when the observed dyad's probability
  underflows).

#### Scenario: rewritten estimators agree with the default engine
- **WHEN** the same model runs on the default R engine and the rewritten
  `default_c`/gather estimators over the baseline fixtures
- **THEN** per-event logL/score/information agree within 1e-10 and the
  cross-engine coefficient tests pass unchanged.

### Requirement: Coordination gather path is index-based and ragged-safe
The coordination gather layout SHALL identify every candidate row by explicit
sanitized indices — per-row `index_i`/`index_j`, per-sender offsets grouping
each sender's allowed receivers, and dyad-pairing indices matching each (i,j)
row to its (j,i) partner — and the coordination compute kernel SHALL consume
only these index structures, with no assumption that the candidate set forms
a rectangular grid (no square reshape of the per-event rows, no
n_candidates = n1 x n2 invariant, no diagonal zeroing of a full matrix). An
unconstrained one-mode model SHALL emit the full off-diagonal dyad list (no
self-tie rows); a model with a folded symmetric `support_constraint` SHALL
emit only the mask-allowed dyads, both directions of every kept dyad present.
A constrained coordination model SHALL run natively on
`backend = "gather"`: the redirect to the `cpp` backend and its
accompanying message SHALL be removed.

#### Scenario: constrained coordination runs natively on the gather backend
- **WHEN** a coordination model with a `support_constraint` is estimated with
  `backend = "gather"`
- **THEN** no redirect message is emitted, estimation runs on the gather
  backend, and coefficients agree with the `cpp` and `r` backends within the
  cross-backend tolerance.

#### Scenario: ragged candidate set evaluates correctly
- **WHEN** the folded mask leaves senders with differently sized
  allowed-receiver sets at an event
- **THEN** the kernel evaluates that event from the index structures alone,
  and per-event logL/score/information agree with the `r` backend within
  1e-10.

#### Scenario: one-mode coordination ships no diagonal rows
- **WHEN** an unconstrained one-mode coordination model is gathered
- **THEN** no self-tie row is emitted, the kernel performs no diagonal
  zeroing, and estimation results agree with the pre-change backends within
  the cross-backend tolerance.

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
### Requirement: Timing evidence for the refactor goal
The change SHALL record before/after wall-clock timings of the frozen-baseline
suite (and of the isolated REM / choice-coordination default cells) in the
change log, demonstrating the bottleneck reduction that motivated the refactor.

#### Scenario: speedup is documented
- **WHEN** the refactor is complete
- **THEN** the change log contains BEFORE and AFTER timings measured with the
  same procedure, and `NEWS.md` notes the default-engine estimation speedup.

### Requirement: The r backend computes contributions via vectorized linear algebra
The `r` backend SHALL compute every per-event likelihood contribution
(log-likelihood, score, information matrix, probability matrix) for all six
sub-models (DyNAM-rate, DyNAM-rate-ordered, DyNAM-choice,
DyNAM-choice-coordination, REM, REM-ordered) using vectorized linear-algebra
operations (`%*%`, `crossprod`, `tcrossprod`, elementwise vector arithmetic)
with NO per-cell or per-row R-closure reductions (`apply` with an anonymous
function over event-sized dimensions) and NO copying reshape where a `dim<-`
metadata change suffices. Risk-set zeroing semantics (reflexive-edge exclusion,
mask/opportunity zeroed rates) SHALL be preserved exactly: excluded entries
enter the reductions with rate/probability zero.

#### Scenario: information matrices are weighted cross-products
- **WHEN** any `r` backend contribution computes an information matrix
- **THEN** it is computed as a weighted cross-product of the (flattened)
  derivative/statistics matrix (one BLAS call), with no `expand.grid`-driven
  per-parameter-pair closure loop and no n²×p² intermediate array.

#### Scenario: softmax utilities via matrix product
- **WHEN** `getMultinomialProbabilities` evaluates the linear predictor of a
  2- or 3-dimensional statistics array
- **THEN** it computes it as a reshape (`dim<-`) plus one matrix product with
  the parameter vector — not `apply(…, c(1, 2), sum)` nor
  `rowSums(t(t(...) * parameters))`.

#### Scenario: excluded dyads still contribute zero
- **WHEN** a REM event is evaluated with reflexive edges disallowed and/or a
  risk-set mask
- **THEN** the excluded entries contribute exactly zero to the rate sum, the
  score, and the information matrix, identically to the pre-refactor code.

### Requirement: The stored per-event score SHALL come from one shared implementation
Every backend SHALL produce its stored per-event score row through the shared
reduction rather than through a private copy of the arithmetic, so a change to
the definition is made once per language rather than once per kernel. The
shared reduction exists as a compiled implementation and an R implementation,
pinned against each other by a direct test on constructed inputs, because the
compiled backends and the R reference backend cannot share one binary. The
estimator's own accumulation of the score SHALL be unchanged, and no
coefficient may move: this requirement governs where the stored diagnostic is
computed, not the quantity the optimizer follows.

Consolidating the event-loop engines onto the shared reduction replaces each
kernel's own arithmetic — in most of them a before/after difference of the
running derivative, in one an open-coded direct evaluation — with the same
quantity computed once, which shifts stored values by a small amount. That
shift is an accepted consequence of having one implementation, **not** a
precision claim: the difference form's conditioning penalty was measured across
the model families at 1.2e-14 to 9.2e-13 relative, one hundred to eight
thousand times tighter than the cross-backend tolerance, so it is real in
mechanism and negligible in size.

Consolidation SHALL NOT be expected to tighten cross-backend agreement.
Agreement between a compiled backend and the R reference is limited by their
being two implementations over different linear-algebra paths, not by which
form each uses internally, so it stays at its floating-point floor either way.

#### Scenario: stored scores agree across backends
- **WHEN** the same model is estimated on each supported backend at one fixed
  parameter vector with per-event scores requested
- **THEN** the stored score matrices agree to 1e-10

#### Scenario: the aggregate identity holds against an independent total
- **WHEN** the column sums of the stored per-event score matrix are compared to
  the fit's final score vector
- **THEN** they agree to 1e-10, the rows having been computed independently of
  the accumulated total rather than derived from it

#### Scenario: coefficients are unaffected
- **WHEN** a model whose coefficients are covered by the frozen baselines is
  estimated with and without per-event scores requested
- **THEN** the coefficients match the frozen baseline in both cases


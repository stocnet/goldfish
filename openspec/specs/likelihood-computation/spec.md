# likelihood-computation Specification

## Purpose
TBD - created by archiving change refactor-likelihood-compute. Update Purpose after archive.
## Requirements
### Requirement: Default-engine contributions computed via vectorized linear algebra
The default R engine SHALL compute every per-event likelihood contribution
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
- **WHEN** any default-engine contribution computes an information matrix
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
`engine = "gather_compute"`: the redirect to `default_c` and its
accompanying message SHALL be removed.

#### Scenario: constrained coordination runs natively on gather_compute
- **WHEN** a coordination model with a `support_constraint` is estimated with
  `engine = "gather_compute"`
- **THEN** no redirect message is emitted, estimation runs on the gather
  engine, and coefficients agree with `default_c` and `default` within the
  cross-engine tolerance.

#### Scenario: ragged candidate set evaluates correctly
- **WHEN** the folded mask leaves senders with differently sized
  allowed-receiver sets at an event
- **THEN** the kernel evaluates that event from the index structures alone,
  and per-event logL/score/information agree with the `default` engine within
  1e-10.

#### Scenario: one-mode coordination ships no diagonal rows
- **WHEN** an unconstrained one-mode coordination model is gathered
- **THEN** no self-tie row is emitted, the kernel performs no diagonal
  zeroing, and estimation results agree with the pre-change engines within
  the cross-engine tolerance.

### Requirement: C++ multinomial normalizers use the shared stable softmax
The `default_c` multinomial normalizer loops SHALL adopt the same max-shift
stable softmax via a shared C++ helper (plain C++/Armadillo, no R-level
callbacks), so both engines have identical overflow behavior for the same
model. Any FURTHER `src/` micro-optimization SHALL be justified by a recorded
profile showing a measurable gain on the baseline fixtures. Every `src/` diff
SHALL pass the cpp-reviewer before commit and keep the R-vs-C++ cross-engine
agreement tests passing at their existing tolerance. If profiling shows no
further gain, the finding SHALL be recorded and no additional change made.

#### Scenario: engines agree under extreme predictors
- **WHEN** the same extreme-parameter choice model is evaluated on the default
  R engine and `default_c`
- **THEN** both return finite, agreeing results through their stable-softmax
  paths.

#### Scenario: no further measurable C++ gain
- **WHEN** profiling the `default_c` likelihood loops beyond the stable-softmax
  adoption shows no hot spot with a measurable improvement
- **THEN** no additional `src/` change is made and the profile result is
  recorded in the change log.

#### Scenario: adopted C++ change is reviewed
- **WHEN** any `src/` change lands (the stable-softmax helper or a profiled
  micro-optimization)
- **THEN** the diff passed the cpp-reviewer and the cross-engine agreement
  tests pass.

### Requirement: Timing evidence for the refactor goal
The change SHALL record before/after wall-clock timings of the frozen-baseline
suite (and of the isolated REM / choice-coordination default cells) in the
change log, demonstrating the bottleneck reduction that motivated the refactor.

#### Scenario: speedup is documented
- **WHEN** the refactor is complete
- **THEN** the change log contains BEFORE and AFTER timings measured with the
  same procedure, and `NEWS.md` notes the default-engine estimation speedup.


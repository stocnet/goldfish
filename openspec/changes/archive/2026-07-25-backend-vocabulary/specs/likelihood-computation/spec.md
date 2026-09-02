# likelihood-computation (delta)

Wording-only, and deliberately partial. Two requirements here describe the C++
implementation itself — "C++ REM and coordination estimators use staged BLAS
form" and "C++ multinomial normalizers use the shared stable softmax" — whose
`default_c` references name the implementation, not a user choice. Per design
D2 those keep their names; only what a user passes or selects is renamed.

## RENAMED Requirements

- FROM: `### Requirement: Default-engine contributions computed via vectorized linear algebra`
- TO: `### Requirement: The r backend computes contributions via vectorized linear algebra`

## MODIFIED Requirements

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

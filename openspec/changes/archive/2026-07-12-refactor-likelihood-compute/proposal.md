## Why

The frozen-coefficient baseline suite (`NOT_CRAN=true`, 14 models × 2 engines) is
dominated by the **default R engine's REM and DyNAM-choice-coordination cells**,
whose per-event likelihood contributions are computed with per-cell `apply()`
closures and avoidable transpose/copy round-trips instead of the equivalent BLAS
linear algebra — e.g. `colSums(t(apply(statsArray, 1, \(x) outer(x, x))) * rates)`
(an n²×p² intermediate built by n² R calls, per event, per Newton iteration) where
one `crossprod()` call suffices, and `exp(apply(weightedStatsArray, c(1,2), sum))`
(n² R `sum()` calls) where a reshape + matrix product suffices. Refactoring these
routines cuts the wall-clock of every `devtools::test()` run and of default-engine
estimation generally, with no behavioral change beyond floating-point
summation order.

The same rewrite is the natural moment to exploit what the C++ layer already
is: the `estimate_*` functions are pure evaluators `(β, preprocessed state) →
{logL, score, Fisher}` with the Newton-Raphson loop living in R. Two additive
features fall out at low cost *if factored during the rewrite* (and would each
cost a third pass over this code later): (a) alternative optimizers via
**maxLik** — robustness when damped NR struggles (flat likelihoods, poor
starts) — since maxLik consumes exactly the evaluator contract; and (b)
**internal process-state evaluators** (choice probabilities, actor/dyad rates)
that a future `simulate()` and DyNES data augmentation will build on as
recipe-style preprocessing variants.

## What Changes

- Refactor **all default-engine event-contribution helpers** in
  `R/estimation_core.R` — `event_contribution_rate` (DyNAM-rate/REM),
  `compute_event_contribution.dynam_rate_ordered_spec`, the choice /
  choice-coordination / REM-ordered contributions, and their derivative and
  information-matrix helpers (`getMultinomialProbabilities`,
  `compute_first_derivative_*`, `getMultinomialInformationMatrix{,M}`,
  `getInformationMatrixREM`, `getLikelihoodMM`) — to vectorized linear algebra:
  `%*%` / `crossprod()` / `tcrossprod()` replacing `apply()`-closure reductions,
  `dim<-` replacing copying flattens (`apply(statsArray, 3, c)`), and direct
  matrix products replacing double-transpose idioms
  (`rowSums(t(t(statsMatrix) * parameters))`).
- **No `matrixStats` Import — decision closed by inspection** of the installed
  package: no `inst/include` headers (no `LinkingTo:`), no `R_RegisterCCallable`
  surface (only its own `.Call` wrappers are registered), so `src/` cannot use
  it legitimately; and on the R side a standalone `logSumExp()` duplicates the
  `exp()` pass this likelihood needs for probabilities/score/information anyway.
- **In-house single-pass stable softmax (R + C++)** for the four multinomial
  contributions (choice, choice-coordination, REM-ordered, rate-ordered): one
  exp pass (`exp(x − max(x))`) yields both the probabilities and the
  log-normalizer; the observed event's logL is computed from the shifted
  predictor (finite even when its probability underflows). The timed
  DyNAM-rate/REM hazard path is explicitly NOT shifted (its exponential enters
  the likelihood absolutely). The C++ side adopts the same form via a shared
  helper so both engines agree under extreme parameters. Every `src/` diff
  goes through the cpp-reviewer before commit (repo rule).
- **Rewrite the `default_c` REM and coordination estimators** — the slowest
  C++ cells: REM's fused per-dyad scalar loop (per-dyad `dot(row, β)` on
  strided rows + rank-1 Fisher outer products) becomes a staged BLAS pipeline
  (GEMV → masked exp → GEMV + one weighted-crossprod GEMM); the coordination
  estimators drop the symmetric full-matrix formulation (`p % p.t()`,
  `accu(P)/2`, and a full n²×p `P_3 = stat_mat` copy per event) for a
  **dyad-triangle representation** (length-d weights, one reused d×p buffer,
  Fisher as one GEMM, log-space dyad softmax) — the shape already validated in
  the goldfish_latent Stan implementation. Residual micro-opts (incl. the
  choice estimator) stay profile-gated.
- **Index-based ragged coordination path on `gather_compute`, end-to-end**:
  the gather emits per-row sanitized indices (`index_i`/`index_j`) plus
  per-sender CSR offsets and dyad-pairing indices; the rewritten kernel
  consumes only that list form (one code path — unconstrained = the full
  off-diagonal list, a folded `support_constraint` = a shorter list), one-mode
  coordination stops shipping diagonal rows (the `twomode_or_reflexive = TRUE`
  forcing in `gather_` is deleted), and the `support-constraint-as-stat` §5.6
  `gather_compute`→`default_c` redirect + `cli_inform` are LIFTED — a
  constrained coordination model runs natively. Storage COMPRESSION of the
  gather stack (row dictionary, symmetry dedup) stays in
  `gather-rem-coordination-format`, which now only changes what the indices
  point into.
- **Index vocabulary on every long-format output**: `gather_model_data()` and
  the db long table gain per-row `index_i`/`index_j` (today candidate rows are
  identifiable only by positional convention — impossible once rows are
  filtered), and the internal process-state evaluators label their returns
  with the same sanitized ids — the join keys for diagnostics, residuals, and
  post-estimation analysis. Filtering rows is within the exported contract
  (the docs promise "up to" the full grid; the writers spec already mandates
  masked stacks) — documented in NEWS.
- **Numerical floor**: results are NOT byte-identical (BLAS reorders sums); the
  frozen coefficient baselines must PASS at their 1e-6 tolerance, and a
  per-event old-vs-new equivalence harness (~1e-10 on small fixtures) guards
  each refactored helper before the old code is deleted.
- Record before/after timings of the baseline suite as evidence the bottleneck
  moved.
- **New `optimizer` argument in `set_estimation_opt()`** selecting the
  algorithm that drives the iterations: `"newton_raphson"` (goldfish's damped
  Newton-Raphson, default and unchanged) or maxLik-backed methods (`"bfgs"`,
  `"bhhh"`, `"nelder_mead"`). The C++ `estimate_*` functions are already pure
  evaluators — `(β, preprocessed state) → {logL, score, Fisher}` — so the
  adapter is a closure factory handed to `maxLik::maxLik()`, plus mapping the
  maxLik result back into the goldfish result object. maxLik enters
  **Suggests** (runtime `requireNamespace()` check); maxLik optimizers run
  exclusively on the `default_c` evaluator (`engine = "gather_compute"` or
  `"default"` combined with a maxLik optimizer errors informatively — the
  gather layout does not scale well enough to be worth the combination).
- **Per-event score matrix** (n_events × p, mirroring `intervalLogL`): an
  opt-in evaluator output in `default_c` (required by BHHH's
  observation-level gradients) plus a user-facing
  `set_estimation_opt(return_event_scores = )` flag returning it as
  `event_scores` in the result (honored by `default_c` and the `default` R
  engine; `gather_compute` errors informatively). Enabler for future
  sandwich/clustered SEs, per-effect score-process (Schoenfeld-style)
  diagnostics, and event-influence measures — documented as uses, not
  implemented here.
- **Internal process-state evaluators** (`goldfish:::`, not exported): given a
  materialized process state (stat-matrix state + active sets, minimal delta
  from what preprocessing already produces) and parameters, return —
  DyNAM-choice: P(i→j) over active receivers; DyNAM-rate / REM: the
  rate/hazard per actor / per dyad; DyNAM-rate-ordered: P(sender is next).
  The D7/D8/D9 rewrites MUST isolate the per-event probability/rate
  computation behind these signatures (factored during the rewrite, not
  bolted on later). A state materializer replays the preprocessed update
  streams up to an event index, reusing the shared `apply_flat_updates()`
  C++ helper. These are the building blocks for a future
  `simulate()`-in-goldfish (recipe-style: preprocess steps + sample/update
  steps) and for DyNES data augmentation — the latter lands only after
  `refactor-single-data-object` provides per-layer panel metadata.

## Capabilities

### New Capabilities
- `likelihood-computation`: the default-engine per-event likelihood
  contributions (log-likelihood, score, information matrix, probability
  matrix) computed via vectorized linear algebra with no per-cell R-closure
  reductions; numerical equivalence contract (frozen baselines at 1e-6,
  old-vs-new per-event agreement at ~1e-10 on benign fixtures); in-house
  single-pass stable softmax for the multinomial paths in both engines
  (timed hazard path excluded); no new Imports (the softmax is in-house;
  maxLik is Suggests and unrelated to it).
- `optimizer-selection`: the `optimizer` argument of `set_estimation_opt()`
  — flat algorithm list, maxLik-backed methods behind a Suggests dependency,
  forced `default_c` evaluator, result-object parity with the
  Newton-Raphson path, BHHH fed by the opt-in per-event score matrix.
- `process-state-evaluators`: the internal state-materializer + per-submodel
  probability/rate evaluator contract that the estimation helpers are
  factored behind; consumed later by `simulate()` and DyNES augmentation.

### Modified Capabilities
- `preprocess-output-writers`: the gather/db writers' long formats carry
  per-row `index_i`/`index_j` (sanitized ids, label-decodable); one-mode
  coordination emits no diagonal rows; constrained coordination emits only
  mask-allowed dyad rows (extending the constraint clause that already covers
  choice); the "value-identical to the pre-refactor output" clause is scoped
  to the unchanged families.

## Impact

- **Code**: `R/estimation_core.R` (the contribution/derivative/information
  helpers listed above, plus the R stable-softmax helper); `src/` (the shared
  stable-softmax helper; rewritten `REM_default.cpp` / `REM_ordered_default.cpp`
  / `DyNAM_MM_default.cpp` / `compute_coordination_selection.cpp`; opt-in
  per-event score matrix in the `default_c` return contract; further
  micro-opts profile-gated; cpp-reviewer on every diff; force recompile);
  `R/set_opt.R` (`optimizer` argument); a new maxLik adapter (closure factory
  + result mapping, e.g. `R/estimation_maxlik.R`); internal state
  materializer + process-state evaluators. `DESCRIPTION` gains NO new Import;
  **maxLik enters Suggests**.
- **Tests**: new old-vs-new equivalence tests on small fixtures; the frozen
  coefficient + C++ golden baselines (`NOT_CRAN=true`, PASS not SKIP) are the
  regression floor — the refactor's goal is to make exactly these run faster.
  New: optimizer-agreement tests (maxLik BFGS vs Newton-Raphson coefficients
  on baseline fixtures), BHHH per-event-score tests, evaluator-vs-estimation
  consistency tests (all conditional on maxLik where applicable).
- **API change (additive only)**: `set_estimation_opt()` gains `optimizer`;
  the default path (`newton_raphson` × any engine) is byte-for-byte the same
  interface and, up to floating-point summation order, the same numbers.
  Coefficients (to 1e-6) and returned objects are unchanged for existing
  code. The process-state evaluators are internal (`:::`) — no exported
  surface until a future `simulate()` change stabilizes the contract.
- **Code (gather/export surfaces, design D13)**: `R/cpp_interface.R`
  (`gather_sender_receiver_model_r` index emit, `gather_` forcing removed),
  `R/preprocess_export.R` (`gather_model_data()` index columns + row-set
  docs), `R/preprocess_writers.R` (`write_gather_to_db` index columns),
  `R/model_estimate.R` (redirect lift).
- **Interaction with `support-constraint-as-stat`**: RESOLVED — archived
  2026-07-10, so its availability interface (`active_sender`/`active_dyad`)
  is final and this change implements directly against it; its §5.6
  coordination redirect is lifted here.
- **Interaction with `gather-rem-coordination-format`**: reduced to pure
  sequencing (it lands after this change and compresses the storage behind
  the fixed index contract); its former task 2.4 (ragged emit + redirect
  lift) moved HERE.

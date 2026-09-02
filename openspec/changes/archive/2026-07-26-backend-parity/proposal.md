# Proposal — backend-parity

## Why

`backend = c("cpp", "r", "gather")` now names a user's choice by what actually
runs, which turns "what do I give up by picking one?" into a contract question —
and the answer today is undocumented, uneven, and fails three different ways.
Of the five per-event diagnostic primitives, only `loglik` is produced by all
three backends: `scores` aborts on `gather`, `ranks` and `margins` are silently
absent on both `r` and `gather`, and `probabilities` silently redirects `cpp`
and `gather` onto `r`. The asymmetry is not mathematical. Every backend already
computes, at every event, the one object all four primitives reduce from; six of
the nine C++ kernels store the reductions and three do not, and the R backend
stores none. `residuals-gof` consumes these primitives as its substrate, so the
gap becomes a correctness problem the moment a diagnostic is asked for on a fit
that silently lacks its input.

## What Changes

- **One per-event reduction contract shared by every backend.** At each event
  every backend already forms a nonnegative weight vector `w_e` over the risk
  set and a scale `c_e`, whose product `m_e = c_e * w_e` is the expected-count
  contribution of each alternative (multinomial: `c_e = 1 / sum(w_e)`, so `m_e`
  is the probability vector summing to 1; timed: `w_e` are the rates and
  `c_e = Δt_e`, so `m_e` is the cumulative hazard over the interval). All four
  primitives are uniform reductions of `(w_e, c_e)`: `probabilities` stores
  the probability-scale vector `p_e = w_e / sum(w_e)` — on exact-time
  sub-models the competing-risks probability that each sender (rate) or dyad
  (REM) creates the *next* event, so the primitive sums to 1 per event in
  every family; `ranks` counts `w_ej > w_e,obs` (scale-invariant, so identical
  code in both families); `margins` scatter-accumulates by actor on the
  probability scale everywhere, with exact-time fits additionally storing the
  labeled expected-count (compensator) variant; and `event_scores` is the
  estimation score `X_e,obs - m_e' X_e`. Exact-time fits also store the
  conditional log-probability component under `loglik` (design D16, D12, D17;
  derivations in the design appendix).
- **The three `gather` kernels gain `scores`, `ranks`, `margins` and
  `total_rate`.** They already compute every input; `compute_multinomial_selection()`
  accumulates the per-event score into `derivative` and discards it. Margins
  additionally need the per-row actor index, which the gather stack already
  materializes as `index_i` / `index_j` and which is not currently threaded into
  the multinomial and Poisson kernel signatures.
- **The `r` backend gains `ranks` and `margins` natively**, accumulated in its
  contribution loop from the probability vector it already forms, without
  materializing the full per-event probability matrix that the storage guardrail
  exists to prevent.
- **The `cpp` backend gains `probabilities`**, ending the silent redirect onto
  `r`, with the primitive defined uniformly as next-event probabilities on
  exact-time sub-models (DyNAM-rate gains it outright). The storage guardrail
  continues to govern the cost.
- **A single failure mode replaces three.** A (backend × primitive) capability
  contract is declared once and consulted once, before preprocessing: an
  unsupported combination aborts with one cli error naming the backends that do
  support the primitive. Silent absence and silent backend redirects for
  primitives are removed. This also fixes an ordering artifact where
  `backend = "gather"` with `diagnostics = "all"` silently succeeds on `r` while
  the same request without `"probabilities"` aborts.
- **The backend vocabulary becomes the only runtime vocabulary.** The rename
  that `backend-vocabulary` stopped at the constructor is pushed through: the
  control object stores `backend` (`"cpp"` / `"r"` / `"gather"`), and the
  legacy engine tokens (`"default_c"`, `"default"`, `"gather_compute"`) are
  deleted downstream of `set_algorithm_newton()` — the estimation gates, the
  dispatch, and the internal `estimate_c_int()` signature compare backend
  values directly, and the `engine_backend()` message round-trip disappears.
  Legacy *input* (`engine =`, legacy values) still folds with one warning,
  unchanged.
- **The documented `$engine` component of the control object is dropped at
  2.0.0.** The control object carries `$backend`; a 1.9-built control list
  carrying only `$engine` is accepted through a read shim at the estimation
  gate. Internal callers still on the deprecated surface
  (`functions_preprocess_em.R` calls `set_algorithm_newton(engine = "default")`)
  migrate to `backend = "r"`.
- **The fitted result records what ran.** `result.goldfish` gains a `backend`
  component, written once in the results assembly, so a consumer — above all
  `residuals-gof`'s diagnostics — can gate on it and say "refit with
  `backend = "cpp"`" instead of guessing. Pre-2.0.0 fits lack the field;
  consumers treat its absence as unknown, never as an error.
- **The gather multinomial and Poisson kernels adopt the shared stable
  softmax**, which the living requirement scoped to `default_c` only. The
  frozen 1e-6 coefficient baselines cover `r` and `cpp` and never froze a gather
  coefficient, so this is constrained by cross-backend agreement tests rather
  than by the regression floor.
- **BREAKING (observable, not API):** `diagnostics` requests that today warn and
  silently change backend now either run on the requested backend or abort. Code
  that relied on the redirect gets its result from the backend it asked for.
- **BREAKING (API, public component):** the control object's `$engine`
  component — carried by the returned list since `set_estimation_opt()` in
  v1.7.0, documented in 1.9.x — is removed; `$backend` replaces it. The
  removal lands at the 2.0.0 major version with a NEWS entry, and estimation
  still accepts old control lists via a read shim. Surfaces that never shipped
  in a public release (CRAN 1.6.x or tag v1.7.0) get no such ceremony — the
  design's deprecation-scope audit (D11) draws that line for every surface
  this change and `residuals-gof` touch.

## Capabilities

### New Capabilities

- `backend-primitive-parity`: the (backend × primitive) support contract, the
  shared per-event reduction all backends compute it from, the numerical
  agreement between backends for each primitive, the single failure mode for
  an unsupported combination, and the fitted result recording which backend
  produced it.

### Modified Capabilities

- `optimizer-selection`: the `return_event_scores` requirement currently states
  that the `gather` backend aborts; it is renamed to "Per-event scores
  primitive" and restated — the primitive is requested via `diagnostics`, all
  three backends honor it and agree within the cross-backend tolerance, and
  the legacy `return_event_scores` flag (never in a public release) is removed
  rather than deprecated. This change owns the requirement wholesale;
  `residuals-gof` no longer carries an `optimizer-selection` delta (design
  D6 revised). The `backend replaces engine` requirement (added by
  `backend-vocabulary`) loses its "internal engine tokens are unchanged"
  clause: the backend values become the runtime vocabulary end-to-end, the
  control object carries `$backend` (no `$engine`), and legacy control lists
  are shimmed at the estimation gate.
- `likelihood-computation`: the shared-stable-softmax requirement is scoped to
  the `default_c` multinomial normalizer loops; it extends to the gather
  multinomial and Poisson kernels so all compiled paths share one overflow
  behavior.

## Impact

- **goldfish C++ (`src/`)**: a new shared reduction header consumed by all nine
  kernels; `compute_multinomial_selection.cpp`, `compute_poisson_selection.cpp`
  and `compute_coordination_selection.cpp` gain the opt-in accumulators, the
  scatter index and the stable softmax; the six `*_default.cpp` engines are
  refactored onto the shared header without changing what they compute.
  `cpp-recompile` discipline applies to every edit.
- **goldfish R**: `cpp_interface.R` (threading `index_i` / `index_j` into the
  gather dispatch, returning the new components, and the `estimate_c_int()`
  signature moving to `backend = c("cpp", "gather")`), `estimation_core.R` (the R
  backend's rank and margin accumulators, mirroring the C++ reductions as
  `stable_softmax()` already mirrors its C++ counterpart), `model_estimate.R`
  (the capability check replacing the per-primitive redirects and aborts, the
  gates comparing backend values, the fit's `backend` component in the results
  assembly), `set_opt.R` (the control object stores `backend`, `$engine` and
  the `engine_backend()` round-trip removed), `functions_preprocess_em.R`
  (internal calls off the deprecated `engine =`), `zzz_testthat_helpers.R`
  (the synthetic fit gains the field). No legacy token reaches C++ as data —
  the backend branch is decided in R — so `src/` sees only the parity work.
- **Tests (vocabulary push)**: ~11 files reference the legacy tokens
  (heaviest `test-set_opt.R`); the references migrate mechanically and
  `_snaps/set_opt.md` regenerates. The frozen 1e-6 baselines are untouched by
  a string rename and must stay PASS — the `NOT_CRAN=true` gate per task is
  the proof.
- **Tests**: three-way parity per primitive per submodel family; the existing
  parity tests that reconstruct `r` ranks and margins from
  `return_probabilities` become direct comparisons.
- **Sequencing**: `backend-vocabulary` archives before this change implements —
  this change's `optimizer-selection` delta is written on top of its wording
  (including modifying its `backend replaces engine` requirement), so archive
  order is not optional. Within this change the vocabulary push runs first
  (tasks section 1), so the capability check and every kernel task are written
  under the final names. This change also modifies `optimizer-selection` ::
  "User-facing return_event_scores option", which `residuals-gof` also modifies. The two must
  not both carry a version of that requirement into the archive; the division is
  settled in design D6, and rebasing `residuals-gof`'s delta is a task here.
- **Superseded EM prototype removed** (design D18): the vocabulary push found
  `R/functions_estimate_emdynam.R` and `R/functions_preprocess_em.R` to be the
  driver half of a prototype whose helper half lives only in `.plan/DyNES/` —
  unexported, referenced nowhere, calling 14 functions that do not exist, and
  carrying a `retunr(...)` typo proving it has never executed. `abmcem`
  productizes the same research line as `set_algorithm_em()` /
  `estimate_dynes()`. The two files move to `.plan/DyNES/` to rejoin their own
  helpers rather than being maintained in either vocabulary; this removes the
  package's entire undefined-global surface before the 2.0.0 CRAN submission.
- **Not in scope**: the `opportunities_list` backend redirect (a preprocessing
  capability, not a per-event primitive), the timed-family Non-Goal that keeps
  plain `exp()` for the absolute-scale hazard, any new primitive beyond the
  five `diagnostics` already names, and the undeclared `parallel` dependency in
  `tests/testthat/helper-baselines.R` (a real but pre-existing and unrelated
  gap).

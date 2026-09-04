## Context

The default R engine computes per-event likelihood contributions in
`R/estimation_core.R` via `compute_event_contribution.<spec>` methods dispatched
once per estimation (`bind_compute_step`). The hot sites, called per event × per
Newton-Raphson iteration:

- `event_contribution_rate` (DyNAM-rate + REM, `estimation_core.R:690-773`):
  - `:703` `statsArray <- apply(statsArray, 3, c)` — a full copying flatten of a
    column-major cube whose first two dims can be merged with `dim<-` for free;
  - `:737-742` `ratesStatsStatsSum <- colSums(t(apply(statsArray, 1, \(x)
    outer(x, x))) * rates)` — n² R-closure calls materializing an n²×p²
    intermediate; equals `crossprod(statsArray, statsArray * rates)`;
  - `:743-750` a scalar-case `for` loop kept alive only because the `apply`
    result drops dimensions.
- `compute_event_contribution.dynam_rate_ordered_spec` (`:824-865`):
  - `:838` `exp(rowSums(t(t(statsMatrix) * parameters)))` — two transposes for
    what is `exp(statsMatrix %*% parameters)`;
  - `:848-858` information matrix via `apply(deviations, 1, outer)` — equals
    `crossprod(deviations, deviations * eventProbabilities)`.
- `getMultinomialProbabilities` (`:1537-1586`, choice / coord / REM-ordered):
  - `:1565` `exp(apply(weightedStatsArray, c(1, 2), sum))` — an R `sum()` per
    CELL of the n1×n2 matrix; equals a `(n1·n2)×p` reshape `%*% parameters`.
- `getMultinomialInformationMatrixM` (`:1505-1531`) and
  `getInformationMatrixREM` (`:1057-1079`): `expand.grid(p, p)` + `apply`
  closures over n² slices — each equals one `crossprod(D, D * w)` on the
  flattened derivative matrix.
- `getMultinomialInformationMatrix` (`:1475-1502`, coordination): p² `sum()`
  closures over n² slices with an upper-triangle mask.
- `compute_first_derivative_choice_coord` (`:1006-1037`): three n²p temporaries
  (`rep`, `apply`, `aperm`) partially reducible to matrix products (the `aperm`
  symmetrization is C-level and stays).

Constraints: the frozen coefficient baselines (`tests/testthat/_baselines/`,
1e-6, PASS not SKIP under `NOT_CRAN=true`) are the regression floor and must not
be regenerated; `support-constraint-as-stat` (ARCHIVED 2026-07-10) landed the
availability consumption this change builds on — its `active_sender`/
`active_dyad` interface is final, so the D6 sequencing precondition is
satisfied. Its one deliberate gap is inherited here: a constrained
coordination model on `engine = "gather_compute"` is redirected to `default_c`
with a `cli_inform` (its §5.6), because `compute_coordination_selection`
requires a rectangular candidate grid a masked risk set cannot form. This
change closes that gap natively (D9/D13).

## Goals / Non-Goals

**Goals:**
- Replace every per-cell/per-row R-closure reduction in the default-engine
  contribution helpers with the equivalent BLAS-backed linear algebra.
- Cut the wall-clock of the frozen-baseline suite (the REM and
  choice-coordination `default` cells dominate); record before/after timings.
- Close the `matrixStats` question on verified evidence (no Import, D1) and
  ship an in-house single-pass stable softmax for the multinomial paths, in
  both R and C++ (D7).
- Rewrite the `default_c` REM and coordination estimators — the slowest C++
  cells, mirroring the R bottleneck — in staged BLAS-3 form (D8) and the
  dyad-triangle representation (D9); residual micro-opts only on measurable
  wins (D5).
- Add the `optimizer` argument to `set_estimation_opt()` with maxLik-backed
  methods (D10) on top of the existing evaluator contract, including the
  per-event score matrix BHHH needs (D11).
- Factor the per-event probability/rate computation behind internal
  process-state evaluator signatures during the D7/D8/D9 rewrites (D12), so
  a future `simulate()` and DyNES augmentation reuse them without a third
  rewrite of this code.

**Non-Goals:**
- No change to coefficients beyond floating-point summation order and the D7
  stabilization (1e-6 baselines PASS; ~1e-10 old-vs-new agreement on benign
  fixtures); the `newton_raphson` default path keeps its exact interface and
  behavior.
- No stabilization of the TIMED rate/REM hazard contribution
  (`event_contribution_rate`): its `exp()` enters the likelihood absolutely
  (−timespan · Σ exp(xᵢ), a hazard sum) — max-shifting changes the value, not
  the conditioning. Overflow there signals diverging parameters, which is the
  damping mechanism's responsibility.
- No preprocessing changes (the state materializer consumes preprocessing
  output as-is) and no change to evaluator/engine dispatch for the default
  optimizer.
- No exported simulation API: the process-state evaluators stay internal
  (`:::`) until a `simulate()` change stabilizes the contract; no event
  generator loop, no sampling steps here.
- No DyNES data augmentation: it requires per-layer panel metadata reserved
  by `refactor-single-data-object` and lands after it.
- No regeneration of the frozen baselines.
- **No change to the undirected reciprocal stat-update** (`model_preprocess.R`
  ~:839: an undirected-network event evaluates its effect template twice — once
  `(i, j)`, once swapped `(j, i)` — and `rbind`s both deltas, so undirected nets
  pay 2× the effect evaluation and 2× the update-buffer rows). Halving it via a
  triangle-stored statistic is the same representational lever as D9, but it
  belongs to a **separate future discussion on undirected semantics —
  undirected-as-DEPENDENT vs undirected-as-EXPLANATORY** (they may want different
  storage/update rules), out of scope here. Flagged to pick up there; noted so
  the D9 triangle work does not silently assume it.

## Decisions

### D1 — No matrixStats Import: decision closed by inspection (gate dropped)
Each hot site is rewritten with `%*%` / `crossprod()` / `tcrossprod()` /
`dim<-` (the mapping is in Context). The originally planned benchmark gate for
`matrixStats` is DROPPED — the question was closed by inspecting the installed
package (4.6 library, 2026-07-07): it ships **no `inst/include` headers** (no
`LinkingTo:` possible), registers **only its 37 `.Call` wrappers** (no
`R_RegisterCCallable` — `getNativeSymbolInfo("rowLogSumExps_double")` fails),
and although the C workers (`logSumExp_double`, `rowLogSumExps_double`) are
exported symbols in `matrixStats.so`, binding them via `dlsym` would depend on
undocumented internals against WRE guidance — not viable for a CRAN package.
On the R side its value would have been the stable log-sum-exp — which the
likelihood cannot exploit as a package call anyway: every contribution needs
the full rate/probability vector (for `pMatrix`, expected statistics, score,
information), so a separate `logSumExp()` duplicates the `exp()` pass. The
useful part — the max-shift stable form — is adopted **in-house** instead
(D7). *Alternative rejected:* keep the benchmark gate — with the C++ door shut
and the R-side value re-implemented in four lines, there is nothing left for
the benchmark to decide.

### D2 — Copy-free cube flatten via dim<-
`apply(statsArray, 3, c)` (`:703`) becomes a `dim<-` merge of the first two
dimensions — R arrays are column-major, so merging leading dims is a metadata
change, not a data move. The same idiom serves the (n1·n2)×p reshape feeding
the softmax matrix product and the information-matrix `crossprod`s.
*Alternative rejected:* keep `apply(…, 3, c)` — it allocates and walks n²p
doubles per event for a no-op layout change.

### D3 — Information matrices as weighted cross-products
Every information matrix here is ∑ᵢ wᵢ dᵢ dᵢᵀ for row-vectors dᵢ of a (rows)×p
matrix `D` and weights `w` (rates, probabilities, or triangle-masked
likelihoods): `crossprod(D, D * w)`. The coordination variant keeps its
upper-triangle mask by pre-masking `w`. This removes `expand.grid` + p²
closures and the scalar-case special branch (`:743-750`), since `crossprod`
preserves matrix dimensions where `apply` dropped them. *Alternative rejected:*
loop-hoisting the existing closures — still O(p²) R calls per event.

### D4 — Old-vs-new equivalence harness before deletion
Each refactored helper lands alongside a test that runs the ORIGINAL
implementation (captured in a test fixture/helper, not exported) and the new
one on small deterministic fixtures (both datasets, all six sub-models,
2–4 parameters), asserting per-event agreement of all four outputs
(logLikelihood, score, informationMatrix, pMatrix) within 1e-10. Only after
the harness passes is the old code path deleted. The frozen baselines at 1e-6
remain the end-to-end floor. Rationale: BLAS summation reordering makes
byte-identity unachievable, so the refactor needs its own tighter,
per-component floor to localize any discrepancy. *Alternative rejected:*
baselines-only verification — a 1e-6 end-to-end net cannot attribute a failure
to a specific helper.

### D5 — C++ scope: stable softmax + the REM/coordination rewrites; residual micro-opts on evidence
The `default_c` side gets THREE distinct treatments. (a) **Decided:** the
multinomial normalizer loops adopt the shared D7 stable-softmax helper (a
small header/source pair mirroring the `src/flat_updates.h` pattern).
(b) **Decided:** the REM and coordination estimators — the measured slowest —
are rewritten in BLAS-3 form (D8, D9); the guiding rule is *multiple BLAS
passes beat one fused scalar pass*: a staged GEMV/GEMM pipeline runs at full
arithmetic intensity while a fused per-dyad loop degrades everything to
strided BLAS-1 (the current REM loop even computes `dot(row, β)` per dyad
instead of one GEMV, `REM_default.cpp:229-230`). (c) **Evidence-gated:** any
further micro-optimization (including the choice estimator, whose per-event
work is n rows not n²) requires a recorded profile showing a measurable gain.
matrixStats is structurally unusable from `src/` (D1), so everything here is
plain C++/Armadillo. Every `src/` diff goes through the cpp-reviewer before
commit. Verified safety: the "C++ golden" suite (`test-cpp_interface.R`)
compares R-vs-C++ **coefficients/vcov at testthat tolerance (~1.5e-8)**, not
byte-frozen snapshots — converged coefficients are optimizer fixed points, so
the rewrites pass it, and the D4-style cross-engine fixtures at 1e-10 provide
the tighter per-component floor. *Alternative rejected:* skip C++ — the two
dominating baseline cells are `default_c` REM/coordination as much as their R
twins, and would leave the engines with different overflow behavior.

### D7 — In-house single-pass stable softmax (R + C++), multinomial paths only
An internal helper (R, e.g. alongside the contribution functions; C++, a shared
header mirroring `src/flat_updates.h`) computes the max-shift softmax in ONE
exp pass and returns BOTH outputs the likelihood needs: `m = max(x)`,
`e = exp(x − m)`, yielding the probabilities `e / Σe` and the log-normalizer
`m + log(Σe)`. It is explicitly NOT a bare `log_sum_exp(x)` scalar function
called next to a separate `exp()` — that would re-create in-house the
double-pass flaw that disqualified `matrixStats::logSumExp` (D1). The observed
alternative's log-likelihood is computed as `(x_sel − m) − log(Σe)` rather than
`log(probabilities[sel])`, which stays finite when the observed event's
probability underflows (today `log(0) = -Inf`, e.g. `estimation_core.R:889`,
`:928`). **Scope:** the four multinomial contributions — DyNAM-choice,
DyNAM-choice-coordination, REM-ordered, DyNAM-rate-ordered — where the
likelihood is shift-invariant (the normalizer cancels `m`). The timed
rate/REM hazard path keeps plain `exp()` (Non-Goal: its scale is absolute).
**Numerics:** on benign fixtures the stable and plain forms agree to machine
precision, so the D4 1e-10 harness is unaffected; for extreme linear
predictors (overflow/underflow) the old code returns `Inf`/`NaN`/`-Inf` while
the new code returns finite correct values — covered by dedicated
extreme-parameter tests as NEW behavior, not by the equivalence harness.
*Alternative rejected:* stabilization as a separate later change — the softmax
sites are being rewritten here anyway; touching them twice doubles the
verification cost.

### D8 — REM estimators (C++): masked weight vector + weighted cross-products
`estimate_REM` / `estimate_REM_ordered` replace the fused per-dyad scalar loop
(`REM_default.cpp:216-240`: per-dyad `dot(row, β)` on rows strided by n², plus
a p×p rank-1 outer product per dyad) with the staged BLAS form over the
(n1·n2)×p `stat_mat`:
1. `x = S β` — one GEMV;
2. `e = exp(x) ⊙ mask` — one vector pass, where the mask folds presence1 ⊗
   presence2, the reflexive-diagonal exclusion, and (once
   `support-constraint-as-stat` lands) `active_dyad` — exclusions enter as
   zeros exactly as today;
3. `normalizer = Σ e`, `weighted_sum = eᵀ S` (GEMV),
   `fisher_event = (S.each_col() % e)ᵀ S` (one SYRK-style GEMM) — the same
   weighted cross-product identity as the R-side D3.
The per-event imputation scan (`find_nonfinite` over all N×p even when nothing
is missing, `:173-185`) gets a cheap non-finite guard if the profile confirms
it, semantics unchanged. *Alternative rejected:* keep the fused loop and only
micro-optimize it — the fusion is the problem, not the constants (BLAS-1 on
hostile strides).

### D9 — Coordination estimators (C++): dyad-triangle representation (Stan-proven)
`DyNAM_MM_default.cpp` and `compute_coordination_selection.cpp` are rewritten
around a **length-d dyad-triangle representation** (d = n(n−1)/2), the shape
already validated in the goldfish_latent Stan implementation
(`DNRE_Q1_coord.stan`: triangular `index_pos`, buffer reuse, log-space
accumulation). Replacing, per event:
- `P = p % p.t(); P /= accu(P)/2` (two full n² temporaries for a symmetric
  matrix, half discarded) → a **length-d weight vector** built in one triangle
  pass: `logw_d = log p(i→j) + log p(j→i)`, normalized via the D7 stable
  softmax over dyads — which also replaces the underflow-prone final
  `log(P(s,r))` with `logw_obs − logSumExp(logw)` (the coordination likelihood
  IS a d-alternative softmax);
- `P_3 = stat_mat` (a FULL n²×p copy per event per iteration, rows strided by
  n², 2× the rows ever read) → a **compact d×p buffer `D` allocated once
  outside the event loop** and reused, filled in one triangle pass with
  `D_d = s_ij + s_ji − E_i − E_j`, where the expected-statistics block
  `E (n1×p)` comes from per-sender GEMVs;
- the Fisher rank-1 loop (`Σ_d w_d D_dᵀ D_d`, d BLAS-1 outer products on
  strided row views — the dominant O(n²p²)) → **one GEMM**
  `(D.each_col() % w)ᵀ D − gᵀ g` with `g = wᵀ D` (D3 identity again).
The gather variant keeps its per-event candidate layout but gets the same
triangle/weights/GEMM core. *Alternative rejected:* keep the n²-matrix
formulation and optimize in place — the symmetric double-work, the per-event
copy, and the BLAS-1 Fisher are all consequences of that representation; the
R translation lineage is not a reason to keep it.

**Ragged risk sets fall out of the dyad-list form — this change owns the
whole path (tasks 5.7–5.9).** The square `p % p.t()` reshape is exactly what
forces `gather_compute` to require a full rectangular candidate grid
(`n_candidates = n1·n2`); a folded point `active_dyad` support mask
(support-constraint-as-stat §5.6) gives each sender a different
allowed-receiver subset, so the grid is ragged and the reshape malforms — today
that path is redirected to `default_c`. Because the dyad-triangle core is a
softmax over a *list* of dyads (per-sender row-softmax over allowed receivers,
then a d-alternative dyad softmax), a masked risk set is simply a shorter list —
no square matrix, no special case. Both halves land HERE, as ONE code path:
the gather emits, per candidate row, explicit sanitized indices
(`index_i`, `index_j`) plus the derived structures the per-iteration compute
wants precomputed — per-sender offsets (CSR: each sender's allowed-receiver
group) and the dyad-pairing permutation matching each (i,j) row to its (j,i)
partner (well-defined because the folded mask is symmetric). The rewritten
kernel consumes ONLY these index structures: an unconstrained model is the
full off-diagonal list, a constrained model a shorter one — the constraint
never appears in the compute, it already happened at emit. This retires the
rectangularity metadata (`n_candidates2` currently stores only the LAST
sender's count, `cpp_interface.R:1096` — meaningless for a ragged stack),
deletes the `twomode_or_reflexive = TRUE` forcing for DyNAM-MM in `gather_`
(diagonal rows are no longer emitted for one-mode coordination; the kernel's
`p.diag().zeros()` dies with the square form), and lifts the §5.6
`default_c` redirect + its `cli_inform`. Earlier drafts split this with
`gather-rem-coordination-format` (compute half here, emit half there); that
left 5.7 as a consumer with no producer, dead until the last change landed —
rejected. What REMAINS in `gather-rem-coordination-format` is pure storage
COMPRESSION (unique-row dictionary, symmetry dedup, triangle single-row
storage): changes to what the indices point into, never to which rows exist
or how the kernel reads them (D13).

### D10 — `optimizer` argument: flat algorithm list, maxLik in Suggests, default_c only
`set_estimation_opt()` gains `optimizer = c("newton_raphson", "bfgs", "bhhh",
"nelder_mead")` (lme4 precedent: `lmerControl(optimizer = ...)` — one knob
naming the algorithm, the implementing package a documented detail). Values
are lowercase snake_case per package style, mapped internally to maxLik's
names (`"BFGS"`, `"BHHH"`, `"NM"`). `"newton_raphson"` (default) is the
existing damped NR loop, untouched. Non-default values: (a) require maxLik at
runtime (`requireNamespace()`, `cli_abort` with install hint if absent —
maxLik goes in **Suggests**, not Imports, so users who never leave NR install
nothing new); (b) run exclusively on the `default_c` evaluator — combining a
maxLik optimizer with `engine = "gather_compute"` (does not scale) or
`"default"` errors informatively. The adapter is a closure factory over the
C++ evaluator with the preprocessed data fixed; because maxLik calls
logLik/grad/hess as separate functions, one evaluator call per β is memoized
and shared by the three closures (the evaluator returns all outputs at once).
The maxLik result maps back into the standard goldfish result object
(coefficients, vcov from the Fisher at the optimum, iteration count,
convergence code) so `summary()`/`vcov()`/`logLik()` work identically.
Offsets/fixed parameters are resolved before the adapter, exactly as on the
NR path. *Alternatives rejected:* new `engine` values (conflates the
evaluator axis with the optimizer axis; each fused value would need a second
convention for which evaluator it calls); `optimizer = "maxlik"` + separate
`optim_method` (two knobs for one decision, leaks the dependency name into
the API); `method` (overloaded — reads as estimation methodology next to
`model`/`sub_model`).

### D11 — Per-event score matrix: C++ opt-in + user-facing `return_event_scores`
maxLik's BHHH needs observation-level gradients (an n_events × p score
matrix), but the evaluators return only the aggregated score. Since D8/D9
rewrite those estimators anyway, the `default_c` evaluators gain an opt-in
flag returning the per-event score matrix alongside the aggregate (mirroring
how `intervalLogL` already returns per-event logL). Default off — no memory
cost for long sequences unless requested.

**Two distinct switches.** The C++ opt-in is internal plumbing: the maxLik
adapter turns it on when `optimizer = "bhhh"` regardless of user settings.
The user-facing switch is `set_estimation_opt(return_event_scores = FALSE)` —
named parallel to `return_interval_loglik`/`return_probabilities` (NOT
`return_interval_scores`: "interval" is the timed-likelihood legacy framing
and reads wrong for ordered submodels; NOT bare `return_scores`: "the score"
unqualified means the aggregate gradient) — controlling whether the matrix is
kept in the result object as `event_scores` (snake_case per the project style
directive — new identifiers are snake_case even though the legacy result names
like `intervalLogL` are camelCase; columns named by effect). **Engine coverage:** `default_c` (the C++ flag) AND
the `default` R engine (its contribution loop computes each event's score
before summing — capturing it is nearly free); `gather_compute` errors
informatively. **Documented uses** (roxygen motivation; the diagnostics
themselves are future changes): `colSums(S) ≈ 0` sanity check at the MLE;
`crossprod(S)` as the OPG "meat" for sandwich/clustered robust SEs;
OPG-vs-Fisher information-matrix misspecification comparison; per-effect
cumulative score processes (`cox.zph`-analog time-constancy diagnostics,
Schoenfeld-residual equivalent — localizing WHICH effect drifts where
`examine_changepoints()` on `intervalLogL` only sees WHERE the fit drifts);
one-step `I⁻¹ sᵢ` dfbeta-style event influence; Louis-identity observed
information for the future DyNES EM. *Alternative rejected:* aggregate-only
launch without BHHH — the marginal cost of the extra output inside a rewrite
already touching every accumulation loop is small, and retrofitting it later
means a third pass over `src/`.

### D12 — Internal process-state evaluators, factored during the rewrite
The D7/D8/D9 rewrites MUST isolate the per-event probability/rate computation
behind named internal helpers with a process-state contract — not inline it
into the estimator loops again. Contract: given a materialized state (dense
stat-matrix state + active/presence sets — a minimal delta from what
preprocessing already produces) and parameters β, return per sub-model:
DyNAM-choice → P(i→j) over active receivers; DyNAM-rate / REM → the
rate/hazard per actor / per dyad; DyNAM-rate-ordered → P(sender is next).
A **state materializer** replays the preprocessed update streams up to an
event index, reusing the shared `apply_flat_updates()` C++ helper (already
extracted). Everything stays internal (`goldfish:::`) — no docs/export
commitment until a future `simulate()`-in-goldfish change (recipe-style:
preprocess steps + [probs → sample → apply update] steps) stabilizes the
contract; DyNES augmentation additionally waits on
`refactor-single-data-object`'s per-layer panel metadata. Verification:
evaluator outputs at observed event indices must agree with the estimation
path's per-event quantities (pMatrix / intervalLogL) at 1e-10. *Alternative
rejected:* exposing an exported API now — the state contract will move when
`simulate()` and the single-data-object land; exporting freezes it too early.

### D13 — Index vocabulary on every long-format output; filtered rows are in-contract
Every long-format (one row per event × alternative) output SHALL carry
explicit row identity in ONE shared vocabulary: the sanitized 1-based integer
actor ids already used internally (`event_mat`, the flat buffers), emitted per
row as `index_i`/`index_j` (dyad-indexed models; sender-set rows carry
`index_i`, receiver-set rows `index_j`). Surfaces: the internal gather
metadata (the D9 emit), the exported `gather_model_data()` return, the db
long table (`write_gather_to_db`), and the D12 process-state evaluators'
per-actor/per-dyad returns — with the existing node-label decoding available
at the export boundary. Rationale: today row identity is purely positional
(the export's `sender`/`receiver` name only the OBSERVED dyad per event; the
db table has `event_id`/`is_selected`/`stat_<i>` and no actor columns at
all), and a positional decode dies the moment rows are filtered — after
filtering, the index columns are the ONLY record of which dyads were in the
risk set at each event. They are also what makes the export usable for its
documented purpose (tabular estimation in GLM-type packages) and for
diagnostics/residuals/post-estimation joins (per-dyad residuals, risk-set
audits, Schoenfeld-style per-effect drift against `event_scores`, event
influence).

**Filtering the row set is within the exported contract — verified, not
assumed.** (a) The roxygen return of `gather_model_data()` promises "up to"
events × actors rows with per-event `n_candidates` and within-event `selected`
— a ragged layout, nothing promises the full grid or the diagonal; (b) the
main `preprocess-output-writers` spec ALREADY mandates constrained gather
stacks contain only mask-allowed rows; (c) the function is the renamed
experimental `GatherPreprocessing()` (renamed without alias in the current
unreleased 1.7.1→1.8.x dev chain; release tags stop at v1.7.0), so no stable
released shape exists to preserve byte-for-byte; (d) for the export's stated
GLM purpose the one-mode coordination diagonal rows are noise the user must
currently drop by unwritten positional convention. Therefore: one-mode
coordination stops emitting diagonal rows, constrained models emit only
allowed rows, `n_candidates`/`selected` stay consistent per the existing
contract, and the NEWS entry documents the coordination export change. The
equivalence floor for the changed row set is cross-engine agreement (the
gather engine is outside the frozen-baseline grid), not byte identity.
*Alternative rejected:* keep the full grid (diagonal included) so the
exported expansion stays byte-identical — it preserves a convention no
document promises, contradicts the writer spec's filtering direction, and
forces every downstream consumer to re-learn the unwritten positional rules
the indices exist to replace.

### D6 — Sequencing with support-constraint-as-stat (RESOLVED — landed first)
That change rewired availability consumption (`riskMask`/`presence` →
`active_sender`/`active_dyad`) in the same functions' call sites; it was
ARCHIVED 2026-07-10, so its consumption interface is final and this change
implements directly against the new availability names (`active_sender`/
`active_dyad`, point encoding for folded REM/coordination constraints). The
equivalence harness (D4) fixes inputs at the helper boundary and is unaffected.

## Risks / Trade-offs

- **BLAS results differ in the last ulp** → the D4 1e-10 harness localizes any
  real error; the 1e-6 frozen baselines catch end-to-end drift; tolerances are
  stated per test, not implied.
- **`crossprod` on masked/zeroed rows must preserve the current zeroing
  semantics** (reflexive edges, `riskMask`, opportunity zeros enter as
  rate = 0) → the rewrite keeps the explicit `rates[maskedOut] <- 0` step and
  multiplies by the zeroed vector, exactly as today — the algebra folds the
  mask through unchanged.
- **Benchmark noise** → benchmarks are now evidence-only (BEFORE/AFTER timings
  and the D5 C++ micro-opt gate): fixed fixtures, ≥ 10× repetitions,
  `system.time` medians, numbers recorded in `progress.md`.
- **Stabilization changes results where the old code overflowed/underflowed** →
  intended and strictly better (finite instead of `Inf`/`NaN`/`-Inf`); benign
  fixtures agree to machine precision, extreme cases get dedicated tests (D7);
  the timed hazard path is excluded so no absolute rate changes value.
- **Same-file collision with support-constraint-as-stat** → resolved: it
  archived 2026-07-10 before this change's implementation starts (D6), so the
  availability interface is fixed and no rebase is needed.
- **Unconstrained coordination gather output changes its row set** (no
  diagonal rows, new index metadata; D13) → in-contract (the docs promise
  "up to" the full grid), but observable: cross-engine agreement tests carry
  the floor (the gather engine is outside the frozen baselines), the
  `preprocess-output-writers` spec delta states the new shape, and NEWS
  documents it for `gather_model_data()` users.
- **`dim<-` flatten assumes column-major contiguity** → guaranteed for R
  arrays; the equivalence harness would catch any misuse instantly.

## Migration Plan

1. Benchmark harness + BEFORE timings of the baseline suite and the isolated
   hot helpers (both datasets).
2. Refactor helper-by-helper with the D4 equivalence tests (rate/REM core →
   softmax/probabilities → derivatives → information matrices), baselines PASS
   after each commit.
3. Implement the D7 stable softmax: the R helper routed through the four
   multinomial contributions (with underflow-finite logL), then the shared C++
   helper in the `default_c` normalizer loops (cpp-reviewer on the diff);
   extreme-parameter tests both sides.
4. Rewrite the C++ REM estimators in the staged BLAS form (D8), then the
   coordination estimators in the dyad-triangle form (D9) — one estimator per
   commit, each gated on the 1e-10 cross-engine fixtures + cpp-reviewer +
   per-estimator BEFORE/AFTER timing.
5. Land the index-based ragged coordination path end-to-end (D9/D13): gather
   emit (indices, per-sender offsets, dyad pairing, no diagonal/forced
   `twomode_or_reflexive`), kernel consumption, §5.6 redirect lifted; then
   surface `index_i`/`index_j` on `gather_model_data()` and the db table.
6. Profile `default_c` residuals (D5c); apply + cpp-review any further
   measurable micro-opt or record "no change warranted".
7. Add the opt-in per-event score matrix to the rewritten `default_c`
   evaluators (D11; cpp-reviewer) and the internal state materializer +
   process-state evaluators over the D12 helper signatures (returns labeled
   with the D13 index vocabulary), with evaluator-vs-estimation consistency
   tests.
8. Add `optimizer` to `set_estimation_opt()` and the maxLik adapter (D10):
   closure factory with per-β memoization, result mapping, engine-combination
   guards, conditional tests (skip without maxLik) asserting BFGS/BHHH
   coefficients agree with Newton-Raphson at baseline tolerance.
9. AFTER timings; record the speedup in `progress.md` and `NEWS.md`
   (speedup, stable softmax, `optimizer` argument, maxLik in Suggests;
   coordination gather/export row-set change + new index columns).

Rollback: each helper's refactor is one commit gated on its equivalence test +
baselines — revertible independently.

## Open Questions

- Exact benchmark tooling: `bench` (accurate, adds a dev-only Suggests) vs
  repeated `system.time` medians (no new package). Default to `system.time`
  medians unless precision proves insufficient.
- Whether `compute_first_derivative_choice_coord`'s `aperm` symmetrization has
  a worthwhile matrix-product form, or stays as-is (C-level `aperm` may already
  be near-optimal; decide from the profile, not on paper).
- **Stat-matrix layout transpose (p×N instead of N×p)**: per-dyad row access
  currently strides by n² (p cache misses per dyad). The D8/D9 GEMM rewrites
  remove most row-wise access, so the transpose is deferred — benchmark-gated
  AFTER the rewrites, and only if the profile still shows stride-bound loops
  (it touches the shared flat/broadcast update helpers used by every
  estimator).
- **Gather-coordination data volume**: even with the diagonal dropped and the
  mask applied at emit (D13), the gather still stores both directed rows of
  every kept dyad and re-stores unchanged rows every event — the storage
  COMPRESSION (unique-row dictionary, symmetry dedup, triangle single-row
  storage) is `gather-rem-coordination-format`, sequenced after this change
  and constrained to change only what the D13 indices point into.

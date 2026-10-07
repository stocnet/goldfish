---
depends-on: availability-one-object-per-loop
status: landed (feature_simulation, awaiting fold)
---

## Why

**The likelihood site is the one per-event loop the cost rules now cover
that was never cleaned to them, and it runs ten to twenty times per fit.**
Commit `7b56821` extended `.claude/rules/walk-cost.md` to the r backend's
per-event contribution loop and the C++ kernels. An inventory on 2026-10-01
(session goldfish-0c, read-only, every item verified by reading) found the
site carrying three kinds of debt:

- **Debug and history leftovers.** Commented-out `Rcpp::Rcout` traces and
  dead sender-correction code in `src/DyNAM_rate_default.cpp`; commented-out
  `snowfall` blocks and `print()` calls in `R/estimation_core.R` and
  `R/cpp_interface.R`; `CHANGED <name>` and `to be make` markers; a
  `TO check(gutian)` note in two kernels; an `impute` argument that no caller
  sets, so its block in all six kernels is dead; `reduce_mat_to_vector()`
  copied into two kernels and mirrored as `.gather_reduce()`, all three an
  identity copy because the rate kernels receive sender-level statistics;
  `parallelize`, `cpus`, `verbose`, `get_data_matrix` threaded through
  signatures and read only inside commented code; `state$time` written and
  never read; the `event_reductions.h` header describing a calling
  convention two kernels do not follow.
- **Per-event allocation, paid per Newton iteration.** The REM, REM-ordered
  and coordination kernels build an `allowed` vector of `n1 * n2` doubles
  from scratch at every event, then `arma::find()` another index vector to
  zero the masked exponentials, then form an `n1 * n2 * p` temporary for
  the Fisher product; `weights`, `lin_pred`, `e` and `probabilities` are
  declared inside the loop so the shared log-sum-exp helper reallocates
  them; `event_score_row()` repeats a GEMV the kernel has just done. The r
  backend copies the whole statistics array on every event that has an
  update, again when it zeroes the diagonal, again when it subsets by the
  risk set, and copies every per-event accumulator (`event_scores`,
  `eventLogL`, the margins) because `state` is a list passed through a
  function; `accumulate_margins()` is quadratic; the coordination path
  allocates about nine `n^2 * p` arrays per event and takes a row maximum
  with `apply()`, which the living spec forbids. The gather builders grow an
  index vector with `c()` per sender and copy the point presence grid per
  event.
- **Work that does not depend on the parameters, redone per iteration.**
  Every `return_*` diagnostic (event scores, total rate and its shifted
  pass, ranks, margins, availability, the per-event probability list) is
  computed on every iteration and read once at the end; dyad index maps are
  rebuilt per call whether or not anything reads them;
  `has_unexpected_na()` unlists the entire result, event scores included,
  per iteration.

Measured on Social Evolution (84 actors, 439 events, 7 to 11 iterations),
median of three warm fits at `7b56821`:

| model | cpp | gather | r | r allocation | gather allocation |
|---|---|---|---|---|---|
| REM | 0.58 s | 2.80 s | 2.56 s | 7325 MB | 1823 MB |
| DyNAM rate | 0.06 s | 0.05 s | 0.12 s | 99 MB | 13 MB |
| DyNAM choice | 0.03 s | 0.05 s | 0.64 s | 1148 MB | 14 MB |

Seven gigabytes to fit an 84-actor REM is the per-event copying above; at
CollegeMsg scale (1899 actors) one `n1 * n2` double vector is 27.5 MB and
the kernels allocate two or three of them per event per iteration. No test
watches any of it: the walk has a recorded budget, estimation has none.

**This change absorbs `exact-time-diagnostic-cost`** (proposed 2026-09-08
at `dd9e5a4`, never applied, deleted with this proposal; ADR-0065 records
its decisions). It found the exact-time kernels paying a second,
max-shifted `exp` pass over the risk set on every event for the
`"loglik"` diagnostic the default `diagnostics` always requests, plus an
unread probability vector, and measured the tax on Social Evolution REM
(439 events, 6972 dyads, median of three):

| build | default diagnostics | `"scores"` only |
|---|--:|--:|
| at `950f21b`'s successor | 6.01 s | 4.72 s |
| probability vector guarded | 5.80 s | 4.74 s |

The block costs about 1.3 s of a 6 s fit, 0.2 s of it the probability
vector. The two items edit the same two event loops this change rewrites,
its open question about hoisting the rate kernel's recomputed linear
predictor is one of this change's decisions, and its fit-level ratio gate
(default against `"scores"`) stops discriminating once diagnostics are
evaluated once per fit: a 27% per-evaluation tax becomes 27% of one
evaluation in `k`, inside timing noise. The gate has to compare
evaluations, which this change's budget measures anyway. Keeping the two
apart would mean two passes over the same kernels and a gate recorded on a
shape the next change retires.

Now is the time because the availability object is being settled
(`availability-one-object-per-loop`) and the next estimation work, the
contract matrix, should run on kernels that cost what they compute.

## What Changes

- **Dead code and debug remnants go.** Commented-out traces and code,
  `snowfall` blocks, `print()` leftovers; the `impute` argument and its
  block from the six kernels and the R interface; `reduce_mat_to_vector()`
  (both copies) and `.gather_reduce()`; the unread `parallelize`, `cpus`,
  `verbose`, `get_data_matrix` parameters and `state$time`. History markers
  (`CHANGED <name>`, `to be make`, `TO check`) become the reason they
  carried where there is one and go where there is none, as Alvaro asked
  (2026-10-01, "clean up, undebug"); every comment that explains a why
  stays. The `event_reductions.h` header and the REM kernel's roxygen are
  corrected to what the code does.
- **Each kernel allocates per evaluation, not per event.** Work buffers
  (`lin_pred`, `weights`, `e`, `probabilities`, the Fisher product's
  weighted copy, the reduced statistics) are declared once before the event
  loop and filled in place. The risk-set `allowed` vector is maintained in
  place: initialized once from the availability's initial value with the
  diagonal rule applied, and written by the same update lambda that writes
  the availability, so an event costs the cells that flipped. The `find()`
  zeroing becomes a plain masked pass. `event_score_row()` reuses the
  weighted sum the kernel already formed. Dyad index maps are built only
  when margins or availability are requested.
- **The exact-time kernels derive the diagnostic normalizer from the pass
  the likelihood already computes** (absorbed from
  `exact-time-diagnostic-cost`). Where the raw normalizer is a normal
  positive double (finite, strictly positive, not subnormal), the
  conditional component's log-normalizer is
  `log(normalizer)` and the probability vector, when a consumer asks for
  it, is the raw weights over that normalizer; the max-shifted
  log-sum-exp stays as the fallback for overflow and subnormal underflow,
  exercised in a test through the same internal evaluator seam the
  allocation counters use. The probability vector is formed only under
  `return_probabilities || return_margins || return_conditional_scores`,
  as the four other kernels already do. The raw pass keeps the absolute
  scale: the compensator enters as `-dt * normalizer`. The one predicted
  numerical consequence: the conditional component may move in its last
  bits on the fast branch, bounded by the 1e-10 parity floor; no
  coefficient, standard error or log-likelihood moves.
- **The r backend writes its state in place.** The per-event state becomes
  an environment whose large members (statistics array, availability,
  accumulators) are written through `set_entries()` / `set_matrix_cells()`,
  never by subassignment of a shared binding; the diagonal is zeroed only
  on the cells an event updated; risk-set subsetting happens once per
  event into a reused buffer; `accumulate_margins()` uses `rowsum()`; the
  coordination path's row maximum and `n^2 * p` temporaries are replaced by
  index arithmetic and reused buffers. The r backend is the reference
  engine, so per-event outputs are pinned to 1e-10 against themselves
  before and after.
- **Diagnostics are evaluated once, at the converged parameters.** Newton
  iterations compute the log-likelihood, score and information only; after
  convergence one evaluation at the returned parameters produces every
  requested diagnostic component, with values identical to those the
  per-iteration path produced (the kernel is deterministic at fixed
  parameters). `has_unexpected_na()` inspects the three estimation
  components, not the diagnostic lists.
- **The gather builders stop growing and copying.** The candidate index is
  preallocated per event; the point presence grid is written in place.
- **An estimation cost budget**, the walk budget's twin: allocated memory of
  one fit on the r and gather backends and wall-clock per backend on fixed
  cells (Social Evolution REM, choice and rate; CollegeMsg 1k REM), with the
  walk budget's floor-and-ceiling discipline, run under `NOT_CRAN=true`;
  plus the self-normalizing diagnostics ratio ADR-0065 asked for, measured
  between two evaluations in one session (one with the requested
  diagnostics, one without) rather than between two fits, since the fit
  pays the diagnostics once. The compiled backend's allocation is not
  visible to R, so its cells are time, the ratio and the kernel allocation
  count. The one-off "Timing evidence for the refactor goal" requirement
  becomes this standing budget.
- **Not changed**: numbers. Coefficients, standard errors, log-likelihoods
  and every stored diagnostic are pinned (frozen baselines and C++ goldens
  PASS; per-event outputs at 1e-10), the conditional component on the fast
  branch excepted in its last bits as stated above. **Not changed**: the
  `diagnostics` vocabulary, its default, and the `"loglik"` mapping to
  `intervalLogL`, `total_rate` and the conditional component. **Not in
  scope**: the engine argument shape, owned by
  `availability-one-object-per-loop`; moving the r backend's per-event loop
  to C++ (ADR-0060 settled the direction for the walk, and the r backend is
  the reference, not the fast path); the `stat_mat = stat_mat_init` copy
  once per evaluation, which is the state reconstruction every iteration
  needs.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `likelihood-computation`: ADDED "Likelihood work buffers are allocated per
  evaluation and the risk set is maintained in place" (the per-event
  allocation contract for the kernels, the r backend and the gather
  builders, with the negative clause); ADDED "Diagnostics are evaluated once
  at the converged parameters"; ADDED "Exact-time kernels derive diagnostic
  normalizers from the likelihood pass" and ADDED "The per-event probability
  vector is materialized only for the primitives that read it" (both
  carried over from `exact-time-diagnostic-cost` unchanged); MODIFIED
  "Timing evidence for the refactor goal" into the standing estimation
  budget (allocation and time cells with floor and ceiling, the
  per-evaluation diagnostics ratio, the documented-speedup scenario kept).
  The existing "The r backend computes contributions via vectorized linear
  algebra" requirement is not modified; the coordination path is brought
  into conformance with it.

## Impact

- **Code, C++**: the six `src/*_default.cpp` kernels (hoisted buffers,
  in-place `allowed`, masked pass, `event_score_row()` reuse, guarded dyad
  maps, `impute` and `reduce_mat_to_vector()` removed, traces deleted),
  `src/event_reductions.{h,cpp}` (`event_score_row()` takes the weighted
  sum; header corrected), `src/log_sum_exp.cpp` (fills a caller-owned
  buffer without reallocating when sizes match), `src/compute_*_selection.cpp`
  (event-block views instead of copies, the duplicate probability pass),
  `src/RcppExports.*` regenerated (cpp-recompile skill, per kernel).
- **Received hand-offs** (from `availability-one-object-per-loop`'s apply,
  goldfish-ce, 2026-10-01; recorded here because that change's task 5.3
  routes estimation-side findings to this proposal): (i) the r backend at
  the outer encoding now reduces REM's rows by the sender factor and its
  columns by the receiver factor, so the reduced array need not be square,
  and names the self-dyads by actor through a `match()` per event when the
  two slot sets differ, one more per-event allocation beside the
  statistics-array reduction copy D3 removes; (ii) the gather backend
  returns DyNAM-rate per-event probabilities as `n x 1` matrices where the
  r backend returns vectors (every event, shape only), a cleanup item for
  D1. Two findings from the same hand-off are not taken here: the
  observed-event check's per-dependent-event row and `which()` in
  `availability_check_event()` is walk cost, and the REM point output's
  unread raw `active_sender_*` beside the grid is that change's own D2.
- **Code, R**: `R/estimation_core.R` (`estimate_int()`, `run_nr_loop()`,
  `compute_iteration_step()`, `compute_step.default()`, `r_reduce_event()`,
  `event_contribution_rate()`, the multinomial and coordination
  contributions, `accumulate_margins()`, `stable_softmax()`),
  `R/cpp_interface.R` (`make_engine_evaluator()`, `estimate_c_int()`, the
  gather builders and helpers, `has_unexpected_na()`), `R/model_estimate.R`
  (the final diagnostic evaluation), `R/utils.R` (`apply_flat_update()` /
  `apply_broadcast_update()` writing in place).
- **Per-event code touched**: all of the above is per-event code under the
  cost rules. No new preprocessed output, no new walk consumer, no new
  buffer format: the in-place writers are `set_entries()` /
  `set_matrix_cells()` from `src/state_write.cpp`, the availability pointer
  walk is `apply_availability_updates()`, the statistics decode is
  `apply_flat_updates()` / `apply_broadcast_updates()`, all existing and
  reused unchanged.
- **Tests**: new `test-likelihood_cost_budget.R` with
  `_budgets/estimation_cost.csv` and `record-estimation-cost.R` (mirroring
  the walk budget); per-group allocation guards recorded failing first;
  per-event output pins (`logLikelihood`, `score`, `informationMatrix`,
  `event_scores`, `event_probabilities`, margins, ranks, availability) at
  1e-10 on the six families before and after each group; a test that the
  diagnostics evaluated once equal those of the per-iteration path; the
  existing C++ goldens, backend-parity and frozen baselines as the
  end-to-end floor.
- **Numbers**: none move. Byte-identity is not claimed for BLAS-reordered
  sums (the living spec's floor is 1e-10); the frozen 1e-6 baselines and the
  goldens stay PASS.
- **Dependencies**: none added. `depends-on: availability-one-object-per-loop`
  (the kernels' argument lists must settle before their loops are
  reorganized; the two changes touch the same six files).
  `exact-time-diagnostic-cost` is absorbed and its directory deleted
  (Alvaro, 2026-10-01); `.claude/rules/walk-cost.md` still names it as the
  estimation gate's owner and is corrected in task 6.3. Branch
  `feature_simulation`; NEWS.d fragment, no Version bump (ADR-0040).
- **ADRs**: ADR-0110 (the likelihood allocates per evaluation, maintains
  the risk set in place, evaluates diagnostics once, and carries a cost
  budget; records the absorption and the gate measured between evaluations),
  ADR-0065 (the exact-time diagnostics derive from the likelihood pass with
  a guarded fallback, the probability vector only for its consumers, a
  self-normalizing ratio gate; implemented here), ADR-0067 (shared
  availability apply is the pointer walk), ADR-0060 (the preprocessing loop
  is not moved to C++), ADR-0054 (retire what no consumer calls), ADR-0021
  (a baseline moves only when the new value was derived beforehand).

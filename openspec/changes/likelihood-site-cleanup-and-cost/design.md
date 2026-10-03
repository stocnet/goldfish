## Context

Measured on `feature_simulation` at `7b56821`. See `proposal.md` for the
motivation and the before table; this section records the shapes the design
starts from, every item verified by reading (inventory of 2026-10-01).

**Kernel skeleton.** All six `src/*_default.cpp` kernels share one loop
shape: reconstruct state (`apply_flat_updates()`, `apply_broadcast_updates()`,
the dead `impute` block, the availability lambda through
`apply_availability_updates()`), build a risk-set mask, form the linear
predictor and weights, accumulate the derivative and Fisher, then the
opt-in blocks (probabilities, availability, ranks, margins, event scores).
The mask is rebuilt from scratch at every event: REM `:357-370`, REM-ordered
`:217-231`, MM `:236-250` loop over all `n1 * n2` cells into a fresh
`arma::vec allowed`; choice `:187` and rate-ordered `:187` allocate `n2` and
`n1` per event. `arma::vec weights` is declared inside the loop in REM,
REM-ordered, rate, rate-ordered and choice, so `log_sum_exp_masked()`'s
`weights.zeros(n)` reallocates (`src/log_sum_exp.cpp:10`). The Fisher
product `(stat_mat.each_col() % e).t() * stat_mat` (REM `:417`, the
multinomial trio likewise) forms an `n1 * n2 * p` temporary per event.
`event_score_row()` (`src/event_reductions.cpp:81-93`) recomputes `w' X`
that each kernel has just formed. REM `:397` zeroes masked exponentials
through `e.elem(arma::find(allowed < 0.5)).zeros()`, two temporaries. Dyad
index maps (`dyad_sender`, `dyad_receiver`; MM's endpoint maps) are built
per call unconditionally. The `impute` argument is FALSE at every caller
(`R/cpp_interface.R:53, 343, 1158-1160`); `reduce_mat_to_vector()` (rate
kernels `:405-426`, `:274-295`) and `.gather_reduce()` (`R/cpp_interface.R:1364`)
are identity copies because the rate kernels receive `n1 x p` statistics
with `n_actors_2 = 1`.

**r backend.** `compute_step.default()` (`R/estimation_core.R:1213-1486`)
holds its state in a list returned through `step_fn` (`:2220`, `:2311`), so
every member it modifies is copied: the statistics array on each update
(`apply_flat_update()`, `R/utils.R:539-550`, subassigns a passed-in array),
again when `diag(statsArrayComp[,, k]) <- 0` runs for every slice
(`:1344-1346`), again on `keepIn` subsetting (`:1376`, `:1432`) and on the
`matrix(...)` reduction (`:1457`); the accumulators `eventLogL`,
`event_scores[i, ]`, `EventProbabilities[[i]]`, margins and ranks
(`:1489-1495`, `r_reduce_event()`). `event_contribution_rate()` reshapes a
shared array with `dim<-` (`:745`) and forms two `n1 * n2 * p` products
(`:807`, `:814`). The coordination path allocates about nine `n^2 * p`
arrays per event (`:991-1045`, `:1111-1145`) and `stable_softmax(rowwise =
TRUE)` takes the row maximum with `apply(x, 1L, max)` (`:2524`), which the
living spec's r-backend requirement forbids. `accumulate_margins()`
(`:2593-2609`) is `vapply` over actors of `sum(contribution[slot == k])`,
quadratic. The legacy unfolded path filters a `compChange*` data frame per
event (`:1305`, `:1333`); `availability-one-object-per-loop` D2 removes it.

**Gather builders.** `idx <- c(idx, ...)` per sender per event
(`R/cpp_interface.R:1503`); `.gather_apply_presence_point()` (`:1350`) passes
the grid through a helper and subassigns; `.gather_broadcast_blocks()`
(`:1304`) runs an `lapply` per event and allocates `seq_len(n1 * n2)` at the
global kind.

**Loop.** Both Newton loops pass every `return_*` flag on every iteration
(`R/cpp_interface.R:473-482`, `R/estimation_core.R:218-230`); the default
`diagnostics = c("loglik", "scores")` sets `return_total_rate` and
`return_event_scores`. `has_unexpected_na()` (`R/cpp_interface.R:25-28`)
unlists the whole result, event scores included, per iteration.

**Already shared**: `flat_updates`, `broadcast_updates`,
`availability_updates.h` (pointer walk), `log_sum_exp`, `event_reductions`
(ranks, margins, availability, event scores, information accumulation),
`state_write.cpp` (`set_entries()`, `set_matrix_cells()`, used by the walk
only), and R mirrors of the reductions in `R/estimation_core.R:2522-2620`.

**Exact-time diagnostics (absorbed from `exact-time-diagnostic-cost`).**
The two exact-time kernels compute the raw rate vector `e` and its sum
`normalizer` for the likelihood, then, under a guard that includes
`return_total_rate` (set by the default `"loglik"` diagnostic), run a
second, max-shifted `exp` pass to obtain `log_normalizer` for the
conditional component (REM `:441-492`, rate `:324-366`), and fill
`probabilities = weights / shifted_total` unconditionally inside that block
(REM `:456`, rate `:341`). Commit `950f21b` added the shifted pass beside
the raw one to keep every frozen coefficient in place, a correct decision
about numbers taken without a cost measurement. Measured on Social
Evolution REM (median of three, pre-`7b56821` build): default 6.01 s against
4.72 s with `diagnostics = "scores"`; guarding the probability vector alone
gives 5.80 s. The rate kernel's guarded block also recomputes `lin_pred =
reduce_stat_mat * parameters` (`:336`) that the accumulation loop formed as
per-row dot products.

**Pins.** The living spec's floor is 1e-10 on per-event outputs for the six
families and 1e-6 on the frozen baselines; byte-identity is not required
(BLAS reorders sums). The `backend-primitive-parity` tests hold cpp, r and
gather to 1e-10 at a fixed parameter vector.

Constraints: the frozen baselines and goldens do not move; a branch writes a
`NEWS.d/` fragment (ADR-0040); the living spec's "Any FURTHER `src/`
micro-optimization SHALL be justified by a recorded profile showing a
measurable gain" is met by the before/after measurements each group records
and the budget that pins them; every `src/` diff passes the cpp-reviewer
and `cpp-recompile` before testing; `.claude/rules/walk-cost.md` applies to
every file touched.

## Goals / Non-Goals

**Goals:**
- No debug remnant, dead argument or identity copy at the likelihood site;
  every surviving comment explains a why.
- Per-evaluation allocation in every kernel; the risk set maintained in
  place; the r backend's state written in place; gather builders that
  neither grow nor copy per event.
- Diagnostics computed once at the returned parameters.
- A recorded estimation budget beside the walk budget.
- Every number identical within the living spec's floors.

**Non-Goals:**
- Changing the `diagnostics` vocabulary, its default, or the `"loglik"`
  mapping to `intervalLogL`, `total_rate` and the conditional component;
  this change makes that contract cheap, not different.
- Kernel argument shapes (`availability-one-object-per-loop` D1); this change
  starts after that one and reorganizes the loops it leaves.
- The once-per-evaluation `stat_mat = stat_mat_init` copy: state
  reconstruction every iteration needs it. Caching the reconstructed state
  across iterations is a future question, not taken here.
- Moving the r backend to C++ or changing its role as the reference engine.
- Unifying the three decode implementations (C++, r array, gather flat) or
  the six kernels' accumulation blocks into one template: a structural
  refactor with no cost claim, recorded as an open question.

## Decisions

### D1 — Debug remnants and dead inputs go; history markers become their reason or go

Commented-out traces and code (`src/DyNAM_rate_default.cpp:223, 267-278,
287, 295-296`; `R/estimation_core.R:254-256, 392-399, 481, 1151-1152,
1368-1374, 2146-2150, 2476, 2648`; `R/cpp_interface.R:540, 685-686`) are
deleted. The `impute` argument is removed from the six kernels, from
`make_engine_evaluator()`, `estimate_c_int()`, `gather_()` and the R
engine's equivalents, with its block; imputation is a preprocessing concern
(`nodal-imputation`) and no caller has set it since. `reduce_mat_to_vector()`
and `.gather_reduce()` are deleted and the rate kernels read `stat_mat`
directly; the unreachable `!twomode_or_reflexive` branch, which would divide
by zero, goes with them. `parallelize`, `cpus`, `verbose` (where unread),
`get_data_matrix` and `state$time` are removed from the signatures and
state they travel through. The `event_reductions.h` header is rewritten to
the convention the kernels follow (raw weights with `c = Dt` on the
exact-time path, probabilities on the multinomial path) and the REM
roxygen's duplicated and missing parameters are corrected.

History markers follow one rule, under Alvaro's instruction of 2026-10-01 to
clean the site: a marker that carries a reason (`CHANGED SIWEI: fixed errors
for composition change update`) is rewritten as that reason in present
tense with no name; a marker that records only that someone edited
(`CHANGED MARION`, `to be make`, `TO check(gutian): handle ignorant`) is
deleted, and where the `to be make` names an unfinished behavior (a NA time
interval, a two- or three-dimensional array) the task records whether the
behavior is still wanted in `progress.md`. Comments that explain a why are
never removed; the user-level rule against deleting explanatory comments
is honored by converting, not dropping, every marker that explains.
*Rejected:* deleting markers wholesale — loses the reasons; keeping them —
the request was to clean them.

### D2 — Kernels allocate per evaluation; the risk set is one buffer the availability lambda maintains

Every event-sized buffer (`lin_pred`, `weights`, `e`, `probabilities`,
`reduce_stat_mat`, the weighted copy for the Fisher product, `std::vector
sides`) is declared once before the event loop; Armadillo's `operator=`
does not reallocate when the size is unchanged, so `lin_pred = stat_mat *
parameters` into an existing vector reuses its memory, and
`log_sum_exp_masked()` is changed to fill a caller-owned `weights` of the
right size (`set_size()` only when it differs). The Fisher product uses a
preallocated `W` (`n1 * n2 x p`): `W = stat_mat.each_col() % e; fisher = W.t()
* stat_mat`, one buffer reused, no temporary. `event_score_row()` takes the
weighted sum the kernel formed (`weighted_sum_current_event`) instead of
recomputing `w' X`; the R mirror follows. The dyad index maps are built only
when `return_margins || return_availability`.

The risk set is one `allowed` buffer per evaluation: initialized from the
availability's initial value with the self-tie rule applied once, then
written by the same lambda `apply_availability_updates()` calls for the
availability — at the point encoding a flipped cell writes `allowed(i * n2 +
j)` directly (the diagonal cells are never set); at the outer encoding a
sender factor flip rewrites row `i` and a receiver factor flip column `j`
(both from the current factors); for the sender loop `allowed` is
`active_sender` itself. Masked zeroing is a plain pass, `for (k) if
(!allowed(k)) e(k) = 0`, keeping the exp-first order the kernel comment
explains (a masked overflowed row must become zero, not NaN; `e %= allowed`
would give NaN there). With `availability-one-object-per-loop` D1 landed,
the REM and coordination point path reads the grid alone, so `allowed` and
`active_dyad` differ only on the diagonal; the buffer is kept separate
rather than mutating the input, which is `const`.
*Rejected:* rebuilding `allowed` per event "because the GEMV is `n1 * n2`
anyway" — the kernel is order `n1 * n2 * p` per event regardless, but the
allocation churn of 27.5 MB vectors is measurable and the cost rules forbid
it; caching the reconstructed statistics across iterations — a different
change (Non-Goal).

### D2a — The exact-time diagnostics derive from the likelihood pass, with a guarded fallback (ADR-0065, implemented here)

In `estimate_REM` and `estimate_DyNAM_rate`, `log_normalizer =
log(normalizer)` and, when a consumer asks for it, `probabilities = e /
normalizer`, whenever `std::isnormal(normalizer)`: finite, strictly
positive and not subnormal, the conditions under which the raw ratio is
what the shifted pass computes to the last bits. (Amended in apply,
2026-10-02: the proposal's "finite and strictly positive" admits a
subnormal normalizer, every rate between about e^-745 and e^-708, where
e / normalizer keeps only a few significant bits; such a normalizer now
takes the fallback too.) Otherwise the kernel falls back to
`log_sum_exp_masked()` as today, so the overflow and subnormal-underflow
guarantee `950f21b` was written for is kept, paid only when needed. The
probability vector is formed only under `return_probabilities ||
return_margins || return_conditional_scores`, which the four
multinomial kernels already do; with D2 it is a hoisted buffer, filled
under that guard. The rate kernel's recomputed `lin_pred` goes with D2's
hoisting (its linear predictor is kept from the accumulation loop). The
fallback branch is exercised in a test through the same internal evaluator
seam D2's allocation counter and D4's evaluation counter use (an internal
argument on the R-side evaluator, not on an exported surface: the kernels
are `@noRd`), and the conditional component is asserted equal between the
two branches to 1e-10 at a fixed parameter vector.

The one predicted numerical consequence, stated before the run as ADR-0021
requires: the conditional component may move in its last bits on the fast
branch, because `log(sum(e))` and a max-shifted log-sum-exp are not
bit-identical; it is a diagnostic, enters no baseline, and is bounded by the
1e-10 parity floor. Coefficients, standard errors, the aggregate
log-likelihood and `intervalLogL` are computed from the raw pass and do not
move.
*Rejected (ADR-0065):* always the shifted pass — the cost being paid;
always the raw pass — reintroduces the underflow failure; making `"loglik"`
stop implying the conditional component — a user-visible contract change
to fix an implementation defect. *Rejected here:* keeping this in its own
change — it edits the two loops D2 rewrites, and its fit-level gate stops
discriminating once D4 lands (D6).

### D3 — The r backend writes its state in place

The per-event state becomes an environment created once per evaluation
(`new.env(parent = emptyenv())`), and every event-sized member is written
through the in-place writers: `set_entries()` for vectors and the flat
statistics array (linear index), `set_matrix_cells()` for cell matrices.
`apply_flat_update()` / `apply_broadcast_update()` (`R/utils.R`) write into
the array they are given through those writers and return nothing, so the
array is never duplicated; the same pattern `.gather_stat_cells()` already
documents. The diagonal is zeroed only on the diagonal cells among the cells
an event updated (the init's diagonal is zeroed once), which is `O(changes)`
instead of `p` slice copies. Risk-set subsetting keeps its staged form (the
living spec allows subsetting as staged linear algebra) but happens once per
event into a reused buffer sized to the current risk set, and `which()` is
called once per axis. The accumulators (`eventLogL`, `event_scores`,
`EventProbabilities`, margins, ranks, `total_rate`) are preallocated at
their final sizes before the loop and written through `set_entries()`;
`EventProbabilities` becomes a preallocated list written by position.
`event_contribution_rate()` receives the array already at the shape it
reads, so no `dim<-` on a shared value, and forms its two weighted products
into reused buffers. `accumulate_margins()` uses `rowsum(contribution,
slot)` (one pass). The coordination path replaces `apply(x, 1L, max)` with
`x[cbind(seq_len(n), max.col(x, ties.method = "first"))]` and its nine
`n^2 * p` temporaries with index arithmetic into two reused buffers (the
deviations and the symmetrized weights), keeping the vectorized form the
living spec requires.

The r backend is the reference engine; its per-event outputs are pinned to
1e-10 against the pre-change implementation on the six families and both
baseline datasets before the old code is deleted, as `likelihood-computation`
demands. *Rejected:* keeping the list and returning it (the copy is
structural, not incidental); porting the r backend to C++ (it exists to be
the readable reference).

*Amended 2026-10-03 (ADR-0113, task 7.1).* The "reused buffer" for the
risk-set subset above is not implementable in pure R (a subset is a fresh
copy), and the prediction below, that nothing of size `n1 x n2 x p` is
allocated per event, was made before a measurement showed that the
information itself, `crossprod(S, S * p)`, needs one such array per event
in R. As landed, the r backend's REM contract is: a REM event allocates
exactly one weighted statistics array plus vectors of the risk set's size,
at most `1 + 3/q` statistics arrays per event (`q` the array's columns,
`p + 1` on the exact-time REM with its intercept, `p` on the ordinal REM);
it never copies, reshapes or subsets the statistics array; an axis filter
at the outer encoding is applied as a `-Inf` mask on the whole grid (the
cells of the excluded rows and columns), so positions stay global and the
self-dyads stay on the diagonal; a point mask rides as its log (0 or
`-Inf`), maintained in step with the point availability and added to the
linear predictor inside the product's buffer; the probability vector and
the risk-set membership are built only when a per-event output that reads
them (probabilities, ranks, margins, availability) is requested; the
ordinal REM reads the flat statistics with `grid_dims` and forms its
information in moment form, `crossprod(S, S * p) - tcrossprod(E)`, score
`S[obs, ] - E`, never the centered derivative array; and the r backend
stays pure R. The coordination path keeps its grid reads (not a REM event).
*Amended again 2026-10-03 (ADR-0113 log, task 7.9).* The first landing
rebuilt the excluded rows' and columns' cells on every filtered event, so
an event's allocation grew with the share of actors absent (2.09 to 2.62
arrays from 10 to 200 of 300 absent; ordinal 3.37). The outer filter is
now a log mask maintained in place like the point one: at each sender or
receiver flip the step rewrites that row or column from the two factors,
and no event touches the excluded cells. Allocation per event is flat in
the share absent (2.08 to 2.10; ordinal 2.55 to 2.59). Time is that of the
full grid whatever the share: on the 300-actor sweep about 110 ms per
evaluation (ordinal about 80 ms) at 10 to 200 absent, against 155 to 22 ms
(356 to 18 ms) for the reduced grid at `237c8dd`, which is slower from
about 50 of 300 absent. Alvaro accepted that time to keep one mask path;
*rejected*: a hybrid that subsets when the risk set is small, and
reverting the filters to a subset.
*Rejected (ADR-0113):* a compiled weighted cross-product helper (676 MB,
but the reference stops being pure R and the information moves by 3e-8);
the minimal empty-buffer and rate-trim fixes alone (leaves the ordinal REM
at 9.8 arrays per event); amending the artifacts to the measured 4 arrays
without a code change.

### D4 — Diagnostics are evaluated once, after convergence

`run_nr_loop()` (both backends) evaluates with every `return_*` flag FALSE
except `return_interval_loglik` where the step acceptance reads it. After
the loop exits (converged, iteration limit, or a non-finite step that
returns the last accepted parameters), `estimate_int()` /
`estimate_c_int()` call the evaluator once more at the returned parameters
with the requested flags, and the fit's diagnostic components come from
that call. The kernel is deterministic at fixed parameters, so each
component equals what the per-iteration path stored; a test asserts it on
the six families. `has_unexpected_na()` inspects `logLikelihood`, `score`
and `informationMatrix` only. Cost: one extra evaluation without
diagnostics' share, against `k - 1` evaluations of every diagnostic; the
exact-time kernels' default-path tax measured by `exact-time-diagnostic-cost`
(27% on Social Evolution REM) is then paid once per fit.
*Rejected:* keeping the diagnostics in the loop and making each cheaper —
that is the sibling change's job and still pays `k` times; evaluating only
on the last iteration inside the loop — the loop does not know which
iteration is last until after it.

### D5 — Gather builders preallocate and write in place

The per-event candidate index is written into an integer vector
preallocated at the event's upper bound (`n_present * n2`) and trimmed; the
point presence grid is updated through `set_matrix_cells()`;
`.gather_broadcast_blocks()` returns index ranges without an `lapply`
closure, and the global kind writes a column without materializing
`seq_len(n1 * n2)`. The gather stack is byte-identical before and after.

### D6 — An estimation budget beside the walk budget, same discipline

`tests/testthat/_budgets/estimation_cost.csv`, recorded by
`record-estimation-cost.R`, checked by `test-likelihood_cost_budget.R`
under `NOT_CRAN=true`. Cells: Social Evolution REM, DyNAM choice and DyNAM
rate (84 actors, 439 events) and CollegeMsg REM on the first 1000 events
(1899 actors), each on cpp, r and gather. Metrics: `alloc_mb` of one fit on
r and gather (deterministic, ceiling 1.05 and floor 0.95 as the walk
budget), `seconds` per backend (ceiling 1.5, checked only with
`GOLDFISH_WALK_COST_SECONDS=true`), and `iterations` (exact, so a change in
the optimizer's path is seen). The compiled backend's allocation is off R's
heap; its time cells, the ratio below and D2's allocation-count scenario
cover it.

The diagnostics ratio ADR-0065 asked for is a cell of this budget, measured
between two evaluations in one session, one with the requested diagnostics
and one without, through the evaluator seam, not between two fits: after
D4 a fit pays the diagnostics once, so a fit-level ratio (default against
`diagnostics = "scores"`) would shrink from the measured 1.27 to about
`1 + 0.27 / k`, inside timing noise, and a regression of `950f21b`'s shape
would pass it. The evaluation-level ratio is the same self-normalizing
quantity ADR-0065 chose, at the level where the cost is paid; its ceiling
is recorded after D2a lands, with headroom for a structural regression, not
a drift. *Rejected:* a single time threshold — machine dependent, which is
why the walk budget gates seconds behind the environment variable; the
fit-level ratio — meaningless after D4.

*As landed (recorded 2026-10-03, task 7.5):* the CollegeMsg cell is not a
1000-event fit on three backends but one compiled REM evaluation on the
first 200 CollegeMsg messages over the full 1899-actor node set at a fixed
vector, timed with and without diagnostics (`college_rem_200_evaluation`
and its `diagnostics_ratio`), decided in apply because an r or gather fit
at that grid size takes minutes per budget run. The fit cells are the
Social Evolution ones only, so no budget cell is grid-dominated on the r
and gather backends; their per-event allocation on a large grid is held by
the generated-fixture cost guards (`test-likelihood_r_backend_cost.R`,
`test-gather_build_cost.R`).

### D7 — Detector-first per group, pins before deletions

Each group opens with its guard recorded failing on the unfixed code and
closes with the measurement against the before values in `progress.md`; the
per-event output pins (1e-10 on the six families, both datasets, each
backend) run before and after each group, and old code is deleted only once
the pins pass. The frozen baselines and goldens run at every commit.

## Storage and per-event cost

`n1`, `n2` sender and receiver counts, `p` parameters, `E` events, `k`
Newton iterations, `C` composition changes.

**D2 (kernels).** Stored objects: none new; the preprocessed object is
untouched. Per evaluation: the work buffers (`lin_pred`, `weights`, `e`,
`probabilities` when requested: `n1 * n2` doubles each; `W`: `n1 * n2 * p`;
`allowed`: `n1 * n2`) allocated once, about `(4 + p) * n1 * n2 * 8` bytes,
against today's per-event allocation of the same amount `E` times. Per
event: the pointer walks, the GEMV, the masked pass and the products, all
into existing memory; `allowed` changes in the flipped cells. Prediction on
an optimized build: Social Evolution REM cpp 0.58 s to about 0.45 s (the
`allowed` loop, `find()`, the Fisher temporary and the duplicated GEMV are
each a fraction of the `n1 * n2 * p` GEMM); CollegeMsg 1k REM, where the
grid is 3.6 M cells, the fall is larger, predicted 25-40%; multinomial
kernels change little (their buffers are `n2`). Allocation count in the
loop: zero.

**D3 (r backend).** Stored objects: none new. Per evaluation: the
environment, the accumulators at final size (`E`, `E x p`, `E` list slots),
the risk-set buffer at `n1 x n2 x p` (reused). Per event: writes of the
changed cells through `set_entries()`; one subsetting copy of the current
risk set into the reused buffer; the contribution's two weighted products
into reused buffers. Reads: the row, cells and counts by linear index on
the stored array, never a reshape of a shared value. Prediction: Social
Evolution REM r allocation 7325 MB to under 700 MB (one reused buffer set
plus the per-event subsetting), time 2.56 s to about 1.5 s; choice r 1148
MB to under 150 MB. The pins hold at 1e-10.
*As landed (amended 2026-10-03, ADR-0113):* the REM prediction was
refuted at 3290 MB (3.5 arrays per event, 4.7 under an outer-encoded
filter, 9.8 on the ordinal REM), and the contract is restated as one
weighted statistics array plus vectors of the risk set's size, at most
`1 + 3/q` arrays per REM event (`q` the array's columns), with no copy,
reshape or subset of the array; per evaluation the state also holds a
log point mask (`n1 x n2` doubles) when the REM object is point-encoded.
Measured on optimized builds against `237c8dd`, median of five: Social
Evolution REM r 3310 to 2035 MB and 1.00 to 0.88 s; REM-ordered r 6072 to
1436 MB and 2.03 to 0.69 s; DyNAM rate r 36.8 to 29.1 MB; choice
unchanged. On the cost guard's 300-actor fixture: 2.07 arrays per event
static, 2.01 under composition changes or an outer filter, 2.01 under a
flipping point mask, 2.52 ordinal (bounds 2 and 2.5 plus the
evaluation's fixed allocations).

**D2a (exact-time diagnostics from the likelihood pass).** Stored objects:
none new. Per event on the default path: one `exp` pass over the risk set
instead of two, and no probability vector unless a consumer asks; the
fallback pays the second pass only on a degenerate normalizer. Prediction:
on Social Evolution REM the default-diagnostics evaluation falls from 1.27
times the diagnostics-off evaluation to about 1.02 (the conditional
component's `log()` and its score reduction remain), and the probability
vector's 0.2 s disappears from every default fit.

**D4 (diagnostics once).** Per fit: one extra evaluation without the
diagnostics' share; `k - 1` evaluations lose the event-score GEMV, the
remaining conditional-component work, the probability vector, ranks and
margins. Prediction: Social Evolution REM cpp falls by a further 10-20% of
the pre-change time once D2a has removed the shifted pass (the two savings
overlap: D2a removes most of the per-evaluation tax, D4 the repetition of
what remains); r and gather similarly on their diagnostic share.

**D5 (gather builders).** Per event in the build: the index vector written
in place (`n_present * n2` upper bound, trimmed), the point grid updated in
its cells; today `idx` grows `n1` times per event (`O(n1^2 * n2)` copying)
and the grid is copied per event with changes. Prediction: Social Evolution
REM gather allocation 1823 MB to under 400 MB (the stack itself is `rows x
p` and stays).

**Guards.** D2: a count of event-sized allocations inside each kernel loop
(a debug counter compiled behind a macro in the test build, or `bench` on
the R wrapper for the R-visible part) recorded failing; the time cells of
the budget. D3: `bench::bench_memory()` of one `compute_step.default` call
on a 1500-actor generated fixture, bounded independent of `n1 * n2 * p`,
recorded failing (as landed: one r evaluation on a 300-actor fixture, in
statistics arrays per event, bounded by `1 + 3/q` plus 0.1 on the
exact-time, point-constrained and ordinal REM fixtures). D4: a counter of evaluations with diagnostics requested,
`k` before and 1 after. D5: `bench::bench_memory()` of one gather build on
the 1500-actor fixture against `rows x p` plus a constant.

## Risks / Trade-offs

- [A hoisted buffer is read before it is written in an edge path (zero
  candidates, first event)] → every buffer is written in full before its
  first read in each event (assignment, not accumulation), and the
  zero-candidate fixtures of the constraint tests run in the pins.
- [The in-place `allowed` drifts from the availability on an encoding edge
  (outer factor flip, diagonal)] → the "risk set follows the flips"
  scenario compares the maintained mask to a from-scratch mask on every
  event of the constrained and composition-change fixtures.
- [`set_entries()` on an R array shared with the caller silently mutates
  the caller's copy] → the environment's members are created fresh per
  evaluation (`stat_mat_init` copied once at entry, as the kernels do), so
  no binding outside the evaluation references them; a test asserts the
  input `prep` is `identical()` before and after a fit.
- [Evaluating diagnostics after the loop changes a stored component where
  the loop's last accepted step differed from the returned parameters] →
  the final evaluation is at the returned parameters by construction, and
  the equality test covers the non-converged exit paths.
- [The r-backend rewrite moves a number past 1e-10] → pins run before
  deletion, both datasets, six families; the frozen baselines at 1e-6 are
  the end-to-end floor.
- [The fast branch is taken on a fixture where the shifted pass mattered,
  and the conditional component silently degrades] → the guard is on
  `normalizer` itself, so the fallback triggers exactly when the raw value
  is not usable; the two-branch equality test pins both.
- [`conditional_logl` moves in its last bits and a downstream snapshot
  fails] → predicted in D2a before the run; the 1e-10 parity assertion
  decides whether a moved snapshot is the expected drift or a defect.
- [Removing `impute` breaks an external caller of the internal kernels] →
  the kernels are `@noRd` internals; the NEWS.d fragment names the removal.
- [Deleting a history marker loses a reason] → D1's rule converts every
  marker that carries one; the diff is reviewed marker by marker in the
  task.
- [The budget's seconds are noisy] → gated behind the environment variable
  as the walk budget is; allocation and iteration counts are the exact
  cells.

## Migration Plan

Commit per task on `feature_simulation`; `NOT_CRAN=true` green with
baselines and goldens PASS at each. Order: budget and pins first (D6, D7),
so every later group is measured → cleanup (D1) → kernels (D2, one kernel
per commit, cpp-recompile each) → diagnostics once (D4) → r backend (D3) →
gather builders (D5) → close. `status: landed (feature_simulation, awaiting
fold)` at the end; no archive on the branch.

## Open Questions

- The diagnostics ratio's ceiling: measured after D2a lands (task 2.5),
  recorded with its derivation, not guessed now.
- Whether the six kernels' accumulation blocks and the three decode
  implementations should later fold into shared templates; no cost claim,
  so not here.
- Whether the reconstructed statistics state can be cached across Newton
  iterations instead of replayed from `stat_mat_init` each evaluation
  (an `E x` saving on the update walk; a memory cost of one array per fit);
  deferred until the per-event waste above is gone and the budget shows
  what remains.

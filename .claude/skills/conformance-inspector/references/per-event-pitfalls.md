# Per-event pitfalls: how to tell a hit from a finding

`.claude/rules/walk-cost.md` is the authority on what per-event code may
do. This file is the inspector's side of it: how to decide whether a
scanner hit is on the per-event path, what it costs, and how to confirm it
with a measurement. Read the rule file first.

## 1. Is it per event?

A construct is per event when it executes once for each stored row, each
dependent event, each simulation step, or each Newton iteration times each
event. Trace the enclosing function upward until it reaches one of:

| Per-event root | Where |
| --- | --- |
| The merged walk's covariate and dependent steps | `R/preprocess_joint.R` (`merged_covariate_step`, the `run_merged_walk()` loop body, the consumers it calls) |
| Effect update closures | `R/functions_effects_*.R` `update_*` functions, called by the walk per update row |
| Constraint, mask and availability folds | `R/support_mask_maintain.R`, `fold_active_*` in `R/model_preprocess.R`, `apply_receiver_count_flips()` and kin |
| The simulation step | `R/simulate_*.R`, `R/process_state_evaluators.R`, the walk handle's `walk_step()` |
| The likelihood contribution | `compute_step.*` in `R/estimation_core.R`, re-run every Newton iteration |
| The gather loops | `.gather_*` in `R/cpp_interface.R` |
| The kernels' event loop | `for (id_event ...)` in `src/*_default.cpp`, `compute_poisson_selection.cpp` |

A function that is only called from `build_*`, `compile_*`, `prepare_*`,
`init_*`, `finalize_*`, `finish_output()` or a test is setup. A hit there
is a finding only when the design's cost section promised otherwise (for
example a seed that builds the grid when the design said two vectors).

Loops over *changes* (the entries that flipped, the receivers that moved)
are proportional to the change count, not to `n1` or `n2`, and are what
the rules ask for. A loop over `seq_len(n1)`, `seq_len(n_events)` or the
grid inside a per-event root is a rule 6 or rule 2 finding.

## 2. What each rule looks like in code

| Rule | Looks like | Why it costs | Confirm with |
| --- | --- | --- | --- |
| 1 one value per event | `out[[id_event]] <- matrix(...)`, `snapshots <- vector("list", n_events)`, a `lapply(seq_len(n_events), ...)` producing `n`-sized elements, a stored field whose length equals the event count | `E x n1 x n2`; 21.9 MB to carry 4 changed entries once | `lobstr::obj_size()` of the preprocessed object vs the budget; `length()` of the stored field vs the event count |
| 2 grid to read a row or cell | `matrix(x, n1, n2)[i, ]`, `dim(x) <- c(n1, n2)`, `support_to_grid()`, `project_value(.., to = 0L)`, `rep(v, each = n1)`, `outer()`, `rowSums(matrix(...))`, `apply(grid, 1, ...)` | 868 us and 13.8 MB per event against 9.8 us and 22 KB for the linear-index read; the constrained walk went 5 s to 15 s | `bench::mark()` of the step with `memory = TRUE`; the budget's alloc_mb |
| 3 copy on write | `get(name, envir)` then subassign then `assign()`, `x <<- `, a state matrix passed into a closure then written, an initializer returning its input network, `store$x[[i]] <- v` on an environment field | One adjacency copy per event, 27.5 MB; 118.8 s against 2.0 s | `tracemem(buffer)` across a walk prints duplications on **stdout** (a message-capturing test sees nothing); `Rprofmem()`; bench `n_gc` |
| 4 allocation per event | `data.frame()`, `as.data.frame()`, `rbind()`, `cbind()`, `do.call(rbind, ...)`, `append()`, `x <- c(x, ...)`, `x[[length(x) + 1]] <-`, `df[i, ] <-`, `which()` on an `n2` row, `logical(n2)` per dependent event, `arma::vec(...)`, `arma::zeros`, `std::vector` declared or `push_back` inside the event loop | An index data frame per sender was 74 percent of a replicate; an `n2` row per dependent event was 69 MB over 3000 events at 1500 actors; four logical grids per simulation step were 34 MB | allocation of one step (`bench::mark(..., iterations = 1)$mem_alloc`), the cost guard the task group had to write, `alloc_mb` before and after |
| 5 lookup by name per event | `match()`, `%in%`, `which(names(x) == )`, `x[["name"]]` with a string built per event, `paste0()`/`sprintf()` building keys, `match.arg()`, `eval(parse())` | Linear in the table per event; 4.5 ms per call in the schedule builder was the leftover of a whole group | time of the step before and after; a counter on the lookup |
| 6 second pass | A second loop over rows after the walk to accumulate what a running sum could have kept; `unique()`, `sort()`, `order()`, `unlist()` over per-row objects; a consumer with its own walk, buffer format or expansion instead of the shared registry | Two schedules over one event stream, 0.13 s of 0.29 s overhead at 1500 events | count the walks (`trace()` the schedule builder or the walk entry); count `build_event_schedule()` vs `build_joint_schedule()` calls |

## 3. Design against delivery

The design's **Storage and per-event cost** section states, per object,
its broadcast kind, its stored shape, its size in `n1`, `n2`, `E`, and the
per-event allocation. Check each statement against the object itself:

```r
prep <- preprocess_or_estimate_on(fixture)   # as the change's tests do
str(prep$<field>, max.level = 1)
dim(prep$<field>); length(prep$<pointer>)
lobstr::obj_size(prep); lobstr::obj_size(prep$<field>)
```

A field the design calls "two vectors" that is a matrix, a pointer whose
length is the event count when the design says one entry per flip, or an
object whose size moved opposite to the prediction, is a **blocks** gap
even when every test is green: the tests compare values, and a grid and a
pair of vectors expand to the same values.

When the design made no cost prediction and the range adds per-event code,
record that as a gap in the design (the `rules.design` entry in
`openspec/config.yaml` requires the section), owned by this change.

## 3b. Costs the scanner cannot see

Some per-event costs are decided by a flag or a shape, not by a construct
on the added lines. The grep finds nothing; the no-op measurement finds
them all.

| Pattern | What happened | How it was found |
| --- | --- | --- |
| An empty stream stored as a zero-length buffer instead of `NULL` | the R engine tests `!is.null(update)` to decide whether to filter the statistics array per event, so an outer-encoded constraint that excludes nothing filtered every event of every Newton iteration: +17 percent allocation on the r backend | constrained fit with an all-allowed mask against the unconstrained fit, same fixture, same iterations, `bench::mark()` allocation per backend |
| An encoding label that sends a consumer down its dense branch | a choice object labelled `outer` with no second factor; a reader relabels it and another builds the grid | `str()` of the stored object against the encoding the requirement defines |
| A guard measured on a static fixture | "reads no row per event" held for a mask that never flips; a point mask flipping every event still read two `n2` rows per dependent event | the guard's measurement re-run on a flipping variant |
| A per-event `which()` or index vector on an `n2` row | the row is never materialized, the index vector is | `Rprofmem()` grouped by function |

The no-op recipe: build the smallest fixture that separates one grid from
noise (300 actors is enough), fix the Newton iteration count and starting
values so every backend evaluates the likelihood the same number of
times, and run unconstrained against a constraint that excludes nothing
(`~ alter(allow) > 0` with `allow` all ones, `~ ego(x) >= 0`), once per
backend (`r`, `cpp`, `gather`) and once for the walk. Equal allocation is
the expected answer; any difference is the cost of the new path and goes
in the report's cost table.

## 4. Measuring

- Use a fixture large enough to show the cost. At 84 actors a grid is
  56 KB and a per-event copy does not separate from noise. The budget
  fixture (`tests/testthat/fixtures/college_msg_5000.txt`, 1899 actors)
  and `tests/testthat/helper-walk-cost.R` give `measure_walk_cost()`;
  `.plan/sp/` holds earlier timing scripts and their `.rds` references.
- Compare warmed medians, allocations as well as time, on an optimized
  build (`load_all(recompile = TRUE, debug = FALSE)`; a plain `load_all`
  compiles at `-O0` and makes C++ paths look slow).
- Measure before and after in the same session, tip to tip in the
  worktree, so machine drift lands on both sides.
- A per-event cost paid in `compute_step` is paid once per Newton
  iteration; multiply by the iteration count before calling it small.
- Report the numbers in the report's cost table beside the design's
  prediction and the journal's claim, and say which of the three disagree.

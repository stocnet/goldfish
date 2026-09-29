---
paths:
  - "R/preprocess_*.R"
  - "R/model_preprocess*.R"
  - "R/support_mask*.R"
  - "R/broadcast_kind.R"
  - "R/walk_handle.R"
  - "R/simulate_*.R"
  - "R/process_state_evaluators.R"
  - "R/event_streams.R"
  - "R/state_at.R"
  - "R/cpp_interface.R"
  - "R/estimation_core.R"
  - "R/functions_effects_*.R"
  - "src/*.cpp"
  - "src/*.h"
---

# Walk cost rules

Rules for any code that runs once per event: the preprocessing walk, the
simulation step, the effect functions, the constraint and availability
folds, and the R gather loops. Each rule below was broken, fixed, and then
broken again in a later change, and every time it was found by a benchmark
or by review, never by a test. The frozen 1e-6 baselines cannot see any of
them: a slower or larger walk returns the same numbers.

`n1`, `n2` are the sender and receiver counts. A grid is an `n1 x n2`
object. On the 1899-actor CollegeMsg data one logical grid is 13.8 MB.

## 1. Store an initial value and its changes, never one value per event

A preprocessed output is three things: the initial value, a flat update
buffer written by the writer rules, and a cumulative per-event pointer into
that buffer. Values that are constant along an axis are stored at their
broadcast kind (scalar, per sender, per receiver) and expanded at the read.

- Do not store a list indexed by event whose elements are matrices or
  vectors of size `n1`, `n2` or `n1 x n2`.
- Do not store at the grid what a vector or a scalar describes.
- A new consumer (statistic, interaction operand, constraint atom, support
  mask, availability) uses the shared functions in `R/broadcast_kind.R`
  and the existing writers. It does not bring its own walk, its own buffer
  format, or its own expansion.

Measured when broken: a support mask stored per event took 21.9 MB to
carry 4 changed entries, and the dense form exhausted 24 GB.

## 2. Read what you need at its stored kind

Reading one row, one cell, or a count does not justify building the grid.

- Do not call `matrix()`, `array()`, `dim<-`, `support_to_grid()` or
  `project_value(..., to = 0L)` to then subset the result.
- Row `i` of a column-major `n1 x n2` vector is
  `x[i + n1 * (seq_len(n2) - 1L)]`. Cell `(i, j)` is `x[i + n1 * (j - 1L)]`.
  Row sums are `.rowSums(x, n1, n2)` on the stored vector.
- A value stored per receiver already is every sender's row.

Measured when broken: reshaping a dyadic mask to read one sender's row cost
868 us and 13.8 MB per event against 9.8 us and 22 KB, and took the
constrained walk from about 5 s to about 15 s.

## 3. Write state in place

R copies a whole object when a modified object has a second reference.

- Write through `write_entries()`. Do not write with `<<-`, with
  `assign()`, or by `get()`, subassign, `assign()`.
- A state buffer lives in exactly one binding. An initializer returns a
  fresh object, never its input network.
- Do not pass a state matrix through a closure call and then subassign it:
  the call leaves a reference behind and the write copies.

Measured when broken: one adjacency copy per event (27.5 MB) made the rate
loop 118.8 s against 2.0 s.

## 4. Allocate outside the loop

- No `data.frame()`, `as.data.frame()`, `rbind()`, `cbind()`,
  `do.call(rbind, ...)`, `append()`, or `x <- c(x, ...)` in per-event code.
  This includes functions that contain no loop but are called once per
  event.
- Collect into preallocated columns that double in capacity, as the
  writers and the simulation collector do, and bind once at the end.
- Row assignment into a data frame (`df[i, ] <- ...`) is also per-event
  allocation.

Measured when broken: an index `data.frame` built and discarded per sender
was 74% of a simulated replicate.

## 5. Resolve once, before the walk

Routing, positions, kinds, effect plans and object lookups are resolved
when the walk context is built. Nothing is looked up by name per event.

## 6. One walk, one pass

Anything that needs the state over time registers on the shared walk. A
quantity that can be accumulated as a running sum during the walk is not
computed by a second pass, and per-row objects are not materialized to be
summed afterwards.

## The cost budget

`tests/testthat/test-walk_cost_budget.R` runs one preprocessing walk on the
first 5000 CollegeMsg messages, without and with a dyadic support
constraint, and compares it with `tests/testthat/_budgets/walk_cost.csv`:

| Measured | Fails above |
|---|---|
| Memory allocated by the walk | recorded + 5% |
| Size of the preprocessed object | recorded + 2% |
| Constrained over unconstrained time, same session | 1.5 |
| Median seconds, only with `GOLDFISH_WALK_COST_SECONDS=true` | recorded + 50% |

It runs under `NOT_CRAN=true`. A failure means the walk costs more than it
did: find the cause before considering the number. Raising a recorded value
or a limit needs the maintainer's approval, and the commit that does it
says what the extra cost buys. After an improvement, lower the recorded
value in the same commit so the gain cannot be given back unnoticed.
`Rscript tests/testthat/_budgets/record-walk-cost.R` prints the current
measurements and writes nothing.

## Reference costs

What one addition costs on the budget fixture (1899 actors, 5000 events,
choice sub-model, against `choice = ~ inertia` at 28.0 MB and 228 MB
allocated). Use these to predict the cost of a design before implementing
it. Recorded 2026-09-29.

| Added | Object size | Allocated per event |
|---|---|---|
| A dyadic effect (`recip`, windowed or not) | +27.5 MB | +0.03 MB |
| A degree effect (`indeg`, `outdeg`) | +27.5 MB | +0.20 MB |
| A closure effect (`trans`) | +27.9 MB | +8.63 MB |
| An interaction of two dyadic effects | +27.5 MB | +0.03 MB |
| A degree constraint (`~ indeg(msg) >= 0`) | +0.0 MB | +0.21 MB |
| A dyadic constraint (`~ !tie(msg)`) | +27.6 MB | +0.18 MB |

One `n1 x n2` slice of doubles is 27.5 MB and one logical grid is 13.8 MB.
Every effect takes one slice of the initial statistics. A dyadic constraint
takes two logical grids. A new object that adds more than one slice, or
allocates more than about 0.2 MB per event, needs a reason. The closure
effect's allocation is a known defect, not a precedent.

`object.size()` counts an object shared by two sub-models twice;
`lobstr::obj_size()` does not.

## Verifying a change to per-event code

- Measure on a fixture large enough to show the cost. At 84 actors a grid
  is 56 KB and a per-event copy does not separate from noise.
- Compare warmed medians, and allocations as well as time. `Rprof` loses
  samples on macOS, so read proportions from it, not totals.
- `tracemem()` reports on stdout. A test that captures messages to detect
  a copy cannot fail.
- A guard test is written first and seen to fail on the unfixed code.

## Context

A pooled sequence's log-likelihood has a θ-free part, contributed by its
fixed-only sub-models (all-offset fids, completed uniform choices, pinned
rates). `pool_fixed_loglik()` (`R/evaluator_dynes.R`) computes it once per
sequence, and is reached only through `new_pool_entry()`, which has two
callers:

- The pool builders (`new_sequence_pool()`, `append_sequence_pool()`, via
  `pool_entries()`), which store it as `fixed_loglik` / `fixed_degenerate`.
  `evaluate_pool_sequence()` starts every later evaluation from it. When
  `fixed_degenerate` is set, the free sub-models are skipped and the result is
  `−Inf` / `NA` / `NA` / `degenerate = TRUE`.
- `evaluate_candidate()` (the proposal evaluator's single-candidate path, and
  the engine of the exported `compute_lik_seq()`), once per candidate.

Today `pool_fixed_loglik()` flags degenerate only inside its `tryCatch`
error handler. A term that the engine *returns* as `−Inf` is summed in, and
the function returns `list(loglik = −Inf, degenerate = FALSE)`. The concrete
case is `pinned_rate_loglik()` with a `−Inf` plateau in a period that holds
events: `sum(event_plateau)` is `−Inf`, and the compensator `e^{−Inf}·x` is
`0`.

`new_pool_entry()` also sets `fixed_degenerate = TRUE` when the walk failed
(`stats = NULL`), so the flag today conflates two causes.

What this breaks downstream, observed in the current code:

- `evaluate_pool_sequence()` evaluates the free sub-models anyway and returns
  their finite score and Fisher information beside a `−Inf` log-likelihood.
- `sandwich_covariances()` keeps rows by `weights > 0 & !degenerate`. A `−Inf`
  row survives only through its zero weight. A `NaN` row is not reliably zero
  after softmax.
- The pool's `degenerate` vector, the one diagnostic that says how many
  sequences the pool lost, undercounts.
- `compute_lik_seq()` reports such a sequence as healthy.
- When a bad pin degenerates the whole pool, nothing points at the pin, and
  the run ends badly either way: under importance weighting every weight is
  0, so Q = ASE = 0 meets the stop rule and the fit reports
  `converged = TRUE` at a θ it never moved; under uniform weighting Q is NaN
  and the loop aborts with retries exhausted (see D6).

## Goals / Non-Goals

**Goals:**

- Any non-finite fixed-only term makes the sequence degenerate, through the
  same path an engine error already takes.
- Fix it once, in the function both callers share, so pool entry and the
  proposal evaluator cannot disagree.
- Make a fixed-term degeneracy visible as such in an `estimate_dynes()` run:
  one classed warning naming the sub-models whose term failed, and a per-pass
  count in `em_trace`.
- Never report a run as converged when every sequence of its pool was dropped
  for a fixed term: stop it early with `converged = FALSE`, on both weighting
  paths.

**Non-Goals:**

- Removing the cause. A `−Inf` pin comes from the completion side reading
  wave snapshots one period late; `dynes-augmentation` task 2.2f fixes that.
  This change only makes the evaluator honest, and the run loud, when a
  non-finite term arrives by any route.
- Fixing a *partly* degenerate pool on the uniform-weighting path. Uniform
  weights (MCMC + refresh) ignore `degenerate`, so a dropped sequence's
  `lambda` is `−Inf − (−Inf) = NaN`, Q is NaN, and the loop aborts with
  retries exhausted. This predates this change (a walk failure or a singular
  information does the same) and is recorded as `abmcem` follow-up 5. D6
  covers only the *fully* fixed-term-degenerate pool, the case a bad pin
  produces.
- Any change to the free sub-model path (`pool_cpp_eval()` already aborts on
  a non-finite log-likelihood, which the caller's `tryCatch` turns into
  degenerate).
- A warning from `compute_lik_seq()`: it returns the `degenerate` flag to the
  caller, which is the report.

## Decisions

### D1 — Check the term inside `pool_fixed_loglik()`, not at the callers

Each term is tested with `is.finite()` right after it is computed. A
non-finite term takes the existing early return
`list(loglik = −Inf, degenerate = TRUE)`, the same as an error. The early
return is per term, so later fixed-only sub-models are not scored for nothing.

*Alternatives.* Checking the sum in `new_pool_entry()`'s callers would need
the same check in `evaluate_candidate()`, which is exactly the drift this
change removes. Checking it in `evaluate_pool_sequence()` covers pool
sequences only, and runs on every evaluation instead of once at entry.

### D2 — Normalize every non-finite value to `−Inf`

`+Inf`, `NaN` and `NA` all become `loglik = −Inf`, not the value as computed.
A degenerate sequence must have zero weight, and only `−Inf` guarantees that
through `stable_softmax()`. `+Inf` would take the whole weight, and `NaN`
would poison the normalizer. This matches what the error path and the free
sub-model path already return.

A pinned rate cannot produce these values today: `make_intercept_only_rate()`
already rejects `+Inf` and `NA` plateaus. D2 is a defense against other
routes (an all-offset fid, a future fixed-only kind), so it is tested by
mocking `fixed_only_loglik()` rather than through a real pin.

### D3 — A pool entry records its degeneracy cause

`pool_fixed_loglik()` returns `list(loglik, degenerate, cause, label)`, with
`cause = "fixed_term"` on both its non-finite and its error branch and
`NA_character_` otherwise, and `label` the failing sub-model's `label` (the
rendered process label `decompose_joint_spec()` attaches; `NA_character_`
when nothing failed). `new_pool_entry()` sets `cause = "walk"` on its
`stats = NULL` path and stores the values as `degenerate_cause` and
`degenerate_label` beside `fixed_degenerate`. The boolean stays, so
`evaluate_pool_sequence()` and every other reader are unchanged.

The label is recorded because a fixed-only term is not always a pinned rate:
an all-offset fid or a completed uniform choice is one too, and an engine
error on one of those has nothing to do with pins. Naming the sub-model lets
the warning (D4) say which term failed instead of guessing.

`degenerate_cause` and `degenerate_label` are coordinator-side only: they are
not added to the resident store's field list in `pool_store_mutate()`,
because no worker reads them.

*Alternatives.* A single generic warning keyed on `fixed_degenerate` would
blame the pin for walk failures. Replacing the boolean by the cause string
would touch every reader for no gain.

### D4 — A cumulative tally and one end-of-run warning

The pool store keeps a running tally, updated in the coordinator after every
`pool_entries()` call (both in `new_sequence_pool()` and
`append_sequence_pool()`): entries built, entries whose `degenerate_cause` is
`"fixed_term"`, and the distinct `degenerate_label`s among the latter.
`N` counts every drawn sequence across the run, including those a redraw
later replaced, so the message says "drawn sequences", not "pooled
sequences", to keep it from reading against the trace's per-pass
`pool_size`.

The warning is raised by a helper that takes the store (so it can be
unit-tested with a cli snapshot), called from `estimate_dynes()` between
`run_abem_loop()` and `new_dynes_fit()`, where `store` is in scope. When the
fixed-term count is positive it warns once with class
`goldfish_dynes_fixed_term_degenerate`:

- `!` "{n} of {N} drawn sequences had a non-finite or unevaluable fixed-only
  log-likelihood in {labels} and were dropped as degenerate."
- `!` (only when D6 stopped the run) "Every sequence in the pool was dropped;
  estimation stopped at the starting values and the fit is flagged
  {.code converged = FALSE}."
- `i` "This is likely a bug in goldfish; please report it."

The report-a-bug wording is deliberate. Every fixed-only term is derived
from the specification and the data by goldfish itself: a pinned rate is
pinned on the same wave grid the augmenter draws on (made an invariant by
`dynes-augmentation` 6.1b), so a non-finite term is not something the user
can fix by changing an argument. Pointing them at `wave_times` would send
them hunting.

One warning per run, with a count that separates a spec-wide problem (all
sequences) from a stray one.

*Alternatives.* Warning on every build or append would repeat at every
redraw. Warning once on first occurrence loses the scale. A
"check your `wave_times`" hint blames the user for a bug they cannot cause.

### D5 — `em_trace` gains a `fixed_degenerate` column

Each pass row records the number of sequences in the *current* pool whose
`degenerate_cause` is `"fixed_term"`, as an integer column placed after
`pool_size` in `abem_trace_tibble()`. The warning (D4) gives the run total,
and the column shows when it started (first build, or only after a redraw).
`em_trace` is the fit's existing diagnostics slot, so no new fit field is
needed.

### D6 — A fully fixed-term-degenerate pool stops the run with `converged = FALSE`

After every pool build and every append in `run_abem_loop()` (the initial
pool, a redraw, and each retry's growth), and **before** weights, SGD and Q,
the loop checks whether every sequence in the current pool is degenerate
with `degenerate_cause == "fixed_term"` (the D5 count equals the pool size).
If so, it returns at once: `abem_loop_result(theta_k, converged = FALSE,
...)`, `theta_k` being the last accepted parameters (the starting values when
it trips on the first pool). The run then returns normally to
`estimate_dynes()`, so the D4 warning fires, with its extra bullet.

Why it must sit before Q, not at the end: the two weighting paths fail
differently on such a pool, and neither reaches a usable end state.

- Importance weighting: softmax sends every `−Inf` to weight 0, so Q = 0 and
  ASE = 0, the stop rule `|Q + z·ASE| < tolerance` holds, and after
  `stop_count` passes the loop returns `converged = TRUE` at a θ it never
  moved. A flag set afterwards would have to overrule the stop rule.
- Uniform weighting (MCMC + refresh): weights stay 1/n, `lambda` is NaN, Q is
  NaN, nothing is ever accepted, and the loop aborts with
  `abort_retries_exhausted()` before returning, so no end-of-run flag or
  warning is reachable.

A check at pool build is the one place both paths still pass through.

A trip on the very first pool leaves `em_trace` with zero rows; the fit
constructor and its covariance must accept that (the sandwich already
returns all-`NA` matrices when no sequence is kept).

*Alternatives.* Aborting would discard a fit the user can still inspect
(trace, pool, spec), and the warning already carries the cause. Setting
`converged = FALSE` only when the loop ends fixes the importance path but not
the uniform one.

## Risks / Trade-offs

- **[A sequence that is today weighted ~0 but non-degenerate becomes
  degenerate]** → Its weight was already zero (`−Inf` log-likelihood), so
  every weighted quantity is unchanged. Only the `degenerate` flag and the
  `NA` score/Fisher change, which is the point.
- **[`compute_lik_seq()` output changes]** → Only for a sequence whose
  fixed-only term is non-finite, which was already `loglik = −Inf`. Recorded
  in its `@details` and the NEWS fragment.
- **[New `em_trace` column]** → The `em_trace` snapshots change; the update
  is intentional and reviewed in the task that adds the column.
- **[Partly degenerate pool, uniform weighting]** → Still aborts with
  retries exhausted (NaN Q); D6 does not cover it. Rare on MCMC, since the
  chain rejects `−Inf` candidates and stays degenerate only if it starts
  there. Tracked as `abmcem` follow-up 5.
- **[Early stop at the starting values]** → The fit carries `theta0` with
  `converged = FALSE` and a warning naming the failing sub-models. That is
  intended: the run had no information to move on.
- **[Delta written against an unfolded requirement]** → The `## MODIFIED`
  header exists only in `abmcem`'s `## ADDED` delta, so
  `opsx-spec-placement-check.sh` fails until `abmcem` folds. That is the
  declared `depends-on` deferral. At the `feature/dynes` merge, `abmcem` is
  archived first, then this change.

## Migration Plan

None. Internal pool structures only, plus one documented behavior change in
`compute_lik_seq()` and one new `em_trace` column. Pools built before the fix
carry `fixed_degenerate = FALSE` for such sequences, but pools are not
persisted across runs.

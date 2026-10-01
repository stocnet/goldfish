## Context

A pooled sequence's log-likelihood has a θ-free part, contributed by its
fixed-only sub-models (all-offset fids, completed uniform choices, pinned
rates). `pool_fixed_loglik()` (`R/evaluator_dynes.R`) computes it once per
sequence, and two callers store or use the result:

- `new_pool_entry()` stores it as `fixed_loglik` / `fixed_degenerate`, and
  `evaluate_pool_sequence()` starts every later evaluation from it. When
  `fixed_degenerate` is set, the free sub-models are skipped and the result is
  `−Inf` / `NA` / `NA` / `degenerate = TRUE`.
- `evaluate_candidate()` (the proposal evaluator's single-candidate path)
  calls the same function once per candidate.

Today `pool_fixed_loglik()` flags degenerate only inside its `tryCatch`
error handler. A term that the engine *returns* as `−Inf` is summed in, and
the function returns `list(loglik = −Inf, degenerate = FALSE)`. The concrete
case is `pinned_rate_loglik()` with a `−Inf` plateau in a period that holds
events: `sum(event_plateau)` is `−Inf`, and the compensator `e^{−Inf}·x` is
`0`. An infinite plateau with exposure gives `Inf − Inf = NaN`.

What this breaks downstream, observed in the current code:

- `evaluate_pool_sequence()` evaluates the free sub-models anyway and returns
  their finite score and Fisher information beside a `−Inf` log-likelihood.
- `sandwich_covariances()` keeps rows by `weights > 0 & !degenerate`. A `−Inf`
  row survives only through its zero weight. A `NaN` row is not reliably zero
  after softmax.
- The pool's `degenerate` vector, the one diagnostic that says how many
  sequences the pool lost, undercounts.

## Goals / Non-Goals

**Goals:**

- Any non-finite fixed-only term makes the sequence degenerate, through the
  same path an engine error already takes.
- Fix it once, in the function both callers share, so pool entry and the
  proposal evaluator cannot disagree.

**Non-Goals:**

- Removing the cause. A `−Inf` pin comes from the completion side reading
  wave snapshots one period late; `dynes-augmentation` task 2.2f fixes that.
  This change only makes the evaluator honest when a non-finite term arrives
  by any route.
- Aborting the run when *every* sequence is degenerate. A pin is set on the
  specification, so a bad one hits the whole pool. The existing D5 ESS guard
  and the fully degenerate pool path already handle that case; whether it
  should become a hard error is a separate question.
- Any change to the free sub-model path (`pool_cpp_eval()` already aborts on
  a non-finite log-likelihood, which the caller's `tryCatch` turns into
  degenerate).

## Decisions

### D1 — Check the term inside `pool_fixed_loglik()`, not at the callers

Each term is tested with `is.finite()` right after it is computed. A
non-finite term takes the existing early return
`list(loglik = −Inf, degenerate = TRUE)`, the same as an error. The early
return is per term, so later fixed-only sub-models are not scored for nothing.

*Alternatives.* Checking the sum in `new_pool_entry()` would need the same
check in `evaluate_candidate()`, which is exactly the drift this change
removes. Checking it in `evaluate_pool_sequence()` covers pool sequences only,
and runs on every evaluation instead of once at entry.

### D2 — Normalize every non-finite value to `−Inf`

`+Inf`, `NaN` and `NA` all become `loglik = −Inf`, not the value as computed.
A degenerate sequence must have zero weight, and only `−Inf` guarantees that
through `stable_softmax()`. `+Inf` would take the whole weight, and `NaN`
would poison the normalizer. This matches what the error path and the free
sub-model path already return.

## Risks / Trade-offs

- **[A sequence that is today weighted ~0 but non-degenerate becomes
  degenerate]** → Its weight was already zero (`−Inf` log-likelihood), so
  every weighted quantity is unchanged. Only the `degenerate` flag and the
  `NA` score/Fisher change, which is the point.
- **[Delta written against an unfolded requirement]** → The `## MODIFIED`
  header exists only in `abmcem`'s `## ADDED` delta, so
  `opsx-spec-placement-check.sh` fails until `abmcem` folds. That is the
  declared `depends-on` deferral. At the `feature/dynes` merge, `abmcem` is
  archived first, then this change.

## Migration Plan

None. Internal function, no API or stored-object change. Pools built before
the fix carry `fixed_degenerate = FALSE` for such sequences, but pools are
not persisted across runs.

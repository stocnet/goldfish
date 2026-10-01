---
depends-on: abmcem
status: landed (feature/dynes, awaiting fold)
---

## Why

`abmcem`'s `make_proposal_evaluator()` (`R/evaluator_dynes.R`) ships a
`rate_queries` argument that today does nothing but `cli_abort()`, naming the
capability as not yet implemented (task 4.3b, 2026-09-21). `dynes-augmentation`
design D20 needs exactly what that abort names: `augment_seq_mcmc()`'s
`permute` move computes its reverse-proposal density `q_rev` from "the moved
pair's frozen rate under Ω′" — the model's instantaneous intensity for a
specific (sender, receiver) pair at a specific position in a *candidate* (not
yet accepted) sequence. Without it, `augment_seq_mcmc()` cannot compute its own
acceptance ratio and the MCMC augmenter is unbuildable. This blocks
`dynes-augmentation` task 3.2/3.4 downstream, wherever they land.

Almost everything this needs already exists. The walk-handle substrate
(`multi-process-walk`) evaluates any fid at a live process state and names
"the DyNES augmenter" as a consumer; the batch-vs-replay test oracle already
replays a schedule through it in batch-consistent order; the simulation driver
already evaluates completed fids and composes a DyNAM pair's intensity as rate
× choice; and `coef_layout()` already maps a flat parameter vector onto fids.
What is missing is the thin layer that binds a candidate sequence to a
completed specification, replays it to a requested position, and reads named
pairs there — and the wiring into `make_proposal_evaluator()`.

## What Changes

- **A schedule-position replay driver** (`walk_replay_to()`), promoted from the
  batch-vs-replay test oracle's loop into `R/`, which the oracle then calls. It
  steps the candidate's own schedule by row position, so stamp ties fold in the
  batch path's order.
- **A candidate binder** (`bind_candidate_data()`): rebinds a completed joint
  specification to a candidate stocnet without re-running construction or
  completion, after checking the candidate's `info` and `nodes` are identical.
- **A pair-intensity reader** (`pair_intensity()`) over the simulation driver's
  evaluation steps: DyNAM pairs as rate × choice probability, REM dyads as their
  rate, completed sub-models evaluated rather than refused.
- **A query entry point** (`query_rate_at_position()`): answers a batch of
  `(position, process, sender, receiver)` queries from one opened handle in one
  forward pass, positions counted in candidate events.
- **Wire `make_proposal_evaluator()`'s `rate_queries` argument** (abmcem): a new
  optional `spec` argument (the completed joint specification), a construction
  check that the sub-models' parameter positions match the spec's coefficient
  layout, and the abort replaced with real computation, honoring the
  named-list `(loglik, rates)` contract abmcem's design D3 already pinned.
- **No changes** to `walk_open()`/`walk_advance()`/`walk_evaluate()`/
  `walk_inject()` (`multi-process-walk` is consumed read-only) and none to
  `augment_seq_mcmc()` itself.

## Capabilities

### New Capabilities

- `rate-queries`: binding a candidate event sequence to a completed
  specification, replaying it through the walk-handle substrate to a requested
  position, and reading the frozen intensity of named (process, sender,
  receiver) pairs there — the machinery `make_proposal_evaluator()`'s
  `rate_queries` argument needs and currently lacks.

### Modified Capabilities

*(none — `multi-process-walk`'s requirements are consumed, not changed;
`abmcem-optimization` carries no requirement text for `make_proposal_evaluator()`
yet to modify, since abmcem's own task 4.3 scoped it as shape-only and wrote no
spec delta for it)*

## Impact

- **R**: new `R/rate_queries.R` (replay driver, candidate binder, theta
  mapping, pair reader, query entry point); `R/evaluator_dynes.R` (abmcem) —
  `make_proposal_evaluator()` gains `spec = NULL` and its `rate_queries` branch
  is replaced. Reads, unchanged: `R/simulate_driver.R` evaluation steps,
  `coef_layout()`.
- **Tests**: `tests/testthat/test-walk_handle.R`'s `walk_replay_against_batch()`
  calls the promoted driver; new `tests/testthat/test-rate_queries.R`;
  `test-evaluator_dynes.R`'s abort-pinning test and snapshot are replaced.
- **Depends on**: `abmcem` (un-archived, actively edited — this change amends
  `make_proposal_evaluator()`'s body and adds one optional argument, not its
  pinned return contract) and the landed `multi-process-walk` capability.
- **Unblocks**: `dynes-augmentation`'s `augment_seq_mcmc()` permute and shift
  moves (design D20).
- **No `src/` changes** — everything builds on pure-R substrate.

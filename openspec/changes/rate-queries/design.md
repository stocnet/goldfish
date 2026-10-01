## Context

`abmcem`'s `make_proposal_evaluator()` (`R/evaluator_dynes.R:make_proposal_evaluator`)
returns a closure `eval_fn(candidate_seq, rate_queries = NULL)`. When
`rate_queries` is non-`NULL` it currently `cli_abort()`s, naming the capability
as not yet implemented — a deliberate, honest stub (abmcem task 4.3, design D3):
"a real `rate_queries` request aborts naming what is missing rather than
returning a fabricated value under that name." `dynes-augmentation` design D20
names the caller and the need precisely: `augment_seq_mcmc()`'s `permute` move
needs "the moved pair's frozen rate under Ω′... emitted as a byproduct of the
same pass that computes f(Ω′)" to compute its reverse-proposal density q_rev,
where the rate is "frozen at the process state after the preceding PE".
`shift`'s ratio needs the analogous quantity for its own moved slot.

Two capabilities already exist and were built with this exact need in mind:

- **`process-state-evaluators`** (living spec) — `evaluate_process_state(state,
  parameters)` (`R/process_state_evaluators.R`) returns, per sub-model family,
  the FULL per-actor (rate/rate-ordered) or per-dyad (REM/REM-ordered/choice)
  vector at a materialized process state, with excluded alternatives as exact
  zeros, labeled by the shared `index_i`/`index_j` vocabulary.
- **`multi-process-walk`** (living spec) — the stepping walk handle
  (`R/walk_handle.R`): `walk_open()`, `walk_advance()`, `walk_inject()`,
  `walk_evaluate()`. The file's header names "the DyNES augmenter" as an
  intended consumer. `simulate_replicate()` (`R/simulate_driver.R`) is the
  existing consumer.

A survey of the package (2026-09-23) found that most of what the first draft of
this design proposed to build already exists, and that three of the drafted
pieces would have been incorrect as specified:

| Needed | Already in the package | Consequence for this design |
|---|---|---|
| a replay loop over a given sequence | `walk_replay_against_batch()` (`tests/testthat/test-walk_handle.R`) — schedule-position driven, proven equal to the batch path | D2 promotes that loop instead of writing a time-driven one (the time-driven loop mis-orders stamp ties) |
| the candidate events as walk events | the candidate stocnet's own schedule, once a walk is opened over it (`handle$schedule`, integer-indexed) | no separate event list, no shape adapter (D2, D6) |
| evaluating any fid, completed ones included | `simulation_fid_state()` / `evaluate_fid()` / `fid_evaluation_inputs()` / `simulation_routing()` (`R/simulate_driver.R`) | `walk_evaluate()` alone refuses deferred fids (D3) |
| a DyNAM pair's intensity | `mark_evaluation()` + `paired_choice_fid()` — rate_i × p(j given i), the choice evaluated for the one sender (`walk_evaluate(..., sender =)`) | one fid's entry is half the quantity (D3) |
| flat theta → per-fid theta | `coef_layout.goldfishJointSpec()` — per-fid slots, free-slot `index` into the flat vector, fixed values | D7 |
| a walkable spec per candidate | nothing — `make_proposal_evaluator()` holds formula-level `sub_models`, not a spec | D6 (new, small) |

## Goals / Non-Goals

**Goals:**
- A primitive that replays a candidate event sequence through the walk-handle
  substrate to a requested sequence position and returns the frozen intensity of
  one or more named `(process, sender, receiver)` pairs there — through the same
  evaluator path `simulate()` draws with, so the rate a proposal density uses is
  the walk handle's `Λ` (`dynes-augmentation` D20, invariant 2).
- Replay that agrees with the batch-preprocessed path to the 1e-10 bar
  `multi-process-walk` already holds, including under stamp ties.
- Wire `make_proposal_evaluator()`'s `rate_queries` argument to it, honoring
  the already-pinned named-list `(loglik, rates)` return contract (abmcem design
  D3, task 4.3).

**Non-Goals:**
- Changing `walk_open`/`walk_advance`/`walk_evaluate`/`walk_inject`'s contracts
  (`multi-process-walk` is consumed read-only).
- Building `augment_seq_mcmc()` itself or its acceptance ratio
  (`dynes-augmentation`'s own task). Tests here stub the *caller* side.
- Caching a live handle or a compiled walk *across* `make_proposal_evaluator()`
  calls (D5).
- Extending `evaluate_sequence_pool()`'s `what` vocabulary (D1).
- Ordered (`rate_ordered`) regimes and `choice_coordination`: the time
  proposals D20 draws are truncated exponentials at a *rate*, which only a timed
  composition has. A query against either aborts naming the regime (D3).

## Decisions

### D1 — Build on the walk handle, not the batch pool evaluator's `what`

`evaluate_sequence_pool()` (abmcem, `R/evaluator_dynes.R`) harvests
loglik/score/Fisher from `estimate_wrapper(..., max_iterations = 0L)` — a
Newton-engine pass over statistics preprocessed *once*. It has no notion of
"process state at position k", so a rate-at-position query there would build
the walk handle's state materialization a second time, against a different data
path.

The decisive reason is `dynes-augmentation` D20 invariant 2: every augmenter
draws its waiting times at "the walk handle's `Λ`" (`walk_evaluate()` →
`.pse_eval_rate`). A `q_rev` computed from any other path could drift from the
rate `sim` draws with, and the weight would be wrong where it is least visible.
Reading the rate from the walk keeps one definition.

*Rejected:* extending `evaluate_sequence_pool()`'s `what` over the
zero-iteration engine path — duplicate state machinery, a second definition of
"the state at position k".
*Rejected:* reading the state from the goldfishStat the closure's loglik pass
already preprocesses, via `materialize_process_state()` (literally D20's
"byproduct of the same pass"). That function is a test oracle deliberately kept
out of `R/` (its header: nothing in the package calls it); it re-reads the
stream from the start per query; and it would make the rate a second path
beside the walk, which invariant 2 forbids.

### D2 — Replay is driven by schedule position, promoted from the test oracle

The walk handle is opened over the candidate sequence itself (D6), so the
candidate's events are already the handle's schedule: dependent rows (the
events being scored) and focal-layer covariate rows (their realizations), with
integer sender/receiver. The replay driver steps that schedule **by row
position**, not by time:

```r
walk_replay_to(handle, row)   # apply schedule rows 1 .. row - 1, in order
```

- an exogenous row (not dependent, not on a focal layer) is applied **alone**,
  through the same internal steps `walk_advance()` runs per row
  (`walk_advance_presence()`, `merged_build_event_args()`,
  `walk_apply_object_event()`, then the handle's `exo_cursor` bumped past it);
- a focal covariate row (a candidate event's realization) is applied through
  `walk_inject()`;
- a dependent row applies nothing; the composition in force at its stamp is
  brought in with `walk_advance_presence()`.

This is the loop `walk_replay_against_batch()` already runs in
`tests/testthat/test-walk_handle.R`, which is proven equal to the batch path,
with one correction. It moves into `R/rate_queries.R` and the test helper calls
it, so one loop defines "the state at a schedule position" for the package and
for its oracle.

**Amended at implementation (2026-09-23): exogenous rows are applied one row
at a time, not through `walk_advance()`.** The oracle reaches an exogenous row
with `walk_advance(handle, schedule$time[k])`, which applies *every* exogenous
row stamped `<= time[k]`. Stream order interleaves a focal realization stream
between exogenous streams at one stamp (schedule `(time, stream_index)`:
e.g. `friendship` idx 2, `calls` realization idx 3, `emails` idx 4), so
reaching the `friendship` row also applied the later `emails` row — past a
requested stop at the realization row. The oracle never saw it: it runs to the
end and stops only at dependent rows, where every row before is at an earlier
stamp or from a lower-indexed stream. A row-bounded driver must not overshoot.

*Chosen:* `walk_replay_to()` applies exactly row `k` itself, mirroring the
per-row branch of `walk_advance()` (the plain exogenous case only; a candidate
replay handle registers no `walk_replay()` rows and schedules no queue).
`R/walk_handle.R` is untouched, so `simulate()`'s path — which advances the
clock through `walk_advance()` (`R/simulate_driver.R`) — runs byte-identical
code. The cost is ~8 lines mirroring `walk_advance()`'s branch; drift is
caught once the batch-vs-replay oracle steps through `walk_replay_to()`.
*Rejected:* extracting that branch into a shared helper in
`R/walk_handle.R` — one definition, but it rewrites `walk_advance()`'s
internals on the simulation path, relying on the suite to prove no change.
*Rejected:* keeping `walk_advance()` and accepting the overshoot — it breaks
the `after = k` semantics below ("no row after it, even one sharing its
timestamp").

**Why not time-driven.** The first draft looped
`walk_advance(handle, event$time); walk_inject(handle, event)` over events with
`time < upto`. `walk_advance(t)` applies every exogenous row stamped `<= t`, but
the batch path scores a dependent event *before* the covariate rows sharing its
stamp. The time-driven loop therefore reads the state one step late whenever a
candidate event ties an exogenous row. A time cutoff also cannot address "after
the first of two tied observed events", which D20 requires (tied REs are forced
adjacent and fold in their observed order).

**Sequence positions.** A query names a position by candidate event index,
`after = k` for `k` in `0 .. n` (n = the candidate's dependent events): the
state **immediately after candidate event k's realization row is applied**,
having applied every schedule row before it in schedule order and none after
it. `after = 0` is the state at open. This is D20's "frozen at the process
state after the preceding PE": exogenous rows later than event k — even at the
same stamp — lie inside the proposal window and are deliberately not folded in.
The mapping from `k` to a schedule row is a small function of the schedule;
`walk_replay_to()` itself stays row-based so the batch-consistency test can
stop at a dependent row's scoring point instead.

*Rejected:* generalizing `simulate_replicate()` to a replay mode — its loop is
dense with simulation-only concerns (waiting-time draws, marks, acceptance,
`SIM_REGIMES`, stopping). *Rejected:* `walk_schedule()` every candidate event
then `walk_advance(upto)` — it inherits the same `<=`-stamp ordering as the
time-driven loop.

### D3 — Pair intensity through the simulation evaluator path

New function `pair_intensity(handle, routing, theta, process, sender, receiver,
pinned = list())` returns the frozen intensity of one pair of one process at the
handle's current state, using the evaluation steps the simulation driver already
owns:

- **Evaluation path.** Each fid is evaluated through `evaluate_fid()` with
  inputs from `fid_evaluation_inputs()` (`R/simulate_driver.R`), not through
  `walk_evaluate()` directly. `walk_evaluate()` refuses deferred fids (a
  completed default or an effect-free sub-model, `R/walk_handle.R`), and DyNES
  specifications routinely carry them (a uniform choice completion, an
  intercept-only rate). `simulation_fid_state()` builds their degenerate state
  over the sibling fid's live support, and a completed timed rate reads its
  pinned intercept at the handle's current time. Reusing these steps means a
  query and a `sim` draw evaluate every fid identically.
- **What "the pair's rate" is.** For a DyNAM process the pair intensity is
  `λ_ij = rate_i × p(j given i)`: the rate fid's entry for the sender times the
  paired choice fid's probability for the receiver, the choice evaluated for
  that sender alone (`sender =`, one row rather than the n1 × n2 matrix) — the
  composition `mark_evaluation()` already performs for a draw. For REM the rate
  fid is already per dyad, and its entry is the intensity. Returning one fid's
  entry, as the first draft did, gives half the DyNAM quantity.
- **Indexing.** Entries are read by closed-form position — the sender for a
  per-actor rate, `(i - 1) * n2 + j` for a per-dyad value, the receiver within
  a sender's choice row — the same layout `.pse_dyad_index()` and
  `default_mark()` use. No search over `index_i`/`index_j`.
- **Excluded vs unresolvable.** An excluded alternative (inactive sender,
  unavailable receiver, a disallowed reflexive dyad, a support-masked cell)
  returns its computed exact zero. A query that does not resolve aborts with a
  classed cli error naming it: an unknown process, an unknown actor label, a
  receiver on a rate-only process, or an ordered / coordination regime (see
  Non-Goals). This mirrors `evaluate_model()`'s convention
  (`abort_if_exposure_undefined()`, `R/model_evaluate.R`) of aborting on an
  undefined *request* while returning a computed zero for an excluded
  *alternative*.
- **Labels.** Queries name actors by label, resolved through the evaluated
  process's `engine$node_lookup` — the `(side, local, global, label)` table
  `layer_node_lookup()` builds and `build_walk_engine()` stores on every walk
  engine. It comes from the same `mode_map` side vectors `remap_layer_refs()`
  used to turn the candidate's tie references into the schedule's local
  indices, so a query and the replayed events share coordinates with no second
  mapping. The sender resolves on side 1; the receiver on side 2 for a two-mode
  layer and on side 1 otherwise. A deferred fid has no engine and reads its
  sibling fid's (the fallback `simulation_fid_state()` already uses);
  `walk_compile_units()` guarantees every layer on an open handle has at least
  one engine. (`sanitizeEvents()` is not the mapping: it serves only the
  legacy environment source, and a stocnet source skips it.)

`process` names a `(layer, flavor)` pair, the identity a flavored DyNES layer's
processes carry in the `process_map`, passed as `list(layer =, flavor =)`
(`flavor` `NA` or omitted for an unflavored process).

**Amended at implementation (2026-09-24): `pinned` is an argument.** A
completed timed rate is evaluated at its pinned intercept
(`fid_evaluation_inputs()` reads it from the pinned table at the handle's
current time), but the walk handle does not carry the specification, so the
table cannot be recovered from `handle`. The caller passes
`spec$completed_rates`, exactly as `simulate_replicate()` does;
`query_rate_at_position()` (D4) holds the spec and supplies it. The default
`list()` suits a specification with no completed rate. *Rejected:* stashing
the table on `routing` — `simulation_routing()`'s return belongs to the
simulation driver, and widening it for one consumer changes its shape on
`simulate()`'s path. *Rejected:* storing the spec on the handle — a change to
`walk_open()`, which this change consumes read-only.

The regime checks read metadata only: a process's rate row with
`has_intercept = FALSE` is ordered (the timed/ordered split
`walk_engine_model_type()` dispatches on), and a `choice_coordination`
sub-model in the `process_map`'s `stat_block` is coordination. Both regimes
open a walk, so the refusal is `pair_intensity()`'s, not `walk_open()`'s.

### D4 — One handle, one forward pass per query batch

```r
query_rate_at_position(spec, theta, queries, control_prep = set_preprocessing())
```

`spec` is the candidate-bound, completed joint spec (D6); `theta` is the
per-fid list (D7). `queries` is a list of
`list(after = <k>, layer = , flavor = , sender = <label>, receiver = <label>)`
(`receiver` omitted only for a rate-only process, which then aborts per D3 —
DyNES queries are pair queries).

The handle only moves forward, so **one** `walk_open()` answers every query:
queries are ordered by `after`, the handle is replayed to each distinct
position in turn (`walk_replay_to()` resumes from where it stopped), and
`pair_intensity()` reads every query at that position. Results are a plain
numeric vector in the caller's original order. The first draft opened a fresh
handle per distinct position, replaying from the start each time.

`make_proposal_evaluator()`'s closure (abmcem, `R/evaluator_dynes.R`) answers a
non-`NULL` `rate_queries` by binding the candidate (D6) and calling
`query_rate_at_position()`, returning `list(loglik = ..., rates = <numeric>)`.
`rate_queries = NULL` keeps returning `rates = NULL`, and the closure does no
walk work at all on that path.

### D5 — No cross-call caching in v1

Every `query_rate_at_position()` call opens a fresh handle, and `walk_open()`
recompiles every formula against the bound data (`compile_process_unit()` →
`compile_recipe_spec_map()`), on top of the loglik path's own per-sub-model
compile. A chain therefore pays two compiles per proposed move. Keeping a live
handle, or the compiled units, across a chain's successive proposals is a real
optimization but belongs to `augment_seq_mcmc()`'s driver loop (which owns the
chain state), consistent with the MCMC-pool-reuse deferral
(`dynes-augmentation` D23). The per-proposal compile inside `walk_open()` is
the first place to look if profiling finds the query dominant.

### D6 — The spec is completed once per EM iteration and rebound per candidate

`make_proposal_evaluator()` holds formula-level `sub_models` and a stocnet
`candidate_seq` — neither can open a walk, which needs a generatively complete
joint spec. The closure therefore also takes the **completed joint spec** the
EM loop already has (it decomposes `sub_models` from it), as a new optional
argument: `make_proposal_evaluator(sub_models, theta, spec = NULL, ...)`. A
`rate_queries` request with no `spec` aborts naming the missing argument; the
loglik path is unchanged and never reads `spec`.

Per candidate, a small internal helper rebinds the data:

```r
bind_candidate_data(spec, candidate)   # spec with candidate as its data
```

It assigns `candidate` to `spec$data` and to every
`spec$specifications[[i]]$data`, after asserting that the candidate's `info`
and `nodes` are identical to the bound spec's. Nothing else is recomputed.

**Why assignment suffices.** `walk_open()` reads data only through
`joint_spec$data` (the unit compiles, the shared state source and the
support-constraint compiles in `build_merged_blocks()`). The data-derived
fields a swap leaves stale are read only by print methods or at construction:
`spec$dependent` (`n_events`, `time_span`, `flavors`; read by
`methods_display.R`) and `completion_gaps` (estimate/simulate entry).

**Why the identity check.** Several things are fixed at construction from
`data$info` and `data$nodes` and are *not* recomputed by a swap: flavor style
and `values_equivalence` (derived masks, mark updates), `modeled_panel` and the
`process_map`'s coupling, the node universe and mode map, and the completion
pins. A candidate differing there would run against a spec built for another
dataset, silently. An augmented candidate legitimately differs from the
observed data in its tie rows only; the check pins exactly that, and its abort
names which of `info` / `nodes` differs.

**Why not rebuild the spec per candidate** (re-running `make_specification()`,
`make_joint_specification()` and `complete_generative_spec()` over
`candidate_seq`). Beyond duplicating the parse and validation `walk_open()`
repeats anyway, it re-runs `pin_completed_rates()` per candidate: an
event-observed completed layer re-pins from `relational_window_risk_set()` over
the candidate's events, a panel layer from its wave endpoints. Endpoint-hitting
candidates probably leave both unchanged, but nothing guarantees it, and a pin
that moved with the candidate would shift `f` between proposals with no
parameter to explain it. Completion runs once, at the consumer entry
(`dynes-augmentation` D21); rebinding keeps that rule.

*Rejected:* the caller passes a ready walk handle or spec per call — it moves
the same construction into `augment_seq_mcmc()` and splits the identity check
from the thing it protects.

### D7 — Flat theta maps to per-fid theta through `coef_layout()`

The closure holds a flat theta over free slots (`sub_models[[j]]$theta_index`);
the walk evaluates per fid, over the fid's full column set (fixed slots
included, with a leading intercept for a timed rate). `coef_layout()` on the
joint spec already gives, per fid, every slot's `fixed` flag, fixed `value` and
free-slot `index` into the flat vector. The per-fid vector is the layout's
`value` column with the free slots filled from `theta[index]`. Completed fids
are not read from theta at all — `fid_evaluation_inputs()` gives them their
pinned or uniform inputs.

The mapping assumes the EM loop numbers `theta_index` in `coef_layout()`'s
canonical order. That is asserted, not assumed: the closure checks, once at
construction, that the sub-models' `theta_index` union equals the layout's free
`index` set, and aborts naming the mismatch.

## Risks / Trade-offs

- **[Replay drift from the batch path]** → the loop is the one the batch-vs-replay
  oracle already runs (D2), and the oracle calls the promoted function; a
  dedicated scenario ties a candidate event to an exogenous row's stamp.
- **[Pinned-rate drift across candidates]** → completion never re-runs per
  candidate (D6); a test asserts a completed fid's queried intensity is
  invariant to swapping between two candidates that differ only in PE times.
- **[Loglik and query use different compiles]** → the loglik path is
  per-sub-model `estimate_wrapper()`, the query path the merged walk; they
  agree through `multi-process-walk`'s batch-vs-replay equality with the
  single-process path. Stated, not re-proven here.
- **[Theta layout mismatch]** → asserted at closure construction (D7).
- **[Performance under a real chain]** → two compiles per proposed move (D5);
  first target if profiling finds it.
- **[Candidate carries different `info`/`nodes`]** → aborts at rebind (D6).

## Migration Plan

Additive: one new file, one test helper that now calls package code, one
previously-aborting branch replaced with real computation, and one new optional
argument on an internal closure constructor. No behavior changes for any caller
that does not supply `rate_queries`. Rollback is reverting the change's commits.

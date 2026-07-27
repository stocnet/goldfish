## Context

Created 2026-07-20 from the `flavored-processes` design interview (its D9/D10
record the shared decisions on that side). The multivariate specification was
sketched in `goldfish_extras/parsing_link.md` and
`goldfish_asta/code/plan/goldfish_dev_plan.md`; both predate the current
architecture — the flavored consumer machinery (`R/preprocess_flavored.R`),
the compiled support-constraint plans, and the stocnet single data object now
exist, and this design supersedes those sketches where they conflict (notably:
no global effects table — gids are scoped per statistic block; no
`estimate_dynes`-independent multivariate estimator).

Sequencing: post-2.0.0, after `flavored-processes` completes; before (and
consumed by) `dynes-augmentation`. Same-node-set restriction until
`multimode-network-support`.

## Goals / Non-Goals

**Goals:**
- `make_joint_specification()` composing process specifications for panel +
  relational-event co-evolution, with coupling detection.
- The extended fid/process_map vocabulary across processes.
- The merged single-clock walk with `(layer, flavor) → fid` routing and
  per-fid preprocessed outputs.
- The stepping/injection walk handle for external drivers.

**Non-Goals:**
- Augmentation and MCEM (`dynes-augmentation`), the `estimate_dynes()` surface +
  ABEM loop (`abmcem`), and the general `simulate()` (the `process-simulation`
  change — a family-agnostic S3 generic driving this change's walk handle).
- Any estimator for fully observed multivariate specifications (exactly
  separable; per-process estimation is the answer, enforced by construction
  and by the estimation-surface contract).
- Mixed node sets (`multimode-network-support` relaxes v1's restriction).
- DyNAM-i processes (their shape is `dynami-stocnet-boundary`'s to settle).
- Cross-family effect deduplication (see D5).

## Decisions

### D1 — Purpose: DyNES substrate; estimation is `estimate_dynes()` only
There is no `estimate()` generic and no `estimate_multivariate()`. The
multivariate specification exists to portray the co-evolving panel +
relational-event system that augmentation couples; `estimate_dynes()` is its only
estimator — its surface and ABEM loop live in `abmcem` and its panel data path in
`dynes-augmentation` (the two share the `dynes-estimation` capability), and it
takes a `make_joint_specification()` object as its `spec`. *Rejected:* a generic
multivariate estimator over fully observed processes — the factorization makes it
identical to separate per-process estimation, so it would be surface without
substance.

### D2 — A referenced panel-observed layer is required at construction
`make_joint_specification()` aborts unless at least one **panel-observed layer is
referenced in the composed formulas** — either as a process's focal/dependent
layer *or* as an exogenous covariate read by another process's effects or
support-constraint atoms (the layer-info observation metadata decides; no model
flag needed). This is broader than requiring a panel *focal* process: a spec whose
only panel reference is an exogenous covariate read by a relational-event process
still needs DyNES, because that covariate's latent between-wave path couples the RE
likelihood (`dynes-augmentation` D8 owns how such an exogenous-only panel reference
is augmented — static step-covariate or random augmenter, the user's per-layer
choice). A combination that references **no** panel-observed layer is rejected with
guidance to estimate each specification separately (it is exactly separable).
*Rejected:* requiring a panel *focal* process specifically (the earlier, narrower
gate) — it would reject legitimate specs whose panel coupling is through an
exogenous covariate; *also rejected:* allowing pure relational combinations with no
panel reference at all — those are separable and belong to the per-process
estimators.

### D3 — fid vocabulary extends by rows and one column
The `process_map` of `flavored-processes` D9 is reused unchanged in kind:
integer fid canonical, labels rendered, constraints keyed by `constraint_id`
shared per `(layer, flavor)` (D3b sharpens what "shared" means once the walks
merge). This change adds rows (each process contributes
`n_families × max(K, 1)` fids) and a `coupled` logical column. Nothing is
re-keyed; consumers, compiled constraints, and outputs index exactly as in the
flavored case.

### D3b — One constraint compile across families; snapshots stay per fid
The merged walk (D5) collapses the two per-family plans into one, so a
`(layer, flavor)` constraint is **compiled once** into a single
`plan$support_constraints` that `constraint_id` indexes directly — rather than
compiled once per family with `constraint_id` sharing only the identity, as
`flavored-processes` leaves it.

*Verified precondition (2026-07-20, against the landed flavored code):* the
compile is already family-invariant, so nothing has to be disentangled first.
`compile_support_constraint()` takes `model`, `dep_name`, `nodes`, `nodes2`,
`envir`, `data` — all identical for a layer's rate and choice — plus
`window_derivations`, which is **inert for constraints**: the atoms are resolved
by `resolve_formula_names()` against the data components with no derivations
argument, so a derived (windowed) name is rejected before any builder receiving
`window_derivations` is reached (`~ tie(calls_5)` aborts with "not in the data"
even in the family whose own formula created `calls_5`). The two per-family
compiles are therefore byte-identical work done twice. Hoisting is available
today; it is scheduled here because the merged walk makes it fall out of the
structure instead of requiring a new seam through `build_spec_map()`.

**Compiling once is not realizing once.** The compiled sub-plan is
family-invariant; the mask *timeline* is not. `preprocess_support_mask()`
snapshots at the stored `event_time` of the object it serves, and a timed rate
fid's stored events include the cross-process/cross-flavor right-censored rows
that a choice fid's do not (in the flavored fixture: creation-rate stores
t = 1,2,3,4 and creation-choice t = 1,3 for one constraint). Each fid MUST keep
its own snapshot sequence; handing one fid another's timeline is a silent
wrong-answer bug, not a crash. The contract is therefore **compile per
`constraint_id`, snapshot per `fid`**.

Optional refinement to measure at task time: maintain the constraint atoms
inside the merged walk rather than in `preprocess_support_mask()`'s separate
self-contained pass, snapshotting per consumer at each consumer's write. That
makes it **maintain once, snapshot per fid** — the same one-clock-one-state
argument D5 makes for effect statistics, applied to constraint atoms. Kept
optional because the separate pass is what keeps the unconstrained statistics
path untouched, and that isolation is a baseline-safety property worth a
deliberate trade rather than an incidental one.

### D3c — One shared atom pool; masks are per-fid projections of it
The mask machinery gets the treatment the effect statistics already got: one
shared computation, many cheap projections.

`preprocess_support_mask()` already splits the two internally — `apply_atom_event()`
maintains one dense matrix per atom, `eval_mask()` projects them through the
constraint's boolean tree — but it takes ONE `sub_plan` and ONE `snapshot_times`,
so it is instantiated per output and re-walks the atoms every time. A two-flavor
rate+choice specification therefore runs four atom walks (and holds four copies of
the atom matrices) for what is structurally one atom stream.

The key observation is that a `mutually_exclusive` layer's derived constraints are
different EXPRESSIONS over the SAME ATOMS: creation is `!tie(L)`, dissolution is
`tie(L)`, and AND-composing a user constraint `U` gives `!tie(L) & U` / `tie(L) & U`
— atom set `{tie(L)} ∪ atoms(U)` either way. Atom maintenance is the expensive part
(walking event streams, calling update closures, dense per-atom matrices);
evaluating the tree is elementwise boolean work over matrices already in hand. So
the union is taken over ATOMS, deduplicated by atom identity exactly as the effect
union deduplicates by canonical term label, and each fid keeps its own `expr` and
its own snapshot times.

Counts for a two-flavor rate+choice specification:

| | atom walks | atom matrix copies |
|---|---|---|
| today | 4 (per output) | 4 |
| per-`constraint_id` sharing | 2 | 2 |
| **shared atom pool (this decision)** | **1** | **1** |

This subsumes the weaker per-`constraint_id` sharing (which only merges the rate
and choice fids of one flavor) and generalizes: in the multivariate case atom sets
across processes overlap partially rather than totally, and a union handles both.
Evaluation cost does not collapse and should not — each fid genuinely needs its own
mask at its own times.

*Kill condition:* this decision is worth implementing only if atom maintenance is a
substantial share of the mask pass. If task 3.0's within-pass split shows evaluation
dominating, pooling the atoms buys little however large the pass is, and D3c is
dropped — the table above counts atom walks, which is the right unit only when
walking is what costs.

Consequence for D3b: this captures the atom-sharing win **without** putting
constraint code on the main walk's hot path, so it preserves the isolation that
keeps the unconstrained statistics path provably untouched. If 3.0's measurement
lands in the middle, do this first and re-measure before considering D3b's fold at
all. *Rejected:* deriving one flavor's mask as the elementwise complement of
another's — true only for the dichotomous mutually-exclusive case with no user
constraint, and the expression tree is general; the shared atom pool gets the same
win without special-casing.

### D4 — Coupling: direct reference; inform when partial, abort when total
A fid is **coupled** iff any effect argument or support-constraint atom in its
formula reads a panel-observed layer's state. Direct reference only: observed
events of an intermediate relational layer are exogenous data in this fid's
likelihood regardless of what that layer's own model references, so coupling is
not transitive. The specification print marks separable (uncoupled) fids. The
estimation contract (enforced by `estimate_dynes()` in `dynes-augmentation`):
a mixed specification proceeds with a cli message naming the separable fids
(their likelihood terms touch no latent path, so joint estimation equals
separate estimation for them); a specification whose fids are all separable
aborts — nothing in it needs DyNES. (The all-separable case is unreachable
through `make_joint_specification()` itself given D2 — the panel process's own
fids are coupled by construction — but the contract guards recomposed or
edited specifications.)

### D5 — Merged one-walk; gid scope per statistic block; dedup within block only
The two per-family walks merge into one single-clock walk hosting both
statistic blocks: `stat_block = (model, sub-model family, statistic dims)`,
gids scoped per block. Per event, the walk updates each block's effect
statistics once and routes to consumers via the `(layer, flavor) → fid`
lookup: an event is dependent for its own `(layer, flavor)` fids, a
right-censoring boundary for every other timed rate fid (cross-process exactly
as cross-flavor — the factorization note's argument is layer-agnostic), and
state-only for choice/ordered fids. Effect deduplication extends across
processes *within* a block (`tie(friendship)` in two processes' choice
formulas is one column) and never across effect-dispatch families —
`inertia(net)` in DyNAM-choice, choice_coordination, and REM resolve to
different update functions even when mathematically kin; proving equivalences
is the effect registry's business, not this change's. *Rejected:* per-process
walks (recompute shared cross-process effects, walk the stream N times);
a global effects table across blocks (parsing_link.md sketch — unsound, shapes
and dispatch differ).

### D6 — Walk handle: stepping + injection
`walk_open(spec, data)` returns a stateful handle owning the merged walk's
clock, process state, and per-block statistics. `walk_advance(handle, t)`
moves the clock (applying exogenous events up to t), `walk_evaluate(handle,
fid, theta)` returns the fid's evaluation at the current state (rate vector
for sender-block fids, choice matrix for dyad-block fids, masks applied),
`walk_inject(handle, event)` applies an event — observed or sampled — to the
shared state. Replay evaluation over an observed sequence is the driver
looping advance/evaluate/inject; the DyNES model-driven augmenter and
`simulate()` are external drivers sampling before injecting. This realizes the
compute/emit separability reserved by `flavored-processes` D10: the writers
and the evaluator are two emit targets over one compute walk. *Rejected:*
replay-only evaluation — the augmenter must propose and apply events
mid-walk, and a replay contract would be reopened immediately.

The evaluation substrate already exists: the archived
`refactor-likelihood-compute` change landed the internal **process-state
evaluators** and **state materializer** (living capability
`process-state-evaluators` — per sub-model rate/probability vectors at a
materialized state + parameters, exclusions as exact zeros, explicitly
reserved "for a future simulate() and for DyNES data augmentation").
`walk_evaluate()` is a thin wrapper applying those evaluators to the handle's
*live* state instead of a materialized replay state; it does not reimplement
any probability/rate computation. The batch-vs-replay equality tests double as
the cross-check against the materializer path: materializing at event k and
evaluating must agree with advancing the handle to event k and evaluating.

### D7 — Scope: DyNAM-i out; choice_coordination and ordered/timed mixes in
Composable processes are DyNAM (rate, choice, choice_coordination), REM, in
timed or ordered sub-models, freely mixed — per-fid `has_intercept` already
carries timed-vs-ordered right-censoring semantics per consumer, and
choice_coordination rides the dyad block with its own dispatch family (no
cross-family dedup per D5). DyNAM-i processes are rejected:
`dynami-stocnet-boundary` owns their future shape.

## Risks / Trade-offs

- **The merged-walk refactor touches both recipe loops at their core** → the
  frozen 1e-6 baselines gate every commit; the single-process and flavored
  paths must stay byte-identical (same discipline that carried the
  `flavored-processes` consumer refactor).
- **Walk-handle statefulness invites divergence from the batch walk** → the
  handle and the batch preprocessing driver share one implementation (the
  batch driver is a replay driver over the handle, or both sit on the same
  internal stepper); tests assert batch-vs-replay equality on fixtures.
- **Coupling detection false negatives** (a coupled fid marked separable)
  would silently bias DyNES estimates → detection reads the parsed effect
  objects and constraint atoms (the same structures preprocessing consumes),
  not formula text; tests cover effects, constraint atoms, and windowed
  variants referencing the panel layer.
- **dynes-augmentation drift**: its proposal predated this change's walk
  handle → re-grounded (2026-07-21): the augmenters and `augment_seq_sim()` are
  external drivers of `multi-process-walk`, the batched `evaluate_model()` reads
  the merged walk's per-fid outputs, and the general `simulate()` moved to the
  `process-simulation` change. Its "per-event simulation hook" framing and its
  modification of `preprocess-output-writers` are dropped.

## Migration Plan

Purely additive surface; the merged-walk refactor replaces the two-walk
internals behind unchanged single-process/flavored behavior (baseline-gated).
Rollback is reverting commits; no data-format or baseline impact.

## Open Questions

- Whether the batch preprocessing driver is literally a replay driver over the
  walk handle or a sibling over a shared stepper is an implementation choice
  measured at task time (replay-driver purity vs. the batch path's current
  performance).

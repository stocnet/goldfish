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

Sequencing: post-2.0.0, after `flavored-processes` and
`multimode-network-support` complete; before (and consumed by)
`dynes-augmentation`. Node-space generality (D8) rides the landed mode map —
processes compose over one shared mode-map object with per-mode-pair walk
blocks; only subset/nested cross-process coupling stays future work.

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
- Subset/nested cross-process coupling — a cross-process effect or constraint
  read that bridges a mode to a *union containing it* (the directors-only ↔
  all-employees shape, "Gap B"). It needs a subset embed/marginalize projection
  no landed capability provides (`multimode-network-support`'s conformance is
  mode-set-identity only), and is recorded future development (D8). Note the
  in-scope case: identity-conforming composition across *distinct* mode-pairs
  that share a *whole* mode (advice `{staff}×{director}` + nominations
  `{director}×{project}`) IS supported (D8).
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

**Estimator guards (each `estimate_*()` owns its own case).** The joint object is a
distinct S3 class (D2b), so the event-stream estimators can reject it by class
before their existing `specification.goldfish` dispatch branch:
- `estimate_dynam()` / `estimate_rem()` SHALL abort on **either** a
  `make_joint_specification()` object (pointing to `estimate_dynes()`) **or** a
  single `specification.goldfish` whose focal/dependent layer is panel-observed
  (the PE-dependent case). The latter reuses the existing
  `check_dependent_panel()` guard, whose message `dynes-augmentation`'s
  `single-data-object` delta already retargets to `estimate_dynes()`; this change
  adds only the joint-object rejection.
- `estimate_dynami()` SHALL abort on a PE-focal specification (same
  `check_dependent_panel()` path). Its rejection of a joint object is a **recorded
  future development** — DyNAM-i is still under development and cannot appear in a
  joint specification anyway (DyNAM-i processes are rejected at composition, D-existing).
- `estimate_dynes()` conversely aborts when **no** modeled panel-dependent process
  is present (the mirror guard, `dynes-augmentation` D19).
The duplicate-focal-layer abort itself lives in the constructor (D2b), not in the
estimators — a malformed join is caught before any estimator sees it.

### D2 — A referenced panel-observed layer is required at construction; DyNES-viability is estimation-time
`make_joint_specification()` aborts unless at least one **panel-observed layer is
referenced in the composed formulas** — either as a process's focal/dependent
layer *or* as an exogenous covariate read by another process's effects or
support-constraint atoms (the layer-info observation metadata decides; no model
flag needed). Construction checks **structural** panel presence only; it does *not*
require a *modeled* panel process. A spec whose only panel reference is an exogenous
covariate composes: the panel layer enters as a **static step-covariate** (state
jumps at wave times, not latent, no random sampling — `dynes-augmentation` D8),
which is a legitimate DyNAM specification with a panel covariate. The **latent-path
requirement is estimation-time**: `estimate_dynes()` aborts on such a spec, naming
`estimate_dynam()` and explaining the panel layers would only be static exogenous
covariates (`dynes-augmentation` D19) — the guardrail that catches a user reaching
for DyNES on a DyNAM model. A combination that references **no** panel-observed
layer is rejected at construction with guidance to estimate each specification
separately (it is exactly separable). *Rejected:* requiring a panel *focal* process
at construction — it would reject legitimate DyNAM-with-panel-covariate specs and
move the tailored redirect message off `estimate_dynes()`, where the user expects
it; *also rejected:* allowing pure relational combinations with no panel reference
at all — those are separable and belong to the per-process estimators. *Superseded:*
the earlier claim that an exogenous-only panel covariate's between-wave path is
latent and so "still needs DyNES" — such a covariate is static, not latent, and its
spec is DyNAM.

### D2b — Constructor named `make_joint_specification()`; distinct class; one focal layer per specification
The composition constructor is **`make_joint_specification(...)`** (renamed from the
working `make_multivariate_spec()`), returning an object of a **new S3 class
`joint_specification.goldfish`** — deliberately *not* inheriting
`specification.goldfish`, so the event-stream estimators' existing
`inherits(x, "specification.goldfish")` dispatch does not fire on it and the D1
class guard can reject it cleanly. The *concept* remains "the multivariate
specification" in prose and capability/dir names (`multivariate-specification`,
`multi-process-walk`); only the exported constructor and its object class carry the
`joint` name. The source file is `R/make_joint_specification.R`.

**One focal layer per joined specification.** `make_joint_specification()` SHALL
abort unless the joined specifications' **focal/dependent layers are pairwise
distinct** — a layer may be modeled by at most one specification in the join. The
uniqueness is on the *dependent* layer only: a layer MAY still be **read as an
exogenous covariate** by any number of other specifications (this is exactly the
coupling that makes joining meaningful — `calls` reading `friendship`), so "appears
in" is scoped to the focal role, never covariate references. All flavors of one
layer MUST be carried by a **single** specification (the flavor-keyed rate/choice
lists of `flavored-processes`); creation and dissolution of the same layer cannot be
split across two joined specifications — this preserves the "modeled for all flavors
or not at all" invariant (`dynes-augmentation` D8/D19). *Rejected:* a strict "a
layer named anywhere is exclusive to one specification" reading — it would forbid one
process reading another's layer as a covariate, eliminating coupling, which is the
whole purpose of the joint specification. *Rejected:* silently coalescing two specs
that share a focal layer — a repeated dependent layer is a user error (two competing
sub-models for the same events), caught at construction with a cli error naming the
duplicated layer.

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
formula reads a **modeled** panel layer's state — a panel layer that is itself a
process of the specification, whose between-wave path is latent and augmented.
Reading a panel layer that appears *only* as an exogenous covariate does **not**
couple: that covariate is a static step-covariate (`dynes-augmentation` D8), so the
reading fid's likelihood touches no latent path. Because the static-vs-augmented
treatment is fully determined by whether the panel layer is a modeled process —
there is no per-layer estimation-time choice — coupling is exactly computable at
construction. Direct reference only: observed events of an intermediate relational
layer are exogenous data in this fid's likelihood regardless of what that layer's
own model references, so coupling is not transitive. The specification print marks
separable (uncoupled) fids. The estimation contract (enforced by `estimate_dynes()`
in `dynes-augmentation`): a mixed specification proceeds with a cli message naming
the separable fids (their likelihood terms touch no latent path, so joint estimation
equals separate estimation for them); a specification whose fids are all separable —
equivalently, no panel layer is a modeled process — aborts, naming `estimate_dynam()`
and explaining its panel layers would only be static exogenous covariates. (This
all-separable case **is** reachable through `make_joint_specification()`: D2 composes a
spec whose only panel reference is an exogenous covariate, and the DyNES-viability
check is deferred to `estimate_dynes()`.)

### D5 — Merged one-walk; gid scope per statistic block; dedup within block only
The two per-family walks merge into one single-clock walk hosting both
statistic blocks (generalized to per-mode-pair blocks by D8):
`stat_block = (model, sub-model family, statistic dims)`,
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
cross-family dedup per D5). Processes MAY be one-mode or two-mode over the
shared mode-map object (D8). DyNAM-i processes are rejected:
`dynami-stocnet-boundary` owns their future shape.

### D8 — Node-space generality: per-mode-pair blocks; identity-conforming composition in, subset projection out
With `multimode-network-support` landed, the v1 "one shared node set"
restriction is lifted to **one shared mode-map object**. The merged walk (D5)
keys its statistic blocks by the mode-map's mode-pair — `stat_block = (model,
sub-model family, mode-pair)`, the "statistic dims" of D5 read as the sender
mode (rate) or the `(sender mode, receiver mode)` pair (choice/dyad). The
"two blocks" framing is the single-mode-pair special case; a multi-mode-pair
join yields one sender block per distinct sender mode and one dyad block per
distinct mode-pair among the composed processes. Processes MAY therefore be
one-mode or two-mode, and dependent processes over *distinct* mode-pairs
compose. This reuses machinery the mode map already ships: nodal state is
keyed by mode-set and **not capped at two** views, and **layers sharing a side
share one view**, so a statistic on a shared mode's marginal is a candidate to
compute once and read from every block that touches that mode (the state layer
already does this for attribute views; whether statistic dedup follows across
blocks is Open Question / a task-time measurement).

**The v1 boundary is mode-set-identity conformance** (the mode map's own rule).
A cross-process effect argument or support-constraint atom that reads another
process's layer is admitted only when the read **conforms by mode-set
identity** — i.e. the shared node space is a *whole* shared mode. Advice
`{staff}×{director}` composed with nominations `{director}×{project}` share the
full `director` mode: a director-indexed read (`indeg(advice)` feeding the
nominations sender block) conforms, so this multilevel shape is in scope with
no new primitive. A read that would bridge a mode **subset** to a union
containing it (a directors-only process coupling to an all-employees process,
`{director}` ⊂ `{staff,director}`) does **not** conform;
`make_joint_specification()` SHALL abort at construction naming the two layers
and the offending modes, recorded as future development ("Gap B" — a subset
embed/marginalize projection with its own embed-with-zeros-vs-marginalize
semantics that no landed capability provides). *Rejected:* keeping the strict
one-node-set restriction — it needlessly blocks the whole-shared-mode
multilevel case and two-mode multiplex, both of which the landed mode map
supports directly; *also rejected:* silently admitting a subset read by
dimension match — `multimode-network-support` deliberately refuses that
("equal-sized distinct modes do not conform"), and coupling on a wrong
projection would silently bias DyNES estimates.

**Hard dependency on `formula-drives-focal` (blocking edge, not a soft
preference).** Each composed process resolves its sides and modes against **its
own** modeled dependent layer, not a single object-level `info$focal`. This is
unavoidable for a join: one shared data object carries at most one `info$focal`,
but a joint specification models N dependent layers, so all but at most one
process is modeling a layer that is *not* `info$focal` — precisely the case
`formula-drives-focal` exists to fix (its two-mode side-validity check otherwise
resolves the wrong dyad's sides against `info$focal`, silently accepting or
rejecting an effect). Rather than replicate and re-sync that fix here, this
change consumes it: section 1b/3 start only after `formula-drives-focal` lands,
and `make_joint_specification()` and the walk never touch focal resolution
themselves. The `info$focal` field and the `manynet::add_info(focal = ...)`
convention are unchanged — this decision changes *nothing* about the vocabulary,
only which layer drives resolution when several are modeled. *Rejected (Option
B):* stamping per-process focal resolution locally to decouple sequencing — it
duplicates `formula-drives-focal`'s fix, and the two-mode side-validity check is
exactly where a drifted copy would silently bias results.

**D8a — In the merged walk, focal is per fid over one shared state (a per-fid
data-source view, never a stamped shared focal).** `formula-drives-focal` is
consumed two ways in the landed code: the single-process preprocess path threads
the resolved dependent *explicitly* — `new_data_source(focal = spec$focal)`, and
that one `src` is passed down (`src = src`) into every builder (Pattern A); the
single-process *estimation* path additionally stamps `work_data$info$focal <-
dep_name` on a local copy so any source built without a threaded focal still
resolves the modeled layer through `new_data_source()`'s `focal %||% info$focal`
fallback (Pattern B). Pattern B is correct only because one estimation models one
layer, so one focal on one data object is complete. The merged walk (D5) breaks
that invariant deliberately: it hosts **N** processes over **one shared state**
(the shared state is what makes coupling possible), so no single focal value —
`src$focal` or a stamped `info$focal` — can serve it. A single stamped focal would
reintroduce, *inside one join*, the exact wrong-dyad side-validity bug
`formula-drives-focal` removed. Therefore the merged walk resolves focal **per
fid**: each fid carries a lightweight data-source **view** whose `focal` is its own
`proc$layer` (= that spec's resolved `spec$focal`), all views sharing the one
underlying state/cache. The merged driver (§3.1/3.2) and the walk handle
(§4.1/4.2) follow Pattern A exclusively and MUST NOT use the single-process
`work_data$info$focal <- dep_name` stamp; a fid's two-mode side-validity,
dependent-row selection, and mode-pair lookup all resolve against its view's
focal. *Rejected:* one shared `src` with focal passed at each call site — every
focal-sensitive call would have to remember to pass it, and a single omission
silently falls back to the shared `info$focal` (the same failure mode by another
route); the per-fid view makes the correct focal the default at every call.

### D9 — Generative-readiness completion: one transform at the consumer entry; the walk handle asserts

**The requirement.** A specification that will *generate* events (drive the walk
handle) or be estimated *jointly* (the coupled DyNES likelihood) must be
**generatively complete**: every modeled DyNAM flavor (the choice /
choice_coordination sub-model family) carries **both** a rate and a choice; REM
carries a rate (already enforced — choice is rejected for REM). This is the shared
precondition of `simulate()` (`process-simulation`), every augmenter and the pool
evaluator (`dynes-augmentation`), and `estimate_dynes()`'s joint preprocessing: a
DyNAM event needs a rate (who acts, when) and a choice (whom), so a half-specified
process cannot be drawn or jointly scored.

**Scope — and the hard exclusion.** Completion applies to the *generative/joint*
consumers only. The single-process / flavored **estimation** path
(`estimate_dynam()` / `estimate_rem()` over one `specification.goldfish`) is
**excluded**, for two independent reasons: (1) rate-only DyNAM estimation is a
legitimate standalone model whose home is the formula interface
(`estimate_dynam(dep ~ ..., rate_sub_model = "rate")`) and the rate-only spec
object — it never draws an event, so it needs no choice; and (2) that path routes
through the merged walk (D5) under the frozen 1e-6 baseline gate, so silently
adding a choice block would change its preprocessed output and break byte-identity.
Completion must never touch it.

**Regime governs the rate side.** A composed specification is either **ordered**
or **timed**, and the two MUST NOT mix — a composition pairing an ordered process
with a timed one is rejected at `make_specification()` time. Regime is
**inferred**, not declared: the system is timed iff any process's rate is a
waiting-time (intensity) rate, ordered otherwise. Regime decides how a missing
rate is handled, because coupling runs on **one shared clock**: a timed system
requires every process to place its events on that continuous clock, an ordered
system only needs a sequence.

**Defaults (per missing sub-model).** The missing half is filled with its
zero-information default; **every default is zero-free-parameter** — completion
never adds an estimated coefficient to θ:

| missing half | default | free params |
|---|---|---|
| choice | uniform over the **support-legal** alternatives (no effects) | 0 |
| choice_coordination | uniform on **both sides** (no effects) | 0 |
| rate (ordered regime) | uniform `rate_ordered` (no effects) | 0 |
| rate (timed regime) | intercept-only rate, intercept **pinned** per wave-period from the per-period count `count_w` (`intercept-only-rate-spec`) | 0 |

The uniform choice **inherits the layer's support constraints** where any were
defined (uniform over the support-legal alternatives, never over all actors); it
does **not** fabricate support constraints and does **not** borrow them from a
sibling flavor on the same layer — the sole automatic restriction is disallowing
self-loops. A missing `choice_coordination` completes to a uniform draw on **both**
coordination sides.

**Timed rate completion is pinned, not estimated.** In a timed system a modeled
flavor missing its rate — including a flavor that would otherwise be *choice-only*
— is completed with an **intercept-only rate** so its events land on the shared
clock. The intercept is **not** a free parameter: it is pinned per wave-period to
`λ_w = count_w / T_w`, where `count_w` is that flavor's per-period count (for a
modeled panel flavor, the **net wave Hamming diff** between the two observed wave
states bounding period *w* — a net-change *floor*, not a directly observed
micro-count) and `T_w` the period's duration. The pinned rate is a **single
aggregate flavor intensity**: when an event of that flavor fires, its sender is
drawn **uniformly among the support-legal actors** and the per-period aggregate
calibration reproduces `count_w` regardless of how the risk set changes — that draw
and calibration property are the **`intercept-only-rate-spec`** primitive's
semantics, not re-specified here. What grants the exclusion from the optimizer is
**θ-independence**, not iteration-constancy: `λ_w` does not depend on the estimated
parameters, so its timing likelihood is **additive-constant w.r.t. θ** and is
**excluded from the score and Hessian**, used **generatively** to place events but
never in the optimization objective (a constant offset that MAY be included in a
*reported* log-likelihood). The likelihood value is also iteration-constant *today*
because `count_w` is the fixed net Hamming diff; a future latent count would vary
per iteration yet stay θ-independent, so the exclusion would still hold. This
primitive is owned by the standalone **`intercept-only-rate-spec`** change; D9's
timed-regime completion supplies the count and periods and **consumes** it (a hard
dependency, analogous to §1b's on `formula-drives-focal`). For
`simulate()`, a mixed process (one flavor rate-modeled, another missing) pins the
missing flavor's constant from observed counts the same way; a **fully** rate-less
process instead draws its event budget from the user's requested event count / time
window (`process-simulation`).

**Ordered regime: no rate completion.** In an ordered system a **choice-only
DyNAM** flavor is *not* rate-completed — its timing rides `process-simulation`'s
pseudo-time / fixed-template modes (there is no clock to place it on).

**Two missing cases, opposite treatment** (the "modeled for all flavors or not at
all" rule, `dynes-augmentation` D8/D19):
- **Half-specified flavor** (keyed in one sub-model list, omitted from the other):
  *complete* with the default above, and **warn** (naming layer, flavor,
  sub-model, and the default applied) — the warning fires at **each consumer
  entry**, see below.
- **Flavor absent from both** on a **modeled panel** layer: *error* — no default. A
  modeled panel layer must model every flavor its wave-diff produces (the augmenter
  must place events of each). This is **panel-gated**: on an **RE** focal layer,
  modeling a subset of flavors stays legal (unmodeled data-flavors update state —
  unchanged `flavored-processes` behavior).

**Where it lives — once, at the consumer entry, NOT inside the walk handle.** The
augmenters are the proof: only `augment_seq_sim()` drives `walk_open`;
`augment_seq_mcmc()` evaluates through an injected `make_proposal_evaluator()`
closure (preprocess + likelihood, "never touches the pool API"),
`augment_seq_random()` draws over the flip set, and `evaluate_sequence_pool()` is a
batched C++ pass — **three of four consumers never call `walk_open`**. Completion
inside `walk_open` would leave the MCMC / random / pool paths on an *uncompleted*
spec, so a run mixing a sim draw with an MCMC or pool evaluation would disagree
fid-for-fid and silently bias estimates. Completion is therefore a **single spec
transform run at each consumer's entry** (`estimate_dynes()`, `simulate()`,
the shared augmenter `init()`), producing one completed spec that *every*
downstream path within that invocation shares — `walk_open`,
`make_proposal_evaluator`, the random augmenter, and the pool evaluator alike. The
completion **warning is emitted at each consumer entry** (not suppressed on
re-entry): routing the same half-specified spec through `simulate()` and then
`estimate_dynes()` warns each time, so the auto-supplied default is never silent on
any surface. Because `make_specification()` must be able
to *build* a half-specified spec for it to reach these consumers, the constructor
**relaxes** its current same-flavor-set abort (it records the gaps without
fabricating defaults); the single-process estimators re-impose the error at
estimation time on an unfilled gap — the abort **relocates** from construction to
estimation for the excluded path.

**`walk_open` asserts, and stays internal.** `walk_open()` **validates** that its
spec is generatively complete and errors otherwise (pointing to `simulate()` /
`estimate_dynes()`); it never performs completion. It remains an internal developer
substrate (not user-exported) in this change; a future export decision inherits the
assert contract, so a direct caller must pre-complete.

**Durable marking.** Completed fids are marked in the `process_map` (a `completed`
logical column beside `coupled`, D3) and rendered in the specification / result
print, so a user inspecting a fitted DyNES or simulated object sees which
sub-models were auto-supplied — the per-consumer-entry warning is not the only
record.

*Rejected:* completion inside `walk_open` (misses the three non-`walk_open`
consumers — the MCMC augmenter is the counterexample); completion baked into
`make_specification()` (the *same* rate-only spec must estimate as rate-only under
`estimate_dynam()` yet simulate with a uniform choice — completion is
per-consumer-purpose, not a property of the object); rejecting rate-only DyNAM on
the generative surface (a uniform choice is a valid zero-parameter model and
friendlier than an abort); per-open completion in the ABEM loop (warning spam plus
a fid-consistency hazard across the augmenter / evaluator split — complete once,
share the result).

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
  external drivers of `multi-process-walk`, the batched `evaluate_engine()` reads
  the merged walk's per-fid outputs, and the general `simulate()` moved to the
  `process-simulation` change. Its "per-event simulation hook" framing and its
  modification of `preprocess-output-writers` are dropped.
- **Completion drift across the four consumer paths** (D9) → a sim draw on a
  completed spec while an MCMC / random / pool evaluation runs on an uncompleted
  one would disagree fid-for-fid and silently bias estimates. Mitigation: one
  completion transform produces a single completed spec that every path consumes,
  and `walk_open` **asserts** completeness — any path that skipped the transform
  fails loudly rather than evaluating a mismatched fid set.

## Migration Plan

Purely additive surface; the merged-walk refactor replaces the two-walk
internals behind unchanged single-process/flavored behavior (baseline-gated).
Rollback is reverting commits; no data-format or baseline impact.

## Open Questions

- Whether the batch preprocessing driver is literally a replay driver over the
  walk handle or a sibling over a shared stepper is an implementation choice
  measured at task time (replay-driver purity vs. the batch path's current
  performance).
- Whether effect-statistic dedup crosses mode-pair blocks when a statistic
  reduces to a *shared mode's marginal* (D8): a director in-degree read by both
  an advice block and a nominations block is one director-length vector, and the
  mode map already shares nodal *state* views across layers on a shared side.
  Compute-once-across-blocks vs recompute-per-block is a task-time measurement,
  gated the same way D3c's atom pooling is (worth it only if the shared
  computation is a substantial share of the block's work).
- Whether fid separability (D4) should be computed as **coupled OR
  on-a-modeled-panel-layer**, not `coupled` alone. D9's uniform-choice completion
  makes reads-nothing panel fids common, so a modeled panel flavor's own completed
  fid could be marked separable purely because its formula reads nothing (it is
  rescued today only when a derived support constraint reads its own layer's
  state). This interacts with `dynes-augmentation` D19's "all fids separable ⟺ no
  modeled panel process" equivalence; deferred, but settle it before the coupling
  column is consumed by `estimate_dynes()`.

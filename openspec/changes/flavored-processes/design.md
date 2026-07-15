## Context

`refactor-single-data-object` reserves `ties$flavor` and lets `make_specification()`
accept a flavor-keyed formula list with **exactly one** modeled flavor (its D19), enough
to reproduce the legacy Fisheries filtered-dependent-events pattern. The dev plan
(`goldfish_asta/code/plan/goldfish_dev_plan.md`) and the 2026-07-03 explorations
(`.plan/OQ_relational_states.md`) define the full mechanism: relational states as
competing processes, K flavors = K parallel processes over one layer, each with derived
support constraints from the tie state. The support-constraint machinery already supports
multiple derived objects in `plan$derivations`, `tie(L)` atoms reading the modeled
layer's state are legal (only mask-reading atoms are forbidden), and mask flips already
segment the right-censored timeline (the D12 interval machinery). The open questions
A1–A5 in `.plan/OQ_relational_states.md` were answered by the user in that document;
this design records those answers as decisions.

Sequencing: implementation starts after `refactor-single-data-object` lands (stocnet
input, flavor seam, layer-info metadata). The follow-up `dynes-augmentation` change
consumes this change's specification surface unchanged.

## Goals / Non-Goals

**Goals:**
- K > 1 modeled flavors as parallel processes: per-flavor formulas, derived support
  constraints, per-flavor preprocessed objects, per-flavor factorized estimation.
- One preprocessing pass over all formulas with shared-effect deduplication.
- `add_flavor()` + layer-info flavor metadata (`flavor_style`, `values_equivalence`).
- A sectioned multi-process result object (per flavor; rate/choice within DyNAM).

**Non-Goals:**
- Panel-observed states, latent-sequence augmentation, MCEM (`dynes-augmentation`).
- Flavor-filtered effect arguments (`outdeg(L, flavor = "dissolution")`) — deferred
  pending a usage study (user answer A3).
- A joint multivariate C++ estimation pass over all flavors (user answer A1: gather
  format already stacks events with no collective gain; a default-format joint updater
  is complicated with unclear time gains — revisit only if profiling motivates it).
- Multistate (> 2 states) `values_equivalence` encodings (user answer A4: ambiguous in
  plain weights; only dichotomous mappings are supported, others warn).
- manynet upstream reservation of `flavor` (proposed separately, non-blocking).

## Decisions

### D1 — Positioning: flavored DyNAM/REM, not `model = "DyNES"` (user answer A1)
Stage A ships as flavored DyNAM/REM: the existing `estimate_dynam()` / `estimate_rem()`
surface accepts multi-flavor specifications; no new model name. The DyNES name is
reserved for the panel-augmentation model (`estimate_dynes()` in `dynes-augmentation`),
where it denotes genuinely new estimation, not a rename of observed-data competing
processes. *Rejected:* `model = "DyNES"` from the start with observed data as the
degenerate no-augmentation case — the OQ document leaned this way, but the user's A1
answer chose flavored DyNAM/REM: the factorized estimation is exactly K existing models,
and users understand "a DyNAM per flavor" without new branding.

### D2 — Gating math note verifies the factorization and the intercept bookkeeping (A5)
Task 1 writes the 1–2 page derivation: under fully observed time-stamped events the
joint likelihood factorizes per flavor, other-flavor events entering each flavor's
likelihood only as exogenous state changes and right-censoring boundaries. The note MUST
also settle `avg_active_actors`: the user's A5 answer expects the same value across
flavors because right-censored events enter the average, but the landed
`flat-preprocess-output` contract computes it over the **post-constraint** active set —
and each flavor's derived mask differs (creation counts non-ties, dissolution counts
ties). The note resolves which is correct for each flavor's intercept
(`log(n_dep_events / total_time / avg_active_actors)` must recover that flavor's
baseline rate over *its* risk set); the spec is written expecting per-flavor scalars
over per-flavor masks and is amended before implementation if the note concludes
otherwise. Everything downstream (intercepts, per-flavor `n_candidates`) hangs on this
note, so it gates all implementation tasks.

### D3 — One preprocessing pass; shared effects computed once; per-flavor outputs
Preprocessing runs all flavors' formulas together in one event-loop pass: the union of
effects is computed once (an effect appearing in several formulas contributes one
statistics column, referenced by each formula's effect map), and the writer emits
per-flavor `preprocessed.goldfish` objects. Routing per event: for **timed rate
sub-models** (DyNAM-rate, REM), an event of flavor g is dependent in flavor g's object
and right-censored in every other flavor's object (its rate integral has a boundary
there); for **ordered/choice sub-models**, other-flavor events carry no right-censoring
semantics — they only update process state (their statistic updates land in the flat
buffers as exogenous changes).

This single-pass walk is not merely an optimization: it is the **multi-consumer
generalization of the recipe loop** — one clock, one process state, N formula plans
reading it — that downstream consumers independently require: a future `simulate()`
must evaluate the rate and choice plans against the same state at the same clock (a
sequential sender-then-receiver draw), and the DyNES model-driven augmenter needs
K flavors × (rate + choice) plans in one walk. The implementation MUST therefore keep
the walk's consumer set general (per-plan effect maps over shared computed statistics,
per-plan emit routing), not a K-writers-only shortcut hard-wired to flavors. The shared
setup-context constructor extracted in `refactor-single-data-object`'s pre-work (its
D12 DRY pass) is the seam this generalization builds on.
*Rejected:* K independent preprocessing runs — recomputes
shared effects K times and walks the event stream K times for no benefit; the recipe
loop already merges multiple event streams by pointer.

### D4 — Per-flavor separate estimation; results as a sectioned list (A1, A5)
Each flavor's model is estimated separately on its preprocessed object with the existing
engines (`default`, `default_c`, `gather_compute`) — the factorization (D2) makes this
exact, not an approximation. `make_specification()` + multi-flavor estimation is thus a
wrapper over what could be K separate `estimate_*()` calls with derived constraints. The
result is a new container class holding one goldfish result per flavor (and per
sub-model for DyNAM), printed in cli sections per flavor. `coef()`, `vcov()`, `logLik()`
on the container dispatch per flavor (named by flavor key); the joint log-likelihood is
the sum. *Rejected:* one stacked estimation with block-diagonal parameters — identical
estimates by factorization, but complicates convergence control and standard errors for
zero statistical gain.

### D5 — Flavor metadata lives in layer info; `add_flavor()` is a thin goldfish verb (A2, A4)
`flavor_style ∈ {mutually_exclusive, redundant}` and the `values_equivalence` named
vector (e.g. `c(creation = 1, dissolution = 0)`) are **layer-info metadata**, not tie
columns — they describe the layer's state semantics once, not per row. `add_flavor()`
is goldfish-owned (user answer A2) and thin: it stamps `ties$flavor` from the update
values via the mapping and records mapping + style in the layer info; it does NOT
precompute constraints (specification time owns that, D6). Names in the mapping must be
syntactic R names (they become formula-list keys). Supported encodings: dichotomous
states only — increment ±1 layers and replace 1/0 layers; for anything else (non-±1
weights, multistate values) `add_flavor()` aborts and estimation-side inference warns
that updates will accumulate/replace raw state values, pointing to `weighted = FALSE`
as the usual compromise (user answer A4). Category-style flavors (labels not derived
from update values) remain a plain `mutate_ties()`-style data operation upstream.
*Rejected:* a data-frame mapping argument — the named vector is the R-conventional
shape for value→label maps and the names double as formula keys.

### D6 — Constraints derived at specification time; AND-composed with user constraints
For a `mutually_exclusive` layer, `make_specification()` derives per flavor:
`creation → ~ !tie(L)`, `dissolution → ~ tie(L)` (generally: the flavor mapped to value
v is supportable where the state is not already v). Each derived formula is AND-combined
with any user `support_constraint`, yielding K compiled masks in `plan$derivations` —
the existing machinery supports multiple derived objects, `tie(L)` atoms on the modeled
layer are legal, and each mask's flips segment that flavor's right-censored timeline
exactly as user constraints do. `redundant` style derives no constraint (repeated
same-direction events are meaningful). The specification print shows each flavor's
derived constraint alongside any user constraint. *Rejected:* deriving in
`add_flavor()` — constraints are model concerns, and deriving at data time would bake
them into objects reused across models.

### D7 — Default flavor inference with a visible assumption (A4)
A flavored formula list on an **unflavored** layer infers the mapping: increment layers
with ±1 updates → `+1 = creation`, `-1 = dissolution`; replace layers with 1/0 values →
`1 = creation`, `0 = dissolution` — each with a `cli_inform` stating the assumption. The
formula keys must then be exactly the inferred names. Ambiguous encodings (non-±1
increments, non-1/0 replaces, keys not matching) abort with guidance to `add_flavor()`.
An explicitly flavored layer never triggers inference — keys resolve against the
`flavor` values present.

### D8 — Unmodeled flavors update state only
A flavor present in the data but absent from the formula list contributes no dependent
events and no likelihood term; its events update process state (and right-censor timed
sub-models per D3, since they are boundaries for every modeled flavor's rate integral).
This is the single-flavor behavior from `refactor-single-data-object` D19 generalized to
K modeled keys — the `NA`-flavor convention is unchanged.

## Risks / Trade-offs

- **The avg_active_actors claim may be wrong in either direction** → the D2 math note
  gates implementation; the spec text marks the per-flavor expectation and the note's
  conclusion is folded in before any preprocessing task starts.
- **Sequencing risk: `refactor-single-data-object` is unimplemented (0/35 tasks)** →
  this change is spec-complete but MUST NOT enter `/opsx:apply` before that change's
  flip milestone lands; the flavor seam (single modeled flavor, layer resolution) is its
  deliverable.
- **Per-flavor RC duplication grows preprocessed size ~K×** for timed sub-models (every
  event appears in every flavor's object) → statistics are computed once (D3) and the
  flat buffers store only *changes*; K is small (typically 2). If profiling shows
  otherwise, a shared-buffer representation is a follow-up optimization, not a contract
  change.
- **Two sources of constraint truth (derived + user)** → composition is a plain AND at
  compile time and the print shows both; tests cover the interaction (user constraint
  tightening/contradicting a derived mask — a contradiction yields empty risk sets,
  caught by the existing preprocessing-time set-size validation).
- **Inference surprises on unflavored layers** → cli message states the assumed mapping
  every time; ambiguity aborts rather than guessing.

## Migration Plan

Purely additive: the only removed behavior is the "multiple flavor keys abort" guard
from `refactor-single-data-object`, replaced by working multi-flavor validation. No
deprecations; existing single-flavor and plain-formula specifications behave
identically. Rollback is reverting the change's commits — no data-format or baseline
impact (frozen baselines untouched throughout).

## Open Questions

*(none blocking design — the D2 math note is scoped as the gating first task, and its
only possible amendment is the per-flavor intercept bookkeeping detail)*

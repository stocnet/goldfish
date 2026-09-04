## Context

Snijders & Steglich (2017) formulate network–behavior coevolution as two coupled
actor-driven Markov processes observed at discrete panel waves: a **network
objective function** (who forms/dissolves ties) and a **behavior objective
function** (who changes their own attribute value, by one step at a time), with
ministeps of both kinds interleaved on one latent chain between waves.

`goldfish` already has the pieces this change needs to reuse rather than
reinvent, but none of them currently reach a *nodal* (per-actor, monadic)
process:

- `single-data-object` gives every layer an `observation` of `"event"`
  (continuous-time, fully observed) or `"panel"` (wave-observed, between-wave
  path latent) — this is already the event-vs-panel split Snijders–Steglich's
  chapter assumes for the network side.
- `multivariate-specification`'s `make_joint_specification()` composes two or
  more `make_specification()` processes over one shared mode-map object, with
  cross-process reads, a `(layer, flavor) → fid` lookup, and separability
  analysis that already distinguishes "estimation-separable but generatively
  coupled" from "genuinely coupled through a modeled panel layer."
- `dynes-augmentation`'s `estimate_dynes()` already does the hard part of the
  book's algorithm for the network side: latent between-wave sequence
  augmentation (ABEM), MCMC chain mutation, and model-driven sequence draws —
  this is the same machinery RSiena's ML estimator uses for panel SAOM.
- The DyNAM `rate`/`choice` split already generalizes an actor-driven,
  two-stage evaluation function (when does an actor act; what do they choose)
  to continuous time — which is exactly what a behavior objective function
  needs, just over a different choice set (the actor's own reachable states,
  not a set of alters).
- Nodal attributes that change over time already exist and are already read
  live by effects such as `ego()`/`alter()`/`similarity()` through the
  standard attribute-update-on-event mechanism (`link_events()`-driven
  covariates work this way today). Nothing about how those effects read
  attribute state is specific to the attribute being exogenously vs.
  endogenously (model-)driven.

What is genuinely missing is a way to declare a nodal attribute as a
**modeled** dependent process (its own rate/choice formulas, its own risk
set) instead of only an exogenously-updated covariate, and a **behavior
evaluation-function effect vocabulary** (shape, similarity-to-alters/average
alter) to put in those formulas.

## Goals / Non-Goals

**Goals:**
- A behavior layer: a nodal attribute declared as a modeled dependent process
  via the existing `make_specification()` `rate`/`choice` surface, reusing the
  DyNAM engine with the actor's own reachable states (one step up, one step
  down) as the choice set instead of a set of alters.
- A behavior evaluation-function effect family: linear and quadratic shape
  effects, and network-state-dependent effects (similarity to alters,
  average-alter value) computed from a composed network layer's current
  adjacency.
- Composition of one network process + one behavior process, both
  **event-observed** (continuous time), via `make_joint_specification()`, so
  ties can read current behavior state (selection) and behavior can read
  current network state (influence) — reusing the existing multi-process walk
  and its per-fid factorized estimation, with no new estimation algorithm.
- A worked vignette example reproducing a Snijders–Steglich-style
  selection/influence analysis.

**Non-Goals:**
- **Panel-observed (wave-based) behavior coevolution** — the setting the book
  chapter and RSiena's SAOM actually estimate, with both tie and behavior
  ministeps interleaved on `estimate_dynes()`'s latent augmented chain. This
  requires extending the DyNES augmentation/chain-sampling machinery
  (`getChainSample`-style draws, `walk_inject`) to interleave two
  heterogeneous ministep kinds (dyad and nodal), plus separability/support-
  constraint analysis for a nodal modeled panel layer. `dynes-augmentation`
  itself is still landing on this branch; layering a second latent-process
  kind onto its augmentation core belongs in a follow-up change once that
  machinery is stable (mirroring how `dynes-augmentation` itself carved
  `abmcem` out as a separate change for its own large chunk of scope).
- Modifying the existing network-side effects (`ego()`, `alter()`,
  `similarity()`, …) — they already read live attribute state generically;
  this change supplies a way to make that state model-driven, not new reader
  logic.
- DyNAM-i (interaction) processes as a coevolution partner.
- More than a single behavior layer composed at once (multi-behavior
  coevolution is a natural follow-up once the two-process case is solid).

## Decisions

### D1: A behavior layer is a nodal dependent process, declared like a network layer

A behavior layer is a `nodes.goldfish` attribute whose change events
(increment ±1, matching the book's one-step-at-a-time restriction) are
declared as the **focal** stream of a `make_specification(rate = ..., choice =
..., model = "DyNAM", layer = <attribute name>, data = ...)` call, exactly as
a network layer is declared today. `observation` on a behavior layer is
`"event"` for this change (continuous-time attribute-change events), reusing
the `single-data-object` vocabulary rather than inventing a third value.

*Alternative considered*: a bespoke `make_behavior_process()` constructor.
Rejected — it would duplicate `make_specification()`'s formula parsing,
validation, and `specification.goldfish` contract for no behavioral
difference; the only new thing is what the "layer" points at (nodal vs.
dyadic) and what its choice set is (D2).

### D2: Behavior's choice set is the actor's own reachable states, not a set of alters

The DyNAM engine's `rate` submodel is reused unchanged (when does this actor's
behavior become eligible to change). The `choice` submodel's risk set is
redefined for a nodal focal layer: instead of "all other actors" it is `{current
value − 1, current value + 1}` (clamped to the attribute's declared range),
matching the book's restriction that behavior changes by one step per
ministep. This is a risk-set-construction change, not a new likelihood or
optimizer — the existing choice likelihood already sums over whatever risk set
preprocessing hands it.

*Alternative considered*: model behavior change as a REM-style event stream
with the new value as a free covariate. Rejected — it loses the ±1 Markov
restriction that makes the evaluation-function effects (shape, similarity)
interpretable the way the book defines them, and it would not reuse the
existing rate/choice split.

### D3: New behavior evaluation-function effect family, same init/update contract

New effects live in `R/functions_effects_behavior.R`, following the existing
`init_DyNAM_choice.*`/`update_DyNAM_choice.*` naming and signature contract
(so they dispatch through the same effect-family machinery as every other
DyNAM effect): `shape_linear`, `shape_quadratic` (own current value and its
square), `similarity_alters` and `avg_alter` (read the composed network
layer's current row of ties and the alters' current behavior values). These
are genuinely new statistics, not reinterpretations of dyadic effects, because
they aggregate over an actor's neighborhood rather than over a dyad.

### D4: Composition reuses `make_joint_specification()` with a nodal fid

The behavior layer's specification composes with the network layer's
specification through the existing `make_joint_specification()` surface. This
requires `multivariate-specification`'s mode-set-identity conformance and
`process_map` fid vocabulary to admit a **nodal fid** — sender-mode-indexed
only, no receiver mode/dyad block — alongside the existing dyadic fids, and
`multi-process-walk`'s `stat_block` keying (`model, sub-model family,
mode-pair`) to accept a mode-only block for the behavior process. Cross-process
reads follow the existing rule: the network process's `similarity()`/`ego()`/
`alter()` effects reading the behavior attribute, and the behavior process's
new effects reading the network layer's ties, are both ordinary cross-process
reads already covered by the walk's per-event, computed-once statistics.
Because neither layer is panel-observed in this change's scope, both fids are
estimation-separable (per `multivariate-specification`'s existing rule) and
estimated independently via `estimate_dynam()` per process — no new
estimator.

### D5: No new effect logic for the network side

Confirmed by reading `functions_effects_DyNAM_choice.R`: `ego`/`alter`
effects (and `similarity`, built the same way) already take a live
`attribute` argument updated at each attribute-change event, regardless of
whether that event stream originates from `link_events()` (exogenous) or a
modeled behavior process (endogenous). Making a behavior layer's simulated/
estimated change events update the same `nodes.goldfish` column existing
effects already read is sufficient — no changes to `functions_effects_DyNAM_*`
are needed for selection effects to see model-driven behavior state.

## Risks / Trade-offs

- **[Risk]** Scoping to event-observed coevolution only means this change
  cannot reproduce the book's own worked examples, which are all panel/wave
  data (RSiena's standard format). → **Mitigation**: the vignette's worked
  example uses continuous-time-equivalent data (or wave data coarsened to
  entry/exit event times), and the proposal/design are explicit that
  panel-observed coevolution is a named follow-up, not a silent gap.
- **[Risk]** A ±1-only choice set may be too restrictive for attributes that
  genuinely jump by more than one unit between observations. → **Mitigation**:
  document the restriction plainly (it is also RSiena's restriction) and leave
  the risk-set width a parameter for a future change if users need it.
- **[Risk]** `multivariate-specification`'s mode-set-identity conformance and
  `multi-process-walk`'s block keying were designed for dyadic processes only;
  admitting a nodal-only block is a real extension of shared, already-shipped
  infrastructure, not an additive change confined to new files. → **Mitigation**:
  land the nodal-fid/block extension as its own early task with the frozen
  single-process preprocessing baseline as a regression gate (per
  `multi-process-walk`'s existing "byte-identical to pre-merge" guarantee),
  before any behavior-specific effect is added.
- **[Trade-off]** Reusing DyNAM's rate/choice engine for behavior (D1–D2)
  keeps this change small and consistent with the rest of the package, at the
  cost of not matching the book's ministep-alternation-probability
  formulation exactly (goldfish's continuous-time rate submodel plays that
  role instead). This is judged acceptable: it is the same trade-off goldfish
  already makes for network dynamics relative to RSiena.

## Open Questions

- Should the behavior attribute's valid range (for clamping the ±1 choice set)
  be declared explicitly by the user, or inferred from the observed data's
  min/max? Inferring risks an artificially narrow range from a short observed
  window.
- Does `estimate_dynam()`'s existing standard-error machinery need any
  adjustment for a nodal (rather than dyadic) risk set, or does it already
  generalize? To confirm during implementation against a hand-computable toy
  example.
- Exact scope of the panel-observed follow-up: does it belong inside
  `dynes-augmentation` (as an extension before that change archives) or as its
  own change afterward? Depends on `dynes-augmentation`'s state when this
  change is ready to implement.

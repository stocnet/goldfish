# active-availability-stat Specification

## Purpose
Define how preprocessing emits per-recipe-loop availability as an encoding-aware
statistic: one `active_sender` object from the sender loop and one folded
`active_dyad` object from the dyad loop, with per-family folding, opportunity
absorption, sender-loop row-reduction, and a homogenized name consumed through
accessors across all engines. Created by archiving change support-constraint-as-stat.
## Requirements
### Requirement: Availability output keyed by recipe loop
Preprocessing SHALL emit exactly ONE availability object per recipe loop:
`active_sender` (a length-n1 logical stat) from the sender recipe loop
(DyNAM-rate, DyNAM-rate-ordered), and `active_dyad` (the folded effective dyadic
availability, at its minimal encoding) from the dyad recipe loop (DyNAM-choice,
DyNAM-choice-coordination, REM-rate, REM-rate-ordered). These SHALL be the ONLY
availability objects stored on the preprocessed object or passed to estimation:
`presence1`/`presence2`, `active_mode1_*`/`active_mode2_*`, `active1`/`active2`,
the opportunity list, the sender gate, and a raw `support` mask SHALL NOT be
stored or passed. The folding operands live in loop state only — the intersection
is computed during the event loop and its net effective flips are emitted
(intersections are not invertible, so operands cannot be re-derived downstream).

#### Scenario: sender loop stores only active_sender
- **WHEN** a DyNAM-rate model is preprocessed (with or without a
  `support_constraint`)
- **THEN** the preprocessed object carries `active_sender` and no
  `presence1`/`active_mode1_*`/`active1`/sender-gate object.

#### Scenario: dyad loop stores only active_dyad
- **WHEN** a DyNAM-choice or REM model is preprocessed (with or without a
  `support_constraint` or opportunity list)
- **THEN** the preprocessed object carries `active_dyad` and no
  `presence2`/`active_mode2_*`/`active2`/opportunity/raw-support object.

#### Scenario: unconstrained objects are today's presence data renamed
- **WHEN** an unconstrained model with composition changes is preprocessed
- **THEN** `active_sender` (sender loop) carries exactly today's sender presence;
  `active_dyad` at the alter encoding (choice) carries exactly today's receiver
  presence; `active_dyad` at the outer encoding (REM) carries exactly today's two
  presence vectors as its factors — and estimation results are bit-identical to
  the pre-change engine.

### Requirement: active_dyad encodings with accessor-mediated consumption
`active_dyad` SHALL be stored at the minimal encoding decided statically at spec
time from the atoms' axis-union kind, the per-family folded presences, and the
opportunity list: **scalar** (one logical), **ego** (length-n1 vector), **alter**
(length-n2 vector), **outer** (two factor vectors with cell = `f1[i] & f2[j]`;
chosen when both axes are dynamic but no point atom and no opportunity list is
present), or **point** (dense `n1 × n2` init plus flat `(node1, node2, replace)`
point flips; forced by a point-kind atom or an opportunity list). An
outer-encoded object SHALL NOT receive point flips (its updates are factor
flips). Consumers SHALL read availability through accessors (row read, cell
read, TRUE-count) and SHALL NOT branch on separate availability objects; a dense
`n1 × n2` value SHALL be allocated only at the point encoding. The object SHALL
be named `active_dyad` at every encoding — the name signals what the object is,
the encoding field signals its shape.

#### Scenario: broadcast-only constraint stores at a vector encoding
- **WHEN** a choice model has an alter-kind `support_constraint` and receiver
  composition changes
- **THEN** `active_dyad` is stored at the alter encoding (one length-n2 vector
  plus flips), no dense matrix is allocated, and the choice likelihood reads row
  `i` through the accessor.

#### Scenario: unconstrained REM with composition changes uses the outer encoding
- **WHEN** an unconstrained one-mode REM with composition changes is preprocessed
- **THEN** `active_dyad` is stored at the outer encoding (two factor vectors),
  no dense matrix is allocated, updates are factor flips only, and cell reads
  return `f1[i] & f2[j]`.

#### Scenario: point constraint or opportunity forces the point encoding
- **WHEN** a point-kind `support_constraint` atom (e.g. `tie(net)`) or an
  opportunity list is supplied
- **THEN** `active_dyad` is stored at the point encoding — a dense init plus
  net-effective point flips (already intersected with the folded operands) — and
  the likelihood reads it directly to drive `n_candidates`/`selected`.

#### Scenario: availability never rides inside the stats array
- **WHEN** any model is preprocessed
- **THEN** `active_sender`/`active_dyad` are separate objects (fields mirroring
  `stat_mat_*` plus an encoding field), `initialStats`/`stat_mat_init` carry no
  availability slice or column, and the effect count seen by estimation equals
  the number of estimated effects.

### Requirement: Per-family folding declared by the recipe constructor
The recipe constructor SHALL declare which operands fold into the loop's
availability object: the sender loop folds sender presence, the sender gate, and
ego/scalar support into `active_sender` (alter/point atoms via the row-reduction);
the DyNAM-choice dyad loop folds receiver presence, alter-support, ego-support
(as row flips), point-support, and opportunity into `active_dyad` but SHALL NOT
fold sender presence (the one-sided choice risk set conditions on the observed
sender); the REM and DyNAM-choice-coordination dyad loops fold BOTH presences and
all support atoms into `active_dyad` (their risk set is dyadic / two-sided).
A DyNAM-choice-coordination constraint SHALL additionally be symmetrised
(dyad `(i, j)` available iff both `(i, j)` and `(j, i)` are allowed) so both
directions of the mutual multinomial likelihood are masked consistently. A scalar
(global) constraint SHALL contribute a scalar flip only when the target object is
otherwise unmodified during preprocessing, else it rides folded at the object's
encoding.

#### Scenario: choice does not fold sender presence
- **WHEN** a one-mode DyNAM-choice model with composition changes and no
  constraint is preprocessed
- **THEN** `active_dyad` is at the alter encoding (receiver presence only), not
  outer/point — sender presence does not enter the choice risk set.

#### Scenario: REM folds both presences
- **WHEN** a one-mode REM with composition changes is preprocessed
- **THEN** `active_dyad` reflects `presence1[i] & presence2[j]` at every event
  (outer encoding when no point residual exists).

#### Scenario: coordination folds both presences and symmetrises the constraint
- **WHEN** a DyNAM-choice-coordination model with a `support_constraint` is
  preprocessed
- **THEN** `active_dyad` reflects
  `presence1[i] & presence2[j] & support[i, j] & support[j, i]` at every event
  (dense point encoding, symmetric), so the two-sided likelihood masks
  `(i, j)` and `(j, i)` consistently and the estimate matches the from-scratch
  masked coordination fit.

#### Scenario: ego constraint in choice zeroes rows, validation guards observed events
- **WHEN** a DyNAM-choice `support_constraint` contains an ego-kind atom that
  gates out a sender who then appears as an observed sender
- **THEN** the constraint folds into `active_dyad` as row flips and the existing
  observed-event-excluded validation fails fast at preprocessing.

### Requirement: Sender-loop dyadic constraints consumed via row-reduction
The system SHALL accept — NOT reject — a dyadic (point-kind) or alter-kind
`support_constraint` on a sender-loop model, consumed as
`active_sender[i] = presence1[i] ∩ ego/scalar-support ∩ (∃j: support[i,j] &
receiver-availability[j])`, preserving the shipped `mask_to_sender_gate`
semantics and matching the joint-specification definition
(`presence1 ∩ rowSums(active_dyad) > 0`). The dyadic state and per-row counters
SHALL live in loop state only; `active_sender` flips SHALL be emitted only on
0 ↔ positive row-count crossings. A one-time informational message
(`cli_inform()`) at specification validation SHALL explain the reduction and the
cheaper ego-kind reformulation, qualified as equivalent only under static
receiver composition.

#### Scenario: dyadic constraint on a rate-only model is accepted
- **WHEN** `estimate_dynam(..., sub_model = "rate", support_constraint = ~ tie(net))`
  is called
- **THEN** estimation proceeds (no error), and the per-event gated sender set
  equals `rowSums(support & receiver_availability) > 0` — the pre-change
  `mask_to_sender_gate` values — with coefficients matching the predecessor to
  1e-6.

#### Scenario: reduction message informs without overclaiming
- **WHEN** a point-kind `support_constraint` is validated for a sender-loop model
- **THEN** one `cli_inform()` message states the "has ≥ 1 available receiver"
  consumption and suggests an ego-kind reformulation as equivalent only when
  receiver composition is static; no warning and no error is raised.

#### Scenario: output buffer carries only crossings
- **WHEN** support cells flip for sender `i` without its available-receiver count
  crossing zero
- **THEN** no `active_sender` update is emitted for `i`.

### Requirement: Opportunity list absorbed in the main loop and applied in order
The opportunity list SHALL be consumed during the main preprocessing loop and
merged into the `active_dyad` point buffer (forcing the point encoding), NOT
re-derived per estimation iteration. For each dependent event `e` with sender
`i = sender(e)`, preprocessing SHALL emit point updates
`(node1 = i, node2 = j, replace = 1)` for every `j` allowed by
`opportunities_list[[e]]` and the folded receiver availability. The per-event
update slice SHALL be applied before that event's likelihood, and the first
dependent event's opportunity SHALL be in `active_dyad_init`. The
`updateopportunities` per-iteration recompute and the `mask_to_opportunities`
adapter SHALL be removed, and the opportunity rejection on the `cpp` and
`gather` backends SHALL be lifted.

#### Scenario: opportunity produces per-event point updates
- **WHEN** a choice model is estimated with an `opportunities_list`
- **THEN** preprocessing emits, per dependent event `e`, the `(sender(e), j)` point
  updates for the allowed-and-available receivers into `active_dyad`, and no
  per-iteration `opportunities <- seq_len(n2) %in% opportunitiesList[[e]]` recompute
  runs at estimation time.

#### Scenario: first event's opportunity is in the init (ordering)
- **WHEN** `active_dyad` is consumed by the likelihood
- **THEN** for every dependent event `e` the availability the likelihood sees equals
  the from-scratch intersection including `opportunity[e]` — the first event's value
  taken from `active_dyad_init` and each later event's from its pre-likelihood update
  slice — with no off-by-one on event 1.

#### Scenario: opportunity reproduces the pre-change coefficients
- **WHEN** the same `opportunities_list` model is estimated before and after this
  change
- **THEN** the estimated coefficients agree to within 1e-6 on every backend
  (`r`, `gather`, `cpp`).

### Requirement: avg_active_entity declared by the recipe constructor
The rate-intercept denominator SHALL be stored as `avg_active_entity` (renaming
`avg_active_actors`), computed during the preprocessing loop from the maintained
availability object, with the reduction declared by the recipe constructor:
the sender loop counts `active_sender` (average active senders); the REM dyad
loop counts TRUE cells of `active_dyad` (average active dyads). It SHALL be
computed only when the recipe uses intercept scalars, and estimation SHALL NOT
recompute it (the `constrained_avg_active_actors` / `mask_to_sender_gate`
estimation-time recombination is removed).

#### Scenario: DyNAM-rate counts active senders
- **WHEN** a DyNAM-rate model with an intercept and a `support_constraint` is
  preprocessed
- **THEN** `avg_active_entity` is the event-averaged count of TRUE entries of the
  maintained `active_sender` (post-constraint), with no estimation-time
  recombination.

#### Scenario: REM counts active dyads
- **WHEN** a REM (timed, with intercept) is preprocessed
- **THEN** `avg_active_entity` is the event-averaged TRUE-count of the maintained
  `active_dyad` (via the encoding accessor, e.g. factor-count product at the
  outer encoding).

### Requirement: One homogenized availability name across all sites
Every site SHALL reference the `active_sender`/`active_dyad` objects (and their
`_init`/`_update`/`_update_pointer`/encoding fields) by these names — every
site that today references `presence1`/`presence2` or
`active_mode1_*`/`active_mode2_*` — the preprocessed object, the default-engine
`compute_step`, the R gather routines, the C++ estimators, and the output
writers. The object keeps its name at every encoding; consumers switch on the
encoding field, never on the name. Because this changes the
`preprocessed.goldfish` structure, the preprocessed format version SHALL be
bumped so stale objects supplied through the estimators' `preprocessed =`
argument (formerly `preprocessing_init =`) are rejected with the existing
outdated-format error.

#### Scenario: consistent naming end to end
- **WHEN** any engine or writer reads availability
- **THEN** it reads `active_sender`/`active_dyad` (not `presence1`/`presence2`,
  `active_mode1_*`/`active_mode2_*`, or `active1`/`active2`).

#### Scenario: stale preprocessed object rejected
- **WHEN** a preprocessed object produced before this change is passed to
  estimation via `preprocessed =`
- **THEN** it is rejected with the outdated-preprocessing-format error, prompting
  recomputation.

### Requirement: Structural self-tie exclusion is not folded into active_dyad
The likelihood loop SHALL keep the structural exclusion of self-ties (the
diagonal) in one-mode choice/REM models; it SHALL NOT be folded into
`active_dyad` nor contribute to its encoding decision.

#### Scenario: diagonal does not force a dyadic encoding
- **WHEN** an unconstrained one-mode choice model with receiver composition
  changes is preprocessed
- **THEN** `active_dyad` is at the alter encoding (not outer/point), carries no
  diagonal information, and self-ties are still excluded from every event's
  candidate set by the likelihood.


## MODIFIED Requirements

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
Each object SHALL be stored as its initial value, its flat update buffer and
its cumulative per-row pointer, and in no other form: a per-change list of
composition changes (`active_sender_changes`, `active_dyad_changes`) SHALL NOT
be stored beside the buffers, and no copy of the raw presences the fold
consumed SHALL be stashed on the output, on the support mask or elsewhere. A
likelihood engine SHALL receive its loop's object only: the sender loop's
engines take `active_sender`, the dyad loop's engines take `active_dyad` at
its encoding, whose outer form carries the sender factor as the object's own
second factor and whose point form carries no sender vector at all.

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

#### Scenario: composition changes are stored once, as buffers
- **WHEN** any model with composition changes is preprocessed
- **THEN** the output carries the `active_*_init`, `active_*_update` and
  `active_*_update_pointer` fields for each axis the loop stores and no
  `active_sender_changes` or `active_dyad_changes` list, and every backend,
  including the default R engine, maintains availability by walking the
  pointer buffer, never by filtering a per-change table by time at each event.

#### Scenario: no raw presence is stashed beside a fold
- **WHEN** a constrained model is preprocessed
- **THEN** the output's support mask carries the mask stream (its initial
  value, update buffer, pointer and kinds) and no `sender_presence_*` or
  `receiver_presence_*` field, and the folded availability is the only
  presence-bearing object on the output.

#### Scenario: an engine receives one object
- **WHEN** a DyNAM-rate model is estimated on any backend
- **THEN** the likelihood kernel receives `active_sender` and no receiver-axis
  availability; and **WHEN** a REM, REM-ordered or DyNAM-choice-coordination
  model whose `active_dyad` is point-encoded is estimated
- **THEN** the kernel receives the point grid and no separate sender vector,
  and the estimates equal those of the kernel that received both.

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
the encoding field signals its shape. The encoding rule SHALL apply to every
dyad-loop family alike: a REM or DyNAM-choice-coordination model whose
`support_constraint` is of alter, ego or global kind SHALL store `active_dyad`
at the outer encoding with the mask folded into the factor of its own axis,
and SHALL NOT allocate a grid for it.

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

#### Scenario: a vector-kind REM constraint stays at the outer encoding
- **WHEN** a REM with composition changes and `support_constraint = ~
  alter(attr) > 0` (or an ego-kind or global atom) is preprocessed
- **THEN** `active_dyad` is stored at the outer encoding, the receiver factor
  equals receiver presence AND the alter mask (the sender factor equals sender
  presence AND an ego mask), no `n1 × n2` object is allocated during the walk,
  each cell read equals the point fold's cell, and the estimates on every
  backend equal those of the point-encoded fold within 1e-10.

#### Scenario: a vector-kind coordination constraint symmetrises as a factor
- **WHEN** a DyNAM-choice-coordination model with `support_constraint = ~
  alter(attr) > 0` is preprocessed
- **THEN** `active_dyad` is stored at the outer encoding with both factors
  equal to presence AND the mask vector, so cell `(i, j)` is available iff
  both `i` and `j` are allowed, and no grid is allocated.

### Requirement: Per-family folding declared by the recipe constructor
The recipe constructor SHALL declare which operands fold into the loop's
availability object: the sender loop folds sender presence, the sender gate, and
ego/scalar support into `active_sender` (alter/point atoms via the row-reduction);
the DyNAM-choice dyad loop folds receiver presence, alter-support, point-support,
and opportunity into `active_dyad` but SHALL NOT fold sender presence or an
ego-kind atom (the one-sided choice risk set conditions on the observed
sender, so a sender-axis condition cannot change its likelihood; such an atom
is enforced by the observed-event check alone); the REM and
DyNAM-choice-coordination dyad loops fold BOTH presences and
all support atoms into `active_dyad` (their risk set is dyadic / two-sided),
an alter-kind or global atom into the receiver factor and an ego-kind atom
into the sender factor when the object is outer-encoded.
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
- **WHEN** a DyNAM-choice-coordination model with a point-kind
  `support_constraint` is preprocessed
- **THEN** `active_dyad` reflects
  `presence1[i] & presence2[j] & support[i, j] & support[j, i]` at every event
  (dense point encoding, symmetric), so the two-sided likelihood masks
  `(i, j)` and `(j, i)` consistently and the estimate matches the from-scratch
  masked coordination fit.

#### Scenario: ego constraint in choice zeroes rows, validation guards observed events
- **WHEN** a DyNAM-choice `support_constraint` contains an ego-kind atom
- **THEN** `active_dyad` is stored at the receiver axis's own encoding with no
  sender-axis fold (the rows the atom would zero belong to senders who never
  draw, so no row flip is stored), the choice estimates equal the
  unconstrained fit on a sequence whose observed senders are all allowed, and
  an observed sender the atom gates out fails fast at preprocessing with the
  empty-risk-set error (the gated sender has no allowed receiver, so the
  check fails on its count before it reads the observed cell).

### Requirement: One homogenized availability name across all sites
Every site SHALL reference the `active_sender`/`active_dyad` objects (and their
`_init`/`_update`/`_update_pointer`/encoding fields) by these names — every
site that today references `presence1`/`presence2` or
`active_mode1_*`/`active_mode2_*` — the preprocessed object, the default-engine
`compute_step`, the R gather routines, the C++ estimators, and the output
writers. The object keeps its name at every encoding; consumers switch on the
encoding field, never on the name. Each likelihood engine SHALL take exactly
the object of the loop it implements and SHALL NOT receive, update or read the
other loop's object. Because this changes the
`goldfishStat` structure, the preprocessed format version SHALL be
bumped so stale objects supplied through the estimators' `preprocessed =`
argument (formerly `preprocessing_init =`) are rejected with the existing
outdated-format error. Within a development series before a release the
version is not bumped for a retired field: an object carrying a retired
field is read and the field ignored, and the rejection applies only to an
object missing a field the engines read.

#### Scenario: consistent naming end to end
- **WHEN** any engine or writer reads availability
- **THEN** it reads `active_sender`/`active_dyad` (not `presence1`/`presence2`,
  `active_mode1_*`/`active_mode2_*`, or `active1`/`active2`).

#### Scenario: stale preprocessed object rejected
- **WHEN** a preprocessed object produced before this change is passed to
  estimation via `preprocessed =`
- **THEN** it is rejected with the outdated-preprocessing-format error, prompting
  recomputation.

#### Scenario: a sender-loop kernel has no receiver axis
- **WHEN** the DyNAM-rate or DyNAM-rate-ordered likelihood is evaluated on the
  compiled backend
- **THEN** its interface carries `active_sender` and no `active_dyad`
  argument, and the fit equals the R backend's within the backend-parity
  tolerance.

#### Scenario: the simulation reads the maintained object
- **WHEN** a constrained REM or choice process is evaluated by the stepping
  walk handle
- **THEN** the risk set is the fid's maintained availability read at its
  encoding; and **WHEN** the simulation driver evaluates a deferred fid (a
  DyNAM rate or choice whose family carries no effect, so it has no
  availability of its own)
- **THEN** its risk set is read from its sibling's live support mask at the
  mask's stored kind (the drawing sender's row for a choice, the sender gate
  for a rate) and presence, and in neither case is an `n1 × n2` grid
  composed from presence and a support mask at the call.

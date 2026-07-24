# support-constraint Specification

## Purpose
Define the `support_constraint` capability: a formula that restricts the per-event risk/choice
set for DyNAM-rate, DyNAM-choice / choice_coordination, and REM, via a live boolean mask
maintained during preprocessing. Created by archiving change support-constraint-risk-set.
## Requirements
### Requirement: support_constraint restricts the per-event risk set
When a `support_constraint` formula is supplied (via `make_specification()` or the `estimate_*()` surface), preprocessing and estimation for DyNAM-rate, DyNAM-choice / choice_coordination, and REM SHALL restrict the per-event risk/choice set to the candidates satisfying the constraint at the event time. Excluded candidates SHALL NOT be
materialized in the preprocessed statistics output (no post-hoc subsetting). A model without
a `support_constraint` SHALL be bit-for-bit unaffected by this capability.

#### Scenario: single binary effect constrains the choice set
- **WHEN** a DyNAM-choice model is estimated with `support_constraint = ~ tie(net)`
- **THEN** each event's choice set contains only receivers `j` with `net[i, j] != 0` at the
  event time, and the estimated coefficients match the post-hoc subsetting reference
  (`goldfish.latent::make_df_cstr`) to within 1e-6.

#### Scenario: unconstrained models reproduce the frozen baselines
- **WHEN** the coefficient-baseline and C++ golden tests run with `NOT_CRAN=true` after this
  change, with no `support_constraint` supplied
- **THEN** all baselines PASS (not SKIP) to within 1e-6 — the mask machinery is inert when no
  constraint is present.

#### Scenario: constraint applies to both submodels of a DyNAM
- **WHEN** a DyNAM specification with rate and choice submodels carries one
  `support_constraint`
- **THEN** the same constraint restricts the rate risk set (sender gate) and the choice set
  (receiver filter); there is no submodel-specific constraint.

### Requirement: Restricted boolean-tree constraint grammar
The `support_constraint` formula SHALL accept the restricted-operator grammar over effect
atoms and numeric constants:

```
bool_expr  := bool_expr & bool_expr | bool_expr | bool_expr | !bool_expr
            | (bool_expr) | comparison | effect        (bare effect ≡ effect != 0)
comparison := arith_expr <cmp> arith_expr              (cmp: > < >= <= == !=)
arith_expr := arith_expr <op> arith_expr | (arith_expr)
            | effect | constant                        (op: + - * /)
```

Comparisons MAY be effect-vs-constant or effect-vs-effect. Inside a constraint, `*` (and
`+ - /`) SHALL be ordinary elementwise arithmetic on the atoms' values — NOT the effects
formula's interaction expansion — and the user documentation MUST state this distinction
explicitly. Constructs outside the grammar (arbitrary function calls, `I()`, `if`,
non-effect symbols) SHALL be rejected with a single consistent `cli` error. Operator
precedence SHALL follow R's own parse of the expression.

#### Scenario: multi-term boolean constraint
- **WHEN** `support_constraint = ~ tie(net) & !same(dept)` is supplied
- **THEN** a dyad is in the risk set iff `tie(net) != 0` and `same(dept) == 0` at the event
  time.

#### Scenario: effect-vs-effect comparison
- **WHEN** `support_constraint = ~ indeg(net) > outdeg(net)` is supplied
- **THEN** the mask is evaluated per cell as the comparison of the two atoms' current
  statistic values.

#### Scenario: arithmetic star is elementwise, not interaction expansion
- **WHEN** `support_constraint = ~ ego(a) * alter(b) > 1` is supplied
- **THEN** the constraint evaluates the elementwise product of the two atom values per cell;
  no interaction term, main-effect expansion, or estimated column is created.

#### Scenario: out-of-grammar construct rejected
- **WHEN** `support_constraint = ~ log(indeg(net)) > 1` is supplied
- **THEN** parsing aborts with a `cli` error identifying the unsupported construct and the
  allowed grammar.

#### Scenario: bare effect is shorthand for a nonzero test
- **WHEN** `support_constraint = ~ tie(net)` is supplied
- **THEN** it is evaluated identically to `~ tie(net) != 0`; there is no separate single-atom
  code path.

### Requirement: Constraint atoms are non-estimated operands
Each effect atom in a `support_constraint` SHALL be parsed as an operand and seeded/updated
through the existing operand / `stat_state` registries (its existing `init_*` / `update_*`
functions), carrying `role = "constraint"` in `plan$effects`. Constraint-role atoms SHALL be
excluded from `initialStats` and from the output statistic columns while their `stat_state`
stays live across the event loop. An atom whose inputs read the availability mask / risk set
itself SHALL be rejected at parse time with a `cli` error (the mask may not depend on
itself), keeping the data flow a two-layer DAG: atoms → mask.

#### Scenario: constraint atom produces no estimated column
- **WHEN** a model with effects `~ inertia(net)` and `support_constraint = ~ tie(other)` is
  preprocessed
- **THEN** the output statistics contain exactly the `inertia(net)` column; `tie(other)`
  appears in `plan$effects` with `role = "constraint"` and contributes no column and no
  coefficient.

#### Scenario: atom state updated by the existing effect machinery
- **WHEN** an event updates the network read by a constraint atom
- **THEN** the atom's `stat_state` is updated by the same `update_*` function an estimated
  effect would use, and the mask re-evaluates at the touched cells.

#### Scenario: availability-dependent atom rejected
- **WHEN** a constraint atom's definition reads the active risk set (an availability-derived
  effect)
- **THEN** parsing aborts with a `cli` error explaining that a constraint may not depend on
  the risk set it defines.

### Requirement: Active mask stored at its axis-union broadcast kind
The active mask SHALL be represented as a broadcastable statistic — an initial
`n1 × n2` value (the support at `startTime`, analogous to `initialStats`) plus a
flat `(node1, node2, replace ∈ {0,1})` update buffer with a per-event pointer,
and a broadcast kind — NOT a materialized list of per-event dense matrices. The
broadcast kind SHALL be the axis-union of its inputs' broadcast kinds (the
interaction `axis_union_kind` rule): sender-axis inputs stored/updated at ego
(row) kind, receiver-axis inputs at alter (column) kind, global inputs at scalar
kind, and a dense n1×n2 value ONLY when a `point`-kind (genuinely dyadic) input —
a dyadic atom or the opportunity list — enters the constraint. Separable inputs
SHALL never materialize a matrix: they store at a broadcast or outer encoding of
the folded `active_dyad` (or fold into `active_sender` in the sender loop).

#### Scenario: presence-only mask stays separable
- **WHEN** a model has composition changes but no `support_constraint`
- **THEN** no n1×n2 mask matrix is allocated; the receiver filter reads
  `active_dyad` at its alter (choice) or outer (REM) encoding.

#### Scenario: ego-gate constraint stores at ego kind
- **WHEN** `support_constraint = ~ ego(active_flag)` (sender-axis atom only) is supplied
- **THEN** the mask stat is stored/updated at ego (row) kind — a length-n1 value and
  ego-kind updates; no dense matrix is allocated.

#### Scenario: dyadic atom forces the dense (point) kind
- **WHEN** the constraint contains a `point`-kind atom such as `tie(net)`
- **THEN** the mask stat is dense (point kind): a `n1 × n2` initial value plus
  `(node1, node2, replace)` point updates, and the receiver filter for sender `i`
  reads row `mask[i, ]` of the maintained mask.

#### Scenario: no per-event dense list is materialized
- **WHEN** the mask is preprocessed for a full event sequence
- **THEN** the preprocessed object carries the mask as one initial value plus a flat
  update buffer (not one dense `n1 × n2` matrix per event).

### Requirement: Mask assembly per model
The effective mask SHALL always conjoin sender presence (`active_1`), the support constraint
(`support`), and receiver presence (`active_2`); presence factors are never bypassed:

```
DyNAM-rate    (sender gate) :  active[i]  = active_1[i] & (rowSums(support[i, ]) > 0)
DyNAM-choice  (row filter)  :  cand[i, ]  = active_2    & support[i, ]
REM           (full matrix) :  mask[i, j] = active_1[i] & support[i, j] & active_2[j]
```

When `support` is absent the mask SHALL degenerate to the separable presence product. For
DyNAM-rate, the sender gate SHALL be maintained incrementally as a per-sender
allowed-receiver counter (a mask cell flip adjusts the sender's count; the gate is
`count > 0`), never recomputed by row reduction, and the compact reduced rate output SHALL
consume this gate so the rate path keeps its per-sender statistic while respecting a
dyadic constraint.

#### Scenario: rate sender with no allowed receivers is gated out
- **WHEN** at some event time a non-observed sender `i` is present but `support[i, ]` allows
  zero receivers
- **THEN** sender `i` is excluded from that event's rate risk set without error — the benign,
  documented gated-out case.

#### Scenario: absent node excluded regardless of the constraint
- **WHEN** the constraint allows dyad `(i, j)` but node `j` is not present (composition
  change)
- **THEN** `(i, j)` is not in the risk set — `active_2` is always ANDed in.

### Requirement: Sender-indexed-only specs take sender-axis constraints
For a specification whose submodels are all sender-indexed (DyNAM `rate` / `rate_ordered` with no choice formula), the `support_constraint` SHALL contain only sender-axis atoms (`ego`-perspective effects, degree `type = "ego"`, `global`); a dyadic (`point`-kind) atom
SHALL be rejected with a single consistent `cli` error asking for a sender-axis
formulation and citing known equivalences (e.g. `~ tie(net)` ≈ `~ outdeg(net) > 0`) with
their composition-changes caveat. The constraint SHALL NOT be rewritten automatically.
When the specification also has a dyad-indexed part, dyadic constraints remain legal and
the sender-indexed pass SHALL derive its gate from constraint-scoped auxiliary dyad state
(the atoms' `stat_state`, the mask, and the counter — no statistic columns),
self-contained within that pass.

#### Scenario: dyadic atom rejected in a rate-only spec
- **WHEN** `make_specification(rate = ~ 1 + outdeg, choice = NULL,
  support_constraint = ~ tie(net), ...)` is validated
- **THEN** it aborts with a `cli` error naming the dyadic atom and suggesting a
  sender-axis reformulation such as `~ outdeg(net) > 0`, noting the equivalence does not
  hold under composition changes.

#### Scenario: sender-axis constraint runs without a dense mask
- **WHEN** a rate-only spec supplies `support_constraint = ~ ego(active_flag)`
- **THEN** preprocessing stores the mask as a length-n1 vector, allocates no n1×n2
  matrix, and gates senders directly.

#### Scenario: rate estimated from a two-formula spec with a dyadic constraint
- **WHEN** a spec has rate and choice formulas with `support_constraint = ~ tie(net)` and
  only the rate submodel is estimated
- **THEN** the rate preprocessing maintains the constraint-scoped auxiliary mask and
  counter, gates senders correctly, and the estimates match a run where the choice pass
  was also executed.

### Requirement: Mask maintained incrementally, consumed per event
The mask SHALL be maintained as a live statistic: when a constraint atom's
`stat_state` cell changes, the boolean tree SHALL be re-evaluated at the touched
cells only, and a resulting flip SHALL be appended to the mask's flat update
buffer as a `(node1, node2, replace)` entry (or a broadcast entry at the mask's
kind), exactly as a statistic's delta is emitted. Consumers (the default engine,
the gather routines, and the C++ estimators) SHALL maintain the mask by applying
that buffer with the SAME flat-update / broadcast-apply routines statistics use,
and SHALL read the current maintained mask when building each event's candidate
set; `n_candidates` and `selected` SHALL be a pure function of (event, current
mask) with no incremental membership reindexing between events.

#### Scenario: incremental mask equals from-scratch evaluation
- **WHEN** the mask is maintained by applying its flat update buffer across a full
  event sequence
- **THEN** after every event it is elementwise equal to a from-scratch evaluation of
  the constraint expression on the atoms' current statistics (boolean equality,
  machine-exact).

#### Scenario: mask updates reuse the statistic apply routines
- **WHEN** a consumer applies the mask's flat/broadcast update buffer
- **THEN** it uses the same shared apply functions the statistic buffers use (no
  mask-specific copy of the flat-update or broadcast-apply logic).

#### Scenario: a mask flip does not ripple into event bookkeeping
- **WHEN** a mask cell flips between two dependent events
- **THEN** no `n_candidates`/`selected` value for already-processed events changes; only
  the next event's maintained-mask read reflects the flip.

### Requirement: Symmetric mask for coordination and undirected REM
For DyNAM `choice_coordination` and REM on an undirected network, the mask SHALL be
symmetric: the evaluator symmetrizes a point-kind mask as `mask & t(mask)` so a mutual
dyad is active only when both directions are allowed. (Directed-REM asymmetry is out of
scope — deferred to the `review-rem-directed` change.)

#### Scenario: one-directional allowance excludes the coordination dyad
- **WHEN** a `choice_coordination` model has `support[i, j] = TRUE` but
  `support[j, i] = FALSE`
- **THEN** the dyad `(i, j)` is not in the coordination risk set.

### Requirement: Mask flips segment the right-censored timeline
A mask flip SHALL be treated as a state event that creates a right-censored interval
boundary. The interval likelihood and intercept denominator SHALL integrate over the active
set as of each sub-interval: a flip that changes the active-set size flows into the
`avg_active_actors` denominator for the affected interval, and dyads leaving the risk set
(mask 1→0) stop contributing right-censored terms from the flip onward.

#### Scenario: flip feeds the intercept denominator
- **WHEN** a constraint atom update shrinks the active sender set between two dependent
  events
- **THEN** the time-weighted `avg_active_actors` uses the pre-flip size for the sub-interval
  before the flip and the post-flip size after it.

#### Scenario: departed dyads stop contributing
- **WHEN** a mask cell for dyad `(i, j)` flips 1→0 during a right-censored interval
- **THEN** `(i, j)` contributes to the interval likelihood only up to the flip time.

### Requirement: Preprocessing-time set-size validation
Preprocessing SHALL validate risk-set sizes under the mask and fail fast with `cli`
conditions, per event `e` with sender `i`, receiver `j`:

| case | condition | action |
|---|---|---|
| A | observed dyad excluded: `mask[i, j] == 0` | error |
| B | the dependent event's own sender has 0 allowed receivers | error |
| C | the risk set has exactly 1 candidate (forced choice) | warn |
| D | the whole risk set is empty at an event time | error |
| E | a node is never active across the whole sequence | warn |

Only the dependent event's own sender triggers A/B; non-observed senders with zero allowed
receivers are gated out silently (documented, with guidance on self-checks such as
inspecting per-event candidate counts).

#### Scenario: observed dyad excluded by the constraint
- **WHEN** a dependent event's `(sender, receiver)` has `mask[i, j] == 0` at its event time
- **THEN** preprocessing aborts with a `cli` error naming the event, dyad, and constraint.

#### Scenario: empty risk set
- **WHEN** the mask allows zero candidates at a dependent event's time
- **THEN** preprocessing aborts with a `cli` error (the softmax denominator is undefined).

#### Scenario: forced choice warns
- **WHEN** exactly one candidate is allowed at a dependent event's time
- **THEN** preprocessing emits a `cli` warning that the event contributes zero to the
  choice log-likelihood.

#### Scenario: never-active node warns
- **WHEN** a node is excluded by the mask at every event time in the sequence
- **THEN** preprocessing emits a `cli` warning naming the inert node.

### Requirement: n_candidates and selected reflect the constrained set
Under a `support_constraint`, `n_candidates[e]` SHALL count only the allowed candidates at
event `e` (rate: senders with at least one allowed receiver; dyadic models: allowed
(sender, receiver) pairs), and `selected[e]` SHALL be the chosen candidate's position within
the constrained active set. The intercept's initial value SHALL be computed from the
post-constraint counts. This is the contract downstream consumers (`goldfish.latent`) rely
on in place of post-hoc subsetting.

#### Scenario: n_candidates shrinks and selected reindexes
- **WHEN** a choice event has 10 present receivers of which 4 are allowed and the chosen
  receiver is the 3rd allowed one
- **THEN** `n_candidates[e]` equals 4 and `selected[e]` equals 3.

#### Scenario: constrained intercept matches a hand-computed value
- **WHEN** a rate model with a constraint is preprocessed on a small fixture with known
  active-set sizes
- **THEN** `avg_active_actors` equals the hand-computed time-weighted mean of
  post-constraint sender counts, and the intercept initial value
  `log(n_dep_events / total_time / avg_active_actors)` uses it.

### Requirement: Opportunity list deprecated in favour of support_constraint
The per-event opportunity list — supplied via `set_preprocessing(opportunities_list =)`, a preprocessing option, not a data component — SHALL be deprecated following the lifecycle process (`lifecycle::deprecate_warn()` pointing to `support_constraint`, badge, NEWS entry). During
the deprecation window it SHALL keep working by entering the mask as a `point`-kind input
(per-event sender-row override), always conjoined with `active_1`/`active_2`. Its
documentation SHALL describe the migration (an opportunity list is a `support_constraint`
reading an allowed-dyad network) and the rate gated-out-sender behaviour.

#### Scenario: opportunity list warns but still works
- **WHEN** a model is estimated with an `opportunities` list
- **THEN** a lifecycle deprecation warning names `support_constraint` as the replacement and
  the estimates are unchanged from the pre-deprecation behaviour.

#### Scenario: equivalent constraint reproduces opportunity-list results
- **WHEN** the same restriction is expressed once as an opportunity list and once as a
  `support_constraint` over an allowed-dyad network
- **THEN** the estimated coefficients agree to within 1e-6.

### Requirement: Specification-derived flavor constraints ride the user-constraint machinery

Derived flavor masks SHALL ride the user-constraint machinery: support-constraint
masks derived by `make_specification()` from a mutually exclusive flavored layer (per
the `flavored-processes` capability) are compiled, stored, and maintained exactly as
user-supplied `support_constraint` formulas are: each
flavor's derived formula is expressed in the restricted boolean-tree grammar over
`tie(L)` atoms reading the modeled layer's evolving state (mask-reading atoms remain
forbidden), AND-composed with any user constraint into one compiled mask per flavor,
stored as multiple derived objects in the plan's derivations, maintained incrementally
during preprocessing, and segmenting that flavor's right-censored timeline at every
flip. The preprocessing-time set-size validation SHALL apply per flavor — a flavor
whose combined mask empties its risk set at some event aborts with the existing
empty-set diagnostics naming the flavor.

#### Scenario: derived masks flip with the modeled layer's own events
- **WHEN** a creation event adds tie (i, j) on a mutually exclusive layer
- **THEN** the creation mask closes (i, j) and the dissolution mask opens it, each flip
  segmenting the respective flavor's right-censored timeline.

#### Scenario: contradiction with a user constraint is caught per flavor
- **WHEN** a user `support_constraint` combined with a flavor's derived mask leaves an
  event with zero allowed candidates for that flavor
- **THEN** preprocessing aborts with the existing empty-risk-set diagnostics, naming the
  flavor whose mask emptied.


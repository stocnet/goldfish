## MODIFIED Requirements

### Requirement: One merged single-clock walk serves all fids

Preprocessing of a multivariate specification SHALL walk the event sequence
once on a single clock, hosting the statistic blocks over one shared process
state. A statistic block is keyed by `stat_block = (model, sub-model family,
mode-pair)` — the sender mode for rate blocks, the `(sender mode, receiver
mode)` pair for dyad blocks — so a single-mode-pair join has exactly the two
blocks (sender-indexed and dyad-indexed) and a multi-mode-pair join has one
sender block per distinct sender mode and one dyad block per distinct mode-pair.
Per event, each block's effect statistics SHALL be computed once; consumer
routing SHALL go through a `(layer, flavor) → fid` lookup: the event is
dependent for its own `(layer, flavor)` fids, a right-censoring boundary for
every other timed rate fid regardless of layer or mode-pair, and state-only for
choice/ordered fids. Effect deduplication SHALL extend across processes within a
statistic block and SHALL NOT cross effect-dispatch families at the level of a
computed statistic. Where two blocks' effects are the same node-level quantity
read at different broadcast kinds — a node's in-degree taken on the sender axis
by a rate block and on the receiver axis by a dyad block — that PRE-BROADCAST
quantity SHALL be maintained once per event and projected to each block's own
kind, so the merged walk computes it once where separate per-family loops
compute it once each. The projection SHALL go through the shared
maintain-at-kind core, and each consuming block SHALL apply its own kind's
rules, including the one-mode diagonal a dyad-kernel statistic zeroes and a
sender-kernel one does not. Sharing SHALL be established by the quantity two
effects compute, never by the name they share: a windowed, weighted or
`type =`-qualified variant that computes a different value SHALL NOT share. The walk SHALL return one `goldfishStat`
object per fid, indexed by fid with the process_map attached, each passing the
engine-readiness checks.

The merged walk SHALL be the only batch preprocessing loop for every
specification whose descriptor `input_shape` is `standard`: a single-process
specification and a flavored specification SHALL preprocess through it as a
one-process join, and the package SHALL carry no separate per-axis recipe
loop. To serve those callers the walk SHALL support windowed effects (each
unit's derived windowed networks realized into the shared object registry and
their expiry streams merged into the schedule as ordinary covariate rows,
deduplicated by derived-object identity across units), an explicit
observation window (`start_time` / `end_time` applied to the shared schedule,
with every timed engine writing the closing right-censored row at the end
time), restricted opportunity sets, and every writer the preprocessing
controls select (`default`, `gather`, `db`). Single-process and flavored
specifications SHALL preprocess byte-identically to the outputs of the deleted
recipe loops, with the frozen 1e-6 coefficient baselines and the C++ goldens
as the gate.


#### Scenario: a shared node-level quantity is computed once across blocks
- **WHEN** a layer's rate formula names `indeg` and its choice formula names the
  same in-degree of the same object, and the specification is preprocessed
  through the merged walk
- **THEN** the in-degree is maintained once per event and projected to the rate
  block's sender axis and the dyad block's receiver axis, and the rate and
  choice statistics are byte-identical to their per-family values

#### Scenario: a shared name over a different quantity is not shared
- **WHEN** a rate formula names `indeg(net)` and a choice formula names
  `indeg(net, window = 5)`
- **THEN** the two are maintained separately, because the windowed term computes
  a different quantity

#### Scenario: each block keeps its own diagonal rule
- **WHEN** a shared node-level quantity is projected into a one-mode dyad block
- **THEN** that block's statistic carries its zeroed diagonal and the sender
  block's does not, the shared value being the pre-broadcast quantity alone

#### Scenario: a specification with no shared quantity is unchanged
- **WHEN** no effect of one block computes the same quantity as an effect of
  another
- **THEN** every block maintains its own statistics exactly as before, and the
  frozen coefficient baselines reproduce

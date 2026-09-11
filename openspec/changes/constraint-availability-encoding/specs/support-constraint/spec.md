## MODIFIED Requirements

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

Every case above is defined over the receivers that are both allowed by the mask AND
present at event `e`. The validation SHALL therefore read the node presence at each
event from the composition-change stream, for the choice family as well as the rate
family, and SHALL NOT hold either presence vector at its time-zero value.

#### Scenario: a choice constraint reports senders it gates out entirely
- **WHEN** a choice-family `support_constraint` leaves a present sender with no
  allowed receiver at any stored event, and that sender is never observed
- **THEN** preprocessing warns once, naming the senders, symmetrically to the
  rate family's report of present senders never at risk, and does not error

#### Scenario: a receiver that joins mid-sequence is validated as present
- **WHEN** a choice-family `support_constraint` is validated over a sequence in which a
  receiver enters the node set after the first stored event
- **THEN** that receiver counts as a candidate from the event it joins, so an observed
  dyad naming it is not reported as excluded, and it is counted among the present
  receivers the never-a-candidate warning is taken over

#### Scenario: a receiver that leaves mid-sequence stops being a candidate
- **WHEN** a receiver departs the node set partway through the sequence
- **THEN** it is excluded from every later event's candidate set, so an event whose only
  allowed receivers have all departed is reported as an empty risk set

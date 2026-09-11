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

#### Scenario: a choice constraint reports senders it gates out entirely
- **WHEN** a choice-family `support_constraint` leaves a present sender with no
  allowed receiver at any stored event, and that sender is never observed
- **THEN** preprocessing warns once, naming the senders, symmetrically to the
  rate family's report of present senders never at risk, and does not error

# single-data-object (delta)

## ADDED Requirements

### Requirement: The order among events sharing a timestamp is explicit and preserved

A data object SHALL allow a user to express an order among events that share a
timestamp, and that order SHALL be the order preprocessing and estimation use.
Events at equal times are otherwise ordered by whatever the underlying storage
happens to yield, which decides which event updates the process state first and
therefore what the next one observes. Because the true order is unknown, a
researcher treating it as missing data must be able to supply and vary it; the
package SHALL NOT choose an order on the user's behalf, nor randomize one
internally.

#### Scenario: a supplied order survives to estimation
- **WHEN** a user supplies two data objects differing only in the order of
  events sharing one timestamp, and fits the same model to each
- **THEN** each fit reflects the order supplied to it, and the fits differ
  wherever that order changes the process state a later event observes

#### Scenario: the order is inspectable before fitting
- **WHEN** a user examines a data object containing events at equal times
- **THEN** the order those events will be processed in is readable from the
  object, rather than being an emergent property of estimation

#### Scenario: no order is invented
- **WHEN** a user supplies data containing tied timestamps and no explicit order
- **THEN** processing is deterministic and reproducible across runs, and the
  package does not randomize among the tied events

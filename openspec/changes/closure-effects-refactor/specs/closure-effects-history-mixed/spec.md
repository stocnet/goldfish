## ADDED Requirements

### Requirement: history selects positions of one enumeration at construction
For every closure member that accepts `history`, the value SHALL be resolved
when the term is constructed into a filter over the positions of the
member's path shape, applied to the one enumeration every member shares:
`"pooled"` keeps every position; `"sequential"` keeps, on a creation, only
the position in which the new tie is the closing leg of the path and keeps
every position on a dissolution; `"consecutive"` keeps that position only
when the previous dependent event's receiver is the event's sender and the
two events are adjacent in the event order, read from the cache's
`last_sender`, `last_receiver` and `last_event_seq` fields. The per-event
update SHALL not branch on `history`. The `mixed_*` members keep
`"pooled"` and `"sequential"` only. Under every model family that accepts
`history = "consecutive"` (DyNAM choice, REM, coordination) the event order
SHALL be the walk's, so a consecutive two-path is counted under REM as it is
under DyNAM choice.

#### Scenario: pooled and sequential reproduce today's statistics
- **WHEN** `trans(net)`, `trans(net, history = "sequential")` and the four
  `mixed_*` members with `history = "sequential"` are preprocessed on the
  parity fixtures
- **THEN** every statistic and update stream is byte-identical to the
  pre-change one

#### Scenario: consecutive counts adjacent closings under REM
- **WHEN** the consecutive-history walk fixture is preprocessed as an REM
  with `trans(history = "consecutive")`
- **THEN** every event after the first closes exactly one two-path, as it
  does under DyNAM choice on the same fixture

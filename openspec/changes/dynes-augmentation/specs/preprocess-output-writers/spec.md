## MODIFIED Requirements

### Requirement: Extension points documented, not implemented
The writer contract documentation SHALL describe, without implementing them: an
alternatives-sampling gather writer (keeps rows for a sample of alternatives; requires
estimation adaptation) and parallel chunk preprocessing (chunk the event sequence by
time points, warm-start each chunk, run recipe + writer per chunk in parallel, merge in
`finalize()`). The **per-event simulation hook** SHALL be implemented on the recipe
loop: after the statistics update for event i and before advancing, a registered
callback SHALL observe the visible process state and MAY append events to the remaining
event stream; the hook contract (visibility, append semantics, interaction with
right-censoring) SHALL be documented on the recipe loop, and when no callback is
registered the loop SHALL behave identically to the pre-hook implementation.

#### Scenario: Contracts are documented
- **WHEN** the writer contract documentation (roxygen for `R/preprocess_writers.R` and
  the recipe loop) is inspected after this change
- **THEN** it describes the sampling writer and parallel chunking as future extension
  points with their interface obligations, and the simulation hook as an implemented
  contract.

#### Scenario: Hook sees state and appends events
- **WHEN** a callback registered on the recipe loop runs after event i's statistics
  update
- **THEN** it can read the process state as of event i and append a subsequent event
  that the loop then processes in time order.

#### Scenario: No registered hook changes nothing
- **WHEN** preprocessing runs without a registered simulation callback
- **THEN** outputs are identical to the pre-hook implementation (frozen baselines PASS).

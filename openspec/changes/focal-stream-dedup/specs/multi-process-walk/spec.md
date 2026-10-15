## ADDED Requirements

### Requirement: Each source event stream is on the joint schedule once

The joint schedule SHALL carry each source event stream key of the shared
registry at most once among its state-update streams, whichever part of the
schedule builder registers the stream's object: a unit's planned streams,
the stream of a focal layer no term planned, or the stream of an object a
support constraint reads. A process's dependent rows are not state-update
streams and are exempt, so a dependent stream MAY share its key with a
state-update stream of the same layer, in its own process or in another.
Each observed event of a stream is therefore applied to the shared process
state exactly once by the batch walk and by every replay over the same
schedule: the stepping walk handle, the rate-query replay, and the DyNES
candidate pass. The schedule builder SHALL skip a stream whose key is
already on the schedule, and SHALL abort with an internal error when a
stream key would be added twice. An object MAY carry several distinct
stream keys. The appended support-constraint streams remain those of
objects that only a constraint reads. A candidate's realizations are the
state-update rows of its focal layers, so there is one realization per
observed or drawn event.

#### Scenario: A constraint on its own focal layer that no term reads

- **WHEN** a specification's support constraint reads its focal layer `calls`
  and no formula term reads `calls`
- **THEN** the joint schedule carries each `calls` event as one dependent row
  and one state-update row, and the walk applies it to `calls` once

#### Scenario: A constraint on another process's focal layer that no term reads

- **WHEN** in a joint specification one process's support constraint reads
  another process's focal layer and no formula term reads that layer
- **THEN** the layer's stream is on the joint schedule once, and each of its
  events is applied to the shared state once

#### Scenario: A weighted constraint on the focal layer reads the observed weight

- **WHEN** a DyNAM choice specification with `choice = ~ tie(friendship)` and
  `support_constraint = ~ outdeg(calls, type = "ego", weighted = TRUE) < 2`
  is fitted on data where an actor calls once and then calls again
- **THEN** the second call is in its sender's support, because its weighted
  outdegree before the call is 1, and the fit completes

#### Scenario: The weighted threshold still excludes at the observed count

- **WHEN** the same specification is fitted on data where one actor calls
  three times
- **THEN** the third call is outside its sender's support, because its
  weighted outdegree before the call is 2, and the fit aborts with an empty
  risk set at that call's event

#### Scenario: A term on the focal layer keeps the planned streams

- **WHEN** a formula term reads the focal layer that its support constraint
  also reads
- **THEN** the joint schedule's state-update streams are exactly those the
  units' formulas planned, in the planned order, and no stream is appended
  for the constraint

#### Scenario: A repeated stream key aborts the build

- **WHEN** the schedule builder is asked to add a state-update stream whose
  key is already on the schedule
- **THEN** it aborts with an internal error of class
  `goldfish_duplicate_schedule_stream` naming the key and the object, and
  distinct keys on one object do not abort

#### Scenario: Candidate positions count each event once

- **WHEN** a DyNES candidate of such a specification is opened on a walk
  handle
- **THEN** its realizations number one per observed or drawn event of its
  focal layers, so the candidate's rate-query position `after = k` addresses
  the same event the MCMC augmenter's ordinal counts

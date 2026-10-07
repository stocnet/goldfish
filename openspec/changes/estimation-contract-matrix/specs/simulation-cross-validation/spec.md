## ADDED Requirements

### Requirement: The estimation contract is covered combinatorially

The test suite SHALL cover the estimation contract over its factor space —
model family (DyNAM rate, DyNAM rate-ordered, DyNAM choice, DyNAM
choice-coordination, REM, REM-ordered), backend (`r`, `cpp`, `gather`),
node sets (one-mode, two-mode), composition (static; nodes leaving and
returning), constraint kind (none; a static allowed network; a dynamic
object a formula term reads; a dynamic object no formula term reads; an
ego-only atom), flavor shape (none; a two-flavor container; a single
modeled flavor of a two-flavor layer) and the deprecated opportunity list
— with a generated case set in which every pair of factor values that the
package admits appears in at least one case, and every admitted
combination of composition, constraint kind and flavor shape appears in at
least one case. The factor space, its values and the rules for what the
package refuses SHALL be recorded as one combinatorial model file beside
the generated case table, and the tests SHALL read the table, so that a
change to the factor space is a change to the model and a regenerated
table, never an edit to a test. Every case SHALL assert the same arms:
goldfish's fit equals the classical oracle on the hand-built frame within
1e-6 where an oracle exists; the exported statistics frame equals the
hand-built frame on `(event, index_i, index_j)` and on every statistic
column; the three backends agree within 1e-6 on the case's data; and, when
the case carries a mask, the stepping handle replays the batch with at
least one excluded alternative. Coordination SHALL run every arm but the
oracle, plus the identity of an all-allowing constraint with the
unconstrained fit. A failing case SHALL be treated as a defect, never as a
number to refreeze.

#### Scenario: every admitted pair appears
- **WHEN** the generated case table is checked against the model
- **THEN** for every two factors, every pair of values the model's rules
  admit occurs in at least one row, and no row violates a rule

#### Scenario: the named triple is exhaustive
- **WHEN** the rows are projected onto composition, constraint kind and
  flavor shape
- **THEN** every admitted combination of the three occurs in at least one
  row, so the case the closed-form shortcuts are known to get wrong
  (composition change under a support constraint on a flavored layer) is
  fitted and compared on every backend the rows name

#### Scenario: a case runs the full contract
- **WHEN** a row names a DyNAM choice on the `gather` backend, one-mode
  data with nodes leaving and returning, a dynamic constraint on an object
  no formula reads, and a two-flavor container
- **THEN** the test fits the container on `gather`, compares each flavor's
  choice to a conditional logit on its hand-built frame within 1e-6,
  asserts the exported frame equals the hand-built frame, asserts `r` and
  `cpp` agree with `gather` within 1e-6, and replays the sequence through
  the handle with a positive exclusion count

#### Scenario: a flavored container with a user constraint estimates on every backend
- **WHEN** a row names a two-flavor container with a user `support_constraint`
- **THEN** the fit on the row's backend equals the oracle on each flavor's
  frame within 1e-6, the exported rows carry no disallowed dyad, and the
  other two backends agree within 1e-6

#### Scenario: an ordered family has a conditional-logit oracle
- **WHEN** a row names DyNAM rate-ordered or REM-ordered
- **THEN** the fit equals a conditional logit stratified by event over the
  senders at risk (rate-ordered) or the dyads at risk (REM-ordered) on the
  hand-built frame within 1e-6

#### Scenario: coordination is compared without an oracle
- **WHEN** a row names DyNAM choice-coordination
- **THEN** the exported frame equals the symmetrized hand-built dyad frame,
  the three backends agree within 1e-6, a constraint allowing every dyad
  reproduces the unconstrained fit within 1e-6, and the handle replays a
  masked row with a positive exclusion count

#### Scenario: the matrix stays affordable
- **WHEN** the suite runs with `NOT_CRAN=true`
- **THEN** the matrix tests report PASS, not SKIP, and their total wall-clock
  on the optimized build stays within the budget recorded with the change

### Requirement: The harness simulates two-mode and ordinal processes

The data-generating harness SHALL simulate, beyond the one-mode exact-time
process, a two-mode process with distinct sender and receiver sets (a rate
over senders, a choice over receivers, a REM over sender x receiver dyads,
self-dyads admitted), SHALL carry a node attribute an ego-only constraint
can read, an exogenous layer no formula reads and a layer a term reads, and
SHALL emit, from the same draw, the opportunity sets a choice row needs so
that the frame and the equivalent constraint are built from one source.
It SHALL build, by the process's own rule, the ordinal frames — one row per
sender at risk per dependent event for the ordered rate and one row per
dyad at risk per dependent event for the ordered REM, each with the chosen
row marked — and the symmetrized dyad frame coordination's frame-identity
arm reads. The harness SHALL NOT share state code with goldfish's walk.

#### Scenario: a two-mode sequence keeps the sets apart
- **WHEN** a two-mode sequence is simulated with receivers leaving and
  returning
- **THEN** no event names a receiver as a sender, an absent receiver
  appears in no choice or dyad row of its absence, and the rate frame has
  one row per present sender per interval

#### Scenario: the ordinal frames mark the observed row
- **WHEN** the ordered REM frame is built for a dependent event
- **THEN** it has one row per dyad at risk at that event, exactly one of
  them marked chosen, and the same rows the exact-time dyad frame has for
  that interval

#### Scenario: opportunity sets and the constraint agree
- **WHEN** a choice row is fitted with the harness's opportunity sets and
  again with the equivalent allowed-dyad constraint
- **THEN** the two fits agree within 1e-6 and both agree with the oracle on
  the frame built from the same sets

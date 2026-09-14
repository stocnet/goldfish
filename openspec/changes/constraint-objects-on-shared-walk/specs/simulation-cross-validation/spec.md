## ADDED Requirements

### Requirement: A seeded harness simulates sequences and builds the oracle frames by the process's own rule

The test suite SHALL provide a self-contained, seeded data-generating
harness that simulates exact-time relational event sequences — waiting
times drawn from the total rate over actors at risk, the sender drawn
proportional to its rate, the receiver drawn from the multinomial over the
allowed receivers — and that builds, inside the same loop and before each
event is applied, the two oracle frames: a rate frame with one row per
actor at risk per interval (`outcome`, `dt`, statistics) and a choice frame
with one row per allowed receiver per dependent event (`chosen`, `event`,
`receiver`, statistics). The frames SHALL apply the rule the process used:
a sender with no allowed present receiver contributes no rate row, an
absent node contributes no row on either axis, and every non-dependent
event (an exogenous constraint-object event, a composition change, an
unmodeled-flavor event) closes the open interval with a censored rate row
and opens a new one. Every statistic SHALL mirror its goldfish effect's own
update rule on a 0/1 state (present-tie in- and out-degree, reciprocity,
the two-path count for transitivity), never a proxy. The harness SHALL NOT
share state code with goldfish's walk.

#### Scenario: a simulated sequence is reproducible
- **WHEN** the harness is called twice with the same seed and arguments
- **THEN** the event streams and both frames are identical

#### Scenario: the frame encodes the risk-set rule
- **WHEN** a fixture carries a dyadic constraint under which some sender has
  no allowed present receiver at some interval
- **THEN** the rate frame has no row for that sender in that interval, and
  the choice frame lists only allowed receivers for every event

### Requirement: goldfish's estimator equals the classical oracle on the hand-built frame

For each cross-validation fixture, goldfish's fit SHALL equal, within the
classical 1e-6 discipline, a Poisson regression with a log-exposure offset
on the rate frame (`stats::glm`, live) for the exact-time rate and REM
families, and a conditional logit on the choice frame (`mlogit::mlogit`
with the receiver as the alternative, or `survival::clogit` stratified by
event, behind `skip_if_not_installed()`) for the choice family, with the
log-likelihoods differing by exactly the sum of the log interval lengths on
the rate side. Before any coefficient is compared, the rows
`compute_statistics(..., output = "data.frame")` exports SHALL equal the
hand-built frame on `(event, index_i, index_j)` and on every statistic
column, so a disagreement in risk-set filtering or statistics is reported
as a frame difference. The fixtures SHALL cover, at minimum: an
unconstrained baseline; mutually exclusive creation and dissolution flavors
under their derived masks; a constraint on a changing object no formula
term reads; time-varying node composition; and a single modeled flavor of
a two-flavor layer. `choice_coordination` SHALL be documented as excluded,
with the reason that it has no conditional-logit twin.

#### Scenario: the baseline rate equals a Poisson regression
- **WHEN** the unconstrained fixture is fitted with
  `estimate_dynam(sub_model = "rate")` and the rate frame with
  `glm(outcome ~ offset(log(dt)) + ..., family = poisson)`
- **THEN** the coefficients agree within 1e-6 and the log-likelihoods differ
  by the sum of the log interval lengths

#### Scenario: a constrained choice equals a conditional logit over the allowed set
- **WHEN** the fixture whose constraint reads an object no formula reads is
  fitted with `estimate_dynam(sub_model = "choice")` and the choice frame
  with `mlogit` over the allowed receivers
- **THEN** the exported statistics frame equals the hand-built frame row for
  row, and the coefficients agree within 1e-6

#### Scenario: composition change drops absent actors from both axes
- **WHEN** the composition fixture, where actors leave and return, is
  fitted and compared
- **THEN** absent actors have no rows in either frame, each presence flip
  opens a censored interval, and both families agree with their oracles
  within 1e-6

### Requirement: flavored processes agree with their standalone and de-flavored twins

The flavored fixtures SHALL agree within 1e-6, on every coefficient and
per-process log-likelihood, across three goldfish routes: the container
fit, a standalone single-flavor specification per flavor, and a de-flavored
data object in which each flavor is its own layer with a hand-written mask
(`~ !tie(<creation layer>)`, `~ tie(<creation layer>)`); each route SHALL
also agree with the oracle on that flavor's frame. On the
single-modeled-flavor fixture the unmodeled flavor's events SHALL appear as
censored rows in the rate frame and as state updates in the choice frame,
and the fit SHALL equal the container's corresponding blocks.

#### Scenario: three goldfish routes, one oracle
- **WHEN** the creation/dissolution fixture is fitted as a container, as
  two standalone specifications, and as two plain layers with hand-written
  masks
- **THEN** all three agree within 1e-6 per process and each agrees with the
  oracle on that flavor's frame

#### Scenario: one modeled flavor
- **WHEN** only `creation` is keyed on the two-flavor data
- **THEN** every dissolution event is a censored rate row and a state
  update, no dissolution parameter is estimated, and the creation estimates
  equal the container's creation blocks within 1e-6

### Requirement: the stepping handle replays every constrained fixture

The walk handle SHALL reproduce the batch per-fid quantities when the
observed sequence of every cross-validation fixture that carries a mask —
derived flavor masks, the constraint-only object, composition change — is
replayed through it, and the replay SHALL report that at least one
alternative was excluded from a compared risk set.

#### Scenario: replay with exclusion
- **WHEN** the constraint-only-object fixture is replayed through
  `walk_open()`/`walk_advance()`/`walk_inject()`/`walk_evaluate()`
- **THEN** every dependent event's evaluation equals the batch and the
  number of excluded alternatives is positive

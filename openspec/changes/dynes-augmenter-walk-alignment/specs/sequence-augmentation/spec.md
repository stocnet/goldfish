## ADDED Requirements

### Requirement: Augmenter support checks read the state strictly before the stamp

The support-legality replay used by `augment_seq_random()` and
`augment_seq_mcmc()` SHALL check each in-interval event of a candidate
sequence against the live support of the walk state strictly before the
event's stamp, with the node composition at the stamp included. This is the
state the batch walk evaluates an observed event at. The replay SHALL inject
the events sharing a stamp only after every one of them has been checked, so
that no event at a stamp informs the support check of another event at the
same stamp. The replay SHALL NOT apply an exogenous row stamped at an event's
time before checking that event.

#### Scenario: An exogenous row sharing the stamp is not applied before the check

- **WHEN** a candidate places an in-interval event at time `t`, an exogenous row
  of the constraint's object is also stamped at `t`, and that row would remove
  the event's dyad from the support
- **THEN** the legality replay accepts the event, because the support at the
  state strictly before `t` admits it

#### Scenario: Tied events are checked against the same state

- **WHEN** two in-interval observed events share a stamp, and injecting the
  first would remove the second's dyad from the support
- **THEN** the legality replay accepts both, because each is checked against
  the state strictly before the stamp

#### Scenario: Untied sequences keep their decision

- **WHEN** no event of the candidate shares its stamp with an exogenous row or
  with another event
- **THEN** the replay returns the same legality decision it returned before
  this requirement

#### Scenario: The legality walk is the likelihood's configuration

- **WHEN** the augmenters are built with a non-default preprocessing control
  (for example a `start_time`)
- **THEN** every legality walk is opened with the same preprocessing control
  the log-likelihood of the candidate is scored with

### Requirement: A rate query after an event reads every row before its stamp and the stamp's realizations through that event

A rate query at position `after = k` of a candidate sequence, where candidate
event `k`'s realization is stamped `s`, SHALL read the walk state with every
row stamped before `s` applied, the node composition at `s` included, and the
realizations stamped at `s` applied in schedule order up to and including
event `k`'s. No exogenous row stamped at `s` SHALL be applied, whatever its
position among the stamp's rows. A realization is any non-dependent row of a
focal layer, including a row of a flavor no process models. Two tied events
are two positions.

#### Scenario: Tied events are two positions

- **WHEN** two candidate events share a stamp `s` and rate queries are made at
  `after = k1` (the first) and `after = k2` (the second)
- **THEN** the first reads the state with only the first realization at `s`
  applied, and the second with both applied

#### Scenario: An exogenous row at the stamp

- **WHEN** a candidate event's realization and an exogenous row share a stamp
  `s`, whichever of their streams sorts first
- **THEN** a rate query after that event reads the state without the exogenous
  row applied

### Requirement: Tied observed relational events in a sim draw are forced

In `augment_seq_sim()`, when observed relational events of an interval share
a stamp, only the first SHALL be selected against the flip candidates. Each
later event of the stamp SHALL be placed as a forced step at the same time,
with no selection and no term added to the log proposal density, and no flip
SHALL be offered while one is pending. Under a user support constraint, the
support of every event of the stamp, the first included, SHALL be checked
exactly once, at the walk state strictly before the stamp with the node
composition at the stamp included. That is the state on the last clock call
before the stamp. A failed support read on an earlier call SHALL NOT strand
the attempt. An event without support at the last call strands the attempt.
The walk that reads this support SHALL be opened with the preprocessing
control the log-likelihood of the drawn sequence is scored with.

#### Scenario: A draw over tied events completes

- **WHEN** an interval holds two observed relational events at the same stamp
  and at least one flip remains to be placed
- **THEN** the sim draw completes, and both events are placed at the stamp
  with no flip between them

#### Scenario: The sibling is not informed by the first event

- **WHEN** a specification has a user support constraint, two observed
  relational events tie at `t`, and injecting the first would remove the
  second's dyad from the support
- **THEN** the second is placed, because its support is read strictly before
  `t`

#### Scenario: Support restored before the stamp

- **WHEN** a specification has a user support constraint, a relational event
  is pending, an exogenous row before its stamp removes its dyad from the
  support, and a later exogenous row still before the stamp restores it
- **THEN** the event is placed, because its support is read only on the last
  clock call before the stamp

#### Scenario: The first event is checked at its stamp

- **WHEN** a specification has a user support constraint, a relational event
  is selected, and an exogenous row between its selection and its stamp
  removes its dyad from the support
- **THEN** the attempt strands

#### Scenario: The draw's walk is the likelihood's configuration

- **WHEN** the sim augmenter is built with a non-default preprocessing control
  (for example a `start_time`) on a specification with a user support
  constraint
- **THEN** every walk a draw opens is opened with the same preprocessing
  control the log-likelihood of the drawn sequence is scored with

### Requirement: An observed event at a wave time is placed last in its interval

In `augment_seq_sim()`, an observed relational event stamped at its
interval's closing wave `w_k` SHALL NOT be weighted against the flip
candidates. It SHALL be selected only when no flip of its interval remains,
with probability 1 and no term added to the log proposal density.

#### Scenario: A draw with an event at the wave end completes

- **WHEN** an interval `(w_{k-1}, w_k]` holds an observed relational event at
  `w_k` and at least one flip to place
- **THEN** the sim draw completes, every flip of the interval is placed
  strictly before `w_k`, and the event is placed last

### Requirement: The sim augmenter evaluates no rate over a replayed prefix

`augment_seq_sim()` SHALL apply the observed events and earlier intervals'
flips that precede the interval being drawn without evaluating any rate or
choice for them. Each is applied over the state strictly before its stamp and
injected, as the time-anchored simulation applies observed stamps. Before the
interval being drawn opens, an attempt SHALL evaluate at most one rate round:
the one the simulation driver evaluates at the walk's opening state, before
the augmenter's first step. Once the interval is open, the driver evaluates
every rate fid at each of its steps, a breakpoint step and a tied sibling's
forced step included, and hands the augmenter only their totals; that
per-step round is not constrained by this requirement. The augmenter's own
rate and choice evaluations SHALL occur only for steps whose result enters
the proposal density, a selection or a support check. For a fixed seed, the
drawn times and the log proposal density SHALL be identical to those drawn
when the prefix was replayed through full driver steps.

#### Scenario: At most one round before the interval opens

- **WHEN** interval `k > 1` of a sim draw is attempted
- **THEN** each rate fid is evaluated at most once per attempt at a walk state
  earlier than the start of interval `k`, independent of the number of
  observed events and earlier flips before it, and no choice fid is evaluated
  there

#### Scenario: Seeded draws are unchanged

- **WHEN** a sim draw is made with a fixed seed on a fixture where no
  observed event shares a stamp with an exogenous row
- **THEN** its event times and `log_proposal` equal those recorded before the
  change

### Requirement: The sim augmenter evaluates each rate fid once per step

Within one step of a sim draw (one walk state), `augment_seq_sim()` SHALL
evaluate each rate fid's values at most once, and each choice fid's row at
most once per sender. Every candidate weight, relational weight and waiting
time read at that state SHALL use those values.

#### Scenario: Many candidates of one process

- **WHEN** a step has several candidate pairs on the same rate fid
- **THEN** that fid is evaluated once at that step

#### Scenario: A value read before the walk moves is not reused after

- **WHEN** within one clock call a relational event is selected, the walk is
  advanced to the state strictly before its stamp, and the node composition
  changes at the stamp (the only change an advance within one clock call can
  bring in, since an exogenous row before the stamp ends the call at its
  breakpoint)
- **THEN** the support check after the advance reads values evaluated at the
  advanced state

### Requirement: An uncoupled sim draw uses one walk

When no modeled layer carries a user support constraint, `augment_seq_sim()`
SHALL draw every interval of one sequence on a single walk, opened once per
draw. Without such a constraint, and with no actor that takes part in an
interval joining or leaving inside it, a step cannot strand and no interval is
ever redrawn, so no interval's starting state needs to be rebuilt. A strand on
the single walk SHALL raise an internal error of class
`goldfish_dynes_uncoupled_strand` and SHALL NOT restart the interval.

#### Scenario: Walk opens per uncoupled draw

- **WHEN** a sim draw is made on a specification with no user support
  constraint and three intervals that each carry flips
- **THEN** exactly one walk is opened for that draw

#### Scenario: An uncoupled strand aborts without a restart

- **WHEN** a sim draw is made on a specification with no user support
  constraint whose composition was not snapped to the waves, and an actor
  involved in an interval leaves inside it before its flip can land
- **THEN** the draw aborts with class `goldfish_dynes_uncoupled_strand`
- **AND** no interval is restarted

#### Scenario: A coupled draw still restarts per interval

- **WHEN** a sim draw is made on a specification with a user support
  constraint
- **THEN** each interval attempt opens its own walk, so a stranded interval
  can be redrawn from its starting state

### Requirement: An MCMC step opens at most two walks

Each move of `augment_seq_mcmc()` SHALL open at most two walks. One reads the
forward rate queries on the current sequence. The other is one pass over the
proposed sequence that both decides support legality and answers the reverse
rate queries, opened with the preprocessing control the log-likelihood is
scored with. A proposal found illegal SHALL be rejected with no reverse rates
returned and no log-likelihood scored.

#### Scenario: A legal proposal

- **WHEN** an MCMC move proposes a legal candidate on a specification with a
  user support constraint
- **THEN** two walks are opened for the move, and the candidate's
  log-likelihood is scored once

#### Scenario: A specification without a support constraint

- **WHEN** an MCMC move proposes a candidate on a specification with no user
  support constraint
- **THEN** at most two walks are opened for the move (the forward and the
  reverse rate queries), as before this change

#### Scenario: An illegal proposal

- **WHEN** an MCMC move proposes a candidate whose support check fails
- **THEN** the move is rejected, no reverse rate is returned, and no
  log-likelihood is scored for the candidate

#### Scenario: A flavored event is checked under its own flavor's support

- **WHEN** a candidate places a flavored event of a layer whose support
  constraint excludes it at that point, such as a creation before the
  dissolution that frees its sender under a max-outdegree-1 constraint
- **THEN** the one pass over the candidate reads that event's support
  through its flavor's processes and rejects the candidate

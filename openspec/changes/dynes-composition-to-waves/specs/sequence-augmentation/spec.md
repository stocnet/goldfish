## ADDED Requirements

### Requirement: Composition is snapped to the panel waves for involved actors

At the DyNES estimation entry, once the wave grid `w_0 < w_1 < ... < w_K` is
known and before the specification is completed, the system SHALL rewrite the
composition (node presence) of every actor involved in a modeled interval, so
that the actor is present over the whole interval. The rewrite SHALL run once
per fit, on the data that the target likelihood, generative completion and
every augmenter read, and SHALL NOT run inside an augmenter or per candidate.

An actor SHALL be involved in interval `k = (w_{k-1}, w_k]` when it is the
sender or receiver of an observed event of a modeled relational process with
time in `(w_{k-1}, w_k]`, or the `from` or `to` node of a flip of interval `k`
in a modeled panel layer's wave diff. An observed event of an exogenous
relational layer SHALL NOT make an actor involved. A receiver of a two-mode
layer SHALL be involved on that layer's second node set. Involvement SHALL be
determined from the data and the wave grid alone, without the augmentation
recipe or the completed specification.

For each actor, with `S(t)` its real presence (initial state, then every real
change at or before `t`), the rewritten presence `P(t)` SHALL be:

- inside an interval, `t` in `(w_{k-1}, w_k)`: present when the actor is
  involved in `k`, otherwise `S(t)`;
- at a wave `w_k`: present when the actor is involved in interval `k + 1`,
  otherwise `S(w_k)`;
- before `w_0` and after `w_K`: `S(t)`.

The actor's presence changes inside `[w_0, w_K]` SHALL be replaced by the
minimal change list that produces `P`. As a result, for an involved actor a
join in `(w_{k-1}, w_k]` takes effect at `w_{k-1}`, a leave in `[w_{k-1}, w_k)`
takes effect at `w_k`, and the result does not depend on the order of
change rows sharing a stamp. The rewrite SHALL edit only the composition
(`var == "active"`) rows of the data's `changes`, SHALL NOT edit `nodes`, and
SHALL leave untouched the rows of every actor whose `P` equals `S` everywhere,
the rows of every other variable, and every row outside `[w_0, w_K]`. When no
actor's `P` differs from `S`, the data SHALL be returned identical to its
input and no warning SHALL be raised.

When the rewrite changes any actor's presence, the system SHALL raise one cli
warning per fit with class `goldfish_composition_snapped_warning`. The warning
SHALL list each changed (actor, interval) with the real change times ignored,
and SHALL state that the moved actor's rate enters the compensator over the
whole interval and that observed events stamped at a wave can see an actor
moved to that wave one interval early.

This rewrite is an approximation: it SHALL NOT constrain an involved actor's
events to its real join and leave times.

#### Scenario: an actor joining at its first event's stamp is present from the interval start
- **WHEN** an actor joins at the stamp of its first observed relational event,
  inside interval `k`, and is absent before
- **THEN** the rewritten composition has the actor join at `w_{k-1}`, the
  warning names the actor, interval `k` and the original join time, and a
  seeded sim draw of interval `k` completes where it strands without the
  rewrite

#### Scenario: a join exactly at a wave belongs to the interval it closes
- **WHEN** an actor joins exactly at `w_k` and is the `from` node of a flip of
  interval `k`
- **THEN** the actor's join takes effect at `w_{k-1}`

#### Scenario: a join at a wave with involvement only in the next interval is kept
- **WHEN** an actor joins exactly at `w_k` and is involved in interval `k + 1`
  but not in interval `k`
- **THEN** the actor's composition rows are unchanged

#### Scenario: a flip's receiver is made present
- **WHEN** an actor joins inside interval `k` after the start of the interval
  and is the `to` node of a flip of interval `k`
- **THEN** the actor's join takes effect at `w_{k-1}`

#### Scenario: a two-mode receiver is snapped on its own node set
- **WHEN** a node of a two-mode layer's second node set joins inside interval
  `k` and is the `to` node of a flip of interval `k`
- **THEN** the join of that second-set node takes effect at `w_{k-1}`, and no
  first-set node with the same local index is touched

#### Scenario: a leave before a flip's end takes effect at the next wave
- **WHEN** an actor leaves inside interval `k` and is the `from` node of a
  flip of interval `k`
- **THEN** the actor's leave takes effect at `w_k`

#### Scenario: a leave at the interval start takes effect at the interval end
- **WHEN** an actor leaves exactly at `w_{k-1}` and is involved in interval `k`
- **THEN** the actor is present over interval `k` and its leave takes effect
  at `w_k`

#### Scenario: a leave and rejoin inside one interval keep the actor present after the wave
- **WHEN** an involved actor leaves and rejoins inside interval `k`, in either
  order of rows in the data
- **THEN** the actor is present over interval `k` and at `w_k`, and no leave
  is written at `w_k`

#### Scenario: an actor involved in two consecutive intervals stays present across the wave
- **WHEN** an actor is involved in intervals `k` and `k + 1` and is really
  absent at `w_k`
- **THEN** the actor is present over both intervals and at `w_k`, and no
  change is written at `w_k`

#### Scenario: an uninvolved actor keeps its real times
- **WHEN** an actor joins or leaves inside interval `k` and is involved in no
  interval
- **THEN** its composition rows are unchanged and the warning does not name it

#### Scenario: data without an involved in-interval change is untouched
- **WHEN** a DyNES fit runs on data whose composition changes, if any, all
  belong to uninvolved actors or lie outside `[w_0, w_K]`
- **THEN** the data the fit reads is identical to its input and no
  `goldfish_composition_snapped_warning` is raised

#### Scenario: the completion reads the rewritten composition
- **WHEN** a DyNES fit completes a missing rate on a relational layer, on data
  with an involved in-interval composition change
- **THEN** the pinned rate's risk-set size is computed on the rewritten
  composition

#### Scenario: the wave diffs do not depend on the rewrite
- **WHEN** the wave diffs used to determine involvement are compared with the
  diffs the augmentation recipe compiles after the rewrite
- **THEN** they are identical

#### Scenario: the warning renders through cli
- **WHEN** a fit rewrites the composition of two actors in two intervals
- **THEN** one warning of class `goldfish_composition_snapped_warning` is
  raised, and its output under a pinned cli context matches the recorded
  snapshot

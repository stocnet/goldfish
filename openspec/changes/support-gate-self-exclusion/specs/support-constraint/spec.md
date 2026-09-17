## MODIFIED Requirements

### Requirement: Mask assembly per model
The effective mask SHALL always conjoin sender presence (`active_1`), the support constraint
(`support`), and receiver presence (`active_2`); presence factors are never bypassed.
On a layer whose risk set excludes self-ties (one-mode and not reflexive), the self-dyad
`(i, i)` SHALL be excluded structurally, as a property of the model: the stored mask keeps
answering "is this dyad allowed" at every cell, diagonal included, and the exclusion is
applied wherever the mask is read as a risk set. With `self[i, j] = (i != j)` on such a
layer and `TRUE` otherwise:

```
DyNAM-rate    (sender gate) :  active[i]  = active_1[i] &
                                 (sum_j support[i, j] & active_2[j] & self[i, j] > 0)
DyNAM-choice  (row filter)  :  cand[i, ]  = active_2    & support[i, ] & self[i, ]
REM           (full matrix) :  mask[i, j] = active_1[i] & support[i, j] & active_2[j] &
                                 self[i, j]
```

When `support` is absent the mask SHALL degenerate to the separable presence product. For
DyNAM-rate, the sender gate SHALL be maintained incrementally as a per-sender
allowed-receiver counter over the receivers other than the sender itself (a mask cell flip
adjusts the sender's count, a flip of the sender's own self-dyad adjusts none; the gate is
`count > 0`), never recomputed by row reduction, and the compact reduced rate output SHALL
consume this gate so the rate path keeps its per-sender statistic while respecting a
dyadic constraint. A sender's rate SHALL be at risk only when its process's choice set is
non-empty at the same state.

#### Scenario: rate sender with no allowed receivers is gated out
- **WHEN** at some event time a non-observed sender `i` is present but `support[i, ]` allows
  zero receivers
- **THEN** sender `i` is excluded from that event's rate risk set without error — the benign,
  documented gated-out case.

#### Scenario: absent node excluded regardless of the constraint
- **WHEN** the constraint allows dyad `(i, j)` but node `j` is not present (composition
  change)
- **THEN** `(i, j)` is not in the risk set — `active_2` is always ANDed in.

#### Scenario: a sender whose only allowed receiver is itself is gated out
- **WHEN** on a one-mode layer a present sender `i` has `support[i, i]` allowed and no other
  allowed, present receiver — for example a creation flavor whose sender already holds a
  tie to every other present actor, since the self-dyad of a tie is always absent
- **THEN** sender `i` is excluded from that event's rate risk set, contributes no rate
  exposure over the interval, and is not counted in the rate event's `n_candidates`

#### Scenario: a separable constraint excludes the self-dyad without a dense mask
- **WHEN** a one-mode DyNAM carries `support_constraint = ~ alter(x) > 0` and sender `i` is
  the only present actor with `x > 0`
- **THEN** sender `i` is gated out while every other present sender is gated in, and the
  mask is still stored and maintained at alter kind

#### Scenario: a two-mode layer keeps every allowed cell
- **WHEN** a two-mode DyNAM rate's sender `i` has exactly one allowed, present receiver `j`
  whose index equals `i`
- **THEN** sender `i` is in the rate risk set, since the two indices name different node
  sets and no self-dyad exists

#### Scenario: a simulated sender never meets an empty choice set
- **WHEN** a free-running simulation of a one-mode flavored layer draws a sender from the
  creation rate
- **THEN** that sender's creation choice set contains at least one receiver, so no drawn
  event carries a missing receiver

### Requirement: One support mask per process, shared by its sub-models
A `(layer, flavor)` process SHALL maintain exactly one support mask, shared by every
sub-model of that process, because a `support_constraint` is a property of the process
and not of the sub-model family. A sender-indexed sub-model SHALL derive its gate from
that shared mask through the per-sender allowed-receiver counter, never by maintaining a
second mask and never by recomputing a row reduction per event. A constraint whose atoms
are all sender-axis SHALL maintain no dyad-shaped mask at all: its mask kind is ego or
scalar, and the gate is the mask conjoined with at least one present receiver other than
the sender on a layer that excludes self-ties.

#### Scenario: rate and choice of one layer share a mask
- **WHEN** a specification supplies both a `rate` and a `choice` formula for one layer
  together with a `support_constraint`, and is preprocessed
- **THEN** the constraint's atom pool is seeded once and its mask is maintained once for
  that layer, not once per sub-model family

#### Scenario: the rate gate equals the reduction of the shared mask
- **WHEN** the shared mask is maintained across a full event sequence, at each of the
  global, ego, alter and point kinds, on a one-mode and on a two-mode layer
- **THEN** at every event the sender gate equals a from-scratch
  `rowSums(mask & receiver-availability & self) > 0` computed on the same mask, with
  `self` the off-diagonal indicator on a one-mode layer and all `TRUE` on a two-mode one,
  boolean-exact, and the live walk's gate equals the batch gate at the same event

#### Scenario: a sender-axis-only constraint allocates no dyad mask
- **WHEN** `support_constraint = ~ ego(active_flag)` is supplied to a specification
  carrying a rate and a choice
- **THEN** the maintained mask is stored at ego kind and no dyad-shaped mask object is
  created for either sub-model

#### Scenario: flavored processes maintain one mask each
- **WHEN** a layer carries creation and dissolution flavors, each with its derived
  complementary constraint and a shared user constraint
- **THEN** each flavor maintains one mask covering its own sub-models, and the atom pool
  underlying them is walked once

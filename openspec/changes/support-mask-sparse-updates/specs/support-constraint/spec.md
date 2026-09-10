## ADDED Requirements

### Requirement: One support mask per process, shared by its sub-models
A `(layer, flavor)` process SHALL maintain exactly one support mask, shared by every
sub-model of that process, because a `support_constraint` is a property of the process
and not of the sub-model family. A sender-indexed sub-model SHALL derive its gate from
that shared mask through the per-sender allowed-receiver counter, never by maintaining a
second mask and never by recomputing a row reduction per event. A constraint whose atoms
are all sender-axis SHALL maintain no dyad-shaped mask at all: its mask kind is ego or
scalar and the gate is the mask itself.

#### Scenario: rate and choice of one layer share a mask
- **WHEN** a specification supplies both a `rate` and a `choice` formula for one layer
  together with a `support_constraint`, and is preprocessed
- **THEN** the constraint's atom pool is seeded once and its mask is maintained once for
  that layer, not once per sub-model family

#### Scenario: the rate gate equals the reduction of the shared mask
- **WHEN** the shared dyad mask is maintained across a full event sequence
- **THEN** at every event the sender gate equals a from-scratch
  `rowSums(mask & receiver-availability) > 0` computed on the same mask, boolean-exact

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

## ADDED Requirements

### Requirement: A candidate's composition matches the specification's composition

When the internal helper binds a completed joint specification to a
candidate sequence, it SHALL also compare the candidate's composition with
the specification's, and SHALL abort when they differ. The comparison SHALL
read only the composition (`var == "active"`) rows of the data's `changes`
and SHALL compare them in a normal form: times coerced to numeric, nodes
mapped to global node ids, values coerced to logical, rows sorted by time and
then node with a stable order. A `NULL` `changes`, a zero-row `changes`, and a
`changes` without composition rows SHALL all have the same, empty, normal
form. The comparison SHALL NOT compare the raw `changes` object, and SHALL NOT
compare nodal attribute rows other than composition. The specification's
normal form SHALL be computed once per bound specification, not per
candidate.

On a mismatch the helper SHALL abort with class
`goldfish_candidate_mismatch` through cli. The message SHALL name the
composition as the differing component, SHALL say that `estimate_dynes()`
may have snapped the composition to the panel waves, and SHALL name the
consistent pairings: the fit's specification with the fit's sequences, or
the authored specification with the original data.

#### Scenario: an encoding difference alone is bound
- **WHEN** a completed joint specification is bound to a candidate whose
  composition equals the specification's, but whose `changes` differ in
  encoding (`NULL` against zero rows, numeric against POSIXct times, label
  against index nodes, or a differing non-composition attribute row)
- **THEN** the candidate is bound without error

#### Scenario: every augmenter's candidate passes the guard
- **WHEN** each built-in augmenter draws a sequence for a fit whose
  composition was rewritten
- **THEN** binding that sequence to the fit's specification does not abort

#### Scenario: a candidate with another composition aborts
- **WHEN** a completed joint specification is bound to a candidate whose
  composition differs from its own
- **THEN** the helper aborts with class `goldfish_candidate_mismatch`, and its
  output under a pinned cli context matches the recorded snapshot

#### Scenario: mixing a fit's objects with the original ones aborts
- **WHEN** `compute_lik_seq()` scores the original data against a fit's
  specification, or a fit's augmented sequence against the authored
  specification, on data whose composition the fit rewrote
- **THEN** the call aborts with class `goldfish_candidate_mismatch`

#### Scenario: consistent pairings still score
- **WHEN** `compute_lik_seq()` scores a fit's augmented sequence against the
  fit's specification, or the original data against the authored
  specification
- **THEN** the call returns a log-likelihood without error

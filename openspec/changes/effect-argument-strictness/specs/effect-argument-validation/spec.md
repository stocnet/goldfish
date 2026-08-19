## ADDED Requirements

### Requirement: Effect argument names are matched exactly
The formula parser SHALL match an effect term's supplied argument names exactly
against the arguments that effect accepts. An unmatched name SHALL raise an
error naming the term and the argument. An unmatched name SHALL NOT be silently
discarded, and an unambiguous prefix of an accepted name SHALL NOT be resolved
to it.

#### Scenario: Unknown argument name errors
- **WHEN** a formula term supplies an argument name the effect does not accept
- **THEN** parsing fails with an error naming the term, the argument, and the
  accepted argument names

#### Scenario: Abbreviated argument name errors
- **WHEN** a formula term supplies an unambiguous prefix of an accepted argument
  name, such as `transformer` for `transformer_fn`
- **THEN** parsing fails rather than resolving the prefix

#### Scenario: A near-miss suggests the intended name
- **WHEN** an unmatched argument name is within a small edit distance of an
  accepted name, such as `transform_fn` for `transformer_fn`
- **THEN** the error suggests the accepted name

#### Scenario: Accepted names are unaffected
- **WHEN** a formula term supplies argument names the effect accepts
- **THEN** parsing proceeds unchanged and estimation results are identical

### Requirement: A retired argument name reports its replacement
The parser SHALL recognize argument names retired by an earlier rename and SHALL
raise an error naming the current spelling, rather than the generic
unknown-argument error. The retired-to-current mapping SHALL be data that a
later rename extends, not logic embedded in the error path.

#### Scenario: Retired name names its replacement
- **WHEN** a formula term supplies `subType`, retired in favor of `sub_type`
- **THEN** parsing fails with an error naming `sub_type` as the current spelling

#### Scenario: All five 1.7.0 renames are covered
- **WHEN** a formula term supplies `transformFun`, `isTwoMode`, `aggregateFun`,
  `ignoreRep`, or `subType`
- **THEN** the error names `transformer_fn`, `is_two_mode`, `summarizer_fn`,
  `ignore_repetitions`, or `sub_type` respectively

#### Scenario: Effect arguments abort where exported-function arguments forward
- **WHEN** a retired effect argument name is supplied inside a formula term
- **THEN** parsing aborts, unlike a retired argument of an exported function,
  which warns and forwards as a deprecated sentinel

### Requirement: Correcting a discarded argument may move a baseline only by derivation
A frozen baseline value SHALL be re-frozen only after the new value has been
derived in closed form and recorded, where correcting an argument name the
parser previously discarded changes that value because the discarded argument
selected a different statistic. The invariants that survive the change SHALL be
stated alongside the derivation.

#### Scenario: The DyNAM-i rate baseline is corrected with its derivation
- **WHEN** `ego(age, subType = "centered")` in the DyNAM-i M1 rate baseline is
  corrected to `sub_type`
- **THEN** the two intercepts are re-frozen, the derivation and the surviving
  invariants are recorded in the baselines README, and the log-likelihood and
  all seven slope coefficients are unchanged

#### Scenario: No other baseline moves
- **WHEN** the full suite runs after the corpus migration
- **THEN** the coefficient, global, and C++ golden baselines are unchanged and
  report PASS

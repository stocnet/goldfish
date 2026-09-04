## ADDED Requirements

### Requirement: DyNAM choice accepts type = "ego" effects natively

The DyNAM `choice` and `choice_coordination` sub-models SHALL accept effects specified with
`type = "ego"` in the formula, producing statistics identical to the equivalent REM-derived
ego expansion. The default perspective MUST remain `type = "alter"` so existing choice
formulas are unaffected.

#### Scenario: ego-type degree effect in a choice formula
- **WHEN** a user estimates a DyNAM choice model with a formula term such as
  `outdeg(network, type = "ego")`
- **THEN** the term is parsed and preprocessed without error and without any special model
  type (no `DyNAMRE`)
- **AND** the resulting statistic for each (sender, receiver) equals the sender-level
  (ego) statistic broadcast across that sender's available receivers, matching the REM
  `to_ego` expansion to within 1e-6.

#### Scenario: default perspective unchanged
- **WHEN** a choice formula term omits `type` (e.g. `indeg(network)`)
- **THEN** the effect is computed with `type = "alter"` exactly as before this change.

#### Scenario: two-mode ego guard preserved
- **WHEN** a `type = "ego"` effect that is invalid for a two-mode network is used on a
  two-mode choice model
- **THEN** estimation aborts with the same guard message REM already raises for that case.

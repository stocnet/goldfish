# choice-ego-effects Specification

## Purpose
TBD - created by archiving change refactor-formula-parsing. Update Purpose after archive.
## Requirements
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

### Requirement: DyNAM choice dispatches the ego() covariate effect
The DyNAM `choice` and `choice_coordination` sub-models SHALL dispatch the standalone `ego(attribute)` covariate effect through `init_DyNAM_choice.ego` / `update_DyNAM_choice_ego` — thin aliases of the REM-choice binding (`init_REM_choice.ego` / `update_REM_choice_ego`), mirroring how `init_DyNAM_choice.global` aliases the REM-choice global — so that `ego()` is a *known* effect that `create_effects_functions()` can resolve into an effect-function caller. This closes the dispatch hole that made `ego()` raise `Unknown effect ego` in choice, blocking it both as an interaction operand and as a `support_constraint` atom. Making `ego()` dispatchable SHALL NOT change identification: `validate_effects()` SHALL still reject a bare `ego()` main effect in choice/choice_coordination as unidentified (constant across the receiver alternatives, so it cancels in the multinomial softmax).

#### Scenario: ego() covariate resolves as an interaction operand
- **WHEN** a DyNAM choice model is estimated with a formula such as
  `callsDependent ~ inertia + recip + trans + ego(actors$floor):inertia`
- **THEN** the formula is parsed and preprocessed without the `Unknown effect ego` error, and
  the interaction product column is estimated, with its `ego()` operand kept as a
  fixed-coefficient design column per the interaction-operand rule.

#### Scenario: ego() atom in a choice-model support_constraint
- **WHEN** a DyNAM choice model supplies `support_constraint = ~ ego(active_flag) & tie(net)`
- **THEN** the `ego()` atom is seeded through the same dispatch (no `Unknown effect ego`) and
  the constraint restricts the risk set on the sender axis conjoined with the dyadic atom.

#### Scenario: bare ego() main effect still rejected
- **WHEN** a DyNAM choice formula uses `ego(actors$floor)` as a standalone main effect
- **THEN** `validate_effects()` rejects it as an ego-perspective statistic that is unidentified
  in the softmax, unchanged by this dispatch fix.

#### Scenario: ego() covariate matches the REM expansion
- **WHEN** the `ego()` covariate statistic is computed in a choice model
- **THEN** for each (sender, receiver) it equals the sender's attribute broadcast across that
  sender's receivers, matching the REM `to_ego` expansion to within 1e-6.


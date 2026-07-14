## MODIFIED Requirements

### Requirement: Plan registries derived from the existing link matrices
`build_update_plan()` SHALL derive normalized plan registries from the existing parser outputs (`eventsObjectsLink`, `eventsEffectsLink`, `objectsEffectsLink`). The parser SHALL resolve each effect through the term registry (`get_term_def()`) and the term constructor rather than string-built `update_*`/`init_*` discovery, so the resolved references and validity originate from the term definition. The registries SHALL be:

- `effects`: one row per effect — `gid` (integer key), `fid` (formula id, a single value in this change), `effect_name`, `stat_kind` (`"sender"` or `"dyad"`), references to the resolved update and init functions **obtained from the effect's `term_def` (via the constructed term)**, and fixed parameters.
- `objects`: one row per data object — `oid` (integer key), `name`, `kind` (`"network"`, `"attribute"`, `"global"`), the state-container key, `directed`, `is_two_mode`, and update semantics (`"increment"` or `"replace"`).
- `effect_objects`: one row per (gid, oid) pair with `object_order` giving the argument position.

A `gid` SHALL be unique per (canonical effect call, parameters, `stat_kind`) — the same effect call used with different statistic shapes SHALL receive distinct gids.

#### Scenario: Registries consistent with link matrices
- **WHEN** `build_update_plan()` runs on a parsed single-formula model
- **THEN** every non-NA cell of `objectsEffectsLink` corresponds to exactly one `effect_objects` row with matching `object_order`, and every effect column corresponds to one `effects` row

#### Scenario: Same call different shape gets distinct gids
- **WHEN** the same canonical effect call appears with `stat_kind = "sender"` and `stat_kind = "dyad"` (future multivariate case, testable via direct builder input)
- **THEN** two distinct gids are assigned

#### Scenario: Resolved references come from the term registry
- **WHEN** the parser resolves an effect for the plan's `effects` registry
- **THEN** the update/init references are those declared by the effect's `term_def` (via the constructed term), and no string-built `update_<model>_<submodel>_<name>` / `getS3method()` lookup is used to discover them

## Context

After `multimode-network-support`, DyNAMi is the only model family whose data
reaches estimation as a legacy `data.goldfish` environment: the constructors
(`make_groups_interaction()` + `link_events()` + `make_dependent_events()`)
fall back to the environment because `is_stocnet_assemblable()` refuses
interaction data, and the `preprocessInteraction` monolith
(`R/model_preprocess_group.R`) consumes that environment through the isolated
DyNAMi front-end left by `refactor-formula-parsing` (task 2.3e). The full
engine conversion is `refactor-dynami-engine` (post-release). Meanwhile the
single-object change's legacy-environment abort is deferred solely because a
public abort would break DyNAMi (its task 8.3 conformance note); the
2.0.0 release plan (2026-07-19) wants the abort in the release.

## Goals / Non-Goals

**Goals:** DyNAMi accepts the single stocnet data object on the public
`data =` surface; DyNAMi `make_data()` assembles to stocnet (env never
user-visible); the `is.environment(data)` abort lands on
`make_specification()`/`estimate_*()`; DyNAMi baselines reproduce to 1e-6;
load-time legacy fixtures cleaned up.

**Non-Goals:** converting `preprocessInteraction` to the recipe loop; the
shared `spec_map`/realizer unification; deleting the internal
`data_source_envir`/`is_legacy` seam; changing DyNAMi effects or the
`make_groups_interaction()` API (it keeps working, now feeding the stocnet
assembly). All of these are `refactor-dynami-engine`.

## Decisions

### D1 — Boundary-only: stocnet in, internal env bridge behind it
`estimate_dynami()` / `make_specification()` accept a stocnet; at the boundary
an internal bridge materializes the environment shape `preprocessInteraction`
expects and hands it to the untouched monolith. The bridge is created inside
the call, never accepted from the user, and is documented in code as the
temporary seam `refactor-dynami-engine` deletes with the monolith.
*Rejected:* converting the monolith now (that is the post-release change — a
half-migration of a loop about to be rewritten); keeping the public env
surface until the engine conversion (blocks the release's abort).

### D2 — Actors × groups is a two-mode stocnet under the mode map
The DyNAMi object is a multipartite stocnet per the `multimode-network-support`
representation: one `nodes` tibble with `mode` distinguishing actors from
groups, the interaction structure as a two-mode layer with disjoint
`info$sender`/`info$receiver` mode sets, composition/attribute changes in
`changes`. This matches the modelling already sketched in
`refactor-dynami-engine`'s design, so the object shape survives the later
engine conversion unchanged. *Rejected:* a DyNAMi-private object shape — it
would need its own validator and die with the monolith.

### D3 — Bridge equivalence is the correctness contract
The bridge's output environment must be indistinguishable, to the monolith,
from a constructor-built one: an exact-equivalence test (same fixtures built
both ways, compare the env components the monolith reads) backs the 1e-6
coefficient equivalence on the DyNAMi baselines. *Rejected:* asserting only
coefficients — a silent representational drift (e.g. event ordering) could
cancel out on one fixture and bite on another.

### D4 — The abort lands last, in this change
Once `make_data()` never returns an environment for any family, the public
guard `is.environment(data)` → `cli_abort()` naming `as_goldfish()` becomes
safe. It is the closing task, after the DyNAMi boundary is green. The
`single-data-object` delta removes the deferral exception recorded at the
`refactor-single-data-object` archive. *Rejected:* aborting earlier behind a
DyNAMi carve-out — exactly the fragile stamp hack the multimode design D7
rejected.

### D5 — Fixture cleanup, not fixture rewrite
`R/zzz_testthat_helpers.R` builds legacy-constructor fixtures at load time
under a quieted lifecycle context. DyNAMi fixtures route through the stocnet
boundary; the lifecycle quieting shrinks to whatever deprecation-cycle tests
still need. A full fixture modernization across the suite is not this change.

## Risks / Trade-offs

- **Bridge drift vs the monolith's expectations** (parse-time windowing
  `assign()`, `cleanInteractionEvents` ordering) → D3 exact-equivalence test
  plus the frozen DyNAMi baselines; the monolith itself is untouched.
- **Derived availability must reproduce the supplied opportunities list** (D6):
  a constructor-era `opportunities` list and the availability derived from
  composition state could disagree on edge cases (simultaneous joins/leaves,
  windowed groups) → the D3 bridge-equivalence test compares the materialized
  list against a constructor-supplied one on the fixtures; disagreements are
  surfaced, not papered over.
- **The abort is breaking for pre-1.9.0 saved objects** → `as_goldfish()`
  conversion already shipped; the error message shows the one-line migration;
  NEWS entry marks it BREAKING.

## Migration Plan

1. Grounding: map what the monolith reads from the environment (the bridge's
   contract); build the actors×groups stocnet fixture.
2. Assembly + bridge behind the D3 equivalence test; DyNAMi baselines PASS.
3. Public surface acceptance (`estimate_dynami()`/`make_specification()`).
4. The abort + fixtures cleanup; snapshot tests; milestone bump.
Rollback: revert the assembly branch and the abort — the constructor/env path
is unchanged underneath.

### D6 — Opportunities become derived availability, not an argument (2026-07-19)
The `opportunities` list is event-shaped state, not free input: when an
individual in a singleton gets the opportunity to join, the choice set is the
set of groups **available at that time** — derivable from the composition/
interaction state the object already carries. The public surface therefore
drops the explicit `opportunities` list in favor of the equivalent
**support-constraint / availability** expression (the machinery landed by
`support-constraint-as-stat`): availability is derived from the object's
composition state at event time, combinable with any user
`support_constraint`. The internal bridge materializes the monolith's
`opportunities` list from that derived availability — equivalence with a
constructor-supplied list is part of the D3 contract. *Rejected:* keeping the
list as an argument (duplicates state the object holds, and contradicts the
single-object principle); a new list-column on the object (not events-shaped
storage of something derivable).

### D7 — Both public surfaces land now (2026-07-19)
`make_specification()` gains the DyNAMi model classes alongside
`estimate_dynami(data = <stocnet>)` — a uniform 2.0.0 API across families.
The DyNAMi spec classes stay thin (their consumption is still the monolith via
the bridge); `refactor-dynami-engine` makes them load-bearing. *Rejected:*
estimate-only surface (would ship 2.0.0 with one family lacking the
specification workflow the docs teach).

## Open Questions

*(none — the 2026-07-19 explore session resolved both: D6 opportunities /
stream mapping, D7 spec-object path in scope.)*

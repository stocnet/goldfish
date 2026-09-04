## Why

The 2.0.0 single-data-object migration wants a hard rule: no legacy
`data.goldfish` **environment** survives at the public surface, so
`estimate_*()` / `make_specification()` can abort on `is.environment(data)` and
name a migration. That abort was **deferred** (`dynami-stocnet-boundary` design
D4) because its precondition is false: `make_data()` still **silently returns an
environment** for one class of valid-looking input, so a blanket abort would
break real workflows with no path forward.

The cause is node-set **name resolution**, not (as first suspected) event
subsetting. `is_stocnet_assemblable()` (`R/legacy_wrappers.R`) requires every
node-set name a layer records to resolve to a node table in the bundle. A layer
built with `nodes = fx$actors` records the deparsed string `"fx$actors"`, which
names nothing in the bundle, so the bundle is judged non-assemblable and
`make_data()` returns the legacy environment with **no signal** — a
constructor-usage slip silently produces a non-model object.

(The subset-dependent case is already handled: a dependent-events object that is
a strict subset of its `default_network`'s stream is assembled as a flavored
focal layer keyed by the dependent object's name — see the living
`single-data-object` "subset-dependent pattern through flavor" requirement — and
models exactly its rows. It is not a source of environment fallback and is out of
scope here.)

Making `make_data()` **abort with guidance** instead of silently returning an
environment closes the last user-facing environment producer, which finally makes
the deferred public abort safe to land.

## What Changes

- **`make_data()` never silently returns an environment.** When a bundle is not
  stocnet-assemblable because a layer's node-set name does not resolve to a node
  table, `make_data()` aborts with a `cli` error naming the offending reference
  and telling the user to bind the node set to a plain, resolvable name (e.g.
  `actors <- fx$actors; make_network(nodes = actors, ...)`). Other
  non-assemblable shapes (no node set, no layer) likewise abort with a clear
  reason rather than the silent environment fallback.
- **The deferred public legacy-environment abort lands.** `estimate_dynam()`,
  `estimate_rem()`, `estimate_dynami()`, and `make_specification()` abort on
  `is.environment(data)` at their entrypoints, naming the migration (rebuild the
  object with `make_data()` / `make_groups_interaction()`, both of which return a
  `stocnet`). The internal `estimate_wrapper()` still accepts the DyNAM-i bridge
  environment it builds after the guard.
- **In-repo fixtures that relied on the environment fallback are migrated.**
  Fixtures building a covariate network with `nodes = fx$actors`
  (`test-support_constraint_rate`/`_rem`, `test-model_spec`) bind a plain
  node-set name so they assemble to a `stocnet`.
- **`as_goldfish()` environment conversion stays deferred** to its own change;
  the abort message points at rebuilding via the constructors.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `single-data-object`: `make_data()` assembles a valid bundle to a `stocnet` or
  aborts with guidance on a non-assemblable one (an unresolvable node-set name);
  it never returns a `data.goldfish` environment. The public legacy-environment
  abort lands on `estimate_*()` / `make_specification()`.

## Impact

- `R/legacy_wrappers.R` (`is_stocnet_assemblable()` / the assembly path: identify
  the unresolvable node-set reference and abort with it, rather than reporting
  "not assemblable").
- `R/make_data.R` (route to assemble-or-abort; remove the silent environment
  fallback for user input).
- `R/model_estimate.R`, `R/make_specification.R` (the `is.environment(data)`
  abort at the public entrypoints — the guard implemented and reverted in
  `dynami-stocnet-boundary`, restored).
- Tests: the unresolvable-name abort (snapshot), the env abort (snapshot), and
  migration of the `nodes = fx$actors` fixtures to plain names.
- `NEWS.md` / `DESCRIPTION` (BREAKING: legacy environments rejected; an
  unresolvable node-set name now errors instead of silently degrading).
- Depends on `dynami-stocnet-boundary` (which deferred the abort to here).

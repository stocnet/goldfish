## Context

`make_data()` (`R/make_data.R`) routes a legacy bundle of constructor objects
through `is_stocnet_assemblable()` (`R/legacy_wrappers.R:328`): assemblable
bundles become a `stocnet`, everything else falls back to the legacy
`data.goldfish` environment. That fallback is the blocker for the 2.0.0 public
legacy-environment abort (implemented and then reverted in
`dynami-stocnet-boundary` when the "make_data never returns an environment"
precondition proved false).

`is_stocnet_assemblable()` returns `FALSE` when a layer records a node-set name
that resolves to no node table in the bundle. The constructors record a node set
by deparsing their `nodes = ` argument, so `make_network(nodes = fx$actors, ...)`
records `"fx$actors"`, which names nothing when the bundle is passed to
`make_data()`. The bundle is then judged non-assemblable and the environment is
returned silently — a constructor-usage slip produces a non-model object with no
diagnostic.

The subset-dependent case is **not** a source of this fallback and is out of
scope: a dependent object that is a strict subset of its `default_network`'s
event stream already assembles to a flavored focal layer keyed by the dependent
object's name (living `single-data-object` requirement "Legacy constructors ...
subset-dependent pattern through flavor"; verified: `cd[1:120, ]` on a 439-event
network assembles, flavors 120 rows with the dependent's name and 319 as `NA`,
and models exactly the 120).

## Goals / Non-Goals

**Goals:**

- `make_data()` never silently returns a `data.goldfish` environment for user
  input: it assembles a `stocnet`, or aborts with a message that names the
  unresolvable node-set reference and how to fix it.
- The deferred public legacy-environment abort lands on `estimate_*()` /
  `make_specification()`.
- In-repo fixtures that relied on the environment fallback are migrated.

**Non-Goals:**

- Changing the **subset-dependent → flavor** behavior (already implemented, keyed
  by the dependent object's name; this change does not touch it or its labels).
- `as_goldfish()` converting a saved environment to a `stocnet` (stays deferred).
- Resolving an unresolvable node-set name by **content** matching (rejected — see
  D2); an unresolvable name aborts.
- Assembling genuinely-degenerate bundles (no node set / no layer): they abort
  with their own clear reason, but inventing an assembly for them is out of scope.

## Decisions

### D1 — `make_data()` assembles or aborts; no silent environment for user input
The environment fallback is removed from the user-facing `make_data()` path. A
well-formed bundle assembles to a `stocnet`; a bundle that is not assemblable
aborts with a `cli` error stating why. The only remaining producers of a
`data.goldfish` environment are objects saved before the 2.0.0 flip (loaded from
`.rds`), which the public abort (D3) rejects. *Rejected:* keeping the silent
fallback — it is exactly what makes the public abort unsafe, and it hides a
fixable user error.

### D2 — An unresolvable node-set name aborts, naming the reference; not content-matched
`is_stocnet_assemblable()` (or its caller) SHALL surface *which* node-set
reference failed to resolve, and `make_data()` SHALL abort with that reference and
a fix: bind the node set to a plain name so it resolves
(`actors <- fx$actors; make_network(nodes = actors, ...)`). *Rejected:* falling
back to matching node sets by content (labels/dimensions) — it guesses at
identity, can bind the wrong node set when two share a shape, and hides a
constructor-usage error trivial for the user to correct.

### D3 — The public legacy-environment abort lands here
With D1 guaranteeing `make_data()` never mints an environment, the guard is safe:
`estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`, and
`make_specification()` abort on `is.environment(data)` at their entrypoints — not
inside `estimate_wrapper()`, which still legitimately receives the internal
DyNAM-i bridge environment built after the guard. The message names the
migration: rebuild the object with `make_data()` / `make_groups_interaction()`.
This restores the guard `dynami-stocnet-boundary` implemented and reverted,
unchanged.

### D4 — Fixtures migrate off the environment fallback, not the reverse
The in-repo fixtures that assembled to an environment via `nodes = fx$actors`
(`test-support_constraint_rate`/`_rem`, `test-model_spec`) bind a plain node-set
name so they assemble to a `stocnet`. The change does not add a compatibility
shim for the deparse pattern; the abort message is the contract.

## Risks / Trade-offs

- **Breaking for the `nodes = fx$actors` pattern.** Bundles that assembled to an
  environment via that deparse now abort. That is intended (the environment was
  never a usable model object), but it is a visible behavior change — any code
  (in-repo fixtures included) using it must bind a plain node-set name. NEWS
  calls it out.
- **Breaking for saved environments.** The public abort rejects objects saved
  before the 2.0.0 flip. `as_goldfish()` conversion is still deferred, so the
  only migration today is to rebuild via the constructors — the abort message
  says so. NEWS marks it BREAKING.
- **Surfacing the offending reference.** `is_stocnet_assemblable()` currently
  returns a bare logical; the abort needs the *name(s)* that failed to resolve, so
  the check must return (or the caller must recompute) the unresolved reference
  for the message. Low risk — the information is already computed inside the
  predicate.

# Design — manynet-develop-alignment

## Context

goldfish delegates its data-object construction to manynet and says so:
`single-data-object` requires that "manynet SHALL be listed in Imports pinned
`>= 2.1.0`" and that manynet functions are called "only where delegation is
DRY". The delegation is working as intended. The **pin** is not: goldfish uses
behavior that first exists in manynet 2.3.3, and declares 2.1.0.

The two packages are co-developed in one ecosystem by overlapping authors, and
goldfish's line has never shipped to CRAN, so the usual reason to hold a low
floor — not stranding installed users — does not apply yet
(ADR-0016).

Constraints this design works inside:

- **ADR-0047** settles the strategy: track manynet's develop branch until
  2.0.0, declare the floor goldfish actually uses, and make a vignette
  rebuild the check.
- The floor number is an *expectation*, not a measurement, until task 1
  rebuilds against the aligned manynet. The design must not hard-code 2.3.3
  anywhere that a measurement could contradict.
- The frozen 1e-6 coefficient baselines are untouched: manynet builds data
  objects, and nothing here changes a statistic.

## Goals / Non-Goals

**Goals**

- The declared manynet floor is the floor goldfish uses.
- The manynet behavior goldfish depends on is written down, so the next drift
  is diagnosed from the spec rather than from a stack trace.
- The check that would have caught this is named, not left to luck.

**Non-Goals**

- Making goldfish work across a range of manynet versions. ADR-0047 rejected
  that explicitly: the behavior goldfish needs is *absent* below 2.3.3, so
  "support both" means implementing manynet's fix a second time inside
  goldfish.
- Fixing the DyNAM-i install-versus-`load_all()` failure found in the same
  rebuild. Different cause, different change (see the proposal's Out of scope).
- Raising the `autograph` `Suggests` bound.

## Decisions

### D1 — The floor is measured before it is declared

Task 1 installs manynet from `origin/develop`, rebuilds
`vignettes/two-mode.Rmd`, and records the result **before** `DESCRIPTION`
moves. The expectation is that 2.3.3 suffices, derived from reading
`a8076517`; that is a code reading, not a run.

*Why the order matters.* The failure being fixed is one goldfish only sees at
runtime, in a document. Declaring a floor and then discovering that a
different one was needed would repeat the exact defect this change exists to
remove — a floor that names a version nobody verified.

If the rebuild still fails at 2.3.3, the change does not raise the floor to a
guess: it stops and reports, because the premise (the fix is upstream) would
be wrong and the whole shape of the change would need revisiting.

### D2 — The floor is stated as a requirement, not only as a `DESCRIPTION` line

`DESCRIPTION` records *a* number. The spec records *why that number* — which
manynet behavior goldfish relies on, at which call sites. A version bound with
no statement of what it buys is the artifact that went stale here: `>= 2.1.0`
was almost certainly true when written, and nothing connected it to anything
that would notice when it stopped being true.

The requirement therefore names the behavior (`add_info()` conforming `ties`
onto the reserved `layers`, and the reserved-field set it validates against)
rather than only the number, so a future reader can tell whether a proposed
downgrade is safe by reading the spec instead of bisecting manynet.

*Rejected:* putting this in `CLAUDE.md` as a developer note. It is a property
of the package's contract with its dependency, which is what the living spec
is for; a note in a contributing document is not checked by anything and is
not where someone debugging a stocnet error will look.

### D3 — A vignette rebuild is the named check for the manynet seam

The unit suite cannot see manynet drift. It builds stocnet fixtures directly
(`make_stocnet_fixture()` and friends) rather than through manynet's verbs, so
`add_info()` silently changing what it writes is invisible to all 7141
assertions. The vignettes are the only artifacts that drive `as_stocnet()`,
`add_info()`, `bind_changes()`, `mutate_globals()` and `from_ties()` in
sequence over real data — and they are what caught this.

So the check is: **rebuild the vignettes after a manynet bump, and require the
rebuild to introduce no new errors.** This is stated as a requirement so it
survives the session that noticed it.

*Rejected:* adding unit tests that call manynet's verbs. Worth doing on its
own merits, and it would narrow the gap, but it is a different change with a
different scope, and it would not have caught *this* failure — which needed
the whole assemble → stamp → estimate sequence the vignette performs.

*Known limit, stated rather than hidden:* a check that only runs when someone
rebuilds is a check with no schedule. Making it automatic means running
vignettes in CI, which is a separate cost decision. Until then the honest
description is "the check exists and is named", not "drift is caught".

### D4 — `Remotes:` is decided by measurement, not by preference

While 2.3.3 is off CRAN, a contributor running `devtools::install_deps()` gets
2.3.1 and a broken vignette build. A `Remotes: stocnet/manynet@develop` line
fixes that for contributors and is ignored by CRAN, but it also pins the
package to a moving branch in a tracked file, and `R CMD check --as-cran`
notes it.

Task 3 decides this by trying it: whether `install_deps()` on a clean library
resolves an acceptable manynet without `Remotes:`. If it does not, the entry
goes in with a comment naming the release that removes it.

### D5 — The 2.0.0 blocker is recorded where releases are checked

"The floor must name a CRAN-available manynet" is the kind of obligation that
is obvious while writing it and invisible six weeks later. It goes into the
release pre-flight (the `release-prep` skill's checklist), not only into this
change's prose, because the change archives and the checklist does not.

## Risks / Trade-offs

- **goldfish's declared dependency is temporarily unsatisfiable from CRAN
  alone** → accepted, bounded by 2.0.0, and recorded as a release blocker
  (D5).
- **Tracking a branch means upstream can break goldfish between sessions with
  no signal** → mitigated, not solved, by D3: the check is named but manual.
  The residual risk is that drift is found late, as it was this time.
- **The measured floor may be higher than 2.3.3** → D1 orders the work so this
  is discovered before anything is declared, and stops the change rather than
  guessing.
- **A second manynet-facing failure hides behind the first** → the DyNAM-i
  failure already looked like one and was not; the proposal records the
  separation explicitly so the next reader does not re-merge them.

## Migration Plan

None for users. Contributors install manynet from `origin/develop` (or tag
`v2.3.3`) before rebuilding vignettes. Rollback is a single-commit revert of
the `DESCRIPTION` bump plus the vignette rebuild.

## Open Questions

- Should the vignette rebuild run in CI, making D3's check automatic rather
  than named? Cost is a long CI job that needs a non-CRAN manynet.
- Is `two-mode.Rmd` the only vignette that touches the changed `add_info()`
  behavior, or does `multivariate-specification.Rmd` reach it too? Task 1's
  rebuild covers all of them, so the answer arrives as data.

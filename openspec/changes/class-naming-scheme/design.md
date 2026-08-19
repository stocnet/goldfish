## Context

goldfish runs two class-naming conventions at once. Seventeen classes carry the
package with a dot (`result.goldfish`, `network.goldfish`, `preprocessed.goldfish`,
…), inherited from the pre-1.7.0 era. Eight classes minted since 1.9.21 carry
nothing at all (`test_gof`, `diagnose_onset`, `margin_table`, …), following a
convention the living spec wrote down when `residuals-gof` was archived: *"the
class is the exported constructor's own snake_case name, with no suffix."*

[stocnet/autograph#60](https://github.com/stocnet/autograph/issues/60) is the
bug report for the second convention. autograph 1.2.0 registers `plot` methods on
those bare names and its method bodies read goldfish-specific columns and
attributes; if another package returns a `test_gof`-classed object, autograph
dispatches goldfish's plot code onto it and cannot detect the mistake, because
nothing in the class vector names a package.

Constraints this design works inside:

- **ADR-0020** settles the scheme: a snake_case `_goldfish` suffix on the
  constructor's own name, for every class goldfish returns to a user.
- **ADR-0016** (accepted, expires at 2.0.0) licenses the hard rename: until
  2.0.0 a user-facing name that misleads is renamed rather than kept, with no
  deprecation shim. Nothing on this line has shipped to CRAN.
- The frozen 1e-6 coefficient and C++ golden baselines in
  `tests/testthat/_baselines/` are not regenerated. Class names do not reach
  `src/`, so a correct rename cannot move a coefficient; if one moves, the rename
  was wrong.
- The package's strict snake_case policy applies to all functions, arguments and
  objects; classes are the surface that currently escapes it.

## Goals / Non-Goals

**Goals**

- Every class a user can hold or dispatch on names goldfish.
- One convention, package-wide, so a user can spell a class from the constructor
  name without knowing when it was minted.
- No goldfish class contains a dot, so `plot.test_gof_goldfish` has exactly one
  reading.
- autograph's `feature/goldfish-diag` methods work against the renamed classes on
  the day the rename lands.
- The living spec ends the change with one class-naming rule in it, not two.

**Non-Goals**

- Renaming classes on the deprecated path. `nodes.goldfish`, `network.goldfish`,
  `dependent.goldfish` and `global.goldfish` come from constructors that already
  `deprecate_warn()` toward `manynet::make_stocnet()`.
- Renaming internal dispatch classes that never leave the package. The effect
  tags (`inertia`, `recip`, `trans`, … 60+ of them), `writer_*`, `data_source_*`,
  `model_spec`, `support_constraint_plan`, `fixed_spec`/`initial_spec` stay.
  `goldfish.formulae` is the one internal exception, taken because it is a
  single site and its current spelling is the convention backwards.
- Providing a translation shim for objects saved by earlier goldfish. Recognize,
  never translate — the rule `format_version.R` already states.
- Changing any object's *contents*, components, or attributes. This change moves
  names only.
- Renaming the exported functions. `test_gof()` stays `test_gof()`; only the
  class of what it returns moves.

## Decisions

### D1 — The scheme is a snake_case `_goldfish` suffix on the constructor's name

Per ADR-0020. `test_gof()` returns `test_gof_goldfish`; `estimate_dynam()`
returns `result_goldfish`.

*Alternatives considered.* Issue #60's `.goldfish` was rejected because it
reintroduces the `plot.test_gof.goldfish` ambiguity the issue itself names — the
string parses equally as a `plot()` method on `test_gof.goldfish` and as a
`goldfish` method of a `plot.test_gof` generic — and because it exempts classes
from the snake_case policy every other user-facing name obeys. A `goldfish_`
prefix avoids both problems but severs the visual link between constructor and
class. The status quo (no suffix) does not answer the bug.

### D2 — Scope is drawn by lifecycle, not by class family

A class is renamed if it is on the live path, and left alone if it is on the
deprecated path. This is why the four legacy data classes keep their dotted
names while `preprocessed.goldfish` moves: not because one group is "data" and
the other "results", but because one group is scheduled for deletion.

Renaming a name on its way out spends churn in the package, in every user script
and in every vignette, and buys a tidier spelling for a string that will not
exist after the stocnet migration completes.

### D3 — `data.goldfish` splits, because it is currently two classes under one name

The name serves two unrelated objects: the legacy environment built by
`make_data()` and DyNAMi (deprecated path), and the marker `as_goldfish()` stamps
on a `stocnet` object (experimental, live). Under D2 the first stays and the
second moves, so:

| Producer | Class after |
|---|---|
| `make_data()`, DyNAMi | `data.goldfish` (unchanged, deprecated) |
| `as_goldfish()` | `data_goldfish` |

Two similar names coexisting is uncomfortable and worth stating plainly as a
cost. It is nonetheless honest: they are two different objects, and one name for
both is the defect, not the fix. Print dispatch splits with them, so the legacy
environment and the stamped stocnet no longer share a print method by accident.

*Alternative considered.* Treating `data.goldfish` wholly as legacy and leaving
the `as_goldfish()` stamp on the dotted name was rejected: `as_goldfish()` is the
forward-looking boundary for the stocnet input, and permanently dressing it in a
legacy-looking name inverts the signal the rename exists to send.

### D4 — The retired `result.goldfish` name becomes the staleness discriminator

`result.goldfish` is not kept as an alias. The live class is `result_goldfish`,
and the old name survives only so that an object saved by an earlier goldfish
still dispatches somewhere legible instead of producing R's raw
`no applicable method` for `print()`.

Exactly two stubs register on the old name:

- `print.result.goldfish` — explains and stops.
- `summary.result.goldfish` — explains and stops.

No other generic registers on it. `coef()`, `logLik()`, `vcov()`, `predict()`,
`residuals()`, `augment()`, `tidy()`, `glance()` and the diagnostics on a saved
object give R's own "no applicable method" error, which is accurate: there is no
method, and the object cannot be repaired.

*Alternative considered.* Stubbing all ~20 user-facing generics so nobody ever
meets a bare dispatch error was rejected as ~20 NAMESPACE entries and 20 roxygen
blocks maintained forever to improve the wording of a dead end. The first thing a
user does with an unfamiliar object is print it; that is where the diagnosis
belongs, and `format_version.R` already reasons this way.

### D5 — The stub diagnoses by epoch, not by class alone

The class rename introduces a case the current message set cannot describe
correctly. `result_format_status()` reads the object's `fit_version` epoch, and
after the rename there are two distinct populations wearing `result.goldfish`:

| Object origin | `fit_version` | Status | What is actually wrong |
|---|---|---|---|
| CRAN goldfish ≤ 1.7.0 | absent | `outdated` | components renamed to snake_case; components missing |
| dev line 1.9.x | `2L` | `current` | **only** the class name is retired |

Firing the existing "fitted before goldfish 2.0.0, when the components were
renamed" bullets at the second population would state something false about the
object in the user's hand — the defect this whole change exists to remove. So the
stub keeps consulting the epoch and adds a third diagnosis for a
current-epoch object on a retired class: the class was renamed, re-fit.

The epoch counters themselves (`FIT_VERSION`, `PREP_VERSION`) do **not** move.
The rule in `format_version.R` is that an epoch moves once per *release* whose
layout differs, never once per dev-line change, and this change alters no
component of any object.

### D6 — The summary object is `summary_result_goldfish`, breaking with base R's idiom

Base R classes a summary as `summary.<class>` (`summary(lm)` → `summary.lm`), so
the idiomatic result here would be `summary.result_goldfish`. It is rejected: the
goal is that no goldfish class contains a dot, and a class that does would be an
exception a reader has to memorize. The method is still `summary.result_goldfish()`;
only the object it returns is `summary_result_goldfish`, printed by
`print.summary_result_goldfish()`.

This also removes a live ambiguity — `summary.result.goldfish` is currently both
a method name and a class name in the same package.

### D7 — Hard rename, no fallback class

The renamed objects carry only the new class. Adding the old name as a trailing
element (`c("test_gof_goldfish", "test_gof", "list")`) would keep autograph's
current methods working, but it re-claims the global name the change exists to
release, so it defeats the purpose rather than easing it. ADR-0016 licenses the
break; there is no CRAN installed base for this line.

### D8 — Living-spec deltas are targeted; the rename table is authoritative

Forty-four requirements across twenty capabilities mention an old class string.
Reissuing all forty-four as `## MODIFIED` blocks means copying forty-four
requirement bodies verbatim, and every copy is an opportunity to drift wording —
the same objection ADR-0016 raised against renaming by sweep, applied to specs.

So: a delta is issued where the requirement's **rule** changes, or where the
class string **is the subject** of the contract. `class-naming` states the
rename table and declares it authoritative package-wide. A closing task then
corrects the remaining stale spellings directly in `openspec/specs/**`,
hand-edited and diff-reviewed.

The one delta that is not optional is `diagnostic-plot-classes`. Its "no suffix"
sentence is an opposing SHALL; a purely additive `class-naming` capability would
leave both in the living spec, which is precisely the failure
`.plan/opsx-spec-placement-check.sh` was written to catch.

### D9 — The rename is applied by hand, per class, never by global search-and-replace

The obvious implementation is a `sed` over the repo. It is wrong here, for two
independent reasons:

1. **The class strings collide with function names.** `test_gof`, `test_time`,
   `test_parameter`, `diagnose_onset`, `diagnose_outliers`,
   `diagnose_changepoints`, `margin_table` and `evaluate_model` are all *exported
   functions* as well as classes. The high occurrence counts are mostly the
   functions. A textual replace renames the API.
2. **Scripted edits must not touch comments or roxygen.** A previous scripted
   rewrap in this package merged an `@noRd` tag into a prose line and broke the
   block silently, with tests and lint still green. Roxygen `@method` and
   `@export` tags, and the prose that names classes, are hand-edited one site at
   a time.

The safe edit surface is therefore: quoted class strings, `inherits()` /
`is()` arguments, `class<-` / `structure(class =)` values, roxygen `@method`
tags, and `NAMESPACE` (regenerated by `devtools::document()`, never hand-edited).

### D10 — One class per commit, tests green at every commit

Work proceeds class by class, or by tightly-coupled cluster (`result_goldfish`
with `flavored_result_goldfish` and `summary_result_goldfish`, since the methods
and the stale-guard helpers are shared). Each commit renames one cluster
end-to-end — code, roxygen, `document()`, tests, snapshots — and leaves the suite
green. This keeps any individual step revertable, which matters more than usual
for a change whose failure mode is a silently unregistered S3 method.

Ordering runs cheapest-first so the pattern is established on small surfaces
before `result_goldfish` (the 131/54/95 one) is attempted:
internal → algorithm/spec → preprocessing → diagnostics → results → data stamp.

### D11 — Snapshot tests are reviewed, not accepted wholesale

Printed output embeds class names, so `testthat::snapshot_accept()` will happily
absorb both the intended rename and any wording regression introduced alongside
it. Each snapshot diff is read before acceptance, and a task that accepts
snapshots states what changed in them.

### D12 — autograph moves in the same change, in the working copy

autograph 1.2.0 breaks against goldfish the moment the rename lands, so the fix
travels with the break rather than behind it. The six `S3method(plot, …)` entries,
the roxygen `@method` tags and the `inherits()` guards in
`R/plot_diagnostics.R` are updated on `feature/goldfish-diag` in
`/Users/ualvaro/Documents/repos/autograph`, and issue #60 is answered with the
final table.

Note that autograph's method set is *smaller* than goldfish's class set: it plots
six of the eight diagnostic classes. `test_parameter` and `evaluate_model` are
renamed in goldfish but have no autograph method to update.

### D13 — Single version bump to 1.9.29 at the end, with one consolidated NEWS entry

The standing rule bumps at each phase milestone. This change is one coherent
rename rather than a sequence of features, and a reader wants the whole table in
one place, so it takes a single 1.9.28 → 1.9.29 bump in the closing task with the
full old→new table under a **Breaking changes** heading.

## Risks / Trade-offs

- **A renamed class silently loses its S3 registration** (roxygen `@method` tag
  updated but `@export` dropped, or `NAMESPACE` not regenerated) → the method
  stops dispatching and the object prints as a bare list. Tests generally still
  pass, because most assert on values rather than on dispatch. → Each cluster's
  verification task asserts dispatch explicitly (`expect_s3_class()` on the
  object *and* an assertion that the print method ran), and the NAMESPACE diff is
  read as part of the commit.
- **The function/class name collision invites a bad `sed`** → D9 forbids it; the
  verification for the diagnostic cluster explicitly greps that the exported
  function names are unchanged (`export(test_gof)` still in NAMESPACE).
- **A frozen baseline moves** → it cannot, if the rename is correct; class names
  never reach `src/`. A moved baseline is the signal that something other than a
  name changed, and the task stops rather than regenerating (design D18 of the
  baselines rule).
- **The living-spec sweep (D8) misses a spelling** → the closing task greps
  `openspec/specs/` for every old class string and requires an empty result,
  excluding the four intentionally-retained deprecated names and the legacy
  `data.goldfish`.
- **`data.goldfish` and `data_goldfish` are confusable in review** (D3) → the
  split is covered by a scenario in `class-naming` asserting both producers and
  both print routes, so the distinction is tested rather than remembered.
- **autograph and goldfish drift out of step** if only one side is committed →
  the autograph task follows the goldfish diagnostic-cluster task immediately and
  its verification loads both working copies.
- **A user's saved fit becomes unusable.** This is the accepted cost of D7, not a
  risk to mitigate. The stub (D4/D5) makes it legible; nothing makes it
  recoverable.

## Migration Plan

For users on the development line, there is no migration path for stored
objects — re-fit. `print()` and `summary()` on a stored fit say so. Scripts
testing `inherits(x, "result.goldfish")` or defining methods on a goldfish class
update to the new spelling; the NEWS table under 1.9.29 is the reference.

Rollback is per-commit (D10). Because each cluster is self-contained, reverting
one commit restores that class's old name without disturbing the others.

## Open Questions

- Do the internal dispatch classes ever become required rather than recommended?
  `fixed_spec` / `initial_spec` in particular are very generic names, but they
  never escape the package. Deferred; `class-naming` records them as recommended.
- Should the deprecated data classes be renamed at the moment they are removed,
  if their removal slips past 2.0.0 and they are still present under the old
  convention?

# Design — class-naming-scheme

> Revised 2026-08-19 (ADR-0031 supersedes ADR-0020): scheme flipped from
> the `_goldfish` snake_case suffix to the `goldfish<Thing>` camelCase
> prefix, scope extended to internal classes, summary idiom flipped to
> base R's, and the autograph lockstep inverted — autograph@develop
> moved first.

## Context

goldfish runs two class-naming conventions at once: seventeen dotted
classes (`result.goldfish`, …) from the pre-1.7.0 era, and eight bare
constructor-named classes minted since 1.9.21 (`test_gof`,
`diagnose_onset`, …) under a convention the living spec wrote down when
`residuals-gof` archived. [stocnet/autograph#60] is the bug report for
the second convention: nothing in those class vectors names a package.

The ecosystem answered upstream. autograph@develop's CONTRIBUTING
(commit 03ec996, 2026-08-17) states the rule for every stocnet package:
a class is named **package + noun, in camelCase** (`<pkg><Thing>`),
following RSiena's `sienaFit`/`sienaGOF`/`sienaAlgorithm`; no dot
suffix (a dot creates no inheritance — dispatch is on exact strings);
no shared parent class (autograph standardizes by coercion). The same
commit renamed autograph's goldfish methods to `goldfishFit`,
`goldfishGOF`, `goldfishTimeTest`, `goldfishOutliers`,
`goldfishChangepoints`, `goldfishOnset`, `goldfishMargins`, keeping
goldfish's current spellings only as defunct aliases to be deleted
"once the oldest supported goldfish is past the rename". manynet's
CONTRIBUTING (origin/develop, verified 2026-08-19; local checkout is
143 behind — read via `git show origin/develop:`) adds no competing
class rule.

Constraints this design works inside:

- **ADR-0031** (supersedes ADR-0020) settles the scheme: `goldfish` +
  short camelCase identifier, decided by Alvaro 2026-08-19.
- **ADR-0016** (expires at 2.0.0) licenses the hard rename; nothing on
  this line has shipped to CRAN. This change folds **next**, before
  `parametric-rates`, so the closing window is used.
- The frozen 1e-6 and C++ golden baselines are not regenerated; class
  names never reach `src/`.
- The strict snake_case policy stays for functions, arguments, and
  objects; classes become an explicit carve-out, written into the
  tracked `CLAUDE.md` by this change (D14).

## Goals / Non-Goals

**Goals**

- Every class goldfish attaches — user-facing or internal — names
  goldfish, in one convention.
- goldfish lands exactly on the seven class strings autograph@develop
  already dispatches on.
- Method names have one parsing: snake_case generic, dot, camelCase
  class.
- The living spec ends with one class-naming rule; active changes'
  verbatim delta copies are swept in the same commit so their archives
  reconcile.

**Non-Goals**

- Renaming deprecated-path classes (`nodes.goldfish`,
  `network.goldfish`, `dependent.goldfish`, `global.goldfish`, the
  legacy `data.goldfish` environment) — lifecycle exemption confirmed
  2026-08-19.
- Renaming the ~60 effect dispatch tags — they stop being classes when
  `effect-term-registry` replaces string-built dispatch; renaming them
  fights a mechanism scheduled for retirement (confirmed 2026-08-19).
- Translation shims for stored objects; changing any object's contents;
  renaming exported functions.

## Decisions

### D1 — The scheme is `goldfish` + a short camelCase identifier (ADR-0031)

`estimate_dynam()` returns `goldfishFit`; `test_gof()` returns
`goldfishGOF`. The identifier is a short word compressing the object
(`Fit`, `Prep`, `Spec`, `Eval`), not a transliteration of the
constructor name. Where autograph@develop fixed a name, that name is
adopted verbatim (the seven above); where RSiena has a precedent, it is
followed (`goldfishAlgorithm` ← `sienaAlgorithm`;
`goldfishTimeTest` ← `sienaTimeTest`).

*Why the flip from ADR-0020's `_goldfish` suffix.* Three reasons, in
order of force. (1) With snake_case generics **and** snake_case
classes, a method name has multiple generic/class parses —
`augment_seq.flavored_result_goldfish` reads at several dot boundaries;
camelCase classes give the dot exactly one reading, which was the
collision ADR-0020's scheme merely relocated. (2) The ecosystem rule
now exists and autograph@develop already ships the concrete strings —
diverging means goldfish objects don't plot. (3) A class visually
distinct from the function namespace is a feature, not a policy breach:
the snake_case policy governs names users *call*; a class string is
data. *Alternatives considered*: `_goldfish` suffix (ADR-0020 —
superseded for the reasons above), `.goldfish` dot suffix (issue #60's
own proposal — rejected then and still: dots create no inheritance and
maximize ambiguity), bare constructor names (the bug).

### D2 — Scope is drawn by lifecycle and by mechanism, not by export status

The rename is full (Alvaro, 2026-08-19): internal classes move too —
`goldfishWriterDefault`/`goldfishWriterGather`/`goldfishWriterDB`,
`goldfishSourceEnvir`/`goldfishSourceStocnet`, `goldfishModelSpec*`,
`goldfishSupportPlan`, `goldfishFixedSpec`/`goldfishInitialSpec`,
`goldfishFormulae`. Two exemptions, each with a mechanism reason, not a
purity one:

- **Deprecated path** (four data classes + legacy `data.goldfish`):
  renaming a name scheduled for deletion spends churn on a string that
  will not survive the stocnet migration; the dotted spelling now
  usefully *marks* the legacy path.
- **Effect dispatch tags**: `effect-term-registry` Layer 1 retires
  their class role entirely (registry lookup replaces `getS3method`
  name construction); renaming ~60 tags and their `init_*`/`update_*`
  method names, then deleting the mechanism, is double churn.

### D3 — `data.goldfish` splits; the stamp side is `goldfishData`

Unchanged in substance from the original design: the legacy
environment keeps `data.goldfish` (deprecated path), the `as_goldfish()`
stamp becomes `goldfishData`, print dispatch splits with them. The two
names are now visually far apart, which removes the earlier
"confusable in review" cost of the split.

### D4 — The retired `result.goldfish` name is the staleness discriminator

Unchanged: exactly two stubs (`print.result.goldfish`,
`summary.result.goldfish`) explain and stop; every other generic gives
R's own "no applicable method". No fallback class on renamed objects.

### D5 — The stub diagnoses by epoch, not by class alone

Unchanged: a no-epoch object (CRAN ≤ 1.7.0) is told its components were
renamed; a current-epoch object (dev line) is told only its class name
is retired. `FIT_VERSION`/`PREP_VERSION` do not move — no component of
any object changes.

### D6 — The summary object is `summary.goldfishFit`, following RSiena and base R

Flipped from the previous design. RSiena classes its summary as
`summary.sienaFit` (with `print.summary.sienaFit`), the base-R
`summary.lm` idiom, and goldfish follows: `summary(fit)` returns
`summary.goldfishFit`, printed by `print.summary.goldfishFit()`. The
former "no dots anywhere" goal narrows to **no dot-suffix package
qualification** (`.goldfish` at the end of a class): the `summary.`
*prefix* is the entrenched base-R idiom, and with camelCase classes it
is unambiguous — `print.summary.goldfishFit` has one parse because no
goldfish generic is named `print.summary`. The previous design rejected
the idiom to keep a uniform no-dot rule; with the ambiguity dissolved
by camelCase, breaking with base R would buy uniformity nobody needs at
the price of surprising every R user. This also still removes the live
`summary.result.goldfish` method-name/class-name collision: the method
is `summary.goldfishFit()` and the class it returns is
`summary.goldfishFit` — the same relationship `stats::summary.lm` has.

### D7 — Hard rename, no fallback class

Unchanged. ADR-0016 licenses the break; autograph's defunct aliases
(upstream) are the only compatibility layer anywhere, and they are
autograph's to delete.

### D8 — Living-spec deltas are targeted; the rename table is authoritative

Unchanged in mechanism: deltas where the rule changes or the class
string is the contract; a closing hand-edited sweep for stale
spellings; the `diagnostic-plot-classes` "no suffix" SHALL is replaced
in place.

### D8a — Active changes' verbatim delta copies are swept in the same commit

New (2026-08-19, from cross-session review). `parametric-rates` and
`two-sided-coordination` carry `## MODIFIED` blocks that are verbatim
copies of living-spec requirements naming `specification.goldfish`.
This change folds *before* them, so the moment its sweep rewrites the
living text, those copies would drift and their archive sync would
reconcile badly. Therefore the sweep task covers, in the same commit as
the living-spec edits: `parametric-rates/specs/model-specification`,
`two-sided-coordination/specs/model-specification`,
`two-sided-coordination/specs/multivariate-specification`, and the
`_goldfish` mention in `parametric-rates` design D11 — updating their
class strings to the post-rename spelling. (Agreed with the session
holding those changes; it will not sweep them itself.)

### D9 — The rename is applied by hand, per class, never by global search-and-replace

Unchanged, and still load-bearing: the *old* diagnostic class strings
collide with exported function names (`test_gof` is both), so a
textual replace renames the API; and scripted edits must not touch
roxygen or comments. Safe edit surface: quoted class strings,
`inherits()`/`is()` arguments, `class<-`/`structure(class =)` values,
roxygen `@method` tags, regenerated NAMESPACE.

### D10 — One cluster per commit, tests green at every commit

Unchanged in mechanism; ordering updated for the wider scope,
cheapest-first: formulae → algorithm/spec → internal
(writers/sources/plans) → preprocessing → diagnostics → results → data
stamp.

### D11 — Snapshot tests are reviewed, not accepted wholesale

Unchanged.

### D12 — autograph@develop is upstream; goldfish matches, verification only

Inverted from the previous design. autograph moved first: its develop
branch (03ec996) already registers `plot.goldfishFit`,
`plot.goldfishGOF`, `plot.goldfishTimeTest`, `plot.goldfishOutliers`,
`plot.goldfishChangepoints`, `plot.goldfishOnset`,
`plot.goldfishMargins`, with goldfish's current names as defunct
aliases. So there is no autograph rename task: goldfish adopts those
seven strings verbatim, the lockstep task becomes *verification*
(build one object of each class in goldfish, plot through
autograph@develop, confirm dispatch hits the new methods, not the
aliases), and the alias deletion is autograph's own cleanup once the
rename ships. `goldfishParamTest` and `goldfishEval` have no autograph
method — confirmed, not assumed. Issue #60 is answered with the final
table.

### D13 — Single version bump at the close, one consolidated NEWS table

Unchanged in mechanism; the bump is the next patch version at fold time
(the numbers in the earlier draft are stale — the package is already at
1.9.29), with the full old→new table under **Breaking changes**.

### D14 — The tracked CLAUDE.md carves classes out of the snake_case policy

New. The repo's CLAUDE.md states the 1.7.0 renames "retired camelCase
from the exported API — never reintroduce it". That sentence stays true
for functions, arguments, and objects, and this change must not leave
it contradicting the class scheme. The naming section gains the
carve-out: S3 *class strings* follow the stocnet ecosystem rule
`goldfish<Thing>` (camelCase), per ADR-0031 and autograph's
CONTRIBUTING; everything callable stays snake_case. The edit is a task
of this change (it lands with the rename, not before it).

### D15 — Lint compatibility is verified in groundwork, not discovered mid-rename

New. S3 method names like `print.goldfishFit` and
`summary.goldfishFit` must pass `.lintr`'s
`object_name_linter("snake_case")`. lintr exempts S3 methods for known
generics, but goldfish defines its own snake_case generics whose
methods on camelCase classes may still be flagged
(`diagnose_onset.goldfishFit`). Groundwork runs the linter over a
one-file spike declaring one method per generic family; if flags
appear, `.lintr` gains the documented adjustment (an additional
accepted style or targeted exclusions) in the same groundwork commit —
never ad-hoc `# nolint` scattered through the rename.

## Risks / Trade-offs

- **A renamed class silently loses its S3 registration** → unchanged
  mitigation: every cluster asserts dispatch explicitly and the
  NAMESPACE diff is read.
- **The function/class collision invites a bad `sed`** → D9 forbids it;
  the diagnostic-cluster verification greps that `export(test_gof)` and
  siblings survive.
- **A frozen baseline moves** → it cannot if the rename is correct; a
  moved baseline stops the task (baselines rule).
- **Active-change delta copies drift** (new) → D8a sweeps them in the
  same commit; the placement pre-flight re-runs for both affected
  changes afterwards.
- **Internal-class rename destabilizes dispatch in hot paths** (new
  scope) → internals are renamed in their own early clusters with the
  full `NOT_CRAN=true` suite green before the big `goldfishFit`
  cluster starts; any effect-adjacent string is checked against the
  effect-tag exemption list first.
- **lintr flags method names** → D15 front-loads the spike.
- **A user's saved fit becomes unusable** → accepted cost (D7); the
  stub makes it legible.

## Migration Plan

Unchanged for users: no migration for stored objects — re-fit; scripts
update `inherits()` checks per the NEWS table. Rollback is per-cluster
commit. autograph: no action required at fold; delete the defunct
aliases in a later autograph release.

## Open Questions

- Should autograph's defunct aliases be deleted immediately after this
  folds (goldfish and autograph are co-developed here) or kept one
  autograph release for third parties?
- `goldfishModelSpec` hierarchy: whether the subclass identifiers keep
  their current suffix words verbatim once inventoried (task 1.1
  decides against the actual strings).

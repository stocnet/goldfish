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
followed (`goldfishAlgo` ← `sienaAlgorithm`;
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

### D6 — The summary object is `goldfishSummFit`; no class carries a dot at all

Reversed 2026-09-05 (Alvaro). An earlier revision of this decision adopted
the base-R/RSiena dotted idiom — `summary()` returning a class literally
named `summary.goldfishFit`, printed by `print.summary.goldfishFit()` —
on the argument that `summary.` is a *prefix* rather than the `.goldfish`
package *suffix* the rule bans, and that camelCase left it unambiguous to
parse. That reasoning was sound and is still true. It is dropped anyway,
because parsing was never the objection: the shape is **confusing to
read**, and it buys conformity with base R at the cost of the one rule
this change exists to make simple.

`summary()` on a fit SHALL return `goldfishSummFit`, printed by
`print.goldfishSummFit()`. The no-dot rule then holds without exception on
the live path, which is worth more than the idiom.

Three concrete reasons the idiom cost more here than it does in base R.
(1) `summary.goldfishFit` is simultaneously a method name and the name of
the class that method returns — exactly `summary.lm`'s situation, and the
previous revision claimed the rename "removes the collision" when in fact
it reproduces base R's. A design that has to explain why its own stated
rule does not apply here has found the wrong rule or the wrong exception.
(2) `print.summary.goldfishFit` carries three dots and reads as a method
on a generic named `print.summary`. Unambiguous to R; not to a reader.
(3) The flavored sibling makes it worse, not better: the dotted form gives
`summary.goldfishFlavFit`, whereas `goldfishSummFlavFit` stays one token.

*Note on the flavored side.* `summary`, `tidy` and `glance` are currently
**absent** from `flavored_result.goldfish` (18 methods against
`result.goldfish`'s 20) — so there is no sibling to be parallel with yet,
only a hole. Whether `goldfishSummFlavFit` comes into existence, or the
flavored class inherits the plain summary, or `summary` is refused for
flavored fits, is decided by ADR-0038's inherit/override/refuse contract
table, not here. This decision fixes only the *name* a summary object
carries if one exists.

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

### D8b — joint-parameters is added to the D8a sweep list; it owns its own R-code rename

New (2026-08-28). `joint-parameters` is `complete` (all tasks checked)
but not yet archived, and — unlike `parametric-rates` /
`two-sided-coordination` — it is not a docs-only proposal: it shipped
real classes, `parameters.goldfish` (its own) and, pervasively,
`joint_specification.goldfish` (the already-archived
`make-multivariate-spec` capability's class, folded into
`openspec/specs/multivariate-specification/spec.md`). Its own delta
(`joint-parameters/specs/multivariate-specification`) is therefore
added to D8a's active-change sweep list for the **doc-text** pass: once
this change's own rename lands in the living spec, that delta's prose
must match. The **code-level** rename of `R/joint_parameters.R` and
`R/make_joint_specification.R` (plus their tests and snapshots) is
*not* duplicated here — `joint-parameters` tracks it as its own task
(see that change's design D17 / tasks.md §3), since it is the change
actively holding those files, and a second change rewriting code
another change's task is mid-editing would race. If `joint-parameters`
archives before this change folds, its rename task runs standalone
against the rename table above (D16); if this change folds first, its
own clusters cover both classes and `joint-parameters`'s task 3 becomes
a verification-only no-op.

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
`goldfishSummFit` must pass `.lintr`'s
`object_name_linter("snake_case")`. lintr exempts S3 methods for known
generics, but goldfish defines its own snake_case generics whose
methods on camelCase classes may still be flagged
(`diagnose_onset.goldfishFit`). Groundwork runs the linter over a
one-file spike declaring one method per generic family; if flags
appear, `.lintr` gains the documented adjustment (an additional
accepted style or targeted exclusions) in the same groundwork commit —
never ad-hoc `# nolint` scattered through the rename.

### D16 — The rename table grows after the initial draft; two more live classes, one prospective family

New (2026-08-28, cross-session audit). Two classes surfaced after this
change's proposal/table were drafted (2026-08-19): `joint-parameters`
landed on a concurrent branch and shipped `parameters.goldfish` and
(pervasively) `joint_specification.goldfish` under the retired
`<noun>.goldfish` house convention its own task 1.1 named explicitly —
this change's table did not yet exist to steer it. Separately,
`abmcem`/`dynes-augmentation` (both unimplemented) had drafted a
shared internal E-step class family — `estep_is`, `estep_resampling`,
`estep_uniform`, parent `dynes_estep` — in snake_case, which the D2
internal-scope rule (no exemption beyond the deprecated path and
effect tags) already forbids.

**Decision:** both are added to the authoritative table (`goldfishJointSpec`,
`goldfishParams`, `goldfishEstepIS`/`goldfishEstepResampling`/
`goldfishEstepUniform`/`goldfishDynesEstep`). The two live classes are
real renames (D8b covers the coordination with `joint-parameters`, the
change that will execute them). The four prospective classes need no
rename at all — `abmcem` and `dynes-augmentation`'s own specs are
edited directly to the `goldfish<Thing>` spelling (2026-08-28), so
those changes are simply implemented under the final names from the
start, per this proposal's original intent ("every class the
parametric/coordination changes create is born under the new rule").
*Rejected:* leaving the table as a closed, one-time snapshot — a table
that cannot absorb classes discovered after its own drafting stops
being authoritative the first time a concurrent branch lands.

### D17 — Class identifiers use a fixed short-word vocabulary

New (2026-09-05, Alvaro). D1 says the identifier is "a short word
compressing the object", which under-determines the result: the same
concept was spelled `Algorithm` in one row and `Prep` in another, and the
`model_spec` hierarchy would have produced names like
`goldfishModelSpecDynamRateOrdered` (33 characters). A class string is read
in method names, `inherits()` calls, test assertions and print output, so
length is a real cost paid many times.

The identifier SHALL be built from this vocabulary, and a new class
extends the table rather than inventing a synonym:

| Concept | Short | Concept | Short |
| --- | --- | --- | --- |
| DyNAM | `Dn` | preprocessed | `Prep` |
| DyNAM-i | `Dni` | specification | `Spec` |
| REM | `Rem` | control | `Ctrl` |
| DyNAMu | `Mu` | algorithm | `Algo` |
| ordered (Cox partial likelihood) | `Cox` | flavored | `Flav` |
| choice coordination | `Coord` | summary | `Summ` |

`Cox` replaces `Ordered` because it names the estimator rather than the
arity, which is the more useful fact at a call site.

Applied, this shortens four already-agreed rows —
`goldfishAlgorithmNewton` (23) becomes `goldfishAlgoNewton` (18),
`goldfishPrepControl` becomes `goldfishPrepCtrl`,
`goldfishFlavoredFit`/`goldfishFlavoredPrep`/`goldfishFlavoredStats` become
`goldfishFlavFit`/`goldfishFlavPrep`/`goldfishFlavStats` — and leaves
`goldfishChangepoints` (20) as the longest live class, which cannot move
because autograph@develop already dispatches on it (D12).

*Deliberately unsettled.* The coordination short name is recorded here as
`Coord`, **not** as a mechanism name. Naming DyNAM's `choice_coordination`
after a DyNAMu mechanism (`Conj`) was proposed and withdrawn the same day:
`estimate_dynamu()` is its own estimator (ADR-0022) with its own `model=`
value, so its variants are siblings of the DyNAM ones, and a mechanism
name would produce two different classes both meaning "conjunctive". How
DyNAM coordination and the five DyNAMu mechanisms relate in the class
hierarchy — flat siblings, a shared coordination family class, or a
mechanism field — is open and is **not** settled by this decision.

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

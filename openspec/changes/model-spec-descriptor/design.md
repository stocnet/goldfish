# Design — model-spec-descriptor

## Context

`model_spec_structure()` stamps `c(variant, indexing, "goldfishKind")`, where
`variant` is `model × sub_model`. Three mechanisms then read model behavior:
the class vector (S3 dispatch), the `risk_set` descriptor field (a list
computed once by `risk_set_descriptor()`), and direct string tests on
`spec$model` / `spec$sub_model`. The second is what the archived
`spec-driven-dispatch` change introduced to retire the third; it succeeded for
risk-set geometry and stopped there.

Measured on the current tree (2026-09-05): 9 variant classes; 21 internal S3
methods across `compute_event_contribution`, `preprocess`, and `estimate_int`;
3 of the 9 likelihood methods are literal aliases; 2 of the 6 non-DyNAMi
preprocess methods are byte-identical; `right_censored`/`intercept_scalars`
appear at 130 sites in 18 files and never disagree; 43 re-derivation sites read
`model`/`sub_model` outside the constructor.

Constraints:

- The frozen 1e-6 coefficient baselines and the C++ goldens do not move. This
  change alters no arithmetic; a moved baseline stops the work (ADR-0021).
- `src/` never sees these classes — no recompile, no C++ risk.
- ADR-0025 makes the waiting-time distribution an orthogonal axis, so the
  descriptor must have a place for it before `parametric-rates` lands.
- `class-naming-scheme` renames class strings on the same files. These two
  must not run concurrently (see D8).

## Goals / Non-Goals

**Goals**

- One object, built once, carrying every behavioral fact; every consumer reads
  it; nothing re-derives.
- One name per behavior, and the name says what the behavior *is*.
- No aliases: a method that exists is a method that differs.
- S3 dispatch retained only where implementations genuinely differ.
- Room for the axes already scheduled (distribution, mechanism) without
  multiplying classes.

**Non-Goals**

- Changing any exported function, argument, return value, or number.
- Merging DyNAMi into DyNAM. This change removes DyNAMi's *likelihood*
  aliases; its preprocessing difference is real today and stays until the
  effect-registry work closes it.
- Renaming class strings to `goldfish<Thing>` — that is
  `class-naming-scheme`'s job (D8).
- Implementing Weibull/Gompertz or the coordination mechanisms; this change
  only leaves the slot they need.

## Decisions

### D1 — One descriptor, computed once, replacing the variant's non-dispatch roles

The spec carries a single `behavior` descriptor built in
`model_spec_structure()` and nowhere else. It subsumes today's `risk_set`
list rather than sitting beside it: two descriptors would recreate the problem
this change exists to remove. Fields, each a closed vocabulary:

| Field | Values | Replaces |
| --- | --- | --- |
| `axis` | `sender`, `receiver_given_sender`, `dyad` | `risk_set$axis`, the indexing class |
| `timing` | `timed`, `ordinal` | `right_censored`, `intercept_scalars` |
| `likelihood` | `poisson`, `multinomial`, `coordination` | `risk_set$normalizer` |
| `input_shape` | `standard`, `grouped` | the DyNAMi variant classes |
| `distribution` | `exponential` (reserved: `weibull`, `gompertz`) | — (ADR-0025) |
| `fold_target`, `encoding` | as today | `risk_set` fields, carried through |

*Why a field and not more classes.* A class earns its place by dispatching a
different implementation. Preprocessing does not: it selects a loop and passes
flags, which is a lookup, not a polymorphism. Estimation genuinely does, so it
keeps dispatch (D3).

*Amended 2026-09-06 (Alvaro) — `axis` keeps three values, not two.* As first
written the row gave `axis` the vocabulary `sender`, `dyad` and said it
replaced "`risk_set$axis`, the indexing class". Those are two different things
on the tree, and only the second is two-valued: the indexing class is
`goldfishAxisSender` / `goldfishAxisDyad`, while `risk_set$axis` also takes
`receiver_given_sender` for choice, which D1a does not address.

That third value is load-bearing. Twenty sites in `R/` branch on it, and
`risk_set_axis()` is exported and documents it — indeed the example in its own
help page is precisely the distinction it draws, that position `i` names a
sender under a rate model and a receiver-given-sender under a choice model.
Collapsing it into `dyad` would change an exported return value and erase the
fact the accessor exists to carry, neither of which this change is entitled to
do.

So `axis` takes `sender`, `receiver_given_sender` or `dyad` — the risk-set
geometry, minus the `dyad_symmetric` that D1a retires — and the preprocessing
loop is read off it as `axis == "sender"` rather than needing a field of its
own. That reproduces today's dispatch for all six non-DyNAMi variants exactly:
the two sender-recipe variants are the two whose axis is `sender`. The
original two-value vocabulary was an abbreviation error, not a proposal to
merge choice into the dyadic geometry.

### D1a — Symmetry is a likelihood property, not a risk-set axis

Corrected 2026-09-05 (Alvaro). Today's `risk_set$axis` takes a fourth value,
`dyad_symmetric`, for one-mode coordination. That conflates two different
facts. Coordination allows **asymmetric covariates** — the statistics are
computed on the same reduced `n1r x n2r` dyad grid every dyadic model uses,
and the code says so: the branch that scatters a reduced grid into the full
one treats `dyad` and `dyad_symmetric` identically. What differs is the
*reduction*: coordination's realized risk set is the unordered pair list
`{a > b}`, so its likelihood sums each unordered dyad once instead of twice.
That divergence lives entirely in the event-reduction step.

So `axis` takes `sender` or `dyad` only, and symmetrization is carried by
`likelihood = "coordination"`, which the likelihood dispatch already reads.
Preprocessing, which is what `axis` drives, sees no difference — which is the
test of whether a fact belongs on that field.

*Consequence:* the `dyad_symmetric` string is retired rather than renamed. Any
site testing it tests `likelihood == "coordination"` instead.

### D2 — `timing` names the behavior; `right_censored`/`intercept_scalars` retire

The pair is one fact under two names, and neither names the fact. Both are
TRUE exactly when the sub-model is a **timed rate** — a waiting time with an
exposure denominator — and FALSE for an ordinal comparison (`rate_ordered`,
`choice`, `choice_coordination`), where only the ordering of events is
modeled and the elapsed time carries no likelihood contribution.

`behavior$timing` takes `timed` or `ordinal`. The recipe loops keep boolean
parameters at their own signatures if that is convenient, but they are derived
from `timing` at the call site, and no other site names either old flag.
*Rejected:* keeping `right_censored` as the single name — it describes a
consequence in the estimation code rather than the property of the sub-model,
which is why the second name was needed in the first place.

*There is a third name (found 2026-09-05).* The preprocessed object stores
both fields, and stores one as a copy of the other:

```r
# R/preprocess_writers.R
has_intercept  = has_intercept,
right_censored = has_intercept,
```

`has_intercept` appears at 129 sites across 15 files, a comparable surface to
the pair it duplicates. It is not a straight synonym everywhere — it states a
property of the *formula* (an intercept term is present), while `timing`
states a property of the *sub-model* — but on the preprocessed object they
carry the same value, which is how a reader ends up unable to tell whether
three knobs are one knob.

The migration therefore has to decide, per site, which of the three a use
actually means, rather than mapping all of them onto `timing`. Where a site
genuinely asks "does this formula carry an intercept term", that question
survives and keeps `has_intercept`; where it is standing in for "is this a
timed rate", it becomes `timing`. Task 3.1 covers the pair; the
`has_intercept` sites are inventoried in task 1.2 and triaged there, because
collapsing them blindly would erase a real distinction.

### D3 — Dispatch survives only for the likelihood

`compute_event_contribution` keeps S3 dispatch: six genuinely different
implementations, and adding a mechanism or a distribution means adding a real
one. It dispatches on a **likelihood class** carried by the spec, not on
`model × sub_model` — so `dynam_choice` and `dynam_choice_coord` map to
different likelihood classes (their methods differ, 46 vs 56 lines) while
`dynam_rate` and `dynami_rate` map to the *same* one, which is what deletes
the aliases rather than re-registering them.

`estimate_int` keeps dispatching on the axis. `preprocess` stops dispatching
per variant and reads `behavior`.

### D4 — Aliases are deleted, not renamed

`compute_event_contribution.dynami_{rate,rate_ordered,choice}_spec` are
literal assignments to their DyNAM counterparts. Under D3 they simply do not
exist: DyNAMi specs carry the same likelihood class. This is the change's
clearest debugging win — a stack frame will name the method that actually
ran.

### D5 — `model` and `sub_model` stay, as provenance

They remain fields, because print methods, error messages, and the user's own
mental model need "you asked for a DyNAM choice model". They stop being
switches. The distinction is enforced by D6, not by convention.

### D6 — A test forbids re-derivation

A guard test greps `R/` for `sub_model ==`, `sub_model %in%`, `model ==` and
`model %in%` outside `R/model_spec.R` and fails on any hit. Without it the 43
sites grow back one convenience at a time. The constructor and the validators
that check a *user-supplied* value against the allowed set are the documented
exceptions (`class_checks.R`, `formula_validate.R`), listed explicitly rather
than pattern-matched.

### D7 — Migrate per consumer, tests green at every commit

Order, cheapest and most-isolated first: descriptor construction (additive,
nothing reads it yet) → preprocessing recipe selection → the
`right_censored`/`intercept_scalars` rename → likelihood dispatch and alias
deletion → re-derivation sweep → guard test. Each is one commit with the
full `NOT_CRAN=true` suite green, so any step can be reverted alone.

The rename step is the wide one (130 sites, 18 files) and is mechanical, but
it is **not** a blind search-and-replace: the flags appear as function
parameters, as list fields, and in prose comments explaining the old
semantics, and the comments must be rewritten to the new concept rather than
have the identifier swapped under them.

### D8 — This change and `class-naming-scheme` do not run concurrently

Both rewrite `R/model_spec.R`, `R/model_preprocess.R` and
`R/estimation_core.R`. Worse, they disagreed about the destination: the rename
gives `dynam_rate_spec` a `goldfish<Thing>` name, while this change deletes
several of those classes outright. Renaming a class and then removing it is
pure waste, and doing both in one branch makes each diff unreviewable.

*Decision (Alvaro, 2026-09-05):* **`class-naming-scheme` lands first; this
change follows.** The rename is already artifact-complete and its blockers are
cleared, while this change is a fresh proposal; sequencing the finished work
first keeps the rename's diff mechanical and reviewable.

The accepted cost is that the `model_spec` hierarchy gets renamed and then
partly dissolved: this change collapses variants whose implementations are
identical, so a handful of names minted by the rename will not survive it.
That is bounded — around eleven internal, unexported class strings, no user
surface — and paid once. It is recorded here rather than discovered later.

*Resolved 2026-09-06.* `class-naming-scheme` has **landed** (48/50 on
`refactor/class-naming-scheme`), and its task 1.1a settled the scope question
the other way: the `model_spec` rows stay in the rename table and the waste is
accepted, so the change carries one rule with no carve-out. The alternative
previously recorded here — excluding the hierarchy from the rename table and
letting this change name those classes when it reshapes them — is therefore
**closed**, not merely unchosen. The concrete starting state for this change:

| Was | Is now |
| --- | --- |
| `model_spec` | `goldfishKind` |
| `dynam_rate_spec`, `dynam_rate_ordered_spec` | `goldfishKindDnRate`, `goldfishKindDnCox` |
| `dynam_choice_spec`, `dynam_choice_coord_spec` | `goldfishKindDnChoice`, `goldfishKindDnCoord` |
| `dynami_rate_spec`, `dynami_rate_ordered_spec`, `dynami_choice_spec` | `goldfishKindDniRate`, `goldfishKindDniCox`, `goldfishKindDniChoice` |
| `rem_rate_spec`, `rem_rate_ordered_spec` | `goldfishKindRemRate`, `goldfishKindRemCox` |
| `sender_spec`, `dyad_spec` | `goldfishAxisSender`, `goldfishAxisDyad` |

The two axis classes are the ones this change leaves alone, so they are the
two of the twelve expected to survive it.

### D9 — The preprocessing output classes collapse into one, distinguished by fields

New (2026-09-05, Alvaro). The same over-specification the variant classes show
appears again on the output side, measured on the current tree:

| Class | S3 methods | `inherits()` checks |
| --- | --- | --- |
| `goldfishStat` | 1 (`print`) | 3 |
| `goldfishStatDB` | 0 | 0 |
| `goldfishFlavPrep` | 0 | 0 |
| `goldfishFlavStat` | 0 | 0 |
| the `output = "gather"` return | **no class at all** | — |

Four classes and one unclassed shape, with one method and three checks between
them. The two flavored classes are structurally identical —
`structure(outputs, process_map = process_map, class = …)` — differing only in
which function built them, which is provenance, not behavior. And the gather
shape, a user-facing return of an exported function, carries no class, so it
is invisible to `inherits()`, to print dispatch, and to any rename.

`compute_statistics()` SHALL return one class, `goldfishStat`, with the
distinctions carried as fields:

| Field | Values | Replaces |
| --- | --- | --- |
| `storage` | `pointer`, `stack`, `db` | `goldfishStat` / gather / `goldfishStatDB` |
| `scope` | `single`, `flavored` | the `goldfishFlav*` pair |

One `print` method reads them. The gather gap closes by construction rather
than by minting a fifth class.

*The `test_*()` precedent, read precisely.* The diagnostic tables were the
model for this, and they do **not** collapse to one class:
`new_diagnostic_table(df, class, context, params, defining)` shares one
constructor and one metadata stamp while keeping distinct class strings. They
keep them because autograph plots each differently — a real dispatch
difference. Here there is none, so the same pattern lands on one class plus
metadata. The rule the precedent actually carries is *share construction and
metadata; keep a class only where behavior branches* — which is D3's rule for
the likelihood, applied to the output side.

*Boundary, and the burden of proof (Alvaro, 2026-09-05).* `goldfishFlavPrep`
and `goldfishJointPrep` are internal preprocessing containers rather than
`compute_statistics()` returns, so they are inventoried separately — but the
default is **convergence, not separation**. Keeping a preprocessing container
distinct from the statistics output requires a **strong** reason: a real
dispatch difference, or a field one shape carries that the other cannot. Minor
differences are not a reason to keep two classes; they are a reason to
converge, and where the inventory finds only minor differences the convergence
is proposed and approved rather than assumed either way. The failure this
guards against is the one already measured here — four classes, one method,
three checks — which arose precisely from treating small differences as
grounds for a new class.

`goldfishPrepCtrl` is out of scope entirely: `set_preprocessing()` controls
preprocessing rather than producing statistics, so it is a different kind of
object, not a variant of the same one.

### D10 — The mechanical flavored fan-outs collapse; the contract table does not

New (2026-09-05, Alvaro). Seventeen generics are implemented twice, once for
`goldfishFit` and once for `goldfishFlavFit`
([ADR-0038](../../../decisions)). Classifying them shows two different
problems wearing one label:

| Shape | Generics | Size |
| --- | --- | --- |
| helper fan-out | `fitted`, `predict`, `residuals`, `evaluate_model` | 8–19 lines |
| trivial hand loop | `coef`, `vcov` | 8 lines each |
| large hand loop | `test_gof`, `test_parameter`, `test_time` | 48–67 lines |
| bespoke | `diagnose_outliers`/`changepoints`/`onset`, `margin_table`, `coef_layout`, `print`, `augment`, `model_terms` | 11–69 lines |

The first six are this change's rule applied a third time. They carry no
behavior of their own: `predict.goldfishFlavFit` is a one-line call to
`flavored_component_apply()`, and `vcov` is the same `lapply` over
`object$results` written by hand instead of through the helper. A container of
N doing exactly what one does, N times, is the same finding as D9 — identical
behavior expressed as separate implementations.

**In scope:** those six collapse to a single fan-out path, with `coef` and
`vcov` moved onto `flavored_component_apply()` rather than keeping their hand
loops.

**Explicitly out of scope:** the shared fit-class parent and its
inherit/override/refuse contract table. That is not a duplication problem and
this change's rule does not reach it. ADR-0038's core is a correctness hazard —
a DyNES fit maximizes a Monte-Carlo estimate of the observed-data likelihood,
so a `logLik()` inherited from a parent lets `AIC()` and `BIC()` return numbers
that look like information criteria and are not. Whether each generic should
inherit, override or refuse is a modeling judgment that differs per generic; it
cannot be derived from a field the way `storage` and `scope` derive print
behavior in D9. Collapsing the six mechanical cases neither answers it nor
prejudges it, and the eleven remaining generics stay exactly as they are until
the change that owns that question decides them.

## Risks / Trade-offs

- **A mechanical rename inverts a comment's meaning** → D7 requires comments
  be rewritten, not have identifiers swapped; the diff is read per hunk.
- **A coefficient moves** → it cannot if the migration is correct; the frozen
  baselines are the detector and a move stops the task (ADR-0021).
- **The descriptor becomes a grab-bag** → fields have closed vocabularies and
  each must name a behavior some consumer branches on; a field nothing reads
  is deleted.
- **A future axis does not fit the fields** → accepted. `distribution` is the
  test case: it is reserved now, filled by `parametric-rates`, and if that
  proves awkward the shape is wrong while it is still cheap to change.
- **Losing per-variant dispatch hides a real difference** → the likelihood
  keeps dispatch precisely so a genuine difference has somewhere to live; the
  guard test fails if behavior is re-derived instead.

## Migration Plan

Internal only: no user migration, no deprecation, no NEWS-worthy behavior
change beyond an internal-refactor note. Rollback is per-commit (D7).

## Open Questions

- ~~Whether `class-naming-scheme` should exclude the `model_spec` hierarchy
  from its rename table (D8's alternative)~~ — **closed 2026-09-06**: the
  rename landed with the hierarchy included (its task 1.1a), so the classes
  this change dissolves are the `goldfishKind*` names, not the `*_spec` ones.
  See D8's resolution note for the old→new table.
- Whether retiring `dyad_symmetric` (D1a) touches the C++ boundary, or only
  the R-side reduction dispatch — `DyNAM_MM_default.cpp` is named in the
  comment at the branch site and must be checked before task 2.1.
- Whether `input_shape` earns its place, or DyNAMi's preprocessing difference
  should be expressed as recipe fields directly — answerable once the
  effect-registry work shows how much of the gap it closes.

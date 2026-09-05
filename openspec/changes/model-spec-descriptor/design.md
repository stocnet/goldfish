# Design — model-spec-descriptor

## Context

`model_spec_structure()` stamps `c(variant, indexing, "model_spec")`, where
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
| `axis` | `sender`, `dyad`, `dyad_symmetric` | `risk_set$axis`, the indexing class |
| `timing` | `timed`, `ordinal` | `right_censored`, `intercept_scalars` |
| `likelihood` | `poisson`, `multinomial`, `coordination` | `risk_set$normalizer` |
| `input_shape` | `standard`, `grouped` | the DyNAMi variant classes |
| `distribution` | `exponential` (reserved: `weibull`, `gompertz`) | — (ADR-0025) |
| `fold_target`, `encoding` | as today | `risk_set` fields, carried through |

*Why a field and not more classes.* A class earns its place by dispatching a
different implementation. Preprocessing does not: it selects a loop and passes
flags, which is a lookup, not a polymorphism. Estimation genuinely does, so it
keeps dispatch (D3).

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
`R/estimation_core.R`. Worse, they disagree about the destination: the rename
gives `dynam_rate_spec` a `goldfish<Thing>` name, while this change deletes
several of those classes outright. Renaming a class and then removing it is
pure waste, and doing both in one branch makes each diff unreviewable.

*Decision:* this change lands **first** where they overlap, or
`class-naming-scheme` explicitly excludes the `model_spec` hierarchy from its
table and picks it up afterward. Which of the two is chosen is a scheduling
call for the maintainer; the design only requires that it be made before
either starts, and recorded in both changes.

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

- Ordering against `class-naming-scheme` (D8) — which lands first.
- Whether `dyad_symmetric` is an `axis` value or an `encoding`, given
  one-mode coordination currently sets both.
- Whether `input_shape` earns its place, or DyNAMi's preprocessing difference
  should be expressed as recipe fields directly — answerable once the
  effect-registry work shows how much of the gap it closes.

# Design — fit-class-hierarchy

## Context

Three fitted-model classes exist or are imminent: `goldfishFit` (single
process), `goldfishFlavFit` (a container of per-fid fits), and a DyNES
fit from the `abmcem` Monte-Carlo EM loop. They are flat — the flavored class
does not inherit the single one — so sixteen generics are written twice, and
four (`summary`, `tidy`, `glance`, `print.summary`) were written once and
forgotten on the flavored side. `coef_layout` exists only on the flavored side.

Constraints:

- ADR-0038 is accepted and settles the shape (parent + contract table). This
  change implements it; it does not reopen it.
- `class-naming-scheme` renames these classes and explicitly disclaims the
  hierarchy (its D19), including a task keeping the fit classes flat so the
  rename cannot answer this silently.
- `model-spec-descriptor` D10 collapses six mechanically identical flavored
  fan-outs. Those rows should be gone before the table is written.
- No arithmetic changes; the frozen 1e-6 baselines do not move.

## Goals / Non-Goals

**Goals**

- A generic written once reaches every fit class unless deliberately excused.
- Every generic-by-class combination has a recorded, tested decision.
- A generic that is meaningless for a class fails loudly instead of returning a
  plausible number.
- Adding a fit class or a generic forces a decision rather than allowing an
  omission.

**Non-Goals**

- Renaming classes (`class-naming-scheme`).
- Collapsing the mechanical fan-outs (`model-spec-descriptor` D10).
- Deciding DyNES's estimation semantics; this change only records what its fit
  class may and may not answer.
- Changing any number.

## Decisions

### D1 — A parent class, with the contract table as the safety mechanism

The parent alone would be dangerous: it is what lets a Monte-Carlo `logLik()`
reach `AIC()`. The parent alone would also be the fix for the leak. Both are
true, so the parent ships **with** the table, never before it.

### D2 — Three verdicts, and `refuse` is the important one

`inherit`, `override`, `refuse`. The first two are ordinary. `refuse` is the
one the design exists for: a generic that is not meaningful for a class aborts
with an explanation naming the class and why. The alternative — no method, so
R's "no applicable method" — is *acceptable* but uninformative; the alternative
that must not happen is silent inheritance producing a number.

### D3 — `logLik` on a Monte-Carlo fit is the worked example

The DyNES fit maximizes a Monte-Carlo estimate of the observed-data likelihood.
Its `logLik` row is decided explicitly and, whatever it is, the consequence for
`AIC()`/`BIC()` is stated in the same place. If the verdict is `refuse`, the
message says the quantity is a Monte-Carlo estimate and names what to use
instead. This row is written first, because it is the reason the table exists.

### D4 — Completeness is enforced by a test, not by review

A test reads the table, enumerates the generics dispatching on fit classes and
the concrete fit classes, and fails on any cell without a verdict. Without it,
the table becomes another document that drifts from the code — which is exactly
how the four missing generics arose. A new fit class fails the test until its
column is filled; a new generic fails until its row is.

### D5 — The table is checked in as data, not as prose

A machine-readable table (a CSV or an R object under `R/`), so the test reads
the same artifact a human edits. Prose in a design document cannot be tested
and would drift.

### D6 — Order: rename, then fan-out collapse, then this

`class-naming-scheme` first (the table should name the final classes, not names
about to change). `model-spec-descriptor` D10 next where practical, so six rows
never enter the table. This change last, and before `abmcem` ships the DyNES
fit — a third flat class is what makes the omission permanent.

*Accepted cost:* this is the third change in a row to touch the fit-class
methods. The alternative, folding it into either predecessor, was rejected:
into the rename it would answer a correctness question inside a mechanical
diff, and into the descriptor change it would mix a modeling judgment with a
refactor whose rule explicitly does not reach it.

### D7 — The parent is `goldfishBaseFit`, and it carries the shape-level methods

New (2026-09-06, Alvaro). `goldfishFit` is the concrete single-process class
and cannot become the parent without a rename this change disclaims, so the
parent is a new name: `goldfishBaseFit`, which satisfies ADR-0031
(`goldfish` + a camelCase noun, no dot).

The parent is not a marker. The `goldfishFit` methods that need only the common
fit surface — `parameters`, `standard_errors`, `names`, `n_params`,
`log_likelihood`, `n_events`, `call` — move onto it: `print`, `summary`,
`tidy`, `glance`, `coef`, `vcov`, `logLik`, `model_terms`. `goldfishFit`'s
column then reads a real `inherit` for those eight, and the DyNES class will
inherit them without writing them a third time.

The generics that read a fit's estimation internals — `augment`, `fitted`,
`predict`, `residuals`, `evaluate_model`, `test_gof`, `test_parameter`,
`test_time`, `diagnose_outliers`, `diagnose_changepoints`, `diagnose_onset`,
`margin_table` — get **no parent default**, so a class that omits one gets a
dispatch error rather than a method reading fields it does not have. This is
ADR-0038's rejected Option C, with the boundary written into the table and
checked, which is what the ADR says Option D is.

`goldfishFlavFit` overrides every row: it is a container of fits rather than a
fit, so each of its methods is a fan-out over components. That answers
ADR-0038's first open question in the direction of *container* — its parent is
shared for the contract's sake, not because it is a subtype.

### D8 — The Monte-Carlo `logLik` verdict is deferred, its mechanism is not

New (2026-09-06, Alvaro). D3 asks for the `logLik` verdict on a Monte-Carlo fit
first, because it is why the table exists. The verdict is **not settled here**:
the DyNES fit class does not exist yet, so deciding what its `logLik` returns
would be deciding DyNES's estimation semantics, which this change's Non-Goals
exclude.

What ships instead is everything the decision needs: the `refuse` verdict, the
refusal helper reading its reason from the table, and the completeness test
that fails until the DyNES column is filled. The verdict is therefore forced at
the moment the class arrives — which is exactly D4's mechanism working — rather
than assumed now against an unwritten estimator.

The refusal machinery is not shipped untested: `coef_layout` on a
single-process fit is a genuine `refuse` today (D9), so the message has a live
row.

### D9 — Answers to the Open Questions

- **`coef_layout` on `goldfishFit`: `refuse`.** The layout is a coefficient
  surface over the fids of a joint specification; a single-process fit has no
  fid blocks, so a one-block layout would be an invented shape rather than a
  smaller one. The refusal names `coef()` and the joint fit as the alternatives.
  This also replaces the "no applicable method" a user currently gets from
  `set_parameters_from_result()` when handing it a single-process fit.
- **`summary`/`tidy`/`glance` on `goldfishFlavFit`: `override`, following the
  return-shape convention already documented on `augment.goldfishFlavFit`.**
  The tidy returns (`tidy`, `glance`) row-bind the per-process tables and
  append the process identity as columns; the non-tidy return (`summary`) is a
  list named by process label, unwrapped when `flavor =` selects one, exactly
  as `coef` and `vcov` behave.
- **`print.summary` is not a row.** `print.goldfishSummFit` dispatches on the
  summary class, not on a fit class. Because `summary()` on a flavored fit
  returns the same `goldfishSummFit` objects (one per process), the existing
  print method reaches them through ordinary list printing, and no second
  summary class is minted.

### D10 — The contract is a spec obligation, not an artifact (reverses D5)

New (2026-09-07, Alvaro). D5 put the table on disk as data "so the test reads
the same artifact a human edits". Built that way, it went into `inst/`, which
means it **installs** — and `refuse_fit_generic()` read its `reason` and
`alternative` cells at runtime to build the refusal message. The package
therefore shipped a 42-row governance table to deliver two sentences, and
assembled a cli message out of data rather than writing cli at the call site.

The premise turned out to be wrong in both directions. The table was never
needed for enforcement: a rule derived from the namespace alone — *every
generic registered on any fit surface is reachable for every fit class, by its
own method or the parent's* — reports the four gaps that motivated this change
(`summary`, `tidy`, `glance` on the container, and `coef_layout` on a single
fit) and is strictly stronger than the table-driven test, because there is no
table to go stale and agree with itself. And the table was in the wrong place
for its real job, which is not checking but **arguing**: the moment a missing
cell matters is when a change is being designed, not when a suite runs.

So the two jobs separate:

- **Enforcement** lives in `tests/testthat/test-fit_class_reachability.R`, with
  no artifact at all. A deliberate exemption still registers a method — one
  that refuses with a reason — so it satisfies reachability rather than needing
  an exemption list to keep in step.
- **The argument** lives in the `fit-class-hierarchy` capability's `spec.md`,
  which folds into `openspec/specs/` at the merge. That is the only place it
  can go and still travel: the living spec is strictly one `spec.md` per
  capability, so a sibling file would not fold, and the archive is append-only,
  so a grid there could never gain the DyNES column.

The grid is recorded as the rule plus the cells a reader could not derive — the
parent-borne group, the no-parent-default group, the container rule, and each
refusal with its reason. Enumerating all 42 cells would restate one rule 33
times and is the part most likely to drift, precisely because nothing checks it
any more.

*Accepted cost:* D5's argument against prose stands and is now the risk taken.
Nothing checks that a reason was written or that the grid still matches the
code; what replaces that is the OpenSpec workflow, since a change touching fit
classes must update the capability spec and is validated when it does. Vault
ADR-0052 records the reversal against ADR-0038's D5.

## Risks / Trade-offs

- **The table is filled in mechanically to make the test pass** → the `refuse`
  rows carry a stated reason, and the `logLik` row (D3) is reviewed
  individually.
- **A parent changes dispatch for a generic nobody re-examined** → that is what
  the completeness test prevents; no cell is undecided by default.
- **Users lose a working call** where a generic becomes `refuse` → intended:
  the previous behavior was either an uninformative error or, worse, a number
  that should not have been produced.
- **Third consecutive change over the same files** → accepted, D6.

## Migration Plan

Flavored fits gain `summary`, `tidy` and `glance`. Any `refuse` verdict turns
an "no applicable method" error into an explanatory abort. No stored object
changes; no re-fit required.

## Open Questions

All three are answered — see D8 (the DyNES `logLik` verdict, deferred to the
change that ships the class, with the mechanism landing here) and D9
(`summary`/`tidy`/`glance` on a flavored fit, and `coef_layout` on a
single-process fit).

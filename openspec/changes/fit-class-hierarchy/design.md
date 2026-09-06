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

- Is the `logLik` verdict for the DyNES fit `refuse`, or `override` returning
  the Monte-Carlo estimate with a class that `AIC()` cannot consume?
- Do `summary`/`tidy`/`glance` on a flavored fit `inherit` (one row per
  component) or `override` (a combined table)?
- Does `coef_layout`, currently flavored-only, become `inherit` for the single
  class or stay flavored-only as a deliberate `refuse`?

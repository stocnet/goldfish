# Design — tied-event-times

## Context

Measured on `Fisheries_Treaties_6070` via `fish_rem`
(`create_bilat ~ 1 + inertia + tie(contignet) + alter(states$regime)`):

```
  241 intervals, 172 right-censored, 69 dependent
   13 intervals with dt == 0
   12 of those carry a DEPENDENT event   (17% of all dependent events)
    1 is right-censored
   12 duplicated timestamps
```

The same fit is the one that stalls: its gradient floors at 6.1e-06 and cannot
be driven lower, and `tie/contiguity` in the neighbouring qmd specification
returns an estimate of -5.4 with a standard error of 31.8. A rate scale informed
by 57 of 69 events is a plausible contributor, though this change does not claim
to have proven that link.

**Key facts established before designing:**

- A zero-length interval contributes `-dt * total_rate = 0` exposure but a full
  `+linear predictor` term for the observed actor or dyad. Nothing errors; the
  arithmetic is well defined and the model is not.
- The multinomial families (choice, coordination, the `_ordered` rate variants)
  do not use `dt` at all, so ties are inert for them at the likelihood level.
  They are not inert for the *state*: the arbitrary input order still decides
  which tied event updates the network first, and therefore what the next tied
  event sees.
- A stocnet data object stores ties with a `time` column and no secondary key.
  Two events at the same timestamp have no representable order, so a user cannot
  express "these five simultaneous events, in this order" and hand it in.

## Goals / Non-Goals

**Goals:**

- A user fitting an exact-time model on tied data learns that they have done so,
  with the count, before interpreting the rate scale.
- A researcher can construct and fit an explicit ordering of tied events, so
  order uncertainty can be handled as missing data by whatever scheme they
  choose.
- The order a user supplies is the order the estimator uses — no silent
  re-sorting anywhere in the pipeline.

**Non-Goals:**

- Performing multiple imputation, or providing a pooling rule. goldfish supplies
  the mechanism; the researcher supplies the analysis. Building a `pool()` here
  would bake one combination rule into an estimator that has no business
  choosing it.
- Changing any coefficient. The warning describes existing behavior.
- Refusing to fit tied data. Ties are common and often harmless (a coarse clock
  on a slow process); refusing would break working analyses to make a point.
- Redefining the multinomial families' treatment of ties, which is already
  coherent.

## Decisions

### D1 — warn, do not refuse, and only for the exact-time families

The tie contradicts the Poisson assumption, but the severity depends on things
goldfish cannot see: whether the clock is coarse (timestamps rounded to the day,
so ties are an artifact and the ordering is genuinely unknown) or the process is
truly simultaneous. A warning that names the count lets the user judge; a refusal
substitutes the package's judgment for theirs and breaks existing analyses.

Scoped to the exact-time families because that is where the likelihood is
affected. The multinomial families get the ordering machinery (D3) but no
warning, since their likelihood is untouched by `dt`.

Rejected: an error. Rejected: warning on every family, which would train users to
ignore it on the models where it does not matter.

### D2 — record the count on the fit, not the tied events themselves

A count answers "does this model have the problem, and how much", which is what
a diagnostic or a reader of the fit needs. Storing the tied event indices would
be a per-event component on an object whose per-event storage is already
guarded, for a question nobody asks of individual events.

### D3 — order among tied events is explicit data, carried end to end

This is the load-bearing decision, and it is what makes the researcher's
treatment possible at all. The requirement is that an ordering supplied by the
user is representable in the data object and survives to estimation unchanged.

The alternative — having goldfish randomize internally behind a `seed` argument
— was rejected. It looks convenient and it is wrong for this problem: the point
of treating order as missing data is that the *analyst* controls the imputation
model and sees every draw. An internal randomizer hides the draws inside a fit
object, makes the ordering impossible to inspect or reproduce outside goldfish,
and invites exactly one scheme (uniform over permutations) that may not suit the
data. Enabling the researcher is both more honest and less code.

**Open:** the mechanism. A secondary sort key on the event stream, a stable
documented sort with an exposed pre-sort hook, or an explicit order column are
all candidates and they differ in how much of the stocnet contract they touch.
Task 0.2 settles it against the actual event-stream code before anything is
built.

**Consumer note (2026-08-19):** the `recency-effects` change binds its
freeze-then-update tie-block contract (statistics read the ordering as of
the block's start; buffer pushes happen once at block end) to whatever
mechanism D3 settles — it consumes the block-boundary definition rather
than deriving its own. Task 0.2's choice therefore has a second consumer
beyond the likelihood warning: per-effect state whose update order is
tie-sensitive.

### D4 — the documentation carries the recipe, the package carries the mechanism

The imputation workflow (generate orderings, refit each, combine) is documented
prose with a worked example, not an exported function. Its steps are all
ordinary R over an exported surface once D3 lands. Writing `impute_order()`
would freeze one scheme and one pooling rule into the package.

## Risks / Trade-offs

- [A warning on a common data shape becomes noise users learn to suppress] → it
  fires only on exact-time families, only when a tie actually exists, and reports
  a count so the magnitude is visible; a single tie in ten thousand events reads
  very differently from 12 in 69.
- [D3 may reach further into the stocnet contract than expected] → task 0.2
  scopes the mechanism before any code, and the fallback (documented stable sort
  plus a pre-sort hook) is far cheaper than a schema change if that is what the
  investigation finds.
- [Recording a count invites someone to gate on it later] → the requirement says
  the fit is still returned, so a future gate is a deliberate change rather than
  a drift.
- [The link between tied events and `fish_rem`'s stall is plausible but
  unproven] → this change does not rest on it; the warning is justified by the
  model assumption alone.

## Open Questions

- Does a tied *right-censored* interval need the same treatment as a tied
  dependent event? There is one in the fixture, and it contributes nothing to
  either term, so it may be simply droppable.
- Should the exact-time contribution skip a zero-length interval's exposure term
  entirely rather than adding a zero? Arithmetically identical today; it would
  matter if the interval count is ever used as a denominator.

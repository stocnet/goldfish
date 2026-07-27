# Design — coordination-tie-consistency

## Context

Measured on `Social_Evolution`, `calls_dependent ~ inertia + trans`,
`sub_model = "choice_coordination"`, at the converged estimate, comparing
`backend = "r"` against `backend = "cpp"`:

```
  439 events, 107 disagree on observed_rank  (24%)
   73 of the 107 differ by exactly 1 rank
  median observed rank among disagreeing events: 4   (1st quartile 2)
  largest disagreement: 478 ranks, out of a 3477-pair list

  event 27 (r rank 2, cpp rank 3):
    6972 nonzero candidates
    r  -> 28 distinct probability values
    cpp -> 19 distinct probability values
    top two distinct values on r:
      0.17545706347744958
      0.010426886209440886   and   0.010426886209440883   <- differ in the
                                                              last two bits
```

The mechanism is visible in that last pair: two values the model makes equal
come out as different floats, and how many such splits occur depends on the
backend. Ranking uses a strict `>`, so a competitor that lands one bit above the
observed counts and one bit below does not.

**Facts that rule out the easy explanations:**

- **Not tie prevalence.** `se_rem` has 13 distinct values among 6972 candidates
  and 8 tied with the observed — more degenerate than coordination — and zero
  disagreements. Exact blocks that stay exact within a backend rank identically
  even when the backends differ in absolute value.
- **Not cross-backend noise alone.** Every family differs across backends at
  1e-15 to 1e-13, including `se_rem_ordered` at 2.9e-13 with zero disagreements.
- **Not mirrored dyads.** `src/DyNAM_MM_default.cpp` ranks over the unordered
  triangle (`a*(a-1)/2 + b`), so `(i,j)` and `(j,i)` are not both present. The
  ±1 differences are not a double-counting artifact.
- **Not endogeneity in general.** `fish_dynam_choice_coord` uses the same
  coordination likelihood and disagrees on nothing, because
  `tie(contignet)` and the regime covariates genuinely differentiate dyads:
  7868 distinct values among 22052 candidates. `se_dynam_choice_coord`'s formula
  is purely structural, so on a nearly empty early network most dyads are
  mathematically identical.

So the failure needs the conjunction: a formula that makes many alternatives
exactly equal, **and** a computation that does not keep them equal.

## Goals / Non-Goals

**Goals:**

- Alternatives the model makes equal are equal in floating point, within a
  backend and across backends.
- A user's `observed_rank` on a coordination model does not depend on which
  backend produced it.
- A decision on whether coordination over directed events is supported.

**Non-Goals:**

- The tie-breaking rule for ranks, and its tolerance. That is `residuals-gof`'s,
  which owns ranks and `recall@k`. Consistency does not remove exact ties; it
  makes them exact everywhere, which is the precondition for any rule to be
  reproducible.
- Changing the coordination likelihood's definition. This is about how it is
  computed, not what it is.
- The other families, which already agree.

## Decisions

### D1 — fix the values before choosing a rule for ranking them

A tolerance-based tie rule would take the disagreements to zero on its own —
measured at 106 to 0 with a collapse tolerance of 1e-12 or looser. That is a
real mitigation and `residuals-gof` should adopt one regardless. It is not a
substitute for this change, for two reasons.

First, it hides a genuine defect: a computation that gives different answers for
alternatives the model says are identical is wrong independently of who consumes
it. Second, the tolerance would have to be chosen large enough to absorb the
inconsistency (>1e-12 here) rather than merely large enough to absorb ordinary
floating-point noise (~1e-15). Fixing the source lets the tie rule use the
smaller, better-justified tolerance.

Rejected: treating the tie rule as sufficient and closing this. Rejected: fixing
the values and skipping the tie rule — exact ties are real in this model and
something must still decide their order.

### D2 — the parity requirement is sharpened, not added

`backend-primitive-parity` already requires the backends to agree numerically on
every primitive, at 1e-10. Coordination *passes* that: its cross-backend
difference is 5.6e-13. The requirement is satisfied and the user-visible answer
is still backend-dependent, because `observed_rank` compares values exactly and a
tolerance-based parity requirement cannot see an ordering flip.

So the sharpening is specific: for alternatives the model makes equal, agreement
must be exact, not within tolerance. That is a stronger statement about a
narrower set of values, and it is the one that would have caught this.

### D3 — the directed-stream question is settled here but implemented elsewhere if it bites

Whether `se_dynam_choice_coord` is a meaningful specification is a modeling
question, and this change is the one that found it. But acting on the answer —
aborting, warning, or requiring pre-symmetrized data — touches the estimator's
validation surface and potentially removes a frozen baseline cell.

This change therefore *decides and records*; if the decision requires a guard,
that guard is scoped as its own task here only if it is small, and otherwise
handed to the change that owns estimation validation. The baseline-cell
consequence is flagged for whoever cleans the freeze set, since a cell that is
not a defensible specification is a poor regression detector regardless of this
investigation's outcome.

## Risks / Trade-offs

- [Fixing the values moves coordination coefficients and the frozen baselines] →
  the two coordination cells are the ones at risk; regeneration is deliberate and
  documented, and the other ten cells are the control.
- [The root cause may be in the shared symmetrization rather than the kernels] →
  that would widen the change to the undirected risk-set path; task 0.1 locates
  it before anything is edited, so the scope is known before it is committed to.
- [`residuals-gof`'s tie rule may land first and mask the symptom] → the
  measurement in this proposal is recorded with the tolerance that produces it,
  so the underlying inconsistency stays visible even once ranks stop disagreeing.

## Open Questions

- Where exactly does the split enter: the mutual product, the symmetrization, or
  the triangle accumulation order?
- Does the same inconsistency affect `margins` and `probabilities` in a way no
  current test detects, since both are compared with tolerances?
- Is `fish_dynam_choice_coord` clean because it is genuinely consistent, or only
  because its covariates hide the problem?

# Classical-equivalence references

These are **not** goldfish coefficient baselines. Those live in `_baselines/`
and are the package's own regression floor — the same code, frozen, so drift
against itself is detectable. These are the opposite kind of artifact: numbers
minted by a *different* implementation, so a goldfish result can be checked
against something goldfish did not compute.

`test-classical_equivalence.R` reads them.

## The equivalences

goldfish's likelihoods are classical likelihoods with a network design matrix,
so each sub-model has a classical fit that is the **same** likelihood, not
merely a similar one:

| goldfish | classical twin | why |
|---|---|---|
| ordinal REM | `survival::coxph(ties = "breslow")` | Cox partial likelihood, dyads in start/stop form on the event clock |
| DyNAM choice | `survival::clogit` | a conditional logit *is* a stratified Cox; one stratum per event |
| ordinal rate | `survival::clogit` over the actors at risk | same, with the sender as the chosen alternative |
| exact-time rate / REM | `stats::glm` Poisson with `offset(log(dt))` | piecewise-exponential equivalence |

A disagreement is therefore a goldfish bug, not a difference of model.

**The design matrix is walked by hand** in
`generate_classical_references.R`, from the event list, with a dense adjacency
matrix updated one event at a time. Neither goldfish's statistic engine nor
any other package computes it. That is the point: a reference built from
goldfish's own statistics would test the likelihood alone, while this tests
the statistic and the likelihood together. The walk is deliberately naive so
that it is obviously right rather than fast.

Only the survival-derived numbers are frozen. The `stats::glm` equivalences
run **live** in the test: stats ships with R, so there is no provenance
question and nothing to keep in step.

## What was measured

Two different quantities, and they deserve two different tolerances.

**Independently optimized** — two optimizers reaching the same maximum from
different code, so the gap is convergence noise:

| comparison | max abs coefficient difference |
|---|---|
| exact-time rate ↔ `glm` | 3.1e-12 |
| ordinal REM ↔ `coxph` | 6.5e-11 |
| ordinal rate ↔ `clogit` | 1.2e-10 |
| DyNAM choice ↔ `clogit` | 1.1e-08 |
| exact-time REM ↔ `glm` | 2.9e-08 |

Worst case 2.9e-08, so the tests gate at the package's ordinary **1e-6**
discipline. Nothing here justifies a tighter number, and a tighter one would
be a test of the two optimizers rather than of the model.

**At a shared parameter vector** — no optimization on either side, so what is
left is the convention alone:

| comparison | max abs difference |
|---|---|
| Schoenfeld residuals | 2.7e-11 |
| scaled Schoenfeld residuals | 3.0e-14 |

This is the load-bearing comparison. It pins the Grambsch–Therneau scaling
`theta + n I^-1 s_k`, the `n` included — an n-fold error there leaves every
rank and every plot shape intact and shows up only against a second
implementation. On a Cox-expressible fixture every likelihood event is a
death, so survival's event count and goldfish's dependent-event count coincide
by construction; that is exactly why the *matrix* comparison, and not the
count, is what carries the check.

The test still gates this at 1e-6 rather than at 1e-10: the frozen residuals
were evaluated at goldfish's fitted vector, so they are only meaningful while
that vector still agrees, and the test asserts that first.

## Coordination has no twin, and this is why

`sub_model = "choice_coordination"` is **absent on purpose**. Its unordered-dyad
log-weight is

```
log w_ij = log P(i->j) + log P(j->i)
         = (s_ij + s_ji)'theta  -  LSE_i(theta)  -  LSE_j(theta)
```

with `LSE_i(theta) = log sum_j' exp(s_ij''theta)`. A conditional logit over the
dyads at risk would need this to be linear in `theta` up to something constant
within the event. The two log-sum-exp corrections are neither: they are
nonlinear in `theta`, and they are indexed by *that dyad's own endpoints*, so
they vary across the alternatives of one event and do not cancel in the
stratum.

Measured rather than argued, on the same calls fixture with a single weighted
inertia term: goldfish returns 0.2849 and the `clogit` on the summed pair
statistics `s_ij + s_ji` returns 0.0575 — a difference of 0.227, with
log-likelihoods of -2522.8 and -2804.6. Those are different models, not a
numerical discrepancy.

## Provenance

Recorded inside the `.rds` under `provenance`: mint date, R version, survival
version, goldfish version, the fixture, and the note that the design was
hand-walked. The fixture is the `social_evolution` calls layer — 439 events,
84 actors, `start_time = min(time) - 1` so the opening interval has positive
elapsed time and the Poisson offset is finite.

Regenerating is a deliberate act needing a documented justification, exactly
as for the coefficient baselines. From the package root:

```sh
Rscript tests/testthat/_references/classical_v1/generate_classical_references.R
```

## Other REM packages

None is in the validation chain, and none is in `Suggests`. The comparison
against `remstimate` that the harness ran once
(`.plan/residuals_comparison.qmd`, coefficient parity ≤ 7e-07) stays there as
recorded evidence with an output-mapping note; no test derives from it.

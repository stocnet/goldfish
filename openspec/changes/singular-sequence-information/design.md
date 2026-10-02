## Context

The DyNES evaluator contract (`abmcem-optimization`, "Pool evaluation through
the zero-iteration engine path") says a pooled sequence whose information
matrix is singular yields a non-finite result, its weight vanishes, and the
ESS guard absorbs the loss. The rule was written for the adapter, which
inverted the information on every pass and would otherwise have aborted the
whole pool. When `dynes-augmentation` task 4.1 moved evaluation to the
compiled `cpp` kernel, which never inverts, the rule was kept by an explicit
`solve(fisher)` in `pool_cpp_eval()` (`R/evaluator_dynes.R`), flagged at the
time as worth revisiting.

Where the per-sequence quantities are consumed:

```
quantity        consumer                         needs J_i invertible?
--------------  -------------------------------  ---------------------
loglik_i        weights f/q, ESS, Q, MCMC f'/f    no
score_i         SGD gradient, sandwich meat       no
J_i             I_c = sum w_i J_i (sandwich)      no -- only I_c
J_i (one path)  warm-start Newton fit per fid     yes, and it already
                                                  falls back to zero
```

So the per-sequence `solve()` is the only place that demands an invertible
`J_i`, and nothing downstream benefits from it. Verified on
`recipe_state_spec()` (calls `inertia` is identically zero on every path):
with the `solve()` removed, `random`, `sim` and `mcmc` all run to their
iteration cap instead of crashing.

The loop already stops on a pool that has lost every sequence, but its
predicate, `pool_lost_to_fixed_terms()`, reads the causes recorded at pool
entry and recognises only `"fixed_term"`.

## Goals / Non-Goals

**Goals:**
- A singular per-sequence information matrix no longer drops the sequence.
- No DyNES run can reach SGD batch selection or the MCMC chain with an
  all-zero weight vector, nor compute Q over a degenerate sequence under
  either weighting scheme.
- A singular pooled information at the estimate is reported, naming what it
  can name.

**Non-Goals:**
- Null-space attribution of collinear groups. That is
  `identifiability-diagnostics`' requirement ("A singular information matrix
  names the coefficients responsible", 0/22 tasks, not a dependency); D3
  leaves it a seam.
- Standard errors on the identified sub-block when `I_c` is singular. The
  covariances stay `NA`, as today.
- Changing the warm-start fallback, the fixed-term warning, or the ESS guard.
  The warm-start fallback zeroes a whole fid when its Newton fit fails, so a
  fid holding one degenerate coefficient beside identified ones loses their
  warm start too; on the reproducer the two degenerate coefficients each
  have a fid of their own (calls choice `inertia`, friendship dissolution
  choice `indeg`), so nothing is lost there.
- Reporting a near-singular pooled information that still inverts. A
  coefficient with information on only a few rare paths gets a tiny but
  nonzero diagonal and huge standard errors without a warning; condition
  reporting is `identifiability-diagnostics`' requirement.

## Decisions

### D1 — Degeneracy means a non-finite quantity, never a singular one

`pool_cpp_eval()` stops inverting the free block. A sequence is degenerate
when its walk fails, a fixed-only term is non-finite (unchanged from
`fixed-loglik-degenerate`), or the engine returns a non-finite log-likelihood
or an `NA` score or information entry at `theta`. A singular `J_i` with
finite entries is an ordinary sequence. The same evaluation unit serves the
pool, the proposal evaluator and `compute_lik_seq()`, so all three follow.
`compute_lik_seq()` is exported (experimental) and its roxygen promises the
old rule, so this is a user-visible change: its documentation is rewritten and
the NEWS fragment says a singular sequence now returns a finite,
non-degenerate result.

*Why not keep dropping, but only "rarely singular" paths?* Any drop rule
keyed on the path's own information is a selection on the latent path the
weights are meant to integrate over; there is no threshold at which it stops
biasing. *Why not regularize `J_i`?* Nothing inverts it, so there is nothing
to regularize.

### D2 — The loop stops when no sequence carries weight, whatever the cause

The fully-lost-pool predicate moves from entry causes to the quantities the
loop is about to use: if no sequence of the current pool has both a finite
log-likelihood and a positive weight at `theta_k`
(`!any(is.finite(run$loglik) & weights > 0)`), the loop stops before Q, the
ESS guard or the M-step, returning the last accepted parameters with
`converged = FALSE` (the existing `abem_loop_result()` path). This subsumes
the fixed-term case and also covers walk failures and degeneracy that arises
only at evaluation time, which has no entry cause.

The finite log-likelihood is part of the test because weights alone do not
see degeneracy under uniform weighting: `compute_pool_weights()` returned
`rep(1 / n, n)` whatever the log-likelihoods, so a weight-only test would
never fire there and would drop today's uniform fixed-term stop.

Uniform weighting is fixed alongside. Its E-step ignored the weights
(`compute_q.goldfishEstepUniform()` is `mean(lambda)`), so a single degenerate
sequence, `lambda = -Inf - -Inf = NaN`, made Q `NaN`. Uniform weights now
spread equally over the sequences with a finite current log-likelihood and
are zero elsewhere, and the uniform Q and ASE are the mean and its sample
standard error over the positive-weight sequences. The filter keys on the
*current* log-likelihood, not on `lambda`: a finite current with a `-Inf`
proposal must still give `Q = -Inf` and a rejection. Uniform weighting runs
only with `mcmc` and refresh, where the chain rejects non-finite states, so a
partly degenerate uniform pool is rare (a degenerate chain start, a walk
failure); the fix is defensive and cheap, and the 6.2c degeneracy sweep is
where it would surface. The combined predicate is kept even though the new
uniform weights would make "positive weight" sufficient: it states the intent
without leaning on the weighting function's behavior.

Reporting: when every sequence was lost to the fixed-only cause the existing
`goldfish_dynes_fixed_term_degenerate` warning keeps saying so, unchanged.
Otherwise the run emits one warning of a new class,
`goldfish_dynes_pool_degenerate`, stating that estimation stopped because no
sequence had a finite likelihood and how many were lost to walk failures and
to non-finite evaluation. On a pool lost to mixed causes both warnings fire,
one per class: the fixed-term one with its count (a goldfish bug to report)
but no stop statement, the pool one with the stop. The `pool_dropped` tally
flag keeps its meaning.

*Alternative rejected:* guarding `sample.int()` alone. It ends the crash but
leaves a full-batch M-step at `Q = ASE = 0`, meeting the stop rule at a theta
that never moved, and uniform weights at `Q = NaN`: the failures the
fixed-term stop was written to prevent.

### D3 — A singular pooled information warns, naming zero-information coefficients

After the covariance pass at the returned estimate, when
`sandwich_covariances()` cannot invert `I_c` the fit emits one warning of
class `goldfish_dynes_singular_information`. It names, through the composite
labels, every free coefficient whose pooled information diagonal is zero (its
statistic has no variation on any weighted path, the unambiguous case and the
one the toy fixture hits). When no diagonal is zero it says the information
is singular without naming a culprit, rather than guessing at collinearity.
`fit$parameters` is not touched (an `NA` there would break
`dynes_fit_parameters()` and the `set_parameters()` round trip). The
covariances stay `NA`. The per-pass `em_trace` standard errors stay silent,
since a warning per pass would repeat one fact.

**The zero test is exact: `I_jj <= 0`.** It is scale-free: a ratio against
the largest diagonal compares coefficients whose statistics carry different
units, and could name a well-identified small-scale coefficient beside an
unrelated collinear pair. A statistic that is zero on every alternative gives
an exact zero on the compiled kernels; `<=` also absorbs the slightly negative
diagonal rounding can leave. The cost is accepted: the choice kernels form the
information uncentered (`sum_j p_j s_j s_j' - E E'`), so a statistic that is
constant but nonzero within each event's alternatives leaves a rounding
residue of order `eps * c^2`, is not named, and falls to the unnamed message.
Explaining that case is the statistics-based diagnosis of
`identifiability-diagnostics`, not this change.

**The starting-value claim is checked, not assumed.** The diagonal is read on
the final pool at the final estimate, but the pool changes across passes
(refresh, ESS-guard redraw, growth appends): a statistic on an augmented layer
can vary on an earlier pool, move the coefficient, and vanish on the final
one. The warning says a named coefficient was not estimated and stays at its
starting value only when its returned value equals its starting value;
otherwise it says the coefficient has no information in the final pool and
that its returned value, which it states, is not an estimate. On the
reproducer two coefficients are named, both with the first wording: calls
choice `inertia`, whose statistic is zero on every possible path (calls is
observed, not augmented, and no sender calls twice), and friendship
dissolution choice `indeg`, whose every dissolution is a forced choice (each
dissolving sender holds a single out-tie), so it contributes nothing to the
likelihood on any path.

D3 does not run after a D2 stop (`pool_dropped`): the covariance pass there
keeps no sequence, the information is `NULL`, and the stop already has its
warning.

**Shape of the helper, built as the seam for `identifiability-diagnostics`.**
Analysis and reporting are split: `singular_attribution(information)` returns
which coefficients are implicated (today the single coefficients with a zero
diagonal and no groups), and a DyNES renderer turns it, the composite labels
and the starting/returned values into the warning. The ML inversion sites that
`identifiability-diagnostics` targets abort rather than warn, so they will
reuse the analysis with their own renderer, and its null-space SVD will fill
in the collinear groups without a change to the DyNES renderer's callers. This
cannot contradict what D3 names: `I_c` is a non-negatively weighted sum of
positive semi-definite matrices, so it is PSD (up to rounding), and in a PSD
matrix `I_jj = 0` zeroes row and column `j`, making the unit vector `e_j` an
exact null vector, which is the SVD's "concentrated on one coefficient" case.
The call sits in `new_dynes_fit()` before the pool store is closed
(`res$pool$store <- NULL`), so the later statistics-based explanation can be
handed the store without moving the call site. The analysis is unit-tested on
synthetic matrices (collinear with no zero diagonal; all-zero diagonal;
a near-zero rounding residue) rather than through a fragile collinear DyNES
fixture.

A warning, not an error: the other coefficients are still estimated, and a
coefficient with no variation has a zero score on every path, so SGD leaves
it at its starting value; the fit is usable and the warning says which
coefficient to drop.

*Alternative rejected:* aborting at the first pool when `I_c` is singular.
It fails fast, but `I_c` is a weighted sum over a random pool, and a
coefficient with information on a few rare paths would abort a run that a
larger pool would have identified.

## Risks / Trade-offs

- [A near-singular `J_i` with large entries makes a noisy score] → It enters
  the gradient with its weight like any other sequence; the minibatch
  variance this adds is what Q's ASE already measures.
- [The exact zero test misses a statistic constant but nonzero within each
  event's alternatives] → Its diagonal is a rounding residue, not zero, so the
  warning fires unnamed; that is still a correct report, and naming it is
  `identifiability-diagnostics`' statistics-based diagnosis. A relative
  tolerance was rejected because it depends on the other coefficients' scale.
- [Tests that pin the old rule] → `test-evaluator_dynes.R` "a singular
  sequence yields a non-finite result, not a pool abort" and "the cpp
  evaluator keeps the singular-information guard" are rewritten to the new
  contract in the RED commit; the abmcem scenario they mirror is replaced by
  this delta.

## Migration Plan

Lands on `feature/dynes` after `fixed-loglik-degenerate` and before
`dynes-augmentation` task 6.2. A `NEWS.d/` fragment, no version bump. At the
merge, archive in order `abmcem`, `fixed-loglik-degenerate`, this change. To
roll back, revert the commits; no data or baseline is touched.

## Open Questions

None. The handoff to `identifiability-diagnostics` is settled in D3 (the
`singular_attribution()` seam, the call before the store closes) and recorded
in that change's proposal as a DyNES caller; it is not a dependency of this
change.

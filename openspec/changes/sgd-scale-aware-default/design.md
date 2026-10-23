## Context

The DyNES M-step is `optimize_abem_sgd()` (`R/optimizer_dynes.R`), called once
per decision pass by `run_abem_loop()` (`R/em_loop_dynes.R`). It ascends the
pool objective of iteration k,

```
Q_k(theta) = sum_i w_i * l_i(theta)        w_i fixed at theta_k (importance),
                                           or 1/m over the resampled multiset
```

with a score-only injected evaluator, and stops when the norm of the tested
gradient (exact under `variant = "full"`, a bias-corrected EMA of batch
gradients under `"minibatch"`) falls below `sgd_tolerance = 1e-3`, or at
`max_iterations = 100L`. The default step is `0.001 * gradient`.

The gradient is a sum over events, so its scale grows with the event count N,
while the standard errors shrink like 1/sqrt(N). How each existing schedule's
step scales:

```
  schedule          step ~                      relative to SE ~ 1/sqrt(N)
  ----------------  --------------------------  ------------------------------
  constant 0.001    eta * N                     under-steps small N (toy,
                                                oracle), overshoots large N (B3)
  adagrad / adam    eta, fixed in theta units   fine while SE >> eta; past
                                                that it overshoots (B4 at 0.1)
  I_c^-1 * g        the Newton step itself      invariant: in SE units by
                                                construction
```

The stop rule has the same defect: a gradient norm of 1e-3 means a
different distance on every pool, and on the cluster sweep it sat below the
minibatch noise floor (B5), so it never fired while the exact Newton step
to the pool optimum was 0.01-0.05 SE.

Three facts in the current code shape the design:

1. **The loop discards the M-step's verdict.** `run_abem_loop()` keeps only
   `$parameters` (`em_loop_dynes.R`, the `optimize_abem_sgd()` call), so the
   `em_trace` has no M-step column. The study had to monkey-patch the
   optimizer (`instrument_optimizer()` in `.plan/dynes/recovery/study_lib.R`)
   to see `iterations`, `converged` and `gradient_norm`.
2. **The unconverged M-step is not silent but unrecorded.** The optimizer
   warns once per pass with no condition class. A 50-iteration run with
   retries emits dozens of identical warnings, none in the trace, none in the
   fit's report, none catchable by class.
3. **The SE-scale quantities are already computed.** Every pass evaluates
   score and Fisher over the full pool at the proposal (`at_proposal`), and
   the fit's final covariance pass (`dynes_covariances()` in
   `R/fit_dynes.R`) has the weighted mean score S and I_obs at the returned
   estimate. D25's gate (`max |I_obs^-1 S / SE| < 0.1`, `dynes-augmentation`
   design) is therefore computable at the fit with no extra pool
   evaluation.

The requirement this changes, "SGD M-step over the concatenated parameter
vector", exists only in the unarchived `abmcem` change's `## ADDED` block.
The delta is written against that text (`depends-on: abmcem`), so the
placement pre-flight fails until `abmcem` folds; that is deferral, not a
defect.

## Goals / Non-Goals

**Goals:**

- A default `estimate_dynes()` M-step that reaches its pool optimum on the
  toy and the count-panel oracle, does not overshoot at the top flips level
  (B3), and does not drift away from a warm start already at the optimum
  (B4).
- An M-step stop rule in standard-error units, so its flag means the same
  distance on every pool.
- An M-step that ends unconverged is recorded per pass in the `em_trace`,
  warned about once per run with a condition class, and the fit reports
  whether its estimate is at the final pool's optimum.
- The user keeps every existing schedule: `constant`, `adagrad`, `adam` (and
  `decay`, `momentum`) remain selectable through `set_sgd_options()`.

**Non-Goals:**

- Changing the EM loop's accept/grow/stop rules or what `converged` means
  (it stays "the EM stop rule was met").
- Exposing adaptive-schedule hyperparameters (the abmcem rule stands).
- Per-sub-model factorized SGD, a final Newton polish step, or a
  per-coefficient gate (D25's "later" list stays later, except the
  scale-aware stop rule this change is).
- B2 (`goldfish_abem_retry_exhausted` at ASE = 0, Q < 0) and C4 (retry growth
  past `max_pool`); both are `abmcem` loop questions.

## Decisions

### D1. The default step rule is chosen by a bake-off, with the Fisher-scoring step as prior

The default is not fixed in this document. Group 1 of the tasks is a
measurement spike, with no package code, that runs the candidates below on
fixed fixtures and records the winner here (an amendment under this
heading) before any red/green pair is written.

**Candidates.**

| Id | Rule | Default `step_size` |
|---|---|---|
| A | `step_schedule = "adagrad"` (the study's incumbent, 2034 cluster fits) | 0.01 |
| B | `step_schedule = "fisher_scoring"` (new): `step = step_size * P^-1 g_t`, `P = I_c(theta_k)` from D3, `g_t` the tested gradient (exact under `"full"`, the bias-corrected EMA under `"minibatch"`) | 1 |
| ref | today's default, `constant` 0.001 | 0.001 |
| ref | `adam` 0.01 | 0.01 |

B is measured under both variants (B-mini and B-full). Under `"full"`, B is a
Newton iteration with the Hessian frozen at theta_k, which on the concave
DyNAM/REM log-likelihoods converges in a handful of steps. Under
`"minibatch"`, feeding the EMA rather than the raw batch gradient keeps the
step's noise at about `sqrt(0.053 * f / B)` SE_c (f the fraction of missing
information, B the batch size), below 0.05 SE for f <= 0.5 at B = 10.

**Fixtures.**

- the abmcem toy (`.plan/dynes/toy/toy_lib.R`; K = 30, 6-11 free theta);
- the count-panel oracle (`.plan/dynes/recovery/lib.R`, seed 3) with its
  closed-form MLE;
- B3: the top flips level (f33), coupled and uncoupled, first EM passes,
  `random` and `mcmc`;
- B4: the f02 replicates that aborted under adagrad 0.1
  (`.plan/dynes/recovery/verify_sgd.R`), warm start at the optimum.

**Acceptance criteria (in order; a candidate failing one is out).**

1. No B3 or B4 abort (`goldfish_abem_retry_exhausted`) attributable to the
   step: Q >= 0 within its ASE on the first pass of every B3/B4 run.
2. On the oracle, |theta_hat - MLE| < 0.01 oracle SE in every coordinate (the
   pilot's gap was 0.019).
3. On the toy and the oracle, the D2 M-step flag is TRUE on the last
   accepted pass, and the fit's `at_optimum` (D4) is TRUE.
4. Fit time on the toy and the oracle no more than 1.5x the reference
   default's (whose M-steps all run to the cap).

Among the survivors the preferred order is B-mini, B-full, A: B is the only
rule that is scale-aware in SE units, minibatch keeps the per-step cost
independent of the pool size. If no candidate survives, the group stops for
a design review. (The order is superseded by the amendment below: B-full is
the default.)

**The user keeps the choice.** Whichever rule wins becomes the
`set_sgd_options()` default. `constant`, `decay`, `adagrad`, `adam` and
`momentum` stay selectable. If B wins, `"fisher_scoring"` is added to the
schedule list. If A wins, no schedule is added, and the spec delta's
default wording and task group 3 are amended in task 1.3, before group 2.

*Alternatives considered:*

- *Fix adagrad 0.01 without a bake-off.* Rejected as the only path: it is
  scale-invariant in the gradient but not in theta, so it re-creates B4 on a
  pool whose SE is near 0.01. It stays a candidate, with its evidence.
- *Normalize the step by the event count.* Makes the step O(1) in N but
  leaves per-coordinate scale (covariate units) untouched, and it needs an
  event count the optimizer does not see. Dominated by B, which uses the
  information the stop rule needs anyway (D3).
- *A line search / step halving on Q_k.* Needs Q_k evaluations the
  score-only contract does not offer, and the EM accept test already
  certifies ascent.

#### Amendment (2026-10-16, task 1.3): B-full is the default

Evidence: the 63-cell bake-off (`.plan/sgd-scale-aware/results/bakeoff_table.csv`)
and a follow-up check of B-full at `sgd_tolerance` 0.05 and 0.1 on the toy
and the oracle (`.plan/sgd-scale-aware/tol05.R`). Per D1's order:

| Candidate | Outcome |
|---|---|
| A (adagrad 0.01), cap 100 and 500 | Out on 2 (oracle gap 0.085 / 0.064 SE) and 4 (oracle 2.5x / 4.9x). |
| ref constant | Out on 1 (both coupled B3 cells abort, as `today`). |
| B-mini | Survives 1-3; slow on toy_inter (805 s, 30 retries, pool 30 -> 3,006) and on `b3_unc_random` (235 s against 48 s). Its retries are iterate noise against the accept test; the pool it grows to is the pool a converged EM needs. |
| ref adam 0.01 | Passes all four, but was a reference, and carries D1's objection to a fixed step in theta units (its `b3_unc_mcmc` gate reads 0.149). Not admitted post hoc. |
| **B-full** | Survives; oracle gap 0.003 SE at tolerance 0.1 and 0.0009 at 0.05; 2 M-step iterations per pass; all B3/B4 cells survive. |

**Decision.** The default is `step_schedule = "fisher_scoring"`,
`step_size = 1`, **`variant = "full"`** for every schedule. Both variants
stay selectable under every schedule, `fisher_scoring` included
(`variant = "minibatch"` keeps the cheap per-step cost, at the price of
iterate noise that can cost EM retries until the augmentation accounts for the
unobserved flips). The earlier preference for B-mini rested on a per-step cost
independent of the pool size. Under D3 the information and the first gradient
come from harvests the loop already holds, and the full variant's last
evaluation serves the pass's `at_proposal` evaluation, so a converged
full-variant M-step of n iterations costs (n - 1) pool evaluations against
today's capped 100 x B batch evaluations plus one: the cost argument for
minibatch no longer holds (cost section).

**Re-readings of the criteria.** No candidate meets D1 as written on
toy_inter at one shared tolerance. The two readings below were adopted
**after** the bake-off outcome was known, so they are post hoc. They are
kept because each rests on a mechanism that the follow-up runs showed
independently of which candidate it favors, and both are written into the
later tasks (4.5, 4.6) as checks on the shipped default rather than left as
bake-off verdicts:

- *Criterion 3.* The fit's `at_optimum` threshold is decoupled from
  `sgd_tolerance` (D2 amendment): the bake-off ran both at 0.1, and the
  missing-information gap between the two (D2) makes a shared value
  unmeetable at f around 0.5. B-full's toy_inter reading, 0.115 at
  tolerance 0.1, falls to 0.051 at 0.05, under the 0.1 gate.
- *Criterion 4.* The reference default is not a converged reference: under
  today's defaults (1.1) toy_base and toy_inter both end `converged = FALSE`
  after 15 capped EM passes, on pools of 30, so the reference is cheap by
  never reaching the pool a converged EM needs. At tolerance 0.05 B-full's
  toy_inter fit takes 12 accepted EM passes (13 trace rows) and a pool of
  2,986 (471 s against 42.1 s in 1.1, 11x), of which the M-steps are 11 s.
  Criterion 4 is therefore gated where the reference converges, the oracle
  (1.1 s against 2.2 s, same machine), and on the M-step's own time
  everywhere. toy_base is recorded (2.8 s against 21.1 s, 0.13x: it passes
  under any reading); toy_inter is recorded, not gated.

*Revisit.* B-mini, a minibatch default, and the full variant's cost at large
pools are carried to `dynes-augmenter-driver-v2`, to be reopened once the
augmentation accounts for the unobserved flips.

#### Amendment (2026-10-23, design review): name, damping, iteration cap

**Name.** Candidate B ships as `step_schedule = "fisher_scoring"`. The
bake-off scripts and `progress.md` call it `"preconditioned"`; they are
records and keep that name. The step is Fisher scoring with the expected
complete-data information held at theta_k (a chord-Newton step). `"newton"`
was considered and rejected because `set_algorithm_newton()` already names
the observed-data Newton-Raphson estimator, and a user could read the
schedule as running that estimator inside each M-step.

**Damping.** With `step_size = 1` and `P` frozen at theta_k, the iteration
`theta <- theta + P^-1 g(theta)` contracts only while the curvature at the
iterate stays below `2 P`. A rate sub-model's information grows like
`exp(theta)`, so a pass that moves a few SE can overshoot and diverge, and
under the full variant every wasted iteration costs a whole-pool evaluation
before the accept test sees the result. So under `"fisher_scoring"` with
`variant = "full"`, whenever `d_t > d_{t-1}`, `step_size` is halved for the
rest of the M-step and the iteration continues from its current iterate. The
halving repeats at each rise, with no floor beyond the iteration cap. Its
count is returned and recorded (`mstep_damped`, D4). This is not the rejected
line search on Q_k: it reads only `d_t`, which the stop rule computes anyway,
so the score-only contract holds and D3's single information request stands.

- *Only `"fisher_scoring"`.* The first-order schedules keep their step rules
  unchanged (D5's reproduction claim), and adagrad's `d_t` can rise briefly
  while its accumulator settles without anything diverging.
- *Only the full variant.* Under minibatch, `d_t` reads the EMA and moves by
  about `sqrt(0.053 * f / B)` SE from batch noise alone, so a strict test
  would halve on noise. A minibatch iteration is cheap, and the accept test
  catches its failures.
- *Shrink, not backtrack (v1).* Backtracking to `theta_{t-1}` (its gradient
  is held, so no extra evaluation) and retaking the step at half size never
  leaves the M-step worse than its best point. Shrinking from the current
  iterate was chosen for v1 as the simpler rule. Q_k is concave on the
  DyNAM/REM/DyNAM-i log-linear models, so neither rule reaches a different
  maximum. Backtracking is carried to `dynes-augmenter-driver-v2`.

**Iteration cap.** An unset `max_iterations` resolves per schedule (D5): 20
under `"fisher_scoring"`, 100 under the rest. Under the full variant a capped
M-step costs `(cap - 1) x K + K`, and under `convergence = "iterations"`
every pass runs to the cap, so today's 100 would cost 100 x K per pass, about
500,000 sequence evaluations at K = 5,000. The bake-off measured B-full at
1-2 iterations per pass (10-13 on the first) and B-mini at 2-12, so 20 fits
both variants of the default schedule with headroom for damping. The
first-order schedules did not converge at 100 (adagrad not on the oracle
even at 500), so no cap that is cheap enough would fit them. They keep
today's 100, which also keeps the D5 reproduction of today's trajectory
without an extra argument.

### D2. The M-step stop rule is the Newton step in SE units

At every iteration the optimizer computes, from the tested gradient `g_t`
and `P` (D3),

```
  delta_t = P^-1 g_t                                Newton step on Q_k
  d_t     = max_j |delta_t[j]| / sqrt((P^-1)[j, j])  in SE_c units
```

and stops with `converged = TRUE` when `d_t < sgd_tolerance`. This applies to
every schedule, not only B: the stop rule is schedule-independent.

The test comes **before** the step: an iteration evaluates the gradient at
its iterate, tests `d_t` there, and steps only when the test fails, so a
converged M-step returns the iterate its last gradient was taken at. Today's
optimizer steps first and tests after. The bake-off ran the test-first order
(its harness, `.plan/sgd-scale-aware/lib.R`), so every number in the D1 and D2
amendments was produced under it. The order matters twice: an M-step whose
start already passes returns `theta_k` unchanged (Q = 0, below), and the
returned parameters were already evaluated over the whole pool, which D3
reuses for the pass's `at_proposal` evaluation.
`sgd_tolerance` is redefined in SE units, default **0.05** (amended by task
1.3; the draft default was D25's 0.1). The fit's `at_optimum` gate is a
separate fixed threshold of **0.1** (D25's v1 threshold, D4). The task 1.2
bake-off recorded the agreement between the M-step flag on the last accepted
pass and the fit's exact gate (D4): 3 of 24 converged-fit comparisons
disagreed (12.5%), over the 10% trigger. (The last accepted pass is the wrong
pass to compare; see the amendment.)

*Amendment (2026-10-16, task 1.3).* All three disagreements are flag TRUE with
a gate of 0.115 or more: `toy_base`/constant (0.090 against 0.158),
`b3_f33_unc_mcmc`/adam (0.099 against 0.149), `toy_inter`/B-full (0.083 on the
stop pass against 0.115; the 0.021 first quoted in the brief is the previous,
last accepted pass). The gate-to-flag ratio is 1.4-1.75 in each. In a scalar
model with `I_obs = (1 - f) I_c` it is `1 / sqrt(1 - f)`, which is 1.5 at the
fits' fraction of missing information (0.57 on toy_inter). So the gap is the
matrix gap, and a tighter M-step tolerance closes it: B-full at 0.05 ends
toy_inter at `newton_se` 0.051.

The tolerance does more than set the flag. The EM stop rule reads Q: an
M-step that does not move gives Q = ASE = 0, a stop hit. At the default
`stop_count = 1L` that stops the EM. Under `stop_count > 1` it is a
`"stop-hit"` that is not accepted, so the loop grows the pool and retries
from the same `theta_k` until the streak completes, which is the
confirmation on a larger pool that the user asked for. At 0.1, toy_inter's
M-step on pass 3 declared convergence at `d_t` 0.083 without stepping, and the
EM stopped there at `newton_se` 0.115. So `sgd_tolerance` sets the EM's
stopping precision as well, and must sit well inside the Monte Carlo error.

Hence the split. `sgd_tolerance` (0.05) is the M-step's precision. The gate
`at_optimum = newton_se < 0.1` (D4) stays at D25's threshold, a documented
internal constant, not an option, because a converged fit at `newton_se`
0.051 must not read as away from its optimum when the M-step precision is
0.05. Exposing the gate threshold is carried to `dynes-augmenter-driver-v2`.

**The assumption behind the split.** A fit that ends on the EM stop rule
returns `theta_k` of its stop pass, whose M-step started at `theta_k` on the
final pool. Its starting statistic `d_0` is the complete-data counterpart of
the fit's gate at the same point, and in the scalar model the gate reads
`d_0 / sqrt(1 - f)`. An M-step that passes at its start does not move, so the
EM stops (under `stop_count > 1`, stop-hits and confirms on a grown pool) at a
`d_0` below `sgd_tolerance`, and the gate is then met by
construction while

```
  sgd_tolerance / sqrt(1 - f) < 0.1     i.e.  f < 0.75 at sgd_tolerance 0.05
```

So the split assumes a fraction of missing information below 0.75. A fit
that stopped on the EM rule but reads `at_optimum = FALSE` is a fit with more
missing information than that (or one whose matrix gap exceeds the scalar
one), which is what the warning alert (D4) reports. The bake-off's measured
gate-to-flag ratios (1.4-1.75) sit inside the bound (2 at f = 0.75).

The right flag to compare with the gate is therefore `d_0` on the **stop
pass**, recorded as `mstep_start_newton_se` (D4), not the final `d_t` of the
last accepted pass: the last accepted pass's M-step started at a different
point, and its final `d_t` is taken at the proposal, not at the returned
estimate. The amendment's toy_inter reading (0.083 against 0.115) is already
the stop-pass `d_0`. Task 4.6 tallies the agreement this way, over fits that
ended on the stop rule (a capped EM has no pass whose M-step started at the
returned estimate).

Why `P = I_c` and not `I_obs`: the M-step's objective is Q_k with the
weights fixed, whose Hessian is exactly `-I_c`. `I_obs` subtracts the
missing information, which is the EM's object, not the M-step's. At an EM
fixed point (theta_hat = theta_k) the two gradients coincide, and only the
matrix differs. The fit-level flag (D4) uses `I_obs`, which is what D25
gates on.

Under `convergence = "iterations"`, nothing is tested, but `d_t` is still
recorded when `P` is available (D3). `sgd_tolerance` is not consulted in that
mode (the fit's `at_optimum` reads the fixed gate of D4, not this value).
Supplying it there does not warn, and the roxygen says it is ignored in that
mode: abmcem's "supplying it warns" clause was never implemented and is
dropped from the delta. An options bundle is built the same way whatever its
mode, and a constructor warning on a harmless combination is noise.

*Alternatives considered:*

- *Keep a gradient-norm test with a rescaled tolerance* (divided by N, or
  by the pool's score SD). It is still not in parameter units, and per-
  coordinate scale is still ignored.
- *A step-norm test.* Rejected by `abmcem` D6 for decaying schedules. In SE
  units it would still fire on a stalled adagrad.
- *Gate on I_obs inside the M-step.* That needs the per-sequence score
  spread at every iteration, which is a full-pool evaluation per step.

### D3. The information and the first score come from the harvests the loop already holds at theta_k

`optimize_abem_sgd()` gains a required injected `information_fn(theta)`
beside `score_fn`. The loop builds it per pass like `score_fn`. It returns a
list:

- `information`, the aggregated `p x p` matrix: `sum_i w_i J_i` under
  importance, `mean over the multiset` under resampling, degenerate sequences
  excluded as `score_fn` excludes them;
- `scores`, the `K x p` per-sequence score matrix of the **pool** at `theta`
  (non-finite entries set to zero, as `score_fn` does).

The optimizer calls it **once**, at `theta_k`, at the start of the M-step, and
holds `P` and its factorization for every iteration. Under `variant = "full"`
the first iteration's gradient is built from `scores` and the weights, with no
further evaluation: `colSums(w * scores)` under importance, and under
resampling `colMeans(scores[resample_indices, , drop = FALSE])`, a row lookup
by the multiset's indices (duplicates repeat their row, and the weight of
every member is 1/m). Under `variant = "minibatch"` the cached scores are not
used: the batches are random draws, and the iteration is unchanged.

**The closure evaluates nothing.** Every per-sequence quantity it needs at
`theta_k` has already been computed, because the evaluator always computes
loglik, score and Fisher together whatever `what` filters (cost section):

- *Pool entry.* Every sequence enters the pool (initial build, refresh,
  ESS-guard redraw, top-up, growth) through `draw_into_pool()`, which
  evaluates it at `theta_k` for its log-likelihood (`em_loop_dynes.R`, the
  `what = "loglik"` call). That evaluation keeps the whole harvest.
- *Acceptance.* On an accepted pass `theta_k` becomes the proposal, and the
  pass's `at_proposal` evaluation is the harvest of the whole pool at exactly
  that point. The loop already carries its log-likelihood forward this way
  (`run$loglik <- loglik_proposal`).

So the loop holds, beside `run$loglik` and maintained at the same two places,
the per-sequence score and Fisher at `theta_k`, aligned with the pool
(appended on top-up and growth, replaced on a fresh build or redraw, replaced
by `at_proposal`'s on acceptance). A retry keeps `theta_k`, so its held
harvest stays valid and only the new draws join it, at their entry
evaluation. `information_fn` aggregates from that harvest with the current
weights. Its results are bit-identical to a fresh evaluation, since the
same stored statistics are evaluated at the same `theta`. A held or reused
harvest is the evaluator's whole return, the per-sequence `degenerate` flags
included (`abem_louis_se()` reads them from `at_proposal`), and the tests
compare the whole return with `expect_identical()`.

**The proposal evaluation reuses the M-step's last.** Under the full
variant every later iteration evaluates the whole pool once at its iterate,
each member once (under resampling the multiset's gradient is the row lookup
above, never one evaluation per duplicate, which costs the same K and leaves
the gradient bit-identical), and the loop's evaluator keeps the last such
harvest with its `theta`. Because the test comes before the step (D2), a
converged full-variant M-step returns exactly that `theta`, and the pass's
`at_proposal` is that harvest. An M-step that passes at its start returns
`theta_k`, and `at_proposal` is the held harvest. Otherwise (the iteration
cap, `convergence = "iterations"`, every minibatch M-step that moved) the
returned parameters were never evaluated, and `at_proposal` evaluates the
pool as today. The reuse is decided by identity of `theta` (`identical()`),
never by tolerance.

So a converged full-variant M-step of n iterations costs (n - 1) pool
evaluations for the whole pass, `at_proposal` included, and one that passes at
its start costs none.

Under a `fixed` mask the columns of `scores` and the rows and columns of
`information` follow the free block, as D3's factorization does.

`P` is factored once (Cholesky). If that fails (a direction with no
information, the `recip` choice at low flips, D7 of the study), `P^-1` is
the Moore-Penrose pseudo-inverse from an eigendecomposition with relative
tolerance `sqrt(.Machine$double.eps)`. Null-space directions then take no
Fisher-scoring step and are excluded from `d_t`, and the M-step result says
so (`rank_deficient = TRUE`, recorded in the trace).

`optimize_abem_sgd()` still accepts a `fixed` mask (the DyNES path leaves it
empty). With a mask, `P`, its factorization, the step and `d_t` use the free
block `P[free, free]`, never the full inverse with zeroed gradient entries,
which would leak the fixed coordinates' cross terms into the free steps.

The score-only rule of `abmcem` is amended. Minibatch batches still request
scores only. Fisher is now **retained** where consumed: at pool entry and
from `at_proposal`, for the information, and from the full variant's
whole-pool evaluations, for the pass's `at_proposal`. Since the evaluator
computes all three regardless, retention costs memory and, under a parallel
map seam, the transfer of a `p x p` matrix per sequence back from the
workers, but no extra evaluation.

*Alternatives considered:*

- *One fresh full-pool evaluation at theta_k per M-step* (the earlier draft
  of this decision). Simple, but it pays K sequence evaluations for numbers
  the loop already holds. An earlier rejection of the reuse said that
  `at_proposal` "is evaluated at the proposal, not at theta_k": that is wrong,
  since an accepted pass sets `theta_k` to the proposal, and the pool's
  later top-up or growth is evaluated at `theta_k` on entry anyway.
- *Cache by sequence id across passes in the pool store.* It would serve
  any `theta` seen before, but only `theta_k` and the last iterate are ever
  asked for again, and the store is shared with the workers. Holding the
  harvest beside `run$loglik` is enough.
- *Recompute `P` at every iteration.* That is a full Newton iteration at
  full-pool cost per step. It is reserved for B-full, if measured worthwhile.

### D4. Reporting: trace columns, one classed warning, a fit-level gate

- **Trace.** The loop keeps the optimizer's whole result. `em_trace` gains
  `mstep_iterations` (integer), `mstep_converged` (logical),
  `mstep_start_newton_se` (double, `d_0` at `theta_k`, the counterpart of the
  fit's gate on a stop pass, D2), `mstep_newton_se` (double, the final `d_t`;
  `NA` when not computed), `mstep_rank_deficient` (logical) and
  `mstep_damped` (integer, the step halvings of D1's damping; 0 when none or
  outside `"fisher_scoring"` with the full variant).
- **Warning.** The per-pass warning without a class is removed. The loop
  warns **once per run**, at the first pass whose M-step ended unconverged
  under `convergence = "gradient"`, with class
  `goldfish_abem_mstep_unconverged`, naming the pass, `d_t` against
  `sgd_tolerance`, the pass's step halvings when non-zero, and the lever
  (`max_iterations`, `step_schedule`).
  Estimation continues: the EM accept test still certifies ascent.
- **Fit.** `new_dynes_fit()` computes, from the covariance pass it already
  runs, `newton_se = max_j |(I_obs^-1 S)[j]| / SE[j]` over the free
  coefficients. That is D25's gate, exact at the returned estimate on the
  final pool. It also sets `at_optimum = newton_se < 0.1`, D25's threshold,
  held as one documented internal constant and independent of
  `sgd_tolerance` and of the M-step's `convergence` mode (D2 amendment). Both
  are `NA` when `covariance_status != "ok"`. `glance()` gains both columns. The
  print/summary status line states it, via cli, beside the `converged` line,
  as a warning alert when `at_optimum` is FALSE. Under `use = "resampling"`
  the alert adds an `i` bullet saying that the gate reads the
  importance-weighted pool, not the multiset the M-step optimized, so that
  the user does not read resampling noise as an M-step failure.

`converged` keeps its meaning (the EM stop rule). The proposal's success
criterion, "reports converged exactly when the Newton step is under the
threshold", is read against `at_optimum`, which meets it by construction.
The M-step flag is the approximate, per-pass counterpart: on the stop pass,
`mstep_start_newton_se < sgd_tolerance` (D2). Group 1 measured how well the
two agree, and task 4.6 measures it on the shipped default.

That counterpart holds under importance weighting only. Under
`use = "resampling"` the M-step maximizes the multiset mean while the gate
reads the importance-weighted pool (the fit already warns
`goldfish_dynes_se_importance`), so the two target different objectives and
the agreement tally is taken over importance-weighted fits. The gate is
still reported under resampling, as it reads (not `NA`, which would hide a
real diagnostic, and not against the multiset, which would give `newton_se`
two meanings).

*Alternative considered:* overloading `converged` with the gate. Rejected:
the EM stop rule and the optimum gate fail for different reasons (a capped
EM at the optimum, a stopped EM off it), and the study tabulates both.

### D5. `set_sgd_options()` resolves `step_size` and `max_iterations` per schedule

`step_size` and `max_iterations` default to `NULL`, resolved by schedule
(the cap by the D1 2026-10-23 amendment):

| `step_schedule` | `step_size` | `max_iterations` |
|---|---|---|
| `fisher_scoring` | 1 | 20 |
| `adagrad`, `adam` | 0.01 | 100 |
| `constant`, `decay`, `momentum` | 0.001 | 100 |

`step_schedule`'s default becomes D1's winner, `"fisher_scoring"`, and
`variant`'s default becomes `"full"` for every schedule (D1 amendment). Both
variants remain selectable under every schedule, so no resolution per
schedule is needed for `variant`. Selecting `step_schedule = "constant"`
alone therefore reproduces today's step size exactly; adding
`variant = "minibatch"` restores today's step rule too (the stop rule is the
SE-unit one, D2). An explicit
`step_size` or `max_iterations` is used verbatim under any schedule. The
roxygen states the resolution table, that the first-order schedules usually
reach their cap (raise `max_iterations` for them), that `"fisher_scoring"` is
a Fisher-scoring step with the information held at the M-step's start and not
`set_algorithm_newton()`, that `sgd_tolerance` is in standard-error units (and
ignored under `convergence = "iterations"`), and the
trade of `variant = "minibatch"` (cheap steps, noisy iterate that can cost EM
retries). DyNES is experimental and unreleased, so no lifecycle
deprecation applies.

The adagrad/adam values (0.01) come from the study (B4: 0.1 overshoots,
0.01 does not). Today's value for all of them is 0.001, which under-steps
like the constant default.

## Storage and per-event cost

No preprocessed output, buffer, walk, read or per-event code is added or
changed. The likelihood's per-event loops (the r backend's contribution
loop, the C++ kernels) are untouched. The change alters **how often** the
pool is evaluated, and at what granularity:

| Per M-step | Today | After |
|---|---|---|
| Information and first score at theta_k | none | served from held harvests (D3): no evaluation |
| Full-pool evaluations | 0 | under `variant = "full"`: one per iteration after the first; the last serves `at_proposal` when the M-step converged |
| Batch score evaluations | `max_iterations` (cap hit: 100 x B) | under `"minibatch"`: iterations to `d_t < sgd_tolerance`, each B sequences |
| `at_proposal` evaluation | 1 per pass | 0 when the M-step converged under `"full"` or passed at its start; else 1 |
| Dense algebra | none | one `p x p` factorization, one `p`-vector solve per iteration |
| Allocation per iteration | a `p`-vector gradient | the same, plus a `p`-vector step |
| Held across a pass | `run$loglik` | plus the per-sequence score (`K x p`) and Fisher (K matrices `p x p`) at theta_k, and the last whole-pool harvest |

With p <= 20 the algebra is negligible beside one pool evaluation.

**What "score-only" saves.** Nothing per sequence. `what` is honored only at
the harvest level: `evaluate_pool_sequence()` always runs the engine and
computes loglik, score and Fisher, and returns the requested ones. "Score-only"
and "Fisher only where consumed" are request contracts, so a whole-pool
evaluation over K sequences costs K sequence evaluations, the same as K / B
minibatch iterations (10 at K = 100, 500 at K = 5000 with B = 10). It is
also why the held harvests of D3 are free to keep: the pool-entry and
`at_proposal` evaluations already compute them.

**Cost prediction (stated before implementation, amended by task 1.3 for the
full default and the harvest reuse of D3).** In sequence evaluations per
pass, whole pass (M-step plus `at_proposal`), with B = 10. The pool-entry
evaluations of new draws are the same before and after and are left out.
n is the M-step's iteration count: the bake-off shows n = 1-2 after the first
pass under the full variant (10-13 on the first) and 2-12 under minibatch.

| Pool K | Today (100 x B + K) | Full, converged ((n - 1) x K), n = 2-3 | Full, passes at start (n = 1) | Minibatch (n x B + K) |
|---|---|---|---|---|
| 100 | 1,100 | 100-200 | 0 | 120-220 |
| 1,000 | 2,000 | 1,000-2,000 | 0 | 1,020-1,120 |
| 5,000 | 6,000 | 5,000-10,000 | 0 | 5,020-5,120 |

A capped full-variant M-step costs (cap - 1) x K plus an `at_proposal` of K,
as an unreused evaluation would: 20 x K under the `"fisher_scoring"` cap of
20 (D5), and 100 x K for a first-order schedule under the full variant.

On the toy and the oracle, where today every M-step hits the cap and the
pools are small, the pass falls well below today's. Fit time there is
expected within 0.7x-1.5x of the 1.1 values where the reference converges
(the oracle), and recorded on the toy (D1 amendment). On grown pools the
full default's pass is about 0.5x-1x at K = 1,000 and 0.8x-1.7x at K = 5,000,
and the minibatch variant's about 0.55x and 0.85x. The 1.5x bound of D1's
criterion 4 is still stated for the toy and the oracle only, and the
grown-pool ratio is recorded, not gated (task 4.5), for both variants. In
the measured fits the M-step is a small share of wall time (2-5% on
toy_inter, where augmentation and `at_proposal` over retries dominate), so
the wall-time ratio on a grown pool is closer to 1 than the evaluation ratio.

Memory: the held harvest at `theta_k` and the last whole-pool harvest
coexist with `at_proposal`'s during a pass, about twice `at_proposal`'s
footprint today: `K x (p + p^2)` doubles each, about 12 MB at K = 11,000 and
p = 11 (C4's largest pool). Peak memory on the toy and the oracle is
unchanged within noise. `walk_cost.csv` and `estimation_cost.csv` unmoved:
neither has a DyNES row, and no walk or likelihood code changes.

## Risks / Trade-offs

- [The bake-off picks on two small fixtures and a handful of study cells]
  → The criteria include the B3/B4 failure cells, which are exactly where
  the default broke, and the study compares its tuned setting with the new
  default after landing (task 5.3).
- [The M-step flag (I_c, Q_k) and the fit gate (I_obs, observed likelihood)
  measure different distances] → They coincide in gradient at an EM fixed
  point and differ by the fraction of missing information in the matrix.
  The bake-off recorded their agreement (12.5% disagreement, all from the
  matrix gap), and task 1.3 tightened the M-step default to 0.05 and fixed the
  gate at 0.1 (D2). The fit gate is exact regardless. The split assumes a
  fraction of missing information below 0.75 (D2); above it a fit can stop
  on the EM rule and read `at_optimum = FALSE`, which the fit reports.
- [The tolerance was tightened after the bake-off, so the B3/B4 cells
  (criterion 1) were measured at 0.1, not at the shipped 0.05] → A tighter
  M-step moves each pass's proposal closer to the pool optimum, which can
  only raise Q. Task 4.6 runs the B3/B4 cells under the shipped default and
  checks that none aborts (`goldfish_abem_retry_exhausted`).
- [The harvest reuse (D3) returns a stale quantity] → The held harvest is
  maintained at the same two places as `run$loglik`, and the proposal reuse
  is decided by `identical()` on `theta`. Tasks 2.2c and 2.3c compare a
  reusing run with a run that evaluates afresh, bit for bit, through
  retries, growth and a redraw.
- [A tight M-step tolerance lets the EM run longer and grow its pool] →
  Intended: at 0.1 the toy_inter EM stopped on a technicality at `newton_se`
  0.115, and at 0.05 it runs to a converged fit (12 accepted passes, 13 trace
  rows, pool 2,986, 471 s
  against 42 s for a default that never converged). The cost is the pool a
  converged EM needs at a fraction of missing information of 0.57, not the
  M-step; it is recorded in 4.5/4.6.
- [A frozen `P` far from theta_k misdirects B's late iterations, or makes
  them diverge when the curvature at the iterate exceeds `2 P`] → The M-step
  starts at theta_k, and a pass's movement is typically under a few SE.
  Under the full variant the step halves whenever `d_t` rises (D1
  2026-10-23 amendment), and the cap of 20 bounds the whole-pool evaluations
  a bad pass can spend. The EM accept test rejects an M-step that does not
  ascend. `mstep_damped` and the iteration count expose it if it happens.
- [The full variant's whole-pool iterations are paid on huge grown pools
  (C4: 1,000-11,000)] → With the reuse of D3 the information is free and
  the last iteration serves `at_proposal`, so a pass of n iterations pays
  (n - 1) x K against today's 100 x B + K: about 0.8x-1.7x at K = 5,000
  (cost table above). A pass that needs many iterations on a grown pool pays
  K for each. Task 4.5 measures it on the fixtures and on a grown pool, for
  both variants, and the study harness records it on the cells. If the
  grown-pool ratio is unacceptable, the lever is `variant = "minibatch"`
  (already selectable).
- [Every default DyNES fit moves] → Intended (BREAKING numerical, unreleased).
  Task 3.3 attributes every moved seeded expectation to this change alone,
  which is why the code waits for `focal-stream-dedup` 3.2.
- [Snapshots of `set_sgd_options()` print/validation and the DyNES fit
  print move] → Updated in the pairs that cause them, never in bulk.

## Migration Plan

Internal to an experimental, unreleased estimator. One `NEWS.d/` fragment
names the new default, the SE-unit `sgd_tolerance` and the new fit and
trace fields. Rollback is per commit. A user who wants today's trajectory
sets `set_sgd_options(step_schedule = "constant", variant = "minibatch",
sgd_tolerance = <raw>)`, but the tolerance is now in SE units, so the old raw value has no exact
equivalent. The fragment says so.

## Open Questions

- Answered by task 1.3 (2026-10-16): the winner is B-full, and `sgd_tolerance`
  defaults to 0.05 with a fixed 0.1 gate (D1, D2 amendments).
- Closed (2026-10-16): `sgd_tolerance` under `convergence = "iterations"` is
  ignored without a warning, and the roxygen says so (D2).
- Carried to `dynes-augmenter-driver-v2` (2026-10-16): whether the gate
  threshold (0.1) becomes an option, and whether B-mini or a minibatch
  default is revisited once the augmentation accounts for the unobserved
  flips.
- Carried to `dynes-augmenter-driver-v2` (2026-10-23): backtracking in place
  of the shrink-only damping (D1 2026-10-23 amendment).
- Whether the study re-runs any cell under the new default: decided in
  `dynes-augmentation` after task 5.3's comparison, not here.

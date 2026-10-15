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

### D1. The default step rule is chosen by a bake-off, with the preconditioned step as prior

The default is not fixed in this document. Group 1 of the tasks is a
measurement spike, with no package code, that runs the candidates below on
fixed fixtures and records the winner here (an amendment under this
heading) before any red/green pair is written.

**Candidates.**

| Id | Rule | Default `step_size` |
|---|---|---|
| A | `step_schedule = "adagrad"` (the study's incumbent, 2034 cluster fits) | 0.01 |
| B | `step_schedule = "preconditioned"` (new): `step = step_size * P^-1 g_t`, `P = I_c(theta_k)` from D3, `g_t` the tested gradient (exact under `"full"`, the bias-corrected EMA under `"minibatch"`) | 1 |
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
a design review.

**The user keeps the choice.** Whichever rule wins becomes the
`set_sgd_options()` default. `constant`, `decay`, `adagrad`, `adam` and
`momentum` stay selectable. If B wins, `"preconditioned"` is added to the
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

### D2. The M-step stop rule is the Newton step in SE units

At every iteration the optimizer computes, from the tested gradient `g_t`
and `P` (D3),

```
  delta_t = P^-1 g_t                                Newton step on Q_k
  d_t     = max_j |delta_t[j]| / sqrt((P^-1)[j, j])  in SE_c units
```

and stops with `converged = TRUE` when `d_t < sgd_tolerance`. This applies to
every schedule, not only B: the stop rule is schedule-independent.
`sgd_tolerance` is redefined in SE units, default **0.1** (D25's v1
threshold). The task 1.2 bake-off records the agreement between the M-step flag on
the last accepted pass and the fit's exact gate (D4). If they disagree on
more than 10% of the bake-off fits, task 1.3 tightens the default and
records the value here.

Why `P = I_c` and not `I_obs`: the M-step's objective is Q_k with the
weights fixed, whose Hessian is exactly `-I_c`. `I_obs` subtracts the
missing information, which is the EM's object, not the M-step's. At an EM
fixed point (theta_hat = theta_k) the two gradients coincide, and only the
matrix differs. The fit-level flag (D4) uses `I_obs`, which is what D25
gates on.

Under `convergence = "iterations"`, nothing is tested, but `d_t` is still
recorded when `P` is available (D3). `sgd_tolerance` keeps its value in that
mode, because the fit's `at_optimum` (D4) reads it. Supplying it there does
not warn: abmcem's "supplying it warns" clause was never implemented and is
dropped from the delta, since the value is consumed.

*Alternatives considered:*

- *Keep a gradient-norm test with a rescaled tolerance* (divided by N, or
  by the pool's score SD). It is still not in parameter units, and per-
  coordinate scale is still ignored.
- *A step-norm test.* Rejected by `abmcem` D6 for decaying schedules. In SE
  units it would still fire on a stalled adagrad.
- *Gate on I_obs inside the M-step.* That needs the per-sequence score
  spread at every iteration, which is a full-pool evaluation per step.

### D3. The information comes from one full-pool Fisher evaluation per M-step

`optimize_abem_sgd()` gains a required injected `information_fn(theta)`
beside `score_fn`. The loop builds it per pass like `score_fn`. It evaluates
`what = "fisher"` over the M-step's pool at `theta` and returns the
aggregated `p x p` matrix: `sum_i w_i J_i` under importance,
`mean over the multiset` under resampling, degenerate sequences excluded as
`score_fn` excludes them. The optimizer calls it **once**, at `theta_k`, at
the start of the M-step, and holds `P` and its factorization for every
iteration. Under `variant = "full"` that evaluation is an extra pass over
the pool, but it is not merged with the first score evaluation, which keeps
the evaluator contract simple.

`P` is factored once (Cholesky). If that fails (a direction with no
information, the `recip` choice at low flips, D7 of the study), `P^-1` is
the Moore-Penrose pseudo-inverse from an eigendecomposition with relative
tolerance `sqrt(.Machine$double.eps)`. Null-space directions then take no
preconditioned step and are excluded from `d_t`, and the M-step result says
so (`rank_deficient = TRUE`, recorded in the trace).

`optimize_abem_sgd()` still accepts a `fixed` mask (the DyNES path leaves it
empty). With a mask, `P`, its factorization, the step and `d_t` use the free
block `P[free, free]`, never the full inverse with zeroed gradient entries,
which would leak the fixed coordinates' cross terms into the free steps.

The score-only rule of `abmcem` is amended: "Fisher only where consumed"
now includes this one evaluation, because the stop rule consumes it.

*Alternatives considered:*

- *Reuse the previous pass's `at_proposal` Fisher.* It is evaluated at the
  proposal, not at theta_k, and over the pool before the top-up, growth or
  redraw. Saving one evaluation is not worth a preconditioner whose pool
  differs from the objective's.
- *Recompute `P` at every iteration.* That is a full Newton iteration at
  full-pool cost per step. It is reserved for B-full, if measured worthwhile.

### D4. Reporting: trace columns, one classed warning, a fit-level gate

- **Trace.** The loop keeps the optimizer's whole result. `em_trace` gains
  `mstep_iterations` (integer), `mstep_converged` (logical),
  `mstep_newton_se` (double, the final `d_t`; `NA` when not computed) and
  `mstep_rank_deficient` (logical).
- **Warning.** The per-pass warning without a class is removed. The loop
  warns **once per run**, at the first pass whose M-step ended unconverged
  under `convergence = "gradient"`, with class
  `goldfish_abem_mstep_unconverged`, naming the pass, `d_t` against
  `sgd_tolerance`, and the lever (`max_iterations`, `step_schedule`).
  Estimation continues: the EM accept test still certifies ascent.
- **Fit.** `new_dynes_fit()` computes, from the covariance pass it already
  runs, `newton_se = max_j |(I_obs^-1 S)[j]| / SE[j]` over the free
  coefficients. That is D25's gate, exact at the returned estimate on the
  final pool. It also sets `at_optimum = newton_se < sgd_tolerance`. Both are
  `NA` when `covariance_status != "ok"`. `glance()` gains both columns. The
  print/summary status line states it, via cli, beside the `converged` line,
  as a warning alert when `at_optimum` is FALSE.

`converged` keeps its meaning (the EM stop rule). The proposal's success
criterion, "reports converged exactly when the Newton step is under the
threshold", is read against `at_optimum`, which meets it by construction.
The M-step flag is the approximate, per-pass counterpart. Group 1 measures
how well the two agree.

That counterpart holds under importance weighting only. Under
`use = "resampling"` the M-step maximizes the multiset mean while the gate
reads the importance-weighted pool (the fit already warns
`goldfish_dynes_se_importance`), so the two target different objectives and
the agreement tally is taken over importance-weighted fits.

*Alternative considered:* overloading `converged` with the gate. Rejected:
the EM stop rule and the optimum gate fail for different reasons (a capped
EM at the optimum, a stopped EM off it), and the study tabulates both.

### D5. `set_sgd_options()` resolves `step_size` per schedule

`step_size` defaults to `NULL`, resolved by schedule: `constant`, `decay` and
`momentum` 0.001, `adagrad` and `adam` 0.01, `preconditioned` 1 (if it
ships). `step_schedule`'s default becomes D1's winner. Selecting
`step_schedule = "constant"` alone therefore reproduces today's default
step exactly. An explicit `step_size` is used verbatim under any schedule.
The roxygen states the resolution table and that `sgd_tolerance` is in
standard-error units. DyNES is experimental and unreleased, so no lifecycle
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
| Full-pool Fisher evaluations | 0 | 1 (D3), at theta_k |
| Batch score evaluations | `max_iterations` (cap hit: 100 x B) | iterations to `d_t < sgd_tolerance` |
| Dense algebra | none | one `p x p` factorization, one `p`-vector solve per iteration |
| Allocation per iteration | a `p`-vector gradient | the same, plus a `p`-vector step |

With p <= 20 the algebra is negligible beside one pool evaluation.

**What "score-only" saves.** Nothing per sequence. `what` is honored only at
the harvest level: `evaluate_pool_sequence()` always runs the engine and
computes loglik, score and Fisher, and returns the requested ones. "Score-only"
and "Fisher only where consumed" are request contracts, so one information
evaluation over K sequences costs K sequence evaluations, the same as K / B
minibatch iterations (10 at K = 100, 500 at K = 5000 with B = 10).

**Cost prediction (stated before implementation).** In sequence evaluations
per pass, with B = 10:

| Pool K | M-step today (100 x B) | M-step after (10-40 x B + K) | `at_proposal` |
|---|---|---|---|
| 100 | 1,000 | 200-500 | 100 |
| 1,000 | 1,000 | 1,100-1,400 | 1,000 |
| 5,000 | 1,000 | 5,100-5,400 | 5,000 |

So the M-step falls from the cap to 10-40 iterations on the toy and the
oracle, where today every M-step hits the cap, and fit time there is expected
within 0.7x-1.5x of the 1.1 values. On grown pools the information
evaluation dominates: the pass is about 1x at K = 1,000 and about 1.7x at
K = 5,000, so the 1.5x bound of D1's criterion 4 is stated for the toy and
the oracle only, and the grown-pool ratio is recorded, not gated (task 4.5).
Peak memory unchanged within noise (one `p x p` matrix). `walk_cost.csv` and
`estimation_cost.csv` unmoved: neither has a DyNES row, and no walk or
likelihood code changes.

## Risks / Trade-offs

- [The bake-off picks on two small fixtures and a handful of study cells]
  → The criteria include the B3/B4 failure cells, which are exactly where
  the default broke, and the study compares its tuned setting with the new
  default after landing (task 5.3).
- [The M-step flag (I_c, Q_k) and the fit gate (I_obs, observed likelihood)
  measure different distances] → They coincide in gradient at an EM fixed
  point and differ by the fraction of missing information in the matrix.
  The bake-off records their agreement, and task 1.3 may tighten the
  M-step's default. The fit gate is exact regardless.
- [A frozen `P` far from theta_k misdirects B's late iterations] → The
  M-step starts at theta_k, and a pass's movement is typically under a few
  SE. The EM accept test rejects an M-step that does not ascend. B-full's
  measured iteration count exposes it if it happens.
- [The extra Fisher evaluation is paid on huge grown pools (C4: 1,000-11,000)]
  → It is one evaluation per pass, against the at_proposal one the pass
  already pays, but it replaces a capped M-step of only 100 x B sequence
  evaluations, so on a pool of several thousand the pass is about 1.7x
  (cost table above). Task 4.5 measures it on the fixtures and on a grown
  pool, and the study harness records it on the cells. If the grown-pool
  ratio is unacceptable, the lever is a fixed-size subsample for `P`.
- [Every default DyNES fit moves] → Intended (BREAKING numerical, unreleased).
  Task 3.3 attributes every moved seeded expectation to this change alone,
  which is why the code waits for `focal-stream-dedup` 3.2.
- [Snapshots of `set_sgd_options()` print/validation and the DyNES fit
  print move] → Updated in the pairs that cause them, never in bulk.

## Migration Plan

Internal to an experimental, unreleased estimator. One `NEWS.d/` fragment
names the new default, the SE-unit `sgd_tolerance` and the new fit and
trace fields. Rollback is per commit. A user who wants today's trajectory
sets `set_sgd_options(step_schedule = "constant", sgd_tolerance = <raw>)`,
but the tolerance is now in SE units, so the old raw value has no exact
equivalent. The fragment says so.

## Open Questions

- The winner and its value (D1), and whether the 0.1 default of
  `sgd_tolerance` stands (D2): answered by task 1.2, recorded by task 1.3.
- Whether the study re-runs any cell under the new default: decided in
  `dynes-augmentation` after task 5.3's comparison, not here.

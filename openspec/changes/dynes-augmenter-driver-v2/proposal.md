## Why

`dynes-augmenter-walk-alignment` (v1) keeps the simulation driver's step
contract unchanged and works around three costs and one approximation that only
a driver change or exact bounds remove:

1. **One wasted rate round per sim attempt.** The driver evaluates every rate
   fid (`rate_values()`) before it calls `clock()`. Even after v1 applies the
   forced prefix inside the clock step, each attempt pays one full round at the
   walk's opening state, before the interval it draws (once per draw on the
   uncoupled single walk, once per attempt when coupled).
2. **Two evaluation rounds per sim step.** `clock()` receives only the
   driver's per-fid rate totals. The augmenter re-reads the same rate vectors
   at the same state (v1's per-step cache makes that one read per fid, not
   zero). The forced tied siblings and the per-attempt opening round also pay a
   driver round that nothing reads. v1's step cost is about
   `sum_k n_k * F + sum_k n_k * F'`, where `F` is the number of rate fids and
   `F'` the distinct fids among a step's candidates. The first term is the
   driver's round, read only for its total.
3. **Composition snapped to waves.** `dynes-composition-to-waves` moves an
   involved actor's in-interval join to the interval start and its leave to
   the next wave, ignoring the real times. That changes the model. The data
   says when actors join and leave, so the events an actor takes part in
   should be bounded on the left by its join and on the right by its leave.

## What Changes

- **A zero-evaluation forced prefix.** An internal `prefix` argument on
  `simulate_replicate()` applies the observed events and earlier intervals'
  flips before the loop's first rate evaluation, with the anchored discipline
  (advance strictly before each stamp, then inject). No rate or choice is
  evaluated at a walk state earlier than the interval's start. This replaces
  v1's clock-step prefix and its "at most one round per attempt" allowance.
- **The clock receives the driver's rate vectors.** `set_simulation_steps()`'s
  clock gains an optional fourth argument carrying the per-fid rate vectors the
  driver already evaluated. Existing steps keep working. The augmenter's
  per-step cache seeds from it, so the step cost falls to about
  `sum_k n_k * F`.
- **True composition bounds.** The sim augmenter bounds each actor's flips and
  the selection of its relational events by the actor's real join (left) and
  leave (right) inside the interval. A flip's truncation anchor includes the
  sender's next leave, and an event is not drawn before its actors join.
  Selection weights stay positive wherever the target's density is, so no
  ordering the target allows has proposal density zero. The wave snapping of
  `dynes-composition-to-waves` and its warning are then removed.
- **Exact flip-versus-event competition (candidate).** Today an observed
  relational event competes with the pending flips by a selection weight (the
  sender's rate). The exact proposal lets the flips' own waiting times compete
  with the event's fixed stamp: if no flip arrives before it, the event comes
  next. Evaluate this against the selection-weight form once the bounds are in.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `sequence-augmentation`: the "no evaluation before the interval opens"
  requirement tightens from at most one round per attempt to zero; the
  composition-snapping requirement is replaced by true join/leave bounds.
- Simulation step contract (the capability that owns `set_simulation_steps()` /
  `simulate_replicate()`; identify it at design time): an optional rate-vector
  argument to the clock and an internal prefix.

## Impact

- **Per-event code — yes.** The driver's step loop and the sim augmenter's
  step. Removes evaluations: one round per attempt, one round per step. Adds
  no preprocessed output, buffer or walk. Reuses `walk_advance(inclusive =
  FALSE)`, `walk_inject()`, `walk_next_breakpoint()`, `rate_values()`, and the
  time-anchored driver's discipline (`R/simulate_anchored.R`).
- **R:** `R/simulate_driver.R` (shared with `process-simulation`, so
  coordinate), `R/simulate_steps*` (contract), `R/augment_seq_sim.R`,
  `R/estimate_dynes.R` (remove the snapping).
- **Model:** fitted coefficients change on data with in-interval composition
  changes of involved actors (back to the data's own times). NEWS.d fragment.
- **Sequencing:** after `dynes-augmenter-walk-alignment` and
  `dynes-composition-to-waves`; not a prerequisite of the
  `dynes-augmentation` 6.2 sweep.

## Carried in from `sgd-scale-aware-default` (open; decide at design time)

`sgd-scale-aware-default` (task 1.3, 2026-10-16) shipped
`step_schedule = "fisher_scoring"` with `variant = "full"` as the M-step
default, and left the questions below to be reopened once the augmentation
accounts for the unobserved flips. None is in scope until this change's design
admits it; if admitted, the capabilities `abmcem-optimization` and
`dynes-estimation` are modified too.

- **A minibatch M-step, or B-mini as the default.** Under the current
  augmentation, B-mini's noisy iterates failed the EM accept test repeatedly
  on toy_inter (805 s, 30 retries, pool 30 -> 3,006, against B-full's single
  converged run on the same pool), so the full variant won. With truer
  proposals the iterate noise may cost fewer retries. Re-run the bake-off's
  toy_inter and `b3_unc_random` cells
  (`.plan/sgd-scale-aware/bakeoff.R`) under both variants, and compare the
  grown-pool cost of each (the full variant pays one pool evaluation per
  iteration after the first; minibatch pays the `at_proposal` evaluation).
- **The `at_optimum` threshold as an option.** The fit's gate
  `at_optimum = newton_se < 0.1` is an internal constant, separate from
  `sgd_tolerance` (0.05). It is met by construction only while the fraction
  of missing information is below 0.75 (`sgd_tolerance / sqrt(1 - f) < 0.1`).
  Decide whether it becomes a user option (on which options constructor),
  and whether its default follows `sgd_tolerance` or stays fixed.
- **Backtracking in place of shrink-only damping** (carried 2026-10-23). The
  `"fisher_scoring"` schedule (renamed from `"preconditioned"`), under the
  full variant, halves `step_size` whenever the Newton-step statistic `d_t`
  rises, and continues from the current iterate. The alternative returns to
  the previous iterate (its gradient is held, so no extra evaluation) and
  retakes the step at half size, so the M-step never ends up worse than its
  best point. Q_k is concave, so the two rules share a maximum and differ
  only in cost and robustness. Decide on the `mstep_damped` counts recorded
  by the study harness.
- **The full variant's first pass on a pool already at K** (carried
  2026-10-25, measured in `sgd-scale-aware-default` task 4.5). From theta_0 on
  a pool of K, the first M-step ran 13 iterations, so its pass cost 12 x K:
  6x the old default's pass at K = 1,000 and 10x at K = 5,000 (7.6 s and
  29.6 s). A fit with the default `n_sequences` starts on a small pool and
  grows it, so its first pass is cheap; the case arises when the pool is
  built large at theta_0. Decide with the B-mini revisit whether that case
  needs a cheaper first pass (a minibatch first M-step, or a warm start).
- **The minibatch variant's start statistic** (carried 2026-10-25). Under
  `variant = "minibatch"`, `start_newton_se` (and the trace's
  `mstep_start_newton_se`) is the first batch's bias-corrected EMA
  statistic, not the exact `d_0`, although the information request's
  whole-pool scores give `d_0` at no extra cost. A lucky first batch can
  also pass the stop test and return `theta_k` unmoved. Outside the default;
  decide with the B-mini revisit whether the first test reads the request's
  exact gradient.

## Carried in from `dynes-augmentation` (open; decide at design time)

- **Super-linear step costs of the augmenters** (carried 2026-11-02, from the
  `dynes-augmentation` conformance report, note 8). Per-draw and per-sweep
  cost grows faster than the flips F: the recipe cursor's `heads()`/`take()`
  scan every chain (`recipe_cursor`, O(chains) per step), `sim_pair_receivers()`
  scans the heads per option (O(heads^2) per option round), the mcmc move
  increments list fields and re-splits with `unsplit(split())` (O(F) per move),
  and the sim step copies `times` per move. Measured at HEAD: the `sim` draw is
  linear in F (0.14 / 0.25 / 0.56 s at 30 / 60 / 120 flips, n = 30) and near
  flat in n (0.13 / 0.17 / 0.21 s, n = 30..240); the `mcmc` sweep is about
  F^1.6 (0.022 / 0.069 / 0.26 s at 30 / 60 / 120 flips, stub evaluator).
  `mcmc` also re-preprocesses every candidate (the full re-preprocess of D5;
  incremental patching is deferred): 76 min per pass at 140 flips and 51
  actors in the vignette. Trivial at the vignette's size; matters if larger
  panels appear. Decide whether to remove the scans and copies here, with the
  step guards (`test-augment_sim_cost.R`, `test-augment_mcmc_cost.R`) as the
  floor.

- **The two-state CTMC oracle for a state panel** (carried 2026-10-26,
  skipped in `dynes-augmentation` task 6.2b). The CTMC's closed-form
  transition probabilities count paths with excursions, which v1's
  minimal-flip estimand excludes, so in v1 the comparison is only a bias
  measurement, and the cluster study already measures that bias (FINDINGS
  D11: about -0.17 at f15, -0.5 at f33). Decide whether the check returns as
  an oracle once this change's augmentation represents more of the
  unobserved flips, or stays a bias measurement.

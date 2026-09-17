## Why

**A DyNAM rate's sender gate counts the sender's own self-dyad as an allowed
receiver, so a sender whose only allowed cell is itself stays at risk.** The
living `support-constraint` spec defines the gate as
`active[i] = active_1[i] & (rowSums(support[i, ]) > 0)`, and every
implementation follows it: the batch fold that builds the estimation risk set
(`fold_active_sender_support()` with `initial_receiver_count()`,
`apply_receiver_count_flips()`, `apply_receiver_presence_flips()`), the live
handle's `walk_risk_set()` and the simulation driver's
`simulation_fid_state()` through `sender_gate_from_mask()`, and the
validation in `model_estimate.R`. The choice and REM engines drop the
self-dyad structurally (`twomode_or_reflexive`), so the rate and the choice of
one process disagree about who can act. The package already applies the
intended rule in one place: the panel completion count
(`support_grid_at()` in `complete_generative_spec.R`) zeroes the diagonal of
a one-mode grid before reducing it with `assemble_model_mask(model =
"rate")`, as the `intercept-only-rate` spec requires ("self-loops
excluded"). The relational pin reads `avg_active_entity` from preprocessing
instead, so the two paths of one primitive count different risk sets today.

The defect is sharpest on flavored layers. A creation mask is `tie(L) == 0`,
and the diagonal of a tie is always 0, so every sender keeps one "allowed"
creation (to itself) forever. It surfaced on 2026-09-17 in
`process-simulation` 2.8: on `flavored_fixture_data()` with only `creation`
modeled and replay disabled, sender 1 at t = 73.7 already held a tie to every
other actor, the rate drew it, the choice had no receiver and returned `NA`,
and the run aborted in `merged_build_event_args()`. In estimation the same
sender contributes rate exposure for events it cannot produce. A user
constraint shows the same defect: on a one-mode DyNAM,
`support_constraint = ~ alter(x) > 0` gates in a sender who is the only actor
with `x > 0`.

The alternative of writing the exclusion into the constraint (an
irreflexivity atom AND-ed into every mask) is rejected. Vault ADR-0063 holds
that the zeroed diagonal is a property of the model, not of a value stored at
a broadcast kind, and that a reduction reads each node's value from a cell
whose other index is not that node. The gate is such a reduction. An atom
would also force every separable (ego / alter) mask to point kind, and it
would fix only the constraints someone remembered to rewrite.

## What Changes

- **The rate sender gate excludes the sender's self-dyad on a one-mode
  layer.** The gate becomes
  `active[i] = active_1[i] & (sum_j support[i, j] & active_2[j] & (j != i) > 0)`
  whenever the process's risk set excludes self-ties (one-mode, not
  reflexive), and stays as today on a two-mode layer. This is a spec-level
  change: estimation, preprocessing output and simulation all move together,
  so the simulator keeps reproducing the likelihood it is checked against.
- **The maintained counter subtracts the diagonal term per mask kind**,
  without building a grid: global `g * (P - a_i)`, ego `e_i * (P - a_i)`,
  alter `sum_j c_j a_j - c_i a_i`, point `rowSums(s & a) - s_ii a_i`. A
  point flip at `(i, i)` changes no count; an alter flip at `j` reaches every
  sender except `j`; a receiver crossing at `j` reaches every allowed sender
  except `j`.
- **One from-scratch reduction.** `sender_gate_from_mask()` takes the
  self-exclusion flag and applies the same rule, so the walk handle, the
  simulation driver, the preprocessing validation and the fold agree
  boolean-exactly. `assemble_model_mask()` keeps its contract; its one
  production caller already passes a diagonal-free grid.
- **Consequences that follow the gate**: `n_candidates` for a rate event,
  the forced-choice and never-at-risk warnings, `avg_active_entity` and so
  the intercept start value, and the right-censored exposure of a sender who
  has only itself left.
- **Not changed**: the constraint grammar and the stored mask values (the
  mask still answers "is this dyad allowed", diagonal included); the choice,
  REM and coordination risk sets, which already exclude self-dyads; the
  unconstrained rate path, whose sender gate is presence alone; two-mode
  layers.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `support-constraint`: "Mask assembly per model" (the gate formula gains
  the structural self-dyad exclusion, owned by the model, not the mask) and
  "One support mask per process, shared by its sub-models" (the counter and
  its from-scratch reduction scenario exclude the self-dyad; a sender-axis
  mask stays unallocated as a dyad object).

## Impact

- **Code**: `R/model_preprocess.R` (`fold_active_sender_support()`,
  `initial_receiver_count()`, `apply_receiver_count_flips()`,
  `apply_receiver_presence_flips()`), `R/support_mask.R`
  (`sender_gate_from_mask()`), their callers in
  `R/preprocess_joint.R`, `R/walk_handle.R`, `R/simulate_driver.R` and
  `R/model_estimate.R`, which pass the model's self-exclusion flag. No C++:
  the gate is folded into `active_sender` before the engines read it.
- **Estimates**: coefficients move only for a constrained or flavored
  one-mode DyNAM rate in which some present sender, at some event, has
  itself as its only allowed, present receiver. The frozen 1e-6 baselines
  carry no support constraint and no flavors, so they must stay PASS. A
  measurement task quantifies the movement on a fixture that reaches the
  case, before the fix lands.
- **Tests**: `tests/testthat/test-support_mask*.R` and the fold tests gain
  per-kind self-exclusion cases, the counter-versus-reduction equality is
  re-run with the flag, and a simulation regression test covers the 2.8
  abort.
- **Related changes**: `constraint-objects-on-shared-walk` builds oracle
  frames "with the same risk-set rule the process used"; its derived-mask
  fixture must use the corrected rule, so this change lands first.
  `process-simulation` is unaffected in its tasks and gains the regression
  test's coverage.
- **Branch**: implemented on `feature_simulation`, where the per-kind counter,
  the live walk gate and `simulate()` live; `develop` has an older gate and
  reaches this fix through the simulation merge (design, Migration Plan).

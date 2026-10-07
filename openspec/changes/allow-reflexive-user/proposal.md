---
depends-on: support-gate-self-exclusion
---

## Why

**Whether a model allows self-ties is a modelling choice the package makes for
the user, and the fact is re-derived four different ways.** `allowReflexive`
exists in `estimation_core.R` (32 occurrences, commented "additional parameter
for DyNAM-MM") and is `FALSE` on every path a user can reach, so an actor
cannot be modeled as acting on itself — a legitimate model for advice-seeking
within a team, self-citation, self-loops in message logs, or any layer whose
diagonal carries data.

The same fact — *does this model's risk set exclude the self-dyad* — is now
decided in four places, from two different sources:

```
 walk_risk_set()          !isTRUE(engine$model_spec$is_two_mode)
 simulation_fid_state()   !isTRUE(engine$model_spec$is_two_mode)
 validate_prep_support()  !isTRUE(prep$model_spec$is_two_mode)
 the rate-gate fold       identical(ctx$nodes, ctx$nodes2)
 interaction reads        engine$op_drop_diagonal = n_inter > 0 && identical(...)
```

Three read the resolved descriptor, one compares node-set names, and one folds
in an unrelated condition under the same name. None of them can answer the
reflexive question, because none of them has it. `support-gate-self-exclusion`
added `abort_if_diagonal_off_grid()` as a tripwire for a wrong flag; it is
partial insurance (a two-mode layer with equal node counts is square, so a
wrong flag passes it) and it exists only because the derivation is not shared.

The gate's flag is also re-derived per event: `sender_gate_from_mask()` takes
`drop_diagonal` on every call, once per fid per event in the live walk and once
per dependent event in the preprocessing checks, for a fact fixed when the unit
compiles.

## What Changes

- **`allow_reflexive` becomes a modelling argument**, `FALSE` by default, so
  today's behavior is unchanged unless it is set:
  - `make_specification(allow_reflexive = )` stores it on the specification and
    its model spec, beside `is_two_mode`, `nodes` and `nodes2`.
  - `set_preprocessing(allow_reflexive = NULL)` overrides it for one run. `NULL`
    means "use the specification's value"; a non-`NULL` value wins. This is the
    surface the one-formula entry to `estimate_dynam()` / `estimate_rem()` /
    `estimate_dynami()` reaches, which builds its specification internally, and
    it reaches `simulate(control_prep = )` and `compute_statistics()` the same
    way.
- **One derivation, resolved once.** A single internal predicate answers
  "does this model's risk set exclude the self-dyad" as
  `!(is_two_mode || allow_reflexive)`, reading the model spec and nothing else.
  It is evaluated once per compiled unit, stored on the unit / engine / walk
  handle / preprocessed object, and read from there by the rate-gate fold, the
  from-scratch gate, the walk risk set, the simulation driver, the
  preprocessing validation and the likelihood's diagonal rule. No consumer
  compares node-set names, and no consumer recomputes it per event.
- **`abort_if_diagonal_off_grid()` goes**, together with the per-call
  `drop_diagonal` argument it guards: with one derivation resolved once there
  is no second source to disagree with, and the guard never caught the class of
  bug it was written for.
- **Refusals where reflexivity cannot mean anything**: a two-mode layer (no
  self-dyad exists) and DyNAM `choice_coordination` (a self-pair is not a
  coordination pair) refuse `allow_reflexive = TRUE` at construction, naming
  why. The `control_prep` override refuses at the point the model becomes
  known, since the control object alone cannot see the layer.
- **A preprocessed object carries the resolved value**, and a call asking for a
  different one aborts naming the stored and the requested value: the baked
  risk set and the likelihood must agree, and re-preprocessing is the remedy.
- **Not changed**: the default (`FALSE` everywhere, so every current fit is
  byte-identical), the constraint grammar, the stored mask values, DyNAM-i's
  internal kernels, and the C++ backends' `allow_reflexive` plumbing, which
  already carries the flag and simply starts receiving a value that can be
  `TRUE` from a user path.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `model-specification`: "make_specification constructs a goldfishSpec object"
  gains `allow_reflexive`, its default, and the refusals.
- `preprocessing-controls`: "set_preprocessing() constructs the preprocessing
  control object" gains `allow_reflexive` and the precedence rule;
  "preprocessed= is the single supply argument on estimators" gains the
  mismatch abort.
- `model-spec-descriptor`: "No component re-derives behavior from model or
  sub_model" extends to the self-tie rule — resolved once at spec
  construction, carried, never recomputed per event and never re-derived from
  node-set names.
- `support-constraint`: "Mask assembly per model" and "One support mask per
  process, shared by its sub-models" — the gate excludes the self-dyad when
  the model excludes self-ties, which is no longer the same as "one-mode".
- `active-availability-stat`: "Structural self-tie exclusion is not folded into
  active_dyad" — the exclusion becomes conditional on the model, and stays out
  of `active_dyad` either way.

## Impact

- **Code**: `R/make_specification.R` and `R/model_spec.R` (the argument, the
  field, the predicate, the refusals), `R/set_opt.R` (`set_preprocessing()`),
  `R/model_estimate.R` (resolution, the `preprocessed =` mismatch check,
  `validate_prep_support()`, the `allowReflexive` wiring that today hard-codes
  `FALSE`), `R/preprocess_joint.R` (resolve once onto the unit / engine,
  `op_drop_diagonal`), `R/model_preprocess.R` and `R/support_mask.R` (read the
  resolved value, drop the per-call argument and the guard),
  `R/walk_handle.R`, `R/simulate_driver.R`, `R/cpp_interface.R` (pass the
  resolved value rather than `FALSE`).
- **Behavior**: unchanged at the default. With `allow_reflexive = TRUE` the
  diagonal enters the risk set, so rates, choice probabilities, `n_candidates`,
  the intercept's starting value and simulated draws all change — that is the
  point of the argument.
- **Baselines**: the frozen 1e-6 baselines carry no reflexive model and must
  stay PASS as byte-identical, which is the detector that the default path did
  not move.
- **Depends on** `support-gate-self-exclusion` (landed, unarchived): this
  change's `support-constraint` delta is written against that change's wording,
  so the placement pre-flight is expected to defer until it folds.

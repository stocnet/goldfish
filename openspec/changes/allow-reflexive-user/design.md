## Context

See `proposal.md` (Why). Five facts from the tree shape the approach.

1. **`allowReflexive` already exists, end to end, pinned to `FALSE`.** It is a
   parameter of the estimation kernels (`estimation_core.R`, 32 occurrences,
   commented "additional parameter for DyNAM-MM") and of the C++ interface
   (`cpp_interface.R`, `allow_reflexive = FALSE`). Its effect is computed as
   `correctReflexive <- !allowReflexive && risk_set_axis(spec) ==
   "receiver_given_sender"`, and the broadcast fan-out reads
   `ctx$is_two_mode || ctx$allowReflexive`. Nothing user-reachable ever sets it,
   so the plumbing exists and the switch is missing.
2. **The fact is now needed before estimation.** Since
   `support-gate-self-exclusion`, preprocessing bakes a diagonal-dependent risk
   set: the folded rate gate under a constraint, `avg_active_entity` (hence the
   intercept's starting value) and the rate event's `n_candidates`. Simulation
   needs it at walk time. So the value must be known at specification /
   preprocessing time, not only at estimation.
3. **Four derivations exist today**, listed in the proposal: three read
   `model_spec$is_two_mode`, one compares `identical(ctx$nodes, ctx$nodes2)`,
   and `engine$op_drop_diagonal` folds in `n_inter > 0` under the same name.
   None can express reflexivity.
4. **The flag is passed per call.** `sender_gate_from_mask(..., drop_diagonal)`
   is called once per fid per event on the live walk and once per dependent
   event in the preprocessing checks, for a fact fixed at compile time.
   `abort_if_diagonal_off_grid()` guards it and is partial: a two-mode layer
   with equal node counts is square, so a wrong flag passes.
5. **`support_constraint` is the precedent for a model property with two
   surfaces**: `make_specification(support_constraint =)` and an
   `estimate_*(support_constraint =)` argument documented as "Ignored when `x`
   is a `goldfishSpec` object". This change deliberately takes a different
   shape for the second surface (below, D2).

## Goals / Non-Goals

**Goals:**

- One predicate answers "does this model's risk set exclude the self-dyad",
  reading the model spec and nothing else.
- It is resolved once per compiled unit and read from the objects consumers
  already hold — never recomputed per event, never re-derived from node names.
- `allow_reflexive` is reachable from both the specification surface and the
  formula surface, and reaches simulation.
- The default path is byte-identical: frozen baselines PASS unchanged.

**Non-Goals:**

- DyNAM-i's internal kernels, which carry their own `allowReflexive` defaults
  (`getMultinomialProbabilities()` and its caller default to `TRUE`); they are
  read through the same resolved value only where the DyNAM-i path already
  reaches it, and their internal defaults are not re-litigated here.
- The C++ backends, which already take `allow_reflexive` and need no change
  beyond receiving a value that can now be `TRUE`.
- Reflexivity for `choice_coordination` and two-mode layers, which is refused.
- Any change to what a self-tie *means* for an effect (an effect reading the
  diagonal of its own layer is the effect's business, not the risk set's).

## Decisions

### D1 — `allow_reflexive` is a field of the model spec, resolved through one predicate

`make_specification(allow_reflexive = FALSE)` stores the flag on the
specification and on the `goldfishKind` model spec, beside `is_two_mode`,
`nodes` and `nodes2`. One internal predicate — `excludes_self_dyads(spec)`,
defined as `!(is_two_mode || allow_reflexive)` — is the only site that reads
those two fields together.

*Alternatives rejected.* Keeping the fact implicit in `is_two_mode` (cannot
express reflexivity); deriving it from the data (`data$info` knows the node
sets but not the modelling choice — ADR-0063 places the diagonal rule on the
model); a second field carrying the resolved boolean on the spec (two fields
that can disagree, where a predicate cannot).

### D2 — The second surface is `set_preprocessing()`, and it wins over the spec

`set_preprocessing(allow_reflexive = NULL)`: `NULL` means "use the
specification's value", and a non-`NULL` value overrides it for that run.

This is the one departure from the `support_constraint` precedent, which
*ignores* its argument when a specification is supplied. The reasons: the
control object is what the formula entry, `compute_statistics()` and
`simulate()` all already accept, so one argument covers every consumer;
"ignored when a spec is supplied" is a rule users report as surprising; and the
override makes a sensitivity check (fit the same specification both ways)
possible without rebuilding the specification.

The accepted cost is that `control_prep` gains its first option that changes
coefficients, and a precedence rule to document. The rule is one sentence and
is stated wherever both arguments are documented.

*Alternatives rejected:* an `estimate_*(allow_reflexive =)` argument matching
the precedent (does not reach `simulate()`, and adds an argument to three
estimators plus `compute_statistics()`); both surfaces at once (three places to
look, two rules).

### D3 — Resolution happens once, where the unit is compiled

```
 make_specification(allow_reflexive=)      set_preprocessing(allow_reflexive=)
             |                                          |
             v                                          v
      spec / model_spec  --------------------->  resolve at entry
                                                        |
                              excludes_self_dyads(spec) | once per unit
                                                        v
                    +---------------- unit$excludes_self ----------------+
                    |                     |                |             |
                    v                     v                v             v
            engine$excludes_self   prep$excludes_self   handle       likelihood
                    |                     |                |             |
              rate-gate fold      validation, n_cand   walk risk set   diag -Inf
              from-scratch gate                        simulation draw
```

The resolved boolean is stored on the compiled unit and copied onto the objects
each consumer already holds (engine, preprocessed object, walk handle). The
per-call `drop_diagonal` argument on `sender_gate_from_mask()` and on the fold's
counter helpers goes away: they read the resolved field of the object they are
given. `op_drop_diagonal` keeps its distinct meaning but is renamed so it no
longer reads as the same question (it is "one-mode AND this unit has
interactions").

*Alternative rejected:* keeping the per-call argument and deriving it at each
call site from the resolved field. It is the current shape, and it is what made
four derivations possible.

### D4 — `abort_if_diagonal_off_grid()` is removed, not kept as insurance

With one derivation there is no second source to disagree with, and the guard
was never able to catch the semantic error (a square two-mode grid passes it).
The invariant it asserted — a model excluding self-dyads has a square grid —
becomes a property of the predicate's inputs, checked once at specification
construction by the two-mode refusal.

*Alternative rejected:* keeping it as a cheap assert at the resolution point.
Defensible, but it would assert something the constructor already refuses.

### D5 — Refusals at construction, and at the first point the control's override is meaningful

`make_specification(allow_reflexive = TRUE)` refuses a two-mode dependent layer
and a `choice_coordination` sub-model, naming which and why. The control
override cannot see the model, so the same refusal runs where the control meets
the specification — at the estimator's / `compute_statistics()`'s /
`simulate()`'s entry — with the message naming `set_preprocessing()` as the
source. One refusal helper, two call sites, one wording.

### D6 — A preprocessed object carries the resolved value and a mismatch aborts

`preprocessed =` supplies a baked risk set. The resolved reflexivity is stored
on it, and a call resolving to a different value aborts naming the stored value,
the requested one, and re-preprocessing as the remedy. It joins the existing
format-version rejection as a precondition of reuse.

*Alternatives rejected:* the stored value silently winning (the call does not do
what it says); re-preprocessing silently (throws away what the user supplied to
save).

## Risks / Trade-offs

- [`control_prep` becomes the first control that moves coefficients] → The
  documentation says so plainly at both surfaces, and the preprocessed-object
  mismatch abort (D6) catches the case where the two disagree after the fact.
- [A reflexive model changes numbers users have on file] → It cannot, unless
  they set the flag: the default is `FALSE` on both surfaces, and the frozen
  baselines are the detector for that.
- [The likelihood path for reflexive models is untested territory] → The
  kernels have carried `allowReflexive` since before this change but only ever
  received `FALSE` from a user path. Tests must cover a reflexive fit against a
  hand-built oracle (a multinomial fit including the self-cell), not merely
  "the code runs".
- [DyNAM-i defaults to `TRUE` internally] → Out of scope, but the wiring must
  not silently flip a DyNAM-i path to `FALSE` while routing the new value; the
  DyNAM-i tests are the detector, and any intended change there is its own
  decision.
- [Depends on an unarchived predecessor] → `support-constraint`'s delta is
  written against `support-gate-self-exclusion`'s wording, so the placement
  pre-flight defers until that folds. Expected, per the branching rules.

## Migration Plan

- No deprecation: `allow_reflexive` is new on both surfaces and defaults to
  today's behavior.
- `NEWS.d/allow-reflexive-user--allow-reflexive.md` records the new argument,
  the precedence rule and the `preprocessed =` mismatch abort.
- Implemented on `feature_simulation`, like its predecessor, since the resolved
  value threads through the walk handle and the simulation driver, which exist
  only there. The trunk merge folds NEWS and archives both changes in
  dependency order.
- Rollback: reverting the change restores the pinned `FALSE`; specifications
  and preprocessed objects built with the field simply carry a field nothing
  reads.

## Open Questions

- Whether `simulate()` should report the resolved reflexivity in its print, the
  way it reports `times`. It changes no behavior and can be decided when the
  print is next touched (`printing-homogenization`).

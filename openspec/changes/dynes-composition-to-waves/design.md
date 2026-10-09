## Context

The sim augmenter (`R/augment_seq_sim.R`) places an interval's events one step
at a time. Each step selects among the remaining flips' (layer, flavor, sender)
pairs and the interval's next observed relational event, weighted by rates read
at the walk state after the previous event. A flip's waiting time is an
exponential censored at the walk's next breakpoint (a composition change is
one), or a truncated exponential up to the anchor (the next relational event,
or the wave end).

Composition reaches that state through the walk's presence buffers
(`presence1_changes` / `presence2_changes`, `R/walk_handle.R`). A change is in
force at its own stamp (`walk_advance_presence()` applies every change with
`time <= t`). In a joint specification the data is always `stocnet`
(`make_joint_specification()` requires it), so composition is the initial
`data$nodes$active` column plus the `data$changes` rows with `var == "active"`,
split per node side by `split_composition()` (`R/event_streams.R`).

The boundary conventions this design relies on:

- Intervals are right-closed: an observed event stamped at `w_k` belongs to
  interval `k` (`findInterval(..., left.open = TRUE)` in
  `sim_observed_events()`), and a wave snapshot includes the rows stamped at
  the wave (`panel_wave_snapshot()`).
- Flips land strictly inside `(w_{k-1}, w_k)`.
- A presence change at `t` is in force for an event at `t`.

What reads composition before the walk (established while exploring):

| Step | Reads presence |
|---|---|
| `diff_panel_waves()` / the recipe | no: node counts and modes only (`panel_layer_context()`) |
| Panel pin, `panel_wave_risk_set()` | no: `endpoint_entity_count()` counts every actor |
| Relational pin, `relational_window_risk_set()` | yes: `compute_statistics()` on `data`, `avg_active_entity` is presence-weighted |
| The walk (target and every proposal) | yes |

## Goals / Non-Goals

**Goals:**

- An actor involved in an interval is present for every event and flip drawn
  in it, so the sim draw neither strands nor gives a positive-target ordering
  zero proposal mass because of composition.
- The target, the completion's pins and every augmenter read one rewritten
  composition.
- Data with no involved in-interval composition change fits byte-identically,
  with no warning.

**Non-Goals:**

- The true bounds (an involved actor's events constrained to its real join
  and leave times). That is `dynes-augmenter-driver-v2`.
- Composition of uninvolved actors, of history before `w_0`, and of rows after
  the last wave.
- `simulate()` and `compute_lik_seq()`: they do not augment and keep the data's
  composition.
- The panel pin's composition-blind risk set (`endpoint_entity_count()`). It is
  unchanged by this rewrite and stays out of scope.

## Decisions

### D1. The rewrite runs before completion; involvement does not wait for the recipe

Order at the `estimate_dynes()` entry:

```
panel_wave_grid --> involvement --> rewrite composition --> complete_generative_spec --> compile_augmentation_recipe
```

Completion pins a missing rate on a relational layer from
`relational_window_risk_set()`, which reads presence. Rewriting after
completion would pin that rate on the original composition while the target
walks the rewritten one. A pinned rate is part of the target, so the target
would read two compositions.

Involvement needs only the interval's relational events (read from
`data$ties`) and its flips (`diff_panel_waves()` per modeled panel layer).
Neither depends on completion. The diffs do not read presence, so the diffs
taken before the rewrite equal the ones the recipe takes after it.

*Chosen:* diff twice, once for involvement and once inside
`compile_augmentation_recipe()`. It is theta-free, once per fit, and leaves the
recipe's signature alone. A test asserts that the two diffs agree.
*Alternative:* pass the precomputed diffs into `compile_augmentation_recipe()`.
That saves one diff per modeled layer per fit, at the cost of a second entry
path into the recipe. Revisit only if the diff shows up in a profile.
*Rejected:* rewrite between recipe and completion. This is correct only when no
relational rate is completed.

### D2. Involvement

An actor is involved in interval `k = (w_{k-1}, w_k]` when it is:

- the sender or receiver of an observed event of a **modeled relational**
  process with time in `(w_{k-1}, w_k]`. Exogenous relational layers are
  replayed, not selected, so they never read a weight. This uses the same
  filter as `sim_observed_events()`.
- the `from` or `to` node of a flip of interval `k` in any modeled panel
  layer's diff.

Involvement is recorded in the global node-id space. Each layer's local
`from` / `to` indices map through that layer's `side1` / `side2` in the mode
map, so a two-mode layer's receiver involvement lands on the second node set.

### D3. The rewrite is a presence path, not a move of individual rows

Moving each row on its own (a join to `w_{k-1}`, a leave to `w_k`) reorders
changes whenever an actor has more than one change in an interval. A
leave-then-rejoin sorts to join @ `w_{k-1}`, leave @ `w_k`, and leaves the actor
absent from `w_k` on. Two moved rows on one stamp fall back on row order.
Neither strands, so neither is caught.

Instead, the rule rebuilds each actor's presence path. Let `S(t)` be the actor's
real state (initial `active`, then every real change at or before `t`). The
rewritten path `P` is:

```
inside interval k, t in (w_{k-1}, w_k):
    involved in k        -->  P(t) = TRUE
    not involved in k    -->  P(t) = S(t)
at a wave w_k, k = 0..K:
    involved in k+1      -->  P(w_k) = TRUE      (k < K)
    otherwise            -->  P(w_k) = S(w_k)
before w_0 and after w_K:     P = S
```

The actor's rows inside `[w_0, w_K]` are then replaced by the minimal change
list that produces `P`: one row at each point where `P` changes value. Rows of
an actor whose `P` equals `S` everywhere are not touched, nor are the rows of
any other actor or any other variable.

What `P` guarantees:

1. An involved actor is present at every point where interval `k` reads its
   weight. That covers the state at `w_{k-1}` where the draw starts, the
   interior, and its own relational events, including one stamped at `w_k`
   unless the data itself has it leave at that stamp (see Risks).
2. An uninvolved interval keeps the actor's real changes inside it.
3. At each wave the actor is in its real state unless the next interval needs
   it present. The rewrite does not spread past the intervals that caused it.
4. The result depends only on times, not on row order.

This covers the v1 boundary rule. A **join** anywhere in `(w_{k-1}, w_k]`,
including exactly at `w_k`, of an actor involved in `k` takes effect at
`w_{k-1}`. A **leave** in `[w_{k-1}, w_k)` takes effect at `w_k`. That holds for
senders and receivers alike.

*Accepted side effect.* A join at `w_{k-1}` is in force for observed events
stamped exactly at `w_{k-1}`, which belong to interval `k-1`. The moved actor
therefore enters those events' receiver sets and their senders' rate
denominators one interval early. Only relational events can sit at a wave
stamp: panel flips have no real stamp and are drawn strictly inside the
interval, and the MCMC augmenter only permutes within an interval. So the
effect is confined to relational events at wave stamps. It is documented in
the warning and left to v2.

*Rejected:* per-row moves (above). *Rejected for v1:* abort when an involved
actor has more than one change in an interval. That is less code, but it fails
exactly on the leave-and-rejoin data where composition matters.

### D4. Where the edit lands, and the guard that keeps it there

The rewrite edits only the `var == "active"` rows of `data$changes`.
`data$nodes` (including the initial `active` column) is not edited: a needed
presence at `w_0` is written as a change at `w_0`. The rewritten `data` is
installed on the joint specification and on every process specification, the
same rebinding `bind_candidate_data()` performs.

Every augmenter builds its candidates from `spec$data`
(`new_augmenter_context()` / `emit_augmented_data()`), so the candidates carry
the rewritten `changes`. However, `bind_candidate_data()` compares only `info`
and `nodes` today. It replaces `spec$data` wholesale, so a candidate with
another composition is walked on that composition without any error. Besides
the augmenters, the exported `compute_lik_seq()` reaches it with a sequence the
user supplies, and the fit exposes the rewritten objects (`fit$spec`,
`fit$pool$sequences`).

The guard is extended to the **composition**, compared in a normalized form,
not to `changes` as a whole:

- **What is compared:** only the `var == "active"` rows of `changes`. Every
  other nodal attribute stream is left out, because the guard concerns
  composition, not attributes.
- **Normal form:** times coerced to numeric (`coerce_time()`), nodes mapped to
  global ids (the same `to_global_id()` mapping `split_stocnet_streams()`
  uses), values coerced to logical, and rows sorted by (time, node) with a
  stable order. A `NULL` `changes`, a zero-row one, and one with no `active`
  rows all normalize to the same empty composition.
- **Cost:** the specification's normal form is computed once per bound
  specification. The candidate's is O(composition rows) per call, negligible
  next to the walk the call guards. Nothing about it is per event.
- **On mismatch:** the guard aborts with the existing
  `goldfish_candidate_mismatch` class. The cli message names composition as
  the field that differs, says that `estimate_dynes()` may have snapped the
  composition to the panel waves, and points to the consistent pairings:
  `fit$spec` with the fit's sequences, or the authored specification with the
  original data.

Effect on `compute_lik_seq()`:

```
compute_lik_seq(fit$spec, fit$pool$sequences[[i]], theta)  both rewritten  -> passes
compute_lik_seq(spec,     original_data,           theta)  both original   -> passes
compute_lik_seq(fit$spec, original_data,           theta)  mixed           -> aborts, explained
compute_lik_seq(spec,     fit$pool$sequences[[i]], theta)  mixed           -> aborts, explained
```

The mixed calls abort only on data the rewrite changed. On any other data both
sides normalize to the same composition, and today's behavior is unchanged.

*Rejected:* `identical()` on all of `changes`. It aborts falsely on
representation alone (`NULL` versus zero rows, numeric versus POSIXct times,
label versus index nodes) and whenever a nodal attribute row differs.
*Rejected:* no guard. Internal candidates are consistent by construction, but
the mixed `compute_lik_seq()` calls would stay silently inconsistent with the
fit's target.

The augmenters cannot apply the rewrite themselves: the target and the
completion must read the same composition (D1).

### D5. One warning per fit

The warning is a `cli::cli_warn()` with class
`goldfish_composition_snapped_warning`, raised once, after the rewrite, only
when some actor's `P` differs from `S`. It shows one bullet per
(actor, interval) whose path changed, giving the actor label, the interval
`(w_{k-1}, w_k]`, and the real change times now ignored. Long lists are
truncated by cli with an "and n more" count. It also states:

- the model changed: the moved actor's rate enters the compensator over the
  whole interval;
- the real times inside those intervals are ignored;
- relational events stamped at a wave can see a moved actor one interval early
  (D3).

Snapshot-tested under a pinned cli context.

## Storage and per-event cost

This change adds no preprocessed output, no walk, no buffer and no per-event
code. The rewrite runs once per fit, before completion, over:

- the modeled relational rows of `data$ties` (one `findInterval()`);
- the flips of each modeled panel layer's diff (computed once more, D1);
- the `active` rows of `data$changes`, grouped by actor, with the `K + 1` wave
  times per actor that has any change.

Its cost is O(R + F + C + A x K) for R relational events, F flips, C
composition rows and A actors with a composition change. It allocates nothing
of size `n1 x n2`.

Cost prediction: on the walk-cost budget fixture (CollegeMsg, no composition
changes) the rewrite is a no-op and the walk's time, allocation and object size
are unchanged. On data with involved changes, the walk's composition buffers
change by at most two rows per (actor, involved interval). The augmenter's
breakpoints change by the same count, which is negligible against the
interval's event count. The fit-entry cost adds one diff per modeled panel
layer.

## Risks / Trade-offs

- **The model changes for moved actors** --> warned (D5). It is the declared v1
  approximation, superseded by `dynes-augmenter-driver-v2`.
- **Relational events stamped at a wave see an early join** --> warned and
  documented (D3). Absent on panel-only data.
- **A leave stamped exactly at `w_k` together with an observed event of that
  actor at `w_k`** --> the data already has the actor absent at its own event
  (a change is in force at its stamp). `P(w_k) = S(w_k)` keeps that, so the
  rewrite neither causes nor fixes it. Out of scope; the existing data checks
  govern it.
- **`compute_lik_seq()` calls that mix a fit's rewritten objects with the
  original ones now abort** (D4), where today they score silently on the
  candidate's composition --> only on data the rewrite changed. The abort
  message names the consistent pairings, and the NEWS.d fragment states the
  behavior change.
- **The guard (D4) could reject a legitimate candidate whose composition is
  encoded differently** --> the comparison is on the normal form (numeric time,
  global node ids, logical values, sorted, empty forms equal), not on the raw
  `changes`. Tests cover each encoding difference, and every augmenter's
  candidate passing the guard.

## Open Questions

None. The ordering (D1), the boundary rule (D3) and the multi-change rule (D3)
were settled while exploring.

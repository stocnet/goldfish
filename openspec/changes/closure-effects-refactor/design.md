## Context

See proposal.md for the motivation. What shapes the approach, read off the
tree at `c145954` (`feature_simulation`):

- The nine dyad closure members share one sink, `apply_two_path_update()`
  (`R/utils_effects_closure.R`), which subassigns into `res$cache`. That
  matrix is bound three times at the call (the closure argument, `res$cache`,
  `engine$stat_cache[[gid]]`), so R duplicates it on the write. This is the
  mechanism ADR-0059 diagnosed for the state matrix and fixed with an
  in-place C++ write behind an aliasing precondition; ADR-0061 then put every
  kind-shaped value of the walk (statistics, operands, atoms, mask) on one
  shared in-place core (`write_entries()` / `set_entries()`). The effect
  caches are the one kind-shaped value not yet on it.
- `merged_covariate_step()` (`R/preprocess_joint.R`) calls each update, then
  rebinds `engine$stat_cache[[gid]] <- effect_update$cache`, and copies a
  `last_update` attribute for `consecutive`. `build_walk_engine()` extracts
  `engine$stat_cache` from `ctx$stat_cache` and keeps both, a second
  reference. `initialize_cache_stat()` (`R/model_preprocess.R`) can return
  one object as both `cache` and `stat` from the `default` init.
- The walk evaluates every update twice on an undirected object (one call
  per orientation, both before the state write). A probe on a five-node
  undirected layer shows `trans` correct under that (each call emits one
  orientation), `degree` reading twice the degree (its init also adds the
  transpose of an already symmetric matrix), and `triangle` correct only
  because it recomputes from scratch.
- `four` forms two `n1 × n2` products per event plus `expand.grid()` and
  `aggregate()`; `triangle` forms two `n × n` products per event; `node_trans`
  is already neighborhood-based. `triangle` is unreleased (absent at v1.7.0).
- The REM wrappers pass `event_order = 0` to `trans`/`cycle`, so
  `consecutive` cannot match under REM.
- Detectors: the parity fixtures cover `trans` (pooled, sequential), `cycle`,
  `common_sender`, `node_trans`, `mixed_trans`, `mixed_cycle`; the two-mode
  frozen baseline and the parity test cover `four`;
  `test-consecutive_history.R` covers `consecutive`; `common_receiver` and
  the `mixed_common_*` have unit tests only; `triangle` and `degree` have
  none.
- Decision record: vault ADR-0102 (the closure family design). Registry
  design D14 (cache as a named list with declared fields), D15 (factories)
  and D18 (reserved argument vocabulary) are the contracts this change
  implements for one family; D18's reserved `combiner_fn` is retired in
  favor of `path_fn`.

## Goals / Non-Goals

**Goals:**
- Every closure member updates through one enumeration and one in-place
  sink, byte-identical on every existing fixture and baseline.
- The per-event closure branches on nothing resolved at construction
  (`history`, role, undirectedness, the reflexive rule, `weighted`).
- The cache contract the registry and recency-effects inherit is the one
  shipped here.

**Non-Goals:**
- Changing the walk's undirected contract for the whole package, or deciding
  which effects have no undirected meaning (`inertia`, `recip`): both belong
  to `effect-term-registry`.
- Maintaining adjacency lists as auxiliary state (section D9), porting the
  members to C++ (ADR-0060, decided in the registry change), or a
  non-additive aggregation across paths.
- Any statistic value moving where a baseline or parity golden exists;
  `degree` on undirected layers and `consecutive` under REM move because they
  are defects with no golden behind them.

## Decisions

### D1 — One enumeration: positions of the changed tie, embeddings from neighborhoods

The change of a closure count is the sum over the positions the changed tie
`a → b` can occupy in the path shape of the contributions of the embeddings
read from `in(a)`, `out(a)`, `in(b)`, `out(b)` (and, for `four`, one further
hop) in the pre-event state. The shape table:

```
shape          position                embeddings                              cells
two-path       first leg (a→b, b→l)    l ∈ out(b) \ X                          (a, l)
i→k→j          second leg (k→a, a→b)   k ∈ in(a)  \ X                          (k, b)
in-star        one in-tie of b         j ∈ in(b)  \ X                          (a, j), (j, a)
out-star       one out-tie of a        l ∈ out(a) \ X                          (b, l), (l, b)
three-path     i→k (i=a, k=b)          j ∈ in(b)\{a}, l ∈ out(j)\{b}           (a, l)
i→k←j→l        j→k (j=a, k=b)          i ∈ in(b)\{a}, l ∈ out(a)\{b}           (i, l)
               j→l (j=a, l=b)          k ∈ out(a)\{b}, i ∈ in(k)\{a}           (i, b)
```

`cycle` transposes the two-path cells; the `mixed_*` members read the second
leg on the other layer; `four`'s three rows are the three terms its product
update computes today, with the `i ≠ j`, `k ≠ l` corrections as the
exclusions. A node member is the same enumeration reduced onto the node in
its role (D5). *Rejected:* keeping per-member bodies and only fixing the
sink; the bodies are the duplication, and `four`'s products are the cost.

### D2 — The cache is a named list, `stat` written in place through the shared core

`cache = list(stat = <buffer>, ...)`, `stat` at the member's broadcast kind
(kind 0 matrix for dyad members, kind 2 vector for node members),
`consecutive` state as `last_sender`, `last_receiver`, `last_event_seq`
length-one fields. The sink writes `stat` through `write_entries()` /
`set_entries()`; the closure still returns `list(cache, changes)`, so the
walk's rebinding is a no-op and the effect contract does not move (the
reason ADR-0059 chose the in-place write over slice passing). Precondition,
made executable as a test: the buffer is materialized fresh at init and held
by one binding — `build_walk_engine()` drops `ctx$stat_cache` after
extraction, the `default` init returns distinct `cache` and `stat`, no init
returns the state matrix. Replicates own their engines through `walk_open()`.
*Alternative kept for measurement (D15 of the registry, open):* the buffers
in an environment keyed by `gid` on the engine, as the interaction operands
are, with the closure returning `changes` only. It changes the return
contract, so it is measured, not assumed.

### D3 — Construction-time factories; nothing resolved per event

`make_closure_update(shape, cells, legs, positions, undirected,
allow_reflexive, weighted, path_fn, transformer_fn)` returns the member's
update closure with the neighborhood readers, the position filter, the
role column and the exclusion set fixed. `history` resolves to a position
filter (pooled: all; sequential: the closing position on a creation, all on
a dissolution; consecutive: that position under the adjacency predicate on
the cache fields); the three `compute_update_two_path_*` functions become
three filters over one enumeration. `type`/`history` resolution stays where
`resolve_effect_args()` puts it today (the parser), so the factory receives
scalars. *Rejected:* branching on `history` and role inside the update, the
pattern the registry's D4/D15 exist to remove; measured against the factory
in task 6.2 so the maintenance choice carries a number.

### D4 — Neighborhood readers with an exclusion set carry the reflexive rule

`neigh_in(net, v, exclude)`, `neigh_out(net, v, exclude)`, one reader for
both on an undirected object. Today's `temp[sender] <- 0` and
`temp[c(sender, receiver)] <- 0` remove a self-loop used as a leg and a
diagonal cell as an emitted entry; with self-dyads excluded neither reaches
an estimate, but dropping them would change the update stream (diagonal
rows) and the node reductions. So the exclusion set is the resolved rule:
`X = {a, b}` when self-dyads are excluded (byte identity), `X = ∅` when
allowed. `four` keeps `i ≠ j`, `k ≠ l` under both, a property of its shape.
`allow_reflexive` is read from `excludes_self_dyads(spec)` once
`allow-reflexive-user` lands and from today's pinned `FALSE` before, at the
one site (`prepare_recipe_context()`) that builds the templates. The readers
are the seam `recency-effects` (ordered neighborhoods, `last_k`) plugs into.

### D5 — `role` is an argument of `node_trans()`, resolved to a reduction column

The transitive triangle `i → k → j`, `i → j` has three node positions; the
enumeration is one, the reduction differs by role (source: today's
`node_trans`; broker; sink). `role = c("source", "broker", "sink")` follows
registry D19 (enum as noun, default first) and is resolved at construction to
the column the factory reduces with. *Rejected:* three named effects, the
precedent recency-effects D2 takes for its scopes; there the scopes differ in
validity and reader, here the three roles share both, so an argument is the
smaller surface. `node_triangle` has one collapsed column and takes no role.

### D6 — Undirected: the walk keeps its two evaluations; this family evaluates one way

A kernel built for an undirected object (closures, `degree`,
`node_triangle`) runs its enumeration on the canonical orientation
(`sender < receiver`), emits the rows both mirrored calls emit today in the
same order, and returns no changes on the other orientation. The update
stream stays byte-identical, `degree` stops doubling, and when
`effect-term-registry` moves the walk to one call per undirected tie the
guard is deleted with the kernel already emitting the complete set.
*Rejected here:* changing the walk contract for every effect now (it needs
the registry's per-effect undirected validity, e.g. refusing `inertia` and
`recip` on an undirected object). The init of an undirected object reads the
stored symmetric matrix and never adds its transpose.

### D7 — `weighted` with `path_fn`; the aggregation across paths is a fixed sum

An embedding exists only when every leg is present and contributes
`path_fn(leg weights)`; the statistic is the sum over embeddings; a leg
moving from `w_old` to `w_new` changes each touched cell by the embedding's
new minus old contribution (zero when the embedding does not exist), read
from the other legs' weights. This keeps the cache a running sum and the
update touched-entries-only for `prod`, `min`, `sum`, `max` or a user
function. A non-additive aggregation across paths (a `max` over brokers)
is not maintainable from a scalar cache; it is computable by recomputing the
touched cell over its brokers, a different update rule left to a later
change. Name: `path_fn` over D18's reserved `combiner_fn` (says "per path,
from its legs") and over the issues' `form` (an enum name from ergm; D19
gives function-valued arguments the `_fn` suffix). `transformer_fn` keeps
acting on the sum, so ergm's geometric form is `path_fn = prod,
transformer_fn = sqrt`.

### D8 — `triangle` → `node_triangle`, and a new dyad `triangle`

`node_triangle` (rate): per node, the undirected triangles it belongs to;
delta form `cache[a] += m`, `cache[b] += m`, `cache[k] += 1` for `k` in the
common neighborhood `m`. The rename is outright: the effect shipped in no
release (absent at v1.7.0). `triangle` (choice, REM, coordination): the
undirected two-path closure, which `trans` already computes on an undirected
object, so it is the two-path shape with the undirected readers plus a guard
that aborts at construction on a directed network, as `trans` aborts on a
two-mode one.

### D9 — No auxiliary adjacency structure in R

A vectorized column scan of 1899 doubles costs microseconds, far below the
copy this change removes, and an R-level list update is itself a copy.
Adjacency lists (Graphs.jl's per-vertex in/out vectors) are the right
structure for the C++ port ADR-0060 defers to the registry, kept at the
state write, one pair per object read by a closure member (a windowed term
reads a derived object with its own state, so windows need nothing
special); igraph is not a fit (edge insertion rebuilds the object, a
dependency on the hottest loop, no per-event mutation API). The readers are
the abstraction, so adding lists later changes their body, not the
enumeration.

### D10 — Detector-first, member by member, baselines at every commit

Address-comparison audit and brute-force oracles land before any member
moves; the sink's in-place write lands alone (byte-identical, timed);
members are rewritten one commit each; `weighted`/`path_fn` last, since it
adds statistics no baseline covers. The frozen 1e-6 baselines, the parity
goldens and the two-mode baseline are the floor; the oracle tests are the
detector for the members those do not cover.

## Risks / Trade-offs

- [In-place writes make a silent class of bug possible] → the aliasing audit
  is an executable test (addresses compared) run on every member and on two
  simulation replicates, the same control ADR-0059 installed for the state
  matrix.
- [A delta form of `four` diverges from the product form on a corner (two-mode
  exclusions, diagonal)] → the product form stays as the oracle in the test
  helper until every fixture matches; the two-mode baseline and the parity
  test gate it.
- [The canonical-orientation guard changes an undirected update stream] → the
  guard reproduces the rows of both mirrored calls in today's order; asserted
  on the undirected parity fixtures.
- [`weighted` semantics differ from a reader's expectation (`sum` counting a
  lone tie)] → the "an embedding exists only when every leg is present" rule
  is a spec scenario and a roxygen sentence.
- [The factory buys less speed than expected] → measured and recorded (task
  6.2); the maintenance case stands on its own.
- [`allow-reflexive-user` lands after this change] → the factory takes the
  pinned `FALSE` today and is wired to the predicate when that change lands;
  its task 2.2 already notes the consumer.

## Migration Plan

Commit per task on `feature_simulation`, `NOT_CRAN=true` green with the six
frozen baseline files PASS at each; `NEWS.d/` fragments, no Version or
`NEWS.md` edit (ADR-0040). Order: audit and oracles → in-place sink →
factory and readers → two-path and star members → `mixed_*` → `four` →
node family and the undirected members → `weighted`/`path_fn` →
measurements → docs and close. `status: landed (feature_simulation, awaiting
fold)` at the end. Rollback is per commit; every step leaves the tree green.

## Open Questions

- Whether the environment-keyed buffer container (D2's alternative) is worth
  adopting after the measurement; it can be answered after the change lands
  without changing the specs.

## Why

Every closure effect keeps a running count of paths through the network in a
cache and updates it per event, and every one of them pays the same defect:
the update subassigns into a buffer the walk also holds, so the whole n1×n2
matrix is copied per update (about 8.6 GB allocated per 1000 CollegeMsg
events, about 88 GB at 10k; `trans` costs 378 µs per effect-event against 11
to 47 for other effects). Around that one sink, nine update bodies are one
body with an orientation table, `four` and `triangle` recompute with matrix
products that grow as n², the node-level closures are one role out of three,
and the family cannot express weighted paths (#104, #106). The fix is one
enumeration rule, one in-place cache, and construction-time factories, done
now so `process-simulation` replicates and every choice model with `trans`
stop paying the copy, and so `effect-term-registry` and `recency-effects`
inherit a family already in the shape they need.

## What Changes

- **One update rule for the family.** The change of a closure count at a cell
  or node is the sum, over the positions the changed tie can occupy in the
  path shape, of the embeddings read from the neighborhoods of its two
  endpoints in the current state. `trans`, `cycle`, `common_sender`,
  `common_receiver`, the four `mixed_*`, `four`, `node_trans` and the
  undirected members are rows of one table; `four` and `node_triangle` lose
  their per-event matrix products.
- **The cache is written in place.** Each effect's cache is a named list
  whose `stat` field is the untransformed count buffer at its broadcast kind,
  written through the shared kind-shaped core (`set_entries()`), with the
  `consecutive` history carried as list fields rather than a matrix
  attribute. Outputs are byte-identical; the frozen 1e-6 baselines and the
  parity goldens are the detector.
- **Construction-time factories.** `history`, the node role, undirectedness,
  the reflexive rule and the weight combination are resolved once when the
  term is built; the per-event closure branches on none of them.
- **`node_trans(role = c("source", "broker", "sink"))`**: the transitive
  triangle counted at the node in the given role (today's statistic is
  `"source"`, the default).
- **`triangle` is renamed `node_triangle`** (rate, undirected, per node;
  unreleased, so the rename is outright), and **a new dyad `triangle`** (choice,
  REM, coordination) counts the two-paths that close a triangle on an
  undirected network. **BREAKING** for development users of `triangle` in a
  rate formula.
- **`weighted = TRUE` with `path_fn`** on every closure member: a path's
  weight is `path_fn` of its legs (default `prod`; `min`, `sum`, `max` or a
  user function), the statistic is the sum over paths, and `transformer_fn`
  keeps acting on the sum. Unweighted, every member is today's count.
- **The reflexive rule replaces the ad-hoc diagonal exclusions**: the
  neighborhood readers exclude the tie's own endpoints when self-dyads are
  excluded and nothing when they are allowed (`allow_reflexive`), for `four`
  and every other member; `four`'s distinct-node constraints stay under both.
- **Undirected objects evaluate one way in this family**: a closure kernel or
  `degree`/`node_triangle` built for an undirected object runs its update on
  the canonical orientation only and emits the complete change set, so the
  walk's second (mirrored) call is a no-op. **Fixes `degree`**, which today
  reports twice the degree on an undirected layer (no test, no baseline).
- **Fixes `history = "consecutive"` under REM**, whose wrappers pass a
  constant event order so the branch never matched.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `closure-effects-dry`: the shared cache-update helper becomes the in-place
  delta sink every closure member (two-path, star, three-path, node) writes
  through; requirements added for the one enumeration rule, the list cache
  written in place, the node role, the undirected members and the one-way
  evaluation, weighted paths with `path_fn`, and the reflexive rule.
- `closure-effects-history-mixed`: `history` resolves at construction to a
  position filter over the one enumeration; `consecutive` fires under REM.

## Impact

- **Code**: `R/utils_effects_closure.R` (the sink, the readers, the factory),
  `R/functions_effects_DyNAM_choice.R` (the nine dyad members and `four`),
  `R/functions_effects_DyNAM_rate.R` (`node_trans`, `node_triangle`,
  `degree`), `R/functions_effects_REM.R` and
  `R/functions_effects_DyNAM_choice_coordination.R` (wrappers; the
  `event_order` fix), `R/preprocess_joint.R` (`build_walk_engine()` drops its
  second reference to the caches), `R/model_preprocess.R`
  (`initialize_cache_stat()` returns distinct `cache`/`stat` objects),
  `R/formula_parser.R` / `R/formula_validate.R` (the new arguments and the
  `triangle` names), `NAMESPACE` (S3 inits).
- **Tests**: unit tests per member against a brute-force oracle;
  `helper-parity-fixtures.R`, `test-preprocess_parity.R`,
  `test-twomode_baselines.R` (`four`), `test-consecutive_history.R` and the
  frozen baselines as byte-identity gates; new tests for `degree`,
  `node_triangle`, `triangle`, `role`, `weighted`/`path_fn`.
- **Users**: `triangle` in a rate formula must become `node_triangle`; new
  arguments `role`, `weighted`, `path_fn` on the family; `degree` on an
  undirected layer changes value (halves).
- **Docs**: the effects reference, `NEWS.d/` fragments (rename, new
  arguments, the two fixes); no `NEWS.md` or Version edit on the branch.
- **Sequencing**: after `rate-init-value-copy-free-updates` (landed); before
  `effect-term-registry` (which absorbs the factories as its first
  registry-shaped family and takes over the walk's undirected contract) and
  `recency-effects` (which reuses the cache list, the readers and the
  factory pattern). `allow_reflexive` is read from `excludes_self_dyads()`
  once `allow-reflexive-user` lands and from today's pinned `FALSE` before.
- **ADR**: ADR-0102 (the closure family design: one rule, in-place cache,
  factories, `path_fn`).

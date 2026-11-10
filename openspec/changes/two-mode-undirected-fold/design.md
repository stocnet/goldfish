## Context

A two-mode layer's `from` and `to` index different node sets. Whether a layer
is two-mode is decided once, in `build_mode_map()` (`R/mode_map.R`): it is
two-mode when its `info$sender` and `info$receiver` mode sets are disjoint.
When they are undeclared, the layer is one-mode over all nodes. On a two-mode
layer `directed = FALSE` has no meaning, but `validate_goldfish_data()` accepts
it (`check_layer_coverage()` checks only that `info$directed` is a named
logical covering every layer; `check_mode_sets()` never looks at `directed`).

`materialize_network_state()` mirrors `(i, j)` to `(j, i)` in local index
space whenever it is passed `directed = FALSE`. Whether a caller passes the
raw declaration or a mode-aware one decides whether a two-mode undirected
layer is corrupted. The callers split into two groups (audit, 2026-11-10):

```
reads the flag through ds_is_directed()            reads info$directed raw
(already ignores it on two-mode: option A)         (mirrors a two-mode layer)
---------------------------------------------      --------------------------------------
ds_is_directed.goldfishSourceStocnet               network_state_at()     R/state_at.R:65
  -> data_source.R:376 starting state              panel context          R/wave_diff.R:44
  -> preprocess_builders.R:735 is_undirected         -> panel_wave_snapshot(), dyad mask,
  -> preprocess_joint.R:362 focal is_undirected         flip enumeration (wave_diff.R:390, 418)
                                                   prepare_layer_shape()  R/prepare_dynes_data.R:726
legacy assembly (legacy_wrappers.R:178)              -> starting fold (:973), checks
  folds the upper triangle on square one-mode only ds_is_directed.goldfishSourceEnvir
                                                     (legacy make_network(nodes2 =, directed = FALSE))
```

So the DyNAM/REM estimation path on a stocnet object already reads a two-mode
undirected layer as directed. The corruption in probe P2
(`.plan/develop/dynes-prepare-data-evidence/probe-twomode-history.{R,log}`)
comes from the right-hand column: the DyNES panel helpers,
`prepare_dynes_data()` and `network_state_at()`. The legacy environment source
mirrors too, but no reader of a goldfish data object reaches it (D3). The
comment above `ds_is_directed()` (`R/data_source.R:420-421`) says "the
validator notes and ignores the declaration". The validator does neither: the
gate lives in the stocnet method only.

The living `single-data-object` spec already settled the rule. Its "Layer
sides via declared sender/receiver mode sets only" requirement ends with
"`directed` SHALL be ignored (with a validation note) on two-mode layers."
The note was never implemented in `validate_goldfish_data()`, and only the
stocnet `ds_is_directed()` method applied the "ignored" part. This change
keeps the rule, drops the note (D1), and applies the rule to every reader of a
goldfish data object (D2, D3).

### What goldfish does with `directed = FALSE`

In goldfish, "undirected" is a storage rule for a square matrix: i -> j and
j -> i are the same tie, so both cells must hold the same value. It is
enforced in two places:

1. **Starting state.** `materialize_network_state()` copies every cell
   `(i, j)` to `(j, i)`.
2. **Every event in the walk.** An object with `is_undirected` set runs its
   statistic update a second time with sender and receiver swapped
   (`R/preprocess_joint.R:1111`). The support-mask maintenance does the same
   (`R/support_mask_maintain.R:393`), and the legacy path writes
   `object[receiver, sender]` as well (`R/model_preprocess_group.R:534`).

Both are symmetrization: keeping the two stored copies of one tie in sync.

```
one-mode, n x n                     two-mode, n1 x n2 (senders x receivers)
     A  B  C                             W  X  Y  Z
  A  .  1  .   <- A-B stored twice    a  1  .  .  .
  B  1  .  .   <- mirror keeps sync   b  .  1  .  .
  C  .  .  .                          c  .  .  .  1
                                      the cell (a, W) IS the tie; there is
                                      no second copy. "(W, a)" is row W,
                                      which doesn't exist (out of bounds), or
                                      on a square layer is row 4 = some
                                      unrelated sender (the phantom E->C)
```

On a two-mode layer there is no second copy, so the mirror either writes
outside the matrix or writes a tie between an unrelated sender and receiver.

### What manynet emits (probe, manynet 2.2.3, 2026-11-10)

manynet treats a two-mode network as undirected by definition:
`is_directed.stocnet()` and `is_directed.igraph()` both return `FALSE` when
`is_twomode()` is true. `as_stocnet()` copies that into `info$directed`. A
3 x 4 incidence matrix, and a square 3 x 3 one with `twomode = TRUE`, both
come back with `info$directed = FALSE` (an unnamed scalar), `nodes$mode`
`"FALSE"`/`"TRUE"`, and global `from`/`to` ids. In manynet's vocabulary,
"two-mode undirected" means "a tie joins a mode-1 node to a mode-2 node". That
is the same graph goldfish reads when it treats the layer as directed from the
sender set to the receiver set: no information is lost, and nothing is
contradicted.

The two packages use the word for different things:

| | manynet `directed = FALSE` | goldfish `directed = FALSE` |
|---|---|---|
| What it describes | The relation: a tie has no orientation | The storage: mirror the matrix |
| On two-mode | Always true by definition (an edge joins a mode-1 node to a mode-2 node) | Means nothing |

The goldfish `n1 x n2` matrix already represents manynet's undirected
bipartite relation completely: one cell per tie, nothing to mirror. When
`ds_is_directed()` returns `TRUE` on a two-mode layer, it does not mean "this
relation is oriented". It means "do not symmetrize". goldfish's flag is named
after the relation but used as a storage instruction. The quantity every
reader actually needs is

```
symmetric = one_mode && !directed
```

and the four direct readers of `info$directed` are the ones that skip the
`one_mode &&` part.

The object as emitted does not pass `validate_goldfish_data()`: there is no
`ties$layer`, `directed` is unnamed, and there are no
`info$sender`/`info$receiver`. A user turning it into a goldfish object adds a
layer name and names the flag per layer, and the obvious way to name it keeps
the value: `directed = c(<layer> = FALSE)`. Undeclared sender/receiver leaves
the layer one-mode over all nodes, where `FALSE` is meaningful and harmless (a
symmetric n x n state, no phantom ties). Declaring the two sides is what turns
it into the broken case.

## Goals / Non-Goals

**Goals:**

- Every reader of a layer's direction agrees on one answer for a two-mode
  layer, so no fold ever mirrors a two-mode layer.
- That answer is decided in one place, not re-derived per caller.
- Every reader of a goldfish data object behaves the same on the same object:
  the stocnet data source and the walk, the DyNES panel helpers,
  `prepare_dynes_data()` and `network_state_at()`.
- The frozen 1e-6 baselines stay PASS.

**Non-Goals:**

- The legacy readers of `network.goldfish` objects (D3). They still mirror a
  legacy two-mode network declared `directed = FALSE`; a follow-up change
  owns them:
  - `ds_is_directed.goldfishSourceEnvir` (`R/data_source.R:425`), the legacy
    environment source;
  - the legacy DyNAMi preprocessing (`R/model_preprocess_group.R:496`), reached
    through `R/dynami_bridge.R`;
  - the legacy network `update` method (`R/methods_update.R:167`, `:179`).
- `print()` on a goldfish data object (`R/as_goldfish.R:124`). It echoes the
  declaration, so manynet's two-mode `FALSE` prints as "undirected", which is
  correct in manynet's terms. It decides no storage.
- Renaming the walk property `is_undirected` to `symmetric`. It already means
  "symmetric", and the rename would touch the hot-path files
  (`R/preprocess_joint.R`, `R/walk_handle.R`, `R/support_mask_maintain.R`,
  `R/preprocess_builders.R`) with no behavior change. Left to a follow-up.

- Inferring two-mode from `nodes$mode` when sender/receiver are undeclared. A
  manynet bipartite object read without them stays one-mode over all nodes,
  as `build_mode_map()` defines today.
- Changing manynet's convention or asking manynet to emit `directed = TRUE` on
  two-mode networks.
- Any change to `materialize_network_state()`'s one-mode undirected fold.

## Decisions

**D1. `directed = FALSE` on a two-mode layer is accepted and translated, not
rejected (confirmed by the user 2026-11-10).** `directed` describes the
relation. On a two-mode layer, manynet's `FALSE` is a correct description
that asks for no storage behavior goldfish could carry out, so goldfish
accepts it and does not symmetrize. This is what the estimation path does
today through `ds_is_directed.goldfishSourceStocnet`. The change makes every
other reader agree. The validator still accepts the flag on two-mode layers,
and the `info$directed` documentation states that it affects one-mode layers
only.

Alternatives considered:

- **B: reject at the validator** (abort, suggesting `directed = TRUE`).
  Rejected. It refuses an object whose `FALSE` is true in manynet's sense and
  harmless in goldfish's, refuses the object a manynet user most naturally
  produces, and breaks the one path that works today (a DyNAM fit on such an
  object currently succeeds) in order to fix the paths that don't.
- **C: translate and inform once** (a `cli_inform()` naming the layer).
  Rejected. The message would tell every manynet user, on every two-mode
  layer, that a correct declaration was "ignored". A notice that fires on
  correct input teaches users to skip notices.
- **Gate at each call site** (a one-mode check in front of every fold).
  Rejected in favor of D2: per-site gating is how four readers ended up
  unguarded while three were guarded.

**D2. One per-layer `symmetric` flag, resolved once in the mode map, read by
every fold and every walk-time mirror write.** `build_mode_map()` already
records one entry per layer (`is_two_mode`, `side1`, `side2`, `n1`, `n2`). It
gains one field:

```
symmetric = !is_two_mode && isFALSE(unname(info$directed[layer]))
```

A layer is symmetric only when it is one-mode and explicitly declared
`FALSE`. Today the readers disagree on an undeclared flag:
`prepare_layer_shape()` reads it as directed through `!isFALSE()`, while
`panel_layer_context()`, `network_state_at()` and
`ds_is_directed.goldfishSourceStocnet` read it as undirected through
`isTRUE()`. The resolver sides with "directed" (not symmetric), matching
`make_network()`'s `directed = TRUE` default and `prepare_layer_shape()`
(user decision 2026-11-10). Validated input always declares the flag, so the
difference only shows on unvalidated input (`new_data_source()` and
`prepare_dynes_data()` do not call the validator themselves), and the single
resolver removes it.

Each reader stops reading `info$directed` and reads the field instead:

| Reader | Today | With D2 |
|---|---|---|
| `network_state_at()` (`R/state_at.R:65`) | `isTRUE(info$directed[layer])` | `!lm$symmetric` passed as the fold's `directed` |
| `panel_layer_context()` (`R/wave_diff.R:44`), used by `panel_wave_snapshot()`, the dyad mask and flip enumeration | `isTRUE(info$directed[layer])` | context carries `symmetric` from the layer entry |
| `prepare_layer_shape()` (`R/prepare_dynes_data.R:726`), used by the starting fold (`:973`) and the double-report and canonical-key checks | `!isFALSE(info$directed[layer])` | shape carries `symmetric` from the layer entry |
| `ds_is_directed.goldfishSourceStocnet` (`R/data_source.R:431`), used by the starting state and both builders' `is_undirected` | its own two-mode check, then `info$directed` | returns `!symmetric` from the layer entry |

The stocnet method is shared by the DyNES walk and the DyNAM/REM paths, and
it is already correct. It is a goldfish-owned internal generic, so it is
refactored here to read the map, which leaves one resolver. Its results do not
change on any validated object, which a pin test written before the refactor
and the frozen baselines both check; on unvalidated input an undeclared
one-mode flag moves from undirected to directed, as above. A layer name with
no map entry (an unresolved focal, for which `ds_layer_map()` returns `NULL`)
reads as not symmetric, so the method returns `!isTRUE(<entry>$symmetric)`
rather than a zero-length logical.

The existing `one_mode && !directed` guards inside these readers (for
example `R/prepare_dynes_data.R:240`, `:976`, `:1157`, `R/wave_diff.R:390`)
become `symmetric` and stay equivalent on one-mode layers.
`materialize_network_state()` keeps its `directed` argument unchanged: each
caller passes `!symmetric`. The walk-side property `is_undirected` keeps its
name (see Non-Goals); it already means "symmetric", and the stocnet method
fills it from the map.

The validator is not changed. It keeps checking only that `info$directed` is
a named logical covering every layer.

Alternative considered: keep `directed` as the field name and give it the
effective value. Rejected because it repeats the conflation that caused the
bug: a field named `directed` that is `TRUE` on a manynet-undirected layer
invites the next reader to take `info$directed` instead.

**D3. Scope: the readers of a goldfish data object (user decision
2026-11-10).** The change exists to unblock DyNES (`dynes-prepare-data`
group 4), so it fixes the readers a goldfish data object reaches: the stocnet
data source and the walk (through `ds_is_directed.goldfishSourceStocnet`),
the DyNES panel helpers (`diff_panel_waves()`, reached from
`compile_augmentation_recipe()`), `prepare_dynes_data()`, and
`network_state_at()`. `network_state_at()` is not DyNES-specific, but it reads
the same object, and it is the natural way to check a DyNES panel's state; if
it were left broken, it would show a phantom tie the pipeline does not have.

Deferred to a follow-up change: the legacy readers of `network.goldfish`
objects (the environment data source, the DyNAMi preprocessing monolith, the
legacy `update` method; listed under Non-Goals). None is on the DyNES path,
and other models are not goals of this change. They keep mirroring a legacy
two-mode network declared `FALSE`, so the `make_network()` documentation is
not changed here; it gains the rule when the follow-up fixes those readers.

Alternative considered: also fix the legacy environment source here
(the former D3), returning "not symmetric" for a network with `is_two_mode`.
Deferred with the other legacy readers, because fixing one of three legacy
mirrors would leave the legacy path inconsistent with itself.

## Storage and per-event cost

None. The rule is resolved once per layer before any walk, and no per-event
code reads or allocates anything new. The fold already runs once per state
materialization; skipping its mirror on a two-mode layer removes work.
Predicted walk time and preprocessed object size on the large fixture:
unchanged.

## Risks / Trade-offs

- [A user who hand-builds a two-mode layer with `directed = FALSE` expecting
  some effect gets none, silently] -> there is no effect they could have
  meant: a two-mode matrix has no symmetric counterpart. The `info$directed`
  roxygen states that the flag affects one-mode layers only.
- [A user expects goldfish to infer two-mode from manynet's `nodes$mode`]
  -> out of scope (Non-Goals). Without declared sender/receiver the layer is
  one-mode over all nodes, where `FALSE` mirrors correctly over the full
  `n x n` matrix.
- [A raw reader is missed] -> a test that builds a square and a non-square
  two-mode undirected layer and checks every audited entry point
  (`network_state_at()`, `prepare_dynes_data()`, the panel snapshot, the data
  source starting state) against the `directed = TRUE` answer, plus a broad
  `directed` grep audit whose every hit is classified as fixed here, already
  correct, or out of scope (D3). `dynes-prepare-data` 4.3 reuses these
  fixtures and covers only what this change does not.
- [The legacy path still mirrors a two-mode `FALSE`] -> accepted for this
  change (D3); the hits are recorded in the audit and in Non-Goals for the
  follow-up.
- [Baselines] -> no frozen set reads a two-mode undirected layer (Q3).

## Open Questions

- **Q1. Confirm D1.** Answered 2026-11-10: confirmed, accept and translate.
  The delta modifies `single-data-object`.
- **Q2. manynet's stocnet constructor.** Answered (probe above, then 0.1):
  `as_stocnet()` emits `info$directed = FALSE` for every two-mode input, as an
  unnamed scalar, because manynet defines two-mode as undirected.
  `make_stocnet()` passes `info` through as given; `validate_stocnet()` does
  not check it. **Answered (task 0.1, 2026-11-10, manynet 2.2.3):** no manynet verb
  names `directed` per layer on its own. `add_info()` / `mutate_info()` store
  exactly what they are given, and a `directed` argument *replaces* the whole
  entry (it does not merge), so `FALSE` survives only because the caller
  passes it. `as_stocnet()` leaves an unnamed scalar `FALSE`, which
  `validate_goldfish_data()` rejects ("info$directed must be named by layer")
  along with the other per-layer fields it requires. The manynet test object
  in 2.4(a) therefore starts from `as_stocnet()` of a two-mode matrix, adds
  `label`, `layer` and `time` columns, and calls
  `add_info(directed = c(<layer> = FALSE), sender = ..., receiver = ...,
  update = ..., observation = "panel")`; the `FALSE` is named for the layer
  by that call. Verified: it validates and `build_mode_map()` gives
  `is_two_mode = TRUE`, `n1 = 2`, `n2 = 3`. Whatever it is, goldfish
  documents the interoperability rule ("on a two-mode layer, `FALSE` is read
  as not symmetric; nothing needs changing") in the `?goldfish_data` stocnet
  components section and in `vignettes/two-mode.Rmd` (tasks 3.1, 3.2).
- **Q3. Frozen baselines.** Answered: the coefficient baselines use the
  social-evolution and fisheries data (`helper-baselines.R`), both one-mode;
  `global_v1`'s generator declares no `nodes2` and no two-mode undirected
  layer. No baseline reads the affected case.
- **Q4. Fixtures and tests.** Answered: every `directed = FALSE` in the
  stub's candidate files (`test-state_at.R:120`,
  `test-support_gate_self_exclusion.R:605`, the legacy `make_network()`
  calls in `test-data_source.R`, `test-legacy_assembly.R`,
  `test-preprocess_builders.R`) is on a one-mode layer. Every two-mode
  fixture in `helper-stocnet-fixtures.R` declares `TRUE`. No test asserts the
  current mirror.
- **Q5. Legacy `make_network(nodes2 =, directed = FALSE)`.** Closed
  2026-11-10 as out of scope (D3): the legacy readers are deferred to a
  follow-up change, which also settles whether `nodes2` identical to `nodes`
  counts as two-mode (the user's leaning: yes, as the `is_two_mode` attribute
  says).
- **Q6. The `R/data_source.R:420-421` comment.** It claims validator
  behavior that does not exist ("the validator notes and ignores the
  declaration"). Under D1/D2 it is reworded to say where the rule lives: the
  mode map resolves `symmetric`, and a two-mode layer is never symmetric.

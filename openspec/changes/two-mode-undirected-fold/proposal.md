---
status: proposal (rule settled 2026-11-10; from the dynes-prepare-data conformance, 2026-11-05)
---

## Why

A two-mode layer's `from` and `to` index different node sets. In goldfish,
`directed = FALSE` is a storage instruction for a square matrix: keep `(i, j)`
and `(j, i)` equal. On an `n1 x n2` two-mode matrix there is no second copy to
keep in sync (`dynes-prepare-data` D13; `panel_dyad_mask()` folds one-mode
layers only). `validate_goldfish_data()` accepts `info$directed = FALSE` on a
two-mode layer, and it should: manynet defines every two-mode network as
undirected, and `as_stocnet()` emits `directed = FALSE` for all of them.

Some readers then hand that raw flag to `materialize_network_state()`, which
mirrors every cell `(i, j)` to `(j, i)` in **local** index space and pairs a
sender with an unrelated receiver:

- on a square layer (as many senders as receivers) the folded state holds
  ties nobody reported: history row G→A yields a phantom E→C;
- on a non-square layer the mirror indexes past the matrix and aborts with a
  raw `subscript out of bounds`.

The DyNAM/REM estimation path is not affected: it reads the flag through
`ds_is_directed.goldfishSourceStocnet`, which already treats a two-mode layer
as not symmetric. The readers that take `info$directed` directly are affected:
`network_state_at()` (`R/state_at.R:65`), the DyNES panel context
(`R/wave_diff.R:44`, feeding `panel_wave_snapshot()`), and
`prepare_dynes_data()`'s layer view (`R/prepare_dynes_data.R:726`, feeding its
starting fold at `:973`). The legacy readers of `network.goldfish` objects
mirror too, but no goldfish data object reaches them; they are deferred
(`design.md` D3).
Evidence: `.plan/develop/dynes-prepare-data-evidence/probe-twomode-history.{R,log}`
(square: a spurious dissolution row plus a style-contradiction warning on
`prepare_dynes_data()`'s own output; non-square: the crash).

The living `single-data-object` spec already says `directed` is ignored on
two-mode layers, "with a validation note". The note was never implemented, and
the rule was never applied to the readers above.

## What Changes

- **Accept and translate.** `directed = FALSE` on a two-mode layer stays valid
  input and means "not symmetric". The validator is not changed, and it emits
  no note: the declaration is correct in manynet's terms, so a note would fire
  on correct input.
- **One per-layer `symmetric` flag on the mode map.** `build_mode_map()`
  resolves `symmetric = one-mode AND directed declared FALSE` once per layer,
  next to `is_two_mode`.
- **Every reader of a goldfish data object uses it.** `network_state_at()`,
  the DyNES panel context, `prepare_dynes_data()`'s layer view, and the
  stocnet `ds_is_directed()` method read the map's flag instead of
  `info$directed`. `materialize_network_state()` keeps its signature; each
  caller passes `!symmetric`.
- **Scope is DyNES and goldfish data objects.** The legacy readers of
  `network.goldfish` objects (the environment data source, the DyNAMi
  preprocessing monolith, the legacy `update` method) are not on the DyNES
  path and are deferred to a follow-up change (`design.md` D3).
- **Documentation.** The `info$directed` docs (`?goldfish_data`) and the
  two-mode vignette state that the flag affects one-mode layers only, and
  that manynet's two-mode `FALSE` is read as not symmetric. The comment above
  `ds_is_directed()` stops claiming a validator note. `make_network()`'s docs
  are not changed, since the legacy path still mirrors.

Rejected: aborting in the validator on two-mode `FALSE` (refuses manynet
output and breaks the DyNAM path that works today), and accepting with a
notice (fires on correct input). See `design.md` D1.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `single-data-object`: the "Layer sides via declared sender/receiver mode
  sets only" requirement drops the validation note and states that a layer's
  symmetry is resolved once on the mode map. A new requirement fixes that every
  reader of a goldfish data object takes its fold and mirror decisions from
  that flag.

## Impact

- Code: `R/mode_map.R`, `R/state_at.R`, `R/wave_diff.R`,
  `R/prepare_dynes_data.R`, `R/data_source.R` (the stocnet method only);
  roxygen for `info$directed` in `R/goldfish-data.R`; `vignettes/two-mode.Rmd`.
  The validator (`R/validate_goldfish.R`) and the legacy readers are unchanged.
- No per-event cost: the flag is resolved once per layer before any walk.
- Gates all of `dynes-prepare-data` group 4 (user decision 2026-11-09): no
  session of it starts until this change lands. The mirror corrupts a
  two-mode undirected layer even without history rows: in probe P2, `G->A`
  reads back with a phantom `E->C` through `network_state_at()`. So
  `dynes-prepare-data` 4.3 tests square and non-square layers, with and
  without `time = NA` rows, under this rule, and `prepare_dynes_data()` gets
  no interim abort.
- No fixture, test or frozen baseline declares `directed = FALSE` on a
  two-mode layer (design Q3, Q4), so the frozen 1e-6 baselines stay PASS.

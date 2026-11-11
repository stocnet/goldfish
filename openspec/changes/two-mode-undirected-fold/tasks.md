Every implementation unit is an (a)/(b) pair. (a) writes the failing test,
confirms RED, and commits the test alone. (b) writes the minimal code, confirms
GREEN, and commits after `air format` + `lintr` on the touched files,
`devtools::document()` when roxygen changed, and the not-cran-test skill. Tests
follow r-lib:testing-r-packages. No new console output, no new or deprecated
API. No group changes per-event code: `symmetric` is resolved once per layer
before any walk, and the walk's `is_undirected` property keeps its value on
every one-mode layer (design, "Storage and per-event cost"), so no group opens
with a cost guard. `R/validate_goldfish.R` is not edited by any task (D1).
Feature branch: no DESCRIPTION bump, no `NEWS.md` edit, no archive. The
milestone is a `NEWS.d/` fragment. Code comments carry the reasoning (a
two-mode matrix has no symmetric counterpart), never a decision ID.

## 0. Close the remaining open question (gate, no code)

- [x] 0.1 Close design Q2's remaining part: check whether `manynet::add_info()`
      / `mutate_info()` name `directed` per layer when a layer is added, and
      whether a `FALSE` from `as_stocnet()` survives that step. Record the
      answer in design Q2. It decides the exact object the manynet scenario in
      2.4(a) builds, not the rule.

## 1. The `symmetric` flag on the mode map

- [x] 1.1(a) Test `build_mode_map()` layer entries (spec "Layer symmetry
      resolved once on the mode map"): `symmetric` is `TRUE` for a one-mode
      layer declared `FALSE`; `FALSE` for a one-mode layer declared `TRUE`;
      `FALSE` for a one-mode layer with `directed` undeclared; `FALSE` for a
      two-mode layer declared `FALSE` and declared `TRUE`; one object mixing a
      one-mode undirected and a two-mode undirected layer gets one value per
      layer. Confirm RED (the field does not exist).
- [x] 1.1(b) Add `symmetric` to each `build_mode_map()` entry
      (`R/mode_map.R`), resolved from `is_two_mode` and `info$directed`, and
      document it in the function's `@return`. GREEN.
- [x] 1.2 Pin the validator's behavior (spec scenario "A two-mode layer
      declared undirected validates silently"): `validate_goldfish_data()` on
      a two-mode layer declared `directed = FALSE` returns the object with no
      condition (`expect_no_condition()`). This test is GREEN on the current
      code by design; it guards against a later note or abort. Commit the
      test alone.
- [x] 1.3 Close group 1: NOT_CRAN=true full suite, baselines PASS (not SKIP),
      `devtools::document()`, commit.

## 2. Every reader takes `symmetric` from the mode map

Shared fixtures (in a helper, per r-lib:testing-r-packages), lifted from the
probe objects in `.plan/develop/dynes-prepare-data-evidence/probe-twomode-history.R`
so the panel and `prepare_dynes_data()` tests reproduce the recorded failure,
wave structure included: a square two-mode layer, senders E..H x receivers
A..D, a `time = NA` history row G->A and a wave-10 report of G->A; a
non-square two-mode layer, senders A..C x receivers D..H, a `time = NA`
history row C->H and a wave-10 report of C->H; each built with
`directed = FALSE` and with `directed = TRUE`; and a one-mode layer declared
`FALSE` with one tie A->B.

- [x] 2.1(a) Test `network_state_at()`: on the square fixture declared `FALSE`
      the state holds exactly G->A (no E->C); on the non-square fixture it
      returns exactly C->H with no `subscript out of bounds`; the `FALSE` and
      `TRUE` objects give identical matrices; the one-mode fixture holds
      A->B and B->A. Confirm RED (phantom on square, error on non-square).
- [x] 2.1(b) `network_state_at()` (`R/state_at.R:65`) passes
      `!lm$symmetric` to `materialize_network_state()` instead of reading
      `info$directed`. GREEN.
- [x] 2.2(a) Test the DyNES panel path through `diff_panel_waves()`: on both
      two-mode fixtures declared `FALSE`, the snapshot at each wave, the
      dyad mask (every cell of a two-mode layer) and the emitted rows equal
      those of the `TRUE` object; no spurious dissolution row on the square
      fixture; no error on the non-square one. The one-mode fixture keeps its
      upper-triangle mask and canonical rows. Confirm RED.
- [x] 2.2(b) `panel_layer_context()` (`R/wave_diff.R:44`) carries
      `symmetric` from the layer entry; `panel_wave_snapshot()`, the dyad mask
      and the flip enumeration (`R/wave_diff.R:71`, `:390`, `:418`) read it
      instead of `directed` / `one_mode && !directed`. GREEN.
- [x] 2.3(a) Test `prepare_dynes_data()` (spec scenarios "A non-square
      two-mode layer declared undirected does not crash" and "The two-mode
      result does not depend on the declaration"): on both two-mode fixtures
      the `FALSE` and `TRUE` objects return identical rows and identical
      starting levels, with no style-contradiction warning on the square
      fixture and no error on the non-square one; the one-mode fixture's
      canonical-key ordering and double-report warning are unchanged.
      Reproduces probe P2. Confirm RED.
- [x] 2.3(b) `prepare_layer_shape()` (`R/prepare_dynes_data.R:719`) carries
      `symmetric` from the layer entry instead of computing `directed` from
      `info$directed`; the starting fold (`:973`), the canonical-key and
      double-report checks (`:240`, `:670`, `:678`, `:976`, `:1157`) read it.
      GREEN.
- [x] 2.4(a) Test the stocnet data source: on both two-mode fixtures declared
      `FALSE`, the starting state (`ds_network()`), and the builders'
      `is_undirected` property are those of the `TRUE` object; the one-mode
      fixture is still `is_undirected = TRUE`. Add the manynet scenario: an
      object converted with `manynet::as_stocnet()` from a two-mode matrix,
      keeping its `FALSE` (named for its layer as found in 0.1) and declaring
      the two modes as disjoint sides, validates and has `symmetric = FALSE`
      (`skip_if_not_installed("manynet")`). This pins behavior that is already
      correct through the stocnet method (and the field 1.1(b) added), so it
      is GREEN before 2.4(b); it guards the refactor. Commit the test alone.
- [x] 2.4(b) `ds_is_directed.goldfishSourceStocnet` (`R/data_source.R:431`)
      returns `!isTRUE(ds_layer_map(src, name)$symmetric)` (`ds_layer_map()`
      already resolves a derived layer to its source; `isTRUE()` keeps a
      `NULL` entry, an unresolved focal, from yielding a zero-length logical)
      instead of its own two-mode check plus `info$directed`. Results are
      unchanged on every validated object (design D2). Rewrite the comment above `ds_is_directed()`
      (`R/data_source.R:420-421`) to say where the rule lives: the mode map
      resolves `symmetric`, and a two-mode layer is never symmetric (design
      Q6). 2.4(a) stays GREEN.
- [x] 2.5 Audit (design D3): grep `R/` broadly for `info$directed`,
      `info[["directed"]]`, `attr(…, "directed")` and `is_undirected`, and
      record every hit in `progress.md` in one of three groups:
      (1) fixed here; (2) already correct (the validator's coverage check,
      `build_mode_map()`, the walk reading the stocnet method's value);
      (3) out of scope, not on a goldfish data object's path, with file:line
      (expected: `ds_is_directed.goldfishSourceEnvir`,
      `R/model_preprocess_group.R:496`, `R/methods_update.R:167`/`:179`,
      `R/as_goldfish.R:124` print, `R/legacy_wrappers.R` assembly). A hit
      that a goldfish data object reaches and that is in none of groups 1–2
      is a missed reader: add an (a)/(b) pair for it before closing. A
      group-3 hit gets no task; check it against the design's Non-Goals list
      and add it there if it is missing.
- [x] 2.6 Close group 2: NOT_CRAN=true full suite, baselines PASS (not SKIP),
      commit.

## 3. Documentation and milestone

- [x] 3.1 Document the rule in the stocnet components section of
      `R/goldfish-data.R` (`?goldfish_data`): `directed` affects one-mode
      layers only; on a two-mode layer either value is accepted and the layer
      is never mirrored, so manynet's two-mode `directed = FALSE` needs no
      change (it is read as not symmetric). Do not edit `make_network()`'s
      docs: the legacy path still mirrors (design D3). Run
      `devtools::document()` and confirm the man/ page renders the text.
- [x] 3.2 Add a short paragraph to the two-mode vignette
      (`vignettes/two-mode.Rmd.orig`; install the package, then re-knit with
      `Rscript vignettes/rebuild-all.R` — `precompile.R` only defines the
      helpers — and check the knitted `two-mode.Rmd`): a manynet
      two-mode network's `directed = FALSE` needs no change, and a two-mode
      layer of a goldfish data object is never mirrored.
- [x] 3.3 Write the `NEWS.d/` fragment per `NEWS.d/README.md`
      (`two-mode-undirected-fold--symmetric-flag.md`): a two-mode layer
      declared undirected no longer gains phantom ties or crashes in
      `network_state_at()`, `prepare_dynes_data()` and the DyNES panel path,
      and every reader of a goldfish data object reads it the same as a
      directed one. Do not claim the legacy `network.goldfish` path.
- [ ] 3.4 Close group 3: NOT_CRAN=true full suite, baselines PASS (not SKIP),
      commit. Then run the conformance-inspector skill on the change before
      `dynes-prepare-data` group 4 is unblocked. On CONFORMANT, rescope
      `dynes-prepare-data` 4.3: it reuses this change's helper fixtures and
      covers only what 2.3 does not (for example, an interval start before
      the first survey on a two-mode layer).

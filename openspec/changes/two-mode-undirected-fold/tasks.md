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
      *Judgment call resolved (4.1):* the `R/add_flavor.R:352` raw read in
      `warn_style_contradicted()` stays, recorded as an exception after the
      reader table in design D2 and in the ADDED requirement "Layer symmetry
      resolved once on the mode map".
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
- [x] 3.4 Close group 3: NOT_CRAN=true full suite, baselines PASS (not SKIP),
      commit. Then run the conformance-inspector skill on the change before
      `dynes-prepare-data` group 4 is unblocked. On CONFORMANT, rescope
      `dynes-prepare-data` 4.3: it reuses this change's helper fixtures and
      covers only what 2.3 does not (for example, an interval start before
      the first survey on a two-mode layer).
      *Conformance (2026-11-11):* `.plan/develop/two-mode-undirected-fold-conformance.md`
      at `dc43a04d`: CONFORMANT, no blocking gap. Suite PASS 18160 / FAIL 0
      / SKIP 5, 54 baseline rows PASS (0 skipped), `_baselines`
      byte-identical, all nine site ablations red. Three should-fix gaps,
      triaged into group 4. Stays unticked until 4.7 closes: the 4.3 rescope
      waits there, because `dynes-prepare-data/tasks.md` carries uncommitted
      edits that are not this change's.
      *Done (2026-11-11, with 4.7):* `dynes-prepare-data` 4.3 rescoped (user
      request) on top of those uncommitted edits. It drops the "validator
      rejects it" branch and what 2.3/4.2 here already assert, and keeps the
      `T:294` rebuild, no history rows, the multi-wave round-trip and a
      start before the first survey. Its `progress.md` records group 4
      unblocked.

## 4. Conformance follow-ups (from 3.4)

Should-fix gaps 1–3 of `.plan/develop/two-mode-undirected-fold-conformance.md`,
amended in place before the trunk fold; the proposal stays `landed`. None
changes behavior on any validated object or touches per-event code. Gap 4
(uncommitted `R/state_at.R` roxygen re-wrap, `man/network_state_at.Rd`, the
`two-mode.Rmd` re-knit warning) is the user's working copy, not this change's,
and gets no task. Gaps 5–6 are notes with no action (guarded sites had no
pre-fix bug; the legacy readers are already owned by the D3 follow-up).

- [x] 4.1 (gap 1) Record `warn_style_contradicted()`'s raw read as a
      harmless exception, in the tracked artifacts rather than only in
      progress.md (user decision 2026-11-11: no code change). This resolves
      the 2.5 judgment call. `R/add_flavor.R:352` keeps reading
      `info$directed` to choose an unordered `pmin/pmax(from, to)` key for its
      style-contradiction check. That check keys dyads for a warning and
      mirrors no state. On a two-mode layer the unordered key is still
      one-to-one, because sender and receiver sets are disjoint and so
      `(j, i)` is never a valid tie; the warning therefore fires exactly as
      for `directed = TRUE`. Add this as one sentence after the reader table
      in design D2, and as one sentence at the end of the ADDED requirement's
      second paragraph ("Readers of legacy `network.goldfish` objects are
      outside this requirement"), naming the function as outside the
      requirement for that reason. Update the 2.5 tick note to point at it.
      Run `openspec validate --strict`. Commit. No code or test change.
- [x] 4.2 (gap 2) Tighten `prepare_dynes_data never mirrors an undirected
      two-mode layer` (`test-prepare_dynes_data.R:1917`) to state what the
      2.3(a) task and the non-square scenario say, not only parity with the
      `TRUE` object: the starting levels are exactly `7 -> 1` (square, G->A)
      and `3 -> 8` (non-square, C->H), each with value 1, and no warning of
      the style-contradiction class is raised on the square fixture. See each
      new assertion red under ablation (`R/prepare_dynes_data.R:975` reading
      the raw flag, as ablation A6 did), then restore. Commit.
      *Done (2026-11-11):* `expect_equal()` on the starting levels against
      `7 -> 1` / `3 -> 8`, value 1, for both shapes; the style-contradiction
      warning (`R/add_flavor.R:381`) has no condition class, so its absence
      is checked on its message ("but an event contradicts it"). Under A6 in
      a scratch worktree each new assertion is red, run one at a time: square
      starting levels gain the phantom `5 -> 3` (E->C), square raises the
      warning, non-square aborts `subscript out of bounds`. Evidence:
      `.plan/develop/two-mode-undirected-fold-evidence/a6_new_assertions.{R,log}`.
- [x] 4.3 (gap 3) Add a `specs/multimode-networks/spec.md` delta with a
      `## MODIFIED` block for "Effect validity derived per argument from the
      mode map" (living `openspec/specs/multimode-networks/spec.md:91`),
      restating the requirement in full with its line 120 sentence changed
      from "`directed` SHALL be treated as vacuous on a two-mode layer
      (noted and ignored)" to the D1 rule (accepted silently, never read as
      symmetric; see "Layer symmetry resolved once on the mode map"). Keep
      every scenario. Run `openspec validate --strict` and
      `bash .plan/opsx-spec-placement-check.sh two-mode-undirected-fold`
      (expect 3 placements, exit 0); add `multimode-networks` to the
      proposal's Modified Capabilities. Commit.
      *Done (2026-11-11):* the delta restates living lines 91-186 verbatim
      except that one sentence (diff against the living spec: one hunk);
      `openspec validate --strict` valid; placement check `OK (3 requirement
      placements)`, exit 0.
Three loose ends the report raised without rating them (user decision
2026-11-11: each gets a task and is fixed here).

- [x] 4.4 (report §4, cosmetic) Put one blank line between
      `test_that()` blocks in `test-wave_diff.R`: "the panel path never
      mirrors an undirected two-mode layer" (`:577`) follows the previous
      test's `})` directly. Commit with 4.2 or alone.
      *Done (2026-11-11):* blank line inserted before `:577`; the file is
      otherwise unchanged. Committed with 4.2.
- [x] 4.5 (report §1, unverified claim) Check the two-mode vignette's claim
      (`vignettes/two-mode.Rmd.orig:207-214`) that naming manynet's `FALSE`
      per layer, `directed = c(support = FALSE, contestation = FALSE)`,
      "gives the same model". Fit the vignette's `rate_fit` and `choice_fit`
      from the purled `.orig` once with each declaration and compare. If they
      differ, correct the sentence and re-knit `two-mode.Rmd` (3.2's method).
      *Done (2026-11-11):* the claim holds and the vignette needs no change.
      Both fits are identical: 5 rate and 6 choice coefficients,
      `identical() = TRUE`, max |diff| 0, log-likelihoods −8478.903 and
      −1765.472 under either declaration. Evidence:
      `.plan/develop/two-mode-undirected-fold-evidence/vignette_directed_check.{R,log}`.
- [x] 4.6 (report §1, behavior change of unknown reach) Establish whether any
      user-facing path sees D2's change for an undeclared `directed` (stocnet
      source and panel helpers: undirected before, directed now). If one does,
      add it to the `NEWS.d/` fragment; either way, record the answer in
      design D2.
      *Reopened (2026-11-11):* the first answer ("no user-facing path sees
      it") was refuted by the group-4 conformance re-check
      (`.plan/develop/two-mode-undirected-fold-g4-conformance.md`, gap A).
      `make_specification()` (`R/make_specification.R:174`, `:375`),
      `network_state_at()` (`R/state_at.R:52`, `:109`) and the internal
      `diff_panel_waves()` (`R/wave_diff.R:257`) do validate, but the formula
      path of `estimate_dynam()`/`estimate_rem()` (formula + `data =`),
      `compute_statistics()` and the deprecated `gather_model_data()` never
      call `validate_goldfish_data()`. On them an undeclared one-mode layer
      moved from undirected to directed: on a 4-actor, 7-event REM fixture,
      `sum(inertia)`/`sum(indeg)` went from 34/102 at `fb7321a0` to 21/63 at
      `26feb4d6`, and the `estimate_rem()` coefficients moved with them.
      `prepare_dynes_data()` already read an undeclared flag as directed
      (`!isFALSE()`), so it is unchanged.
      *Done (2026-11-11):* a `NEWS.d/` bullet with sub-bullets names the
      affected entries and says to declare `directed`; the D2 paragraph after
      the validator now states the change is user-facing and calls
      `diff_panel_waves()` internal. The missing validation on those entries
      predates this change (it breaks the living `single-data-object`
      requirement "Dual input path with one validate-and-stamp boundary") and
      is not fixed here; it is owned by `estimation-entry-validation`.
      *Follow-up (2026-11-11, scoped 4.6 re-check
      `.plan/develop/two-mode-undirected-fold-g4b-conformance.md`, G1–G4):*
      the fragment bullet now leads with `Fixed` (`Updated` is for
      documentation) and also names `gather_model_data()`, matching D2; the
      proposal's Impact states the formula-path move; the owner change's
      artifacts are committed. The re-check also reproduced the
      `estimate_dynam()` move on a 4-actor, 10-event DyNAM choice fixture
      (undeclared reads as `FALSE` at `fb7321a0`, as `TRUE` at head).
- [x] 4.7 Close the group: NOT_CRAN=true full suite (baselines PASS, not
      SKIP), then a scoped `conformance-inspector` re-check of gaps 1–3 and
      4.4–4.6 by a fresh agent. On CONFORMANT, do the rest of 3.4: rescope
      `dynes-prepare-data` 4.3 (once the user has committed or set aside the
      uncommitted edits in that file) and tick 3.4 and 4.7.
      *Done (2026-11-11):* suite at `26feb4d6`: PASS 18164 / FAIL 0 / SKIP 5,
      54 baseline rows PASS (0 skipped). Every later commit is docs or a test
      title only; `test-wave_diff.R` was re-run after the title change (86
      PASS, 0 FAIL). Re-checks:
      `.plan/develop/two-mode-undirected-fold-g4-conformance.md` (gaps 1–3,
      4.4, 4.5 closed; 4.6 refuted, gap A) and
      `.plan/develop/two-mode-undirected-fold-g4b-conformance.md` (4.6
      reworked: CONFORMANT; G1–G4 fixed in `49762c32`, `dbe01095`,
      `d4b2afc0`). The 4.3 rescope went ahead on the user's request, with
      that file's edits still uncommitted (see 3.4).

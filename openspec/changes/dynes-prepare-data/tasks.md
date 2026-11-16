Every implementation unit is an (a)/(b) pair. (a) writes the failing test,
confirms RED, and commits the test alone. (b) writes the minimal code, confirms
GREEN, and commits after `air format` + `lintr` on the touched files,
`devtools::document()` when roxygen changed, and the not-cran-test skill. Tests
follow r-lib:testing-r-packages. New console output follows r-lib:cli, with a
pinned cli context in snapshots. The new export is marked experimental per
r-lib:lifecycle. No group changes per-event code (design, "Storage and
per-event cost"), so no group opens with a cost guard. Feature branch: no
DESCRIPTION bump, no `NEWS.md` edit, no archive. The milestone is a `NEWS.d/`
fragment.

## 0. Settle the open questions (gate, no code)

- [x] 0.1 Decide design open question 1 with the user: the inputs and their
      order, whether the input form is declared or detected, and the
      function's name.
      *Resolved 2026-11-02:* `prepare_dynes_data(x, layers, wave_times =
      NULL)`, `layers` a named vector of declared forms (D7), per-layer waves
      (D8), no detection.
      *Revised 2026-11-04:* `prepare_dynes_data(x, layers, form = NULL,
      wave_times = NULL)`; `layers` declares the style, `form` the form (D7).
- [x] 0.2 Decide design open question 2 with the user.
      *Resolved 2026-10-30:* the fix, in `dynes-augmentation` 6.2i/6.2j
      (`67379526`, `55d682f5`), recorded there. No guard for rows at `t0`.
- [x] 0.3 Decide design open question 3 (count panels).
      *Resolved 2026-11-02:* a count panel is a valued state (D10), no abort.
      *Revised 2026-11-04:* counts arrive as totals or as amounts, and a
      falling total aborts (D10); a count layer is compact and unflavored
      (D16).
- [x] 0.4 Decide design open question 4: does the panel state at `t0` become
      `time = NA` rows?
      *Resolved 2026-11-02:* no; every wave keeps its time (D1), D2 and D5
      withdrawn.
- [x] 0.5 Decide whether flavoring belongs in the function.
      *Resolved 2026-11-04:* yes for `"mutually_exclusive"` (D5 reinstated);
      a layer with a recorded map aborts (D15 revised); ordinal and
      `"redundant"` panels deferred to v2; `estimate_dynes()` unchanged.
- [x] 0.6 Decide design open questions 5 (`form` default or required) and 6
      (renaming the flavors) with the user.
      *Resolved 2026-11-04:* `form` defaults to `"states"`; the flavors can be
      relabeled through `flavor_names`, since the names do not affect the
      algorithm (D5). The D15 abort stays; the warning pass-through and
      several flavors in one count layer go to v2.

## 1. Arguments, input checks, prepared-layers record

- [x] 1.1(a) Test the `layers` and `form` arguments: a layer without a `form`
      entry is `"states"`; a `"count"` or `"none"` layer given as
      `"changes"` is returned unchanged; an empty `layers`, a name repeated
      within `layers`, `form` or `flavor_names`, an unnamed `layers` entry, a style or
      form outside the vocabularies, a `form` entry for a layer not in
      `layers`, `"mutually_exclusive"` with `"per_wave"`, a missing layer, and
      an event layer each abort with a cli error (snapshot) before any row is
      rewritten (spec "Each layer declares a style and a form"). Test the
      `flavor_names` checks: an entry for a layer not in `layers`, for a
      `"none"` layer, with roles other than `creation`/`dissolution`, with a
      non-syntactic label, or with two equal labels each abort; an entry for
      a `"count"` layer aborts with the count guidance (no flavor needed, one
      `"count"` layer per event type) (snapshot) (spec "A mutually exclusive
      layer leaves the function flavored").
- [x] 1.1(b) Implement the argument validation and the pass-through (design
      D5, D7).
- [x] 1.2(a) Test the shared wave checks on `"states"` and `"per_wave"`
      layers: a dyad repeated at one wave aborts, a 0 row equals absence (no
      events on `"per_wave"`), an `NA` tie value aborts, a diagonal row of a
      one-mode layer aborts, an undirected one-mode tie reported in both
      orders with one value warns (snapshot) and counts once, and with two
      values aborts, on a binary layer and on a `"count"` `"per_wave"` layer
      (2 and 2 read as 2, not 4); repeated `time = NA` rows are not checked
      row by row; a two-mode
      undirected layer is not reordered (spec "A wave defines one value per
      dyad, valid for the layer's style").
- [x] 1.2(b) Implement the shared checks (design D9, D13, D14).
- [x] 1.3(a) Test the style checks: a `"mutually_exclusive"` `"states"` value
      other than 0/1 aborts; a `"mutually_exclusive"` `"changes"` value outside
      the update encoding aborts; a `"mutually_exclusive"` `"changes"`
      event that contradicts the style (creation on a present tie,
      dissolution on an absent one) aborts naming the layer, the wave and the
      dyad (snapshot); a fractional or negative `"count"` value aborts; a
      `"count"` `"changes"` layer is returned unchanged but aborts on a
      negative increment, or on a falling level under `replace` (starting
      state as wave 0); a `"count"` `"states"` total that falls aborts, naming the
      layer, the wave and the dyad (snapshot); a `"none"` layer takes any
      non-`NA` value; the starting state: two `increment` history rows
      folding to 2 on a `"mutually_exclusive"` layer abort, and a history
      level of 6 followed by a wave-1 total of 4 on a `"count"` `"states"`
      layer aborts naming wave 1 (snapshot); a passing starting state adds
      no grid time and no row (same spec requirement).
- [x] 1.3(b) Implement the style checks, including the starting-state checks
      and the change-list checks (design D5, D9, D10). The starting state is
      checked only, never written; each change-list check reads the layer's
      rows once, in time order, keyed by dyad.
- [x] 1.4(a) Test the record and the flavored-layer abort: the returned object's
      `info$dynes_prepared` is the declared styles named by layer, with no
      form recorded, a `"count"` `"changes"` pass-through included; naming a
      recorded layer again aborts (spec "A
      prepared layer cannot be prepared again"); a named layer with a recorded
      flavor map aborts with class `goldfish_layer_already_flavored`
      (snapshot), naming every such layer, and a flavored layer declared
      `"count"` gets the count guidance in that error (snapshot) (spec "A
      layer that is already flavored aborts").
- [x] 1.4(b) Implement the record `info$dynes_prepared` (design D6) and both
      aborts (design D15).
- [x] 1.5 Close the group: full `NOT_CRAN=true` suite with the frozen
      baselines PASS, `devtools::document()`, commit.

## 2. Rewrite by form, flavoring, wave grids

- [x] 2.1(a) Test the `"states"` diff: the first wave is written at its own
      time against the `time = NA` rows or the empty network; a dropped tie
      gets 0 under `replace` and −1 under `increment`; a stable tie is not
      repeated; no row moves to `time = NA`; an undirected tie that switches
      orientation between waves writes no row; on an `increment` layer the
      state before wave 1 sums repeated `time = NA` rows and wave values are
      read as levels; totals 6 then 9 on a `"count"` `increment` layer are
      written as 6 then 3; an `order` column is not carried onto the written
      rows; a `"none"` value of 0.3 then `0.1 + 0.2` writes a row (exact
      comparison); rows of layers not named keep position and values, written
      rows are sorted by (time, from, to) and appended, a missing `flavor` or
      value column is created with `NA` / 1 on other layers' rows, and rows
      written at a `Date` grid time on a `POSIXct` column are `POSIXct`.
      Include the `social_evolution` 51-actor counts (276 first-wave
      rows, 70 formed, 70 dissolved) (spec "Every wave is written at its own
      time as a change-list").
- [x] 2.1(b) Implement the diff (design D3, D4, D9: the starting state from
      `materialize_network_state()`, D13: canonical keys on undirected
      one-mode layers; `restore_time_class()` for the written times). No
      dense matrix per wave.
- [x] 2.2(a) Test the `"per_wave"` sum: amounts 6 then 3 are written as 6
      then 3 under `increment` and as 6 then 9 under `replace`, one row per
      dyad per wave, never one row per unit; a dyad without a row at a wave
      writes nothing there (same spec requirement).
- [x] 2.2(b) Implement the cumulative sum (design D10, D16), on dyad keys.
- [x] 2.3(a) Test the round-trip invariant: for every wave, the state
      materialized from the returned rows equals the survey's level, on a
      binary state fixture with three waves and on one calls fixture given as
      totals and as amounts, each under `increment` and `replace` (all four
      materialize the same state) (spec "The prepared state at every wave
      equals the survey").
- [x] 2.3(b) Make it pass, or confirm that 2.1(b) and 2.2(b) already do.
- [x] 2.4(a) Test the flavoring: a `"mutually_exclusive"` `"states"` layer's
      rows, `flavor` column, `info$values_equivalence` and `info$flavor_style`
      equal those of the diff followed by `add_flavor()` under both update
      semantics; a `"mutually_exclusive"` `"changes"` layer gains the map with
      its rows unchanged; a `"count"` or `"none"` layer gets no map and no
      `flavor` value; `flavor_names` relabels the map and the `flavor` column
      without changing the map's values (spec "A mutually exclusive layer
      leaves the function flavored").
- [x] 2.4(b) Implement the flavoring through `add_flavor()`'s internals
      (`validate_flavor_mapping()`, the `info` record,
      `warn_style_contradicted()`) (design D5, D16). `add_flavor()` itself is
      unchanged.
- [x] 2.5(a) Test the wave grids: one vector for all layers, a per-layer list,
      inference from row times for a layer without an entry (assumes no empty
      wave: a wave with no rows is not in the grid), full dissolution at an
      empty declared `"states"` wave, no row at an empty declared
      `"per_wave"` wave, layers on different schedules, the abort on a row off
      the grid, and a `Date` grid matching `POSIXct` rows; a `wave_times`
      entry for a layer absent from `layers` or declared `"changes"` aborts,
      and so do an unnamed list entry, an `NA` or non-finite grid time, a
      vector when every layer is `"changes"`, and a `"states"` or
      `"per_wave"` layer with no wave;
      an unsorted grid gives the sorted result silently; a duplicated time
      warns once with class `goldfish_duplicate_wave_time` (snapshot) and is
      kept once (spec "Waves are given per layer, ...").
- [x] 2.5(b) Implement the wave grids (design D8; `coerce_time()` on both
      sides of the grid match; grid checks, sort and de-duplication before
      any row is rewritten).
- [x] 2.6(a) Test that layers not named are unchanged and event rows keep
      their times (spec "Event rows keep their times").
- [x] 2.6(b) Implement, or confirm that the earlier units already pass it.
- [x] 2.7(a) Test the empty-wave warning: an unchanged middle wave and a first
      wave equal to the `time = NA` rows each warn with class
      `goldfish_empty_wave` (snapshot), naming the layer and the wave; empty
      waves on a `"states"` and a `"per_wave"` layer raise exactly one
      warning naming both; the function still returns (spec "A wave that
      writes no rows warns").
- [x] 2.7(b) Implement the warning (design D12).
- [x] 2.8(a) Test the summary: one message of class
      `goldfish_prepare_summary` per call, one line of totals per layer
      (snapshot, pinned cli context); a `"mutually_exclusive"` layer reports
      its events per flavor under relabeled names, with the first-wave rows on
      a separate starting-state line (276 starting, 70 formed, 70 ended on
      `social_evolution`); a `"count"` layer reports unit events and changed
      dyads, a `"none"` layer changed cells, a `"changes"` layer the same
      counts; `suppressMessages(classes = "goldfish_prepare_summary")`
      silences it and leaves the result unchanged (spec "A successful call
      reports what it wrote").
- [x] 2.8(b) Implement the summary (design D17), data interpolated per
      r-lib:cli; no `quiet` argument.
- [x] 2.9 Close the group: full `NOT_CRAN=true` suite with baselines PASS,
      `devtools::document()`, commit.

## 3. End to end

Each test here runs at `skip_on_cran()`. If one fails because of the
estimator, the fix is recorded and made in `dynes-augmentation`, not here: no
estimator change in this change (user decision 2026-11-04). No fit pipes into
`add_flavor()`: the function flavors (design D5).

- [x] 3.1(a) Test the `social_evolution` panel prepared as
      `"mutually_exclusive"` and fitted with one EM pass of `random` and the
      default start: the creation rate intercept is log(70 / (51 · T)) (spec
      "the scored creation count is the change count").
- [x] 3.1(b) Make it pass.
- [x] 3.2(a) Test a calls panel prepared as `"count"` from amounts: the wave
      diff reads it as a count panel and each interval's event count equals
      that wave's summed amounts (spec "a prepared count panel is estimated as
      a count panel").
- [x] 3.2(b) Make it pass.
- [x] 3.3(a) Test a fit started at the third wave of a panel with at least
      four waves: the augmentation's entering state equals the third wave's
      survey, the earlier changes are applied and not scored, and the
      equivalent `"changes"` input gives the same (spec "a fit started at a
      later wave enters with that wave's survey").
- [x] 3.3(b) Make it pass.
- [x] 3.4(a) Test the first-wave diff: under the default start the recipe
      holds no flip for it and the entering state is the first survey; with
      `start_time` before the first wave it is the first interval's flip set
      and the entering state is the `time = NA` rows; on a `"count"`
      `"states"` layer with `time = NA` rows and three waves, the recipe's
      intervals are only those between waves and no flip comes from the
      history rows (spec "The first-wave diff is augmented only when an
      interval opens before it").
- [x] 3.4(b) Make it pass, and document the earlier-start behavior in the
      function's roxygen (design D11).
- [x] 3.5 Replace the `dynes-panel` vignette's hand-written diff and its
      `add_flavor()` call with the function, if `dynes-augmentation` 6.3 has
      not been precompiled by then. Otherwise add a short section showing the
      call and stating that it reproduces the vignette's data. State in the
      vignette and the function's roxygen which style a binary modeled panel,
      a count panel and a valued covariate take, what each form means, that
      `order` is not carried on a diffed or summed layer, and that a layer
      flavored before preparation aborts.
- [x] 3.6 Write the `NEWS.d/` fragment (protocol in `NEWS.d/README.md`).
      Full `NOT_CRAN=true` suite with baselines PASS. Commit.

## 4. Conformance follow-up

From the S8 conformance report (`.plan/develop/dynes-prepare-data-conformance.md`,
HEAD `823757b6`, NOT CONFORMANT). Gaps 1 and 2 block. Gaps 3–6 are should-fix,
amended here before archive, and 7–9 are archive-time wording. The code under
gaps 1 and 2 is correct at HEAD. What is missing is a test that fails when that
code is removed, so the red step of each detector task is an ablation of the
named line, run in a detached worktree, not a RED commit. The root of gap 3 (the
walk mirrors a two-mode layer declared `directed = FALSE`) is owned by the new
change `two-mode-undirected-fold` (user decision 2026-11-05), not by this group.

**Gate (user decision 2026-11-09):** no group-4 task starts until
`two-mode-undirected-fold` is settled and landed. The mirror also corrupts the
read-back of a two-mode layer without history rows (probe P2: `G->A` shows a
phantom `E->C` in `network_state_at()`), so 4.3 cannot be split around it. With
the two-mode rule landed first, `prepare_dynes_data()` can no longer crash on
such a layer, so no interim abort is added here.

- [x] 4.1 Detect the skipped-wave fall (gap 1). On a `"count"` `"states"`
      layer, test a dyad at 2 at wave 1, unreported at wave 2 and at 5 at
      wave 3: it aborts as a total falling from 2 to 0, naming wave 2
      (snapshot). Also test that a declared empty wave on a `"count"`
      `"states"` layer aborts the same way (gap 6b). Red: replacing
      `skipped <- same & levels$k > prev_k + 1L` in
      `check_totals_never_fall()` with `rep(FALSE, n)` makes both fail. No
      code change.
- [x] 4.2 Detect the history fold under `increment` in `row_levels()`
      (gap 2). Test that a `"mutually_exclusive"` `"changes"` layer under
      `increment`, with a history tie (3, 7) = 1 and rows −1 then +1, passes
      unchanged and gains the map. Snapshot the summary of a `"count"`
      `increment` layer with history rows. Red: dropping `start_level +` from
      the increment branch of `row_levels()` makes both fail. No code change.
- [x] 4.3 Test two-mode layers faithfully (gap 3). Rescoped 2026-11-11 when
      `two-mode-undirected-fold` closed CONFORMANT. The rule it landed: a
      two-mode layer is never symmetric, and `directed` affects one-mode
      layers only. The "validator rejects it" branch is dropped (that
      change's D1 rejected the abort). Its 2.3 and 4.2 already test, through
      `prepare_dynes_data()` on `two_mode_survey_fixture()` (square, receiver
      mode listed first so sender G = 7 exceeds receiver A = 1; non-square),
      each with a `time = NA` history row and one wave: `FALSE` and `TRUE`
      give identical rows and starting levels, the absolute starting levels,
      no dissolution row, no style-contradiction warning, no error on the
      non-square layer. Do not repeat those. Reuse that helper (extend it,
      for example with a `history` argument and extra wave rows, rather than
      writing a new fixture) for only what it leaves out:
      (a) Rebuild `T:294` ("a two-mode layer has no diagonal and is not
      reordered"; find it by title) on the square shape, so receivers are
      receiver-side nodes and a sender's global id exceeds its receiver's;
      `check_rows()` keeps `from`/`to` as given.
      (b) Without history rows, on both shapes declared `FALSE`: the rows
      equal those of `TRUE`, with no error.
      (c) Several waves with a formation and a dissolution, on both shapes
      declared `FALSE`: the prepared state round-trips at every wave through
      `network_state_at()`, and no row is written that no wave reported.
      (d) An explicit `start_time` before the first survey, on a two-mode
      layer declared `FALSE`: the first survey is an interval endpoint
      (D11), and its flips are exactly the reported ties, with no mirrored
      partner.
- [x] 4.4 Make the flavor record one body (gap 4). Extract the stamping and
      `info` record that `add_flavor()` (`R/add_flavor.R:302-318`) and
      `flavor_prepared_layer()` both write into one internal helper, called by
      both. This is behavior-preserving: the frozen baselines and `T:953` stay
      green. Reword the proposal's "`add_flavor()` itself is unchanged" to say
      its behavior is unchanged. If the extraction turns out not to be
      behavior-preserving, reword D5 and the proposal instead ("the same rule,
      guarded by an equality test") and record why.
- [x] 4.5 A grid must have the `time` column's class (gap 5; user decision
      2026-11-09, design D8, replacing "a `Date` is midnight UTC").
      (a) Test: a `Date` grid on a `POSIXct` column, and a `POSIXct` grid on a
      `Date` column, each abort before any row is rewritten, naming the
      layer, the grid's class and the column's class (snapshot); a character
      grid on a `POSIXct` column aborts the same way, its hint asking for a
      conversion and naming no unit (snapshot, plus an assertion against
      `days|seconds`), and is never parsed; a numeric grid on a numeric
      column (integer grid on a double column included), a `POSIXct` grid on
      a non-UTC `POSIXct` column, and a UTC `POSIXct` grid on a
      Europe/Zurich column at the same instants still match. Turn `T:792` and `T:1178` (the Date-on-POSIXct
      matches) into the abort case, or replace them with same-class grids;
      the written-rows-take-the-column's-class scenario uses a `POSIXct`
      grid. RED.
      (b) Add the class check to `resolve_wave_grids()` by calling
      `check_grid_time_class(wave_times, time_column, layer, call)`, which
      `dynes-augmentation` 6.6.1(b) adds in `R/wave_diff.R` (decided
      2026-11-16: that change owns the helper, so the two functions give one
      message; do not write a second check). Call it before the
      finite-time check, which runs `coerce_time()` and would parse a
      character grid as GMT. Remove
      `time_on_column_scale()` (its call sites compare with `coerce_time()`
      on both sides), state the rule in `@param wave_times`, and format the
      off-grid abort's times in the column's class. Under one class the zone
      cannot differ in meaning, but print it for `POSIXct` so a reader sees
      it.
      (c) The helper compares classes only and assumes the column is a time
      class (`dynes-augmentation` 6.6.3 conformance, note N1). `estimate_dynes()`
      gets that for free, because `validate_goldfish_data()` rejects a
      character `ties$time`, but `prepare_dynes_data()` never calls it
      (`check_prepare_object()` checks only for a `ties` table). Without a
      guard, a character grid on a character `time` column passes the check
      and `coerce_time()` parses both as GMT, and a numeric grid on that
      column gets the hint "Convert the grid to <character> first". Test
      (snapshot): a character `time` column aborts before the grid check
      with the validator's "unsupported class" message. Then run the same
      time-class check on `x` at entry; do not loosen the helper.
- [x] 4.6 Move the journal-only behavior into the tracked artifacts (gap 6).
      (a) Add the abort on a `wave_times` list naming a layer twice to D8 and
      to the spec's grid-abort list and its malformed-grid scenario, and test
      it (snapshot). (b) State in D8/D10 and in `@param wave_times` that a
      declared empty wave on a `"count"` `"states"` layer aborts as a fall to
      0, since a total that drops to nothing is not a count. 4.1 pins it.
- [x] 4.7 Wording and cosmetics (gaps 7–9). In the spec, make the `flavor`
      column conditional ("created when a `"mutually_exclusive"` layer is
      prepared"). Add to the design's "Storage and per-event cost" section the
      one dense `n1 x n2` starting fold per layer with history rows (about
      93 MB at 3000 actors, setup only, D9). Change `clean_wave_rows()`'s bare
      `\`-newline string continuations to the file's `\\` form.
- [x] 4.8 Vignette: when the call is needed (user request 2026-11-09). Add a
      `dynes-panel` section stating when `prepare_dynes_data()` is needed
      (binary, count and valued panels given as survey snapshots), when it is
      not (event layers; a layer already written as a change-list, where
      `add_flavor()` alone suffices and `form = "changes"` only checks,
      records and flavors), and why `add_flavor()` is not enough (it labels
      rows and never rewrites them: on the raw `social_evolution` surveys it
      gives 552 creations, no dissolution, and warns on the 206 stable ties
      re-reported at wave 2). Prose only, so the precompiled `.Rmd` takes
      the same text and `dynes-panel.R` is unchanged.
- [x] 4.9 Close the group: `openspec validate --strict` and the placement
      pre-flight, `devtools::document()`, full `NOT_CRAN=true` suite with the
      baselines PASS, commit. Then flip `proposal.md` to `status: landed
      (feature/dynes, awaiting fold)` and request a scoped re-check by a fresh
      agent. The re-check must see ablations A07b and A21 red and gaps 3–9
      closed, gap 3 in full (with and without history rows, under the rule
      `two-mode-undirected-fold` landed), and gap 5 closed by the class rule.

## 5. Re-check follow-up

From the S12 scoped re-check
(`.plan/develop/dynes-prepare-data-g4-conformance.md`, HEAD `65db73bd`,
CONFORMANT for the scope asked, four should-fix items). As in group 4, where
the code is already right the red step of a detector task is an ablation of
the named line in a detached worktree, not a RED commit.

- [x] 5.1 Narrow the entry check's exemption to a logical column (gap 1;
      user decision 2026-11-16: keep the exemption, logical only).
      `check_prepare_object()` skips the time-class check when the ties
      `time` column is entirely `NA`, whatever its class, so an all-`NA`
      character column with a character grid is parsed as GMT and character
      times are written.
      (a) Test: an all-`NA` character `time` column aborts at entry with the
      validator's "unsupported class" message, with and without a character
      `wave_times` (snapshot); an all-`NA` logical column still reaches the
      "layer has no wave" abort. RED on the character case.
      (b) Exempt the column only when it is logical and entirely `NA`
      (`time = NA` on every row, which carries no class to check).
      (c) An all-`NA` logical column with a declared grid now aborts with
      "Convert the grid to <logical> first; a <numeric> grid is never
      parsed". Make that case say what is wrong: no row has a time, so the
      grid has no column class to match. Test it (snapshot). Do not loosen
      `check_grid_time_class()`; handle the case before calling it.
      (d) State the exemption in the spec requirement "Each layer declares a
      style and a form" (the entry check applies unless every `time` is a
      logical `NA`) and in D8, with the reason. It lives only in the
      gitignored `progress.md` today.
- [x] 5.2 Detect the order of the class check and the finite-time check
      (gap 2). Test that an unparseable character grid (`c("abc", "x")` on a
      `POSIXct` column) and a bare `NA` grid on a numeric column each abort
      with class `goldfish_wave_grid_time_class`, not with "must hold finite
      times". Red: moving the `check_grid_time_class()` call in
      `resolve_wave_grids()` below the finite-time check makes it fail. No
      code change.
- [x] 5.3 Detect the zone in the off-grid abort (gap 3). Snapshot the abort
      for a UTC-midnight `POSIXct` grid on rows at Europe/Zurich midnight:
      the row times print with their zone ("2008-09-09 CEST") and the waves
      with theirs ("2008-09-09 UTC"). Red: `format_wave_time()` reduced to
      `format(time)` makes it fail. No code change.
- [ ] 5.4 Reflow the NEWS fragment (gap 4). The grid-class bullet in
      `NEWS.d/dynes-prepare-data--prepare-panel-layers.md` wraps over two
      lines; `NEWS.d/README.md` asks for one line under 81 characters. Shorten
      it to one line, or split it into two bullets.
- [ ] 5.5 Close the group: `openspec validate --strict` and the placement
      pre-flight, `devtools::document()` if roxygen changed, full
      `NOT_CRAN=true` suite with the baselines PASS, commit. Then request a
      scoped re-check: ablations G5b and G5d red, the all-`NA` character
      probes aborting at entry. The archive-time wording (the spec's
      "whatever the class of the grid", the two unused `template` formals of
      `check_totals_never_fall()` and `warn_empty_waves()`) is notes 5 and 6
      of the report.

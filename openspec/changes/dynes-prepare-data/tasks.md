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
- [ ] 2.5(a) Test the wave grids: one vector for all layers, a per-layer list,
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
- [ ] 2.5(b) Implement the wave grids (design D8; `coerce_time()` on both
      sides of the grid match; grid checks, sort and de-duplication before
      any row is rewritten).
- [ ] 2.6(a) Test that layers not named are unchanged and event rows keep
      their times (spec "Event rows keep their times").
- [ ] 2.6(b) Implement, or confirm that the earlier units already pass it.
- [ ] 2.7(a) Test the empty-wave warning: an unchanged middle wave and a first
      wave equal to the `time = NA` rows each warn with class
      `goldfish_empty_wave` (snapshot), naming the layer and the wave; empty
      waves on a `"states"` and a `"per_wave"` layer raise exactly one
      warning naming both; the function still returns (spec "A wave that
      writes no rows warns").
- [ ] 2.7(b) Implement the warning (design D12).
- [ ] 2.8(a) Test the summary: one message of class
      `goldfish_prepare_summary` per call, one line of totals per layer
      (snapshot, pinned cli context); a `"mutually_exclusive"` layer reports
      its events per flavor under relabeled names, with the first-wave rows on
      a separate starting-state line (276 starting, 70 formed, 70 ended on
      `social_evolution`); a `"count"` layer reports unit events and changed
      dyads, a `"none"` layer changed cells, a `"changes"` layer the same
      counts; `suppressMessages(classes = "goldfish_prepare_summary")`
      silences it and leaves the result unchanged (spec "A successful call
      reports what it wrote").
- [ ] 2.8(b) Implement the summary (design D17), data interpolated per
      r-lib:cli; no `quiet` argument.
- [ ] 2.9 Close the group: full `NOT_CRAN=true` suite with baselines PASS,
      `devtools::document()`, commit.

## 3. End to end

Each test here runs at `skip_on_cran()`. If one fails because of the
estimator, the fix is recorded and made in `dynes-augmentation`, not here: no
estimator change in this change (user decision 2026-11-04). No fit pipes into
`add_flavor()`: the function flavors (design D5).

- [ ] 3.1(a) Test the `social_evolution` panel prepared as
      `"mutually_exclusive"` and fitted with one EM pass of `random` and the
      default start: the creation rate intercept is log(70 / (51 · T)) (spec
      "the scored creation count is the change count").
- [ ] 3.1(b) Make it pass.
- [ ] 3.2(a) Test a calls panel prepared as `"count"` from amounts: the wave
      diff reads it as a count panel and each interval's event count equals
      that wave's summed amounts (spec "a prepared count panel is estimated as
      a count panel").
- [ ] 3.2(b) Make it pass.
- [ ] 3.3(a) Test a fit started at the third wave of a panel with at least
      four waves: the augmentation's entering state equals the third wave's
      survey, the earlier changes are applied and not scored, and the
      equivalent `"changes"` input gives the same (spec "a fit started at a
      later wave enters with that wave's survey").
- [ ] 3.3(b) Make it pass.
- [ ] 3.4(a) Test the first-wave diff: under the default start the recipe
      holds no flip for it and the entering state is the first survey; with
      `start_time` before the first wave it is the first interval's flip set
      and the entering state is the `time = NA` rows; on a `"count"`
      `"states"` layer with `time = NA` rows and three waves, the recipe's
      intervals are only those between waves and no flip comes from the
      history rows (spec "The first-wave diff is augmented only when an
      interval opens before it").
- [ ] 3.4(b) Make it pass, and document the earlier-start behavior in the
      function's roxygen (design D11).
- [ ] 3.5 Replace the `dynes-panel` vignette's hand-written diff and its
      `add_flavor()` call with the function, if `dynes-augmentation` 6.3 has
      not been precompiled by then. Otherwise add a short section showing the
      call and stating that it reproduces the vignette's data. State in the
      vignette and the function's roxygen which style a binary modeled panel,
      a count panel and a valued covariate take, what each form means, that
      `order` is not carried on a diffed or summed layer, and that a layer
      flavored before preparation aborts.
- [ ] 3.6 Write the `NEWS.d/` fragment (protocol in `NEWS.d/README.md`).
      Full `NOT_CRAN=true` suite with baselines PASS. Commit.

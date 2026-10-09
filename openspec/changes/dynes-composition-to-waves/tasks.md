Every task is verified with the `not-cran-test` skill before its commit (the
frozen 1e-6 baselines report PASS, not SKIP), closed with `close-task`, and
follows `r-lib:r-package-development` / `r-lib:testing-r-packages`. Touched R
files are `air format`-ed and then linted. User-facing conditions follow
`r-lib:cli`, and their snapshots pin a cli context. This is a feature branch,
so the milestone writes a `NEWS.d/` fragment and bumps no Version.

The change adds no per-event code (design, "Storage and per-event cost"): the
rewrite runs once per fit and the guard once per bind. No group therefore
carries the cost-guard bracket. A task that finds itself editing the walk, the
simulation step or the likelihood stops and reopens the design.

The rewrite lives in a new `R/composition_waves.R`, with its tests in
`tests/testthat/test-composition_waves.R`. Fixtures are small `stocnet` joint
specifications with one modeled panel layer and, where needed, one modeled
relational layer, built through the existing `helper-estimate_dynes.R`
helpers where they fit.

Sequencing: this change lands before `dynes-augmenter-walk-alignment` task
1.1 records its references, and before the `dynes-augmentation` 6.2 sweep.

## 1. Involvement (D2)

- [x] 1.1a Failing test: the involvement helper, given a joint
      specification's data and a wave grid, returns per interval the
      involved global node ids. Fixtures:
      - a modeled relational event's sender and receiver, including one
        stamped exactly at `w_k` (interval `k`);
      - a flip's `from` and `to`;
      - an exogenous relational event, whose actors are not involved;
      - a two-mode panel layer whose `to` node is involved on the second
        node set, not on the first-set node with the same local index.

      Confirm RED and commit the test alone.
- [x] 1.1b Implement it. Modeled relational events come from `data$ties`
      with `findInterval(..., left.open = TRUE)` and the same modeled filter
      as `sim_observed_events()`. Flips come from `diff_panel_waves()` per
      modeled panel layer, with local indices mapped to global ids through
      the layer's `side1` / `side2`. It reads neither the recipe nor a
      completed specification. Confirm GREEN and commit.
- [x] 1.2 Close the group: NOT_CRAN=true full suite (baselines PASS), commit.

## 2. The presence-path rewrite (D3, D5)

- [x] 2.1a Failing test: one actor, one change, against the rewritten
      `active` rows of `changes`:
      - a join inside interval `k` of an involved actor moves to `w_{k-1}`;
      - a join exactly at `w_k` of an actor involved in `k` moves to
        `w_{k-1}`;
      - a join exactly at `w_k` of an actor involved only in `k + 1` is kept;
      - a leave inside interval `k` moves to `w_k`;
      - a leave exactly at `w_{k-1}` moves to `w_k`;
      - an uninvolved actor's join and leave are kept.

      Confirm RED and commit the test alone.
- [x] 2.1b Implement the rewrite as the presence path `P` of D3. Build `S`
      from `nodes$active` (default present) and the actor's real changes,
      build `P` interval by interval, and write the minimal change list
      inside `[w_0, w_K]` for each actor whose `P` differs from `S`. Times
      are compared after `coerce_time()`. Confirm GREEN and commit.
- [x] 2.2a Failing test: several changes and adjacency.
      - A leave and rejoin inside an involved interval leaves the actor
        present over the interval and at `w_k`, with no leave at `w_k`,
        under both row orders of the two changes.
      - Join, leave, join gives the same result.
      - An actor involved in `k` and `k + 1` and really absent at `w_k` is
        present across `w_k`, with no change written there.
      - An actor involved in `k` but not `k + 1` and really absent at `w_k`
        gets a leave at `w_k`.

      Confirm RED and commit the test alone.
- [x] 2.2b Make 2.2a pass. If 2.1b built `P` as specified, this should be a
      no-op or an edge fix at the wave points. Confirm GREEN and commit.
- [x] 2.3a Failing test: untouched data.
      - When no actor's `P` differs from `S`, the returned data is
        `identical()` to the input. This covers no `changes`, a zero-row
        `changes`, changes only for uninvolved actors, and changes only
        before `w_0` or after `w_K`.
      - On data that is rewritten, the rows of every other actor, of every
        other variable, and every row outside `[w_0, w_K]` are byte-identical
        to the input.
      - `nodes` is never edited.

      Confirm RED and commit the test alone.
- [x] 2.3b Make 2.3a pass: return the input object when nothing changes, and
      splice only the rewritten actors' in-window `active` rows otherwise.
      Confirm GREEN and commit.
- [x] 2.4a Failing test: the warning. A rewrite of two actors in two
      intervals raises exactly one `goldfish_composition_snapped_warning`.
      Its snapshot, under a pinned cli context, lists each (actor, interval)
      with the ignored real times and carries the two caveats of D5: the
      moved rate enters the whole interval's compensator, and wave-stamped
      events can see an early join. A no-op rewrite raises nothing. Confirm
      RED and commit the test alone.
- [x] 2.4b Implement the warning with `cli::cli_warn()`, interpolating data
      rather than literal markup, and letting cli truncate long lists.
      Confirm GREEN and commit.
- [x] 2.5 Close the group: NOT_CRAN=true full suite (baselines PASS), commit.

## 3. Wiring into `estimate_dynes()` (D1)

- [x] 3.1a Failing test: on a fixture with an involved in-interval join,
      `estimate_dynes()` warns with `goldfish_composition_snapped_warning`.
      The fit's `spec$data` and every `spec$specifications[[i]]$data` carry
      the rewritten composition. A completed relational rate's pin is
      computed on the rewritten composition: compare against
      `relational_window_risk_set()` on the rewritten and on the original
      data. Keep the fit small (one EM iteration, few sequences). Confirm RED
      and commit the test alone.
- [x] 3.1b In `R/estimate_dynes.R`, call involvement and the rewrite after
      `panel_wave_grid()` and the `control_prep` window, and before
      `complete_generative_spec()`. Install the rewritten data on the joint
      specification and on every process specification, as
      `bind_candidate_data()` rebinds. Confirm GREEN and commit.
- [x] 3.2 Characterization test (not a red/green pair: it holds by design):
      the diffs taken for involvement equal those inside the compiled
      recipe, on a fixture the rewrite changes. Commit.
- [x] 3.3a Failing end-to-end test: a seeded sim draw over an interval whose
      only remaining events belong to an actor that joins at its first
      event's stamp completes, and a draw with a leave before a flip's end
      completes. Today both strand: guard with the restart ceiling's
      condition so the RED run fails rather than loops. Confirm RED and
      commit the test alone.
- [x] 3.3b Expected GREEN from 3.1b with no further code. If not, the gap is
      a design defect: stop and reopen the design instead of patching the
      augmenter. Confirm GREEN and commit.
- [x] 3.4 Byte-identity check: on the existing DyNES fixtures and the frozen
      baselines (none of which has an involved in-interval change), fits are
      unchanged and no warning is raised. This is covered by the full-suite
      run. Record the confirmation in `progress.md`.
- [x] 3.5 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if any roxygen changed, commit.

## 4. The normalized composition guard (D4)

- [x] 4.1a Failing test: the composition normal form. It equates `NULL`,
      zero-row and composition-free `changes`; numeric and POSIXct times;
      label and index nodes; and two `changes` that differ only in a
      non-`active` attribute row. It separates two compositions that differ
      in one change's time, node or value. Confirm RED and commit the test
      alone.
- [x] 4.1b Implement the normal form: the `active` rows only, numeric time,
      global node id (the `to_global_id()` mapping), logical value, stable
      sort by (time, node). Confirm GREEN and commit.
- [x] 4.2a Failing tests:
      - `bind_candidate_data()` aborts with `goldfish_candidate_mismatch` on
        a composition difference, with a cli snapshot naming the composition
        and the consistent pairings, and binds on encoding-only differences.
      - `compute_lik_seq()` aborts on both mixed pairings (fit specification
        with original data; authored specification with a fit sequence) and
        scores both consistent pairings.

      Confirm RED and commit the tests alone.
- [x] 4.2b Extend `bind_candidate_data()` in `R/rate_queries.R`. Compute the
      specification's normal form once per bound specification, not per
      candidate, and abort through cli with the existing class. Keep the
      `info` / `nodes` checks and their message as they are. Confirm GREEN
      and commit.
- [x] 4.3 Characterization test: each built-in augmenter (`random`, `sim`,
      `mcmc`) draws on a rewritten fixture and its candidate binds to the
      fit's specification without abort. Commit.
- [x] 4.4 Close the group: NOT_CRAN=true full suite (baselines PASS), commit.

## 5. Milestone

- [x] 5.1 Write the `NEWS.d/` fragment (protocol in `NEWS.d/README.md`): the
      composition snapping and its warning, the v1 approximation it is, and
      the `compute_lik_seq()` behavior change on mixed pairings. No Version
      bump and no `NEWS.md` edit on this branch. Commit.
- [x] 5.2 Flip the change to landed (`status: landed (feature/dynes, awaiting
      fold)` in `proposal.md`) and run `conformance-inspector` on the whole
      change; triage its report in `.plan/develop/`.
      Report: `.plan/develop/dynes-composition-to-waves-conformance.md`,
      CONFORMANT, no blocking gap. Its seven should-fix gaps are group 6
      below. Its notes are owned elsewhere: the sim fit on
      `composition_fit_spec()` hangs because calls sit on the waves, which is
      `dynes-augmenter-walk-alignment` 1.2c/1.2d (folded into 1.2d); the
      restart-ceiling message that blames coupling for a composition stall
      goes to `dynes-augmenter-driver-v2`.

## 6. Conformance follow-ups (report of 5.2)

Group 6 must close before the trunk fold, but it does not gate
`dynes-augmenter-walk-alignment`. None of its tasks changes behavior on data
without an involved in-interval composition change, and that is the only data
walk-alignment's 1.1 references are recorded on.

- [ ] 6.1 Commit this change's `proposal.md`, `design.md`, `specs/` and
      `.openspec.yaml` (only `tasks.md` is tracked today; a clone has no spec
      to fold). Never `progress.md`.
- [ ] 6.2a Failing test: on data with POSIXct times, the snapped warning shows
      the interval bounds and the ignored times in the data's time class,
      not as epoch seconds; a rewrite of POSIXct `changes` writes POSIXct
      rows (pins `restore_time_class()`, which no test reaches today).
      Snapshot under a pinned cli context. Confirm RED and commit the test
      alone.
- [ ] 6.2b Format the warning's times through the data's time class. Align
      the NEWS.d fragment's "original and new times" with what the warning
      shows. Confirm GREEN and commit.
- [ ] 6.3 Detector for "the specification's normal form is computed once per
      bound specification": bind two candidates to one specification and
      count `composition_normal_form()` calls (mocked counter), expecting one
      for the specification and one per candidate. Confirm the test fails
      with the memo disabled. Commit.
- [ ] 6.4 Detector for the no-op at the snap level:
      `snap_composition_to_waves()` returns the specification identical, with
      no warning, on a fixture whose process specifications carry their own
      data. Confirm the test fails with the identity return at the snap
      removed. Commit.
- [ ] 6.5 Extend the two-mode test through `rewrite_composition()`: the
      second-set node's join moves to `w_{k-1}`, and the first-set node with
      the same local index is untouched. Commit.
- [ ] 6.6 Record the deviations that live only in `progress.md`: the
      warning's manual cap of 10 bullets (cli does not truncate separate
      bullets) in design D5; the single-slot memo in D4; and in the 4.3 tick
      note, that the sim case is a direct draw plus the 3.3 fits, because
      the sim fit on `composition_fit_spec()` waits on
      `dynes-augmenter-walk-alignment` 1.2d. Correct the design's cost
      formula to O(involved actors x composition rows). Commit.
- [ ] 6.7 Close the group: NOT_CRAN=true full suite (baselines PASS), then a
      scoped `conformance-inspector` re-check of group 6.

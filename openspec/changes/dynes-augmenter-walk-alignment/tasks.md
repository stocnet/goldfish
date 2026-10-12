Every task is verified with the `not-cran-test` skill before its commit (the
frozen 1e-6 baselines report PASS, not SKIP), closed with `close-task`, and
follows `r-lib:r-package-development` / `r-lib:testing-r-packages`. Touched R
files are `air format`-ed and then linted. This is a feature branch, so the
milestone writes a `NEWS.d/` fragment and bumps no Version.

## 1. Record the pre-change reference, fix the tie hang, pin the stamp order

- [x] 1.0 Gate: `dynes-composition-to-waves` has landed on this branch. Task
      1.1's references are recorded after it, never before.
- [x] 1.1 Record the pre-change sim draws. On one uncoupled and one coupled
      DyNES fixture with no stamp ties, no observed event at a wave time and
      no in-interval composition change, save the event times and
      `log_proposal` of seeded `augment_seq_sim()` draws (3 intervals,
      several seeds) as a test fixture with its generator script beside it.
      Record, in `progress.md`, the walk-open and `evaluate_fid()` counts per
      sim draw and per MCMC step, and the seconds and allocated MB per sim
      draw and per MCMC sweep on the 6.2 recovery fixture. These are the
      "before" values every later group is measured against. No package code
      changes.
      *Recorded (WA1, `b0c14308`):* the fixture is the study's toy shape with
      three intervals, and its coupled variant carries
      `support_constraint = ~tie(friendship)` on calls (without a constraint
      no legality walk runs). The generator and references are in
      `tests/testthat/_references/walk_alignment_v1/`; the values are in
      `progress.md` "Cost record". Note for later groups: the coupled sim
      draw restarts on every seed (8 to 498 walk opens), and an uncoupled
      MCMC step already opens 2 walks.
- [x] 1.2a Failing test (D7, standalone `fix:`): a sim draw over an interval
      with two tied observed relational events and at least one flip left
      completes. Guard the run with an iteration cap on `sim_flip_wait()`'s
      truncated draw (or a `callr` timeout) so the RED run fails rather than
      hangs. Confirm RED and commit the test alone.
- [x] 1.2b While a tied sibling of a placed relational event is pending, offer
      no flip candidate: the sibling is the next step with `wait = 0`. This
      removes the `width = 0` path. Confirm GREEN, confirm 1.1's untied
      references are unmoved, and commit as a `fix:`.
- [x] 1.2c Failing test (D7, standalone `fix:`): a sim draw over an interval
      `(w_{k-1}, w_k]` with an observed relational event at `w_k` and at least
      one flip completes, with every flip strictly before `w_k` and the event
      last. Use the same iteration-cap or `callr` guard as 1.2a. Confirm RED
      (a seed that selects the event first hangs) and commit the test alone.
- [x] 1.2d An observed relational event at its interval's closing wave is not
      weighted: it is selected only once no flip of the interval is left, with
      probability 1 and no `log_q` term. Confirm GREEN, confirm 1.1's
      references are unmoved, and commit as a `fix:`. Regression check: the
      one-iteration `sim` fit on `composition_fit_spec()`
      (`test-composition_waves.R`, calls at the waves 2 and 5) now finishes.
      Today it hangs with or without a composition change. Turn
      `dynes-composition-to-waves` 4.3's sim case into a fit. That fit is the
      only fixture combining a snapped composition with observed events at
      wave times: N6's join (real time 3) snaps to `w_1 = 2`, the stamp of
      the interval-1 `calls` event 2 -> 3, which reads N6 as present (D7,
      composition at the stamp). Assert the fit
      completes, not only that it terminates.
- [x] 1.3 Pin the stamp order D2 relies on: a test over a fixture that ties a
      dependent event with a covariate change and a composition change
      asserts that the merged schedule puts the dependent row first at the
      stamp. Put the tie at a wave time, where snapped composition rows now
      land (optional, but it is the case that occurs). It characterizes current behavior (it passes on the unfixed
      code), so it is not a red/green pair. Also add the stamp rule's *why*
      to the ordering comment in `R/preprocess_joint.R` (the actor's decision
      precedes the stamp, so nothing at the stamp informs it), with no
      reference to this change. Commit.
- [x] 1.4 Correct the record of the removed refusal (D6). Rewrite the two
      `openspec/changes/dynes-augmentation/design.md` passages that say a
      `goldfish_walk_unsupported` refusal remains, and replace the vacuous
      test in `tests/testthat/test-augmenters_dynes.R` with one asserting that
      a support constraint on a network no formula term reads flips
      `walk_event_supported()` when that network changes mid-interval. This is
      a characterization of behavior that already landed, so it is not a
      red/green pair.
- [x] 1.5 Close the group: NOT_CRAN=true full suite (baselines PASS), commit.

## 2. Legality replay at the batch stamp semantics (D1, D8)

- [x] 2.1 Cost guard: assert the legality replay opens one walk per check and
      allocates no per-event buffer of size `n1`, `n2` or `n1 x n2` beyond the
      stamp buffer built once per replay. D1 adds no cost, so this guard pins
      the current cost rather than failing on it. Record that it passes on the
      unfixed code, and commit it alone.
      *Passes on the unfixed code (WA4):* the guard is the per-event slope of
      a check's allocation (40 vs 160 replayed calls), walk steps mocked out,
      compared at 60 and 240 actors: 17279 vs 17262 B per event. A mocked
      per-event `runif(n)` fails it (+1428 B against a 360 B limit). The
      first support read of a walk allocates a grid once per check (the
      availability catching up), which the slope cancels.
- [x] 2.2a Failing test: a candidate whose in-interval event shares its stamp
      with an exogenous row that would exclude the event's dyad is judged
      legal. Confirm RED and commit the test alone.
- [x] 2.2b Advance the replay with `walk_advance(handle, t, inclusive = FALSE)`
      before checking. Confirm GREEN and commit.
- [x] 2.3a Failing test: two tied in-interval observed events, where injecting
      the first would exclude the second, are both judged legal. Confirm RED
      and commit the test alone.
- [ ] 2.3b Group the replay by stamp: check every event of the stamp, then
      inject them in row order (the time-anchored driver's discipline). Write
      the discipline as the D8 stamp helper (advance strictly before a stamp,
      run an optional check hook on every event of the stamp, then inject the
      stamp's realizations), and run the legality replay through it with
      `walk_event_supported()` as the hook. Confirm GREEN and commit.
- [ ] 2.3c Failing test (D5): with a non-default `control_prep` (a
      `start_time`), the legality checker's walk is opened with the same
      `control_prep` the candidate's log-likelihood is scored with (assert
      through a mocked `walk_open()` capturing its control). Confirm RED and
      commit the test alone.
- [ ] 2.3d Open every legality walk with the `control_prep` the likelihood
      uses, not `set_preprocessing()`. Confirm GREEN and commit.
- [ ] 2.4 Rerun every seeded `augment_seq_random()` / `augment_seq_mcmc()`
      test. Any expectation that moved must trace to a stamp tie in its
      fixture; record each in `progress.md`.
- [ ] 2.5 Close the group: measure legality replay time and allocation on the
      6.2 recovery fixture against 1.1's values (expected unchanged; the
      legality walks are inside the coupled sim draw and MCMC step values 1.1
      recorded, so measure the replay on its own), run the
      NOT_CRAN=true full suite (baselines PASS), commit.

## 3. Rate-query replay on the stamp helper (D2, D8)

- [ ] 3.1 Characterization test (passes on the unfixed code) of the `after =
      k` contract: for two tied candidate events at `s`, `after = k1` reads the
      state with only the first realization at `s` applied and `after = k2`
      with both, and neither has an exogenous row at `s` whose stream sorts
      after the focal stream. Commit it alone.
- [ ] 3.2a Characterization test (passes on the unfixed code, not a red/green
      pair): on a fixture where a row of a flavor no process models, on a
      focal layer, shares a stamp with a modeled event, `walk_replay_to()`
      leaves the same state at every `after = k`. It keeps the rewrite
      injecting those rows as realizations. (The replay-skip drift the
      original 3.2a targeted is unreachable here: no rate-query walk
      registers replay rows.) Commit it alone.
- [ ] 3.2b Failing guard: `walk_replay_to()` reaches no handle internal that
      `walk_advance()` owns (assert through a mocked
      `walk_apply_object_event()` call count of zero from `rate_queries.R`).
      The existing `query_rate_at_position()` tie tests, 3.1 and 3.2a are the
      behavioral oracle. Confirm RED and commit the test alone.
- [ ] 3.2c If a fixture can put a covariate stream before the focal stream:
      failing test that an exogenous row at `s` sorting before the focal
      stream is not applied at `after = k`. Confirm RED and commit the test
      alone. Otherwise record in `progress.md` why no fixture can.
- [ ] 3.2d Rewrite `walk_replay_to()` for `after = k` (realization `k` at
      stamp `s`) as the D8 helper (no check hook) over the candidate's stamps
      before `s`, then `walk_advance(handle, s, inclusive = FALSE)`, then the
      realizations at `s` through `k` in schedule order. Retire
      `walk_replay_exogenous_row()`, and correct the
      `query_rate_at_position()` comment if needed. Confirm GREEN (3.1, 3.2a,
      3.2b, 3.2c and every existing rate-query test) and commit.
- [ ] 3.3 Close the group: NOT_CRAN=true full suite (baselines PASS), commit.

## 4. Sim augmenter: no prefix evaluation, one evaluation per fid, one walk when uncoupled (D3, D4)

- [ ] 4.1 Cost guard: count `evaluate_fid()` calls per sim draw (mocked
      counter) and walk opens per draw on the uncoupled and coupled fixtures
      from 1.1. Assert the targets: at most one evaluation per rate fid per
      attempt at a state before the drawn interval's start (none for a choice
      fid), at most one evaluation per rate fid per step, and one walk per
      uncoupled draw. Record it FAILING on the unfixed code and commit it
      alone.
- [ ] 4.2a Failing test: interval `k > 1` evaluates each rate fid at most once
      per attempt at a walk state before `wave_start`, independent of the
      prefix length, and the seeded draws equal 1.1's reference. Confirm RED
      (the evaluation assertion fails; the identity holds) and commit the
      test alone.
- [ ] 4.2b Add the augmenter's move wrapper (the D8 helper plus single
      advances; the only code in the augmenter that moves the handle). Apply
      the forced prefix in the first `sim_clock()` call of an attempt through
      it (no check hook): for each forced stamp, `walk_advance(inclusive =
      FALSE)`, then inject the stamp's events; then advance through the gap to
      `wave_start` (`inclusive = FALSE`) before returning the breakpoint wait.
      Drop the prefix from `n_events`. Confirm GREEN, including seeded
      identity, and commit. If the step-contract route proves fragile, switch
      to D3's fallback (an internal `prefix` argument on
      `simulate_replicate()`) and record why in `design.md`.
- [ ] 4.3a Failing test: a step with several candidate pairs on one rate fid
      evaluates that fid once. Confirm RED and commit the test alone.
- [ ] 4.3b Add the per-step cache on the attempt state, cleared at each
      `sim_clock()` entry and by the move wrapper, and read by
      `sim_flip_options()`, `sim_relational_weight()`, `sim_flip_wait()` and
      `sim_mark()`. Confirm GREEN and seeded identity, then commit.
- [ ] 4.3c Test (pins D4's invalidation rule; RED only once 6.2's last-call
      advance exists, so it lands with 6.2 if written earlier as `skip()`):
      on a fixture with an exogenous row between a relational event's
      selection and its stamp, the last-call support check reads the value at
      the advanced state, and fails against a cache cleared only at entry.
- [ ] 4.4a Failing guard: no `data.frame` row extraction or `unique()` over
      data-frame columns in the sim step path (count `[.data.frame` calls per
      step through a trace, as the walk-cost helpers do). Confirm RED and
      commit the test alone.
- [ ] 4.4b Build the plan's flip, forced and relational columns once in
      `sim_plan()` / `new_sim_attempt()`, and read them by integer index. The
      chain heads become event ids. Confirm GREEN and seeded identity, then
      commit.
- [ ] 4.5a Failing tests: (i) an uncoupled three-interval draw opens one
      walk, and its seeded output equals 1.1's reference; (ii) retarget
      `test-composition_waves.R`'s "a sim draw strands on the real
      composition". Its fixtures (`join_at_event_spec()`,
      `leave_before_flip_spec()`) are uncoupled and call `augment_seq_sim()`
      on an unsnapped spec, which bypasses `dynes-composition-to-waves`. It
      now expects `goldfish_dynes_uncoupled_strand` with no restart (drop
      its `restart_ceiling` mock and assert no restart was counted). The
      coupled ceiling test in `test-augment_seq_sim.R` ("an interval no draw
      can complete aborts at the ceiling") is the control and must stay
      unchanged. Confirm RED and commit the tests alone.
- [ ] 4.5b Run an uncoupled draw as one driver run over all intervals, with
      the attempt state advancing `k` when interval `k` has placed everything
      (returning a breakpoint wait to the next `wave_start` before drawing the
      next interval). A strand on the single walk aborts with class
      `goldfish_dynes_uncoupled_strand` (D3), never a restart. Keep the
      per-interval attempt for a coupled spec. Confirm GREEN (4.5a (i) and
      (ii), the coupled control), the 4.1 guard GREEN, and seeded identity,
      then commit.
- [ ] 4.6 Close the group: measure sim draw seconds, allocated MB, walk opens
      and evaluations on the 6.2 recovery fixture against 1.1's values and
      design.md's prediction, and record them in `progress.md`. Compare the
      coupled fixture per seed and on the median (1.1's coupled draws
      restart 8 to 498 times, so a mean is the tail), and report evaluations
      per walk open as well as per draw. An unchanged coupled restart count
      is expected: D3 changes only the uncoupled path. Confirm
      `walk_cost.csv` / `estimation_cost.csv` are unmoved. Run the
      NOT_CRAN=true full suite (baselines PASS) and commit.

## 5. MCMC: one pass over the proposed sequence (D5)

- [ ] 5.1 Cost guard: count walk opens per MCMC move (mocked `walk_open()`
      counter) on a constrained fixture (the coupled one from 1.1: an
      unconstrained fixture has no legality walk and already opens two, so
      it cannot fail this guard): at most two for a legal proposal,
      and no reverse rate read or log-likelihood score for an illegal one.
      Record it FAILING (three opens) on the unfixed code and commit it alone.
- [ ] 5.2a Failing test: a fused candidate pass over a proposed sequence
      returns the same `legal` decision as the group 2 replay, and the same
      reverse rates as `query_rate_at_position()`, on fixtures with and without
      a stamp tie. It stops at the first illegal stamp with no rates. Confirm
      RED and commit the test alone.
- [ ] 5.2b Implement the fused pass inside `make_proposal_evaluator()`'s
      closure, behind an optional `legality = list(interval, waves)`
      argument: one walk on the proposed sequence opened with the evaluator's
      `control_prep`, the D8 helper with `walk_event_supported()` as the hook,
      and `pair_intensity()` at the `after = k` positions, with row positions
      computed once per candidate. It returns `list(legal, rates, loglik)`;
      an illegal candidate gets no rates and no log-likelihood. Calls without
      `legality` are unchanged. Confirm GREEN and commit.
- [ ] 5.3a Failing test: `mcmc_step()` on a seeded chain makes the same
      accept/reject sequence as before over a sweep with no stamp ties, and
      meets the 5.1 guard. Confirm RED and commit the test alone.
- [ ] 5.3b Route `mcmc_step()` through the evaluator's `legality` call in
      place of the separate checker and reverse-query walk. Keep
      `make_proposal_evaluator()`'s existing contract. Confirm GREEN and
      commit.
- [ ] 5.4 Close the group: measure MCMC sweep seconds, allocated MB and walk
      opens on the 6.2 recovery fixture against 1.1's values and the
      prediction, and record them in `progress.md`. Report the coupled
      fixture for the 3 to 2 claim (1.1: 2.67 opens per step, 3 on a legal
      step) and the uncoupled one as an unchanged control (2.0). Run the NOT_CRAN=true full
      suite (baselines PASS) and commit.

## 6. Sim augmenter: tied observed relational events at the pre-stamp state (D7)

Builds on 1.2b (no flip while a sibling is pending) and 4.4b (the relational
time column read by integer index).

- [ ] 6.1a Failing test: on a coupled fixture where two observed relational
      events tie at `t` and injecting the first would remove the second's
      dyad from the support, a seeded sim draw does not strand at the stamp:
      the second is supported at the state strictly before `t`. Confirm RED
      and commit the test alone.
- [ ] 6.1b Failing test: a tied sibling is not selected, so it adds no term to
      `log_proposal`. Assert through a mocked `sim_relational_weight()` /
      `sample_index()` call count of zero between the first event's placement
      and the sibling's. Confirm RED and commit the test alone.
- [ ] 6.1c Failing tests (the last-call check): under coupling, (i) a pending
      relational event whose support an exogenous row removes and a later row
      before the stamp restores is placed; (ii) a selected first event whose
      support an exogenous row between selection and stamp removes strands
      the attempt. Confirm RED and commit the tests alone.
- [ ] 6.2 Make each later event of a tied stamp a forced step (`wait = 0`, no
      selection, no `log_q` term). On the clock call where
      `walk_next_breakpoint(handle) >= stamp` (the last before the stamp),
      advance through the move wrapper with `walk_advance(handle, stamp,
      inclusive = FALSE)`, then, under coupling, check the support of every
      relational event at the stamp, the first included; strand if any lacks
      it. Earlier calls only carry the step forward. Confirm GREEN (6.1a,
      6.1b, 6.1c, 1.2a, 4.3c) and commit.
- [ ] 6.3 Rerun every seeded `augment_seq_sim()` test. Any expectation that
      moved must trace to a stamp tie in its fixture; record each in
      `progress.md`. 1.1's untied references must not move.
- [ ] 6.4 Close the group: NOT_CRAN=true full suite (baselines PASS), commit.

## 7. Milestone

- [ ] 7.1 Write the `NEWS.d/` fragment (protocol in `NEWS.d/README.md`):
      faster DyNES augmentation draws, legality checks and sim draws that
      agree with the likelihood at stamp ties and under a non-default
      preprocessing control, and the fixes for sim draws that hung on tied
      observed events and on an observed event at a wave time. Do not touch the DESCRIPTION Version
      or `NEWS.md`. Commit.
- [ ] 7.2 Run `conformance-inspector` on the change. Triage its report and
      re-check the scoped findings before the change is trusted.

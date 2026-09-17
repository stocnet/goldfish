## 1. Simulation surface decisions

- [x] 1.1 Resolve the design.md open questions before implementing (entry points
      and generic naming, ordered-family default mode, legal writer sinks and
      output class, exogenous-horizon behavior, `max_events` default, coordination
      rejection bookkeeping); record the decisions as design amendments
      — done 2026-08-19 (explore session): D2/D3 revised, D6–D9 added,
      ADR-0033/ADR-0034; residual surface items (writer sinks, output class
      name, threshold constants) settle at implementation
- [x] 1.2 Measure the preprocessing substrates before any dispatch flips (added
      2026-09-08): time `preprocess_flavored()` / the recipe loops against
      `preprocess_joint()` on (a) Social Evolution with a full model (rate +
      choice, windowed and interaction terms, a support constraint) and (b)
      CollegeMsg (`.plan/datasets/CollegeMsg.txt`, ~60k events) as the stress
      case; also time one `walk_open()` replay of the observed sequence
      against the batch merged walk, since that is what one `simulate()`
      replicate costs. Record wall time, peak memory and the merged/oracle
      ratio in `.plan/`; the joint test's 0.92x on the two-flavor fixture is
      the only number so far and the fixture is tiny. The result decides
      whether the recipe loops stay as the batch fast path or the merged walk
      becomes the single loop (design D10's ADR-0045 note); no code moves on
      this task
      — done 2026-09-09 (session `goldfish-50`), full write-up in
      `.plan/sp/preprocess_timing_2026-09.md`, scripts beside it. CollegeMsg
      base model: recipe loops 219.6 s, merged 67.2 s, ratio **0.31**; Social
      Evolution the other way, merged 1.31x-1.78x. The reversal is a per-event
      adjacency-matrix copy present in both substrates, so the gate is recorded
      as a **no-go** and the fixes move to `preprocess-one-walk` group 0
      (ADR-0057). Four findings land on THIS change instead: one replay
      replicate is 74% index `data.frame()` construction that
      `walk_evaluate_choice_matrix()` discards; a DyNAM step needs one sender's
      row, not the matrix (D12); `walk_open()` refuses user support constraints
      and both merged entries crash on a windowed term before the intended
      abort; one CollegeMsg replicate extrapolates to about 1.7 hours at 104 ms
      per step, peaking at 4.2 GB over 500 steps

- [x] 2.0 Walk-handle lifts the driver depends on (added 2026-09-07; each with
      **(a) waits on `preprocess-one-walk` task 0.5a**, which stores the support
      mask by its axis-union kind — wiring the mask onto the handle before that
      would inherit one dense n1 x n2 logical per snapshot, which exhausts 24 GB
      on a realistic node set. Both changes run on `feature_simulation`, so this
      is task ordering on one branch, not a cross-branch fold (decided
      2026-09-09; the two changes' spec deltas do not overlap, so neither
      declares `depends-on`).
      Each lift ships with
      the batch-vs-replay equality test in `test-walk_handle.R` extended to
      the lifted case): (a) user support constraints and the derived flavor
      masks maintained live on the handle, replacing the all-active
      `active_sender`/`active_dyad` in `walk_build_state()` (D4); (b)
      node-composition changes advancing through `walk_advance()` (D6); (c)
      window effects in the merged walk (`build_walk_engine()` abort lifted,
      eager expiry pseudo-events per engine) plus a driver-inserted breakpoint
      API on the schedule, since `exo_rows` is precomputed at open (D8); (d)
      per-ego parameters on `evaluate_process_state()` and `walk_evaluate()`:
      `parameters` may be an `n_ego × p` matrix (row-wise product), the
      vector case byte-identical, a coordination fid taking one matrix per
      side (D11). Land (a), (b) and (d) before 2.1; (c) before 2.7. Frozen
      baselines untouched: the lifts are on the stepping handle and the
      evaluators' matrix branch, not the batch loops
      — done 2026-09-13 (session `goldfish-23`), frozen baselines PASS in
      every full run. (d) `9a56835`: vector or `n_ego x p` matrix; a
      coordination fid takes ONE per-actor matrix (each directed half uses its
      sender's row), the per-side shape left to `two-sided-coordination`.
      **Reverted 2026-09-15 (ADR-0074, task 2.0e):** the matrix branch is not
      goldfish's; the variant is the `evaluate` plug point.
      (a) `8641656`: the handle advances the batch walk's own atom stores and
      recomputes masks with `build_mask_maintainer()`; flavored derived masks
      ride the same path; replay matches batch exactly on dyadic,
      receiver-axis and flavored fixtures. **Gap:** a constraint on a changing
      object no formula term reads is not stepped by the shared schedule, so
      the handle aborts on it (`goldfish_walk_unsupported`). (b)+(c)
      `a21b20c`: presence cursors, `walk_schedule()`,
      `walk_next_breakpoint()`; windows already ran on the merged walk
      (`preprocess-one-walk`), so (c) added the breakpoint API and the windowed
      replay tests
- [x] 2.0e Substrate corrections before 2.1 (added 2026-09-15): (i)
      `git revert 9a56835` — the evaluators and `walk_evaluate()` take a
      vector only; the reverted diff saved as `.plan/per_actor_evaluate.patch`
      and handed to goldfish.latent as its evaluate step (its
      `randomEffects-actor.R`); the three matrix tests go with it. (ii)
      `.pse_eval_choice()` stops building the index `data.frame` its only
      caller discards (parallel integer vectors, as the R estimation backend
      does) and `walk_evaluate()` grows the `sender` argument (D12; absent =
      today's full matrix, so `test-walk_handle.R`'s oracle is unchanged).
      (iii) Re-measure D12's three numbers on the current tree and date them
      in the design. (iv) Correct `preprocess_flavored()`'s header comment
      (two walks per family) or retire the function if
      `printing-homogenization` task 1.1 has landed. Verification:
      `NOT_CRAN=true` green, baselines PASS not SKIP.
      — done 2026-09-15 (session `goldfish-23`), one commit per item, the full
      `NOT_CRAN=true` suite green at each (8670 PASS / 0 FAIL / 5 SKIP, all six
      baseline files PASS not SKIP). (i) `8d77ba4`: the revert applied cleanly,
      nothing resolved by hand; the diff is `.plan/per_actor_evaluate.patch`
      (gitignored) for goldfish.latent. (ii) `4ae35b2`: `.pse_eval_choice()`
      returns parallel integer vectors — column access (`index$index_i`) reads
      the same as the frame did, so only the one test counting `nrow()`
      adapted; `walk_evaluate(sender =)` slices the per-sender model from the
      same evaluator and aborts `goldfish_walk_bad_sender` on a fid that has no
      sender's row to give. (iii) `8519207`: D12 dated —
      `.plan/sp/replay_split_2026-09-15.R`. (iv) `5ae1362`: header corrected,
      `preprocess_flavored()` NOT retired (`printing-homogenization` 1.1 has
      not landed)
- [x] 2.0f The walk carries the focal layer of every compiled unit (added
      2026-09-16, D13): `build_merged_blocks()` appends each unit's focal
      layer to the shared object registry **after** the units' own objects
      (no existing oid moves) and appends that layer's event stream to the
      joint schedule as a covariate stream, so the state it materializes is
      also advanced. Batch included: byte-identical wherever a formula reads
      its own focal layer, which is every frozen baseline. Fixes two
      refusals a specification of only exogenous covariates hits today —
      `walk_inject()`'s `goldfish_walk_bad_event` on the layer it writes, and
      `walk_assert_covered_constraints()`'s `goldfish_walk_unsupported` when a
      derived flavor mask's atoms read that layer. Tests: an exogenous-only
      DyNAM (`rate = ~ 1 + ego(floor)`, `choice = ~ alter(floor)` on
      `social_evolution`) simulates and its drawn events land on the focal
      network; a flavored specification whose effects read only another layer
      opens and draws under its derived masks; `preprocess_joint()` output is
      unchanged on a fixture that reads its own focal layer, and the frozen
      baselines PASS. Storage (dense vs sparse state) is out of scope — vault
      ADR-0081
      — *scheduled 2026-09-16*: **second, after 3.1a and before 2.8**. It is
      2.8's prerequisite, not merely tidier first: 2.8 replays an unmodeled
      flavor's observed events as *exogenous rows*, and a focal layer's rows
      can only be applied that way once the layer is a registered object with
      a covariate stream. With 2.0f landed, 2.8 filters that stream (apply the
      rows whose value maps to an unmodeled flavor) instead of building the
      plumbing itself; `exo_rows`' wholesale exclusion of focal layers is the
      one piece 2.8 still owns
      — *done 2026-09-16* `2822625` (goldfish-f0), 8825 / 0 / 4, six baseline
      files PASS. `build_shared_objects()` appends the focal network row after
      the terms' objects (deduped on network key); `build_joint_schedule()`
      appends its streams after every planned stream, from the unit's own
      source. `preprocess_joint()` output compared with waldo before/after on
      four fixtures (endogenous single process, two-process join, flavored,
      and exogenous-only): identical on all four, the exogenous-only one too,
      since no engine reads the new rows
- [x] 2.1 `simulate()` S3 generic + methods (fitted result → θ̂; specification →
      explicit `coef`) with the `times = c("generated", "observed")` axis,
      driving `walk_open`/`walk_advance`/`walk_evaluate`/`walk_inject`; the
      per-step draw-next-mark core factored for reuse by `augment_seq_sim()`.
      One `walk_open()` per replicate, every fid evaluated from its compiled
      engine, no per-sub-model pass (D10). The driver evaluates the completed
      effect-free fids the handle refuses (`assert_walkable_submodels()`):
      uniform choice as an equiprobable draw over the receiver support, pinned
      rate as the period's constant `exp(intercept_w)` (D5). Fit methods
      register per the `fit-class-hierarchy` delta (`goldfishFit` override,
      `goldfishFlavFit` override as one competing run, not a fan-out); the
      result class is `goldfishSim` (confirm against the class-naming guard
      test). The loop is written against the four plug points of D11 from
      the start: exported `set_simulation_steps(parameters, evaluate, clock, mark,
      accept)` → `goldfishSimSteps` and `set_parameter_provider(init, at)` →
      `goldfishParamProvider`, each slot defaulting to the descriptor-keyed
      step, the `evaluate` step receiving the narrow (stats rows, θ, risk
      set, meta) contract and returning the fid's values (D11, ADR-0074);
      `coef` resolves (numeric / `goldfishParams` / provider) to whatever the
      evaluate step accepts — a per-fid vector for the default — before the
      loop; the handle reaches the closures as an opaque object
      through the exported accessors only; construction validates arity and
      dry-runs the closures on a one-actor toy handle; the provider's latent
      path is written per event. Tests: a constant provider reproduces the
      plain `coef` path byte-for-byte; a toy two-regime provider (P1 only)
      and a toy evaluate step forming a per-actor row-wise product (PE, the
      goldfish.latent shape, defined inside the test) drive the loop on a
      seeded fixture, proving both hooks without goldfish carrying either
      variant. Evaluation path
      per D12: `walk_evaluate()` grows a `sender` argument (absent = today's
      full matrix, so the oracle tests are unchanged; supplied = that sender's
      row), and `.pse_eval_choice()` stops building the index `data.frame` its
      only caller discards, using parallel integer vectors as the R estimation
      backend already does. Retirement per
      ADR-0054: this task lands the consumer `materialize_process_state()`
      was reserved for and does not call it, so the function moves from
      `R/process_state_evaluators.R` to a `tests/testthat/helper-*.R` file
      as the batch-vs-replay oracle (header rewritten to say so); confirm
      `joint_simulation_parameters()` / `reconcile_joint_parameters()` are
      called here or retire them the same way
      — done 2026-09-15 (session `goldfish-23`) together with 2.2, commit
      `4712566` plus the materializer retirement; full `NOT_CRAN=true` green
      (8760 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not SKIP). Both
      joint-parameter helpers ARE called here, so neither is retired;
      `materialize_process_state()` is, to
      `tests/testthat/helper-process_state.R` (ADR-0054 — its reserved
      consumer landed on the live handle and does not call it). Result class
      `goldfishSim`, confirmed against the class-naming guard. **Two design
      gaps the task text assumed away, both settled in the design:** (a) a
      `goldfishFit` stores neither its specification nor its data, so the fit
      method takes `data =` and rebuilds the spec from the stored formula
      (vault ADR-0075; D1 corrected, the `fit-class-hierarchy` delta's reason
      corrected); (b) `walk_open()` refused the effect-free sub-models that
      completion installs, so NO completed spec could open a walk — it now
      takes `completed = "defer"`, and since dropping them renumbers the fids
      the driver matches walk to spec on the rendered process label (D5
      extended). `simulate(times = "observed")` and `simulate()` on a
      `goldfishFlavFit` abort as unavailable; they are 2.3 and the flavored
      run. The `goldfishSim` pool shape for `nsim > 1` is a plain list, as
      designed, until the augmenter's format lands
- [x] 2.2 Exponential free-running clock: total rate + exact exponential waiting
      time with breakpoint redraws (events, exogenous changes, expiries);
      stopping targets `horizon =`/`n_events =` (whichever binds first);
      `max_events` guard (default `10 * n_dep`) with the rate-trajectory early
      trigger and total-rate diagnostic; capped-replicate flagging; exogenous
      state frozen-and-warned past the last observed change
      — done 2026-09-15 (session `goldfish-23`) with 2.1, commit `4712566`.
      Landed together on the user's call: 2.1's own tests drive the loop on a
      seeded fixture, which needs a clock, so splitting them would have left a
      half-built clock at a commit. Breakpoint redraws read
      `walk_next_breakpoint()` and the provider's own `breakpoint`, which is
      what keeps the competing-exponential draw exact. **Not yet landed from
      this task: the rate-trajectory early trigger and the total-rate
      diagnostic** — the guard is the plain `max_events` cap, which flags
      `capped` and reports `stop_reason`; the trajectory trigger needs the
      threshold constants D3 leaves to fixtures and is carried into 2.8 with
      the regime record
- [ ] 2.1b ~~`simulate()` on a `goldfishFlavFit`~~ **Moved 2026-09-15 to
      `printing-homogenization` task 6.2a**: the one-competing-run method
      reassembles the flavored specification and its parameters through the
      flat, fid-ordered container surface that change lands
- [x] 2.2a One evaluate seam for every fid (added 2026-09-15, D11 amendment):
      each fid resolves its evaluate step by regime, read from
      `process_map$completed` and never from formula shape — `default`
      (log-linear, modeled), `constant` (a completed pinned rate,
      `exp(intercept_w)` over the risk set, reads no θ), `uniform` (a completed
      choice, equal weight over the risk set given the sender, reads no θ). The
      built-ins are the process-state evaluator on a degenerate zero-column
      state, not new samplers. Removes 2.1's walked/deferred branches in
      `rate_values()` / `mark_evaluation()`, `pinned_rate_values()` and
      `uniform_choice_values()`. Tests: an authored `rate = ~ 1` with a modeled
      choice simulates reading its intercept from `coef` (today it is looked up
      in `completed_rates`, evaluated at zero, and the run stops `no_rate`); a
      supplied `evaluate` still replaces `default` only
      — done 2026-09-16 (session `goldfish-f8`), commit `2bed81f`; full
      `NOT_CRAN=true` 8777 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not
      SKIP. `simulation_routing()` records the regime from `completed` and a
      step key per fid; `fid_evaluation_inputs()` resolves step and θ by it
      (`constant` reads the pinned intercept of the current period, `-Inf`
      when nothing is pinned); `simulation_fid_state()` gives every fid one
      path — its engine's state, or for a deferred fid the degenerate state
      (one intercept column / zero columns) beside its same-process sibling.
      Verified in a scratch run: a completed rate in a flavored gap, which
      the walk DOES compile, evaluates to exactly `exp(-4.274)` over its 12
      active senders under `constant`. Found on the way: before this task a
      flavored gap's walked completed choice already simulated correctly by
      accident (the parameter resolver handed it `numeric(0)`), but its
      regime read `modeled`
- [x] 2.2b A completed default's candidate space is its process's live support
      mask, not presence (D5 amendment 4): in a flavored gap the completed fid
      reads its own mask as a member of its family's unit; in the whole-family
      case the deferred fid reads its same-process sibling's live mask through
      the shared `constraint_id` (senders by the rowSums gate, receivers by the
      mask row and presence, self dropped on a one-mode layer). Tests: a
      flavored gap's completed dissolution choice draws only existing ties; a
      constrained rate-only process's completed choice never draws a receiver
      the mask excludes
      — done 2026-09-16 (session `goldfish-f8`), commit `43d52e4`; full
      `NOT_CRAN=true` 8783 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not
      SKIP. The flavored-gap half needed no code (the walked completed fid
      already reads its own mask through `walk_build_state()`); the
      whole-family half reads `walk_live_support()` on the sibling. Red check:
      with the previous presence-only state, 28 of 40 receivers fell outside
      the friendship mask; with this task, none. **Finding:** the walk
      registers only objects some formula term reads, so a rate reading only
      `outdeg(friendship)` left the focal `calls` layer unregistered and
      `walk_inject()` aborted `goldfish_walk_bad_event`. The test fixture
      adds `indeg` to give `calls` a term; the gap itself is the same class
      as `constraint-objects-on-shared-walk`'s and is not fixed here
- [x] 2.2c `default_mark()` carries the flavor's update value from
      `info$values_equivalence[[layer]]` (bug found 2026-09-15: the mark
      hardcodes `increment = 1`, so every simulated dissolution creates a tie).
      Test: a two-flavor creation/dissolution run where each dissolution
      removes an existing tie and the dissolution mask is honored
      — done 2026-09-15 (session `goldfish-f8`), commit `b2f0495`; full
      `NOT_CRAN=true` 8763 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not
      SKIP. `simulation_mark_updates()` builds a per-fid update once per
      replicate and the mark writes it under the layer's own semantics
      (`increment` ±1, `replace` 1/0). Confirmed first on the fixture: before
      the fix `8 → 9` was "dissolved" twice. An unflavored process keeps
      `increment = 1` exactly as before, which is still wrong for an
      unflavored `replace` layer; left out of scope and noted in progress.md
- [x] 2.2d Remove `walk_open(completed =)` and the refusal (D5 amendment 1;
      pre-2.0.0, no lifecycle). A flavored gap opens with nothing deferred. A
      whole effect-free family is stripped, the completed process_map is kept
      so fids keep their numbers, and `build_effect_union()` and every other
      map consumer skip rows no unit owns; the deferred fid stays on the
      handle's map and `walk_evaluate()` on it aborts naming it as a completed
      default. The driver's label routing and renumbering go
      — done 2026-09-16 (session `goldfish-f8`), commit `cb67ea7`; full
      `NOT_CRAN=true` 8770 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not
      SKIP. **The specification is not stripped at all**, contrary to the task
      text: `joint_fid_bundles()` numbers fids by position, so stripping
      bundles while keeping the map would have misnumbered every fid after
      the stripped family in a join (the explore check ran on a single
      process, where nothing follows). `walk_open()` instead compiles units
      for the walked families (`walk_compile_units()`) and hands them to
      `build_merged_blocks(units =)`, whose `plan_block_unions(fids =)` plans
      over the fids a unit owns; the batch path is unchanged. A layer left
      with no walked family aborts (`goldfish_walk_unsupported`), which is
      what an effect-free REM process reaches. `walk_evaluate()` on a
      deferred fid aborts `goldfish_walk_deferred_fid`. Also here: the
      default `n_events` / `max_events` now come from the opened walk's
      schedule, because `observed_dependent_count()` compiled the
      uncompleted specification and aborted on an authored `rate = ~ 1`
      before the run began. Tests: the join case keeps fids 1–4 and defers 2,
      and emails' choice (fid 4) evaluates exactly as it does with calls
      absent
- [x] 2.2e `times` derived from the specification (ADR-0076; D2 and D6
      amendments): `simulate(..., times = times_of(object))` with the
      exported accessor on specifications and fits, returning `"generated"`
      for a timed rate and `"observed"` for an ordered rate or no rate, with a
      `reason`. An explicit value equal to the default is silent; an override
      to `"observed"` messages; an override to `"generated"` on a choice-only
      DyNAM fires the completion warning naming the override and completes a
      constant exponential rate pinned at the crude rate (2.11's exposure once
      it lands); an override to `"generated"` on an ordered rate or a coordination process
      aborts saying why (ADR-0079). The result carries `times` and
      `times_source` and prints them. The accessor is `times_of()`
      — done 2026-09-16 (session `goldfish-f8`), commit `d4f4335`; full
      `NOT_CRAN=true` 8806 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not
      SKIP. `R/simulation_times.R`: `times_of()` with methods on
      `goldfishSpec`, `goldfishJointSpec`, `goldfishFit`, `goldfishFlavFit`
      and a refusing default; `resolve_simulation_times()` applies the D6
      table at entry (`goldfish_sim_times_override` message,
      `goldfish_sim_no_clock` abort); `complete_generative_spec(times =)`
      installs the pinned crude rate for a choice-only composition asked for
      `"generated"`, its warning naming the override. **The first full run
      failed two package guards** (8803 / 2 / 4): `test-descriptor_guards.R`
      caught the fit rebuild choosing its family with `sub_model %in%`, and
      `test-fit_class_reachability.R` caught `times_of()` missing on
      `goldfishFlavFit`. Both fixed: a fit's reason and family now come from
      its descriptor (`behavior_timing()`, `behavior_likelihood()`,
      `is_choice_family()`), and the flavored fit combines its processes'
      reasons. Two readings taken and flagged, not settled: "no rate" only
      when no rate exists anywhere (a timed join completes a choice-only
      process's rate, as the pinned-warning test already simulates), and a
      fourth reason `"coordination"` beside the three D6 names. Also here:
      `specification_from_fit()` carries the fit's resolved sub-model, so an
      ordered rate is not rebuilt timed and an REM fit is not routed to
      `choice =`. `pkgdown::check_pkgdown()` fails on a pre-existing stale
      topic (`logLik.result.goldfish`), not on `times_of`
- [x] 2.2f Refuse the effect-free model and stop reading completion from shape
      (D5 amendments 2 and 3): `simulate()` aborts at entry on a DyNAM whose
      authored rate is `~ 1` and which carries no choice (today it reaches the
      walk and fails with an internal "argument must be coercible to
      non-negative integer"); `mark_pinned_rates()` marks only `completed`
      rates, so an authored intercept-only rate is no longer warned "pinned,
      not estimated"
      — done 2026-09-15 (session `goldfish-f8`), commit `608eb8e`; full
      `NOT_CRAN=true` 8768 PASS / 0 FAIL / 4 SKIP, six baseline files PASS not
      SKIP. Abort class `goldfish_sim_no_effect`, snapshotted. The rule change
      moved exactly four tests in `test-intercept_only_rate.R`, all through
      the `completion_rate_bundle()` stand-in, which now carries the
      `completed = TRUE` mark it stands in for; nothing else changed there.
      **Both failure modes quoted above reproduce only when `max_events` is
      supplied**: left at its default, both authored-`~ 1` shapes died earlier
      in `observed_dependent_count()` with the parser's "A model needs at
      least one effect term" (fixed in 2.2d)
- [x] 2.2h The fit rebuild keeps the fit's right-hand side verbatim (added
      2026-09-16, D1 amendment): `specification_from_fit()` drops the
      response and keeps every term as written instead of rebuilding through
      `terms()` + `reformulate()`, which never writes an explicit `1`, and
      prepends `1` when `model_spec$has_intercept` is `TRUE` and the stored
      formula does not write it (a fit of `~ indeg` stores `calls ~ indeg`).
      Rewrite the stale comment claiming the legacy `fit$sub_model` reports an
      REM rate as `"choice"`: the family is read from the descriptor because
      that is where it lives, not to dodge a label no current fit carries.
      Tests: `simulate(fit, data =)` on a timed rate fitted from `~ indeg` and
      on an REM fit of `~ 1 + indeg` emits no intercept message (the 3.1a REM
      test gains that assertion, so it bites); a fit with an interaction
      written before a main effect simulates with its coefficients on the
      same terms
      — *scheduled 2026-09-16*: **before 2.8**, in the order 2.2h → 2.2i →
      2.2j → 2.2k → 2.2l. Independent of 2.8's replay; ahead of it so 2.8's
      tests run against the rebuilt fits, the horizon default, the
      flag-only guard and the pool
      — *done 2026-09-16* `469b79d` (goldfish-f0), 8830 / 0 / 4, six baseline
      files PASS. **Contradicts the verified fact** that no misalignment
      exists today: `fit$parameters` carries operand-only columns in written
      order (`Intercept, indeg, ego, outdeg, indeg:ego`) while the
      `reformulate()` rebuild compiled `Intercept, outdeg, indeg, ego,
      indeg:ego`, so `~ 1 + indeg:ego(floor) + outdeg` simulated with the
      `outdeg` estimate on `ego(floor)`. The interaction test (fit events ==
      specification events at `fit$parameters`, same seed) failed before the
      change for that reason. `1` is prepended on the leftmost operand
- [x] 2.2i With no target, a free-running run stops at the observed horizon
      (added 2026-09-16, D3 amendments, ADR-0083 and ADR-0084): when neither
      `n_events` nor `horizon` is given and `times = "generated"`, `horizon`
      defaults to `resolve_walk_extent(handle$merged, control_prep)$end` — the
      window end rate estimation uses: the last observed event, dependent or
      exogenous, window-expiry rows excluded, an explicit `end_time` taking
      precedence — replacing the observed-count default in
      `simulate_replicate()`; explicit `n_events` unchanged. `max_events` stays
      `10 * n_dep` over proposals. Lands the rate-trajectory early trigger and
      total-rate diagnostic 2.2 left unlanded (its "carried into 2.8" never
      reached 2.8's text), plus the clock-resolution case
      (`t + wait == t`). **No guard stop aborts** (ADR-0084): each ends its
      replicate only, keeps the events drawn, sets `capped = TRUE`, names the
      guard in `stop_reason` (`"max_events"` / `"rate_trajectory"` /
      `"clock_resolution"`) with the trajectory in `diagnostics`; targets
      report `"horizon"` / `"n_events"` (today both report `"target"`).
      Guard-stopped replicates stay in GOF (ADR-0085); nothing is excluded by
      default. The frozen-state reference moves from `last_exogenous_time()`
      (last covariate row, `-Inf` with none, window expiries included) to the
      same window end (D6 amendment): no warning inside the window, a warning
      once a run passes it. **One warning per call** at most, its bullets
      reporting guard stops by guard and runs past the window end with the
      freeze time, its classes `goldfish_sim_guard_stop` /
      `goldfish_sim_frozen_exogenous` as present. Update the
      `simulate()` roxygen, `NEWS.d/process-simulation--simulate.md`, and the
      test "a run with no target generates the observed event count". Tests:
      a timed fixture with no target stops at its last observed event (a
      fixture whose windowed effect places an expiry row past that event
      still stops at the event) with a count that differs across seeds; the
      REM fit of `~ 1 + indeg` on `social_evolution` (`ideg` 1.15, which today
      reaches `max_events` 4390 on five of five seeds with 3951 events at one
      instant) returns flagged replicates instead of stamped ones; an
      `nsim > 1` call with one runaway replicate returns every replicate and
      warns once; explicit `n_events` still returns exactly that count; a
      default run on a fixture whose covariates stop changing before the last
      event, and on a windowed fixture, emits no frozen-state warning, while
      an `n_events` run past the window end on `nsim = 3` emits exactly one
      warning naming both conditions when both occur
      — *scheduled 2026-09-16*: **before 2.8**, in the order 2.2h → 2.2i →
      2.2j → 2.2k → 2.2l. Independent of 2.8's replay; ahead of it so 2.8's
      tests run against the rebuilt fits, the horizon default, the
      flag-only guard and the pool
      — *done 2026-09-16* `878e267` (goldfish-f0), 8874 / 0 / 4, six baseline
      files PASS. Trajectory thresholds (ADR-0088, proposed): total rate >
      1000 x its value at the first drawn event, or median of the last 50
      waits < observed mean wait / 1000. Measured on `social_evolution`,
      six seeds to the window end: bounded fits peak at 22.6x and keep the
      median wait >= 0.041 of the observed mean; every degree-effect rate fit
      (REM `indeg`, `outdeg`; DyNAM `indeg`, `outdeg`, `indeg + outdeg`) hits
      `max_events` on every seed and crosses both by its 1500th event. The
      REM `~ 1 + indeg` run now stops at `rate_trajectory` after ~30 events.
      `diagnostics` gains `end_time`, `window_end`, `trajectory`; the capped
      print names the guard and stop time
      — *2026-09-17*: the default thresholds above are **rejected**
      (ADR-0088 rejected; ADR-0089). The rate-trajectory trigger becomes
      opt-in and the guards move into `set_simulation_guard()`: task 2.2m
- [x] 2.2j Collect simulated events into pre-allocated columns (added
      2026-09-16, D1 amendment): replace the per-event one-row `data.frame`
      and final `rbind` (about 130 us per event, 10% of a 439-event run) with
      one typed vector per output column plus a latent list, allocated once
      at capacity `min(n_events, max_events)` when `n_events` is the target,
      else `max_events`; write by index; at the end cut to the drawn count and
      set class `data.frame` and compact row names on the list. Above a
      documented byte budget, start at the observed dependent count and
      double. Output columns and types unchanged. Tests: the frame is
      identical (`expect_identical`) to the current collector's on a seeded
      run; a zero-event run returns the typed empty frame; a run stopping
      short of capacity is cut to its count
      — *scheduled 2026-09-16*: **before 2.8**, in the order 2.2h → 2.2i →
      2.2j → 2.2k → 2.2l. Independent of 2.8's replay; ahead of it so 2.8's
      tests run against the rebuilt fits, the horizon default, the
      flag-only guard and the pool
      — *done 2026-09-16* `e520894` (goldfish-f0), 8881 / 0 / 4, six baseline
      files PASS. Characterization fixture `fixtures/simulated_events.rds`
      (from `make-simulated-events.R`, captured on the per-row collector)
      reproduced identically on the default and the doubling path; the budget
      is 64 MB (`sim_collector_byte_budget()`). A 439-event DyNAM run: median
      0.239 s -> 0.188 s
- [x] 2.2k `nsim > 1` returns a `goldfishSimPool` (added 2026-09-16, D1
      amendment, ADR-0086 and ADR-0085): class `c("goldfishSimPool",
      "list")` with `[` keeping the class; a per-replicate summary
      (`replicate`, `n_events`, `end_time`, `stop_reason`, `capped`,
      `n_proposals`, `acceptance_rate`) returned by `summary()`; a print in
      the joint specification's layout (`cli_rule()` header, `·` count line,
      process lines with `render_process_label()` and fid, events-per-replicate
      min / median / mean / max, stop-reason counts, guard alert, closing `i`
      hint), rendered with cli and pinned by a snapshot under a reproducible
      cli context; the exported `filter_simulation(pool, ...)` (ADR-0087):
      conditions evaluated against the summary and combined with AND,
      returning the kept replicates' pool with original replicate numbers,
      aborting on an unknown column (listing the summary's columns) or a
      condition that is not one logical per replicate. `@return` of
      `simulate()`, roxygen, `devtools::document()`, a
      `NEWS.d/process-simulation--simulate.md` bullet. Tests: print snapshot
      for `nsim = 3`; `pool[[2]]` prints one replicate; `lapply()` and
      `length()` work; `summary()` columns and types; a filter keeping
      replicates by stop reason and by event count, the source pool unchanged;
      two conditions combine with AND; an unknown column aborts with its
      snapshot; a filter keeping nothing returns an empty pool that prints
      — *scheduled 2026-09-16*: **before 2.8**, in the order 2.2h → 2.2i →
      2.2j → 2.2k → 2.2l. Independent of 2.8's replay; ahead of it so 2.8's
      tests run against the rebuilt fits, the horizon default, the
      flag-only guard and the pool
      — *done 2026-09-16* `496a702` (goldfish-f0), 8904 / 0 / 4, six baseline
      files PASS. New `R/simulate_pool.R`; the summary is computed from the
      replicates, not stored. A condition's `NA` drops the replicate; a name
      that is neither a column nor visible from the caller aborts
      (`goldfish_sim_bad_filter`). ADR-0086's open question (end time on the
      events line) left open: not printed
- [x] 2.2l `test_gof()` names its replicate count `nsim` (added 2026-09-16,
      ADR-0087): rename the `n_sim` argument of `test_gof.goldfishFit()` and
      `test_gof.goldfishFlavFit()` to `nsim`, matching `stats::simulate()`,
      whose argument goldfish's `simulate()` methods must keep. `test_gof()`
      is post-v1.7.0 and in no tag, so the rename is outright, with no
      lifecycle deprecation. Covers the `check_replication_count()` message
      (`{.arg nsim}`), the stored `params$n_sim`, internal helpers
      (`gof_simulated_p()`, `gof_block()`), the roxygen `@param`,
      `devtools::document()`, `tests/testthat/test-test_gof.R` and its
      snapshot (re-accept the three changed blocks, review the diff),
      `vignettes/diagnostics.Rmd.orig` then regenerate the precompiled
      `.Rmd`, and a NEWS.d bullet. Both methods take `...` and never check it
      (`R/test_gof.R`), so a leftover `n_sim = 200` would be swallowed and the
      test would run silently at the default 1000: the methods check their
      dots (`rlang::check_dots_empty()` on `goldfishFit`; on
      `goldfishFlavFit`, whatever it forwards to the components, checked
      there). Tests: `test_gof(fit, nsim = 100)` runs with 100 replications;
      `test_gof(fit, n_sim = 100)` aborts naming `n_sim`, pinned by a snapshot
      — *scheduled 2026-09-16*: **before 2.8**, in the order 2.2h → 2.2i →
      2.2j → 2.2k → 2.2l. Independent of 2.8's replay; ahead of it so 2.8's
      tests run against the rebuilt fits, the horizon default, the
      flag-only guard and the pool
      — *done 2026-09-16* `c9a31e3` (goldfish-f0), 8907 / 0 / 4, six baseline
      files PASS. Both methods call `rlang::check_dots_empty()`; the snapshot
      names `n_sim = 100`. Regenerating `diagnostics.Rmd` also redrew its
      unseeded information-clock p-values (recip 0.0199 -> 0.0348) and the
      fit-size notes; the prose quotes none of them
- [ ] 2.2m The guards are one object, and early stops are opt-in (added
      2026-09-17, D3 amendment, ADR-0089; ADR-0088 rejected): an exported
      `set_simulation_guard(max_events = NULL, rate_multiple = Inf,
      wait_collapse = Inf, wait_window = 50L, clock_resolution = 0)`
      returning a classed `goldfishSimGuard`, validated at construction
      (`max_events` NULL or one positive whole number; `rate_multiple` and
      `wait_collapse` one number >= 1, `Inf` allowed; `wait_window` one
      positive whole number; `clock_resolution` NULL or one number >= 0; each
      refusal a cli abort with a condition class), passed to `simulate()` as
      `control_sim = set_simulation_guard()`. Remove the `max_events` argument
      from `simulate()` outright (in no release). The driver reads every
      guard from the object: `rate_multiple` and `wait_collapse` at `Inf`
      never fire (replacing the 2.2i constants `SIM_RATE_MULTIPLE`,
      `SIM_WAIT_COLLAPSE`, `SIM_WAIT_WINDOW`); `clock_resolution = r` stops a
      replicate when a step moves the clock by no more than `r` times the
      window length (`resolve_walk_extent()` end minus start), so `r = 0` is
      the current `t + wait == t` test and `NULL` turns the stop off. The
      measurement comment in `R/simulate_driver.R` goes with the constants.
      Roxygen for the new function and `simulate()` (`control_sim`, the
      explosion-guard paragraph, `@param max_events` removed),
      `devtools::document()`, `_pkgdown.yml`, a `NEWS.d` bullet, and the 2.2i
      sentence "Guards stop a replicate whose rate runs away, whose clock
      stops advancing, or that reaches `max_events`" corrected. Tests: the
      REM fit of `~ 1 + indeg` on `social_evolution` with the default guard
      stops at `clock_resolution` (not `rate_trajectory`); a runaway whose
      clock keeps advancing reaches `max_events` flagged (REM `~ 1 + outdeg`
      at its estimates, or a lighter fixture that does the same); a finite
      `rate_multiple` stops it at `rate_trajectory`; a finite `wait_collapse`
      stops it at `rate_trajectory`; `clock_resolution = NULL` lets a stalled
      clock run on to `max_events`; a positive `clock_resolution` stops
      earlier than `0`; construction refusals pinned by snapshot; the tests
      that pass `max_events =` to `simulate()` move to `control_sim`
      — *scheduled 2026-09-17*: **before 2.8**. Queue: 2.2m → 2.8 → 2.8b →
      2.3
- [ ] 2.8 Per-component regime record (modeled / completed / anchored-replay) on
      `process_map` and print; replay coherence guard (skip-and-count, never
      clamp). Unmodeled flavors of a relational layer take `anchored-replay`
      by default (ADR-0055): their observed events enter the walk schedule as
      rows the driver never draws, right-censoring the timed engines; the
      per-flavor override keeps the explicit flag. Test: the Fisheries shape
      (creation modeled, dissolution in neither list) simulates with no
      completion warning, replays every dissolution, and reports the skip count
      — *moved ahead of 2.3 on 2026-09-15*: 2.3 anchors per flavor through this
      record, and the interim abort 2.2g is dropped. The replayed rows are the
      focal layer's update rows whose value `info$values_equivalence[[layer]]`
      maps to an unmodeled flavor (the mapping 2.2c adds to the mark); as
      exogenous rows they are breakpoints, so the clock redraws at each one,
      which is the right-censoring with no mechanism of its own. The regime
      reads `process_map$completed` (2.2a)
      — *scheduled 2026-09-16*: **after 2.0f**, which registers the focal
      layer and appends its covariate stream. The replayed rows are that
      stream's rows, so 2.8 selects which of them to apply rather than
      inventing a stream for them; what stays 2.8's own is `exo_rows`, which
      today excludes every focal layer wholesale
      — *re-scheduled 2026-09-16 (later)*: after **2.2h → 2.2i → 2.2j →
      2.2k → 2.2l**. Queue: 2.2h → 2.2i → 2.2j → 2.2k → 2.2l → 2.8 → 2.8b
      → 2.3; *2026-09-17*: 2.2m inserted before 2.8 (queue 2.2m → 2.8 →
      2.8b → 2.3)
- [ ] 2.8b Incoherence flag past a documented threshold of skipped replayed
      events (split from 2.8 on 2026-09-15): the threshold constant is the
      design's open `[surface]` question, settled against fixtures
- [ ] 2.3 Time-anchored variant on every family (`times = "observed"`): marks
      redrawn at observed stamps from the fitted conditionals; per-flavor
      anchoring through the regime record
- [ ] 2.4 ~~Weibull/Gompertz free-running clocks~~ **Moved 2026-09-15 to
      `parametric-rates` task 3.4a** (ADR-0074): the clocks are `clock`
      steps owned by the change that owns the distribution axis; this
      change ships the exponential clock and the `clock` plug point they
      plug into, and 2.2's breakpoint contract is what they honor.
- [ ] 2.5 Cox/ordered strategies, keyed on `behavior$timing == "ordinal"`
      (rewritten 2026-09-16, ADR-0079): time-anchored only, no pseudo-time.
      `times = "generated"` on an ordered rate aborts stating that the ordered
      rate estimates no clock; the default resolves to `"observed"` through
      `times_of()`. Test: a Cox specification simulates at its observed stamps
      without a `times` argument and aborts, naming the ordered rate, with
      `times = "generated"`
- [ ] 2.6 Coordination: the conjunctive mutual-choice draw only, time-anchored
      from the mark multinomial (free-running rejection withdrawn 2026-09-16,
      ADR-0079), through the `mark` plug point. ~~The other four mechanisms~~ **Moved 2026-09-15 to
      `two-sided-coordination` task 2.5a** (ADR-0074): per-mechanism mark
      kernels are `mark` steps owned by the change that defines the
      mechanisms; its 2.5 fixtures seed them.
- [ ] 2.7 Windowed effects in free-running simulation (after 2.0c; revised
      2026-09-15, D8): the handle fans an injected source-layer event out to
      each derived `layer_ω` (entry row now, expiry scheduled through
      `walk_schedule()` at `t + ω`, lengths from the unit's plan
      derivations), so a driver injects once; no FIFO structure. Tests:
      eager-recompute agreement on a windowed fixture; two window lengths on
      one layer expire in time order regardless of entry order; an expiry is
      a breakpoint for the clock.
- [ ] 2.9 Flavored/multivariate competing-flavor draws under the live derived
      masks from 2.0a; evaluator-compatible pool output carrying capped flags +
      regime record (a plain list of `goldfishSim` until the
      `dynes-augmentation` pool format lands); optional writer-sink statistics
      recording; `devtools::document()`
- [ ] 2.10 Multi-period relational `|R_w|` slicing (owned here per
      `make-multivariate-spec` D9a — the one genuinely new piece the panel/DyNES
      path did not need): for a **relationally-observed** flavor completed with a
      pinned intercept-only timed rate inside a **wave-gridded** join, slice the
      preprocessed presence walk per inter-wave period to a per-period
      time-weighted `avg_active_entity = (1/T_w)·∫|R_g(t)| dt`, so each plateau's pin
      `log(count_w / (T_w · |R_w|))` uses its own period's risk-set size (the single
      window K=1 case reuses the preprocessed `avg_active_entity` unchanged, already
      served upstream). Period membership follows the half-open
      `findInterval(t, wave_times, rightmost.closed = TRUE)` convention. Tests: a
      multi-period relational fixture reproduces each period's `count_w`; single-window
      reduction matches the upstream value byte-for-byte

- [ ] 2.11 Exposure integral on the merged walk (added 2026-09-15, D5): the
      walk accumulates, per timed fid, `∫|R_w(t)| dt` — the sum over
      intervals of `dt × |active ∩ gated senders|`, presence changes applied
      at their stamps — and stores it beside `total_time` on the
      `goldfishStat` (`exposure_actor_time`); `avg_active_entity` for an
      exact-time family becomes `exposure_actor_time / total_time` (the
      time-weighted value the `intercept-only-rate` and
      `multivariate-specification` living specs already require, in place of
      `assemble_default_output()`'s event-averaged count); the pinned
      completion `log(count_w / exposure_w)` and the intercept seed read it;
      the panel path keeps its period average. Spec delta: MODIFIED
      `active-availability-stat` "avg_active_entity declared by the recipe
      constructor". Frozen baselines PASS (the scalar seeds the intercept
      only); the completion-warning snapshots on composition-changing
      fixtures re-recorded and reviewed.
      *Note 2026-09-15 (explore):* the walk accumulates exposure per timed fid,
      and a flavored layer's fids are per flavor, so this is also the
      flavor-aware relational risk set `pin_completed_rates()` lacks — it
      routes a flavored relational layer to the panel wave-Hamming path as a
      stopgap, and `per-family-flavor-modeling`'s delta pins such a gap to a
      zero hazard. When this lands the flavored relational pin reads it;
      coordinate that delta's wording with its owner

## 3. Tests and documentation

- [ ] 3.1 Tests (testthat 3e): seeded intensity/event-count sanity on timed
      fixtures, anchored-times preservation, parametric-shape recovery,
      acceptance-rate fixtures per mechanism, trajectory-trigger and explosion
      abort snapshots, capped-replicate exclusion, skip-and-count on a
      misfitting replay fixture, simulate→evaluate round trip at generating vs
      perturbed parameters, batch-vs-replay consistency against the walk handle
      (including a windowed fixture)
- [x] 3.1a Fitted-model round trip for the families no test simulates (added
      2026-09-16): `simulate()` on a fitted model rebuilds the specification
      from the fit's stored formula, and 2.2e made that rebuild read the
      family from the descriptor and carry the fit's resolved `sub_model`.
      Before it, an **REM** fit was rebuilt with `choice =` (the legacy
      `fit$sub_model` reports an REM rate as `"choice"`), which
      `make_specification()` refuses; an **ordered** rate was rebuilt as a
      timed one and would have simulated a clock the fit never estimated.
      Both paths are unguarded — the only fitted-model test simulates a
      DyNAM rate-only fit. Tests: `estimate_rem()` then `simulate(fit,
      data =)` draws events on the focal layer (verified by hand 2026-09-16,
      4 events on the walk fixture, `times_of(fit)` = `"generated"`); a
      `rate_sub_model = "rate_ordered"` fit rebuilds ordered, so
      `times_of(fit)` is `"observed"` with reason `"ordered rate"` and
      `simulate(fit, times = "generated")` aborts `goldfish_sim_no_clock`
      rather than generating one
      — *scheduled 2026-09-16*: **first**. It depends on nothing, touches only
      tests, and guards a fix that shipped in 2.2e with no test behind it, so
      it rides the next `NOT_CRAN=true` run rather than earning one of its own
      — *done 2026-09-16* `6e21357` (goldfish-f0), 8813 / 0 / 4, six baseline
      files PASS. The ordered test bites: with the rebuild reverted to the
      legacy `fit$sub_model` reading it errors. The REM test does not — no
      reachable REM fit reports `"choice"` today (a deprecated
      `estimate_rem(sub_model = "choice")` is rewritten to `"rate"`), so it
      guards the REM round trip rather than that regression
- [ ] 3.2 `simulate()` reference + a simulation section in the model-usage
      vignette (the `times` axis, per-distribution clocks, per-mechanism
      coordination, which GOF statistics need which variant);
      `devtools::document()`
- [ ] 3.3 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (simulation milestone);
      commit

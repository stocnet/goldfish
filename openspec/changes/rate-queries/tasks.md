> **Depends on `abmcem`** (un-archived, actively edited): group 5 amends
> `make_proposal_evaluator()` in `R/evaluator_dynes.R` — its body and one new
> optional `spec` argument — not its already-pinned named-list return contract
> (abmcem design D3, task 4.3). Land after abmcem's task 4.3b is on the branch
> (it already is).

> **TDD.** Implementation tasks below run as red/green pairs — an "(a)" task
> writes a failing test and commits it RED, the paired "(b)" task makes it
> pass and commits GREEN (per `openspec/config.yaml` `rules.tasks`).

> **Changelog.** Everything here is internal (no export, no user-facing
> output), so no `NEWS.d/` fragment is written, and on this feature branch
> there is no `DESCRIPTION` bump, root `NEWS.md` edit or archive (deferred to
> the merge, `rules.merge`).

## 1. Schedule-position replay driver

- [x] 1.1a Write failing tests for `walk_replay_to(handle, row)` (design D2):
      replay to a candidate event's realization row applies earlier candidate
      events and nothing after the requested row; a candidate event tied with
      an exogenous row's stamp is read with that row NOT applied at its
      dependent row; a second call to a later row applies only the rows in
      between and equals a fresh replay. Run to confirm RED; commit the tests
      alone.
- [x] 1.1b Implement `walk_replay_to()` in new file `R/rate_queries.R` by
      promoting the loop of `walk_replay_against_batch()`
      (`tests/testthat/test-walk_handle.R`): exogenous rows one at a time via
      `walk_advance()`'s per-row steps (not `walk_advance()` itself, which
      overshoots same-stamp rows — design D2 amendment), focal realization
      rows via `walk_inject()`, dependent
      rows via `walk_advance_presence()`; track the resume cursor on the
      handle. No changes to the `walk_*()` primitives. Re-run 1.1a; commit.
- [x] 1.2a Refactor test: rewrite `walk_replay_against_batch()` to step through
      `walk_replay_to()` instead of its own loop, and add a batch-consistency
      case over a fixture with a stamp tie between a dependent event and an
      exogenous row. Run the walk-handle suite; the refactored helper must
      stay GREEN on every existing case — only the new tie case may be RED if
      1.1b missed it. Commit.
- [x] 1.2b Close any gap 1.2a reveals in `walk_replay_to()` (expected: none —
      a real gap here means revisiting design D2, not special-casing the
      fixture). Re-run; commit.
- [x] 1.3 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      commit.

## 2. Candidate binding and parameter mapping

- [x] 2.1a Write failing tests for `bind_candidate_data(spec, candidate)`
      (design D6): a candidate differing only in its ties is bound at the joint
      level and on every process specification, with `process_map` and
      `completed_rates` unchanged; a candidate whose `info` differs aborts with
      a classed error naming `info`; likewise for `nodes`. Run to confirm RED;
      commit the tests alone.
- [x] 2.1b Implement `bind_candidate_data()` in `R/rate_queries.R`: identity
      checks on `info` and `nodes`, then assign the data. No construction or
      completion re-run. Re-run 2.1a; commit.
- [x] 2.2a Write failing tests for the flat-to-per-fid theta mapping (design
      D7): built from `coef_layout()` on a joint spec with an offset term and a
      timed rate, each walked fid's vector has the fixed values in place, the
      free slots from `theta[index]`, and the length `walk_evaluate()` accepts;
      sub-model `theta_index` positions that do not match the layout's free
      `index` set abort naming the mismatch. Run to confirm RED; commit.
- [x] 2.2b Implement the mapping and the layout check in `R/rate_queries.R`
      over `coef_layout()`. Re-run 2.2a; commit.
- [x] 2.3 Closing: full `NOT_CRAN=true` run; commit.

## 3. Pair intensity

- [x] 3.1a Write failing tests for `pair_intensity()` (design D3): a DyNAM pair
      equals the rate fid's sender entry times the choice fid's receiver
      probability, each matching `walk_evaluate()` at the same state; a REM
      dyad equals its rate entry; a process whose choice is a completed uniform
      default and one whose rate is a completed intercept-only rate both
      return a value (no abort); an excluded pair returns exact `0`; an
      unknown label, an unknown process and an ordered-regime process each
      abort with a classed error naming the query. Run to confirm RED; commit.
- [x] 3.1b Implement `pair_intensity()` in `R/rate_queries.R` over
      `evaluate_fid()` / `fid_evaluation_inputs()` / `simulation_routing()` /
      `paired_choice_fid()` (`R/simulate_driver.R`), choice evaluated with
      `sender =`, entries read by closed-form index. Resolve labels through the
      engine's `node_lookup` (`layer_node_lookup()`, set in
      `build_walk_engine()`), reached via the sibling fid's engine for a
      deferred fid: sender on `side == 1`, receiver on `side == 2` for a
      two-mode layer and `side == 1` otherwise. Re-run 3.1a; commit.
- [x] 3.2 Closing: full `NOT_CRAN=true` run; commit.

## 4. Query entry point

- [x] 4.1a Write failing tests for `query_rate_at_position()` (design D4): the
      handle is opened once for a batch of queries at several positions (mock
      `walk_open()` and count), and each schedule row is applied at most once;
      results return in the caller's original order when submitted out of
      position order; position `after = k` excludes an exogenous row sharing
      event k's stamp that follows it; two tied candidate events give two
      distinct positions; a single query equals `pair_intensity()` after a
      manual `walk_replay_to()` to the same row. Run to confirm RED; commit.
- [x] 4.1b Implement `query_rate_at_position(spec, theta, queries,
      control_prep = set_preprocessing())`: map each `after = k` to its
      schedule row, order queries by row, one `walk_open()`, then
      `walk_replay_to()` + `pair_intensity()` per distinct position;
      reassemble in original order. Internal: no roxygen export. Re-run 4.1a;
      commit.
- [x] 4.2a Write a failing test for pinned-rate invariance (spec scenario
      "completed rates do not move with the candidate"): the same query against
      a completed intercept-only rate, under two bound candidates differing
      only in augmented event times, uses an identical pinned intercept. Run to
      confirm RED (or GREEN if 2.1b + 4.1b already hold it — then commit it as
      a regression pin and say so). Commit.
- [x] 4.2b Close any gap 4.2a reveals. Re-run; commit.
- [x] 4.3 Closing: full `NOT_CRAN=true` run; commit.

## 5. Wire `make_proposal_evaluator()`'s `rate_queries`

- [x] 5.1a Write failing tests (in `tests/testthat/test-evaluator_dynes.R`,
      alongside abmcem's existing `make_proposal_evaluator()` tests): built with
      a completed joint `spec`, a non-`NULL` `rate_queries` returns `rates`
      equal to `query_rate_at_position()` at the mapped parameters and does not
      abort (replacing the abort-pinning test and snapshot abmcem task 4.3a
      left); without `spec`, a non-`NULL` `rate_queries` aborts naming the
      missing argument; a sub-model/layout mismatch aborts at construction; a
      `NULL`/omitted `rate_queries` still returns `rates = NULL` and opens no
      walk (a regression pin — already GREEN). Run to confirm RED on the new
      assertions; commit the tests alone.
- [x] 5.1b Implement: add the optional `spec = NULL` argument to
      `make_proposal_evaluator()`, run the layout check at construction when
      `spec` is supplied, and replace the closure's `rate_queries` abort with
      `bind_candidate_data()` + `query_rate_at_position()`. Update the
      closure's header comment, which today says the query "needs a `what`
      extension". Re-run 5.1a; commit.
- [x] 5.2 Final verification: full `NOT_CRAN=true` suite (frozen baselines PASS
      not SKIP); `devtools::document()` only if roxygen was touched; commit.

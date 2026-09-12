# Tasks — observation-tail-right-censoring

**Review-first, detector before fix.** Every cell of the matrix in design D3
that targets the defect is written and shown to FAIL on the current tree before
any code moves; a cell that passes the moment it is written proves only that its
path was already correct, and is recorded as a pin, not a fix. `NOT_CRAN=true`
with the frozen 1e-6 coefficient and C++ golden baselines PASS not SKIP at every
commit. Decide every test verdict from the numeric failed count
(`sum(as.data.frame(testthat::test_file(f, reporter = "silent"))$failed)`), never
from a reporter's printed list: `create_effects_functions()` warns on every
windowed effect and the `support_constraint` validations warn by design, and a
warning is not a failure.

## 0. Ground the invariant and the fixtures

- [ ] 0.1 Write the invariant down in `progress.md` in code terms, from design
      D1: with no explicit `end_time`, the resolved window closes at the last
      non-window-derived event; a window-derived (`window_derived` /
      `is_window_effect`) row or a composition-change row past it updates state
      and emits no right-censored observation; an explicit `end_time` past the
      last event counts exposure for the exact-time families and stores no
      trailing row for the multinomial families (unchanged).
- [ ] 0.2 Inventory where the resolved end is computed and where a right-censored
      row is emitted, per substrate: the recipe loops' clip branch
      (`prepare_recipe_context()` `hasEndTime` / the loop's `final_step`), the
      merged walk (`resolve_walk_window()` / `resolve_walk_extent()` /
      `run_merged_walk()`), and the walk handle (`walk_open()` /
      `walk_apply_object_event()`, which computes no end today). Record the exact
      sites so the matrix's assertions read them, not a proxy.
- [ ] 0.3 Build the fixture set the matrix needs and confirm each exercises the
      tail it targets: a windowed term whose expiry falls strictly past the last
      dependent event; a composition change (`active_1` and `active_2`) after the
      last dependent event; a windowed support-constraint atom; a real exogenous
      covariate after the last dependent event (for D2). State for each what its
      last real event time is and what its last scheduled row time is, so a
      trailing row is unambiguous.

## 1. The detector matrix (design D3) — build red, measure D2

- [ ] 1.1 Enumerate the meaningful cells of the D3 product and, for each,
      write the assertion: relationally (merged / handle output equals the
      recipe loop's, stripped of decoration) and absolutely (no stored row past
      the resolved end unless an explicit `end_time` licenses it, and the
      exact-time trailing row — when licensed — carries no borrowed identity).
      Mark impossible or already-covered cells (multinomial stores no trailing
      row; walk handle refuses a user constraint) with the reason, not a skip
      that reads as a gap.
- [ ] 1.2 Run the matrix on the current tree and record, per cell, PASS or the
      exact divergence (as tasks 1.6-1.9 recorded "8 stored events vs 6,
      total_time 7 vs 5"). The windowed rate cell on the recipe loop and the
      walk handle are the first suspects, since only the merged walk's was
      fixed. A cell that fails is a fix in group 2; a cell that passes is a pin.
- [ ] 1.3 Measure D2: on the real-exogenous-tail fixture, an exact-time rate fit
      with the resolved end (which today includes the exogenous event) against
      the same fit with the window closed at the last dependent event. Record
      the coefficient difference and the `total_time` difference in `.plan/` and
      `progress.md`. This is the number Alvaro's D2 decision reads; do not decide
      it here.
- [ ] 1.4 Verification: `NOT_CRAN=true` green (the matrix's failing cells are
      new tests marked with their expected-red reason until group 2; keep them
      out of the suite's failure count with `skip()` carrying the divergence, or
      hold them in a separate file the group-2 commits move into place — state
      which, so a red cell is intended, not a broken commit). Baselines PASS.

## 2. Close each violation the matrix found (only the failing cells)

- [ ] 2.1 For each failing cell from 1.2, fix it in the substrate's extent /
      emission logic, not by filtering rows after the fact, so the walk handle
      and the recipe loop inherit the same bound the merged walk got in 1.7.
      One commit per substrate-and-family cause (a single root may fix several
      cells; say which cells each commit turns green). The relational assertion
      (equals the recipe loop) and the absolute assertion (no row past the
      resolved end) both go green; baselines unchanged, because a spurious
      trailing row on a windowed/constrained model is not in any frozen baseline.
- [ ] 2.2 Re-examine the two `constraint-availability-encoding` scope-outs
      against the matrix (design D4): the frozen `active_1` sender axis and the
      frozen REM receiver axis. Fix here only the cell where a frozen axis moves
      a stored row or interval at the tail; if a frozen axis changes only a
      warning's completeness, record the measurement and hand it to a dedicated
      composition change, naming it. Do not widen scope past the tail.
- [ ] 2.3 Verification: `NOT_CRAN=true`, baselines PASS not SKIP; every group-1
      cell now green or explicitly pinned; the walking-time gate cells
      (rate-only single-family and CollegeMsg 10k, base and constrained) within
      3 percent of the recorded reference — a tail fix must not add per-event
      work that scales with the risk set.

## 3. Settle D2 and fold the result

- [ ] 3.1 Put D2 to Alvaro with group 1.3's number. Record the answer as an ADR
      (claim the id in the vault `decisions/_id-ledger.md` before drafting), with
      the rejected side, and cite the ADR id here and in `proposal.md`.
- [ ] 3.2 If D2 changes today's behavior (close at the last dependent event):
      implement it, and re-freeze every exact-time rate baseline whose formula
      has a trailing real exogenous event — under ADR-0021, deriving the moved
      `Intercept` from the dropped exposure first and committing the derivation
      beside the baseline diff. Enumerate the exposed baselines before editing;
      confirm the coordination / multinomial and cpp goldens are untouched with
      `git diff --stat` over `_baselines/`. If D2 is "keep", this task is a
      one-line record that no baseline moved and why.
- [ ] 3.3 A `NEWS.d/` fragment only if a user-visible number changed (a moved
      baseline is user-visible; a pure detector-matrix + pinned-scenarios outcome
      writes none). No `NEWS.md` / Version edit on the branch (ADR-0040).

## 4. Close

- [ ] 4.1 Confirm the tightened `observation-window` requirement is met on all
      three substrates and every family, and the `support-constraint` presence-at-
      the-tail scenario holds, by pointing at the matrix cell that proves each.
- [ ] 4.2 `bash .plan/opsx-spec-placement-check.sh observation-tail-right-censoring`
      clean; `openspec validate observation-tail-right-censoring --strict` clean.
      On a D-stop (no code fix), trim the spec deltas to the scenarios that landed.
- [ ] 4.3 Final verification: full `NOT_CRAN=true` suite green; baselines and
      goldens PASS not SKIP; `devtools::document()` if any roxygen changed. Note
      the outcome for the neighbors: `process-simulation` (the walk handle's tail
      for `simulate()`), and any composition change D4 handed a scope-out to.

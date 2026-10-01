---
depends-on: constraint-objects-on-shared-walk
---

## Why

**Estimation is tested where someone once had a reason to look, not across
the space the package claims to cover.** A coverage survey on 2026-10-01
(sessions goldfish-ce and goldfish-0c) laid the existing tests over the
estimation contract, the six model families by three backends under
composition change, support constraints of every kind, flavored layers and
the deprecated opportunity list, and found estimates asserted in a handful
of cells: constraint-only on static data for rate, choice and REM on all
three backends; composition plus constraint only for coordination; the
uncovered-object constraint only for the rate on cpp. Nothing asserts an
estimate for a flavored layer with a user constraint (a live probe in that
survey found it correct on all three backends, to 4e-16, and untested),
for composition plus constraint on DyNAM rate, choice or REM, for any
flavored fit on the r or gather backend, for two-mode data with a
constraint, or for gather and database row filtering outside the choice
model. No frozen baseline carries a constraint or a flavor. ADR-0006,
ADR-0010 and ADR-0019 each name composition change plus a support
constraint as the case the closed-form shortcuts get wrong, and that triple
has one test, on coordination.

`constraint-objects-on-shared-walk` built the instrument that makes the
rest affordable: a seeded data-generating harness that simulates exact-time
sequences and writes the oracle frames with the process's own risk-set
rule, and five fixtures (F1 to F5) proving goldfish equals `glm`, `clogit`
and `mlogit` on them. Those five are the fixtures its design needed; they
are not a coverage argument.

The full factor space is 2160 combinations, 476 of them valid under what
the package refuses or cannot build. Pairwise Independent Combinatorial
Testing (PICT, Microsoft's tool, 3.7.4 built from source at `~/.local/bin/pict`)
covers every pair of factor values in 30 cases; seeding it with every
valid composition x constraint x flavor triple, the one interaction the
ADRs name, gives 37 cases with that triple exhaustive. Seven cases buy the
interaction a pairwise set would only sample; a PICT sub-model for the same
triple came out at 112 and full order 3 at 95, so seeding is the mechanism.

## What Changes

- **The estimation contract is stated once and applied to every case.** A
  case is one row of the matrix: a family, a backend, one- or two-mode
  data, static or changing composition, a constraint kind (none, static
  dyadic, dynamic covered, dynamic uncovered, ego-only), a flavor shape
  (none, two-flavor container, single modeled flavor) and whether the
  deprecated opportunity list is used. Every case asserts the same arms:
  (a) goldfish's fit equals the classical oracle on the hand-built frame
  within 1e-6 where an oracle exists (Poisson `glm` with a log-exposure
  offset for the exact-time rate and REM, conditional logit for the
  choice, the ordered rate and the ordered REM); (b) frame identity,
  `compute_statistics(output = "data.frame")` rows equal the hand-built
  rows on `(event, index_i, index_j)` and every statistic column, which
  proves the risk-set filtering before any optimizer runs; (c) the three
  backends agree within 1e-6 on the case's data, whichever backend the row
  names; (d) the stepping handle replays the batch with at least one
  excluded alternative whenever the case carries a mask. Coordination has
  no conditional-logit twin (documented by the predecessor) and runs arms
  (b) to (d) plus the identity of an all-allowing constraint with the
  unconstrained fit.
- **The PICT model is the record of the contract space.** The model file
  (factors, values, the four constraints encoding what the package refuses:
  the ordered rate takes no constraint, the opportunity list is choice on
  the r backend without a constraint, flavors are one-mode DyNAM layers, no
  ego-only constraint on the two-sided coordination risk set) and the
  generated case table live under the tests with a README naming the
  command that produced the table. Adding a factor value regenerates the
  table; the tests read the table, so coverage is a property of the model,
  not of which cells someone remembered.
- **The harness grows to serve every row.** Two-mode sequences (distinct
  sender and receiver sets, a rate over senders, a choice over receivers,
  REM over sender x receiver dyads); a node attribute for the ego-only
  constraint; an exogenous layer no formula reads for the uncovered
  constraint and a layer a term reads for the covered one; the ordinal
  frames (one row per sender at risk per event for the ordered rate, one
  row per dyad at risk per event for the ordered REM); the opportunity
  sets a choice row needs, emitted so the frame and the constraint
  equivalent are built from the same draw; the symmetrized dyad frame for
  coordination's frame-identity arm.
- **Fixtures are shared by data, not by row.** The 37 rows use a small
  number of simulated data objects (one per combination of modes,
  composition, flavor shape and the presence of the exogenous and allowed
  layers), each fitted under several families, backends and constraints,
  so the matrix costs minutes under `NOT_CRAN=true`, not an afternoon.
- **Not changed**: the predecessor's F1 to F5 tests and their extra arms
  (three goldfish routes for flavors, the single modeled flavor's blocks),
  which stay as written; the frozen baselines, which remain the 1e-6 floor
  on real data. **Not in scope**: parameter recovery against the true
  parameters (finite samples, as the predecessor decided); DyNAM-i;
  parametric waiting-time distributions; an oracle for coordination.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `simulation-cross-validation` (created by the predecessor's delta, not yet
  folded; this delta is written against that wording): ADDED "The
  estimation contract is covered combinatorially" (the factor space, the
  pairwise-plus-triple coverage, the model file as the record, the arms
  every case runs, the exclusions) and ADDED "The harness simulates two-mode
  and ordinal processes" (the data shapes and frames the matrix needs beyond
  the predecessor's harness).

## Impact

- **Code**: tests only. `tests/testthat/helper-simulate-dgp.R` (two-mode
  mode, node attribute, exogenous and allowed layers, ordinal frames,
  opportunity sets, symmetrized dyad frame); new
  `tests/testthat/helper-contract-matrix.R` (row to data, specification,
  constraint, backend and arms); new `tests/testthat/test-estimation_contract_matrix.R`
  (table-driven, one `test_that()` per row, `skip_on_cran()`);
  `tests/testthat/_fixtures/estimation_contract/` (`model.pict`,
  `cases.tsv`, `README.md`); `_references/simulation_v1/README.md` gains the
  matrix's measured deltas. No package code changes; no per-event code.
- **Numbers**: none move. Every arm compares goldfish to an oracle or to
  itself across backends; a failing row is a defect report, not a baseline
  to refreeze.
- **Dependencies**: none added to the package. The tests use `survival` and
  `mlogit` behind `skip_if_not_installed()`, `stats::glm` live, as the
  predecessor does. PICT is a developer tool for regenerating the table,
  not a test dependency: the committed `cases.tsv` is what the tests read.
- **Sequencing**: `depends-on: constraint-objects-on-shared-walk` (the
  harness and F1 to F5; its deltas are unarchived on this branch, so the
  placement pre-flight is expected to find no living `simulation-cross-validation`
  until it folds). Run after `likelihood-site-cleanup-and-cost` lands, so
  the matrix exercises kernels whose pins and budget are in place; nothing
  in the matrix depends on that change's code. Branch `feature_simulation`;
  NEWS.d fragment (developer-facing), no Version bump (ADR-0040).
- **Cost**: the matrix is the suite's largest new runtime item; a task
  measures it and records the budget (target under three minutes on the
  optimized build, the r backend rows being the slow ones).
- **ADRs**: ADR-0111 (pairwise coverage seeded with the named triple; the
  PICT model is the record; fixtures shared by data), ADR-0006, ADR-0010,
  ADR-0019 (the composition-plus-constraint case), ADR-0021 (no baseline
  moves), the predecessor's ADR-0073.

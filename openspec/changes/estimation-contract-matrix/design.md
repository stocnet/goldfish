## Context

On `feature_simulation` at `7b56821`, with `constraint-objects-on-shared-walk`
groups 0-4 landed. See `proposal.md` for the coverage survey and the
motivation; this section records what the matrix builds on.

**The harness.** `tests/testthat/helper-simulate-dgp.R`:
`simulate_relational_sequence()` simulates an exact-time DyNAM on a 0/1
adjacency (`n_actors`, `n_events`, `withr::local_seed()`), writes the rate,
choice and dyad frames inside the loop before each event is applied, and
takes hooks for flavored processes (`processes`), an exogenous stream with
a `constraint` closure (`dgp_allowed()`), composition changes and an
unmodeled flavor; `dgp_stocnet()` renders the stocnet data object. The
statistics it knows are the rate's present-tie in- and out-degree and the
choice's `recip` and two-path `trans`. It is one-mode only, has no node
attribute, no ordinal frames and no opportunity sets.

**The predecessor's fixtures.** F1 (baseline), F2 (creation/dissolution
with derived masks, three routes), F3 (constraint on an object no formula
reads), F4 (composition change, choice arm; the rate arm waits for
`process-simulation` 2.11c), F5 (one modeled flavor), each with oracle,
frame-identity and replay arms; `_references/simulation_v1/README.md`
records the measured deltas (worst 2.7e-08 with `mlogit`, goldfish fitted
with `score_tol = 1e-10`). Coordination is excluded from the oracle arm by
evidence (its README cites 0.2849 against 0.0575).

**The model.** `.plan/sp/estimation_contract.pict` (local, moved into the
tests by this change): seven factors, four rules, 2160 raw combinations,
476 admitted. PICT 3.7.4 yields 30 pairwise cases; seeded with the 30
admitted composition x constraint x flavor triples (the exhaustive order-3
cover of those three factors, generated separately and passed as
`/e:seeds`), 37 cases; a sub-model `{ Composition, Constraint, Flavor } @ 3`
yields 112 and full order 3 yields 95. The 37-row table is
`.plan/sp/estimation_contract_seeded_cases.tsv`. PICT is on this machine
at `~/.local/bin/pict`, built from source; the `pypict` binding does not
build here (its PyPI source lacks its Cython file, its GitHub source fails
to link), so the CLI is the tool.

**Oracles available.** Exact-time rate and REM: Poisson `glm` with
`offset(log(dt))`. Choice: `mlogit` (receiver as alternative) or `clogit`
stratified by event. Ordered rate: `clogit` over senders at risk per event
(a conditional logit with the sender as the alternative). Ordered REM:
`clogit` over dyads at risk per event (the Cox partial likelihood, which
`test-classical_equivalence.R` already uses). Coordination: none.

Constraints: no package code moves; the frozen baselines are untouched;
`survival` and `mlogit` behind `skip_if_not_installed()`; a branch writes a
`NEWS.d/` fragment (ADR-0040).

## Goals / Non-Goals

**Goals:**
- One table-driven test file that runs the same arms on every row of the
  generated case table.
- The harness able to produce the data every row names.
- The model file as the single record of the contract space.
- The matrix within a recorded runtime budget.

**Non-Goals:**
- Replacing F1 to F5 or the classical equivalence tests; they stay.
- Parameter recovery, DyNAM-i, parametric waiting times, a coordination
  oracle (the predecessor's exclusions).
- Testing the deprecation path of the opportunity list beyond its three
  rows (lifecycle messages are covered elsewhere).
- Making PICT a test-time dependency.

## Decisions

### D1 — Pairwise coverage, seeded with the exhaustive composition x constraint x flavor triple

The case set is PICT's order-2 cover of the admitted space, seeded with
every admitted combination of composition, constraint kind and flavor
shape. Pairwise guarantees every two-factor interaction a case; it does not
guarantee triples, and the one triple with a documented failure history
(ADR-0006, ADR-0010, ADR-0019: composition change under a constraint,
sharpened by a flavored layer whose mask flips with the modeled events) is
the one to cover exhaustively. Seeding costs seven cases over pairwise;
the alternatives cost more for less: a sub-model at order 3 (112 cases,
PICT's sub-model mixing is poor here), full order 3 (95), or trusting
pairwise (30, the triple sampled). *Rejected:* exhaustive (476 cases, an
afternoon per run); hand-picked cells (the state the survey found).

### D2 — The model file is the record; the tests read the generated table

`tests/testthat/_fixtures/estimation_contract/model.pict` holds the
factors, values and rules; `cases.tsv` is PICT's output for the seeded
run; `README.md` names the two commands (`pict model.pict /o:3` on the
three-factor projection to make `seeds.tsv`, then `pict model.pict
/e:seeds.tsv`) and the PICT version. The test file reads `cases.tsv` and
makes one `test_that()` per row, named from the row's values, so a failure
names its cell. A unit test checks the table against the model's rules
(no row violates a rule; every admitted pair appears; every admitted
triple appears), so a hand edit of the table cannot silently drop
coverage. *Rejected:* generating cases at test time (PICT would become a
dependency; the greedy stand-in generator is not the tool the skill names);
encoding the rows in R by hand (the thing the model exists to prevent).

### D3 — Fixtures are shared by data; a row is a fit on one of a few sequences

The data a row needs is determined by four of its factors: modes,
composition, flavor shape (the two-flavor data serves both the container
and the single-modeled rows) and whether the exogenous and allowed layers
are present (always generated; a row that does not read them ignores
them). That is at most 2 x 2 x 2 = 8 simulated data objects, built once
per test file with fixed seeds and sizes (one-mode: 15 actors, 160
events; two-mode: 10 senders, 12 receivers, 160 events), each carrying the
node attribute, the exogenous layer, the allowed panel layer and, where
flavored, the creation/dissolution stream. The 37 rows are fits on those
objects: the family picks the formula (`rate ~ 1 + indeg + outdeg`,
`choice ~ recip + trans`, REM `~ 1 + inertia + recip`, ordered variants
the same without the intercept), the constraint kind picks the formula
(`~ tie(allowed)`, `~ tie(emails)` with `recip(emails)` in the formula for
the covered case, `~ !tie(emails)` with no term reading it for the
uncovered case, `~ ego(grp) > 0`), the backend is passed through
`set_algorithm_newton()`, and the flavor shape picks `make_specification()`'s
form. The harness builds the frames per row from the same draw, since a
frame depends on the family and the constraint, not only on the data.
*Rejected:* one simulation per row (37 draws, 37 seeds, slow and no
shared diagnosis when a data object is wrong).

### D4 — Two-mode and ordinal extensions of the harness, by the process's own rule

Two-mode: senders and receivers are disjoint sets, the state is an
`n1 x n2` 0/1 matrix, the rate is over present senders, the choice over
present allowed receivers, the REM over present allowed dyads, and
self-dyads are admitted (no diagonal rule). Composition changes may hit
either set. Ordinal frames: for the ordered rate, one row per sender at
risk per dependent event with the observed sender marked; for the ordered
REM, one row per dyad at risk per dependent event with the observed dyad
marked; the rows are the exact-time frame's rows for that interval, which
the harness already has, so the ordinal frame is a projection, not a
second loop. The node attribute is drawn once per node (`grp` in {0, 1})
and the ego-only constraint reads it; the sequence is drawn under the
constraint so every observed sender is allowed. The opportunity sets are
the allowed receivers of the event's sender under `~ tie(allowed)`, so the
opportunity row and the constraint row share one frame. The coordination
frame is the dyad frame restricted to dyads allowed in both directions,
with the chosen dyad marked. *Rejected:* a second harness for two-mode
data (two sources of truth for the risk-set rule).

### D5 — The arms, and what a failure means

Each row runs: (a) oracle where it exists, 1e-6, goldfish fitted with
`score_tol = 1e-10` as the predecessor does, the log-likelihood offset
asserted for the rate side; (b) frame identity on `(event, index_i,
index_j)` and every statistic column, run before (a) so a risk-set or
statistic defect is reported as a frame difference; (c) `r`, `cpp` and
`gather` within 1e-6 on the same data and specification; (d)
`walk_replay_against_batch()` with `expect_gt(n_excluded, 0)` when the row
carries a mask. Coordination runs (b) to (d) plus the all-allowing
identity. The opportunity rows add the equivalence with `~ tie(allowed)`.
A failing arm is a defect in goldfish, in the harness or in the row's
construction, in that order of suspicion, and never a number to refreeze;
the test file's header says so. *Rejected:* asserting recovery of the true
parameters (finite samples).

### D6 — The matrix has a runtime budget

The matrix is table-driven and runs 37 fits plus their backend-equality
fits (two more each) and oracles; on the optimized build the cpp and
gather rows are sub-second and the r rows a few seconds on 15 actors. A
task measures the file's wall-clock under `NOT_CRAN=true` and records it
in `progress.md` and the README; the target is under three minutes. If a
row class dominates, its data object shrinks (fewer events), never the
arms. The whole file is `skip_on_cran()`, as the predecessor's is.

## Risks / Trade-offs

- [An oracle disagrees because of the optimizer, not the model] → the
  predecessor measured 2.7e-08 worst case with `mlogit`; `clogit` is the
  second choice oracle and the delta table records which was used per
  row.
- [The two-mode harness encodes a rule goldfish does not use (self-dyads,
  two presence streams)] → arm (b) runs first and reports the disagreement
  as a frame difference, before any coefficient is compared.
- [A row cannot be built because the package refuses the combination] →
  the model's rules encode every documented refusal; a new refusal found
  while building a row is a rule added to the model and a regenerated
  table, recorded in `progress.md`, not a skipped row.
- [The r backend rows make the suite slow] → D6's budget; 15 actors and
  160 events keep an r fit in seconds; the file is one `skip_on_cran()`
  unit.
- [F4's rate arm is excluded until `process-simulation` 2.11c lands] → the
  rows with composition change and an exact-time rate assert their choice
  and frame arms and mark the rate oracle arm as waiting on 2.11c, with the
  same note the predecessor's F4 carries; the task to lift the mark is
  named.
- [The committed `cases.tsv` drifts from `model.pict`] → the rules test of
  D2 fails on a row that violates a rule or a missing pair or triple.

## Migration Plan

Commit per task on `feature_simulation`; `NOT_CRAN=true` green with
baselines PASS at each. Order: model and table into the fixtures with the
rules test (D2) → harness extensions with their own unit tests (D4) → the
row runner on the one-mode unconstrained rows first, then every row (D3,
D5) → runtime budget (D6) → close. `status: landed (feature_simulation,
awaiting fold)` at the end; no archive on the branch.

## Open Questions

- Whether F1 to F5 should later be re-expressed as rows of the table with
  their extra arms attached, once the matrix is stable; not needed for
  coverage, so deferred.
- Whether the rows with an exact-time rate under composition change get
  their rate oracle arm here once `process-simulation` 2.11c lands on the
  trunk, or in that change's fold.

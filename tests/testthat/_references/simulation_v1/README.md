# Simulation cross-validation

`test-simulation_cross_validation.R` checks goldfish's estimators against
classical fits of the same likelihood on sequences simulated from a known
process. Nothing is frozen here: the sequences are regenerated under their
seeds on every run, `stats::glm` ships with R, and `survival` and `mlogit`
run live behind `skip_if_not_installed()`. This file records what was
measured, so a later gap can be read against it.

## The harness

`helper-simulate-dgp.R` simulates exact-time DyNAM sequences: an exponential
waiting time from the total rate over the senders at risk, the sender in
proportion to its rate, the receiver from the multinomial over the receivers
it may choose. Inside the same loop, before each event is applied, it writes
the frames the classical fits use, from its own 0/1 state and with the rule
the process used (an absent node has no row; a sender with no allowed
receiver has no rate row; every event that is not a dependent event of the
process closes the interval with a censored row). It shares no state code
with goldfish, so an agreement is evidence about goldfish.

The statistics follow each effect's definition on a 0/1 state: the rate's
`indeg` and `outdeg` count present ties, the choice's `recip` reads the
reverse tie and `trans` counts two-paths `i -> k -> j`. A REM degree effect
reads the receiver by default (`type = "alter"`), `outdeg` included; the
first version of the harness read the sender's out-degree there, and the
frame-identity arm reported it before any coefficient was compared.

## Two arms per fixture

**Frame identity.** The rows `compute_statistics(output = "data.frame")`
exports equal the hand-built frame on `(event, index_i, index_j)`, the
realized outcome, the interval length and every statistic column. This
proves the risk-set filtering and the statistics before any optimizer runs.

**Oracle parity**, gate `1e-6`, the package's ordinary discipline:

| fixture | comparison | max abs coefficient difference |
|---|---|---|
| F1 baseline | DyNAM rate ↔ `glm` Poisson, `offset(log(dt))` | 2.3e-11 |
| F1 baseline | REM rate ↔ dyadic `glm` Poisson | 2.6e-13 |
| F1 baseline | DyNAM choice ↔ `survival::clogit` | 1.8e-14 |
| F1 baseline | DyNAM choice ↔ `mlogit::mlogit` | 2.7e-08 |
| F2 creation | DyNAM rate ↔ `glm` Poisson | 3.1e-14 |
| F2 creation | DyNAM choice ↔ `survival::clogit` | 1.8e-12 |
| F2 dissolution | DyNAM rate ↔ `glm` Poisson | 1.3e-13 |
| F2 dissolution | DyNAM choice ↔ `survival::clogit` | 5.6e-12 |
| F3 constraint-only layer | DyNAM rate ↔ `glm` Poisson | 3.2e-14 |
| F3 constraint-only layer | DyNAM choice ↔ `survival::clogit` | 1.7e-14 |
| F4 composition change | DyNAM choice ↔ `survival::clogit` | 2.0e-11 |
| F5 creation alone | DyNAM rate ↔ `glm` Poisson | 3.1e-14 |
| F5 creation alone | DyNAM choice ↔ `survival::clogit` | 1.8e-12 |

The log-likelihoods differ by exactly `sum(log(dt))` over the realized
intervals on the rate side (measured 6e-13 for the DyNAM rate, 1.3e-12 for
REM) and agree on the choice side (6e-14 against `clogit`).

goldfish is fitted with `score_tol = 1e-10`, `step_tol = 1e-12`. At its
default stop (`score_tol = 1e-6`) the choice differs from `clogit` by
3.5e-07 and the REM from `glm` by 8.8e-07: the whole gap is goldfish's last
Newton step, most of the gate, so the tests fit to the maximum and compare
the two optimizers rather than goldfish's stopping rule.

F1: 10 actors, 150 dependent events, seed 42; rate coefficients
(intercept, indeg, outdeg) = (-1, 0.3, -0.2), choice (recip, trans) =
(1, 0.4). The window opens at the simulation's clock origin
(`set_preprocessing(start_time = 0)`).

On every constrained fixture the log-likelihoods agree to within 3.7e-13
after the `sum(log(dt))` offset on the rate side.

## The constrained fixtures

**F2**, creation and dissolution under their derived masks: a mutually
exclusive layer, 8 actors, 200 events (111 creations, 89 dissolutions), a
third of the dyads tied at the start, seed 21. Each flavor's risk set is its
derived mask, so a sender with no absent (creation) or present (dissolution)
tie is not at risk. Three goldfish routes agree exactly, coefficients and
log-likelihoods: the flavored container, a standalone single-flavor
specification per flavor, and a de-flavored data object where each flavor
is its own layer and the mask is written by hand (`~ !tie(calls)`,
`~ tie(calls)`). Each equals its oracle.

**F3**, a constraint on an object no formula term reads: senders 1-3 email
every other node at spread times (8 actors, 120 events, seed 9) and
`~ !tie(emails)` removes the emailed receivers, so each of them is gated out
of the rate partway through. Each `emails` event closes the rate's interval
with a censored row (ADR-0103). The choice is fitted as `~ recip`.

**F4**, composition change: F1's process with N3 and N7 leaving and
returning (10 actors, 150 events, seed 42). The choice arm only: closing the
rate interval at a presence change is `process-simulation` task 2.11c,
applied at the trunk merge with the move of the fisheries baselines, so the
rate arm lands there.

**F5**, one modeled flavor: F2's sequence with creation alone keyed. The
dissolution events still change the state and close the creation rate's
interval as censored rows, and the creation estimates equal F2's creation
blocks.

## The handle replay

Every constrained fixture is replayed through the walk handle
(`walk_replay_against_batch()`, `helper-walk-replay.R`), which compares the
handle's evaluation at each dependent event with the batch's and counts the
alternatives the compared risk sets excluded: F2 400 checks and 609
exclusions, F3 240 and 8, F4 300 and 13 (absent receivers), F5 222 and 377.

## Coordination is excluded, by evidence

`sub_model = "choice_coordination"` has no fixture here because it has no
classical twin to be checked against. Its unordered-dyad log-weight is
`(s_ij + s_ji)'theta - LSE_i(theta) - LSE_j(theta)`: the two log-sum-exp
corrections are nonlinear in `theta` and indexed by the dyad's own
endpoints, so they vary across the alternatives of one event and do not
cancel in a conditional-logit stratum. The classical references measured
it (`_references/classical_v1/README.md`): on the Social Evolution calls
fixture with one weighted inertia term goldfish returns 0.2849 and a
`clogit` on the summed pair statistics 0.0575, with log-likelihoods
-2522.8 and -2804.6 -- different models, not a numerical discrepancy. The
coordination fixtures keep their byte-identity references
(`fixtures/constraint_atoms_refs.rds`, family `coord`) as their gate.

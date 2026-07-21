## ADDED Requirements

### Requirement: Relational-event permutation sampler

The package SHALL provide a Metropolis–Hastings sampler that generates new event
sequences from an existing sequence by **permuting relational-event (RE) times
while holding every panel-event (PE) time fixed** — the mirror of the estimation
augmenter, which permutes PE times with RE times fixed. A single move MUST:

1. choose a pair of relational events `ω_h`, `ω_k` (`h < k`) to permute;
2. sample a new time `t′_k` for the first, strictly inside the open window
   `(t_{h-1}, U)` where `U = t_{h+1}` for `k > h+1` and `U = t_{h+2}` for
   `k = h+1`, using the neighbors among the *unmoved* events padded by the sequence
   boundaries;
3. sample a new time `t′_h` for the second, strictly inside
   `(max{t′_k, t_{k-1}}, t_{k+1})`;

with both proposed times drawn so that the order among the unmoved events is
respected and only the position of the moved REs relative to the PEs (and other
REs) can change. On acceptance the sampler MUST rename the moved events to their
new times.

#### Scenario: RE pair permuted, PE times untouched

- **WHEN** a permutation move is applied to a sequence
- **THEN** exactly the two chosen relational events receive new times inside their
  windows, every panel-event time is unchanged, and the resulting sequence remains
  time-ordered

#### Scenario: Adjacent pair window

- **WHEN** the chosen pair is adjacent (`k = h + 1`)
- **THEN** the upper bound for `t′_k` is `t_{h+2}` and the window rule still yields a
  non-empty open interval for both proposed times

### Requirement: Proposal density and acceptance ratio

The sampler SHALL draw the proposed RE times from truncated-exponential proposals
whose rates come from the fitted model, accumulate the forward and reverse
log-proposal densities over the full move, and accept the move with probability
`α = [f(Ω′)/f(Ω)] · [q_rev/q_fwd]`, where `f` is the sequence likelihood at the
fitted parameters. The move-type and pair-selection probabilities MUST cancel from
`α` (they depend only on move-invariant structure), and an invalid proposal (one
violating an ordering or support constraint) MUST be excluded before it is built
rather than evaluated and rejected.

#### Scenario: Metropolis–Hastings acceptance

- **WHEN** a candidate sequence `Ω′` is proposed from `Ω`
- **THEN** the move is accepted when a uniform draw `u ≤ α` and otherwise the chain
  retains `Ω`, with `α` using the likelihood ratio and the ratio of accumulated
  truncated-exponential proposal densities

#### Scenario: Invalid proposals never evaluated

- **WHEN** a proposed permutation would violate a time-ordering or support
  constraint
- **THEN** it is excluded from the proposal set before construction, so no likelihood
  evaluation is spent on an invalid sequence

### Requirement: Null-drawn whole-space pool builder

The package SHALL provide a builder that, from the null model `m0`'s PE-augmented
pool (reused from `m0` or regenerated at `θ̂₀`), grows a **whole-space** pool in
which both the RE and PE placements vary **under `θ̂₀`**, by running the
RE-permutation sampler to produce at least 10 new sequences per seed sequence (the
count MUST be user-controllable, defaulting to 10). Each produced sequence MUST
carry the bookkeeping the deviance evaluation needs (its likelihood contribution and
weight to the `m0` target), and burn-in and thinning MUST be applied so retained
draws are approximately independent.

#### Scenario: Ten-per-seed whole-space pool under the null

- **WHEN** the builder is run on `m0`'s PE-augmented pool with the default grow count
- **THEN** it returns a whole-space pool of at least 10 resampled sequences per seed
  sequence, each varying both layers under `θ̂₀` and carrying its likelihood/weight
  bookkeeping

#### Scenario: Grow count configurable

- **WHEN** the user sets the per-seed grow count to `m`
- **THEN** the builder produces `m` retained sequences per seed after burn-in and
  thinning

### Requirement: Parallel generation behind a map seam

The whole-space builder SHALL run behind a single map seam so per-seed chains may be
generated in parallel. The default backend MUST be `mirai`, running under the
non-nested thread budget shared with estimation, and a serial fallback MUST be used
when no parallel daemons are configured. Parallel generation MUST use stream-split
seeds so the produced pool is identical for a fixed seed regardless of the number of
workers.

#### Scenario: Serial and parallel agree under a fixed seed

- **WHEN** the builder runs serially and with `mirai` daemons under the same fixed
  seed
- **THEN** both produce an identical whole-space pool

#### Scenario: Serial fallback without daemons

- **WHEN** no parallel daemons are configured
- **THEN** the builder generates the pool serially through the same map seam without
  error

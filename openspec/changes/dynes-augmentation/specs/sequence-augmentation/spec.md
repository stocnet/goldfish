## ADDED Requirements

### Requirement: Wave diffing produces the candidate flip set

The system SHALL diff consecutive snapshots of a panel-semantics focal layer into the
candidate flip set for each between-wave interval: dyads changing 0→1 are creation
candidates, 1→0 dissolution candidates, keyed to the interval's wave-time boundaries.
The diffing function SHALL be usable standalone on a validated data object. Every
augmented sequence for an interval MUST consist of flip events that transform the
interval's start state exactly into its end state (endpoint-hitting), with event times
strictly inside the interval boundaries.

#### Scenario: two waves diff into flips
- **WHEN** wave t has ties {(1,2)} and wave t+1 has ties {(1,2 absent), (3,4)}
- **THEN** the candidate set for the interval is one dissolution (1,2) and one creation
  (3,4).

#### Scenario: endpoint-hitting enforced
- **WHEN** an augmenter returns a sequence for an interval
- **THEN** applying the sequence to the start state reproduces the end state exactly,
  or the sequence is rejected with a diagnostic.

### Requirement: Augmenter contract with three built-in variants

The package SHALL define an augmenter contract —
`augment(state_t, state_t1, theta, control)` returning one or more endpoint-hitting
sequences together with their proposal densities — and SHALL provide three
constructors: `augment_sequence_random()` (uniform ordering and uniform times over the
flip set), `augment_sequence_model()` (sequential draw from the flavored model at
`theta`, consuming the per-event simulation hook so each draw sees the state after
event i), and `augment_sequence_mutate()` (MCMC moves on a current sequence; the move
set — order permutations and any excursion insert/delete moves — follows the RSiena
ML-estimator study). Augmenters SHALL NOT compute importance weights; they report
proposal densities for the evaluator. New augmenter variants SHALL be addable without
modifying the ABEM loop (selection by contract, mirroring the writer strategy).

#### Scenario: random augmenter yields valid sequences
- **WHEN** `augment_sequence_random()` draws 10 sequences for an interval
- **THEN** each is endpoint-hitting with times inside the interval, and orderings vary
  across draws.

#### Scenario: model augmenter consumes the simulation hook
- **WHEN** `augment_sequence_model()` draws a sequence at parameters `theta`
- **THEN** each next event is drawn from the model's rates/choices evaluated at the
  process state after the previously drawn event, via the recipe loop's simulation
  hook.

#### Scenario: mutation preserves validity
- **WHEN** `augment_sequence_mutate()` proposes a move on a valid sequence
- **THEN** the proposed sequence is endpoint-hitting and the move's forward/reverse
  proposal densities are reported for acceptance computation.

### Requirement: Batched pool evaluation with per-sequence sugar

The evaluator SHALL compute, for a pool of augmented sequences at a parameter vector,
the requested per-sequence quantities — log-likelihood, score, and/or Fisher
contribution, selected by a `what` request flag so callers pay only for what they
consume (the SGD loop runs score-only; Fisher is computed at convergence and for
opt-in trace SEs) — in a batched C++ call
over the sequences' flat preprocessed objects (default format; zero optimizer
iterations), plus importance weights formed from model density over proposal density.
Each pooled sequence SHALL permanently carry its reference record (the parameters it
was drawn under, its log-likelihood there, and its log proposal density), kept on the
log scale, so cross-iteration reweighting is a likelihood ratio, never a re-draw.
The package SHALL export `compute_lik_seq(spec, sequence, theta)` as per-sequence sugar
over the same batched path (named to avoid the `logLik()` S3 collision). Each drawn
sequence SHALL be fully preprocessed through the existing recipe path (no incremental
patching); rate/probability evaluation at a given state SHALL reuse the estimation
kernels. The batched contract SHALL be revised only through the Phase-1 benchmark
outcome recorded in the change's design.

#### Scenario: batched evaluation matches per-sequence estimation
- **WHEN** a pool of 10 sequences is evaluated at `theta`
- **THEN** each sequence's log-likelihood equals (within 1e-10) the existing engine's
  zero-iteration evaluation of the equivalent single-sequence model at `theta`.

#### Scenario: weights live in the evaluator
- **WHEN** a pool drawn by `augment_sequence_random()` is evaluated under the model
  density at `theta`
- **THEN** each sequence's importance weight is the model-to-proposal density ratio
  computed by the evaluator, and weights normalize over the pool.

### Requirement: Pool storage stays memory-bounded

The pool SHALL be held as an in-memory list of flat preprocessed objects by default,
with the acceptance bound measured in the Phase-1 profiling spike (pools of 100–1000
sequences at Social-Evolution scale within approximately 5 GB). If the bound is
exceeded, the fallback SHALL reuse existing machinery (a broadcast-aware on-disk
variant of the default format, or the DBI writer) rather than introducing a new
storage format; the chosen strategy and its measurements SHALL be recorded in the
change's design before the evaluator phase is implemented.

#### Scenario: profiled decision recorded
- **WHEN** the Phase-1 memory spike completes
- **THEN** the design records wall/RSS measurements over the (n × events × K) grid and
  the storage decision they imply, and the implementation follows it.

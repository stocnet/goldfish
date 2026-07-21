## ADDED Requirements

### Requirement: Wave diffing produces the candidate flip set

The system SHALL diff consecutive snapshots of a panel-semantics layer into the
candidate flip set for each between-wave interval, consuming the layer's
transition/support specification: each observed value change maps through the
(from-value, to-value) → flavor mapping, which SHALL be validated as injective (the
flavor is never latent); a change larger than one allowed step SHALL decompose into
a per-dyad ordered chain of events; net-zero changes SHALL produce no events. All
panel-semantics layers SHALL share one wave grid, the node set SHALL be constant
over the period, and an observed relational event falling exactly on a wave time
SHALL belong to the earlier interval (preceding the snapshot). The diffing function
SHALL be usable standalone on a validated data object. Every augmented sequence for
an interval MUST consist of flip events that transform the interval's start state
exactly into its end state (endpoint-hitting), with event times strictly inside the
interval boundaries and same-dyad chain order respected.

#### Scenario: two waves diff into flips
- **WHEN** wave t has ties {(1,2)} and wave t+1 has ties {(1,2 absent), (3,4)}
- **THEN** the candidate set for the interval is one dissolution (1,2) and one creation
  (3,4).

#### Scenario: multi-step change decomposes into an ordered chain
- **WHEN** a dyad's value changes 0→2 between waves under ±1-step transitions
- **THEN** the flip set holds two events on that dyad (0→1 then 1→2) with a forced
  order that every augmenter respects.

#### Scenario: endpoint-hitting enforced
- **WHEN** an augmenter returns a sequence for an interval
- **THEN** applying the sequence to the start state reproduces the end state exactly,
  or the sequence is rejected with a diagnostic.

### Requirement: Augmenter contract with three built-in variants

The package SHALL define an augmenter contract —
`augment(data_window, theta, control)`, the data window carrying the wave snapshots,
per-interval flip sets, and observed event streams, returning one or more
endpoint-hitting sequences together with the log proposal density of their full
generative path — and SHALL provide three constructors: `augment_seq_random()`
(iid-uniform times per interval, same-dyad chains ordered by within-chain sorting),
`augment_seq_sim()` (constrained sequential simulation from the model at `theta` as an
external driver of the `multi-process-walk` handle — reusing the `process-simulation`
per-step drawing core under wave-endpoint conditioning: sender–flavor risk sets
restricted in R to support-applicable remaining events plus the single globally-next
unplaced modeled relational event; receivers among remaining observed receivers;
waiting times from truncated exponentials at the selected pair's rate, bounded by the
next anchor — min(next relational event, wave end)), and `augment_seq_mcmc()` (one serial global
chain over the whole sequence; within-wave permute and shift moves mixed by the
user-facing `move_probs`; rate-based truncated-exponential time proposals frozen at
the state after the preceding panel event; burn-in and thinning counted in sweeps;
moves violating same-dyad chain order excluded before proposal). The MCMC augmenter
SHALL receive an injected proposal-evaluator closure providing each candidate's
log-likelihood and the named-pair rates its reverse densities need; it SHALL NOT
call the pool evaluator directly. Augmenters SHALL NOT compute importance weights;
they report proposal densities for the evaluator. New augmenter variants SHALL be
addable without modifying the ABEM loop (selection by contract, mirroring the writer
strategy).

#### Scenario: random augmenter yields valid sequences
- **WHEN** `augment_seq_random()` draws 10 sequences for an interval
- **THEN** each is endpoint-hitting with times inside the interval, chain order
  respected, and orderings vary across draws.

#### Scenario: simulation augmenter drives the walk handle
- **WHEN** `augment_seq_sim()` draws a sequence at parameters `theta`
- **THEN** each next event is drawn from the model's rates/choices obtained by
  `walk_evaluate()` at the process state after the previously injected event, and the
  recorded log proposal density includes every selection step — relational-event
  selections included.

#### Scenario: mutation preserves validity
- **WHEN** `augment_seq_mcmc()` proposes a move on a valid sequence
- **THEN** the proposed sequence is endpoint-hitting (moves stay within one wave and
  respect chain order) and the forward/reverse truncated-exponential time densities
  are reported for the acceptance computation.

#### Scenario: single-event intervals still mix
- **WHEN** a between-wave interval contains exactly one panel event
- **THEN** shift moves redraw its time (its position relative to relational events
  can change), so the chain does not freeze that interval.

### Requirement: Batched pool evaluation with per-sequence sugar

The evaluator SHALL compute, for a pool of augmented sequences at a parameter vector,
the requested per-sequence quantities — log-likelihood, score, and/or Fisher
contribution, selected by a `what` request flag so callers pay only for what they
consume (the SGD loop runs score-only; Fisher is computed at convergence and for
opt-in trace SEs), and, on request, named-pair rates at named positions emitted
during the same pass (the reverse-density byproduct the MCMC augmenter's proposal
evaluator consumes) — in a batched C++ call
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
- **WHEN** a pool drawn by `augment_seq_random()` is evaluated under the model
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

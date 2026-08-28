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
SHALL be usable standalone on a validated data object. The diff output SHALL be
compiled once, at estimation entry, into a shared `augmentation_recipe` — a θ-free,
sequence-free plan holding the per-interval flip sets, the per-dyad ordered chains,
the injective flavor map (validated exactly once, here), the risk-set membership
universe (which dyads flip and their chain order; live support applicability stays in
the walk handle), and the same-wave pair/constraint graph — which every augmenter reads
while holding only a mutable per-draw cursor over it, never mutating the shared plan.
The plan SHALL also cache the per-wave start states (identical across all
endpoint-hitting sequences, θ-free), materialized once and shared by every sequence's
re-preprocess rather than reconstructed per draw. This restriction to a single shared
wave grid is a v1 bound; multiple/nested panel grids are a recorded future development. Every augmented sequence for an interval MUST consist of flip
events that transform the interval's start state exactly into its end state
(endpoint-hitting), with event times strictly inside the interval boundaries and
same-dyad chain order respected.

#### Scenario: two waves diff into flips
- **WHEN** wave t has ties {(1,2)} and wave t+1 has ties {(1,2 absent), (3,4)}
- **THEN** the candidate set for the interval is one dissolution (1,2) and one creation
  (3,4).

#### Scenario: multi-step change decomposes into an ordered chain
- **WHEN** a dyad's value changes 0→2 between waves under ±1-step transitions
- **THEN** the flip set holds two events on that dyad (0→1 then 1→2) with a forced
  order that every augmenter respects.

#### Scenario: endpoint-hitting holds by construction
- **WHEN** an augmenter returns a sequence for an interval
- **THEN** applying the sequence to the start state reproduces the end state exactly;
  this holds by construction and is asserted in tests and an opt-in debug path, not
  re-checked per draw on the production hot path.

#### Scenario: augmentation recipe compiled once and shared
- **WHEN** `estimate_dynes()` opens a panel specification whose augmenters run over
  many EM iterations
- **THEN** the wave-diff-derived flip sets, per-dyad chains, injective flavor map,
  risk-set universe, and pair/constraint graph are compiled once into the shared
  `augmentation_recipe`, the injective map is validated exactly once, and each
  augmenter reads that static plan through its own mutable per-draw cursor without
  re-diffing per sequence or per iteration.

### Requirement: Augmenter contract with three built-in variants

The package SHALL define an augmenter contract —
`augment(data_window, theta, control)`, the data window carrying the wave snapshots,
the shared compiled `augmentation_recipe` (per-interval flip sets, per-dyad chains,
injective flavor map, risk-set universe, and pair/constraint graph — θ-free, built
once), and observed event streams, returning one or more endpoint-hitting sequences
together with the log proposal density of their full generative path — and SHALL
provide three constructors: `augment_seq_random()`
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

Augmenters SHALL NOT break ties among observed events with a rule of their own, nor
sample their order: the only tie an augmented sequence can carry is between two observed
relational events (a sampled panel event never equals an anchor, by the strictly-inside
time rule; continuous panel-event draws tie with probability zero), and such a tied pair
is forced adjacent by construction. Their mutual state-fold order SHALL be taken from the
observed event stream (the order the `tied-event-times` change makes explicit and
user-controllable), and the augmenter (`q`) and the evaluator (`f`) SHALL fold state over
tied observed events in the identical order, so the importance weight stays consistent
regardless of which order the input carries.

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

#### Scenario: tied observed relational events fold identically in augmenter and evaluator
- **WHEN** two observed relational events share a timestamp inside an interval and a
  sequence carrying them is both drawn by an augmenter and scored by the evaluator
- **THEN** both fold state over the tied pair in the order the observed event stream
  carries (neither invents a tie-break nor samples the order), the pair is placed
  adjacent with no sampled panel event between them, and the augmenter's proposal
  density and the evaluator's model likelihood agree (within 1e-10) on that ordering so
  the importance weight is unaffected by which input order the tie carries.

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
The evaluator SHALL return a **classed E-step object** (`estep_is`, `estep_resampling`,
or `estep_uniform`, sharing a `dynes_estep` parent) carrying those per-sequence
quantities and weights, so that the scheme-specific `compute_q()` / `compute_ase()`
generics (specified by the `abmcem` change) dispatch on it without the EM loop
re-branching per weighting scheme; constructing this object is the evaluator's
responsibility, the generics that read it are not.
Each pooled sequence SHALL permanently carry its reference record (the parameters it
was drawn under, its log-likelihood there, and its log proposal density), kept on the
log scale, so cross-iteration reweighting is a likelihood ratio, never a re-draw.
The package SHALL export `compute_lik_seq(spec, sequence, theta)` as per-sequence sugar
over the same batched path (named to avoid the `logLik()` S3 collision). Each drawn
sequence SHALL be fully preprocessed through the existing recipe path (no incremental
patching), except that the per-wave start states — identical across all
endpoint-hitting sequences and θ-free — SHALL be materialized once and shared rather
than reconstructed per drawn sequence; rate/probability evaluation at a given state
SHALL reuse the estimation kernels. The batched contract SHALL be revised only through the Phase-1 benchmark
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

#### Scenario: evaluator returns a scheme-classed E-step object
- **WHEN** a pool weighted under importance sampling is evaluated
- **THEN** the returned object carries class `estep_is` (a `dynes_estep`), holding the
  per-sequence quantities and weights, ready for the `compute_q()` / `compute_ase()`
  generics to dispatch on without the caller inspecting the weighting scheme.

### Requirement: Pool storage stays memory-bounded

The pool SHALL be held as an in-memory list of flat preprocessed objects by default,
with **no fixed byte threshold**: the acceptance bound is set empirically by the
Phase-1 profiling spike on the broadcast (`stat_mat_update` + pointers) representation,
measuring the pool footprint over the n × events × K grid, rather than by a constant
(the earlier ~2 GB / ~5 GB figures referred to a superseded heavier representation).
No user-facing cap argument is exposed; the spill fallback is the whole memory story.
If the measured budget is exceeded, the fallback SHALL reuse existing machinery (a
broadcast-aware on-disk variant of the default format, or the DBI writer) rather than
introducing a new storage format; the chosen strategy and its measurements SHALL be
recorded in the change's design before the evaluator phase is implemented.

#### Scenario: profiled decision recorded
- **WHEN** the Phase-1 memory spike completes
- **THEN** the design records wall/RSS measurements over the (n × events × K) grid and
  the storage decision they imply, and the implementation follows it.

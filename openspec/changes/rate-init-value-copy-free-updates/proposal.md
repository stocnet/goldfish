## Why

A dyadic (kind-0) support mask is stored as one flat logical vector, and every
reader that wants a slice of it rebuilds the whole `n1 x n2` grid first. On
CollegeMsg (1899 actors) that is 13.8 MB and about 0.87 ms per read, and a
constrained DyNAM choice does it once per event, in its point fold
(`build_active_dyad_point()` via `support_row()`). The fold accounts for 68% of
a constrained choice's preprocessing: 7.5 s against 2.8 s unconstrained at 5000
events, 15.9 s against 5.3 s at 10000. Reading the row by index gives the same
values at 9.8 µs and 22 KB. The cost matters now because
`process-simulation` 2.11/2.11b measure the exposure's own cost on these runs,
and this overhead would hide it.

Scope is the mask readers only. The other large cost on the same runs, the
two-path effect cache copied on every update (`apply_two_path_update()`,
about 8.6 GB allocated per 1000 CollegeMsg events), needs more exploration
(in-place writes require an ownership audit, including simulation
replicates) and is left out.

## What Changes

- `support_row()` reads a kind-0 sender row by strided index into the stored
  vector (`support[sender + n1 * (seq_len(n2) - 1L)]`) instead of
  `matrix(support, n1, n2)[sender, ]`. Every caller benefits: the choice point
  fold, the constraint validation loop, and the stepping handle.
- `sender_gate_from_mask()` and `initial_receiver_count()` count a kind-0
  mask's allowed, present receivers per sender without reshaping the vector
  into a matrix and subsetting it (two full copies per call today).
- The per-event sender gate in the constraint validation loop is kept
  incrementally from the mask's flips, as the rate fold already does
  (`fold_active_sender_support()`), instead of being recomputed from the
  whole mask at each event.
- No output changes: every folded availability, mask stream, coefficient and
  message is byte-identical.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None. This is a performance change with no spec-level behavior change;
`.openspec.yaml` sets `skip_specs: true`.

## Impact

- **Code**: `R/support_mask.R` (`support_row()`, `sender_gate_from_mask()`),
  `R/model_preprocess.R` (`initial_receiver_count()`, the choice point fold's
  row reads), `R/model_estimate.R` (the validation loop's gate).
- **Callers affected, behavior unchanged**: `build_active_dyad_point()`,
  `validate_support_constraint()`, `walk_handle.R` and
  `simulate_driver.R`'s gate reads.
- **Tests**: equality of each reader against the grid form on every mask
  kind (and one-mode and two-mode shapes); byte-identical folded
  availability on the constrained reference families; frozen baselines PASS.
- **Timing**: CollegeMsg subsets before and after, constrained choice and
  constrained rate plus choice.
- **Relation**: `constraint-objects-on-shared-walk` D9 moves mask
  composition into the walk; the row and gate readers here stay the ones it
  calls, so this change lands independently and first.

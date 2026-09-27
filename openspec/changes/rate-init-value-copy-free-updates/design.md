## Context

See proposal.md for why. A support mask is stored at its own kind: `3`
scalar, `2` ego (length `n1`), `1` alter (length `n2`), `0` point (a flat
column-major logical of length `n1 * n2`). The separable kinds already answer
every read without a grid. Only kind `0` reshapes: `support_row()` builds
`matrix(support, n1, n2)` to take one row, and `sender_gate_from_mask()`
builds it and then subsets its columns (two full copies) to count each
sender's allowed, present receivers. The rate fold
(`fold_active_sender_support()`) and the REM fold already update
incrementally from `mask_flips()`; the choice point fold
(`build_active_dyad_point()`) and the validation loop in
`validate_support_constraint()` read from scratch at every event.

Measured on a 1899 x 1899 mask: the grid row read takes 868 µs and allocates
13.8 MB; the strided read takes 9.8 µs and allocates 22 KB, with identical
values.

## Goals / Non-Goals

**Goals:**
- No kind-0 reader materializes the `n1 x n2` grid to read a slice.
- The validation loop's per-event sender gate costs a flip's worth of work,
  not a full mask's.
- Every output byte-identical.

**Non-Goals:**
- The two-path effect cache copy (`apply_two_path_update()`): it needs an
  ownership audit before an in-place write is safe, and is left to its own
  exploration.
- Moving mask composition into the walk: `constraint-objects-on-shared-walk`
  D9.
- Any change to what a mask means, when it is read (the strictly-before
  rule) or how it is stored.

## Decisions

### D1 — A kind-0 row is read by strided index

`support_row()`'s kind-0 branch reads `support[sender + n1 * (seq_len(n2) -
1L)]`, the column-major addresses of row `sender`, and coerces to logical as
today. *Rejected:* caching the reshaped matrix across calls. The mask changes
between events, so a cache would have to be invalidated by the same flips
that make the strided read cheap, and it would hold a second grid-sized
object.

### D2 — Kind-0 counts are taken on the vector, not on a reshaped matrix

`sender_gate_from_mask()` and `initial_receiver_count()` count allowed,
present receivers per sender with `.rowSums()` on the flat vector, restricted
to the present receivers' columns by linear index only when some receiver is
absent. With every receiver present this allocates nothing grid-sized; with
some absent it allocates the present columns once, where today it allocates
the grid twice. The self-dyad handling is unchanged (it already reads the
diagonal by index). *Rejected:* a matrix product with the presence vector.
It needs the matrix object this decision removes.

### D3 — The validation loop maintains its sender gate from flips

`validate_support_constraint()` recomputes the kind-0 gate from the whole
mask at each dependent event. It keeps per-sender allowed-receiver counts
instead, seeded by `initial_receiver_count()` and adjusted from
`mask_flips()` and the receiver presence changes. That is the rate fold's own
bookkeeping, and the implementation shares its helper rather than
re-deriving it. Its row read (which receivers the observed sender may
choose) uses D1. *Rejected:* leaving validation from-scratch because it runs
once per fit. On a constrained 10000-event fit it is 10000 full-grid passes,
the same order as the fold this change fixes.

### D4 — Equality first, then the switch

For each reader, a test compares the new form against the grid form on every
mask kind, on one-mode and two-mode shapes, with the diagonal dropped and
kept, before the old form is deleted. The folded availability of the
constrained reference families (`helper-constraint-atoms.R`) is asserted
byte-identical.

## Risks / Trade-offs

- [An off-by-one in the strided address reads a column instead of a row] →
  D4's equality tests on non-square two-mode masks, where a transposed read
  cannot pass by symmetry.
- [The maintained validation gate drifts from the from-scratch one at a
  presence change or a tied stamp] → the validation tests' existing fixtures
  plus an equality test of the maintained gate against the from-scratch one
  at every event of a constrained fixture with composition change.
- [Snapshot or message changes in validation] → none expected; the messages
  read the same sets, and existing snapshots are the check.

## Migration Plan

Commit per task on `feature_simulation`, `NOT_CRAN=true` green with the
frozen baselines PASS at each. Order: equality tests (D4) → D1 → D2 → D3 →
timing. Lands before `process-simulation` 2.11, so the exposure's before and
after timings are read without this overhead.

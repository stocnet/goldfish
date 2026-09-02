# Proposal — backend-vocabulary

## Why

`set_algorithm_newton(engine = c("default_c", "default", "gather_compute"))`
misnames its own values twice over: `"default_c"` IS the default but reads like
a variant, `"default"` is NOT the default (it is the R reference
implementation), and `"gather_compute"` is an internal strategy name on a
user-facing argument. The vocabulary has also leaked into the living spec — 16
requirements across 8 capabilities describe user-selectable behavior in terms of
`default_c` / `default` / `gather_compute`, several of them pinning a
requirement to one implementation where both compute backends actually provide
it. 2.0.0 (CRAN ~mid-Aug) is the release that already carries the estimation
naming migration, so this is the moment to fix the last misnamed argument on
that surface and sweep the vocabulary behind it, in one deprecation cycle
rather than two.

Carved out of `revise-gather-output` (2026-07-25): the rename originally rode
there because that change reworks the gather backend's consumption path, but
the sweep reaches capabilities that change never touches — broadcast decoding,
multimode equivalence, the data object. Folding it there would make that
change's archive rewrite requirements unrelated to statistics export, which is
the same shape `algorithm-naming` was carved out for.

## What Changes

- **`backend = c("cpp", "r", "gather")` replaces `engine`** on
  `set_algorithm_newton()` — values naming what actually runs: `"cpp"` (the C++
  event loop, the default), `"r"` (the R reference implementation), `"gather"`
  (the gather-stack C++ path). Alternatives considered and rejected: keeping
  `engine` with new values (the parsnip/knitr term-of-art precedent, rejected in
  favor of the plainer word), `evaluator` (exports internal vocabulary),
  `implementation` (no precedent in the family).
- **`engine =` remains a `deprecated()` sentinel** and the legacy values are
  accepted wherever supplied, mapped `default_c → cpp`, `default → r`,
  `gather_compute → gather`, with one soft-deprecation warning naming the final
  spelling. The surface shipped in the 1.7 line, so there is no
  delete-outright here (contrast `compute_stats`, dev-line only). No two-hop
  messages: every warning names `backend` and its final value.
- **Internal tokens are NOT renamed.** The compiled interface's engine strings
  stay as they are; this is a user-vocabulary rename mapped at the constructor
  boundary. A living requirement describing the C++ implementation's internals
  keeps its names — only what a user passes or selects is swept.
- **The living spec is swept to the backend vocabulary**, and where a
  requirement's behavior is genuinely provided by more than one backend it
  SHALL say so rather than pinning the sentence to `default_c` with the R
  implementation as an afterthought. Where a behavior is single-backend
  (maxLik-driven optimizers; the C++ BLAS staging) it stays single.
- **BREAKING** only in the sense that the argument name changes; every previous
  call keeps working for the 2.x cycle through the sentinel and the value map.

## Capabilities

### New Capabilities

None. This change renames an argument on an existing surface and re-words
existing requirements; it introduces no new behavior.

### Modified Capabilities

- `optimizer-selection`: the constructor argument becomes `backend` with
  descriptive values, the maxLik gating requirement is stated against the `cpp`
  backend, and `return_event_scores` is stated against both compute backends
  with the gather backend rejecting it. (Moved here wholesale from
  `revise-gather-output`.)
- `likelihood-computation`: the R reference implementation and the coordination
  gather path are named by backend rather than by legacy engine value; the
  requirements describing C++ internals keep their implementation names.
- `broadcast-stat-updates`: the R-decode and C++-decode requirements, and the
  shared-core requirement, name backends instead of engine values.
- `flat-preprocess-output`: the coefficient-reproducibility and per-flavor
  requirements name the `r` and `cpp` backends.
- `active-availability-stat`: the opportunity-list requirement's cross-backend
  agreement scenario names the three backends.
- `multimode-networks`: the two-mode coefficient-equivalence requirement names
  both compute backends.
- `single-data-object`: the recipe-input contract names backends rather than
  engine values.

## Impact

- **goldfish R**: `R/set_opt.R` (the `backend` formal, the `engine` sentinel and
  value map, roxygen), `R/model_estimate.R` and the estimation gating messages
  (maxLik-requires-cpp, gather rejections) speaking the backend vocabulary;
  `R/goldfish-deprecated.R` (reusing the existing `fold_renamed_arg()` helper).
  No `src/` changes: the compiled interface keeps its tokens.
- **Tests and vignettes**: every `engine =` call site migrates; snapshots for
  the argument and value mapping.
- **Docs**: NEWS entry for the rename with the value map; DESCRIPTION bump at
  the milestone.
- **Sequencing**: implements BEFORE `revise-gather-output` finishes, so that
  change's new requirements (gather writer, db writer) are written under the
  final vocabulary and neither change modifies a requirement the other also
  modifies. `revise-gather-output` keeps its own `preprocess-output-writers`
  delta; this change does not touch that capability.

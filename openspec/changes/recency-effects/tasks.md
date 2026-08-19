# Tasks — recency-effects

Implementation waits for `effect-term-registry` (Layer 1) and
`effect-naming-scheme` (Layer 2); group 1 happens at proposal time.

## 1. Homogenization edits to active changes (proposal-time)

- [x] 1.1 `effect-term-registry` design D18: reserve `kernel`, `k`,
      `last_k`, `bands` in the schema argument vocabulary and the
      rank-buffer `cache_spec` fields (recency ordering state,
      replay-reconstructible), with a pointer to this change
- [x] 1.2 `tied-event-times` design D3: add consumer note — the
      recency freeze-then-update block contract binds to whatever
      tie-order mechanism D3 settles
- [x] 1.3 `two-sided-coordination` design D8: add cross-reference —
      its t⁻ wording is canonical and inherited by `recency-effects`;
      rank statistics flow through the directed p_ij unchanged
- [x] 1.4 `gather-rem-coordination-format` proposal: note that
      indicator/band kernels are dictionary-friendly while `1/r`
      yields all-distinct rows

## 2. Rank buffer infrastructure

- [ ] 2.1 Move-to-front buffer (distinct keys, O(1) push, risk-set
      walk assigning ranks) with unit tests: empty history, single
      partner, repetition keeps ordering length at distinct-partner
      count, dissolution no-op, rank-1 no-op
- [ ] 2.2 Ring-buffer finite-k fast path with property test:
      finite-k output equals the MTF walk truncated at k on random
      event streams
- [ ] 2.3 Replay test: buffer state at any event index reconstructed
      from initial state + update stream equals the forward pass
- [ ] 2.4 Declare buffer fields via the registry `cache_spec`
      (activation predicate on the recency effects)
- [ ] 2.5 Freeze-then-update tie-block contract: bind block boundary
      to the `tied-event-times` mechanism if landed; otherwise
      document data-order dependence and add the within-block
      permutation test as skipped-with-reason

## 3. Sender-scope effects on DyNAM choice

- [ ] 3.1 Register `recency_send` / `recency_receive` (registry
      entries, validity metadata, init/update via the buffer)
- [ ] 3.2 Acceptance: bit-identical statistics to
      `.plan/recency/functionsEffects.R` cache pair on its example
      data (kernel = "inverse", k = Inf)
- [ ] 3.3 Parity harness vs remstats `rrankSend`/`rrankReceive` and
      relevent `RSndSnd`/`RRecSnd` with the convention-mapping table
      (t⁻ reads, tie handling, first-contact value)
- [ ] 3.4 Kernels `inverse`/`indicator`/`geometric`/`rank` with
      truncation-composes-with-kernel tests
- [ ] 3.5 Truncation bound in the fitted-model summary (cli) and the
      refit-at-k-and-2k diagnostic documented

## 4. REM and dyad scope

- [ ] 4.1 Register the family for REM; parity vs remstats tie-oriented
      variants
- [ ] 4.2 `recency_dyad` implementation + parity vs `recencyContinue`

## 5. Rate side

- [ ] 5.1 Settle the `recency_global` aggregate menu (open question)
      and implement the v1 subset
- [ ] 5.2 Acceptance: reproduce the `.plan/recency/04DyNAMSBM.R`
      truncated raw-rank covariate stream (kernel = "rank", finite k)
- [ ] 5.3 Pin the fractional index-advance interaction by test
      (counter-defined "last k", ordering unaffected)

## 6. `last_k =` restriction

- [ ] 6.1 Build-plan fulfillment: derived network + count-driven
      expiry pseudo-event stream merged into the schedule
- [ ] 6.2 `window` × `last_k` intersection semantics; panel-layer
      rejection with the window error family
- [ ] 6.3 Decide the v1 set of effects accepting `last_k` (open
      question: closure semantics) and document the
      restricted-network meaning per effect

## 7. Rank-band basis

- [ ] 7.1 `kernel = "bands"` expansion via the build-plan/expansion
      machinery with parent-term decoder metadata
- [ ] 7.2 Default log-spaced bands + `bands =` boundary override;
      collinearity warning; empty-band interplay with
      `identifiability-diagnostics` noted in docs
- [ ] 7.3 LR-vs-fixed-kernel specification-check workflow documented
      with an example

## 8. Coordination slice (gated)

- [ ] 8.1 Verify gates: `coordination-tie-consistency` landed and the
      `two-sided-coordination` surface is the registration target
- [ ] 8.2 Register the family for coordination; seed from the
      reference implementation; tied-alternatives behavior tests

## 9. Baselines, docs, close-out

- [ ] 9.1 Mint new frozen baselines for the new statistics per the
      baselines rule (new set; frozen `global_v1` untouched)
- [ ] 9.2 Effect gallery/reference entries; vignette section
      contrasting `window =` (time domain) with `last_k =`/kernels
      (event-index domain)
- [ ] 9.3 Full NOT_CRAN suite green; DESCRIPTION/NEWS milestone bumps
      per phase discipline

**The detector comes before the fix, and here the detector is partly already
written.** The frozen 1e-6 coefficient baselines and the C++ goldens execute
every line this change touches, so unlike the preprocessing work they are a real
detector and not a floor. `NOT_CRAN=true`, PASS not SKIP, at every commit. **A
coefficient that moves is a defect in this change**, never a baseline to
refreeze.

**C++ is expected here.** Route every `src/` edit through `cpp-recompile` so
`RcppExports.*` is regenerated and the goldens never run against a stale object,
and record the interface delta in `progress.md`.

## 1. Establish what is duplicated

- [x] 1.1 Inventory the availability apply on all three layers, with file and
      line: the two inline subassignments in `R/estimation_core.R`, the two
      `.gather_apply_presence*()` helpers in `R/cpp_interface.R`, and the
      per-engine loop in each of the six `src/*_default.cpp`. Record the buffer
      shape each one walks and the encodings it branches on.
- [x] 1.2 Diff the six C++ loops against each other and record where they
      genuinely differ from one another versus where they are copies. Six
      near-copies and six exact copies are different problems and the fix
      differs; a reader of this change should not have to take it on faith.
- [x] 1.3 Verification: `NOT_CRAN=true` green at the branch point, baselines
      PASS not SKIP. This is the reference every later task is read against.
- [x] 1.4 Prove the detector detects, once per layer, before any code moves.
      The goldens and the parity tests are inherited, not written for this
      change, so their sensitivity to THIS defect class is an assumption until
      shown: on a scratch branch, offset one engine's availability walk by one
      event (skip the first update, or apply it twice) and confirm that
      family's C++ golden fails; do the same to one inline apply in
      `R/estimation_core.R` and confirm a backend-parity test fails; revert
      both. Record the failing test names in `progress.md`. A layer whose
      break produces no red gets a targeted test written in this task, before
      2.x or 3.x touch it.

## 2. The C++ layer (design D1, D2)

- [x] 2.1 Write the shared cursor as a header beside `flat_updates.h` and
      `broadcast_updates.h`: advance to event `i`, yield this event's columns.
      The WALK is what is shared; each engine keeps its own write, since that is
      the part that genuinely differs by encoding.
- [x] 2.2 Route the six engines through it, one commit per engine, each
      reporting the goldens. Six commits is not ceremony here: a pointer bug in
      one engine moves that family's likelihood and nothing else, so a bisect
      that lands on one engine is worth the extra commits.
- [x] 2.3 Tests: the C++ golden tests are the detector and must be byte-identical
      throughout. Add a targeted test only where the inventory in 1.2 found an
      engine whose loop differed from the others, since that is the one the
      shared cursor could silently change.
- [x] 2.4 Verification: `NOT_CRAN=true`, baselines and goldens PASS not SKIP,
      after a `cpp-recompile`. Record the `RcppExports` delta, which should be
      empty: a header shared between existing engines exports nothing new.

## 3. The R layers (design D1, D3)

- [x] 3.1 Route the R backend's two inline applies in `R/estimation_core.R`
      through the same cursor shape. The write stays per-consumer; the pointer
      walk does not.
- [x] 3.2 The gather layer: either route `.gather_apply_presence()` and
      `.gather_apply_presence_point()` through it, or exempt the layer under
      design D3 with the diff that shows sharing reads worse. **An exemption is
      a spec delta narrowing the requirement for that layer with its reason**,
      not a note in `progress.md`.
- [x] 3.3 Tests: the backend-parity tests are the detector — the R backend, the
      gather stack and the C++ engines must agree as they do today. Assert the
      agreement, not the implementation.
- [x] 3.4 Verification: `NOT_CRAN=true`, baselines PASS not SKIP.

## 4. Close

- [ ] 4.1 Confirm the requirement is met as written, layer by layer, in
      `progress.md`: name the shared function each layer's availability apply
      now consumes. If any layer was exempted, its delta is written and its
      reason is in the capability spec, not only in the journal.
- [ ] 4.2 ADR on what "one shared apply per layer" means when the buffers differ
      in shape — the question the requirement never answered and the next
      consumer will ask again. Claim the id in the vault ledger before drafting.
- [ ] 4.3 `NEWS.d/` fragment under Internal only if anything user-visible moved.
      Nothing should; if something did, that is the finding and it belongs in
      the fragment.
- [ ] 4.4 `bash .plan/opsx-spec-placement-check.sh shared-availability-apply`
      and `openspec validate shared-availability-apply --strict` clean.
- [ ] 4.5 Final verification: full `NOT_CRAN=true` suite green, baselines and
      goldens PASS not SKIP, `devtools::document()` if any roxygen changed.

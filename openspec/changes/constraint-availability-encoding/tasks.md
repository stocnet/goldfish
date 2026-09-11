**Two halves, one mental model: what the folded availability object means and
what the user is told about it.** They share two functions, so they land
together.

`NOT_CRAN=true` at every commit with the frozen baselines PASS not SKIP. No
baseline model carries a constraint, so here they are a floor and the captured
reference fit is the real detector.

## 1. The outer fold for an ego-kind choice constraint (design D1, D2)

- [ ] 1.1 Detector first: an ego-kind constraint on a DyNAM choice model
      produces `active_dyad_encoding == "outer"` and allocates no dyad-shaped
      object. It fails today — `active_dyad_encoding_decide()` returns `"outer"`
      and `build_active_dyad_point()` hardcodes it back to `"point"`.
- [ ] 1.2 Capture the before-state of the reference fit in
      `_fixtures/ego_outer_standalone_ref.rds` and confirm it reproduces on the
      unfixed tree, so the comparison in 1.4 is against a known-good capture
      rather than a fresh one taken after the change.
- [ ] 1.3 Implement the outer branch in `fold_active_dyad_support()`: two factor
      vectors, cell `(i, j) = active_sender[i] & active_dyad[j]`. The
      unconstrained REM path already produces this encoding and the engines
      already consume it.
- [ ] 1.4 Move the expectation at `test-support_constraint_ego_fold.R:65` from
      `"point"` to `"outer"`, AND assert the reference fit is unchanged in the
      same test. Changing an assertion to match new behavior is how a regression
      gets ratified; the fit comparison is what licenses the edit. Say that in
      the test, in its own terms.
- [ ] 1.5 Rewrite the comment above the fold, by hand, so it describes the
      encodings the function now produces. Do not delete it — it has been
      describing the intended design correctly all along; only the code was
      behind.
- [ ] 1.6 Verification: `NOT_CRAN=true`, baselines PASS not SKIP; record the
      ego-kind availability object's size before and after.

## 2. The choice family's missing sender warning (design D3)

- [ ] 2.1 Detector first: an ego-kind constraint on a choice model with a
      gated-out, never-observed sender warns once and names the node. It is
      silent today.
- [ ] 2.2 Add the sender-side accumulation to `validate_support_constraint()`'s
      choice branch, worded symmetrically to the rate family's "present senders
      never at risk". A warning, not an error: a sender that never appears is a
      legitimate model, and the constraint may describe a wider population than
      the observed events.
- [ ] 2.3 `r-lib:cli`: semantic elements, data interpolated rather than literal
      markup, pluralisation handled, and a pinned reproducible cli context for
      the snapshot.
- [ ] 2.4 Tests: the four existing fail-fast validations are unchanged in
      message and in trigger condition. Assert that explicitly — this task adds
      a condition to a function whose other conditions users rely on.
- [ ] 2.5 Verification: `NOT_CRAN=true`, baselines PASS not SKIP.

## 3. Close

- [ ] 3.1 Answer the design's open question: does an ego-kind constraint on REM
      or coordination hit the same gap? Those go through the REM fold, which is
      point-only because a dyadic risk set is the whole matrix — confirm it
      rather than assume it, since that is exactly what was assumed about the
      choice fold.
- [ ] 3.2 `NEWS.d/` fragment: the encoding under Internal, the new warning as a
      user-facing bullet, since a new condition is something a user sees.
- [ ] 3.3 `bash .plan/opsx-spec-placement-check.sh constraint-availability-encoding`
      and `openspec validate constraint-availability-encoding --strict` clean.
- [ ] 3.4 Final verification: full `NOT_CRAN=true` suite green, baselines PASS
      not SKIP.

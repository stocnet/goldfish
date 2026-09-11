## Context

`support-mask-sparse-updates` made the mask a stream and folded availability
incrementally. It did not revisit which ENCODING the choice fold picks, and a
post-landing review found that the decision function and the fold disagree.

```
active_dyad_encoding_decide("alter", mask_kind)

  mask_kind 1 alter  -> "alter"   fold produces alter    OK
  mask_kind 3 global -> "alter"   fold produces alter    OK
  mask_kind 2 ego    -> "outer"   fold produces point    <-- unreachable
  mask_kind 0 point  -> "point"   fold produces point    OK
```

The `"outer"` branch is reachable in the decision and unreachable in the fold.
Nothing is wrong numerically — a dense point buffer represents the same risk set
— so this costs memory and a lie in a comment, not correctness.

## Goals / Non-Goals

**Goals:**
- The choice fold produces the encoding its own decision function returns.
- An ego-kind choice constraint allocates no dyad-shaped object, which is what
  the living spec asks for in words.
- A choice-family constraint tells the user about senders it gates out, as the
  rate family already does.

**Non-Goals:**
- The frozen receiver axis in the rate gate. That is a correctness defect and is
  being fixed on the branch before `support-mask-sparse-updates` archives, not
  here.
- New encodings. `alter`, `outer` and `point` are what exist; this change makes
  one of them reachable.
- Changing what any constrained model computes. The reference fit is the proof.

## Decisions

### D1 — Implement the outer fold rather than delete the unreachable return

Two ways to make the code and its comment agree: produce the encoding, or stop
claiming to. Producing it is right for three reasons that are not about elegance.

The living spec asks for it in words — no dyad-shaped object for a sender-axis
constraint — and the rate side already honours that. The consumer exists, since
the unconstrained REM path produces and the engines consume an outer encoding,
so this is wiring rather than invention. And the saving is three orders of
magnitude at realistic sizes, 14 MB against 15 KB at 1899 actors, which is the
same argument the mask stream already won.

*Rejected:* deleting the `"outer"` return and rewriting the comment to say the
choice fold has two encodings. Cheaper, honest, and it leaves a requirement
unmet and a known 14 MB on the floor.

### D2 — The moved test expectation needs the reference fit, not an argument

`test-support_constraint_ego_fold.R:65` asserts the current behavior. Changing an
assertion to match new behavior is how a regression gets ratified, so the burden
is to show no number moved: the captured fit in
`_fixtures/ego_outer_standalone_ref.rds` is compared before and after, and the
comparison is what licenses the edit.

State that reasoning in the test, in its own terms. A reader who cannot open
this design should still understand why the expectation changed.

### D3 — The sender warning mirrors the rate one and stays a warning

The rate family already warns that N present senders are never at risk, always
gated out. The choice counterpart says the same thing about senders allowed no
receiver at any event.

It is a warning rather than an error because a sender that never appears in the
data is a legitimate model: the constraint may be describing a population wider
than the observed events. An error would refuse models that are fine.

*Rejected:* folding it into the existing "empty risk set" error. That fires per
event on an observed sender; this is a pattern over the whole sequence, and
reporting a pattern one event at a time is what makes the current message hard
to act on.

## Risks / Trade-offs

- [The outer fold changes a number] → the reference fit is the gate, and the
  estimation side already consumes this encoding for unconstrained REM, so the
  path is exercised.
- [The new warning is noisy on legitimate models] → it fires only when a present
  sender is allowed NO receiver at ANY event, which is a statement about the
  constraint rather than the data. If it proves noisy in practice, that is
  evidence the constraint is wrong, which is what it is for.
- [Two changes in one] → they share two functions and one mental model, the
  meaning of the folded object. Splitting them means touching both functions
  twice.

## Migration Plan

Internal. One commit per task. `NEWS.d/` fragment under Internal for the
encoding, and a user-facing bullet for the new warning, since a new condition is
something a user can see.

## Open Questions

- Does an ego-kind constraint on REM or coordination hit the same gap? Those go
  through `fold_active_dyad_support_rem()`, which is point-only by construction
  because a dyadic risk set is the whole matrix. Worth confirming rather than
  assuming, since "point by construction" is what was assumed about the choice
  fold too.

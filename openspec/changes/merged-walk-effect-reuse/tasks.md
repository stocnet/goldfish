**Research first, implementation second, and phase 3 may not happen.** The stop
conditions are named in design D4 and any of them is a result. A phase that
measures produces a recorded number before the next phase reads it.

**The frozen 1e-6 coefficient and C++ golden baselines are the floor
throughout**, `NOT_CRAN=true`, PASS not SKIP. This change alters how a statistic
is computed, never what it equals, so unlike the constraint work the baselines
here are a real detector and not merely a floor.

## 0. The interaction product is emitted at its own broadcast kind

**First, because task 1.1 measures the walk and this changes what the walk
emits.** Measuring before this lands would produce a baseline that the very next
group invalidates.

- [ ] 0.1 `augment_interactions()` sets a product column's `broadcast_kind` to
      the axis-union of its operands, and the walk then emits every product
      delta as point cells regardless. An alter-by-alter interaction emits one
      point update per sender where a plain alter effect emits a single
      broadcast entry. Maintain the product at its declared kind: keep the dirty
      set at that kind rather than expanding it to cells before the recompute.
- [ ] 0.2 Tests: an interaction whose operands are all alter-kind emits
      broadcast entries, not point cells, and its estimated column is
      byte-identical. A genuinely dyadic product still emits point cells.
- [ ] 0.3 Verification: `NOT_CRAN=true`, frozen baselines and C++ goldens PASS
      not SKIP. The product column reaching the engines is unchanged in value,
      so the baselines are a real detector for this group.

## 1. Decompose the gap (design D1)

- [ ] 1.1 **After group 0**, extend `.plan/sp/substrate_timing.R` to sweep event
      counts rather than take one: Social Evolution at its full 439, and CollegeMsg at 500, 1500,
      5000 and 10,000, unconstrained, merged against the two loops. Medians of
      repeated runs after a discarded warm-up — a single un-warmed run inflated
      a cell by 35 percent during `support-mask-sparse-updates` and read as a
      regression that did not exist.
- [ ] 1.2 Fit time against event count per substrate and record the intercept
      (per-call constant) and slope (per-event work) for each, with the
      residuals, in `.plan/sp/substrate_decomposition_2026-09.md`. **State
      whether the fit is linear**: curvature is itself a finding, and the
      constrained choice loop is known to grow faster than linearly.
- [ ] 1.3 Name the parts of the merged walk's constant by instrumenting the
      phases it has and the loops do not — compiling the plan, building one
      engine per unit, the shared schedule, the per-unit writers and consumer
      specs. Instrument, do not profile: during `support-mask-sparse-updates` a
      profile credited one function with 64.7 percent of a cost it owned 47 of,
      while the largest single item was spread across four callers of a fifth
      and appeared on no line (ADR-0057 is the standing version of this
      lesson).
- [ ] 1.4 Verification: `NOT_CRAN=true` green, baselines PASS not SKIP. Nothing
      has moved yet; this is the reference point.

## 2. Reduce the constant where it is reducible (design D1, D4)

- [ ] 2.1 From task 1.3's attribution, fix the reducible parts, one commit each,
      each with its own before/after on the task 1.1 sweep. Do not batch them:
      a combined number cannot say which change paid.
- [ ] 2.2 Record what is NOT reducible and how big it is, so the gate reads a
      floor rather than a hope. If the floor keeps the unconstrained ratio above
      1.0 at typical sizes, that is design D4's first stop condition — record it
      and go to group 4.
- [ ] 2.3 Verification: `NOT_CRAN=true`, baselines PASS not SKIP; re-run the
      task 1.1 sweep and tabulate against it.

## 3. The shared-quantity seam (design D2, D3)

- [ ] 3.1 Count, per `(layer, flavor)` process with more than one sub-model, the
      effect-update calls each engine makes, and identify which name the same
      object AND the same quantity. **Same name is not the test**: windowed,
      weighted and `type =` variants can share a name without sharing a value.
      Record the table in `.plan/sp/`.
- [ ] 3.2 Establish whether the seam is clean for the clearest candidate.
      `indeg` in a rate formula is a node-level in-degree broadcast on the
      sender axis; in a choice formula the same in-degree broadcast on the
      receiver axis. Assert the two kernels agree on the pre-broadcast quantity
      at every event on a fixture, as a TEST, before any code moves. The rate
      kernel rejects some formula shapes the choice kernel accepts (observed
      2026-09-10: `rate = ~ 1 + indeg(friendshipNetwork)` aborts at
      initialization), so establishing a shape both accept is part of this task.
- [ ] 3.3 If 3.2 holds: prototype maintaining the shared quantity ONCE in the
      merged engine and projecting it to each sub-model's kind through
      `project_value()`. The diagonal rule is per consumer, not shared
      (ADR-0063): a dyad-kernel statistic masks its diagonal and a sender-kernel
      one does not, so what is shared is the pre-mask vector.
- [ ] 3.4 If 3.2 does NOT hold, stop and record why, naming what the two kernels
      actually disagree about. Design D4 makes this a result; group 4 reports it.
- [ ] 3.5 Tests: the shared-quantity path is byte-identical on every fixture
      that carries a shared term, and the frozen baselines reproduce. Any model
      with no shared term is untouched, asserted rather than assumed.
- [ ] 3.6 Verification: `NOT_CRAN=true`, baselines PASS not SKIP; re-run the
      task 1.1 sweep.

## 4. Report and hand back

- [ ] 4.1 Tabulate the sweep before against after at every size, both substrates,
      both arms, and state the unconstrained ratio's size profile plainly.
- [ ] 4.2 Re-read `preprocess-one-walk`'s 1.10x gate on that profile and write
      the result into its task 0.9 (claim it first, ADR-0036). The rule names
      the 10k cell, which passed at 0.98 on 2026-09-10 while the 439- and
      1500-event cells read 1.35 and 1.18; say which cells pass now and let the
      decision be taken on all of them.
- [ ] 4.3 Record what this change does NOT fix, with numbers, as
      `support-mask-sparse-updates` did.
- [ ] 4.4 An ADR if a design choice settled — the shared-quantity seam either
      existing or not is exactly the kind of finding that should not be
      rediscovered. Claim the id in the vault ledger before drafting.
- [ ] 4.5 `NEWS.d/` fragment only if user-visible behavior changed; a pure
      measurement change writes none. No `NEWS.md`, no Version bump (ADR-0040).

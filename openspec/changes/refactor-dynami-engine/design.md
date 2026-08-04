## Context

This is a **stub** capturing the deferral promised by `refactor-formula-parsing`
(design D8 + revised Non-Goals). It is intentionally thin: the detailed design is
written when the change is taken up, after `refactor-formula-parsing` has merged
and the actual DyNAMi recipe surface is known.

`refactor-formula-parsing` leaves DyNAMi in this state:

- **Isolated (done there, task 2.3e):** `estimate_dynami` routes to its own
  preprocessing front-end — old parse-time windowing (`assign()`),
  `cleanInteractionEvents`, and the `preprocessInteraction` monolith — sharing
  only the **leaf** parse/link helpers with DyNAM/REM and handing `prep` back to
  the model-agnostic estimation tail. The shared DyNAM/REM recipe path
  (`preprocess(spec_map, data)` + data-boundary realizer) carries no DyNAMi
  branches. DyNAMi does **not** consume the shared `spec_map`/realizer yet.
- **Deferred (this change):** (a) convert the DyNAMi **engine** — the monolithic
  `preprocessInteraction` loop (`R/model_preprocess_group.R`) and its
  `cleanInteractionEvents` pre-step (`R/make_data_group.R`) — to the recipe loop;
  (b) **unify** DyNAMi onto the shared `spec_map` + state-creation realizer
  (retiring the 2.3e isolated front-end); (c) the DyNAMi `make_specification()` /
  spec-object estimate path.

## Goals / Non-Goals

**Goals:** convert DyNAMi rate/choice preprocessing to the recipe architecture
consuming the shared `spec_map`; unify DyNAMi onto the shared state-creation
data-boundary realizer (retiring the 2.3e isolated front-end); reproduce existing
DyNAMi coefficients to 1e-6; add the DyNAMi spec-object estimate path.

**Non-Goals:** changing DyNAMi model semantics or effect definitions; the shared
formula leaf parsing and the DyNAM/REM recipe/realizer infrastructure themselves
(owned by `refactor-formula-parsing`) — this change makes DyNAMi *consume* them,
it does not rebuild them.

## Decisions

_To be written when the change is taken up._ Open questions to resolve then:

1. How DyNAMi's post-event update order and group-network (`groupsNetwork`)
   handling map onto the recipe loop's per-event update model.
2. Whether `cleanInteractionEvents`' order correction + windowed-interaction
   class tagging become a plan derivation, a schedule transform, or stay a
   pre-step feeding the recipe loop.
3. `subType` normalisation placement (plan vs effect closure).
4. The DyNAMi `stat_state` / broadcast classification (if any) versus a plain
   point-update loop.
5. **The observation window is silently ignored, and this change owns the fix**
   (found 2026-08-04 while applying `prep-diag-debug` group 2; deferred to here
   rather than fixed there). `preprocess_interaction()` refuses a window
   outright (`R/model_preprocess_group.R:100-107`, "DyNAMi doesn't support
   setting the endTime parameter"), but `preprocess.dynami_rate_spec()` and its
   siblings absorb `startTime`/`endTime` into `...` and never forward them, so
   the guard is unreachable. Measured on the RFID fixture: an `end_time` at the
   median of the interaction span produces preprocessed output identical to
   setting none, with the fit reporting the last event as its end time and
   storing rows past the requested boundary — no error, no warning, no message.

   The cheap remedy is to forward the two arguments so the existing `stop()`
   fires. The question this change has to answer is whether that is the right
   one, because the recipe loop it converts to **does** support a window, and
   the honest end state may be DyNAMi accepting one rather than aborting on it.
   Decide that before wiring the abort, so the abort is not built and then
   immediately retired.

   Recorded as **ADR-0012**
   (`decisions/ADR-0012-dynami-silently-ignores-the-observation-window.md`),
   which carries the measurement, the options weighed, and the instruction to
   decide *whether* DyNAM-i should accept a window before wiring any abort.

   ADR-0007 (`decisions/ADR-0007-an-inert-argument-is-signaled-not-dropped.md`)
   decides the general form: an argument rendered inert is signaled where the
   user made the choice, not silently dropped, because the mistake and the
   moment of discovery are far apart and the diagnostics that would ordinarily
   catch it report nothing wrong. That reasoning describes this case exactly.
   The ADR is scoped to effect-term arguments and is still `proposed`, so it
   does not formally bind; if it is accepted as stated, this is a second
   instance and the ADR is worth widening to control arguments. Until the fix
   lands, `set_preprocessing()`'s documentation names `estimate_dynami()` as
   ignoring both time arguments, so the documented contract stays true.

   Scope note: only `startTime`/`endTime` are affected. The dispatch absorbs
   other arguments into `...` as well, but those are exercised by DyNAMi's
   existing use and are working.

**Dev-plan guidance (added 2026-07-03):** the interface proposal
(`goldfish_asta/code/plan/goldfish_dev_plan.md`, §ties notes) recommends rethinking the
DyNAM-i input data from scratch as a **two-mode network (actors × groups) with a support
constraint**, rather than porting the bespoke `make_groups_interaction()` representation.
When this change is taken up — after `support-constraint-risk-set` (masks) and
`refactor-single-data-object` (stocnet input, mode map D7) — evaluate modeling groups as a
second mode with joining/leaving as constrained two-mode events atop that machinery,
before deciding to keep a dedicated group preprocessing path.
`make_groups_interaction()` is the one legacy constructor deliberately left undeprecated
for this change to resolve.

## Risks / Trade-offs

- DyNAMi has the largest existing baseline suite (rate + choice); the recipe
  conversion must reproduce it exactly — the 1e-6 floor is the guard.
- Retiring the monolith touches `cleanInteractionEvents`, which several DyNAMi
  windowed-effect tests exercise; keep them green throughout.

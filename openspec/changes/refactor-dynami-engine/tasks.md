> **Stub.** Detailed tasks are written when this change is taken up, after
> `refactor-formula-parsing` merges. Follow `openspec/config.yaml` disciplines
> (commit-per-task, `devtools::document()` inline, `NOT_CRAN=true` baselines PASS
> not SKIP, version/NEWS at the milestone). DyNAMi baselines are the 1e-6 floor.

## 0. Prerequisite

- [ ] 0.1 Confirm `refactor-formula-parsing` is merged: DyNAMi **isolated** in its
      own preprocessing front-end (task 2.3e) — old parse-time windowing `assign()`,
      `cleanInteractionEvents`, and the `preprocessInteraction` monolith, with the
      shared DyNAM/REM path carrying no DyNAMi branches. DyNAMi does not yet consume
      the shared `spec_map`/realizer; that unification is part of this change.

## 1. Engine conversion + unification (to be detailed)

- [ ] 1.1 Design the DyNAMi recipe loop (resolve design open questions 1–4).
- [ ] 1.2 Convert DyNAMi rate preprocessing to the recipe loop; reproduce the
      rate baseline to 1e-6.
- [ ] 1.3 Convert DyNAMi choice preprocessing to the recipe loop; fold in
      `cleanInteractionEvents` (order correction, windowed-interaction tagging,
      `subType`); reproduce the choice baseline to 1e-6.
- [ ] 1.4 Unify DyNAMi onto the shared `spec_map` + state-creation data-boundary
      realizer (`preprocess(spec_map, data)`): retire the isolated front-end's
      parse-time windowing `assign()` (from 2.3e) in favour of the shared realizer,
      so DyNAM/REM and DyNAMi share one windowing/`sanitizeEvents` mechanism.
- [ ] 1.5 Add the DyNAMi `make_specification()` / spec-object estimate path.
- [ ] 1.6 Retire `preprocessInteraction`, the isolated DyNAMi front-end, and the
      DyNAMi branches in `cleanInteractionEvents` once the recipe loop reproduces
      them.

## 2. Milestone

- [ ] 2.1 Bump `DESCRIPTION` + `NEWS.md`; full `NOT_CRAN=true` suite green
      (DyNAMi baselines PASS not SKIP); `openspec validate --strict`.

## Class cleanup (added 2026-09-05)

- [ ] Retire the three DyNAM-i interaction marker classes if the converted
      recipe path no longer reads them: `goldfishInterNet`,
      `goldfishInterGrp`, `goldfishInterWindow` (renamed by
      `class-naming-scheme`; stamped in `R/make_data_group.R`, read only by
      `preprocessInteraction`). If the recipe path still needs the
      distinction, record why in `progress.md` — a marker class nothing reads
      is exactly the over-specification `model-spec-descriptor` was written
      about.

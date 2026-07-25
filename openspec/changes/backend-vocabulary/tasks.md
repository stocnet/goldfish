# Tasks — backend-vocabulary

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline whenever roxygen /
exports / signatures change, `air format` the R files a task touched before
`lintr` runs on them, `NOT_CRAN=true` baselines PASS (not SKIP) before each
commit, r-lib skills (r-package-development, testing-r-packages, cli,
lifecycle) invoked before the work they cover.

This change implements BEFORE `revise-gather-output` finishes (design D5), so
that change's new requirements are written under the final vocabulary and the
two never modify the same requirement. No `src/` edits: the compiled
interface's tokens are unchanged (D1).

## 1. The argument and its deprecation

- [x] 1.1 `set_algorithm_newton(backend = c("cpp", "r", "gather"))` replaces
      `engine` in `R/set_opt.R`: `backend` is the real formal, `engine` stays a
      `lifecycle::deprecated()` sentinel folded in by the existing
      `fold_renamed_arg()` helper. The constructor maps `backend` to the token
      the compiled interface expects, so nothing downstream changes; the
      returned object carries the resolved value under the name estimation
      already reads. Roxygen for both arguments (deprecated badge on `engine`);
      `devtools::document()`. Tests: each value selects the same path its
      legacy counterpart did; the default is `"cpp"`.
- [x] 1.2 Legacy values map with one warning (D4): `default_c → cpp`,
      `default → r`, `gather_compute → gather`, accepted whether supplied to
      `engine` (old argument) or to `backend` (half-migrated call), each
      producing exactly one soft-deprecation warning naming the final spelling
      — no two-hop. Invalid values abort with a cli error listing the new
      vocabulary. Tests: snapshots for argument-deprecation, value-mapping, and
      the invalid-value error; supplying both arguments resolves to `backend`.
- [x] 1.3 Estimation gating messages speak the backend vocabulary:
      maxLik-requires-`cpp`, the `gather` rejections
      (`return_event_scores`, and any other), and the coordination redirect
      text if still present. Grep gate: no user-facing message names
      `default_c` / `default` / `gather_compute`. Tests: snapshots for each
      re-pointed message.

## 2. Call-site migration

- [x] 2.1 Migrate the test suite and vignettes to `backend =` (grep gate: no
      `engine =` outside the deprecation tests and their snapshots). Vignette
      sources and their precompiled outputs move together — the rename changes
      no printed result. Full `NOT_CRAN=true` suite with baselines PASS: the
      frozen coefficients must be untouched, since no numerical path changes.

## 3. Living-spec sweep (D2, D3)

- [x] 3.1 Verify the delta set against the living spec before implementing
      anything else in this section: every `## MODIFIED` header must exist
      verbatim in `openspec/specs/<capability>/spec.md`, or be the `TO` of a
      `RENAMED` block in the same delta. `openspec validate` checks
      SHALL/scenario structure but NOT section placement, so a block written
      under the wrong `##` silently becomes an ADD at archive and leaves the
      old wording in place. Confirm the deltas cover: optimizer-selection (4),
      broadcast-stat-updates (3), likelihood-computation (2 + 1 renamed),
      flat-preprocess-output (2), active-availability-stat (1),
      multimode-networks (1), single-data-object (1).
- [x] 3.2 Confirm the deliberate non-renames are still deliberate, so a
      reviewer can tell "skipped" from "missed": `likelihood-computation`'s
      "C++ REM and coordination estimators use staged BLAS form" and "C++
      multinomial normalizers use the shared stable softmax" describe the
      implementation, whose tokens D1 keeps; generic prose
      ("cross-engine tolerance", "formula→engine boundary") is domain
      vocabulary, not the argument. Record the list in progress.md.
- [ ] 3.3 Cross-change coherence: sweep the OTHER active changes' artifacts for
      legacy engine values used as user-facing vocabulary (residuals-gof's
      optimizer-selection delta, and any change minting new requirements
      against `engine =`), leaving implementation references alone.
      `revise-gather-output` is excluded — it owns `preprocess-output-writers`
      and already writes it under the final vocabulary (D5). `openspec
      validate` green for every change touched.

## 4. Closure

- [ ] 4.1 NEWS entry: the rename with the value map
      (`default_c → cpp`, `default → r`, `gather_compute → gather`), the
      sentinel, and the ≥3.0.0 removal horizon shared with the rest of the
      2.0.0 alias layer. DESCRIPTION version bump. Full `NOT_CRAN=true` suite
      green with baselines PASS.

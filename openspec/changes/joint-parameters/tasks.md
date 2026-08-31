> **Sequencing (cross-change).** This change ships the shared parameter surface
> `abmcem` (`set_algorithm_em(initial_parameters=)`) and `process-simulation`
> (`simulate(coef=)`) both consume, so it lands **before** `abmcem`'s
> `estimate_dynes()` surface and before `process-simulation` wires `coef`. It has
> no dependency on either — a pure projection over the archived
> `make-multivariate-spec` object (`process_map` + the parsed offset mask).

## 1. `set_parameters()` and the `parameters.goldfish` object

- [x] 1.0 **Build-time offset-value enforcement** (design D8): `make_joint_specification()`
      rejects any joined process with an offset term lacking an inline `coef=`
      (assert `offset_coef_parameter` non-`NA` at every `offset_parameter`
      position of every fid) so D4/D6 can rely on the value being in the spec. Its
      `cli_abort` points **only** to `offset(term, coef = value)` (mirroring
      `estimate_flavored.R:64`) — **not** `set_algorithm_newton(offset_coef=)`,
      which is closed for joint specs. Test: bare-offset joint spec aborts at
      build (snapshot); inline-`coef=` joint spec builds. **Audit sweep**: check
      existing joint-spec construction sites for bare offsets that D8 now rejects
      at build — the joint-spec test fixtures (`test-preprocess_joint.R`,
      `test-complete_generative_spec.R`, `test-walk_handle.R`,
      `test-make_joint_specification.R`) and `vignettes/multivariate-specification.R`
      — and migrate any to inline `coef=` so the vignette build does not break
- [x] 1.1 `parameters.goldfish` S3 class (house `<noun>.goldfish` convention):
      carries validated per-fid values, **two projections** (design D11 — the flat
      **free-parameter** vector for `estimate_dynes()`, and the **full per-fid
      coefficient vectors** for `simulate()`), both in the **canonical order**
      (`process_map` fid order, then coefficient order within each fid), the
      complete flag, and layout metadata; `print()` via cli semantic elements
      (grouped by process, fixed/free marked); `devtools::document()`
- [x] 1.2 `set_parameters(spec, ...)` (design D1–D3, D7, D9, D10): key resolution
      by **membership** against the rendered process label (`render_process_label()`,
      the `layer › flavor › family` form `coef()`/`print()` use — keyed on
      `family`, flavor elided when the process carries none; never split the key
      back into components); an unresolvable/ambiguous (colliding or
      separator-bearing) key aborts naming valid labels; full-length per-fid
      vectors **in coefficient-layout order** (`coefficient_term_labels()`,
      `[intercept?, effects, interactions]`, length `n_params` — design D9, **not**
      the raw rhs-aligned parser list), length mismatch aborts naming the fid;
      per-effect names optional but **all-or-nothing** (design D3 — fully named
      validated against effect labels, or fully positional; a **partially named**
      vector is rejected)
- [x] 1.2b **Single canonical label renderer** (design D10): teach
      `render_process_label()` `NA`-flavor elision (drop the flavor segment when
      `NA`, keeping the `›`-segment form) and route `walk_handle.R`'s completeness
      message through it, retiring its parenthesized ad-hoc format — so
      `set_parameters()` keys and `coef()`/`vcov()`/`print()` names are one
      vocabulary. Snapshot the shifted non-flavored name (`friendship › rate`, was
      `friendship › NA › rate`)
- [x] 1.3 NA disambiguation (design D4, D9, D11): classify each slot from the
      offset mask **projected into coefficient space** (reuse the `intercept_shift`
      projection `assemble_fixed_parameters()` computes — do **not** read the raw
      rhs-aligned `offset_coef_parameter` directly). Fixed prevails: at an offset
      or **operand-only (fixed-at-0)** slot a non-NA value warns once and is
      ignored, NA is silent; NA at a free slot is free; non-NA at a free slot pins
      it. The user-authored rate Intercept is a free slot. An omitted **authored**
      process key is all-free (design D13). **Autocompleted defaults are NOT
      classified here** (design D16 — they are not in the authored spec
      `set_parameters()` sees; they resolve at consumer entry, task 2.2). Set the
      complete flag over the authored fids (design D5, complete iff every free slot
      is filled)
- [x] 1.4 Tests (testthat 3e): membership resolution (rendered-label match) +
      flavor elision + separator/collision abort, wrong-length
      abort snapshot, optional-name validation, offset-prevails warn snapshot,
      free/fixed classification and complete-flag fixtures
- [x] 1.5 Verification: `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      commit. **No DESCRIPTION version bump and no root `NEWS.md` edit on this
      branch** — record the parameters-surface milestone as a bullet in the
      change-local `openspec/changes/joint-parameters/NEWS.md`. The version bump,
      root-NEWS fold, and archival are deferred to branch merge (see
      progress.md "Version / NEWS / archival")

## 2. `coef_layout()` generic and consumer wiring

- [x] 2.1 `coef_layout()` generic + methods (design D6, D12): one row per
      **coefficient-space slot** (`n_params`: intercept, effects, interactions —
      not one per effect); `joint_specification` (empty layout for authoring),
      `parameters.goldfish` (values + free/fixed), fitted result (θ̂/SE); columns
      `fid`, process label, `sub_model`, `flavor`, effect **name** (mirror
      `coefficient_term_labels()`: `"Intercept"` for the rate intercept, interaction
      label for interactions, `"1"` placeholder for an autocompleted-default slot;
      no intercept row where `estimate_dynam()` surfaces none), `fixed` (offset ∪
      operand-only ∪ autocompleted-default), fixed value, `index`;
      `devtools::document()`. **Completion-aware `joint_specification` method
      (design D16a)**: a completed spec is still class
      `joint_specification.goldfish` (discriminated by its populated `completed`
      column), so this is the **same** method reading content — a **raw** spec
      spans authored fids only; a **completed** spec
      (`coef_layout(complete_generative_spec(spec))`) also renders the
      autocompleted fids' rows `fixed = TRUE` with their frozen values (the full
      pre-fit walked layout). The `parameters.goldfish` method spans **authored**
      fids only (built from the raw spec). Test: raw-spec layout omits
      autocompleted rows; completed-spec layout includes them as fixed
      (design D16 boundary)
- [x] 2.2 Consumer acceptance surface: a shared coercion/validation entry that
      `set_algorithm_em(initial_parameters=)` and `simulate(coef=)` call to
      accept **only** a `parameters.goldfish` (v1). `estimate_dynes()` reads the
      **free-θ** projection (fixed values from the spec); `simulate()` reads the
      **full per-fid coefficient** projection and the completeness assertion
      (design D5, D11) aborts on any free NA naming the effects. **Consumer-entry
      autocomplete reconciliation (design D16)**: the object is over the authored
      fid set; after the consumer completes the spec, any fid **absent from the
      object** (an autocompleted zero-free-parameter default) is treated as
      trivially resolved — needs no user value, does not make the object
      incomplete. (The consumer call sites themselves live in `abmcem` /
      `process-simulation`; this change ships the acceptance/assertion helper they
      import.) The value gate and
      `walk_open()`'s structural gates are **complementary** (design D15 — values
      vs. spec shape); `simulate()` runs the value gate first, `walk_open()`'s
      asserts are a spec-layer backstop this change does not duplicate.
- [x] 2.2b `set_parameters()` **from-result** form (design D14):
      `set_parameters(spec, result)` — spec supplied explicitly, fitted
      joint/DyNES result in place of the per-fid vectors; **assert the result was
      fit against that same `spec`** (matching fid vocabulary / `coef_layout()`)
      and abort on mismatch; reconstruct per-fid vectors from the result's
      `coef_layout()` (keeps the `fid` grouping flat `coef()` discards) for the fit
      → re-simulate round-trip; a flat free-only `coef()` vector is **not** accepted
      directly (cross-fid name collision), nor a bare `set_parameters(result)`.
      Test: round-trip fixture (fit → `set_parameters(spec, result)` → complete
      object → `simulate()`); spec/result-mismatch abort
- [x] 2.3 Tests: empty-layout authoring fixture, result-layout grouping used by a
      stub `summary()`, completeness-assertion abort snapshot, partial object
      accepted for estimation
- [x] 2.4 Verification: `NOT_CRAN=true` (PASS not SKIP); commit. **No DESCRIPTION
      version bump, no root `NEWS.md` edit, no archive on this branch** — record
      the coef_layout milestone as a bullet in the change-local
      `openspec/changes/joint-parameters/NEWS.md`. Version bump, root-NEWS fold,
      and `/opsx:archive` are deferred to branch merge (see progress.md
      "Version / NEWS / archival")

## 3. Class-naming migration to `goldfish<Thing>` camelCase (design D17)

> Added 2026-08-28 (explore pass). `class-naming-scheme` retires the
> `<noun>.goldfish` house convention task 1.1 built `parameters.goldfish`
> under; this section renames it, and the `joint_specification.goldfish`
> class this change consumes throughout, to the new scheme before archive
> — see design D17 for why this change executes both renames itself rather
> than waiting on `class-naming-scheme` (unstarted) to fold. Target
> spellings are recorded in `class-naming-scheme`'s rename table (design
> D16 there): `parameters.goldfish` → `goldfishParams`,
> `joint_specification.goldfish` → `goldfishJointSpec`.

- [x] 3.1 Rename inventory (design D9's hand-edit-only discipline, borrowed
      from `class-naming-scheme`): grep every class-string site for both
      names across `R/` (`make_joint_specification.R`, `joint_parameters.R`,
      `intercept_only_rate.R`, `model_estimate.R`, `preprocess_joint.R`,
      `complete_generative_spec.R`, `walk_handle.R`, `methods_display.R`),
      roxygen `@method`/`@export` tags, NAMESPACE, and
      `tests/testthat/{test-estimate_joint_guard,test-joint_consumer_parameters,
      test-joint_parameters,test-coef_layout,test-intercept_only_rate,
      test-make_joint_specification}.R` plus their `_snaps/*.md`; record
      exact sites in `progress.md`. Confirm no site is a substring collision
      (`joint_specification.goldfish` contains `specification.goldfish`,
      so rename the longer string first).
- [x] 3.2 Rename `parameters.goldfish` → `goldfishParams`: class strings,
      `inherits()`/`is()` checks, `class<-`/`structure(class =)` values,
      the `print()`/`coef_layout()` S3 methods (`print.goldfishParams`,
      `coef_layout.goldfishParams`), roxygen `@method` tags;
      `devtools::document()`.
- [x] 3.3 Rename `joint_specification.goldfish` → `goldfishJointSpec`:
      same edit surface in `R/make_joint_specification.R` and
      `R/complete_generative_spec.R` (`rebuild_completed_joint()`'s
      reassigned class, design D16a); `coef_layout.joint_specification` →
      `coef_layout.goldfishJointSpec`; `devtools::document()`. Record the
      rename as a `## RENAMED Requirements` block in this change's own
      `specs/multivariate-specification/spec.md` (design D17 — the class's
      producer is the archived `make-multivariate-spec` capability, folded
      into the living spec this change already deltas).
- [x] 3.4 Update snapshots (`tests/testthat/_snaps/{intercept_only_rate,
      complete_generative_spec,make_joint_specification,estimate_joint_guard,
      joint_from_result}.md`) for the new class strings; review each diff,
      never accept wholesale (mirrors `class-naming-scheme` design D11).
- [x] 3.5 Cross-change grep: confirm `abmcem`, `dynes-augmentation`, and
      `process-simulation` already spell `goldfishParams`/`goldfishJointSpec`
      in their specs (updated 2026-08-28, same session) — no further sweep
      needed there. If any of the three has since drifted back to the old
      spelling, fix it here too.
- [x] 3.6 Tests (testthat 3e): assert both renamed objects inherit their
      new class and not the retired one; extend or add alongside the
      existing `test-make_joint_specification.R` / `test-joint_parameters.R`
      suites (a dedicated package-wide guard test is `class-naming-scheme`'s
      own task 1.3, not duplicated here).
- [x] 3.7 Verification: **not-cran-test** (`NOT_CRAN=true`; frozen 1e-6 and
      C++ goldens PASS, not SKIP); `openspec validate joint-parameters
      --strict`; commit. Record the rename as a bullet in the change-local
      `openspec/changes/joint-parameters/NEWS.md` (breaking: two renamed
      classes) — version bump and root `NEWS.md` fold stay deferred to
      branch merge per §1/§2 above.

## 4. Rename `set_parameters()` to `set_init_param()` (design D18)

> Added 2026-08-31 (naming review). A direct rename request, not a sibling
> change's retitled convention like §3 — see design D18 for the naming-fit
> tradeoff (the name serves `estimate_dynes()`'s starting-point framing better
> than `simulate()`'s generative-coefficient one, documented rather than
> resolved) and the full edit-surface inventory (messages, snapshots,
> cross-change docs, living-spec delta) this rename touches beyond the
> function definition itself.

- [x] 4.1 Rename inventory (mirrors 3.1's discipline): grep every
      `set_parameters` site — the definition and its unexported
      `set_parameters_from_result()` helper (`R/joint_parameters.R`), every
      `{.fn set_parameters}` token inside a `cli_abort()`/`cli_warn()` call
      (message text, not just call sites), roxygen `@seealso`/backtick
      mentions in `intercept_only_rate.R` and `walk_handle.R` (and any other
      file a grep turns up), the four owning test files' call sites and
      `test_that()` description strings, and the three `_snaps/*.md` files.
      Record exact sites in `progress.md`.
- [x] 4.2 Rename `set_parameters()` → `set_init_param()` and
      `set_parameters_from_result()` → `set_init_param_from_result()`:
      function definitions, internal call sites (the from-result dispatch in
      the renamed main function), every `{.fn set_parameters}` message token,
      the roxygen block (`@title`/prose/`@param`/`@return`/`@seealso`,
      including the `[set_parameters()]` cross-links this file and
      `intercept_only_rate.R`/`walk_handle.R` carry); `devtools::document()`.
      `git rm man/set_parameters.Rd` (roxygen does not delete an orphaned
      `.Rd`) once `man/set_init_param.Rd` is generated.
- [x] 4.3 Update the four owning test files (`test-joint_parameters.R`,
      `test-coef_layout.R`, `test-joint_consumer_parameters.R`,
      `test-joint_from_result.R`): every call site, `test_that()` description
      string naming the old function, and file-header comments.
- [x] 4.4 Regenerate and review the three affected `_snaps/*.md` files
      (`joint_parameters.md`, `joint_consumer_parameters.md`,
      `joint_from_result.md`) via `testthat::test_file()` +
      `snapshot_accept()`; review each diff individually — it should be
      exactly the `set_parameters` → `set_init_param` text swap in the
      abort/warning message, never accepted wholesale (mirrors 3.4).
- [x] 4.5 Cross-change sweep (mirrors 3.5): update `set_parameters()`
      mentions in `abmcem/design.md`, `dynes-augmentation/tasks.md`, and
      `process-simulation/design.md` to `set_init_param()`. Confirm no other
      in-progress change's artifacts reference the old name.
- [x] 4.6 Living-spec delta (mirrors 3.3): add a `## RENAMED Requirements`
      FROM/TO block in this change's own
      `specs/multivariate-specification/spec.md` retitling
      `### Requirement: set_parameters builds a validated parameter object
      over the joint spec`; sweep every other requirement body in that file
      naming `set_parameters()` inline (the NA-disambiguation,
      complete-vs-partial, and `coef_layout()` requirements) to the new name.
      `.plan/opsx-spec-placement-check.sh joint-parameters` and
      `openspec validate joint-parameters --strict` both pass.
- [x] 4.7 Tests: a `test_that()` asserting `set_init_param` is exported and
      `set_parameters` is not (`expect_true(exists("set_init_param", where =
      asNamespace("goldfish")))` / the retired name absent from
      `getNamespaceExports("goldfish")`), alongside the existing suites.
- [x] 4.8 Verification: **not-cran-test** (`NOT_CRAN=true`; frozen 1e-6 and
      C++ goldens PASS, not SKIP); `openspec validate joint-parameters
      --strict`; commit. Record the rename as a "Breaking (pre-release)"
      bullet in the change-local `openspec/changes/joint-parameters/NEWS.md`
      and update the existing bullets' `set_parameters()` mentions to the new
      name — version bump and root `NEWS.md` fold stay deferred to branch
      merge per §1/§2/§3 above.

## 5. Flavoured-specification test coverage for `set_init_param()` (design D19)

> Added 2026-08-31 (naming review). Closes a coverage gap found while
> reviewing the test suite: every fixture the three owning test files define
> joins plain, unflavored layers, so nothing exercises the label grammar's
> flavor-inclusion case or the proposal's own motivating scenario (the same
> effect name recurring across fids). See design D19 for the full fixture
> rationale and why `helper-flavored-fixtures.R`'s fixtures cannot be reused
> directly (not a `goldfishJointSpec`).

- [x] 5.1 New flavored joint fixture (design D19): a `calls` layer built with
      `add_flavor()` (`creation`/`dissolution`) and
      `make_specification(rate = list(...), choice = list(...))`, joined via
      `make_joint_specification()` with a plain `emails` layer. All three
      fids' choice formulas carry `inertia` (reproducing the proposal's
      motivating collision — one name, three fids — for the first time in a
      test fixture). **Fixed-parameter coverage is load-bearing, not
      optional**: `calls`'s two flavors each carry their own inline-`coef`
      offset on the *same* term, at two distinct values —
      `offset(tie(friendship), coef = -0.3)` for `creation`,
      `offset(tie(friendship), coef = 0.4)` for `dissolution` — while
      `emails`'s choice carries no offset (stays fully free). No existing
      fixture in this change has more than one offset in the whole join, so
      this is the first fixture that can catch a fixed value resolved from
      the wrong fid. Local to `test-joint_parameters.R` (own generator,
      matching the file's existing `parameters_join()` pattern), reused by
      5.5/5.6 below rather than re-defined.
- [ ] 5.2 Test: flavor-inclusion labels alongside an elided one on the same
      object — `calls › creation › rate`, `calls › dissolution › choice`, and
      `emails › rate` (no flavor segment) are all valid `set_init_param()`
      labels on the fixture from 5.1 (the mirror of the existing
      "labels elide the flavor segment for a non-flavored process" test).
- [ ] 5.3 Test: same-name (`inertia`) **free**-slot resolution across
      flavors — per-fid vectors keyed `calls › creation › choice` and
      `calls › dissolution › choice` each pin their own `inertia` slot
      without leaking into the sibling flavor's slot or `emails › choice`'s.
- [ ] 5.4 Test: per-flavor **fixed**-slot classification (extends D4) — on
      the fixture from 5.1, `creation`'s and `dissolution`'s
      `tie(friendship)` offsets resolve to their own distinct values (`-0.3`
      / `0.4`) on one `set_init_param()` call; a non-`NA` value supplied at
      *one* flavor's fixed slot warns naming only that flavor's slot and
      leaves the sibling flavor's resolved value, classification, and
      warning count unaffected. Test: omitted-key coverage across flavors
      (extends D13) — omitting one flavor's key leaves only that flavor's
      non-fixed slots free; the sibling flavor and `emails` are unaffected.
- [ ] 5.5 Test (`test-coef_layout.R`): `coef_layout()` on the flavored
      authored spec from 5.1 (or a locally adapted copy) carries a non-`NA`
      `flavor` column for the `calls` fids' rows and elides it for `emails`'s,
      on the same layout call; the fixed-value column reads `-0.3` for
      `creation`'s offset row and `0.4` for `dissolution`'s — not each
      other's value or `NA` (extends D6/D12).
- [ ] 5.6 Test (`test-joint_from_result.R`): a fabricated fitted result over
      the flavored fixture (following the existing
      `fabricate_joint_result()` pattern) round-trips through
      `set_init_param(spec, result)` correctly — the free `inertia` slots
      land at the right flavor's fid despite the shared name, **and** each
      flavor's fixed slot keeps the specification's own offset value (`-0.3`
      / `0.4`), not a value read off the result, silently and with no
      warning (extends D14).
- [ ] 5.7 Test: a colon-grammar key (`calls:creation:rate`, the retired D2
      `:`-grammar) still aborts naming the valid labels against the flavored
      fixture, now proven where a naive string-split would have actually
      resolved something (extends the existing ambiguous-key negative test,
      exercised non-vacuously per D7).
- [ ] 5.8 Verification: **not-cran-test** (`NOT_CRAN=true`; frozen 1e-6 and
      C++ goldens PASS, not SKIP); commit. Record the new flavored coverage
      as a bullet in the change-local `openspec/changes/joint-parameters/NEWS.md`
      — version bump and root `NEWS.md` fold stay deferred to branch merge
      per §1/§2/§3/§4 above.

## 6. Switch the joint coefficient vocabulary to the console form (design D12 correction)

> Added 2026-08-31 (naming review). The coefficient `name` column and the
> per-fid vector keys `set_init_param()` accepts currently use
> `coefficient_term_labels()` (the raw deparse `inertia(calls, window = "1 hour")`),
> but that is **not** what a fit surfaces to the user: `summary()` renders the
> self-describing console form `inertia [1h,W]`. See the D12 correction for the
> three-scheme analysis, the decision (joint = console via
> `compact_term_strings(names, "console")`; single-process stays terse coef,
> deliberately), and why intercept/interaction slots render identically under
> both schemes so the switch touches effect rows only. Sequence after §4 so the
> function is already `set_init_param()`; independent of §5 but shares its
> fixtures.

- [ ] 6.1 Build the effect-description matrix from a spec, pre-fit. Add a small
      helper (in `R/joint_parameters.R` or alongside `fid_coefficient_layout()`)
      that produces, per fid, the same `names` matrix estimation builds —
      `GetDetailPrint(get_objects_effects_link(parsed$rhs_names, …), parsed)`
      (`model_estimate.R:2515` is the estimation-side reference call) — from the
      bundle's `parsed` and the spec's referenced objects, with the `fixed`
      column supplied from the layout's own `assemble_fixed_parameters()` result
      rather than a fitted `is_fixed`. Confirm the spec-built matrix reproduces
      the intercept row (`"Intercept"`) and interaction rows (keyed by
      `$label`) identically to a fitted result's (design D12 correction,
      intercept/interaction paragraph). Record the exact call shape in
      `progress.md`.
- [ ] 6.2 Point `fid_coefficient_layout()` (`R/joint_parameters.R:105`) and
      `coef_layout.goldfishJointSpec` (`:730`) at
      `compact_term_strings(names, "console", width = Inf)` over the 6.1 matrix,
      replacing the `coefficient_term_labels()` call for the `names`/`coef_names`
      slot. Keep the coefficient **order** (`[Intercept?, effects, interactions]`,
      length `n_params`) and the fixed mask/values exactly as today — only the
      name strings change.
- [ ] 6.3 Point `coef_layout.flavored_result.goldfish` (`:786`) off
      `term_label(sub$names, ".coef_name", "coef")` onto the **same** console
      form (`compact_term_strings(sub$names, "console", width = Inf)`), so the
      from-result round-trip skeleton comparison — which includes the `name`
      column (`set_parameters_from_result()`, `:347`) — matches spec-side and
      result-side name-for-name. This is the tie that makes the round-trip
      correct rather than coincidentally-passing on bare effects.
- [ ] 6.4 Fixture: extend the §5.1 flavored fixture (or a local companion) so at
      least one fid carries an **argument-bearing** effect —
      `inertia(calls, window = "1 hour")` or equivalent with a weight/transformer
      — the minimum that makes the deparse and console forms differ. Without it
      no test distinguishes `inertia [1h,W]` from the old deparse string and the
      round-trip `name`-column agreement stays vacuously true (design D19,
      "Related gap"). Reuse across 6.5/6.6.
- [ ] 6.5 Tests: on the 6.4 fixture, (a) `coef_layout(spec)$name` for the
      windowed slot reads the console form (`inertia [1h,W]`, not
      `inertia(calls, window = "1 hour")`); (b) a `set_init_param()` call keyed
      by that console name resolves the slot, and the old deparse string is
      **rejected** (unknown-name abort), pinning the vocabulary; (c)
      `coef_layout(spec)$name` and `coef_layout(result)$name` agree on the
      windowed slot (the round-trip precondition 6.3 restores); (d) the intercept
      and an interaction slot render identically to the pre-switch strings
      (`"Intercept"` and the interaction `$label`) — the no-regression guard for
      the two slots the D12 correction says are unaffected.
- [ ] 6.6 Regenerate the affected `_snaps/*.md` files (`print.goldfishParams`
      output in `joint_parameters.md`, and any `coef_layout` snapshot carrying a
      `name` column) via `testthat::test_file()` + `snapshot_accept()`; review
      each diff individually — for bare-effect fixtures it must be a **no-op**
      (deparse == console there), and a change should appear **only** where an
      argument-bearing effect was added in 6.4. A wholesale accept would hide a
      naming regression on the unchanged rows.
- [ ] 6.7 Living-spec delta: sweep this change's
      `specs/multivariate-specification/spec.md` for any requirement body that
      pins the coefficient `name` to `coefficient_term_labels()` / the deparse
      form (the `coef_layout()` and NA-disambiguation requirements) and retitle
      the source to the console form; `.plan/opsx-spec-placement-check.sh
      joint-parameters` and `openspec validate joint-parameters --strict` pass.
      (If no requirement pins the exact form, note that in `progress.md` and skip
      — do not invent a `## MODIFIED` block for text that does not exist.)
- [ ] 6.8 Verification: **not-cran-test** (`NOT_CRAN=true`; frozen 1e-6 and C++
      goldens PASS, not SKIP); `openspec validate joint-parameters --strict`;
      commit. Record the vocabulary switch as a bullet in the change-local
      `openspec/changes/joint-parameters/NEWS.md` (user-visible: pins and
      `coef_layout()` now name coefficients as `summary()` does) — version bump
      and root `NEWS.md` fold stay deferred to branch merge per §1–§5 above.

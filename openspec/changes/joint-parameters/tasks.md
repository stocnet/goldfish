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

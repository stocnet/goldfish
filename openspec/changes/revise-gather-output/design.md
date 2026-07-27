# Design — revise-gather-output

## Context

Two exported functions overlap: `gather_model_data()`
(R/preprocess_export.R) wraps `compute_stats(..., output = "gather")`
(R/model_estimate.R:532) but re-validates with its own narrower `match.arg`
(`choice`/`choice_coordination`/`rate`; models DyNAM/REM only), while
`compute_stats()` → `estimate_wrapper()` already validates the full
per-model vocabulary via `check_model_par()` (model_estimate.R:955-964:
DyNAM = rate/rate_ordered/choice/choice_coordination, REM =
rate/rate_ordered/choice, DyNAMi = choice/rate) and hosts the REM
`"choice"` deprecation warn+remap. Verified empirically:
`compute_stats(REM, rate_ordered, output = "gather")` produces the correct
ordinal stack today; `gather_model_data()` rejects the same call.
`check_model_par()` (R/class_checks.R:1083) is base `stop()`, pre-cli.
DyNAMi preprocessing runs through an isolated legacy front-end
(model_estimate.R:763-770, "fenced so the shared recipe path is
DyNAMi-free") — the gather writer lives in the shared path, so DyNAMi
gather support requires routing work. Flavored specifications already have
an identity convention: preprocessing returns fid-indexed
`preprocessed.goldfish` lists with a `process_map` table attribute (fid,
layer, flavor, family, stat_block, has_intercept, constraint_id), and the
estimation container renders labels from that map (flavored-processes
living spec). The residuals-gof change (active) defines the replay surface
this function feeds: consumers accept `preprocessed =`, and its
diagnostic-primitives guiding error currently names
`estimate_*(..., preprocessing_only = TRUE)` as the supply route.
A repo-wide naming pass (`algorithm-naming`, memory note 2026-07-24) runs
AFTER this change settles; interim decisions here are scoped to the new
function's own signature.

## Goals / Non-Goals

**Goals:**

- One statistics-product function, `compute_statistics()`, covering
  everything `gather_model_data()` does plus frames; deprecations for the
  old surfaces.
- Vocabulary correctness by delegation; `check_model_par()` on cli.
- DyNAMi coverage; flavored fid-list outputs with `process_map`.
- Reported intercept/censoring semantics; frame contract; verified
  cross-package recipes as examples.

**Non-Goals:**

- Renaming estimator arguments or any surface beyond the new function
  (deferred to `algorithm-naming`, which re-checks all names once this
  change settles).
- Changes to the gather stack format or writer strategy contract.
- NCC sampling, remstats-style arrays, simulation exports.

## Decisions

### D1 — `compute_statistics()` absorbs `gather_model_data()`; asymmetric retirement of the old names

`compute_stats()` is renamed `compute_statistics()` (precision; full-word
naming). Signature: `compute_statistics(x, model, sub_model = NULL,
data = NULL, output = c("preprocessed", "gather", "data.frame", "db"),
control_prep = set_preprocessing(), progress, max_length = 63L, ...)` —
first argument `x` (formula or specification, matching `estimate_*()`),
model selectors before `data` (the `estimate_*(x, sub_model, data)`
family order), and the final control names from `algorithm-naming`,
which implements BEFORE this change (2026-07-24 alignment session).
Retirement is asymmetric, keyed on release exposure:

- `gather_model_data()` **shipped in released versions** (the 1.7 rename
  line) → lifecycle soft-deprecated wrapper for at least one release cycle
  (`deprecate_soft`, direct pointer, no two-hop chains).
- `compute_stats()` **only ever existed inside the unreleased 2.0.0
  development line** (introduced in a NEWS dev section; never on CRAN) →
  **deleted outright** (decided 2026-07-24; supersedes the brief
  defunct-stub variant — a stub is still kept code, and there is no
  released user it would serve): removed from NAMESPACE and source, no
  stub; calling it yields R's standard could-not-find-function error, and
  the NEWS entry records the rename so dev-line scripts can grep their way
  to `compute_statistics()`. This is a deliberate exception to the
  deprecate-soft-at-2.0.0 policy, which exists to protect released
  users — `compute_stats` has none.

Alternative rejected: keeping the layered two-function split (previous
D1) — the explore session settled on one precise function; presentation
layers multiply doc surfaces and the vocabulary drift was caused by
exactly such a wrapper. Coordination: `algorithm-naming` implements
first, so `set_preprocessing()` and the `control_prep` vocabulary exist
when this function is born — it arrives fully final and
`algorithm-naming` has no code task against it; it does not touch the
`compute_stats` lifecycle.

### D2 — Vocabulary by delegation; `check_model_par()` upgraded to cli

`compute_statistics()` performs no local `match.arg` on model/sub_model;
validation happens once in `estimate_wrapper()` via `check_model_par()`,
which is upgraded (touched-internal migration) to `cli_abort` listing the
allowed sub_models for the given model. The REM `"choice"` deprecation
message at its single site (`estimate_wrapper()`) names both successors:
`"rate"` (exact-time) and `"rate_ordered"` (ordinal). `sub_model = NULL`
keeps the current defaulting (REM → "rate", else "choice"), now documented
together with its intercept consequence. No-drift-by-construction: there is
no second vocabulary to fall out of sync.

### D3 — Intercept/censoring semantics inherited and reported at finalization

`rate` (exact-time): force-added time intercept, right-censored rows with
`timespan`; `rate_ordered`: no intercept, no censored rows, formula `1`
dropped with the estimator's message — all inherited from the shared
parsing path, never reimplemented. `has_intercept` and `right_censored`
fields are attached in `finalize_gather_output()`, so every output form
and every entry point carries them; for flavored outputs they mirror the
`process_map` columns. Documentation mandates name-based statistic-column
access (the positional trap).

### D4 — Flavored outputs are fid-indexed lists with `process_map`

On a flavored specification, every `output` form returns a list indexed by
integer fid carrying the same `process_map` table attribute as flavored
preprocessing and the estimation container — the identity authority.
Display labels (messages, print) are rendered from the process_map, never
parsed back from list keys. Alternative rejected: composite string names
(`submodel_layer_flavor`) as list keys — the flavored-processes spec
explicitly forbids key-parsing, and fid+map is what `estimate_*` already
returns; one keying convention across the package.

### D5 — DyNAMi is covered, with the front-end routing verified first

`compute_statistics()` SHALL support `model = "DyNAMi"` for the outputs
its engines can produce. Because the DyNAMi front-end is fenced off the
shared recipe path, the first implementation task verifies whether the
gather writer can be routed (or the legacy output post-converted) and
records the answer; if a DyNAMi output form is genuinely unavailable, it
aborts with a cli error naming the supported forms — never a silent wrong
result. (DyNAMi's joining/leaving structure is extra parameters within one
fit, not fid flavors — its output is a single stack.)

### D6 — Frame output (`output = "data.frame"`) and example recipes

The long base data frame: `event` (integer), `chosen` (0/1), `sender`,
`receiver` (labels; NA where not applicable), `index_i`, `index_j`
(1-based), `timespan` (exposure; NA for multinomial rows), `is_dependent`
(FALSE = right-censored row), then statistic columns named by
`names_effects`; `effect_description` as attribute; per-fid list under
flavoring (D4). Help-page recipes (verified in
`.plan/residuals_comparison.qmd`): ordinal ↔ `coxph(Surv(rep(1, n),
chosen) ~ stats + strata(event))` and `clogit` (one case per stratum ⇒
tie methods coincide ⇒ exact conditional logit); exact-time ↔
`glm(chosen ~ stats + offset(log(timespan)), poisson)` over dependent +
right-censored rows; choice ↔ `mlogit` via `dfidx` with `option = index_j`
(real alternative identity). Guarded with `@examplesIf
requireNamespace(...)`, fits in `\donttest`.

### D7 — Final naming from birth; this change owns the `preprocessing_only` retirement

`compute_statistics()` is born under the final vocabulary
(`control_prep = set_preprocessing()`, `x`-first signature; the interim
`control_preprocess` spelling is superseded — 2026-07-24 alignment
session, `algorithm-naming` implements first). The estimator control
arguments (`control_estimation` → `control_algo`, `control_preprocessing`
→ `control_prep`, `preprocessing_init` → `preprocessed`) are renamed by
`algorithm-naming`, NOT here. One estimator flag is retired HERE because
its replacement is this function: `preprocessing_only = TRUE` is
soft-deprecated with a warning naming
`compute_statistics(output = "preprocessed")` (through 2.x it still
returns the preprocessed object) — deprecating it in `algorithm-naming`
would have pointed users at a function that did not exist yet.

### D8 — Replay-surface coherence with residuals-gof

`compute_statistics(output = "preprocessed")` IS the preprocessed replay
object residuals-gof's consumers take via `preprocessed =` — the value
names the class it returns and mirrors the argument it feeds (chosen over
`"default"`, which leaned on undocumented engine vocabulary, and
`"compact"`, which requires knowing the delta/broadcast internals). The
diagnostic-primitives guiding-error wording in the residuals-gof change is
updated (before its task 1.8 implements it) to name
`compute_statistics(output = "preprocessed")` as the supply route
alongside `return_preprocessed = TRUE` (the flag's post-algorithm-naming
name). This change adds no second replay path; `algorithm-naming` dropped
its `make_preprocessed()` sketch in favor of this single route
(2026-07-24 alignment session).

### D9 — Products are rendered after the constraint folds, and the gather expansion honors the availability encoding

Discovered while implementing D4 (2026-07-25): `output = "gather"` and
`output = "db"` are broken today for **any** model carrying a
`support_constraint`, flavored or not. Verified pre-existing on `HEAD`, and not
covered by a test — 20 test files exercise `support_constraint`, none with a
gather output. Two independent defects:

**(A) Rendering precedes constraint realization.** `writer_gather()$finalize()`
converts the assembled object into the gather stack, and only then does
`finalize_consumers()` call `finish_output()`, which realizes the mask with
`preprocess_support_mask(snapshot_times = out$event_time)` and folds it into
availability. The stack carries no `event_time`, so this raises
`order(): argument 1 is not a vector`. The ordering is wrong in principle, not
only in its error: the fold has to precede the expansion, or the expansion
enumerates candidates the mask excludes — the opposite of what the
`preprocess-output-writers` requirement "constrained gather emits only allowed
candidates" states.

**(B) The gather expansion drops the availability encoding.** Folding a
constraint upgrades availability from the `alter` encoding (a length-n2 vector)
to `point` (an n1 x n2 matrix). `estimate_c_int()` reads
`statsList$active_dyad_encoding` and flattens accordingly; `gather_from_prep()`
never passes it to `gather_()`, which then falls back to its own `"alter"`
default and reads the matrix as a vector — `which(mask == 1)` yields up to
n1*n2 linear indices, so the row index runs past the statistics matrix and
raises `subscript out of bounds`. (B) is independent of (A): it bites whenever
the availability is point-encoded, including when a stored constrained object is
replayed into a stack, where the fold has already happened correctly.

**Decision.** The writer contract gains a **render stage**, and the encoding is
forwarded:

```
finalize_consumers()
  out <- writer$finalize(tail)        # ALWAYS the assembled default shape
  out <- finish_output(out, constraint)   # mask realized + folded, per consumer
  writer$render(out, spec)            # the product
```

`render()` is identity for `writer_default()`, `gather_from_prep()` for
`writer_gather()`, and inherited by `writer_db()` (persistence stays where it is,
in `write_gather_to_db()` after the names resolve). `gather_from_prep()` passes
`active_dyad_encoding` through to `gather_()` exactly as the estimation path
does.

Two consequences worth stating, because they are why this belongs here rather
than in a follow-up:

- It is the *only* ordering under which flavored gather output is correct. The
  fold is per consumer (`finish_output(out, cspec$constraint)`), so rendering
  after it gives each fid a stack constrained by its own derived mask — which is
  what D4's "per-fid stacks match single-flavor runs with the derived
  constraint" asserts. Mutually exclusive flavors always derive masks, so
  **every** flavored gather hits this path.
- The consumer writers must therefore be the output's writers, not always
  `writer_default()`: `build_consumer_specs()` takes the factory for the
  requested output. Per-fid naming is then the union description projected onto
  that consumer's `effect_map`, which the consumer plan already carries.

*Alternative rejected:* leave the writer contract alone and render flavored
products by re-entering `estimate_wrapper()` per fid with the stored object
(`preprocessed = prep, output = "gather"`). It works — a replayed stack is
byte-identical to a direct one — and it gets per-flavor naming free from the
re-parse. But it still requires (B), it leaves the direct
`compute_statistics(spec, output = "gather")` path broken for constrained
models, and it introduces a second product-shaping route for the same product,
which is the duplication this whole change exists to remove. The replay path
still has to work (a stored object is the D8 replay surface), so the supplied-
object conversion is kept — it is just no longer the mechanism flavoring
depends on.

**Implementation note (2026-07-25, tasks 2.1/2.2 as landed).** The shipped
flavored mechanism IS the per-fid re-entry (`flavored_statistics_output()`
re-entering `estimate_wrapper()` per fid) — but the objection above did not
materialize, because each re-entered run flows through the D9 writer contract
itself: there is one product-shaping route (finalize → finish_output →
render), invoked per fid by the re-entry rather than duplicated beside it.
The 2.1 render stage independently fixed the direct constrained
single-flavor path (its own tests), and the per-fid equality test passes
precisely because both routes share it. Consequences: 2.1(c)'s writer-factory
threading is correct but a no-op on the flavored path
(`preprocess_flavored()` always preprocesses with `writer_default`, the flat
shape re-entry needs); and 2.2's flavored `output = "db"` abort is temporary
scaffolding that task 2.3 (D10) removes.

### D10 — The db export is self-describing: one table per fid, effect-named columns, map and node tables

`output = "db"` is available for flavored specifications too (decided
2026-07-25, superseding an implementation sketch that refused it). The db route
exists for data that does not fit in memory, so the exported schema has to be
readable without the R session that produced it:

```
stats_1, stats_2, …   one long table per fid, named <db_table>_<fid>
stats_map             the process_map, plus each fid's table name
stats_nodes           side, local, global, label -- shared by every fid
```

- **One table per process, not one shared table.** Rejected alternative: a
  single table with a `fid` column. Flavors carry different formulas, so their
  statistic sets differ; one table would need the union of all effects with
  NULLs wherever a process does not have that effect, and every query would
  filter by fid anyway. Per-fid tables keep each process's schema exactly its
  own.
- **The schema is uniform: an export is always K processes, K >= 1.** A
  single-process export writes `<db_table>_1` and a one-row map, not a bare
  `<db_table>`. A consumer reads the map, then the tables it names, without
  knowing or asking whether the run was flavored — and the same script keeps
  working when a model later gains flavors. The one-row map is not degenerate:
  it names the layer, family, and intercept of the process whose table it
  points at. This mirrors how the package already thinks internally, where
  `init_consumers()` returns a one-element consumer list for the non-flavored
  case and `finalize_consumers()` reads `consumers[[1L]]`; fids stay 1-based,
  so a single process is fid 1 under exactly the numbering flavoring uses.
  Cost: the pre-existing single-table name changes. That is free by this
  change's own precedent (D1) — `output = "db"` arrived in 1.8.0 and the last
  CRAN release is 1.6.12, so the whole db surface, table name and `stat_<i>`
  columns alike, exists only in the unreleased 2.0.0 line and has no released
  users to protect.
- **The R return value stays asymmetric on purpose**: a single process returns
  its descriptor, a flavored one a fid-keyed list. The two surfaces have
  different consumers. A database is a published artifact, read later by other
  tools and other people, where a stable schema beats convenience; an R return
  is read immediately by the caller, who knows what they asked for and should
  not have to write `[[1]]` for the common case. A future reader should not
  "fix" this asymmetry into uniformity without that argument changing.
- **Statistic columns are named by effect, not `stat_<i>`.** The positional
  naming is a trap this change exists to close (the proposal's *Why* cites it
  as hit in practice), and it is worse per fid, where `stat_2` means different
  effects in different tables. `names_effects` is already valid, unique and
  bounded by `max_length = 63L`, documented as "a database-safe value" -- the
  identifier limit this default was chosen for. The reserved identity columns
  (`event_id`, `is_selected`, `index_i`, `index_j`) participate in the
  uniqueness pass, so an effect name colliding with one is disambiguated rather
  than silently overwriting it. This applies at every export, single or
  flavored.
- **Rows exclude what the constraint excludes.** The db writer persists the
  rendered gather stack, so with D9's ordering (fold, then render) a
  `support_constraint` removes candidate rows before they are ever written, and
  `n_candidates` on the descriptor reflects the constrained set. This is the
  property the whole schema rests on: rows that reach the database are the rows
  the model actually had in its risk set.
- **The identity columns are the join back to the original data.**
  `index_i`/`index_j` are sanitized local indices, which alone cannot address
  the original `nodes` rows on a subset or two-mode model; `<db_table>_nodes`
  carries `(side, local, global, label)` so the join is doable in SQL. It is
  written once per export, since every fid of a flavored specification shares
  the layer's node set.
- **The map table is the authority for what belongs to a run.** Re-running with
  fewer flavors than a previous run leaves orphan `<db_table>_<fid>` tables
  behind; they are NOT dropped automatically (a prefix-matching drop would be a
  destructive guess against tables the connection may own for other reasons).
  The map lists exactly the tables of the current run.
- **A mid-export failure names the process.** The existing contract reports the
  last successfully written event index; with several tables it also names the
  fid whose table failed, since "event 812" is otherwise ambiguous across
  processes.

## Risks / Trade-offs

- [Retiring exported names] → `gather_model_data()` keeps working one
  cycle under `deprecate_soft` with a direct mapping; `compute_stats()` is
  deleted with no stub — justified by zero release exposure
  (dev-line-only) and recorded in NEWS; dev-line scripts fail loudly at
  the call site (could-not-find-function), never silently.
- [DyNAMi routing unknowns] → D5's verify-first task; cli error rather
  than silent unsupported output; scope can shrink to a documented
  limitation without touching the rest.
- [Flavored gather/frames volume (per-fid stacks)] → same order as flavored
  preprocessing already produces; row-count formula documented per fid; db
  writer remains the out-of-memory route.
- [Estimator-frame drift despite delegation] → parity test: the qmd
  identity route (clogit on the frame vs `estimate_dynam` choice) becomes
  a unit test with a ≤1e-4 gate.
- [Cross-change edit inside residuals-gof] → one wording edit in an
  unimplemented requirement; validated in both changes after the edit.
- [D9 changes the writer contract mid-change, and the writers are shared with
  estimation] → the render stage moves *when* the conversion happens, not what
  it computes; `writer_default()`'s render is identity, so the estimation path
  (which consumes the default product) is unchanged by construction. The
  unconstrained gather output is pinned byte-for-byte against
  `gather_model_data()` by existing tests, which is the regression net for the
  move; the constrained case gains the tests it never had.

### D11 — `backend` replaces `engine` — MOVED to the `backend-vocabulary` change

Recorded here on 2026-07-25 and carved out the same day. The rename itself is
small, but the vocabulary it retires reaches 16 living requirements across 8
capabilities — broadcast decoding, multimode equivalence, the data object —
none of which this change otherwise touches. Folding it here would make this
change's archive rewrite requirements unrelated to statistics export, the same
shape `algorithm-naming` was carved out for. See `backend-vocabulary` for the
decision, the value map, and the sweep.

Consequence here: `backend-vocabulary` implements FIRST, so this change writes
its own requirements under the final vocabulary — `backend = "gather"`, the
`cpp` backend — and the `optimizer-selection` delta moved out with it. The
division is strict: that change does not touch `preprocess-output-writers`,
this one does not touch `optimizer-selection`, so no requirement is modified by
both (which would mean whichever archived second silently overwrote the first).

## Migration Plan

1. `compute_statistics()` rename + delegation + cli `check_model_par` +
   reported fields (D1–D3); deprecation wrappers.
2. Writer render stage + encoding forward (D9) — a prerequisite for D4, since
   every flavored gather is a constrained gather. Then flavored fid-list
   outputs (D4); DyNAMi verification then routing (D5).
3. Frame output + examples + NEWS/DESCRIPTION (D6); residuals-gof wording
   edit (D8); qmd consumer switch.
4. Rollback: wrappers are self-contained; the rename is alias-backed, so
   reverting restores the current surface without preprocessing changes.

## Open Questions

- None blocking. (Suggests entries for survival/mlogit decided
  mechanically by R CMD check on the guarded examples; final names
  re-checked by `algorithm-naming` once this settles.)

# Design — backend-parity

## Context

`backend-vocabulary` (complete; archives before this change implements) renamed
`set_algorithm_newton(engine =)` to `backend = c("cpp", "r", "gather")`, values
that name what runs. That made a latent question user-facing: choosing `"r"`
because it is the reference implementation, or `"gather"` because it is faster
on a wide risk set, silently changes which per-event diagnostics the fit comes
back with.

That rename also deliberately stopped at the constructor (its D1):
`set_algorithm_newton()` maps `backend` onto the legacy engine token at
`set_opt.R:367` and everything downstream still reads
`control_algo$engine ∈ {"default_c", "default", "gather_compute"}`. The seams
this leaves, verified on the tree:

- the token is *documented API* — the control object's `$engine` component is
  in the roxygen `@return`, so `set_algorithm_newton(backend = "cpp")$engine`
  is `"default_c"`, visible surface carrying the retired vocabulary;
- the gating messages in `model_estimate.R:1223-1291` read the token and call
  `engine_backend()` to translate it *back* to backend vocabulary for display —
  a round-trip that marks the internal name as being on the wrong side of the
  boundary;
- the branch between compiled paths is decided entirely in R
  (`model_estimate.R:2147`, `estimate_c_int()` is unexported) — no token
  reaches C++ as data, so the "don't touch the compiled interface" fear that
  motivated the boundary protects only comments and one internal `match.arg()`;
- `functions_preprocess_em.R` still calls
  `set_algorithm_newton(engine = "default")` five times — the package's own
  code on its own deprecated surface;
- the fitted `result.goldfish` records nothing about which backend produced it
  (verified against the results assembly in `model_estimate.R:2195-2260`), so
  a diagnostics consumer cannot tell a `gather` fit from a `cpp` fit;
- the word "engine" is additionally overloaded by `new_model_spec()`'s
  unrelated spec-level `engine` field (`model_spec.R:398`, reserved
  `"incremental"`), giving the same word three meanings.

Measured on the current tree (`estimate_wrapper()` on the `dataTest` fixture,
DyNAM-choice, one `diagnostics` request per cell):

| primitive | `cpp` | `r` | `gather` |
|---|---|---|---|
| `loglik` | yes | yes | yes |
| `scores` | yes | yes | **aborts** (explicit request only) |
| `ranks` | yes | **absent, no message** | **absent, no message** |
| `margins` | yes | **absent, no message** | **absent, no message** |
| `probabilities` | **redirects to `r`** | yes | **redirects to `r`** |

Three failure modes for one class of problem. They also interact: the
`return_probabilities` redirect runs *before* the scores/gather abort in
`estimate_wrapper()`, so

```r
set_algorithm_newton(backend = "gather", diagnostics = c("loglik", "scores"))
#> Error: The "scores" diagnostic ... is not supported with `backend = "gather"`

set_algorithm_newton(backend = "gather", diagnostics = "all")   # a superset
#> Warning: `backend = "gather"` does not support `return_probabilities`.
#> i Estimating with `backend = "r"` instead.
#> ... returns event_scores
```

Adding an unrelated primitive to the request turns a hard abort into a success
on a backend the user did not choose.

Task 0.1's re-measurement found the artifact reaches further than that. Because
the redirect fires for *any* backend, `diagnostics = "all"` also silently
strips `ranks` and `margins` from a **`cpp`** fit — the one backend that
implements them — by moving the fit onto `r`, which does not:

```r
set_algorithm_newton(backend = "cpp", diagnostics = c("loglik","scores","ranks","margins"))
#> carries loglik, scores, ranks, margins
set_algorithm_newton(backend = "cpp", diagnostics = "all")   # a superset
#> carries loglik, scores, probabilities — ranks and margins silently gone
```

Asking for *more* returns *less*, on the default backend, with no message about
the two primitives that vanished. This is the strongest argument for D3: the
redirect is not a convenience with an edge case, it is a backend substitution
whose primitive-level consequences are invisible.

**Key code facts, verified before writing this design:**

- Flag coverage across the nine compiled kernels — all six `*_default.cpp`
  (the `cpp` backend) implement `return_event_scores`, `return_ranks` and
  `return_margins`; none of the three `compute_*_selection.cpp` (the `gather`
  backend) implement any:

  ```
                                       scores ranks margins stable_softmax
    DyNAM_choice_default.cpp              y     y      y          y
    DyNAM_rate_default.cpp                y     y      y          -   (timed)
    DyNAM_rate_ordered_default.cpp        y     y      y          y
    DyNAM_MM_default.cpp                  y     y      y          y
    REM_default.cpp                       y     y      y          -   (timed)
    REM_ordered_default.cpp               y     y      y          y
    compute_multinomial_selection.cpp     -     -      -          -
    compute_poisson_selection.cpp         -     -      -          -
    compute_coordination_selection.cpp    -     -      -          y
  ```

- `compute_multinomial_selection.cpp:65-66` computes the per-event score and
  discards it: `derivative += currentEffect.row(id_receiver);
  derivative -= expected_stat_current_event;`. That increment is exactly what
  `DyNAM_choice_default.cpp:178-184` stores into `event_scores.row(id_event)`.
- The gather stack already materializes `index_i` / `index_j` per row for every
  model, in **two** builders (`cpp_interface.R:1350-1351` and `1461-1462` — the
  dyad-indexed and the sender-set shapes); the coordination-only
  `sender_of_row` / `dyad_partner` already reach the kernel dispatch, the
  general indices do not.
- The `r` backend forms the probability vector every event
  (`estimation_core.R:843-845`, `stable_softmax(linearPredictor)$probabilities`)
  and returns it only as the whole `pMatrix`.
- The existing parity tests do not compare native `r` ranks/margins against
  `cpp` — they reconstruct them in R from `return_probabilities`
  (`test-diagnostic_primitives.R`, `ranks_from_probabilities(fd)` and
  `Reduce(`+`, fd$eventProbabilities)`), which is the direct evidence that the
  `r` backend has no native accumulator.
- The frozen 1e-6 coefficient baselines are keyed `default` / `default_c` only:
  `baselines_backends <- c("r", "cpp")`. **No gather coefficient is frozen.**
- `likelihood-computation` :: "C++ multinomial normalizers use the shared stable
  softmax" is scoped to "the `default_c` multinomial normalizer loops ... so
  both engines have identical overflow behavior". Gather was never in scope;
  its naive `arma::exp` is an unclosed gap, not a regression.

## Goals / Non-Goals

**Goals:**

- One declared, testable answer to "which backend produces which primitive",
  with the same failure mode everywhere.
- Every primitive available on every backend where it is mathematically defined,
  which — per the reduction identity in D1 — is every backend.
- Numerical agreement per primitive across backends, at a stated tolerance, on
  the same fixtures.
- A single place where each reduction is written, so a future primitive is added
  once rather than nine times.
- One vocabulary, everywhere it is observable: the backend values are the only
  runtime names downstream of the constructor, the control object and the
  fitted result both speak them, and the legacy tokens survive solely in the
  input-side compatibility map. (This supersedes `backend-vocabulary`'s
  non-goal that kept the internal tokens; the grounds are in D8.)

**Non-Goals:**

- Renaming the `*_default.cpp` kernel *files* or the compiled function names —
  spec and comment prose that names those files keeps their real names.
- `new_model_spec()`'s spec-level `engine` field (reserved `"incremental"`) —
  a different axis; the push actually disambiguates it as the only remaining
  meaning of the word.
- The `output` vocabulary of `compute_statistics()` and the
  `preprocess-output-writers` capability (owned by `revise-gather-output`).
- Extending maxLik optimizers beyond `backend = "cpp"`. Gather gaining
  per-event scores makes BHHH's observation-level gradients *computable*
  there, but the adapter memoizes through the cpp evaluator closure and no
  demand exists; the requirement stays cpp-only, stated here so the parity
  sweep is not read as implying the extension (decided 2026-07-25).

- New primitives beyond the five `diagnostics` already names.
- The `opportunities_list` backend redirect — a preprocessing-capability
  restriction, not a per-event primitive; it keeps its current behavior.
- Adopting the stable softmax on the timed hazard path. `stable_softmax.h`
  records this as a deliberate Non-Goal ("its scale is absolute"), and this
  change does not reopen it.
- Changing which backend is the default, or any coefficient on `r` / `cpp`.
- The `diagnostics` vocabulary, defaults and storage guardrail — owned by
  `residuals-gof`'s `diagnostic-primitives` capability (see D6).
- **Sweeping the EM prototype's vocabulary — the fragment leaves `R/`
  instead** (D18). Not an exclusion with a dangling pointer: the two files are
  removed by this change rather than maintained in either vocabulary.

## Decisions

### D1 — One per-event reduction: `(w_e, c_e)`, not "the probability vector"

The tempting framing is that everything reduces from the per-event probability
vector. That is true for the multinomial family and false for the timed one,
where the per-event object is a rate vector on an absolute scale and the fitted
"expected count" carries the interval length. The framing that covers both:

at each event `e` every backend forms a nonnegative weight vector `w_e` over the
risk set and a scalar scale `c_e`, and the expected-count contribution of
alternative `j` is `m_ej = c_e * w_ej`.

**Amended 2026-07-25 (D20):** `w_e` is the **probability vector `p_e` in every
family**, and `c_e` alone carries the family difference. The contract value
`m_ej` is unchanged — only its factorization is:

| family | `w_ej` | `c_e` | `sum_j m_ej` |
|---|---|---|---|
| multinomial (choice, rate_ordered, REM_ordered, coordination) | `p_j = exp(x_j - lse)` | `1` | `1` |
| exact-time (rate, REM), probability scale | `p_j = exp(x_j - lse)` | `1` | `1` |
| exact-time (rate, REM), compensator scale | `p_j = exp(x_j - lse)` | `Δt_e * T_e` | `Δt_e * T_e` |

This is the same `m` as the original `(rate, Δt)` factorization, by Appendix
eq. (5): `m_ej = Δt_e * λ_ej = Δt_e * T_e * p_ej`. Grounds for preferring it are
in D20 — one weight vector per event feeding both exact-time scales, and the
only large factor living in a scalar rather than inside the vector.

The four primitives are then the same four reductions in both families:

```
  probabilities  :  m_e                                    (stored)
  ranks          :  1 + #{ j in risk set : w_ej > w_e,obs }   (scale-free)
  margins        :  expected[actor(j)] += m_ej   ;  observed[actor(obs)] += 1
  event_scores   :  X_e,obs * 1{dependent}  -  m_e' X_e
```

`ranks` is scale-invariant, so `c_e` never enters it and the code is byte-identical
across families. `margins` and `event_scores` differ only in which `c_e` is
supplied. This is not a new definition imposed on the code — it is what the six
`*_default.cpp` engines already do, read back: `DyNAM_choice_default.cpp:169-174`
accumulates `weights(j) / normalizer`, `DyNAM_rate_default.cpp:180-181`
accumulates `timespan_current_event * exp_current_sender`.

Rejected: defining the contract per submodel (six definitions, six test
matrices, and the next submodel starts from nothing). Rejected: normalizing the
timed family into a probability *and dropping the compensator* so one formula
covers both — that would divide out `Δt` and lose the property that timed
margins total the expected number of events, which is what makes them
comparable to observed counts. D20 keeps both scales; it only changes which of
the two factors is the vector.

### D2 — A shared C++ reduction header, mirrored once in R

`src/stable_softmax.h` is the precedent, including its stated purpose:
"mirroring the R `stable_softmax()` helper so the `default` and `default_c`
engines share the same numerics". The same shape applies here — one header of
small inline reductions taking `(w_e, c_e, X_e, obs, index)` and nothing else.

One clarification, since the word "mirror" does two jobs in this change: the
reductions below are a **strict** mirror — same inputs, same outputs, pinned to
each other by a parity test on constructed values. `stable_softmax()` and
`log_sum_exp_masked()` are only an *algorithmic* mirror: they share the
max-shift but differ in parameters (`rowwise` vs a mask) and in what they return
(derived scales vs the normalizer plus shifted weights). Do not read the
precedent as implying a symmetry that was never there — see D19.

```
   src/event_reductions.h  ── consumed by all 9 kernels
        rank_of_observed(w, allowed, obs)          -> int
        accumulate_margins(w, c, allowed, idx, obs, &obs_acc, &exp_acc)
        event_score_row(X, w, c, obs, dependent)   -> rowvec
                     |
                     |  mirrored, not linked (R cannot call the inline C++)
                     v
   R/estimation_core.R  ── the same three reductions on the R backend's
                           per-event probability vector
```

**Two-sided margins (task 0.2 finding).** Three of the six `cpp` kernels
scatter one per-alternative contribution into *two* accumulators —
`REM_default.cpp:296-298` and `REM_ordered_default.cpp:196-197` add to sender
and receiver margins, `DyNAM_MM_default.cpp:262-263` to both endpoints of a
coordination tie — while the three single-sided kernels scatter into one. So
`accumulate_margins()` takes a *side list* rather than one `(idx, acc)` pair:
one call per event, N sides, each a `(index vector, observed index,
observed accumulator, expected accumulator)` tuple sharing the same
`(w, c, allowed)`. Folding the two-sided kernels onto a one-sided helper would
mean calling it twice and iterating the risk set twice, which for REM's
`n1 × n2` inner loop is a real cost, not a stylistic one.

The reductions depend on no loop state, no presence buffers, no broadcast
decoding — which is precisely why they are extractable from an event-loop engine
and a gather kernel alike.

Rejected: exporting the C++ helpers to R via Rcpp so there is literally one
implementation. The R backend's inner loop is R-level; calling into C++ per event
would cost more than the reduction and would make the R backend depend on
compiled code it exists to be independent of. The mirror + parity test is the
established trade, and D5 makes the parity test the thing that keeps them honest.

### D3 — The capability contract is data, checked once, before preprocessing

A single table keyed `(backend, primitive)` — plus, where it matters, the
submodel family — is the one source of truth. `estimate_wrapper()` consults it
once, before any preprocessing, and an unsupported combination aborts with one
cli error naming the backends that do support that primitive. Nothing silently
disappears and nothing silently changes backend.

After this change the table is expected to be uniformly supported; it exists
anyway, because (a) the abort has to say something specific while the work lands
incrementally, and (b) the next primitive will not be universal on day one, and
the mechanism should already be there rather than being invented as a fourth
failure mode.

This replaces: the `gather` + `scores` abort in `estimate_wrapper()`, the silent
`return_event_scores <- FALSE` drop beside it, the `return_probabilities`
backend redirect, and the unmarked absence of `ranks` / `margins`. Removing the
probabilities redirect is what fixes the ordering artifact in Context — there is
no longer a mutation of the backend for one primitive that changes the verdict
for another.

Rejected: keeping the redirect for `probabilities` "because it always worked".
It answers a question the user did not ask (they chose a backend), and it is the
mechanism behind the inconsistency.

### D4 — Gather adopts the stable softmax, and the baselines say what that costs

Extending `likelihood-computation`'s stable-softmax requirement to the gather
multinomial and Poisson kernels is in scope because the reductions in D1 consume
`w_e`, and leaving one backend's `w_e` on naive `exp` would mean the primitives
agree only to that backend's overflow behavior — parity tests that pass on
well-conditioned fixtures and diverge exactly where the diagnostic matters.

The cost is bounded by a fact worth stating plainly: **no gather coefficient is
in the frozen baseline set** (`baselines_backends <- c("r", "cpp")`; the
`.rds` is keyed `default` / `default_c`). Gather is constrained only by
tolerance-based cross-backend agreement tests, and a max-shift changes results
only where naive `exp` overflows — which the well-conditioned fixtures do not.
So this is the cheap moment to close the gap, and it must not be confused with
the `default_c` adoption, which did move a frozen floor.

**Clarified 2026-07-25 — the split is per quantity, not per kernel.** The
original wording ("the gather multinomial and Poisson kernels adopt the stable
softmax") is wrong for the Poisson kernel and contradicted this decision's own
closing sentence. A max-shift is only invariant where the normalizer enters as a
*ratio*:

```
  multinomial   intervalLogL = log( exp(x_obs) / Σ exp(x_j) )   a RATIO
                every other use is p_j = exp(x_j)/Σ, also ratios
                ⇒ the shift cancels; adopting it is exact AND fixes the
                  underflow (x_obs − lse instead of log(p_obs))

  exact-time    intervalLogL = −Δt · Σ exp(x_j) + x_obs         Σλ ENTERS RAW
                ⇒ shifting would give Σλ·e^(−m): a different model, not a
                  stabilization. The likelihood keeps the raw total rate.
```

So the timed *likelihood* keeps plain `exp()`, per the Non-Goal — but the timed
*kernel* still adopts the log-sum-exp, as the substrate for the quantities that
are ratios or logs (D19). Overflow in the likelihood is already handled: the
Newton loop gates step acceptance on `is.finite(logLikelihood)`
(`cpp_interface.R:453`), so an overflowing step is rejected and damping backs
off, rather than producing a silent wrong answer.

### D5 — Parity is proven per (primitive × submodel family × backend pair), at declared tolerances

The reductions are algebraically identical, so the agreement is exact up to
accumulation order, and the tolerance should say which:

| primitive | comparison | tolerance | why |
|---|---|---|---|
| `ranks` | integer equality | exact | a count of strict inequalities; only ties are fragile, and ties are handled by the same `>` rule everywhere |
| `margins` | numeric | 1e-10 | one scatter-add per alternative, same terms, different order |
| `event_scores` | numeric | 1e-10 | the existing cross-backend score tolerance |
| `probabilities` | numeric | 1e-10 | same softmax on the same predictors |

Every comparison is made **at a fixed parameter vector** (`max_iterations = 0`,
`initial_parameters` set from a converged fit), not at each backend's own
optimum: converged estimates agree only to the ~1e-6 cross-backend tolerance,
which is far coarser than the reductions themselves and would hide a real
discrepancy. This is the pattern the existing `event_scores` parity test already
uses.

The two existing tests that reconstruct `r` ranks and margins from
`return_probabilities` become direct three-way comparisons; keeping the
reconstruction as a *third* independent expectation for one fixture is cheap
insurance that the shared reduction did not standardize a shared mistake.

### D6 — Strict capability division with `residuals-gof`, settled up front

Both changes touch `optimizer-selection` :: "User-facing return_event_scores
option": `residuals-gof` modifies it for the `diagnostics` soft-deprecation
prose, and this change must modify it because the `gather` clause stops being
true. Two changes carrying a version of the same requirement means whichever
archives second silently overwrites the first — the failure `backend-vocabulary`
hit and had to repair.

The division:

- **this change owns** the new `backend-primitive-parity` capability and the
  backend clauses of `optimizer-selection` and `likelihood-computation`;
- **`residuals-gof` owns** `diagnostic-primitives` (the vocabulary, defaults,
  guardrail, legacy-flag deprecation) and everything downstream of the
  primitives;
- `residuals-gof`'s `optimizer-selection` delta is **rebased onto this
  change's wording** as a task here, keeping its diagnostics prose and taking
  this change's backend clause, so either archive order lands the same text.

`diagnostic-primitives` does not exist in the living spec yet — `residuals-gof`
adds it — so this change does not modify it, and the parity contract goes into
its own capability rather than pre-empting one it does not own.

### D7 — Refactor the six working engines last, and only onto the header

The six `*_default.cpp` engines already produce correct primitives. Moving them
onto the shared header is a pure refactor with a real regression surface (they
are the frozen 1e-6 floor), and it buys nothing until the header exists and is
proven by the backends that currently lack the reductions.

So: header first, then `gather` and `r` written against it (where the tests are
new and a bug shows up as a parity failure, not a baseline break), then the six
engines folded onto it as the last structural step, with the baselines as the
gate. If the fold turns out to perturb a baseline, the header is already
delivering its value on the other two backends and the fold can be dropped
without stranding the change.

### D8 — The vocabulary push: backend values are the only runtime names

`control_algo$backend ∈ {"cpp", "r", "gather"}` replaces the token field. The
gates and dispatch in `model_estimate.R` compare backend values;
`estimate_c_int()` becomes `backend = c("cpp", "gather")`; `engine_backend()`
and its message round-trip are deleted; the `BACKEND_ENGINE_TOKENS` /
`LEGACY_ENGINE_BACKENDS` pair survives only as the *input-side* map that folds
`engine =` and legacy values with one warning (unchanged behavior). Internal
callers migrate off the deprecated surface (`functions_preprocess_em.R` →
`backend = "r"`).

Why fold this here rather than a separate change: D3's capability check
rewrites exactly the gating block (`model_estimate.R:1223-1291`) that carries
the tokens — folding means that block is written once, in the final
vocabulary, instead of being written in tokens and renamed later. And the fact
that decided `backend-vocabulary`'s boundary — protecting the compiled
interface — is moot: the backend branch is decided in R, `estimate_c_int()` is
unexported, and no token crosses into C++ as data. The push is a pure string
rename with no numerical path; the frozen 1e-6 baselines staying PASS is the
per-task proof.

Sequencing within the change: the push is tasks section 1, before any `src/`
work, so every later task (kernels, capability table, parity suite) is written
under the final names.

Rejected: leaving the tokens in place (every future gate perpetuates the
round-trip, and the retired vocabulary stays documented API via `$engine`);
renaming only the field but keeping token values (worst of both).

### D9 — `$engine` is dropped from the control object at 2.0.0, with a read shim

The control object carries `$backend` and no `$engine`. The component has been
public since v1.7.0 — `set_estimation_opt()`'s returned `estimation_opt.goldfish`
list carried `engine = <token>` from its introduction (verified against the
v1.7.0 tag), with the roxygen `@return` item added later in 1.9.x — so per the
deprecation-scope audit (D11) its removal is a genuine breaking change, taken
at the 2.0.0 major version with a NEWS entry. Estimation's control-object gate
grows a one-line shim: a list carrying only `$engine` (built by any pre-2.0.0
constructor — `set_estimation_opt()` since 1.7.0 or `set_algorithm_newton()`
in 1.9.x — e.g. constructed once in a user script or restored from an `.rds`)
has its token translated to the backend value on read, so old control objects
keep working without a warning cascade. The shim is fully silent — no message
and no lifecycle signal: the deprecated surface already warned at construction
time, and warning again at estimation time would punish serialized objects
twice (decided 2026-07-25).

Rejected: writing both fields through 2.x — it preserves in the return value
precisely the two-vocabulary confusion this change exists to end, for readers
who by definition are on a deprecated surface the shim already serves.

### D10 — The fit records its backend, written once in the results assembly

`result.goldfish` gains a `backend` component (`"cpp"` / `"r"` / `"gather"`),
written at a single point in the `### 6. RESULTS` assembly in
`model_estimate.R` — not at the three `class<-` sites (`estimation_core.R`,
`cpp_interface.R` ×2), which would triple the write and invite drift. The
roxygen `@return` component list documents it; the synthetic fit in
`zzz_testthat_helpers.R` carries it so test doubles look like real fits.

The consumer contract is the point: the (backend × primitive) capability table
of D3 makes primitive availability a function of the backend, so a
post-estimation consumer — `residuals-gof`'s `diagnose_*()` above all — needs
the fit to say what ran in order to abort with "refit with
`backend = "cpp"`" rather than a generic missing-component error. Pre-2.0.0
fits lack the field; consumers SHALL treat `NULL` as unknown backend and fall
back to component presence, never error on the absence itself. Display in
`print()` / `summary()` is left to `residuals-gof`'s diagnostics surface —
this change stores the fact, it does not restyle output.

### D11 — Deprecation scope: the public surface is CRAN (1.6.x) plus tag v1.7.0

Lifecycle ceremony (sentinels, `deprecate_soft`, mapping warnings, shims) is
owed only to surfaces that shipped in a public release. The public surface is
the CRAN series (1.6.x, where `estimate(estimationInit = list(...))` exposed
`engine`, `returnIntervalLogL`, `returnEventProbabilities`) and the v1.7.0
tag (the last tagged release: `set_estimation_opt()` with `engine =
c("default_c", "default", "gather_compute")`, `return_interval_loglik`,
`return_probabilities`, `convergence_criterion`, and the returned list's
`engine` component). Everything introduced in 1.7.1+ exists only on the
development branch and is removed or renamed outright, without deprecation.

The audit for the surfaces this change and `residuals-gof` touch:

| surface | public? | consequence |
|---|---|---|
| `engine =` argument | CRAN + 1.7.0 | sentinel + one warning stays (backend-vocabulary) |
| legacy values `default_c` / `default` / `gather_compute` | CRAN + 1.7.0 | value map + soft warning stays |
| `$engine` control-object component | 1.7.0 | BREAKING at 2.0.0 + D9 read shim |
| `return_interval_loglik`, `return_probabilities` | CRAN (camelCase) + 1.7.0 | soft-deprecation onto `diagnostics` stays (residuals-gof) |
| `return_event_scores` | never (1.8.x dev only) | **removed outright at 2.0.0** — no sentinel, no warning (residuals-gof implements) |
| `estimate_c_int(engine =)` | never exported | free rename |
| fit's `backend` component | new | nothing to deprecate |

The `return_event_scores` row is the decision shared with `residuals-gof`,
whose task 1.2 had already soft-deprecated all three flags uniformly; the
never-public third flag now comes out entirely, and its spec home (the
`optimizer-selection` requirement) is renamed accordingly — see the revised
D6.

### D6 (revised) — This change owns the scores requirement wholesale; residuals-gof drops its optimizer-selection delta

The original D6 had both changes carrying a version of `optimizer-selection`
:: "User-facing return_event_scores option", reconciled by a rebase task. The
D11 removal decision makes that untenable: a requirement *titled after* a
removed argument cannot be the shared landing text, and a RENAMED block
duplicated across two active changes breaks at whichever archives second
(the FROM header no longer exists).

Resolution: this change renames the requirement to "Per-event scores
primitive" and owns its full text — the scores primitive requested via
`diagnostics` (vocabulary still owned by `diagnostic-primitives`), honored by
all three backends, the never-shipped legacy flag removed rather than
deprecated. `residuals-gof`'s `optimizer-selection` delta is **deleted**; its
deprecation prose lives where it always belonged, in its own
`diagnostic-primitives` capability (two public flags soft-deprecated, the
third removed). No requirement is modified by both changes, in either archive
order — the hazard is gone rather than managed.

### D12 (revised 2026-07-25) — margins are probability-scale everywhere; exact-time fits add the labeled expected-count variant

Originally this decision labeled a single margins vector with its family's
construction. Superseded the same day by the uniform-scale insight: every
family forms the probability scale `p_ej = w_ej / sum_j w_ej` — the
multinomial choice probability and, on exact-time sub-models, the
competing-risks probability that `j` creates the *next* event — so
probability-margins `sum_e p_ej` (over dependent events only — a
right-censored interval realizes no mover, so it feeds the expected-count
variant alone; Appendix item 4) are the parallel-to-choice calibration map
in both families, totalling the event count at **any** parameter vector.
Exact-time fits additionally store the expected-count variant
`sum_e Δt_e λ_ej` (the compensator, including right-censored intervals),
whose total equals the event count only at the MLE (the intercept
score-equation identity) and whose per-actor observed-minus-expected is the
martingale residual. Both vectors ship under the one `"margins"` primitive,
each labeled (`"probability"` / `"expected_count"`); multinomial fits carry
`"probability"` only. The shared helper is unchanged by the dual storage:
`accumulate_margins()` takes `c_e` and knows nothing of labels — it is
called with `c = 1/sum(w)` for the probability scale (both families) and
`c = Δt` for the compensator (exact-time only); labels attach at the R-side
result assembly (one site). The shape requirement lives in `residuals-gof`'s
`diagnostic-primitives` capability; this change's reductions feed it. Full
algebra in the Appendix. Rejected: document-only (a consumer cannot branch on
prose); split primitive names (a vocabulary fork for one concept on two
scales); expected-count-primary (breaks the parallel-to-choice default).

### D13 — `total_rate` is part of the loglik parity contract

On exact-time sub-models the `loglik` primitive includes the per-event
`total_rate` on every backend, asserted in the parity suite. The gather
Poisson kernel already computes it as `normalizer` and discards it (task 3.3
wires it). **Task 0.1 settled the `r` question: the R backend has no
`total_rate` accumulator at all** — the name appears only in
`DyNAM_rate_default.cpp` and `REM_default.cpp` (plus their R-side plumbing in
`cpp_interface.R`), so an exact-time `r` fit carries `intervalLogL` but no
`total_rate`. Task 4.1 therefore builds it rather than wiring it, and D17's
conditional component (which divides by `T_e`) cannot be assembled on `r` or
`gather` until it exists. The storage
*vocabulary* (that `total_rate` rides `"loglik"`) remains
`diagnostic-primitives`' statement; this change specs the all-backends
availability and agreement. Resolves the second Open Question. `total_rate`
is also the **bridge between the two exact-time scales** (Appendix, eq. 5):
`m^c_ej = Δt_e · T_e · p_ej`, so probability-scale primitives plus
`total_rate` span the compensator scale, which is what makes D12's dual
margins and D17's stored conditional component assembly-side derivations
rather than kernel work.

### D14 — Coordination ranks use the same scale-free rule over the whole realized risk set

Rank-of-observed is `1 + #{alternatives in the event's realized risk set with
w > w_obs}` regardless of the CSR group structure — the scale-free rule needs
no group notion, so coordination is not a fourth family. A coordination
fixture joins the three-way parity suite (task 5.3); if task 0.1's grounding
finds the groups semantically load-bearing for ranks, that is an escalation
back to design, not a silent reinterpretation. Rejected: per-group ranks (a
different diagnostic — quality of choice given the sender — nobody asked
for); deferring coordination ranks (breaks "every primitive everywhere it is
mathematically defined" for no savings). Resolves the third Open Question.

### D15 — A gather coefficient baseline is frozen once parity is green

D4 exploited the absence of frozen gather coefficients to adopt the stable
softmax cheaply; once the three-way parity suite is green that gap closes
deliberately: a final task freezes gather-keyed baselines as a **separately
versioned set** (the established pattern — `global_v1` is never regenerated),
giving future gather work a 1e-6 floor instead of only tolerance tests. The
direct beneficiary is `gather-rem-coordination-format`, whose storage rewrite
then lands against a real regression floor. Sequenced after the engine fold
(section 6) so the frozen coefficients capture the final numerics of this
change.

### D16 — `probabilities` is the next-event probability on every family

`"probabilities"` stores `p_e = w_e / sum(w_e)`, summing to 1 per event on
every sub-model: the multinomial families' choice probability and, on
exact-time sub-models, the competing-risks probability that the sender
(DyNAM-rate) or dyad (REM) creates the next event (Appendix, eq. 1). This is
what the R backend already stores where the primitive exists (the softmax at
`estimation_core.R:844`); the change is that `cpp` and `gather` produce it
natively and that DyNAM-rate gets the primitive defined at all. The storage
guardrail applies with the family's risk-set width — senders for rate, dyads
for REM.

### D17 — exact-time `loglik` stores the conditional component

On exact-time sub-models the `"loglik"` primitive stores, besides
`intervalLogL` (the full per-event log-likelihood) and `total_rate` (D13),
the conditional component `log p_obs` — the Cox-partial-likelihood
contribution, the "which" of the which/when decomposition (Appendix, eq. 2).
Storing it rather than deriving it on demand is an explicit user choice
(2026-07-25 interview, against the derive-on-demand recommendation) so
partial-likelihood diagnostics read it directly.

**Revised 2026-07-25 — it is computed in the kernel, not at the R-side
assembly.** The original decision said "computed at the R-side result assembly
from stored pieces — no kernel change", via the algebraic identity
`log p_obs = intervalLogL − log T_e + Δt_e T_e`. That route is catastrophic
cancellation: `intervalLogL` is itself `x_obs − Δt_e T_e`, so the term added
back cancels one just subtracted, and the surviving digits fall with
`log10(Δt_e T_e)`:

```
  Dt*T      assembly (the identity)   direct (x_obs - lse)     digits lost
  1         -0.495181898085856        -0.495181898085856        0
  1e+03     -0.495181898085889        -0.495181898085856        3.8
  1e+06     -0.495181898120791        -0.495181898085856        6.8
  1e+09     -0.495181918144226        -0.495181898085856        9.6
  1e+15     -0.500000000000000        -0.495181898085856       15.0
```

`Δt_e T_e` is the expected event count over the interval, and by the intercept
score equation it averages ~1 **at the MLE** — so the identity is accurate
exactly where it is least needed, and degrades away from the MLE, which is
where `evaluate_model()`, `test_parameter()` and every diagnostic operate. At
`Δt_e T_e = 1e6` the relative error is ~7e-8, **already coarser than the 1e-10
cross-backend tolerance this change requires**, so the specified route could not
have met its own parity requirement.

The kernel has `x_obs` and the log-normalizer in hand, so
`log p_obs = x_obs − lse` is one subtraction, exact, and finite even where `T_e`
has overflowed. The identity remains a useful *cross-check* near the MLE and is
asserted there, not used as the computation. The conditional *score* (the
Schoenfeld residual, Appendix eq. 3) is by the same interview NOT stored:
`event_scores` stays the estimation score, whose column-sums identity is its
test anchor, and `residuals(type = "schoenfeld")` derives the conditional
variant on demand.

### D18 — The EM prototype leaves `R/` rather than being swept

Task 1.3 migrated five `set_algorithm_newton(engine = "default")` calls in
`R/functions_preprocess_em.R` off the deprecated argument, and then its
verification failed in an informative way: the planned behavioral gate could not
run, because the code cannot execute. What the grounding found:

- `estimate_emdynam()` and `preprocess_emdynam_competition()` are the **driver
  half** of a prototype. The helper half — `sgd_refactor()`, `get_weights()`,
  `residual_resample()`, `stratified_resample()`, `getChainSample()`,
  `computeSupportConstrain()` — exists only in `.plan/DyNES/`, and was never
  brought into the package.
- Neither function is exported; nothing in `R/`, `tests/`, `man/`, `inst/`,
  `_pkgdown.yml` or `vignettes/` references either.
- They call **14 functions that do not exist** anywhere resolvable, read 26 and
  31 free variables from a calling frame they no longer have (they were written
  to be `source()`d into a script environment), and use `parallel` and `dplyr`
  without either being declared in DESCRIPTION.
- `functions_estimate_emdynam.R:623` is `retunr(list(...))` — a typo for
  `return`, which would abort on first execution. The code has never run in
  this form, and it entered the package this way in a single bulk commit.
- Those two files are the **entire** source of the package's undefined-global
  surface: all 14 come from them, and removing them leaves none in `R/`.

They are also superseded, not merely stale. `abmcem` productizes the same
research line (confirmed 2026-07-25) as `set_algorithm_em()` / `estimate_dynes()`
with the E-step weighting machinery and the SGD M-step — sourcing from
`.plan/DyNES/`, the directory that holds this fragment's own missing helpers.

**Decision.** The two files move to `.plan/DyNES/`, rejoining the helper half
they were split from, and this change stops sweeping EM vocabulary. Because
`.plan/*` is gitignored (three files excepted), the move takes them out of
version control — deliberately, and consistently: the helper half was never
tracked either, so afterwards the whole prototype lives in one untracked place
instead of being split across a tracked driver and an untracked helper set. The
content stays recoverable at `git show 628b4d7:R/functions_estimate_emdynam.R`,
the same "history retains it" rule the repo already applies to archived changes.

**Why here rather than in `abmcem`.** The Non-Goal and the removal are one
thought; splitting them leaves this change pointing at a task in a change with
20 open tasks that has not started, while 2.0.0 ships the fragment to CRAN
in mid-August. `abmcem` gets a pointer note instead, so a later reader does not
rediscover a fragment that is already gone.

Task 1.3 is left as it stands: the removal subsumes it, and reverting five
tokens in a file about to be deleted would only add a commit. The history reads
"migrated, then removed", which is what happened.

Rejected: keeping and sweeping it (maintains a superseded fragment forever, and
the sweep is what exposed that it cannot run); keeping and excluding it (the two
EM files are already split — `functions_estimate_emdynam.R` went to `backend`
under the archived `backend-vocabulary`, so an exclusion would leave siblings on
opposite vocabularies for no reason a reader could reconstruct);
`.Rbuildignore` or `inst/prototypes/` (keeps it in git and out of the build, but
separates the driver from its helpers again — the split that caused this).

**Not fixed here:** `tests/testthat/helper-baselines.R` uses
`parallel::detectCores()` / `parallel::mclapply()` while `parallel` is in
neither Imports nor Suggests. A real gap, pre-existing and independent of the
EM files; it does not belong to this change.

### D19 — `stable_softmax_masked()` is a log-sum-exp, and is named one; the Poisson kernel adopts it

The helper's name says softmax; its return value is
`m + log Σ exp(x − m)` and it never forms a softmax. Callers derive both scales
from its two outputs — `p_j = w_j / Σw` from the shifted weights out-param, and
`log p_obs = x_obs − lse` from the return value. That is the standard shape
(log-sum-exp is the primitive, log-softmax is free and exact, softmax is a
transformation), and the implementation already follows it. Only the name does
not, and the misnomer has already cost: it is what produced the wrong task line
"the Poisson kernel adopts the stable softmax", which for a rate model reads as
nonsense because it *is* nonsense — adopting the *lse* is the sensible thing.

So: **`stable_softmax_masked()` → `log_sum_exp_masked()`**. A pure symbol rename
with no arithmetic — 7 call sites across 4 kernels plus the header and impl.
Three of those kernels are frozen-baseline paths, but section 6 opens them
anyway, and a rename cannot move a coefficient.

**The R-side `stable_softmax()` keeps its name, and this is settled rather than
deferred.** An earlier draft of this decision said it kept the name "for now"
because it would grow a `logNormalizer` for `total_rate` and D17; that was
wrong on both halves, and grounding it corrected two things:

- It is **not** the R counterpart of the C++ helper. `stable_softmax(x, rowwise)`
  returns `list(probabilities, logProbabilities)`; `log_sum_exp_masked(lin_pred,
  allowed, weights)` returns the normalizer and fills shifted weights. Different
  parameter, different return — they share the max-shift algorithm, not the
  interface. Its return value genuinely *is* a softmax, so unlike the C++ helper
  its name is accurate.
- The R exact-time path **never calls it**. `event_contribution_rate()` computes
  `rates <- exp(objectiveFunctions); ratesSum <- sum(rates)` on its own — raw
  and unshifted. So the R side's `total_rate`, conditional component and exact
  `p` come from *that* function, not from `stable_softmax()`, and no R caller
  ever needs a normalizer out of it (`total_rate` and the conditional component
  are exact-time-only concepts; the multinomial callers want `p` and `log p`,
  which they already get).

So the R lse work is real but lives in `event_contribution_rate()` — the same
shifted-weights treatment this decision gives the gather Poisson kernel, applied
to the R backend's exact-time core, and carried by task 4.1. It is not a rename.

**The Poisson kernel adopts it, on one shifted `exp` pass.** Since
`λ_j = e^m · w_j`, a single pass over the shifted weights yields every quantity
the kernel needs, with the likelihood unchanged in value *and* in overflow
behavior:

```
  m, w = e^(x−m), lse = m + log Σw        one exp pass, as today
  T   = exp(lse)                          == the old raw normalizer (measured
                                             bit-identical; overflows at the
                                             same max(x) ≈ 710, unavoidably)
  Σλs = e^m · Σ w_j s_j                   derivative (measured to 1.4e-14)
  p_j = w_j / Σw                          exact where the raw ratio gives
                                             `1` (subnormal) or `NaN`
  lse                                     log T for D17, total_rate for D13
```

The underflow cell is the one that justifies this on correctness rather than
taste: at `max(x) = −745` the naive ratio returns **`1`** where the truth is
`0.705` — silently wrong, no `NaN`, no warning, and nothing downstream sanity-
checks a probability. Ranks and probability-scale margins built on it would be
quietly wrong.

Rejected: leaving the Poisson kernel on the raw pass and computing a separate
lse only when primitives are requested — two passes, two code paths, and the
raw ratio still silently wrong whenever someone asks for probabilities.

### D20 — The reduction takes the probability vector as its weights in every family

Given D19's lse, the kernel has `p_e` in hand, so it hands the shared reduction
`(w = p_e, c = Δt_e T_e)` rather than `(w = λ_e, c = Δt_e)`. Algebraically
identical (Appendix eq. 5), and better in two ways.

**The large factor moves into the scalar.** With `w = λ`, an overflowing rate
multiplies a mixed-sign statistic and gives `Inf − Inf = NaN`; with `w = p`
bounded in `[0, 1]`, only the scalar carries the infinity:

```
  max(x)   score via (λ, Δt)          score via (p, Δt·T)
  0        -1.796  0.359 -1.343       -1.796  0.359 -1.343    identical
  700      -1.82e+304 ...             -1.82e+304 ...          identical
  710      -Inf  Inf  -Inf            -Inf  Inf  -Inf         identical
  800      NaN   Inf   NaN     ✗      -Inf  Inf  -Inf    ✓
```

For estimation this is immaterial — the damping loop rejects either. It matters
for `evaluate_model()` at fixed parameters, which rejects nothing and hands
whatever it computed to a diagnostic; an `Inf` is a signal, a `NaN` is a
corruption.

**One weight vector serves both exact-time scales.** D12's dual margins become
`accumulate_margins(p, 1, …)` and `accumulate_margins(p, Δt·T, …)` — one vector,
two scales — instead of `(λ, 1/T)` and `(λ, Δt)`. The same helper with `c = 1`
also yields the conditional (Schoenfeld) score that D17 leaves derivable on
demand, so `residuals-gof` gets it without new machinery.

Rejected: keeping `(λ, Δt)` because it reads closer to the likelihood's algebra.
It does, but the reduction contract is about the *contribution* `m`, and `m` is
identical either way; the factorization should be chosen for conditioning.

## Risks / Trade-offs

- [Touching all nine compiled kernels risks perturbing the frozen 1e-6
  coefficient baselines, which cover `r` and `cpp`] → D7 sequences the frozen
  paths last and behind proven reductions; every `src/` edit goes through
  `cpp-recompile` before testing, and `NOT_CRAN=true` with baselines PASS (not
  SKIP) gates every commit.
- [Gather's numerics change when it adopts the stable softmax] → gather is not
  in the frozen set (D4), so the exposure is the cross-backend agreement tests;
  a max-shift is a no-op where `exp` does not overflow, so a *moving*
  well-conditioned fixture is itself the signal that something else broke.
- [Storing `probabilities` on `cpp` and `gather` multiplies the memory the
  existing guardrail warns about across two more backends] → the guardrail is
  per-fit and already sized from the risk set; it is not backend-specific and
  needs no new logic, only to remain in front of the new paths.
- [Removing the `probabilities` redirect is observable: code that got a result
  now gets a result computed by a different backend, or an abort] → it is a
  stated **BREAKING** item in the proposal with a NEWS entry; the abort names
  the supporting backends, and the redirect it replaces was already emitting a
  warning, so no path was silent-and-working.
- [Two in-flight changes over `optimizer-selection`] → D6's division, plus the
  rebase of `residuals-gof`'s delta as an explicit task, plus the
  header-placement check (a `MODIFIED` block written under `## ADDED` validates
  and then duplicates at archive) before the delta is accepted.
- [The shared header could standardize a shared mistake across all backends] →
  D5 keeps one independent reconstruction from `return_probabilities` as a
  cross-check that does not go through the header.
- [Dropping the `$engine` component breaks readers of it, and it has been
  public since 1.7.0] → it is a stated BREAKING item at the major version with
  a NEWS entry; it carried deprecated vocabulary, and old control *objects*
  (the realistic survivors, via `.rds` or long-lived scripts, from any
  pre-2.0.0 constructor) keep working through the D9 read shim.
- [Serialized pre-2.0.0 fits lack `$backend` and could trip consumers] → D10's
  contract: `NULL` means unknown, consumers fall back to component presence;
  the scenario is specced so `residuals-gof` implements against it.
- [The push sweeps ~11 test files while two other changes are in flight in
  `model_estimate.R`] → the push lands first within this change and is pure
  renames; `revise-gather-output`'s deltas are verified token-free as part of
  task 7.1's header check.

## Open Questions

All three original questions were resolved in the 2026-07-25 interview and
promoted to decisions: margins scale marker (D12), `total_rate` under loglik
parity (D13), coordination ranks over the whole risk set (D14). The same
session also settled the fold-generated questions: the D9 shim is silent,
maxLik optimizers stay cpp-only (Non-Goals), backend display stays deferred
(D10), and a gather baseline freeze closes the change (D15). A follow-up
session the same day refined the exact-time scale semantics: D12 revised
(dual margins, probability scale primary), D16 (uniform next-event
probabilities), D17 (conditional loglik stored; conditional scores derived
on demand). None remain open.

## Appendix — exact-time scale algebra (derivations)

Per event `e`: risk set `R_e`, effect statistics `s_ej`, weights
`w_ej = exp(β' s_ej)`. Multinomial family: `w` are softmax weights. Exact-time
family (DyNAM-rate, REM): `w` are rates `λ_ej`, with interval `Δt_e` and total
rate `T_e = sum_{j∈R_e} λ_ej`. Right-censored intervals contribute timing
terms only.

**(1) Next-event probability (competing risks).** With independent exponential
waiting times `T_j ~ Exp(λ_j)`, `P(j fires first) = λ_ej / T_e = p_ej` — the
softmax of the linear predictors, formally identical to the multinomial choice
probability. Hence one uniform `probabilities` primitive: `p_e` sums to 1 per
event in every family (D16).

**(2) Per-event log-likelihood and its which/when split.** For a dependent
event, `ℓ_e = β' s_obs − Δt_e T_e` (right-censored intervals: `−Δt_e T_e`
only). Adding and subtracting `log T_e`:

```
ℓ_e  =  log p_obs           +   ( log T_e  −  Δt_e T_e )
        └─ "which": Cox          └─ "when": timing term, a function of
           partial-likelihood       (T_e, Δt_e) only
           contribution ─┘
```

Stored under `"loglik"` on exact-time fits: `intervalLogL = ℓ_e`,
`total_rate = T_e` (D13), and the conditional component
`log p_obs = ℓ_e − log T_e + Δt_e T_e` (D17).

**(3) Score decomposition.** With `s̄_e = sum_j p_ej s_ej` (the
probability-weighted mean statistic):

```
∂ℓ_e/∂β  =  s_obs − Δt_e T_e s̄_e  =  ( s_obs − s̄_e )  +  ( 1 − Δt_e T_e ) s̄_e
             └─ estimation score        └─ Schoenfeld        └─ timing part
                (stored event_scores)      (conditional score)
```

The conditional score `s_obs − s̄_e` is exactly the Schoenfeld residual;
`residuals()` derives it on demand (from stored probabilities or an
`evaluate_model()` pass). Column sums of the stored estimation score equal
the aggregate score — the parity suite's identity.

**(4) The two margins and their identities — and their different
accumulation sets.** The two variants deliberately sum over different sets,
because they condition on different information:

- Probability variant (both families): `M^p_j = sum_{e dependent} p_ej
  1{j ∈ R_e}` — over **dependent events only**. `p_e` is the conditional
  distribution of *who moves given that an event occurs*; a right-censored
  interval realizes no mover, so it carries no "which" outcome to calibrate
  against. Since each dependent event contributes total 1,
  `sum_j M^p_j = n_events` at **any** parameter vector, and
  `observed_j − M^p_j` is a sum of conditionally-mean-zero terms — including
  censored intervals would add probability mass with no corresponding
  observed count, biasing the map by construction (measured in task 4.x
  grounding: 441 interval-sum vs 439 observed on the fixture).
- Expected-count variant (exact-time only): `M^c_j = sum_e Δt_e λ_ej` over
  **dependent and right-censored intervals** — the compensator integrates
  over all exposure time. The expected side is indexed by the *risk set*,
  not the mover: every `j` at risk accumulates its own `Δt_e λ_ej`, so a
  censored interval always has accumulation targets (only the observed side
  needs a realized mover, and there it correctly adds 0). Excluding
  censored intervals would understate
  exposure and break both the martingale-residual interpretation and the
  MLE identity. The intercept score equation `sum_e (1 − Δt_e T_e) = 0`
  (summed over all intervals) gives `sum_j M^c_j = n_events` at the **MLE**
  only.

Per-actor `observed_j − M^c_j` is the martingale residual (the Boschi–Wit
GOF substrate); per-actor `observed_j − M^p_j` is the parallel-to-choice
calibration map (D12). The asymmetry is itself diagnostic — it is the
which/when split of (2) applied to margins: a discrepancy visible in `M^c`
but not `M^p` localizes misfit to the timing/rate part; one visible in both
implicates the allocation part.

**(5) The bridge.** `m^c_ej = Δt_e λ_ej = Δt_e T_e · p_ej` — the compensator
scale is a per-event scalar rescaling of the probability scale. So
probability-scale primitives plus `total_rate` (plus the event times) span
both scales, and every dual quantity is assembly-side algebra, not kernel
work. In the `(w_e, c_e)` reduction contract: `c_e = 1/sum(w_e)` gives the
probability scale in both families; `c_e = Δt_e` gives the compensator where
`Δt` exists.

**(6) Ranks.** `w → p` is a strictly monotone per-event rescaling, so ranks
are identical on either scale — one variant, no label (D14 unaffected).

**(7) Family coverage.** Ordinal sub-models (rate_ordered, REM_ordered,
choice, coordination) have no `Δt`: probability scale only, nothing dual.
DyNAM-i rate follows DyNAM-rate. Fitted values and `predict()` inherit the
same split — "who is next" (probability scale) vs "how much activity"
(compensator) — which is `residuals-gof` D5's territory, fed by these stored
pieces.

**(8) The full likelihood factorization, and the Cox partial likelihood.**
The which/when split of (2) is not an algebraic trick but an exact density
factorization. Given the risk set at event `e`, the waiting time is
exponential with rate `T_e` and, independently of *when*, the identity of
the mover is categorical with probabilities `p_e` (the competing-risks
construction of (1)):

```
ℓ_e  =  log p_obs   +   ( log T_e − Δt_e T_e )
     =  log P(which)  +  log f_Exp(Δt_e ; T_e)
```

— the second term is literally the log-density of the observed waiting time,
`f_Exp(Δt; T) = T e^{−T Δt}`. Cox's proportional-hazards partial likelihood
is precisely `sum_e log p_obs`: `coxph` maximizes the "which" factors and
*discards* the timing factors, because with an unspecified baseline hazard
they carry no information about `β`. goldfish's exact-time models keep the
timing factor — the rate intercept is a parametric (constant) baseline, so
timing *is* informative here — which is why both pieces are worth storing.
The conditional component of D17 is therefore exactly the Cox
partial-likelihood contribution, which is what makes `residuals-gof`'s
`survival::cox.zph` cross-check exact (not approximate) on a REM expressible
as a Cox model.

**(9) After the timing factor: one categorical scale across sub-models.**
Once the waiting-time density is accounted for (subtracted), every
sub-model's remaining per-event contribution is a categorical
log-probability on one common scale — nats of a discrete outcome, `≤ 0`,
with uniform baseline `−log |R_e|`:

- choice / ordinal sub-models: `log p_obs` by construction (no timing factor
  exists to remove);
- exact-time rate / REM: the stored conditional component `log p_obs`.

DyNAM's factorization sharpens this: `P(dyad (i,j) next) =
p_rate(i) · p_choice(j | i)`, so conditional-rate plus choice per-event
log-likelihoods sum to the log-probability of the observed *dyad* — directly
comparable to a REM's conditional component over dyads. And because the
choice probabilities sum to 1 within each sender, the dyad-level total rate
under DyNAM equals the sender-level `T_e`, so the timing factor is *common*
to the two parameterizations: a DyNAM-vs-REM comparison on the same events
is carried entirely by the categorical parts. Two cautions: the timing
density `log T − Δt T` is on a per-time scale (it can be positive and
depends on the time unit) and must never be pooled with the categorical
parts; and raw categorical log-probabilities compare cleanly only at similar
risk-set widths — across widths (senders vs dyads vs receivers), normalize
against the `−log |R_e|` baseline or use the scale-free `ranks` / recall.

**(10) Next-dyad probability from the DyNAM product parameterization.**
DyNAM specifies the dyad intensity as a product — sender rate times choice
probability:

```
λ_ij  =  λ_i · p(j | i),        p(j | i) = exp(γ' s_ij) / sum_{h∈C_i} exp(γ' s_ih)
```

with `C_i` the sender's choice set. Competing risks over the dyad risk set
gives the next-dyad probability, and the denominator telescopes because the
choice probabilities sum to 1 within each sender:

```
sum_{(k,l)} λ_kl  =  sum_k λ_k · sum_{l∈C_k} p(l | k)  =  sum_k λ_k  =  T_e

P( (i,j) next )  =  λ_ij / sum_{(k,l)} λ_kl
                 =  (λ_i / T_e) · p(j | i)
                 =  p_rate(i) · p_choice(j | i)
```

The factorization is exact at any parameter vector, not an approximation:
the dyad-level next-event probability is the product of the rate sub-model's
`probabilities` primitive (over senders) and the choice sub-model's (over
that sender's choice set). A REM parameterizes `λ_ij` directly, so its
`probabilities` primitive is the un-factored `λ_ij / sum λ_kl` on the same
dyad space — comparing it with a DyNAM's composed product compares the two
parameterizations on identical footing, and by the telescoped denominator
the dyad-level total rate equals the sender-level `T_e`, which is why (9)'s
timing factor is common to both. Consequences: dyad-level probabilities for
a DyNAM fit are the outer composition of two stored primitives (no new
storage); dyad-level margins compose the same way,
`M^p_{ij} = sum_e p_rate(i) p(j|i)`, as assembly algebra; and
`log P(dyad) = log p_rate + log p_choice` is the additive form (9) uses for
cross-family comparison. The composition requires the rate and choice fits
evaluated on the same events and risk sets — the pairing that
`residuals-gof`'s model-evaluation pass provides.

**(11) Vuong statistic for the non-nested REM-vs-DyNAM comparison
(recorded for later — not implemented by this change or `residuals-gof`).**
The two parameterizations are provably non-nested:
`log λ^DyNAM_ij = log λ_i + γ' s_ij − log sum_{h∈C_i} exp(γ' s_ih)`, and the
per-sender log-normalizer is nonlinear in `γ`, so no linear-predictor REM
statistic reproduces it. For non-nested model selection, Vuong (1989): with
per-event log-likelihood differences `d_e = ℓ^REM_e − ℓ^DyNAM_e` (full, or
conditional-only when the question is "which dyad" without pacing — the
choice must be stated) over the `n` shared dependent events:

```
LR_n = sum_e d_e ,     ω̂² = (1/n) sum_e d_e² − (LR_n / n)² ,
Z = LR_n / (ω̂ √n)  →  N(0, 1)   under H0: E[d_e] = 0
```

(equal Kullback–Leibler distance from the true process), with the Schwarz
correction `LR_n − ((k_REM − k_DyNAM)/2) · log n` for unequal parameter
counts. Implementation caveats to resolve when this is picked up: (a) `d_e`
are serially dependent in an event sequence with history-dependent
statistics — Vuong's iid assumption calls for a HAC/robust `ω̂` or a block
bootstrap; (b) the near-equivalence degenerate case (`ω → 0`) needs the
variance pre-test before the normal approximation is trusted; (c) every
`d_e` is subtraction on stored primitives (D17 conditionals plus
`intervalLogL` / `total_rate`), so the statistic itself costs one pass over
stored vectors.

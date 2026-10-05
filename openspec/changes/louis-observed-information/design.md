## Context

`estimate_dynes()` maximizes the observed-data likelihood of a panel whose
latent event paths are integrated out by importance sampling over a pool of
augmented sequences. At the returned estimate `θ̂` the result runs one
full-pool evaluation (`dynes_covariances()` → `sandwich_covariances()`,
`R/fit_dynes.R`) that yields, for each kept sequence `i`, the complete-data
score `sᵢ`, the complete-data information `Jᵢ` and the normalized importance
weight `w̄ᵢ`. Today it returns:

```
I_c        = Σ w̄ᵢ Jᵢ
covariance = I_c⁻¹ (Σ w̄ᵢ sᵢsᵢᵀ) I_c⁻¹              ← vcov()
mc         = I_c⁻¹ (Σ w̄ᵢ² (sᵢ − S̄)(sᵢ − S̄)ᵀ) I_c⁻¹
```

The em_trace's opt-in per-pass SEs (`abem_sandwich_se()`,
`R/em_loop_dynes.R`) reuse the same function.

The observed-data score is the posterior mean of the complete-data score,
`S̄ = Σ w̄ᵢ sᵢ`, and is ≈ 0 at `θ̂`. Louis (1982) gives the observed
information as complete minus missing:

```
I_obs = E[J | y] − Var[s | y] = Σ w̄ᵢ Jᵢ − (Σ w̄ᵢ sᵢsᵢᵀ − S̄S̄ᵀ)
```

So today's middle term `Σ w̄ᵢ sᵢsᵢᵀ = Var_w(s) + S̄S̄ᵀ` is the *missing*
information plus an optimizer-tolerance term. With no missing information
`vcov()` collapses to roughly `|S̄|/I_c`. The 6.2b pilot
(`.plan/dynes/recovery/PILOT_findings.md`) shows this on a count panel with
exogenous-only effects, where every path has the same likelihood: `vcov()` SEs
are 0.0034 / 0.0014 / 0.00011, the glm closed form gives 0.183 / 0.097 / 0.153,
and `I_c⁻¹`, which equals `I_obs⁻¹` there, gives 0.186 / 0.098 / 0.153.

The code comment cites "Ruth 2024, 2.2" for the standard error, while abmcem
D9 cites §3.3. The prototype's §2.2 variant was `(I_c − S̄S̄ᵀ)⁻¹`. Neither form
is Louis. Ruth's own text says what the §3.3 form measures (Ruth 2024): "Note
that this standard error applies to Monte Carlo variability within an
iteration; it does not measure sampling variability due to the observed data.
... Note that there is no first moment term in the conditional variance of Sc
because θˆk is a maximizer of ˆE[lc(θ)|Y = y]." The bread-meat-bread form
was never a sampling covariance. abmcem D9 read a within-iteration
Monte-Carlo error as the estimator's standard error. The missing first-moment
term is why its middle term is the uncentered `Σ w̄ᵢsᵢsᵢᵀ`: at the
maximizer of the E-step objective, `S̄ = 0`.

## Goals / Non-Goals

**Goals:**
- `vcov()` is a consistent estimate of the observed-data asymptotic
  covariance, checked against a closed form.
- The Monte-Carlo covariance uses the right bread, so the MC-to-asymptotic SE
  ratio that `dynes-augmentation` 6.2c/6.3 report as the operating envelope
  compares like with like.
- An MC estimate of `I_obs` that is not positive definite is reported, never
  inverted into negative variances or silently replaced.

**Non-Goals:**
- MCMC autocorrelation in the MC covariance. Under `weighting = "uniform"`
  the middle term treats draws as independent. That is unchanged and stays
  with the chain diagnostics.
- Bias correction of `I_obs` for small pools, and the balance-heuristic
  multiple-importance weights (`abmcem` D14 future refinement).
- Any change to the point estimate, the weights, the M-step or the evaluator.
  Only the post-fit and em_trace covariance algebra moves.

## Decisions

### D1 — `vcov()` is the inverse of Louis's observed information

`I_obs = Σ w̄ᵢJᵢ − Σ w̄ᵢsᵢsᵢᵀ + S̄S̄ᵀ`, over the kept sequences (positive
weight, not degenerate), with the importance weights over the whole pool,
as today regardless of `use`. `vcov() = I_obs⁻¹`.

The weights are the run's own (`res$weights`, from `pool_weights()`), the
ones `θ̂` solved the weighted score equation with, including any
`transformation`. The rule, for every scheme and for the per-pass em_trace
SEs alike: a covariance is formed from the same imputed paths, with the same
weights, that the estimate it describes was solved with. The identity is exact for raw self-normalized importance
weights. Under `"clipping"` or `"smoothing"` the transformed weight's
θ-derivative is no longer `w̃ᵢ(sᵢ − S̄)` (piecewise under clipping, rescaled
under smoothing), so the estimator is then an approximation. The `@return`
documentation says so, and no warning fires, because the transformation is
a deliberate choice. Recomputing raw identity weights for this pass was
rejected: it would bring back the high-variance weighting that the user
chose the transformation to avoid, in the one pass whose output is reported.

It needs exactly the per-sequence `(sᵢ, Jᵢ)` that the evaluator already
returns. No new evaluation pass, no new evaluator output, no `src/` change.

The formula requires `Jᵢ` to be the negative Hessian of the complete-data
log-likelihood, not only its expectation. For the DyNES estimand that holds
in closed form. A DyNAM rate is log-linear in θ over its intervals, so its
Hessian `−Σ Δt·exp(θᵀx)·xxᵀ` is non-random given the path. A DyNAM choice is
a multinomial logit (canonical link), whose Hessian is the choice-probability
covariance of the statistics. Fixed-only terms carry no θ. A characterization
test pins this (task 1.1): the engine's `fisher` equals the numerical negative
Hessian of `compute_lik_seq()`'s log-likelihood on a fixture with a timed
rate, a pinned rate and a choice. If a future family breaks it, that test
fails before the covariance silently goes wrong.

*Alternatives rejected:*
- Keep the sandwich `I_c⁻¹ (Σ w̄ sᵢsᵢᵀ) I_c⁻¹`, that is
  `(−Q'')⁻¹ E[ssᵀ | y] (−Q'')⁻¹`. It is wrong by orders of magnitude when
  little information is missing (the oracle). The ingredients are Louis's,
  but they are combined by multiplication rather than subtraction. A
  bread-meat-bread form is a sampling covariance only when its middle term
  is the outer product of observed-data scores over independent units. Here
  the `sᵢ` are the scores of imputed paths of the same data. Their spread is
  uncertainty about the latent path, which is missing information, not
  sampling variability. Ruth (2024) states as much for the form itself: it
  "applies to Monte Carlo variability within an iteration; it does not
  measure sampling variability due to the observed data" (see Context). It
  is not offered as an option either (D7).
- `I_c⁻¹`, the complete-data inverse. It is anti-conservative whenever paths
  differ, since it ignores the missing information. It is the right answer
  only in the oracle's degenerate case.
- `(I_c − S̄S̄ᵀ)⁻¹` (the prototype's Ruth §2.2). It drops `Σ w̄ sᵢsᵢᵀ` and so
  equals `I_c⁻¹` up to the tolerance term.
- A per-event outer product of gradients. It needs per-event scores the
  evaluator does not expose, and it estimates the complete-data information,
  not the observed one.
- A numerical Hessian of the MC log-likelihood `log Σ wᵢ(θ)`. It is
  equivalent in expectation, costs 2p extra pool passes, and is noisier.
- Reweighting uniform (MCMC) draws to `θ̂` for the covariance pass. Under
  `weighting = "uniform"` the final pool was drawn at the previous
  iterate, and each draw's stored `log_proposal` would let the covariance
  pass alone importance-reweight the same paths to `θ̂`. Rejected: `θ̂`
  solved the score equation on these paths with these weights, and the
  covariance describes that estimator, not a neighboring one. The lag is one
  accepted step, within what convergence already tolerates. It also keeps
  D2's fixed-point argument, which assumes the run's uniform weights. Fresh
  draws at `θ̂` belong to goodness of fit, not to the covariance.

### D2 — The Monte-Carlo covariance takes `I_obs⁻¹` as its bread

`θ̂` solves `Ŝ(θ) = Σ w̄ᵢ(θ) sᵢ(θ) = 0`, where the weights depend on θ through
the self-normalized likelihood ratio. Differentiating gives
`∂Ŝ/∂θ = −Σ w̄ᵢJᵢ + Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ = −I_obs`. The delta method then gives
`Var_MC(θ̂) ≈ I_obs⁻¹ Var_MC(Ŝ) I_obs⁻¹`. The self-normalized IS variance of
`Ŝ`, `Σ w̄ᵢ² (sᵢ − S̄)(sᵢ − S̄)ᵀ`, is today's middle term and stays. Only the
bread changes, from `I_c⁻¹` to `I_obs⁻¹`. With the old bread the MC SE is
understated by the same factor as the missing information, which would bias
the 6.2c error ratio in exactly the cells it is meant to judge.

That derivative assumes θ-dependent weights. Under `weighting = "uniform"`
the weights are constant, and the same argument applied literally gives
`∂Ŝ/∂θ = −I_c`, the old bread. The uniform scheme takes `I_obs⁻¹` too, but
by the MCEM fixed-point argument. The returned estimate is a fixed point
`θ* = M(θ*)` of the Monte-Carlo EM map, and one M-step propagates the
E-step's score error through `I_c⁻¹`. At the fixed point that error is
amplified by `(I − ∂M)⁻¹`, and the EM rate matrix is
`∂M = I_c⁻¹(I_c − I_obs)`, so the product is
`(I − ∂M)⁻¹ I_c⁻¹ = I_obs⁻¹` (Dempster, Laird & Rubin 1977, §3; Booth &
Hobert 1999). Every scheme therefore uses the same bread. A reader of the
self-normalized derivation must not "fix" uniform back to `I_c⁻¹`.

### D3 — A non-positive-definite `I_obs` returns `NA` covariances and warns

With a finite pool, `Σ w̄ sᵢsᵢᵀ − S̄S̄ᵀ` can exceed `Σ w̄ Jᵢ` in some direction.
Few effective draws carry the missing-information estimate, and inverting the
result would give negative variances. The decision is made by a Cholesky
factorization of `(I_obs + I_obsᵀ)/2`, with no tolerance. That is the same
exactness rule `singular_attribution()` uses, so the two tests cannot
disagree about a borderline matrix.

Order of checks, each one exclusive with the next:
1. The pool stopped with no finite likelihood: the stop's own warning only
   (unchanged).
2. `I_c` is singular: `goldfish_dynes_singular_information`, naming
   zero-information coefficients (unchanged, still read off `I_c`, since a
   coefficient with no complete information has none observed).
3. `I_obs` is not positive definite: the new
   `goldfish_dynes_observed_information_not_pd`, with `NA` covariance and MC
   covariance, suggesting a larger `n_sequences` or a model-driven `routine`.

`louis_covariances()` returns `singular` and `not_pd` flags beside the
matrices. `new_dynes_fit()` chooses the warning, and the em_trace path takes
`NA` silently, as the singular case does today.

On a `converged = FALSE` fit the estimate need not be a maximum. Away from a
maximum the true observed information can be indefinite, whatever the pool
size, so on such a fit the warning adds an `"i"` bullet saying that the
estimate may not be a maximum and the observed information need not be
positive definite there. The condition class is the same, so
`covariance_status` (D5) does not distinguish the cause. The
`converged` flag beside it does.

*Alternatives rejected:* falling back to `I_c⁻¹` (silently anti-conservative,
which is the error this change removes); projecting to the nearest
positive-definite matrix (arbitrary, and hides the pool-size symptom);
aborting (loses a fit whose point estimate is fine).

### D4 — Verified by hand-computed forms and a closed-form oracle

- **Hand-computed forms** (CRAN-safe, `test-fit_dynes.R`). The existing test
  drives the fit through `local_analytic_evaluator()`, with identity
  `Jᵢ = I`, `sᵢ = μᵢ − θ` and weights summing to one. Its expectations become
  the Louis forms. The fixture's spread of `μᵢ` must keep `I_obs` positive
  definite (`Var_w(μ) < 1` per direction). A second fixture spreads them
  further to hit D3. Equal scores reduce `vcov()` to `I_c⁻¹` and the MC
  covariance to zero.
- **Monotonicity.** Where `I_obs` is positive definite,
  `I_obs ≤ I_c` in Loewner order, so every SE is at least `I_c⁻¹`'s.
- **Closed-form oracle** (`skip_on_cran()`). This is the pilot's count panel:
  n = 12, `calls` a count panel with rate `~ 1 + ego(x)` and choice
  `~ alter(y)`, beside a separable observed `emails` stream. It is generated
  by a seeded `simulate()` and projected to wave increments. The SEs are
  compared with the glm SEs of the Poisson closed form (sender counts with
  offset `log T`; the choice as a Poisson regression with sender fixed
  effects). The 10% relative tolerance absorbs evaluating the information at
  `θ̂` rather than at the glm MLE: the SGD under-steps, and the pilot gap is
  1.5%. It still catches the factor-50 error. The fixture goes into
  `helper-estimate_dynes.R` so that `dynes-augmentation` 6.2b reuses it
  instead of copying `.plan/` code. By construction it has **no** missing
  information (`I_obs = I_c` there), so it checks the scale of the estimator
  but cannot tell Louis from `I_c⁻¹`.
- **Pool Hessian identity** (`skip_on_cran()` if slow). This is the
  independent check on the missing-information term. With importance
  weights `w̄ᵢ ∝ exp(ℓᵢ(θ) − log qᵢ)` over a fixed pool, the negative Hessian
  of `log Σᵢ exp(ℓᵢ(θ) − log qᵢ)` equals `Σ w̄ᵢJᵢ − Σ w̄ᵢsᵢsᵢᵀ + S̄S̄ᵀ`
  algebraically, given `Jᵢ = −∇²ℓᵢ` (task 1.1). The test takes a pool whose
  paths carry different scores (at least one endogenous effect), uses
  `transformation = "identity"`, and compares `louis_covariances()`'s
  `observed_information` with a central-difference Hessian of that function,
  through `evaluate_sequence_pool()` and `pool_log_proposal()`, within 1e-5
  relative. A sign error in the missing-information term, which the
  hand-computed test would share with the code, fails here.

### D5 — The fit records why its covariances are `NA`

`new_dynes_fit()` stores
`covariance_status ∈ {"ok", "pool_dropped", "singular", "not_pd"}`, set by
the first D3 check that applies (`"ok"` when none does). Without it, only a
warning tells the three `NA` causes apart. The 6.2 sweep would then have to
catch and classify warnings to count how often each fires per cell, and
`summary()` could not explain a blank SE column. The status is a fit field
and a `glance()` column, the one-row-per-fit shape the sweep tabulates.
`summary()` prints, after the convergence status line, a
`cli_alert_warning` naming the reason when the SE columns are blank: the
`not_pd`/`singular` lever, or a pointer to the pool stop for
`pool_dropped`. `print()` on the fit shows no SEs and is unchanged.

*Alternative rejected:* warnings only. This is the current state, and it is
the one the sweep cannot tabulate.

### D6 — The fit records the largest fraction of missing information and the covariance ESS

A positive-definite `I_obs` can still be badly estimated. When the missing
information covers 99% of `I_c` in some direction, the SEs grow tenfold and
are noisy, and the Cholesky check says nothing. That regime is what
`dynes-augmentation` 6.2c/6.3 characterize. The fit stores
`fraction_missing`, the largest eigenvalue of `I_c⁻¹ Var_w(s)` with
`Var_w(s) = Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ`. It is computed as the largest eigenvalue of
the symmetric `L⁻¹ Var_w(s) L⁻ᵀ`, with `L` the Cholesky factor of `I_c`
(`eigen(symmetric = TRUE)`), in the same pass.

- `ok`: the value, in `[0, 1)`.
- `not_pd`: the value, `≥ 1`, which shows how far the pool is from usable.
- `singular`, `pool_dropped`: `NA_real_` (there is no `I_c⁻¹`, or no
  information at all).

It is a fit field and a `glance()` column, and `summary()` prints it on an
`ok` fit as one line ("Largest fraction of missing information: …"). There
is no warning and no threshold. The sweep plots SE stability against it.
The em_trace schema is unchanged: no per-pass status or fraction, only the
corrected per-pass SE values.

**Covariance ESS (added 2026-10-05).** `fraction_missing` cannot tell "nothing
is missing" from "the weights collapsed". `Var_w(s)` is a self-normalized
estimate. When the importance weights concentrate on a few sequences
(ESS → 1), it goes to zero however much information is missing, so `I_obs`
falls back to `I_c`, `fraction_missing` reads about 0, the status is `"ok"`,
and the SEs are the anti-conservative complete-data ones. That is the silent
fallback to `I_c⁻¹` that D3 rejects, and D3's Cholesky test cannot see it,
because it detects only an *overestimated* `Var_w(s)`. The fit therefore
also stores `covariance_ess = 1 / Σ w̄ᵢ²` of the weights the covariance
pass used (`res$weights`, the same vector as D1's), not of any em_trace pass.

- `ok`, `not_pd`, `singular`: the value under importance weighting (the
  weights are finite even when an inverse fails).
- `weighting = "uniform"`: `NA_real_`. There `1/Σw̄ᵢ²` is the number of
  kept sequences on every fit. The uniform scheme's failure mode is a poorly
  mixing chain, which also shrinks `Var_w(s)` but shows only in the
  em_trace's acceptance and autocorrelation diagnostics. A column that always
  reads "full" would suggest otherwise exactly when it should not. The
  documentation points to those diagnostics.
- `pool_dropped`: `NA_real_` (no weight carries mass).

Like `fraction_missing`, it is a fit field and a `glance()` column.
`summary()` prints it whenever it is not `NA`: on an `ok` fit beside the
fraction, and on a `not_pd` or `singular` fit beside the reason the SE
columns are blank, where a low value helps explain an indefinite estimate. It has no warning and no
threshold, since the 6.2 sweep plots SE stability against both numbers.

*Alternatives rejected:* warning below `ess_threshold` (a third covariance
warning class, on runs that the ESS guard already let through); relying on
the em_trace's last `ess` (recorded inside the loop, not necessarily from the
weights the covariance pass used, and absent from `glance()`).

### D7 — The fit exposes the information decomposition and the complete-data inverse

The Louis pass already holds both matrices whose difference it inverts. The
fit keeps them, so the user can see how much of the complete information the
missing information takes, and can rebuild the alternative.

- **Stored decomposition.** `new_dynes_fit()` stores
  `fit$information = list(complete = I_c, missing = Var_w(s), observed = I_obs)`:
  p×p matrices over the free coefficients, named by the composite labels.
  `missing` is the **centered** `Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ`, so
  `observed == complete − missing` is a plain subtraction. The `S̄S̄ᵀ` term
  lives inside `missing`, not as a fourth entry. It is filled on `ok`,
  `not_pd` and `singular` fits, where the matrices are finite and only their
  inverses can fail. A `not_pd` fit is exactly when a user wants to inspect
  them. On `pool_dropped` it is `NULL`, because nothing was evaluated.
- **`vcov(fit, information = c("observed", "complete_data"))`**, validated
  with `rlang::arg_match()`, defaults to `"observed"` (`I_obs⁻¹`, D1).
  `"complete_data"` returns `I_c⁻¹`, the variance that ignores the missing
  data. It is a lower bound on the observed one (D4 monotonicity). The
  argument name says which information is inverted. The value is not
  `"complete"`, because `vcov.goldfishBaseFit()` already takes
  `complete = FALSE` in the `stats` sense (pad the fixed coefficients), and
  one word would then mean two things across goldfish fits.
- **`"complete_data"` on a `not_pd` fit** returns the finite `I_c⁻¹`, with no
  warning. D3 rejects a *silent* fallback to `I_c⁻¹`. An explicit request
  is not silent, and the `not_pd` warning already fired at fit time. The
  argument's roxygen states that `I_c⁻¹` ignores the missing information.
  On a `singular` or `pool_dropped` fit it is `NA`.
- **The MC error has no per-type variant.** By D2, `I_c⁻¹` is the wrong
  bread under every weighting scheme, so a "complete-data MC error" would
  restore the understatement this change removes. `mc_covariance` stays
  `I_obs⁻¹`-bread only.
- **`tidy()`, `summary()` and `coef_layout()` take no `information`
  argument.** Every reported table shows one estimator, and the 6.2 sweep
  reads one column. `fraction_missing` (D6) already summarizes the gap
  between the two. A user who wants complete-data SEs takes
  `sqrt(diag(vcov(fit, information = "complete_data")))`.
- **User documentation.** One `@details` paragraph, written once in
  `estimate_dynes()` and inherited, says that the standard errors use
  Louis's identity. In E-step terms that is `−Q''` minus the conditional
  variance of the complete-data score, `E[ssᵀ | y] − S̄S̄ᵀ`. It also says why
  the bread-meat-bread `(−Q'')⁻¹ E[ssᵀ | y] (−Q'')⁻¹` does not apply: its
  middle term is a sampling variance only over independent units, and here
  the scores are imputations of the same data. Following Ruth (2024), that
  form measures "Monte Carlo variability within an iteration; it does not
  measure sampling variability due to the observed data". The paragraph
  cites Louis (1982), McLachlan & Krishnan (2008, ch. 4) and Ruth (2024).
  The first two are added to `inst/REFERENCES.bib`.

*Alternatives rejected:*
- Louis only, with no decomposition: it hides the one comparison that tells
  a user how much the latent paths cost.
- Offering the old sandwich as a `vcov()` type: an option that returns the
  wrong estimand invites its use.
- A cluster-robust sandwich over independent units: it needs per-unit
  observed-data scores the evaluator does not return (a `src/` change), and
  it presumes independent units (see Open Questions).
- A numerical Hessian of `log Σ wᵢ(θ)` as a user option: it estimates the
  same quantity as Louis at 2p extra pool passes. It stays a test oracle
  (D4).

## Risks / Trade-offs

- [Every DyNES SE changes value] → `estimate_dynes()` is experimental, and
  the old values were wrong. A `NEWS.d/` fragment says so. Snapshot tests of
  `summary()`/`print()` that print SEs are re-accepted after reading the
  diff, never blindly.
- [Small pools give non-positive-definite `I_obs` more often than they gave
  a singular `I_c`] → D3 makes it a warning with a lever, not an error. The
  rate at which it fires is something the 6.2 sweep records per cell, from
  `glance()`'s `covariance_status` (D5).
- [Collapsed importance weights shrink the missing-information estimate
  toward zero, giving `I_c⁻¹`-sized SEs with status `"ok"`] →
  `covariance_ess` (D6) is recorded beside `fraction_missing`, so a low pair
  is readable as collapse rather than "nothing missing". Whether the SEs
  are calibrated where information is missing is checked by
  `dynes-augmentation` task 6.2's SE-calibration comparison, not here.
- [Non-identity weight transformations make the estimator approximate] →
  documented in `@return` (D1); the Hessian test pins the identity case.
- [The `Jᵢ` = negative-Hessian assumption breaks for a future effect family]
  → task 1.1's characterization test pins it on the current families.
- [`abem_sandwich_se()` callers expect finite SEs] → only em_trace reads it,
  and it already tolerates `NA` from a singular `I_c`.

## Migration Plan

No data migration. The function is renamed in place (`sandwich_covariances`
→ `louis_covariances`, `abem_sandwich_se` → `abem_louis_se`). Both are
internal. Rollback is a revert of the implementation commit, and the tests
revert with it. On landing, `dynes-augmentation`'s design D7 and its
`sequence-augmentation` delta are amended to cite this estimator (task 3.2),
and abmcem D9 gets a dated amendment block.

## Open Questions

None blocking. Whether to add the balance-heuristic MIS weights to the
covariance stays with `abmcem` D14's future refinements.

A cluster-robust (misspecification-robust) sandwich is out of scope (D7). It
would need per-unit observed-data scores `Σᵢ w̄ᵢ sᵢ⁽ᵘ⁾`, which the evaluator
does not expose. It also presumes that a DyNES panel has independent units.
Actors that share one evolving network are not independent, and until that
is settled the estimator is not defined.

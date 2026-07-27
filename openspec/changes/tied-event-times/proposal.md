> **Track (user, 2026-07-27): post-release.** Not on the 2.0.0 spine.
> When taken up: the tie-count component it records on the fit must
> respect the result-format stamp `snake-case-result-components`
> introduced, and its warning wording should be aligned with
> `identifiability-diagnostics` (also post-release), as both changes
> already note.

## Why

The exact-time (Poisson) models assume a continuous-time process, in which two
events cannot occur at the same instant. Real data ties anyway. In the shipped
`Fisheries_Treaties_6070` fixture, **12 of 69 dependent events (17%) share a
timestamp with the event before them** — 13 of 241 intervals have `dt == 0`, and
12 of those carry a dependent event.

Today goldfish neither refuses this nor tells the user. A tied dependent event
contributes its linear predictor to the log-likelihood but `-dt * total_rate = 0`
exposure, so it informs *which* dyad acted and says nothing about *when*. The
rate scale is then estimated from 57 of 69 events while reporting 69. Nothing on
the fitted object records that this happened.

The two model families are not in the same position:

- **Choice / coordination (multinomial).** Ties are already meaningful: the
  choice is conditioned on the process state immediately before the event, and
  two choices at equal clock time are simply two choices in sequence. The
  existing behavior is defensible; what is missing is that the order among tied
  events is arbitrary and nothing says so.
- **Rate / REM exact-time (Poisson).** A tie contradicts the model. The waiting
  time is zero, which the likelihood cannot interpret, and the arbitrary input
  order decides which event updates the state first.

Order among tied events is genuinely unknown. The statistically honest treatment
is to **treat it as missing data**: refit under several orderings and combine.
That is a researcher's job, not the package's — goldfish's obligation is to make
it *possible* and to stop hiding the problem. Today it is not possible, because
the ordering a user supplies cannot survive the pipeline: a stocnet object keys
its ties by time, so a re-ordering of events that share a timestamp is not
representable and cannot be handed to preprocessing.

## What Changes

- **Exact-time fits warn about tied event times**, reporting how many dependent
  events carry `dt == 0` and what it means for the rate scale. The fit is still
  returned; this is a diagnosis, not a refusal.
- **The count of tied dependent events is recorded on the fitted object**, so a
  downstream diagnostic (and the user's own record) can see it without
  re-deriving it from the event times.
- **A user-supplied order among tied events is representable and is preserved**
  end to end, from the data object through preprocessing to estimation. This is
  the enabling change: without it, a researcher cannot construct the alternative
  orderings that a multiple-imputation treatment of order uncertainty requires.
- **Documentation of the tie problem and the imputation recipe** — how to
  generate orderings, refit, and combine — as guidance, with goldfish supplying
  the mechanism and the researcher supplying the analysis.

## Capabilities

### Modified Capabilities

- `likelihood-computation`: exact-time contributions state how a zero-length
  interval is treated, and estimation reports tied dependent events.
- `single-data-object`: the data object carries an explicit, user-controllable
  order among events sharing a timestamp, and preprocessing consumes it.

## Impact

- **Code:** the exact-time contribution paths and the estimation entry points
  that build the event schedule; the stocnet event-stream ordering in
  `R/event_streams.R` / `R/data_source.R`; the results assembly.
- **Users:** a new warning on exact-time fits with tied timestamps. No
  coefficient moves — the warning reports what already happens.
- **Not in scope:** performing multiple imputation, combining fits across
  orderings, or any pooling rule. Those are the researcher's, and belong with
  the analysis, not the estimator.
- **Adjacent:** `identifiability-diagnostics` owns the other "should I trust this
  fit?" reporting; this change's warning is a sibling of that one and should read
  consistently with it.

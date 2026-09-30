# dynes-augmentation — change-local NEWS staging

These bullets stage the user-facing changes for this change. They are folded
into the root `NEWS.md` at branch merge, together with the `DESCRIPTION`
version bump — neither the root `NEWS.md` nor the `DESCRIPTION` version is
touched on this branch (see `progress.md`, "Version / NEWS / archival").

## Panel seam (Group 2)

* A panel layer modeled as a dependent process is now the augmentation target
  of `estimate_dynes()`, with no separate flag; the event-stream estimators
  refuse it and point to `estimate_dynes()`, and a panel layer read only as a
  covariate stays a static step-covariate.
* Two panel kinds are diffed between waves: a flavored **state** panel into one
  flip per changed dyad, named by its flavor, and an unflavored **count**
  panel, whose cumulative counts rise by `Δ`, into `Δ` indistinguishable
  events on that dyad. Incomplete (`NA`) snapshots, states outside the flavor
  mapping and decreasing counts abort before diffing.
* A change-list row stamped exactly at a wave belongs to that wave's snapshot,
  for the wave diff and for a completed rate's pinned periods alike, so a
  pinned period counts the events of the interval it pins (on both panel
  kinds). This fixes a first period pinned to a zero hazard.
* `estimate_dynes()` infers the wave grid from the modeled panel layers' row
  times, opened by `set_preprocessing(start_time =)` when given and otherwise
  by the first wave, and takes an optional `wave_times` override. It compiles
  the wave diffs, per-dyad chains and per-wave states once at entry and shares
  them with every augmented draw.

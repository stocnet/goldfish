# abmcem — change-local NEWS staging

These bullets stage the user-facing changes for this change. They are folded
into the root `NEWS.md` at branch merge, together with the `DESCRIPTION`
version bump — neither the root `NEWS.md` nor the `DESCRIPTION` version is
touched on this branch (see `progress.md`, "Version / NEWS / archival").

* Group 1 (control constructors) complete: `set_algorithm_em()` and its three
  nested option bundles — `set_augmenter_options()`, `set_weights_options()`,
  `set_sgd_options()` — ship with child-local validation, the cross-object
  validity matrix and precedence rules, and the cold-start construction guard
  (`routine = "random"` with a non-zero `initial_parameters` or
  `warm_start = TRUE` warns).

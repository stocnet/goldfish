* Fixed two-mode layers declared `directed = FALSE` in goldfish data objects:
  * `network_state_at()` no longer adds phantom ties or fails out of bounds.
  * `prepare_dynes_data()` and the DyNES panel path no longer mirror them.
  * Every reader now treats such a layer as it treats a `directed = TRUE` one.
  * `directed` affects one-mode layers only; manynet's two-mode `FALSE` is fine.
* Updated how a one-mode layer with no `info$directed` entry is read:
  * It now reads as directed; it used to read as undirected (mirrored).
  * Seen by `estimate_dynam()` and `estimate_rem()` given a formula and data.
  * Also by `compute_statistics()`; declare `directed` to keep the old reading.

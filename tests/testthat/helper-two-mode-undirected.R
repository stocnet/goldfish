# Shared fixtures for the readers that decide whether a layer's state is
# mirrored: a two-mode matrix has no symmetric counterpart, so a two-mode layer
# declared `directed = FALSE` must read exactly as the same layer declared
# `TRUE`, while a one-mode layer declared `FALSE` is still mirrored.
#
# The two-mode objects reproduce the recorded failure (a square layer gaining a
# phantom E->C from the history row G->A; a non-square layer indexing past its
# matrix), wave structure included: a `time = NA` history row and a wave-10
# report of the same tie, on a panel-observed layer under `replace`.

# A panel-observed "survey" layer from mode "x" (senders) to mode "y"
# (receivers). `shape = "square"`: receivers A..D, senders E..H, history and
# wave-10 report G->A. `shape = "non_square"`: senders A..C, receivers D..H,
# history and wave-10 report C->H. `history = FALSE` drops the `time = NA` row.
# `rows` (from, to, time) replaces the default wave-10 report with the given
# timed rows, in global node ids.
two_mode_survey_fixture <- function(
  shape = c("square", "non_square"),
  directed,
  history = TRUE,
  rows = NULL
) {
  shape <- match.arg(shape)
  if (shape == "square") {
    modes <- rep(c("y", "x"), each = 4L)
    tie <- c(from = 7L, to = 1L)
  } else {
    modes <- c(rep("x", 3L), rep("y", 5L))
    tie <- c(from = 3L, to = 8L)
  }
  ties <- data.frame(
    from = unname(rep(tie[["from"]], 2L)),
    to = unname(rep(tie[["to"]], 2L)),
    time = c(NA, 10)
  )
  if (!is.null(rows)) {
    ties <- rbind(ties[is.na(ties$time), , drop = FALSE], rows)
  }
  if (!history) {
    ties <- ties[!is.na(ties$time), , drop = FALSE]
  }
  ties$weight <- 1
  ties$layer <- "survey"
  rownames(ties) <- NULL
  list(
    info = list(
      name = "toy",
      focal = "survey",
      update = c(survey = "replace"),
      directed = c(survey = directed),
      observation = c(survey = "panel"),
      sender = c(survey = "x"),
      receiver = c(survey = "y")
    ),
    nodes = data.frame(
      label = LETTERS[seq_along(modes)],
      mode = modes,
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

# A panel-observed one-mode "survey" layer over A..C declared `directed =
# FALSE`, holding one tie A->B as a `time = NA` history row. `rows` appends
# timed rows (from, to, time, weight) so a test can add the wave reports it
# needs (a reversed report, a double report).
one_mode_undirected_fixture <- function(rows = NULL) {
  history <- data.frame(from = 1L, to = 2L, time = NA_real_, weight = 1)
  ties <- rbind(history, rows)
  ties$layer <- "survey"
  list(
    info = list(
      name = "toy",
      focal = "survey",
      update = c(survey = "replace"),
      directed = c(survey = FALSE),
      observation = c(survey = "panel")
    ),
    nodes = data.frame(
      label = c("A", "B", "C"),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

# Run prepare_dynes_data() with its summary message silenced, collecting every
# warning it raises, so two objects can be compared warning for warning.
prepare_collecting_warnings <- function(x, layers) {
  warnings <- list()
  out <- withCallingHandlers(
    suppressMessages(prepare_dynes_data(x, layers)),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(out = out, warnings = warnings)
}

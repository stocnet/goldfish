# Regenerate tests/testthat/_fixtures/stale_prep/pre_buffers.rds.
#
# Run from the package root at a commit BEFORE the preprocessed object stopped
# storing the per-change composition lists (`active_sender_changes`,
# `active_dyad_changes`), the fold flags and the support-mask presence stashes
# beside the update buffers -- captured at 1dfee79:
#   Rscript tests/testthat/_fixtures/stale_prep/make-stale-prep.R
#
# The file holds objects of that layout, stamped with the same `prep_version`,
# so a test can show that such an object still estimates on every backend with
# the retired fields ignored. Regenerating at a later commit would capture the
# new layout and defeat the test.
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-constraint-atoms.R")

suppressWarnings(suppressMessages({
  data("Social_Evolution", package = "goldfish", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  dep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  dep <- dep[seq_len(60L), ]
  observed <- as.data.frame(dep)
  idle <- setdiff(actors$label, unique(c(observed$sender, observed$receiver)))
  comp <- data.frame(
    time = c(observed$time[10], observed$time[20] + 1, observed$time[30]),
    node = idle[c(1, 2, 1)],
    replace = c(FALSE, FALSE, TRUE)
  )
  actors <- make_nodes(actors)
  actors <- link_events(actors, comp, attribute = "present")
  data <- make_data(dep, call_network, calls, actors, comp)
  stale <- list(
    rem = estimate_rem(
      dep ~ 1 + inertia,
      sub_model = "rate",
      data = data,
      preprocessing_only = TRUE
    ),
    choice = estimate_dynam(
      dep ~ inertia,
      sub_model = "choice",
      data = data,
      preprocessing_only = TRUE
    ),
    constrained_rate = constraint_atoms_prep("uncovered_rate")
  )
}))
stopifnot(
  length(stale$rem$active_sender_changes) > 0L,
  !is.null(stale$constrained_rate$support_mask$sender_presence_init)
)
saveRDS(
  stale,
  "tests/testthat/_fixtures/stale_prep/pre_buffers.rds",
  version = 2
)

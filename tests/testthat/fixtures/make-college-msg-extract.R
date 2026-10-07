# Walk cost fixture: the first 5000 messages of SNAP CollegeMsg.
#
# Source: https://snap.stanford.edu/data/CollegeMsg.html
#   Panzarasa, Opsahl and Carley (2009). Patterns and dynamics of users'
#   behavior and interaction: Network analysis of an online community. JASIST
#   60(5), 911-932.
# One line per message, "sender receiver unix_time", 1899 actors numbered 1 to
# 1899, 59835 messages in the full file.
#
# The cost tests need a node set large enough that one n1 x n2 grid is
# expensive (13.8 MB as a logical matrix at 1899 actors): on the 84 actors of
# Social_Evolution a per-event copy of the grid does not separate from noise.
# The extract keeps the full node set and shortens only the event sequence, so
# the per-event cost is the real one and the test stays under a minute.
#
# Regenerate from the package root, with the full file downloaded from the
# URL above and unpacked at `path`:
#
#   Rscript tests/testthat/fixtures/make-college-msg-extract.R <path>
#
# Changing the extract moves every recorded value in
# tests/testthat/_budgets/walk_cost.csv, so it is re-recorded in the same
# commit.

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1L) args[[1L]] else "CollegeMsg.txt"
n_keep <- 5000L

writeLines(
  readLines(path, n = n_keep),
  file.path("tests", "testthat", "fixtures", "college_msg_5000.txt")
)

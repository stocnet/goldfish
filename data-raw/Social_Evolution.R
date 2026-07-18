# Social Evolution: human-readable raw times + the prebuilt stocnet object.
#
# Two products, kept in one script because both derive from the same raw frames
# (and a case-insensitive filesystem cannot hold both `Social_Evolution.R` and a
# separate `social_evolution.R`):
#
#   1. Raw times -> POSIXct. The MIT Reality Commons Social Evolution study ran
#      in 2008; `calls$time` / `friendship$time` shipped as raw Unix epoch
#      seconds. They convert in place to POSIXct in GMT so the times read as
#      dates, and data/Social_Evolution.RData re-saves with the same three
#      objects (`actors`, `calls`, `friendship`) and columns. The conversion is
#      coefficient-neutral: as.numeric(POSIXct) round-trips to the identical
#      epoch seconds, so the frozen 1e-6 baselines remain valid and MUST still
#      PASS under NOT_CRAN=true (do NOT regenerate them).
#
#   2. The prebuilt `social_evolution` stocnet -- a plain list of tibbles that
#      goldfish consumes directly -- assembled from the converted frames.
#
# Run from the package root: source("data-raw/Social_Evolution.R").

library(manynet)

load(here::here("data", "Social_Evolution.RData"))

# 1. Raw times -> POSIXct -----------------------------------------------------

# 2008 data; GMT keeps the epoch seconds unshifted. Idempotent: numeric only.
to_gmt <- function(time) {
  if (is.numeric(time)) {
    as.POSIXct(time, origin = "1970-01-01", tz = "GMT")
  } else {
    time
  }
}

epoch_calls <- as.numeric(calls$time)
epoch_friendship <- as.numeric(friendship$time)

calls$time <- to_gmt(calls$time)
friendship$time <- to_gmt(friendship$time)

# The epoch seconds the estimator reads internally must be byte-for-byte the
# pre-conversion values, or the frozen baselines would drift.
stopifnot(
  identical(as.numeric(calls$time), epoch_calls),
  identical(as.numeric(friendship$time), epoch_friendship),
  inherits(calls$time, "POSIXct"),
  inherits(friendship$time, "POSIXct")
)

save(
  actors,
  calls,
  friendship,
  file = here::here("data", "Social_Evolution.RData"),
  compress = "xz"
)

# 2. Prebuilt `social_evolution` stocnet --------------------------------------
#
# friendship survey snapshots enter as a `panel` layer (wave-timed replace
# updates); phone calls enter as an `event` layer (timestamped increments).
# from_ties() merges the two layers over one node set, join_nodes() brings the
# actor attributes, add_info() records the per-layer metadata the goldfish
# validator requires (update is derived from the change column; direction,
# observation, and the focal layer are declared).

social_evolution <- from_ties(
  as_stocnet(friendship),
  as_stocnet(calls),
  layer_names = c("friendship", "calls")
) |>
  join_nodes(actors) |>
  rename_nodes() |>
  add_info(
    name = "Social Evolution MIT",
    focal = "calls",
    directed = c(friendship = TRUE, calls = TRUE),
    observation = c(friendship = "panel", calls = "event")
  )

# design D10: the shipped object is a plain list of tibbles, never a goldfish
# environment or a stamped `data.goldfish`.
stopifnot(
  inherits(social_evolution, "stocnet"),
  is.list(social_evolution),
  !is.environment(social_evolution),
  all(vapply(
    social_evolution[c("nodes", "ties", "changes", "global")],
    function(component) is.null(component) || is.data.frame(component),
    logical(1)
  ))
)

usethis::use_data(social_evolution, overwrite = TRUE, compress = "xz")

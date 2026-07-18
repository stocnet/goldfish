# Prebuilt `fisheries_treaties` stocnet object.
#
# Assembles the Fisheries_Treaties_6070 raw objects into a single manynet
# stocnet (a plain list of tibbles) that goldfish consumes directly. Run from
# the package root: source("data-raw/fisheries_treaties.R"). The raw Fisheries
# times are already POSIXct, so no time conversion is needed.
#
# - The `treaties` layer starts from the `bilatnet` history matrix (its ties
#   enter with time = NA) and binds the timed `bilatchanges`. Each change is a
#   creation (increment +1) or a dissolution (increment -1); that distinction
#   rides on a reserved `flavor` column so a specification can model creations
#   alone (`rate = list(creation ~ ...)`) while every change still updates the
#   network state. History ties carry NA flavor (state-only).
# - The `contiguity` layer starts from `contignet` and binds `contigchanges`.
#   Contiguity updates are `replace`, and the raw data carries same-time,
#   same-dyad replaces (a value set to 1 and 0 at one timestamp); the stocnet
#   coercion loses the incoming row order, so a reserved integer `order` column
#   pins the original sequence as the deterministic tie-break. The treaty layer
#   needs none: its `increment` updates commute.
# - Node-level covariates (gdp, active/sovereignty, regime) bind as change
#   streams via bind_changes().

library(manynet)

data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())

# Reserved `flavor` column: creation / dissolution mapped from the increment
# (+1 / -1). American spelling per the goldfish language policy.
bilatchanges$flavor <- ifelse(
  bilatchanges$increment == 1,
  "creation",
  "dissolution"
)

bilat_net <- as_stocnet(bilatnet) |>
  join_nodes(states) |>
  rename_nodes() |>
  bind_ties(bilatchanges)

# Original row order pins the same-time replace tie-break.
contigchanges$order <- seq_len(nrow(contigchanges))

contig_net <- as_stocnet(contignet) |>
  bind_ties(contigchanges)

fisheries_treaties <- from_ties(
  treaties = bilat_net,
  contiguity = contig_net
) |>
  add_info(
    name = "Fisheries Treaties",
    focal = "treaties",
    directed = c(treaties = FALSE, contiguity = FALSE),
    observation = c(treaties = "event", contiguity = "event")
  ) |>
  bind_changes(changes = gdpchanges, var = "gdp") |>
  bind_changes(sovchanges, var = "active") |>
  bind_changes(regchanges, var = "regime")

# design D10: the shipped object is a plain list of tibbles, never a goldfish
# environment or a stamped `data.goldfish`.
stopifnot(
  inherits(fisheries_treaties, "stocnet"),
  is.list(fisheries_treaties),
  !is.environment(fisheries_treaties),
  all(vapply(
    fisheries_treaties[c("nodes", "ties", "changes", "global")],
    function(component) is.null(component) || is.data.frame(component),
    logical(1)
  )),
  setequal(
    stats::na.omit(unique(as.data.frame(fisheries_treaties$ties)$flavor)),
    c("creation", "dissolution")
  )
)

usethis::use_data(fisheries_treaties, overwrite = TRUE, compress = "xz")

# Frozen two-mode baseline fixture: a subset of manynet::irps_nuclear converted
# to a goldfish `stocnet`.
#
# goldfish ships no copy of irps_nuclear (manynet is in Imports, so the data is
# always installed). The two-mode coefficient baselines must not break when the
# upstream manynet copy is revised, so a small, deterministic subset of the
# converted object is frozen here as `irps_nuclear_subset.rds`. The full
# conversion is documented end to end in the `two-mode` vignette; this script
# performs the same core conversion on a time window and saves the result.
#
# Regenerate (only with documented justification -- this is a frozen baseline
# fixture) from the package root:
#
#   Rscript tests/testthat/fixtures/make-irps-nuclear-subset.R

suppressPackageStartupMessages({
  library(manynet)
  library(tidygraph)
})

# Convert the manynet `mnet` two-mode discourse network into a goldfish stocnet.
# The rules follow Haunss & Hollway (2023, doi:10.1017/nws.2022.31):
#   - `type` marks the mode (FALSE = actor/speaker, TRUE = concept/claim);
#   - `increment` sign splits ties into a focal `support` (+1) layer and a
#     `contestation` (-1, flipped to +1) covariate layer;
#   - claims are choosable from their own introduction (activation stamped one
#     second before a claim's first event on the numeric-day time axis);
#   - `party`/`power` missingness is *structural* (a non-politician cannot hold a
#     party or formal power), so it is sentinel-recoded to 0 at conversion --
#     never imputed -- and diversity summarizers exclude the sentinel;
#   - the paper's modeled events are the `default` support claims, carried on the
#     support layer's `flavor` column so the non-default rows still update state.
irps_to_stocnet <- function(mnet) {
  nodes_raw <- as.data.frame(as_tibble(activate(mnet, "nodes")))
  ties_raw <- as.data.frame(as_tibble(activate(mnet, "edges")))

  is_actor <- !nodes_raw$type
  individual <- (nodes_raw$org != nodes_raw$name | is.na(nodes_raw$org)) &
    !nodes_raw$office
  nodes <- data.frame(
    label = nodes_raw$name,
    mode = ifelse(is_actor, "actor", "concept"),
    active = is_actor,
    politician = nodes_raw$politician,
    govt = nodes_raw$govt,
    office = nodes_raw$office,
    individual = individual,
    party = ifelse(is.na(nodes_raw$party), 0L, as.integer(nodes_raw$party)),
    power = ifelse(is.na(nodes_raw$power), 0L, nodes_raw$power),
    stringsAsFactors = FALSE
  )

  time_num <- as.numeric(ties_raw$time)
  epsilon <- 1 / 86400

  support_rows <- ties_raw$increment == 1
  contest_rows <- ties_raw$increment == -1
  support <- data.frame(
    from = ties_raw$from[support_rows],
    to = ties_raw$to[support_rows],
    time = time_num[support_rows],
    increment = 1,
    layer = "support",
    flavor = ifelse(ties_raw$default[support_rows], "default", NA_character_),
    stringsAsFactors = FALSE
  )
  contestation <- data.frame(
    from = ties_raw$from[contest_rows],
    to = ties_raw$to[contest_rows],
    time = time_num[contest_rows],
    increment = 1,
    layer = "contestation",
    flavor = NA_character_,
    stringsAsFactors = FALSE
  )
  ties <- rbind(support, contestation)
  ties <- ties[order(ties$time), ]

  concept_ids <- which(!is_actor)
  first_event <- tapply(
    time_num,
    factor(ties_raw$to, levels = concept_ids),
    min
  )
  activated <- concept_ids[!is.na(first_event)]
  activation <- data.frame(
    time = first_event[!is.na(first_event)] - epsilon,
    node = activated,
    var = "active",
    stringsAsFactors = FALSE
  )
  activation$value <- as.list(rep(TRUE, nrow(activation)))
  changes <- activation[order(activation$time), ]

  info <- list(
    name = "IRPS nuclear discourse (subset)",
    focal = "support",
    directed = c(support = TRUE, contestation = TRUE),
    update = c(support = "increment", contestation = "increment"),
    observation = c(support = "event", contestation = "event"),
    sender = c(support = "actor", contestation = "actor"),
    receiver = c(support = "concept", contestation = "concept"),
    active = list(update = "replace")
  )

  make_stocnet(info = info, nodes = nodes, ties = ties, changes = changes)
}

# A deterministic window: claim support/contestation through 2011-03-20 (the
# opening of period 2). Small but genuinely two-mode -- 110 speakers, 34 claims,
# 122 modeled `default` support events.
cutoff <- as.Date("2011-03-20")
mnet_subset <- dplyr::filter(
  activate(manynet::irps_nuclear, "edges"),
  time <= cutoff
)
irps_nuclear_subset <- irps_to_stocnet(mnet_subset)

saveRDS(
  irps_nuclear_subset,
  file.path("tests", "testthat", "fixtures", "irps_nuclear_subset.rds")
)

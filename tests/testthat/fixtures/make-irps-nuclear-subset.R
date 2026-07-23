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
  library(dplyr)
  library(lubridate)
})

# Convert the manynet `mnet` two-mode discourse network into a goldfish stocnet,
# following Haunss & Hollway (2023, doi:10.1017/nws.2022.31) and the datetime
# recipe taught in the `two-mode` vignette:
#   - `as_stocnet(twomode = TRUE)` reads `type` into the two modes (speakers are
#     initially active, concepts inactive);
#   - the tie `weight` sign splits ties into a focal `support` (+1) layer and a
#     `contestation` (-1, flipped to +1) covariate layer; `weight` is the tie
#     value goldfish applies, so both layers set `weight = 1`;
#   - the modeled `default` support events carry `flavor = "modeled"` (the rest
#     `"conditional"`), so the non-default rows still update state;
#   - time casts to POSIXct (UTC) and each concept activates one hour before its
#     first event -- a sub-day, readable offset that leaves no phantom
#     availability (a concept is choosable only from its own introduction);
#   - `party`/`power` missingness is *structural* (a non-politician cannot hold
#     a party or formal power), so it is sentinel-recoded to 0 at conversion --
#     never imputed -- and diversity summarizers exclude the sentinel.
# `cutoff` restricts the ties to a deterministic window on the POSIXct axis.
# `as_stocnet()` cannot consume an edge-filtered tidygraph, so the window is
# applied to the converted stocnet's ties rather than to the manynet object.
irps_to_stocnet <- function(mnet, cutoff) {
  s0 <- as_stocnet(mnet, twomode = TRUE) |>
    mutate_ties(
      layer = if_else(weight == 1, "support", "contestation"),
      flavor = if_else(
        weight == 1 & default %in% TRUE,
        "modeled",
        if_else(weight == 1, "conditional", NA_character_)
      ),
      weight = 1,
      time = as.POSIXct(time, tz = "UTC")
    ) |>
    mutate_nodes(
      party = if_else(is.na(party), 0L, as.integer(party)),
      power = if_else(is.na(power), 0L, power),
      individual = (org != label | is.na(org)) & !office,
      mode = if_else(active, "actor", "concept")
    )
  s0$ties <- dplyr::filter(s0$ties, time <= cutoff)

  # Concepts activate one hour before their first (windowed) event, on the
  # POSIXct axis.
  first_event <- tapply(s0$ties$time, s0$ties$to, min)
  concept_idx <- as.integer(names(first_event))
  activation <- tibble(
    time = as.POSIXct(first_event, tz = "UTC", origin = "1970-01-01") -
      hours(1),
    node = s0$nodes$label[concept_idx],
    value = TRUE
  )

  s0 |>
    add_info(
      name = "IRPS nuclear discourse (subset)",
      ties = c("support", "contestation"),
      focal = "support",
      directed = c(support = TRUE, contestation = TRUE),
      update = c(support = "increment", contestation = "increment"),
      observation = c(support = "event", contestation = "event"),
      sender = c(support = "actor", contestation = "actor"),
      receiver = c(support = "concept", contestation = "concept"),
      active = list(update = "replace")
    ) |>
    bind_changes(activation, var = "active")
}

# A deterministic window: claim support/contestation through 2011-03-20 (the
# opening of period 2). Small but genuinely two-mode.
cutoff <- as.POSIXct("2011-03-20", tz = "UTC")
irps_nuclear_subset <- irps_to_stocnet(manynet::irps_nuclear, cutoff)

saveRDS(
  irps_nuclear_subset,
  file.path("tests", "testthat", "fixtures", "irps_nuclear_subset.rds")
)

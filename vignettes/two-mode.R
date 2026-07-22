## ----load, message = FALSE----------------------------------------------------
library(goldfish)
library(manynet)
library(ggplot2)


## ----data---------------------------------------------------------------------
data("irps_nuclear", package = "manynet")
irps_nuclear


## ----conversion---------------------------------------------------------------
irps_to_stocnet <- function(mnet) {
  nodes_raw <- as.data.frame(tidygraph::as_tibble(
    tidygraph::activate(mnet, "nodes")
  ))
  ties_raw <- as.data.frame(tidygraph::as_tibble(
    tidygraph::activate(mnet, "edges")
  ))

  # (1) The mode. `type` marks the two sides; it becomes the reserved `mode`
  #     column. `individual` (natural person, not office-holder) is derived.
  is_actor <- !nodes_raw$type
  individual <- (nodes_raw$org != nodes_raw$name | is.na(nodes_raw$org)) &
    !nodes_raw$office

  # (2) Structural missingness -> a sentinel, at conversion. A non-politician
  #     cannot hold a party or formal power: the value is absent by definition,
  #     not missing-and-worth-modeling. Recoding NA -> 0 here means no NA ever
  #     reaches goldfish's imputation seam; the diversity summarizer below then
  #     excludes the 0. (See the closing section.)
  nodes <- data.frame(
    label = nodes_raw$name,
    mode = ifelse(is_actor, "actor", "concept"),
    # Actors are always present; claims are activated below.
    active = is_actor,
    politician = nodes_raw$politician,
    govt = nodes_raw$govt,
    office = nodes_raw$office,
    individual = individual,
    party = ifelse(is.na(nodes_raw$party), 0L, as.integer(nodes_raw$party)),
    power = ifelse(is.na(nodes_raw$power), 0L, nodes_raw$power),
    stringsAsFactors = FALSE
  )

  # (3) Support and contestation are separate processes -> separate layers.
  #     Support (+1) is the focal, modeled layer; contestation (-1, flipped to
  #     +1 so `indeg()` counts it) is a covariate layer, never modeled.
  time_num <- as.numeric(ties_raw$time)
  support_rows <- ties_raw$increment == 1
  contest_rows <- ties_raw$increment == -1
  support <- data.frame(
    from = ties_raw$from[support_rows],
    to = ties_raw$to[support_rows],
    time = time_num[support_rows],
    increment = 1,
    layer = "support",
    # (4) The paper models the `default` support events; the rest still update
    #     the network state. A `flavor` column marks the modeled sub-process.
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

  # (5) Claims are choosable from their own introduction: each concept activates
  #     one second (on the numeric-day axis) before its first event, and never
  #     deactivates. A concept never used in this window stays inactive.
  concept_ids <- which(!is_actor)
  first_event <- tapply(
    time_num, factor(ties_raw$to, levels = concept_ids), min
  )
  activated <- concept_ids[!is.na(first_event)]
  changes <- data.frame(
    time = first_event[!is.na(first_event)] - 1 / 86400,
    node = activated,
    var = "active",
    stringsAsFactors = FALSE
  )
  changes$value <- as.list(rep(TRUE, nrow(changes)))
  changes <- changes[order(changes$time), ]

  info <- list(
    name = "IRPS nuclear discourse",
    focal = "support",
    directed = c(support = TRUE, contestation = TRUE),
    update = c(support = "increment", contestation = "increment"),
    observation = c(support = "event", contestation = "event"),
    # Both layers run actor -> concept: a two-mode declaration per layer.
    sender = c(support = "actor", contestation = "actor"),
    receiver = c(support = "concept", contestation = "concept"),
    active = list(update = "replace")
  )

  manynet::make_stocnet(
    info = info, nodes = nodes, ties = ties, changes = changes
  )
}

nuclear <- as_goldfish(irps_to_stocnet(irps_nuclear))
nuclear


## ----reject-------------------------------------------------------------------
# `recip()` needs a reciprocated tie, but an actor->claim tie has no
# claim->actor counterpart. goldfish aborts, naming the two modes:
tryCatch(
  estimate_dynam(
    make_specification(
      choice = list(default ~ indeg(support) + recip(support)),
      model = "DyNAM", choice_sub_model = "choice", data = nuclear
    ),
    preprocessing_only = TRUE
  ),
  error = function(e) cat(conditionMessage(e))
)


## ----rate---------------------------------------------------------------------
rate_fit <- estimate_dynam(
  make_specification(
    rate = list(
      default ~ 1 + outdeg(support) + ego(power) + ego(office) + ego(individual)
    ),
    model = "DyNAM", rate_sub_model = "rate", data = nuclear
  ),
  sub_model = "rate"
)
summary(rate_fit)


## ----choice-------------------------------------------------------------------
max_power <- function(x) {
  x <- x[x != 0]
  if (!length(x)) 0 else max(x)
}
# Shannon diversity of parties among a claim's partied supporters.
party_diversity <- function(x) {
  x <- x[x != 0]
  if (!length(x)) return(0)
  p <- proportions(table(x))
  -sum(p * log(p))
}

choice_fit <- estimate_dynam(
  make_specification(
    choice = list(
      default ~ indeg(support) + indeg(contestation) + four(support) +
        tertius(support, power, summarizer_fn = max_power) +
        tertius_diff(support, govt,
          summarizer_fn = function(x) mean(x, na.rm = TRUE)
        ) +
        tertius(support, party, summarizer_fn = party_diversity)
    ),
    model = "DyNAM", choice_sub_model = "choice", data = nuclear
  )
)
summary(choice_fit)


## ----periods------------------------------------------------------------------
boundaries <- as.numeric(as.Date(
  c("2011-03-11", "2011-03-17", "2011-04-09", "2011-06-01", "2011-07-01")
))
period_labels <- c("P1: 11-16 Mar", "P2: 17 Mar-8 Apr",
  "P3: 9 Apr-31 May", "P4: 1 Jun-1 Jul")

period_fits <- lapply(seq_len(4), function(p) {
  estimate_dynam(
    make_specification(
      choice = list(
        default ~ indeg(support) + indeg(contestation) + four(support)
      ),
      model = "DyNAM", choice_sub_model = "choice", data = nuclear
    ),
    control_preprocessing = set_preprocessing_opt(
      start_time = boundaries[p], end_time = boundaries[p + 1]
    )
  )
})

period_coefs <- do.call(rbind, lapply(seq_len(4), function(p) {
  cf <- period_fits[[p]]
  data.frame(
    period = period_labels[p],
    effect = names(coef(cf)),
    estimate = coef(cf),
    se = sqrt(diag(vcov(cf)))
  )
}))


## ----period-plot, fig.alt = "Dot-and-whisker plot of the support popularity, contestation popularity, and shared-choices coefficients across the four discursive periods, with 95% confidence intervals."----
ggplot(period_coefs, aes(estimate, effect)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(
    xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se
  )) +
  facet_wrap(~period, nrow = 1) +
  labs(x = "Coefficient (95% CI)", y = NULL) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 10)
  )


## ----events-per-day, fig.alt = "Time series of support and contestation claim events per day from March to June 2011, with vertical lines at the four period boundaries."----
ties <- nuclear$ties
events_per_day <- aggregate(
  list(n = ties$time),
  by = list(day = as.Date(ties$time, origin = "1970-01-01"), layer = ties$layer),
  FUN = length
)

ggplot(events_per_day, aes(day, n, color = layer)) +
  geom_vline(
    xintercept = as.Date(c("2011-03-17", "2011-04-09", "2011-06-01")),
    linetype = "dashed", color = "grey60"
  ) +
  geom_line() +
  scale_color_manual(values = c(support = "#1b7837", contestation = "#762a83")) +
  labs(x = NULL, y = "Claim events per day", color = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


## ----load, message = FALSE----------------------------------------------------
library(goldfish)
library(manynet)
library(dplyr)
library(lubridate)
library(ggplot2)


## ----data---------------------------------------------------------------------
data("irps_nuclear", package = "manynet")
irps_nuclear


## ----convert-ties-nodes-------------------------------------------------------
nuclear <- as_stocnet(irps_nuclear, twomode = TRUE) |>
  mutate_ties(
    layer  = if_else(weight == 1, "support", "contestation"),
    flavor = if_else(weight == 1 & default %in% TRUE, "modeled",
      if_else(weight == 1, "conditional", NA_character_)),
    weight = 1,
    time   = as.POSIXct(time, tz = "UTC")
  ) |>
  mutate_nodes(
    party      = if_else(is.na(party), 0L, as.integer(party)),
    power      = if_else(is.na(power), 0L, power),
    individual = (org != label | is.na(org)) & !office,
    mode       = if_else(active, "actor", "concept")
  )


## ----activation---------------------------------------------------------------
first_event <- tapply(nuclear$ties$time, nuclear$ties$to, min)
concept_idx <- as.integer(names(first_event))
activation <- tibble(
  time  = as.POSIXct(first_event, tz = "UTC", origin = "1970-01-01") - hours(1),
  node  = nuclear$nodes$label[concept_idx],
  value = TRUE
)


## ----periods-globals----------------------------------------------------------
boundaries <- as.POSIXct(c("2011-03-17", "2011-04-09", "2011-06-01"), tz = "UTC")
step_time  <- boundaries - hours(1)
na_utc     <- as.POSIXct(NA, tz = "UTC")


## ----assemble-----------------------------------------------------------------
nuclear <- nuclear |>
  add_info(
    name  = "IRPS nuclear discourse",
    ties  = c("support", "contestation"),
    # focal sets the default dependent layer. The specifications below key a
    # flavor ("modeled ~ ...") to it, so focal is what names the support layer
    # here; a plain `layer ~ ...` LHS (or a spec `layer =`) makes it optional.
    focal = "support",
    directed    = c(support = TRUE, contestation = TRUE),
    update      = c(support = "increment", contestation = "increment"),
    observation = c(support = "event", contestation = "event"),
    # Both layers run actor -> concept: a two-mode declaration per layer.
    sender   = c(support = "actor", contestation = "actor"),
    receiver = c(support = "concept", contestation = "concept"),
    active   = list(update = "replace")
  ) |>
  bind_changes(activation, var = "active") |>
  mutate_globals(
    time  = c(na_utc, step_time[1], na_utc, step_time[2], na_utc, step_time[3]),
    var   = rep(c("period2", "period3", "period4"), each = 2),
    value = as.list(c(0, 1, 0, 1, 0, 1))
  )

nuclear <- as_goldfish(nuclear)
nuclear


## ----fig1, fig.alt = "Two time series from March to June 2011: the number of distinct actors speaking per day and the number of distinct claims raised per day, each with a 7-day rolling mean, with vertical lines at the three period boundaries."----
per_day <- nuclear$ties |>
  mutate(day = as.Date(time)) |>
  group_by(day) |>
  summarise(
    Actors = n_distinct(from),
    Claims = n_distinct(to),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(c(Actors, Claims), names_to = "series", values_to = "n") |>
  group_by(series) |>
  mutate(roll = as.numeric(stats::filter(n, rep(1 / 7, 7), sides = 2))) |>
  ungroup()

ggplot(per_day, aes(day, color = series)) +
  geom_vline(
    xintercept = as.Date(c("2011-03-17", "2011-04-09", "2011-06-01")),
    linetype = "dashed", color = "grey60"
  ) +
  geom_point(aes(y = n), alpha = 0.25, size = 0.8) +
  geom_line(aes(y = roll), linewidth = 0.8) +
  scale_color_manual(values = c(Actors = "#1b7837", Claims = "#762a83")) +
  labs(x = NULL, y = "Distinct per day (7-day mean)", color = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


## ----reject-------------------------------------------------------------------
# `recip()` needs a reciprocated tie, but an actor->claim tie has no
# claim->actor counterpart. goldfish aborts, naming the two modes:
tryCatch(
  estimate_dynam(
    make_specification(
      choice = list(modeled ~ indeg(support) + recip(support)),
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
      modeled ~ 1 + outdeg(support) + ego(power) + ego(office) + ego(individual)
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
      modeled ~ indeg(support) + indeg(contestation) + four(support) +
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


## ----periods-interacted-------------------------------------------------------
choice_periods <- estimate_dynam(
  make_specification(
    choice = list(
      modeled ~ indeg(support) + four(support) +
        indeg(support):global(period2) + indeg(support):global(period3) +
        indeg(support):global(period4) +
        four(support):global(period2) + four(support):global(period3) +
        four(support):global(period4)
    ),
    model = "DyNAM", choice_sub_model = "choice", data = nuclear
  )
)


## ----periods-windowed---------------------------------------------------------
period_labels <- c("P1: 11-16 Mar", "P2: 17 Mar-8 Apr",
  "P3: 9 Apr-31 May", "P4: 1 Jun-1 Jul")
window <- as.POSIXct(
  c("2011-03-11 00:00", "2011-03-16 23:00", "2011-04-08 23:00",
    "2011-05-31 23:00", "2011-07-01 00:00"),
  tz = "UTC"
)

period_fits <- lapply(seq_len(4), function(p) {
  estimate_dynam(
    make_specification(
      choice = list(modeled ~ indeg(support) + four(support)),
      model = "DyNAM", choice_sub_model = "choice", data = nuclear
    ),
    control_preprocessing = set_preprocessing_opt(
      start_time = window[p], end_time = window[p + 1]
    )
  )
})


## ----equivalence--------------------------------------------------------------
jc <- coef(choice_periods)
windowed   <- vapply(period_fits, function(f) coef(f)[["ideg"]], numeric(1))
interacted <- jc[["ideg"]] +
  cumsum(c(0, jc[["ideg:glob_period_Fx"]],
    jc[["ideg:glob_period_Fx_1"]], jc[["ideg:glob_period_Fx_2"]]))
data.frame(period = period_labels, windowed, interacted)


## ----period-plot, fig.alt = "Dot-and-whisker plot of the support popularity and shared-choices coefficients across the four discursive periods, with 95% confidence intervals."----
period_coefs <- do.call(rbind, lapply(seq_len(4), function(p) {
  cf <- coef(period_fits[[p]])
  se <- sqrt(diag(vcov(period_fits[[p]])))
  data.frame(
    period = period_labels[p], effect = names(cf),
    estimate = cf, se = se
  )
}))

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


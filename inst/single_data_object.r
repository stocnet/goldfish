library(dplyr)
library(purrr)
library(manynet)
library(igraph)
library(tidygraph)
library(goldfish)

# data("Social_Evolution")
data("Fisheries_Treaties_6070")

edgesFisheries <- bilatchanges |>
  rename(from = sender, to = receiver) |>
  mutate(type = "bilateral") |>
  rbind(
    contigchanges |>
      rename(from = sender, to = receiver) |>
      mutate(
        type = "contiguity",
        increment = ifelse(replace == 1, 1, -1)
      ) |>
      select(-replace)
  ) |>
  mutate(prev_history = FALSE) |>
  rbind(
    bilatnet |>
      graph_from_adjacency_matrix(mode = "directed", weighted = "increment") |>
      igraph::as_data_frame(what = "edges") |>
      mutate(
        time = NA,
        prev_history = TRUE,
        type = "bilateral"
      )
  ) |>
  rbind(
    contignet |>
      graph_from_adjacency_matrix(
        mode = "undirected",
        weighted = "increment"
      ) |>
      igraph::as_data_frame(what = "edges") |>
      mutate(
        time = NA,
        prev_history = TRUE,
        type = "contiguity"
      )
  )


changesFisheries <- sovchanges |>
  as_tibble() |>
  mutate(
    var = "active",
    replace = map(replace, ~.x)
  ) |>
  rbind(
    regchanges |>
      as_tibble() |>
      mutate(
        var = "regime",
        replace = map(replace, ~.x)
      )
  ) |>
  rbind(
    gdpchanges |>
      as_tibble() |>
      mutate(
        var = "gdp",
        replace = map(replace, ~.x)
      )
  ) |>
  rename(value = replace) |>
  select(time, node, var, value)

nodesFisheries <- states |>
  rename(active = present)


dataFisheries <- tbl_graph(
  nodes = nodesFisheries,
  edges = edgesFisheries,
  directed = TRUE
) |>
  add_changes(changesFisheries)

class(dataFisheries)
?add_changes

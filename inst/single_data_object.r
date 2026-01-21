library(dplyr)
library(purrr)
library(manynet)
library(igraph)
library(tidygraph)
library(goldfish)

# data("Fisheries_Treaties_6070")

# edgesFisheries <- bilatchanges |>
#   rename(from = sender, to = receiver) |>
#   mutate(type = "bilateral") |>
#   rbind(
#     contigchanges |>
#       rename(from = sender, to = receiver) |>
#       mutate(
#         type = "contiguity",
#         increment = ifelse(replace == 1, 1, -1)
#       ) |>
#       select(-replace)
#   ) |>
#   mutate(prev_history = FALSE) |>
#   rbind(
#     bilatnet |>
#       graph_from_adjacency_matrix(mode = "directed", weighted = "increment") |>
#       igraph::as_data_frame(what = "edges") |>
#       mutate(
#         time = NA,
#         prev_history = TRUE,
#         type = "bilateral"
#       )
#   ) |>
#   rbind(
#     contignet |>
#       graph_from_adjacency_matrix(
#         mode = "undirected",
#         weighted = "increment"
#       ) |>
#       igraph::as_data_frame(what = "edges") |>
#       mutate(
#         time = NA,
#         prev_history = TRUE,
#         type = "contiguity"
#       )
#   )


# changesFisheries <- sovchanges |>
#   as_tibble() |>
#   mutate(
#     var = "active",
#     replace = map(replace, ~.x)
#   ) |>
#   rbind(
#     regchanges |>
#       as_tibble() |>
#       mutate(
#         var = "regime",
#         replace = map(replace, ~.x)
#       )
#   ) |>
#   rbind(
#     gdpchanges |>
#       as_tibble() |>
#       mutate(
#         var = "gdp",
#         replace = map(replace, ~.x)
#       )
#   ) |>
#   rename(value = replace) |>
#   select(time, node, var, value)

# nodesFisheries <- states |>
#   rename(active = present)


# dataFisheries <- tbl_graph(
#   nodes = nodesFisheries,
#   edges = edgesFisheries,
#   directed = TRUE
# ) |>
#   add_changes(changesFisheries)

# class(dataFisheries)
# ?add_changes


# Example with data Social Evolution:

data("Social_Evolution")
# Small description of the data: 
#  - actors: 84 students with floor and gradeType attributes
#  - calls: directed network of phone calls among students between "2008-09-06 22:37:50 CEST" and "2008-10-19 01:39:12 CEST"
#  - friendship: two surveys of friendships recorded at  "2008-09-09 02:00:00 CEST" and "2008-10-19 02:00:00 CEST"

nodes <- actors |>
  rename(active = present)

edges_calls <- calls |>
  rename(from = sender, to = receiver) |>
  mutate(type = "call") |>
  mutate(flavour = "call")

edges_friendship <- friendship |>
  rename(from = sender, to = receiver, increment = replace) |>
  mutate(
    type = "friendship",
    flavour = ifelse(increment == 1, "creation", "deletion")
  )  

total_edges <- edges_calls |>
  rbind(edges_friendship) 

data <- tbl_graph(
  nodes = nodes,
  edges = total_edges,
  directed = TRUE
) 
class(data)
data <- structure(data, class = c("mnet", class(data)))
data

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

# Define adjacency matrices with the nodeset as `nodes`
timestamp_matrix_1 <- friendship |>
  filter(time == unique(time)[1]) |>
  select(2, 3) |>
  graph_from_data_frame(directed = TRUE, vertices = nodes) |>
  as_adjacency_matrix()

timestamp_matrix_2 <- friendship |>
  filter(time == unique(time)[2]) |>
  select(2, 3) |>
  graph_from_data_frame(directed = TRUE, vertices = nodes) |>
  as_adjacency_matrix()

# Define edges_friendship as the changes between timestamp_matrix_1 and timestamp_matrix_2, recording the difference in an `increment` column
edges_friendship <- as_data_frame(graph_from_adjacency_matrix(timestamp_matrix_2 - timestamp_matrix_1, mode = "directed", weighted = TRUE)) |>
  rename(increment = weight) |>
  filter(increment != 0) |>
  mutate(
    time = unique(friendship$time)[1],
    type = "friendship",
    flavour = ifelse(increment > 0, "creation", "deletion")
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

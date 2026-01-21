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

my_data <- tbl_graph(
  nodes = nodes,
  edges = total_edges,
  directed = TRUE
) 
class(my_data)
my_data <- structure(my_data, class = c("mnet", class(my_data)))
my_data


# Sample formulas

friend_spec <- make_specification(
  rate = list(
    creation ~ 1 + outdeg(calls),
    deletion ~ 1 + outdeg(friendship)
  ),
  choice = list(
    creation ~ recip + tie(calls),
    deletion ~ indeg + alter(floor)
  ),
  model = "icecream"
)

calls_spec <- make_specification(
  rate = calls ~ 1 + outdeg(calls),
  choice = calls ~ cycle + tie(friendship),
  model = "icecream"
)

coevol <- make_multivariate_spec(
  friend_spec,
  calls_spec,
  data = my_data
)


# Updated sample effects based on the sample specifications
# In the example specifications we have 6 unique effects in total: 
#   1. outdegree
#   2. reciprocity
#   3. tie
#   4. indegree
#   5. alter
#   6. cycle

effects <- data.frame(
    gid = c(1, 1, 2, 3, 4, 5, 1, 6, 3),  # Global effect ID (unique across all formulas)
    type = c("friendship", "friendship", "friendship", "friendship", "friendship", "friendship",
             "call", "call", "call"),  # "friendship", "call"
    flavour = c("creation", "deletion", 
                "creation",  "creation", "deletion", "deletion", 
                "call", "call", "call"),  # "creation", "deletion", "call"
    submodel = c("rate", "rate",
                 "choice", "choice", "choice", "choice", 
                 "rate", "choice", "choice"),  # "rate", "choice"
    effect_name = c("outdeg", "outdeg", "recip", "tie", "indeg", "alter", "outdeg", "cycle", "tie"),  # "indeg", "recip", "tie", etc.
    lid = c(1, 1, 2, 2, 1, 2, 1, 2, 3),  # Local ID within this specific formula
    param_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),  # Position in global parameter vector
    stringsAsFactors = FALSE
  )

# Updated sample formulas based on the sample specifications
formulas <- data.frame(
    fid = c(1, 2, 3, 4, 5, 6),  # Global formula ID (unique across all formulas)
    type = c("friendship", "friendship", "friendship", "friendship", "call", "call"),  # "friendship", "call"
    flavour = c("creation", "deletion", "creation", "deletion", "call", "call"),  # "creation", "deletion", NA_character_
    submodel = c("rate", "rate", "choice", "choice", "rate", "choice"),  # "rate", "choice"
    intercept = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),  # Logical for intercept
    gids = I(list(c(1), c(1), c(2, 3), c(4, 5), c(1), c(6, 3))),  # Grouped effect IDs
    stringsAsFactors = FALSE
  )


 #### Sample of events to effects matrix

# From the sample data, we have 711 (events), 439 calls and 272 friendship changes 
my_data %>% activate(edges) %>% as_tibble %>% nrow()
my_data %>% activate(edges) %>% as_tibble %>% filter(type == "friendship") %>% nrow()
my_data %>% activate(edges) %>% as_tibble %>% filter(type == "call") %>% nrow()


# Then the matrix will have 711 rows and 6 columns (one for each unique effect)
event_effect_link = matrix(0, nrow = 711, ncol = 6)

aux_rows <- which(as_tibble(my_data, active = "edges")$type == "friendship")
event_effect_link[aux_rows, c(1,2,3,4,5)] <- 1
aux_rows <- which(as_tibble(my_data, active = "edges")$type == "call")
event_effect_link[aux_rows, c(1,3,6)] <- 1


## Sample separating by submodel 
event_effect_link = list(rate = matrix(0, nrow = 711, ncol = 6),
                          choice = matrix(0, nrow = 711, ncol = 6))     


aux_rows <- which(as_tibble(my_data, active = "edges")$type == "friendship")
event_effect_link$rate[aux_rows, c(1)] <- 1
event_effect_link$choice[aux_rows, c(2,3,4,5)] <- 1

aux_rows <- which(as_tibble(my_data, active = "edges")$type == "call")
event_effect_link$rate[aux_rows, c(1)] <- 1
event_effect_link$choice[aux_rows, c(3,6)] <- 1


 #### Sample of events to objects matrix
 # The objects are: friendship net, calls net, floor attribute
 # The mateix will have 711 rows (events) and 3 columns (objects)
event_object_link = matrix(0, nrow = 711, ncol = 3)
col_names(event_object_link) <- c("friendship", "calls", "floor")

aux_rows <- which(as_tibble(my_data, active = "edges")$type == "friendship")
event_object_link[aux_rows, c(1,2,3)] <- 1  # friendship, calls and floor are involved in friendship changes
aux_rows <- which(as_tibble(my_data, active = "edges")$type == "call")
event_object_link[aux_rows, c(1,2)] <- 1  # friendship and calls are involved in call events

## Sample separating by submodel
event_object_link = list(rate = matrix(0, nrow = 711, ncol = 3),
                         choice = matrix(0, nrow = 711, ncol = 3))     
col_names(event_object_link$rate) <- c("friendship", "calls", "floor")
col_names(event_object_link$choice) <- c("friendship", "calls", "floor")  

aux_rows <- which(as_tibble(my_data, active = "edges")$type == "friendship")
event_object_link$rate[aux_rows, c(1,2)] <- 1  # friendship, calls are involved in friendship changes
event_object_link$choice[aux_rows, c(1,2,3)] <- 1  # friendship, calls and floor are involved in friendship changes
aux_rows <- which(as_tibble(my_data, active = "edges")$type == "call")
event_object_link$rate[aux_rows, c(2)] <- 1  # friendship and calls are involved in call events
event_object_link$choice[aux_rows, c(1,2)] <- 1  # friendship and calls are involved in call events 


#### Sample of objects to effects matrix 
# The matrix will have 3 rows (objects) and 6 columns (effects)
object_effect_link = matrix(0, nrow = 3, ncol = 6)
col_names(object_effect_link) <- c("outdeg", "recip", "tie",
                                    "indeg", "alter", "cycle")      
row_names(object_effect_link) <- c("friendship", "calls", "floor")
object_effect_link["friendship", c("outdeg", "recip", "tie", "indeg", "alter")] <- 1
object_effect_link["calls", c("outdeg", "recip", "tie", "cycle")] <- 1
object_effect_link["floor", c("alter")] <- 1  

## Sample separating by submodel
object_effect_link = list(rate = matrix(0, nrow = 3, ncol = 6),
                          choice = matrix(0, nrow = 3, ncol = 6))     
col_names(object_effect_link$rate) <- c("outdeg", "recip", "tie",
                                    "indeg", "alter", "cycle")      
col_names(object_effect_link$choice) <- c("outdeg", "recip", "tie",
                                    "indeg", "alter", "cycle")
row_names(object_effect_link$rate) <- c("friendship", "calls", "floor")
row_names(object_effect_link$choice) <- c("friendship", "calls", "floor")
object_effect_link$rate["friendship", c("outdeg")] <- 1
object_effect_link$choice["friendship", c("recip", "tie", "indeg", "alter")] <- 1
object_effect_link$rate["calls", c("outdeg")] <- 1
object_effect_link$choice["calls", c("recip", "tie", "cycle")] <- 1
object_effect_link$choice["floor", c("alter")] <- 1    



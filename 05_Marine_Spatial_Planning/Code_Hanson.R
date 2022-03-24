# load packages
library(prioritizr)
library(raster)
library(plyr)
library(dplyr)

# load data
data(sim_pu_sf)
data(sim_features_zones)

# here sim_pu_sf is our planning unit in 2d format
print(sim_pu_sf)

# create a list with the feature data at different depths
# here, we will just use the built-in data for teaching purposes
sim_features_depths <- as.list(sim_features_zones)
names(sim_features_depths) <-
  paste0("depth_", seq_along(sim_features_depths))
print(sim_features_depths)

# create a tibble with the planning unit data
pu_data <- tibble(
  ## id column with unique ids for each 3D planning unit
  id = seq_len(nrow(sim_pu_sf) * length(sim_features_depths)),
  ## id column indicating which the 2d identifier for the planning unit
  id_2d = rep(seq_len(nrow(sim_pu_sf)), length(sim_features_depths)),
  ## depth column indicating which depth the 3d planning unit is at
  id_depth = rep(names(sim_features_depths), each = nrow(sim_pu_sf)),
  ## randomly simulate cost data
  cost = runif(length(id))
)

# print table
print(pu_data)

# extract information on the amount of each feature within each planning unit
## here we assume sim_features_depths is a list of raster stack objects
## e.g., sim_features_depths[[1]][[1]] has data for 1st feature in 1st depth
## e.g., sim_features_depths[[2]][[1]] has data for 1st feature in 2nd depth
## e.g., sim_features_depths[[1]][[2]] has data for 2nd feature in 1st depth
## e.g., sim_features_depths[[2]][[2]] has data for 2nd feature in 2nd depth
rij_data <- lapply(seq_along(sim_features_depths), function(i) {
  ## overlay the 2d planning units with the feature data at i'th depth
  rij_matrix(sim_pu_sf, sim_features_depths[[i]])
}) %>% do.call(what = cbind)

# now let's create a table with the feature data
feat_data <- tibble(
  id = seq_len(nlayers(sim_features_depths[[1]])),
  name = names(sim_features_depths[[1]])
)

# now let's create the boundary matrix
## initialize matrix for 2d data
b_matrix_2d <- boundary_matrix(sim_pu_sf)
b_data_2d <- as(b_matrix_2d, "dgTMatrix")
b_data_2d <- tibble(
  id1 = b_data_2d@i + 1,
  id2 = b_data_2d@j + 1,
  boundary = b_data_2d@x
)
### compute edge length
edge_length <- raster::modal(b_data_2d$boundary)

## create matrix for 3d planning unit data
### initialize with horizontal connections between 3d planning units
### that are located at the same depths
bound_data <-
  pu_data %>%
  plyr::ddply("id_depth", function(x) {
    id1_data <- left_join(b_data_2d, x, by = c("id1" = "id_2d"))
    id2_data <- left_join(b_data_2d, x, by = c("id2" = "id_2d"))
    tibble(
      id1 = id1_data$id,
      id2 = id2_data$id,
      boundary = id1_data$boundary
    )
  }) %>%
  dplyr::select(-id_depth) %>%
  as_tibble()
### now we add in the vertical connections between 3d planning units
### that are located at different depths (same water column)
bound_data <- bind_rows(
  bound_data,
  plyr::ddply(pu_data, "id_2d", function(x) {
    tibble(
      id1 = x$id[-nrow(x)],
      id2 = dplyr::lead(x$id)[-nrow(x)],
      boundary = edge_length
    )
  }) %>%
    dplyr::select(-id_2d) %>%
    as_tibble()
)
### add in vertical lengths for planning units on top and bottom depth
### note these connections are id1 == id2, they are adding the boundary
### lengths for the roof and floor of the planning units
bound_data <- bind_rows(
  bound_data,
  pu_data %>%
    dplyr::filter(
      id_depth %in% c(
        dplyr::first(names(sim_features_depths)),
        dplyr::last(names(sim_features_depths))
      )
    ) %>%
    dplyr::transmute(
      id1 = id,
      id2 = id,
      boundary = edge_length
    )
)
### aggregate data so we don't have any duplicate rows with exactly
### the same id1 and id2 values
bound_data <-
  bound_data %>%
  group_by(id1, id2) %>%
  summarize(boundary = sum(boundary)) %>%
  ungroup()

# now let's create the problem
p <-
  problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()
print(p)

# generate a solution
s <- solve(p)

# prepare data for plotting
## add solution to planning unit data
pu_data$solution_1 <- c(s)

## lets create a list (one element per depth) with a copy of the solution
## values that correspond to the planning units at each depth
sol_data <-
  lapply(seq_along(sim_features_depths), function(i) {
    x <- sim_pu_sf
    x$id_2d <- seq_len(nrow(x))
    x$id_depth <- names(sim_features_depths)[[i]]
    x <- left_join(
      x,
      pu_data %>% dplyr::select(id_2d, id_depth, solution_1),
      by = c("id_2d", "id_depth")
    )
    x$solution_1
  }) %>%
  setNames(paste0("solution_1_", names(sim_features_depths)))

## let's now create a version of the 2d planning unit data and insert
## a column for each depth, indicating (using 0s and 1s) if the
## planning unit was prioritized at a specific depth
sol_2d_data <-
  sim_pu_sf %>%
  bind_cols(do.call(tibble, sol_data))

# now let's visualize the solution
plot(
  sol_2d_data[, paste0("solution_1_", names(sim_features_depths))]
)

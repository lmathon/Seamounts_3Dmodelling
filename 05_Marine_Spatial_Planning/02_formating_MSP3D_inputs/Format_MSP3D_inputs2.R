# load packages
library(prioritizr)
library(raster)
library(plyr)
library(dplyr)
library(sf)

###################################################################################################################################################
## Formate features
###################################################################################################################################################

load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_0_200.rdata")
load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_200_400.rdata")
load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_400_600.rdata")
# load df_pelagic for full spatial extent
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_pelagic.rdata")
df_pelagic <- df_pelagic %>% filter(Sampling_Depth==20)

df_0_200$x <- df_pelagic$x
df_0_200$y <- df_pelagic$y

df_200_400$x <- df_pelagic$x
df_200_400$y <- df_pelagic$y

df_400_600$x <- df_pelagic$x
df_400_600$y <- df_pelagic$y

# transform each depth to raster

df <- df_0_200
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_1 <- stack(df)
projection(depth_1) <- "+proj=longlat +datum=WGS84 +no_defs"
sr <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
depth_1 <- projectRaster(depth_1, crs = sr)
depth_1 <- stack(depth_1)


df <- df_200_400
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_2 <- stack(df)
projection(depth_2) <- "+proj=longlat +datum=WGS84 +no_defs"
sr <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
depth_2 <- projectRaster(depth_2, crs = sr)
depth_2 <- stack(depth_2)


df <- df_400_600
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_3 <- stack(df)
projection(depth_3) <- "+proj=longlat +datum=WGS84 +no_defs"
sr <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
depth_3 <- projectRaster(depth_3, crs = sr)
depth_3 <- clamp(depth_3, lower = 0, useValues = TRUE)
depth_3 <- stack(depth_3)


features_depth <- list(depth_1, depth_2, depth_3)
names(features_depth) <-
  paste0("depth_", seq_along(features_depth))



save(features_depth, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata2/features_depth.rdata")

###################################################################################################################################################
## Formate planning unit template 
###################################################################################################################################################

# create pu_sf object

df <- df_pelagic[,1:3]
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pu_raster <- raster(df)
projection(pu_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
pu_raster <- projectRaster(pu_raster, crs=sr)
pu_polygon <- rasterToPolygons(pu_raster)
pu <- st_as_sf(pu_polygon)

pu_sf <- pu
names(pu_sf) <- c("cost", "geometry")
pu_sf$cost <- 1

save(pu_sf, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata2/pu_sf.rdata")

# planing unit for depth 1 : 0-200

pu_1 <- as.data.frame(pu)
pu_1$id_2d <- c(1:nrow(pu_1))
pu_1$id_depth <- "depth_1"
pu_1$cost <- pu_1$BottomDepth
pu_1$cost[pu_1$cost>=0] <- 1
pu_1 <- pu_1[,3:5]


# planing unit for depth 2 : 200-400
pu_2 <- as.data.frame(pu)
pu_2$id_2d <- c(1:nrow(pu_2))
pu_2$id_depth <- "depth_2"
for (i in 1:nrow(pu_2)) {
  if (pu_2[i,"BottomDepth"]>200 & !is.na(pu_2[i,"BottomDepth"])){
    pu_2[i,"cost"] <- 1
  } else {
    pu_2[i,"cost"] <- NA
  }
}
pu_2 <- pu_2[,3:5]

# planing unit for depth 2 : 200-400
pu_3 <- as.data.frame(pu)
pu_3$id_2d <- c(1:nrow(pu_3))
pu_3$id_depth <- "depth_3"
for (i in 1:nrow(pu_3)) {
  if (pu_3[i,"BottomDepth"]>400 & !is.na(pu_3[i,"BottomDepth"])){
    pu_3[i,"cost"] <- 1
  } else {
    pu_3[i,"cost"] <- NA
  }
  
}
pu_3 <- pu_3[,3:5]


pu_data <- rbind(pu_1, pu_2, pu_3)
pu_data$id <- c(1:nrow(pu_data))
pu_data <- pu_data %>% dplyr::select(id, id_2d, id_depth, cost)

pu_data$cost[is.na(pu_data$cost)] <- 100000


save(pu_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata2/pu_data.rdata")




# extract information on the amount of each feature within each planning unit
rij_data <- lapply(seq_along(features_depth), function(i) {
  ## overlay the 2d planning units with the feature data at i'th depth
  rij_matrix(pu_sf, features_depth[[i]])
}) %>% do.call(what = cbind)

rij_data@x[rij_data@x< 1e-6] <- 1e-6

save(rij_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata2/rij_data.rdata")

# create a table with the feature data
feat_data <- tibble(
  id = seq_len(nlayers(features_depth[[1]])),
  name = names(features_depth[[1]])
)

save(feat_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata2/feat_data.rdata")

##################################################################################################################################################
## Boundary data
##################################################################################################################################################

# now let's create the boundary matrix
## initialize matrix for 2d data
b_matrix_2d <- boundary_matrix(pu_sf)
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
        dplyr::first(names(features_depth)),
        dplyr::last(names(features_depth))
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



save(bound_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata2/bound_data.rdata")





# create the problem
p <-
  problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(0,0.5,data=bound_data) %>%
  add_binary_decisions()%>%
  add_gurobi_solver()
print(p)


# generate a solution
s <- solve(p)

# prepare data for plotting
## add solution to planning unit data
pu_data$solution_1 <- c(s)

## lets create a list (one element per depth) with a copy of the solution
## values that correspond to the planning units at each depth
sol_data <-
  lapply(seq_along(features_depth), function(i) {
    x <- pu_sf
    x$id_2d <- seq_len(nrow(x))
    x$id_depth <- names(features_depth)[[i]]
    x <- left_join(
      x,
      pu_data %>% dplyr::select(id_2d, id_depth, solution_1),
      by = c("id_2d", "id_depth")
    )
    x$solution_1
  }) %>%
  setNames(paste0("solution_1_", names(features_depth)))

## let's now create a version of the 2d planning unit data and insert
## a column for each depth, indicating (using 0s and 1s) if the
## planning unit was prioritized at a specific depth
sol_2d_data <-
  pu_sf %>%
  bind_cols(do.call(tibble, sol_data))


sol_depth1 <- sol_2d_data %>%
  filter(solution_1_depth_1 == 1)
plot(sol_depth1[,"solution_1_depth_1"])

sol_depth2 <- sol_2d_data %>%
  filter(solution_1_depth_2 == 1)
plot(sol_depth2[,"solution_1_depth_2"])

sol_depth3 <- sol_2d_data %>%
  filter(solution_1_depth_3 == 1)
plot(sol_depth3[,"solution_1_depth_3"])

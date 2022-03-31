# load packages
library(prioritizr)
library(raster)
library(plyr)
library(dplyr)
library(sf)

###################################################################################################################################################
## Formate planning unit template 
###################################################################################################################################################

# load df_pelagic for full spatial extent
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_pelagic.rdata")
df_pelagic <- df_pelagic %>% filter(Sampling_Depth==20)

# transform in raster and back to df to complete empty cells with NA

df <- df_pelagic
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_pelagic <- raster(df)
raster_pelagic[raster::Which(!is.na(raster_pelagic))] <- 1
projection(raster_pelagic) <- "+proj=longlat +datum=WGS84 +no_defs"
pu_raster <- rasterToPolygons(raster_pelagic)
pu_sf <- st_as_sf(pu_raster)
names(pu_sf) <- c("cost", "geometry")

save(pu_sf, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_sf.rdata")

# planing unit for depth 1 : 0-200
pu_1 <- df_pelagic[,1:3]
pu_1$id_2d <- c(1:nrow(pu_1))
pu_1$id_depth <- "depth_1"
pu_1$cost <- pu_1$BottomDepth
pu_1$cost[pu_1$cost>=0] <- 1
pu_1 <- pu_1[,4:6]


# planing unit for depth 2 : 200-400
pu_2 <- df_pelagic[,1:3]
pu_2$id_2d <- c(1:nrow(pu_2))
pu_2$id_depth <- "depth_2"
for (i in 1:nrow(pu_2)) {
  if (pu_2[i,"BottomDepth"]>200 & !is.na(pu_2[i,"BottomDepth"])){
    pu_2[i,"cost"] <- 1
  } else {
    pu_2[i,"cost"] <- NA
  }
}
pu_2 <- pu_2[,4:6]

# planing unit for depth 2 : 200-400
pu_3 <- df_pelagic[,1:3]
pu_3$id_2d <- c(1:nrow(pu_3))
pu_3$id_depth <- "depth_3"
for (i in 1:nrow(pu_3)) {
  if (pu_3[i,"BottomDepth"]>400 & !is.na(pu_3[i,"BottomDepth"])){
    pu_3[i,"cost"] <- 1
  } else {
    pu_3[i,"cost"] <- NA
  }
  
}
pu_3 <- pu_3[,4:6]


pu_data <- rbind(pu_1, pu_2, pu_3)
pu_data$id <- c(1:nrow(pu_data))
pu_data <- pu_data %>% dplyr::select(id, id_2d, id_depth, cost)


save(pu_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_data.rdata")


###################################################################################################################################################
## Formate features
###################################################################################################################################################

load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_0_200.rdata")
load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_200_400.rdata")
load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_400_600.rdata")

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
#depth_1 <- clamp(depth_1, lower = 1e-6, useValues = TRUE)

df <- df_200_400
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_2 <- stack(df)
projection(depth_2) <- "+proj=longlat +datum=WGS84 +no_defs"
#depth_2 <- clamp(depth_2, lower = 1e-6, useValues = TRUE)

df <- df_400_600
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_3 <- stack(df)
projection(depth_3) <- "+proj=longlat +datum=WGS84 +no_defs"
#depth_3 <- clamp(depth_3, lower = 1e-6, useValues = TRUE)

features_depth <- list(depth_1, depth_2, depth_3)
names(features_depth) <-
  paste0("depth_", seq_along(features_depth))



save(features_depth, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/features_depth.rdata")


# extract information on the amount of each feature within each planning unit
rij_data <- lapply(seq_along(features_depth), function(i) {
  ## overlay the 2d planning units with the feature data at i'th depth
  rij_matrix(pu_sf, features_depth[[i]])
}) %>% do.call(what = cbind)

save(rij_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/rij_data.rdata")

# create a table with the feature data
feat_data <- tibble(
  id = seq_len(nlayers(features_depth[[1]])),
  name = names(features_depth[[1]])
)

save(feat_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/feat_data.rdata")

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


save(bound_data, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/bound_data.rdata")





# create the problem
p <-
  problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(50,1,data=bound_data) %>%
  add_binary_decisions()
print(p)


# generate a solution
s <- solve(p)

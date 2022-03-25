# load packages
library(prioritizr)
library(raster)
library(plyr)
library(dplyr)

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
raster_pelagic <- stack(df)

pu_df <- as.data.frame(raster_pelagic, xy=TRUE)
pu_df <- pu_df[,1:3]
save(pu_df, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/pu_df.rdata")

#sf object for spatial extent
pu_sf <- st_as_sf(pu_df, coords = c("x", "y"), crs=4326)
names(pu_sf) <- c("cost", "geometry")
pu_sf$cost[pu_sf$cost>=0] <- 1

save(pu_sf, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/pu_sf.rdata")

# planing unit for depth 1 : 0-200
pu_1 <- pu_df
pu_1$id_2d <- c(1:nrow(pu_1))
pu_1$id_depth <- "depth_1"
pu_1$cost <- pu_1$BottomDepth
pu_1$cost[pu_1$cost>=0] <- 1
pu_1 <- pu_1[,4:6]


# planing unit for depth 2 : 200-400
pu_2 <- pu_df
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
pu_3 <- pu_df
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
pu_data <- pu_data %>% select(id, id_2d, id_depth, cost)

save(pu_data, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/pu_data.rdata")


###################################################################################################################################################
## Formate features
###################################################################################################################################################

load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_0_200.rdata")
load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_200_400.rdata")
load("05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_400_600.rdata")

# each depth to full spatial extent
df1 <- left_join(pu_df[,1:2], df_0_200, by=c("x", "y"))
df2 <- left_join(pu_df[,1:2], df_200_400, by=c("x", "y"))
df3 <- left_join(pu_df[,1:2], df_400_600, by=c("x", "y"))

# transform each depth to raster

df <- df1
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_1 <- stack(df)

df <- df2
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_2 <- stack(df)

df <- df3
coordinates(df) <- ~x+y
gridded(df) <- TRUE
depth_3 <- stack(df)

features_depth <- list(depth_1, depth_2, depth_3)
names(features_depth) <-
  paste0("depth_", seq_along(features_depth))

save(features_depth, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/features_depth.rdata")

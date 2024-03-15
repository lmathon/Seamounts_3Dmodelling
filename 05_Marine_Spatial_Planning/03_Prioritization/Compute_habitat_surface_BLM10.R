library(raster)
library(tidyverse)
library(sf)


# load df_pelagic for full spatial extent
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_pelagic.rdata")
df_pelagic <- df_pelagic %>% filter(Sampling_Depth==20)



df <- df_pelagic[,c(1:3)]
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pu_raster <- raster(df)
projection(pu_raster) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
pu_polygon <- rasterToPolygons(pu_raster)
pu <- st_as_sf(pu_polygon)

pu_habitat <- pu
pu_habitat <- cbind(pu_habitat, df_pelagic$Habitat)
names(pu_habitat) <- c("cost", "Habitat", "geometry")
pu_habitat$cost <- 1

save(pu_habitat, file="05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_habitat.rdata")


# Load solution 2D BLM10

load("05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Solution30_blm_10/sol30_2d_data.rdata")

pu_habitat <- as.data.frame(pu_habitat)
sol_2d_habitat <- left_join(sol_2d_data, pu_habitat[,c(2,3)], by="geometry")


# surface totale pente = 43181
surf_slope_tot <- sol_2d_habitat %>%
  filter(Habitat=="DeepSlope") %>%
  nrow()

# surface totale seamount = 42066
surf_seamount_tot <- sol_2d_habitat %>%
  filter(Habitat=="Seamount") %>%
  nrow()


# filter surface selectionnee
surface <- data.frame(depth1=sol_2d_data$solution_1_depth_1, depth2=sol_2d_data$solution_1_depth_2, depth3=sol_2d_data$solution_1_depth_3)
sol_2d_habitat$surface <- rowSums(surface[,1:3])
sol_2d_habitat_select <- sol_2d_habitat %>%
  filter(surface>0)

# surface selectionnee pente = 13213
surf_slope_select <- sol_2d_habitat_select %>%
  filter(Habitat=="DeepSlope") %>%
  nrow()

# surface selectionnee seamount = 10700
surf_seamount_select <- sol_2d_habitat_select %>%
  filter(Habitat=="Seamount") %>%
  nrow()


perc_SM <- surf_seamount_select/nrow(sol_2d_habitat_select)*100
perc_DS <- surf_slope_select/nrow(sol_2d_habitat_select)*100

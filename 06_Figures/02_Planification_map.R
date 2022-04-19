library(gurobi)
library(prioritizr)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(topsis)
library(ggplot2)
library(sf)
library(stars)


load("05_Marine_Spatial_Planning/03_Prioritization/Solution_blm_0.001/sol_depth1.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Solution_blm_0.001/sol_depth2.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Solution_blm_0.001/sol_depth3.rdata")

## Plot best solution

bathy <- raster("00_metadata/environmental/bathytopoMNT100m/MNT-nettoye_v3_FINAL.tif")
extent <- extent(158.1504, 171.7728,-25.632, -17.856)
bathy <- crop(bathy, extent)
sr <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
bathy <- projectRaster(bathy, crs = sr)
bathy[bathy>=0] <- 1
bathy[bathy < 0] <- NA

bathy_pol <- rasterToPolygons(bathy)
bathy_sf <- st_as_sf(bathy_pol)



depth1 <- ggplot(sol_depth1) +
  geom_sf(col="red")+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Planning units 0-200m")+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(depth1, filename="06_Figures/pu_depth1.png")
  
depth2 <- ggplot(sol_depth2) +
  geom_sf(col="blue")+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Planning units 200-400m")+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(depth2, filename="06_Figures/pu_depth2.png")

depth3 <- ggplot(sol_depth3) +
  geom_sf(col="green")+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Planning units 400-600m")+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(depth3, filename="06_Figures/pu_depth3.png")

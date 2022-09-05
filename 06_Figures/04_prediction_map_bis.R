library(raster)
library(ggplot2)
library(sf)
library(scales)
library(tidyverse)
library(ggpubr)


# load bruvs richness predictions
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/bruvs_richness_predict.rdata")
load("04_Modelling/01_benthic/01_BRUVs/04_GJAM_abundance_BRUVS/GJAM_Output_bruvs/predictions.rdata")

# load land contour
world <- st_read("c://Users/mathon/Desktop/PhD/Projets/Megafauna/Carto_megafauna/GSHHS_f_L1.shp")

### A - Richness total

df <- bruvs_richness_predict[,1:3]
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_bruvs_richness_predict <- raster(df)
projection(raster_bruvs_richness_predict) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict <- projectRaster(raster_bruvs_richness_predict, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf <- as(raster_bruvs_richness_predict, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Richness", "x", "y")


# plot prediction map
b <- c(0, 5, 10, 15, 20)

map_predict1 <- ggplot()+
  #geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df, !is.na(Richness)), aes(x = x, y = y, fill = Richness))+
  scale_fill_gradientn(limits=c(0,20), colours=terrain.colors(255), breaks=b, labels=format(b), aesthetics = "fill")+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(map_predict1, file="06_Figures/Rdata/pred1.png")  


### B - Abund species shallow

df2 <- predictions[,c(4,24,25)]
coordinates(df2) <- ~x+y
gridded(df2) <- TRUE
raster_bruvs_abund_predict2 <- raster(df2)
projection(raster_bruvs_abund_predict2) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_abund_predict2 <- projectRaster(raster_bruvs_abund_predict2, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf2 <- as(raster_bruvs_abund_predict2, "SpatialPixelsDataFrame")
test_df2 <- as.data.frame(test_spdf2)
colnames(test_df2) <- c("Abundance", "x", "y")


# plot prediction map
b <- c(0, 10, 20, 30, 40)

map_predict2 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df2, !is.na(Abundance)), aes(x = x, y = y, fill = Abundance))+
  scale_fill_gradientn(limits=c(0,40), colours=rev(terrain.colors(255)), breaks=b, labels=format(b), aesthetics = "fill")+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(map_predict2, file="06_Figures/Rdata/pristipomoides_filamentosus.png", width = 8, height = 5)

### C - Abund species deep

df3 <- predictions[,c(18,24,25)]
coordinates(df3) <- ~x+y
gridded(df3) <- TRUE
raster_bruvs_abund_predict3 <- raster(df3)
projection(raster_bruvs_abund_predict3) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_abund_predict3 <- projectRaster(raster_bruvs_abund_predict3, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf3 <- as(raster_bruvs_abund_predict3, "SpatialPixelsDataFrame")
test_df3 <- as.data.frame(test_spdf3)
colnames(test_df3) <- c("Abundance", "x", "y")


# plot prediction map
b <- c(0, 10, 20, 30)

map_predict3 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df3, !is.na(Abundance)), aes(x = x, y = y, fill = Abundance))+
  scale_fill_gradientn(limits=c(0,30), colours=rev(terrain.colors(255)), breaks=b, labels=format(b), aesthetics = "fill")+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(map_predict3, file="06_Figures/Rdata/squalus_megalops.png", width = 8, height = 5)


map_predict <- ggarrange(map_predict1, map_predict2, map_predict3, nrow=3, labels=c("A", "B", "C"))
ggsave(map_predict, file="06_Figures/Figure4bis.png", height = 12, width = 8)

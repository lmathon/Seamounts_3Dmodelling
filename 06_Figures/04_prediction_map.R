library(raster)
library(ggplot2)
library(sf)
library(scales)
library(tidyverse)
library(ggpubr)


# load bruvs richness predictions
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/bruvs_richness_predict.rdata")

# load land contour
world <- st_read("c://Users/mathon/Desktop/PhD/Projets/Megafauna/Carto_megafauna/GSHHS_f_L1.shp")



# Cut predictions in 3

bruvs_richness_predict1 <- bruvs_richness_predict %>%
  filter(BottomDepth <= 200)

bruvs_richness_predict2 <- bruvs_richness_predict %>%
  filter(BottomDepth > 200) %>%
  filter(BottomDepth <=400)

bruvs_richness_predict3 <- bruvs_richness_predict %>%
  filter(BottomDepth > 400)

### A - Depth 1

df1 <- bruvs_richness_predict1[,1:3]
coordinates(df1) <- ~x+y
gridded(df1) <- TRUE
raster_bruvs_richness_predict1 <- raster(df1)
projection(raster_bruvs_richness_predict1) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict1 <- projectRaster(raster_bruvs_richness_predict1, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf1 <- as(raster_bruvs_richness_predict1, "SpatialPixelsDataFrame")
test_df1 <- as.data.frame(test_spdf1)
colnames(test_df1) <- c("Richness", "x", "y")



# plot prediction map 1
b <- c(0, 5, 10, 15, 20)

map_predict1 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df1, !is.na(Richness)), aes(x = x, y = y, fill = Richness))+
  scale_fill_gradientn(limits=c(0,20), colours=rev(terrain.colors(255)), breaks=b, labels=format(b), aesthetics = "fill")+
  #geom_rect(aes(xmin = 162.5, xmax = 164, ymin = -19.7, ymax = -17.7), color = "red", fill = NA)  +
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.key.height = unit(3, "mm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))




### B - depth 3

df2 <- bruvs_richness_predict3[,1:3]
coordinates(df2) <- ~x+y
gridded(df2) <- TRUE
raster_bruvs_richness_predict3 <- raster(df2)
projection(raster_bruvs_richness_predict3) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict3 <- projectRaster(raster_bruvs_richness_predict3, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf2 <- as(raster_bruvs_richness_predict3, "SpatialPixelsDataFrame")
test_df2 <- as.data.frame(test_spdf2)
colnames(test_df2) <- c("Richness", "x", "y")

#plot prediction map 

map_predict2 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df2, !is.na(Richness)), aes(x = x, y = y, fill = Richness))+
  scale_fill_gradientn(limits=c(0,20), colours=rev(terrain.colors(255)), breaks=b, labels=format(b), aesthetics = "fill")+
  #geom_rect(aes(xmin = 167, xmax = 168.8, ymin = -24, ymax = -22.7), color = "red", fill = NA)  +
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.key.height = unit(3, "mm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))




# load bruvs abundance predictions
load("04_Modelling/01_benthic/01_BRUVs/04_GJAM_abundance_BRUVS/GJAM_Output_bruvs/predictions.rdata")

### C - Abund species shallow

df3 <- predictions[,c(4,24,25)]
coordinates(df3) <- ~x+y
gridded(df3) <- TRUE
raster_bruvs_abund_predict2 <- raster(df3)
projection(raster_bruvs_abund_predict2) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_abund_predict2 <- projectRaster(raster_bruvs_abund_predict2, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf3 <- as(raster_bruvs_abund_predict2, "SpatialPixelsDataFrame")
test_df3 <- as.data.frame(test_spdf3)
colnames(test_df3) <- c("Abundance", "x", "y")


# plot prediction map
b <- c(0, 10, 20, 30, 40)

map_predict3 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df3, !is.na(Abundance)), aes(x = x, y = y, fill = Abundance))+
  scale_fill_gradientn(limits=c(0,40), colours=rev(terrain.colors(255)), breaks=b, labels=format(b), aesthetics = "fill")+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.key.height = unit(3, "mm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


### D - Abund species deep

df4 <- predictions[,c(18,24,25)]
coordinates(df4) <- ~x+y
gridded(df4) <- TRUE
raster_bruvs_abund_predict3 <- raster(df4)
projection(raster_bruvs_abund_predict3) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_abund_predict3 <- projectRaster(raster_bruvs_abund_predict3, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf4 <- as(raster_bruvs_abund_predict3, "SpatialPixelsDataFrame")
test_df4 <- as.data.frame(test_spdf4)
colnames(test_df4) <- c("Abundance", "x", "y")


# plot prediction map
b <- c(0, 10, 20, 30)

map_predict4 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df4, !is.na(Abundance)), aes(x = x, y = y, fill = Abundance))+
  scale_fill_gradientn(limits=c(0,40), colours=rev(terrain.colors(255)), breaks=b, labels=format(b), aesthetics = "fill")+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.key.height = unit(3, "mm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))



## plot together

richness <- ggarrange(map_predict1, map_predict2, ncol=2, labels = c("A", "B"), common.legend = T, legend = "bottom")
abund <- ggarrange(map_predict3, map_predict4, ncol=2, labels = c("C", "D"), common.legend = T, legend = "bottom")

Fig4 <- ggarrange(richness, abund, nrow=2)


ggsave(Fig4, file="06_Figures/Figure4.png", width=10, height = 7.5)

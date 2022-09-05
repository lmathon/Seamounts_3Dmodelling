library(raster)
library(rgeos)
library(ggplot2)
library(sf)
library(scales)
library(tidyverse)



# load bruvs richness predictions
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/bruvs_richness_predict.rdata")

# load land contour
world <- st_read("c://Users/mathon/Desktop/PhD/Projets/Megafauna/Carto_megafauna/GSHHS_f_L1.shp")


# load sites coordinates
load("00_metadata/bruvs_explanatory_variables.rdata")

sites <- bruvs_var[,c("Site", "Latitude", "Longitude")]

sites <- sites %>%
  distinct(Site, .keep_all=TRUE)
sites$coords <- st_as_sf(sites[,c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs=4326)


# Cut predictions in 3

bruvs_richness_predict1 <- bruvs_richness_predict %>%
  filter(BottomDepth <= 200)

bruvs_richness_predict2 <- bruvs_richness_predict %>%
  filter(BottomDepth > 200) %>%
  filter(BottomDepth <=400)

bruvs_richness_predict3 <- bruvs_richness_predict %>%
  filter(BottomDepth > 400)


### Depth 1


df1 <- bruvs_richness_predict1[,1:3]
coordinates(df1) <- ~x+y
gridded(df1) <- TRUE
raster_bruvs_richness_predict1 <- raster(df1)
projection(raster_bruvs_richness_predict1) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict1 <- projectRaster(raster_bruvs_richness_predict1, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf1 <- as(raster_bruvs_richness_predict1, "SpatialPolygonsDataFrame")

test_spdf1$id=as.character(1:nrow(test_spdf1))
fort1 <- fortify(test_spdf1,region="id")
fort1 <- left_join(fort1, test_spdf1@data, c("id"="id"))

sm <- matrix(c(2,1.2,0,1),2,2)

# Get transformed coordinates:
xy <- as.matrix(fort1[,c("long","lat")]) %*% sm

# Add xy as extra columns in fortified data:
fort1$x <- xy[,1]; fort1$y = xy[,2]

ggplot(fort1, aes(x=x, y=y, group=id, fill=bruvs_richness)) + 
  geom_polygon() + 
  coord_fixed()+
  scale_fill_gradientn(limits=c(0,20), colours=terrain.colors(255), breaks=b, labels=format(b), aesthetics = "fill")+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

test_df1 <- as.data.frame(test_spdf1)
colnames(test_df1) <- c("Richness", "x", "y")



# plot prediction map
b <- c(0, 5, 10, 15, 20)

map_predict1 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(fort1, !is.na(bruvs_richness)), aes(x = x, y = y, fill = bruvs_richness))+
  scale_fill_gradientn(limits=c(0,20), colours=terrain.colors(255), breaks=b, labels=format(b), aesthetics = "fill")+
  geom_sf(col="black", size=1.5, data= sites$coords, shape=19, show.legend = F)+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(map_predict1, file="06_Figures/Rdata/pred1.png")  

### Depth 2

df2 <- bruvs_richness_predict2[,1:3]
coordinates(df2) <- ~x+y
gridded(df2) <- TRUE
raster_bruvs_richness_predict2 <- raster(df2)
projection(raster_bruvs_richness_predict2) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict2 <- projectRaster(raster_bruvs_richness_predict2, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf2 <- as(raster_bruvs_richness_predict2, "SpatialPixelsDataFrame")
test_df2 <- as.data.frame(test_spdf2)
colnames(test_df2) <- c("Richness", "x", "y")


# plot prediction map

map_predict2 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df2, !is.na(Richness)), aes(x = x, y = y, fill = Richness))+
  scale_fill_gradientn(limits=c(0,20), colours=terrain.colors(255), breaks=b, labels=format(b), aesthetics = "fill")+
  geom_sf(col="black", size=1.5, data= sites$coords, shape=19, show.legend = F)+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

ggsave(map_predict2, file="06_Figures/Rdata/pred2.png")  


### depth 3

df3 <- bruvs_richness_predict3[,1:3]
coordinates(df3) <- ~x+y
gridded(df3) <- TRUE
raster_bruvs_richness_predict3 <- raster(df3)
projection(raster_bruvs_richness_predict3) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict3 <- projectRaster(raster_bruvs_richness_predict3, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf3 <- as(raster_bruvs_richness_predict3, "SpatialPixelsDataFrame")
test_df3 <- as.data.frame(test_spdf3)
colnames(test_df3) <- c("Richness", "x", "y")

#plot prediction map

map_predict3 <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df3, !is.na(Richness)), aes(x = x, y = y, fill = Richness))+
  scale_fill_gradientn(limits=c(0,20), colours=terrain.colors(255), breaks=b, labels=format(b), aesthetics = "fill")+
  geom_sf(col="black", size=1.5, data= sites$coords, shape=19, show.legend = F)+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


ggsave(map_predict3, file="06_Figures/Rdata/pred3.png")  


map_predict <- ggarrange(map_predict1, map_predict2, map_predict3, nrow=3, labels=c("A", "B", "C"), common.legend = TRUE, legend = "right")
ggsave(map_predict, file="06_Figures/Figure4.png", height = 12, width = 8)

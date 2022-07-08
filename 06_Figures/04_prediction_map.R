library(raster)
library(ggplot2)
library(sf)
library(scales)


# load bruvs richness predictions
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/bruvs_richness_predict.rdata")

df <- bruvs_richness_predict[,1:3]
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_bruvs_richness_predict <- raster(df)
projection(raster_bruvs_richness_predict) <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_bruvs_richness_predict <- projectRaster(raster_bruvs_richness_predict, crs="+proj=longlat +datum=WGS84 +no_defs")

test_spdf <- as(raster_bruvs_richness_predict, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Richness", "x", "y")

# load land contour
world <- st_read("c://Users/mathon/Desktop/PhD/Projets/Megafauna/Carto_megafauna/GSHHS_f_L1.shp")


# load sites coordinates
load("00_metadata/bruvs_explanatory_variables.rdata")

sites <- bruvs_var[,c("Site", "Latitude", "Longitude")]

sites <- sites %>%
  distinct(Site, .keep_all=TRUE)
sites$coords <- st_as_sf(sites[,c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs=4326)



# plot prediction map

map_predict <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="grey80") +
  geom_tile(data=filter(test_df, !is.na(Richness)), aes(x = x, y = y, fill = Richness))+
  scale_fill_gradientn(colours=terrain.colors(255), aesthetics = "fill")+
  geom_sf(col="black", size=2, data= sites$coords, shape=19, show.legend = F)+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))
  
  

ggsave(map_predict, file="06_Figures/Figure4.png")  
  
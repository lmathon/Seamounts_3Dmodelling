library(raster)
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(rnaturalearth)
library(ggrepel)
library(ggpubr)
library(cartomisc)

# load sampling data and maps

load("00_metadata/bruvs_explanatory_variables.rdata")
load("00_metadata/edna_explanatory_variables_pelagic.rdata")
edna_var_pelagic <- edna_var
load("00_metadata/edna_explanatory_variables_benthic.rdata")
load("00_metadata/acoustic_explanatory_variables_pelagic.rdata")
acoustic_var_pel <- acoustic_var
load("00_metadata/acoustic_explanatory_variables_benthic.rdata")


bathy100 <- raster("00_metadata/environmental/bathytopoMNT100m/MNT-nettoye_v3_FINAL.tif")
bathy100[raster::Which(bathy100<(-10000))] <- NA
bathy100[raster::Which(bathy100>0)] <- NA


test_spdf <- as(bathy100, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")


# defines sites and summit class

sites <- bruvs_var[,c("Site", "Latitude", "Longitude")]

sites <- sites %>%
  distinct(Site, .keep_all=TRUE)


sites <- sites %>%
  mutate(Habitat = case_when(
    Site %in% c("Noumea", "PoyaNepoui", "Poum", "GrandLagonNord") ~ "DeepSlope",
    Site %in% c("Antigonia", "Torche", "Capel", "Fairway") ~ "Summit50",
    Site %in% c("JumeauWest", "KaimonMaru", "Crypthelia", "Argo") ~ "Summit250",
    Site %in% c("IleDesPins", "Stylaster", "Eponge") ~ "Summit500"
  ))           
sites$id <- c(1:nrow(sites))

sites$coords <- st_as_sf(sites[,c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs=4326)


# sampling data 

edna <- rbind(edna_var[,c(1,3:4)], edna_var_pelagic[,c(1,3:4)])
edna$coords <- st_as_sf(edna[,c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs=4326)

bruvs <- bruvs_var[,c(1,3:4)]
bruvs$coords <- st_as_sf(bruvs[,c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs=4326)

acoustic <- rbind(acoustic_var[,c(1:3)], acoustic_var_pel[,c(1:3)])
acoustic$coords <- st_as_sf(acoustic[,c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs=4326)


# global sampling map
world <- st_read("c://Users/mathon/Desktop/PhD/Projets/Megafauna/Carto_megafauna/GSHHS_f_L1.shp")

map_global <- ggplot()+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_tile(data=filter(test_df, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = T)+
  scale_fill_gradient2(limits=c(-8000,0), low = 'black', mid="grey30", high = 'white', midpoint=-4000, na.value = NA)+
  geom_sf(aes(col = sites$Habitat), size=4, data= sites$coords, shape=19, show.legend = F)+
  geom_text_repel(data = sites, aes(x=Longitude, y=Latitude, label = id),size=4, min.segment.length = 0.2, force = 2, max.overlaps=20) +
  scale_color_manual(values=c("#FDE725FF", "#B40F20", "#20A387FF", "#440154FF"))+
  coord_sf(xlim = c(158.1504, 171.7728), ylim = c(-25.632, -17.856))+
  labs(x="", y="", fill="Seafloor depth (m)  \n  ")+
  ggtitle("A")+
  guides(col=FALSE)+
  theme_minimal()+
  theme(title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(15, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))
  


save(map_global, file = "06_Figures/Rdata/global.rdata")
ggsave(map_global, filename="06_Figures/Rdata/global.png")

################################################################################################################################

# map site 1 = noumea
acoustic_1 <- acoustic %>%
  filter(Site=="Noumea")

extent <- extent(165.80,166.28,-22.25,-21.91)
bathy1 <- crop(bathy100, extent)
spdf1 <- as(bathy1, "SpatialPixelsDataFrame")
df1 <- as.data.frame(spdf1)
colnames(df1) <- c("value", "x", "y")


site1 <- ggplot()+
  geom_tile(data=filter(df1, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient2(limits=c(-8000,0), low = 'black', mid="grey30", high = 'white', midpoint=-4000, na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_1$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#FDE725FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#FDE725FF", show.legend = F)+
  coord_sf(xlim = c(165.80,166.28), ylim = c(-22.25,-21.91))+
  labs(x="", y="")+
  ggtitle("B")+
  theme_minimal()+
  theme(title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

save(site1, file = "06_Figures/Rdata/site1.rdata")
ggsave(site1, filename="06_Figures/Rdata/site1.png", width = 4, height = 3)

# map site 2 = PoyaNepoui

acoustic_2 <- acoustic %>%
  filter(Site=="PoyaNepoui")

extent <- extent(164.744,165.217,-21.619,-21.29)
bathy2 <- crop(bathy100, extent)
spdf2 <- as(bathy2, "SpatialPixelsDataFrame")
df2 <- as.data.frame(spdf2)
colnames(df2) <- c("value", "x", "y")


site2 <- ggplot()+
  geom_tile(data=filter(df2, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_2$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#FDE725FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#FDE725FF", show.legend = F)+
  coord_sf(xlim = c(164.744,165.217), ylim = c(-21.619,-21.29))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

save(site2, file = "06_Figures/Rdata/site2.rdata")
ggsave(site2, filename="06_Figures/Rdata/site2.png")


# map site 3 = Antigonia

acoustic_3 <- acoustic %>%
  filter(Site=="Antigonia")

extent <- extent(167.893,168.22,-23.55, -23.25)
bathy3 <- crop(bathy100, extent)
spdf3 <- as(bathy3, "SpatialPixelsDataFrame")
df3 <- as.data.frame(spdf3)
colnames(df3) <- c("value", "x", "y")

site3 <- ggplot()+
  geom_tile(data=filter(df3, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_3$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#20A387FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#20A387FF", show.legend = F)+
  coord_sf(xlim = c(167.893,168.22), ylim = c(-23.55, -23.25))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

save(site3, file = "06_Figures/Rdata/site3.rdata")
ggsave(site3, filename="06_Figures/Rdata/site3.png")

# map site 4 = IleDesPins

acoustic_4 <- acoustic %>%
  filter(Site=="IleDesPins")

extent <- extent(167.334,167.48,-22.44,-22.333)
bathy4 <- crop(bathy100, extent)
spdf4 <- as(bathy4, "SpatialPixelsDataFrame")
df4 <- as.data.frame(spdf4)
colnames(df4) <- c("value", "x", "y")


site4 <- ggplot()+
  geom_tile(data=filter(df4, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_4$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#440154FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#440154FF", show.legend = F)+
  coord_sf(xlim = c(167.334,167.48), ylim = c(-22.44,-22.333))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site4, file = "06_Figures/Rdata/site4.rdata")
ggsave(site4, filename="06_Figures/Rdata/site4.png")


# map site 5 = JumeauWest

acoustic_5 <- acoustic %>%
  filter(Site=="JumeauWest")

extent <- extent(167.91,168.129,-23.764,-23.612)
bathy5 <- crop(bathy100, extent)
spdf5 <- as(bathy5, "SpatialPixelsDataFrame")
df5 <- as.data.frame(spdf5)
colnames(df5) <- c("value", "x", "y")


site5 <- ggplot()+
  geom_tile(data=filter(df5, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_5$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#B40F20", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#B40F20", show.legend = F)+
  coord_sf(xlim = c(167.91,168.129), ylim = c(-23.764,-23.612))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site5, file = "06_Figures/Rdata/site5.rdata")
ggsave(site5, filename="06_Figures/Rdata/site5.png")



# map site 6 = Stylaster

acoustic_6 <- acoustic %>%
  filter(Site=="Stylaster")

extent <- extent(167.596,167.862,-23.734,-23.55)
bathy6 <- crop(bathy100, extent)
spdf6 <- as(bathy6, "SpatialPixelsDataFrame")
df6 <- as.data.frame(spdf6)
colnames(df6) <- c("value", "x", "y")


site6 <- ggplot()+
  geom_tile(data=filter(df6, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient2(limits=c(-8000,0), low = 'black', mid="grey30", high = 'white', midpoint=-4000, na.value = NA)+
  scale_x_continuous(breaks = c(167.6, 167.7, 167.8))+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_6$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#440154FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#440154FF", show.legend = F)+
  coord_sf(xlim = c(167.596,167.862), ylim = c(-23.734,-23.55))+
  labs(x="", y="")+
  ggtitle("C")+
  theme_minimal()+
  theme(title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site6, file = "06_Figures/Rdata/site6.rdata")
ggsave(site6, filename="06_Figures/Rdata/site6.png", width = 4, height = 3)


# map site 7 = KaimonMaru

acoustic_7 <- acoustic %>%
  filter(Site=="KaimonMaru")

extent <- extent(167.905,168.297,-24.9,-24.618)
bathy7 <- crop(bathy100, extent)
spdf7 <- as(bathy7, "SpatialPixelsDataFrame")
df7 <- as.data.frame(spdf7)
colnames(df7) <- c("value", "x", "y")


site7 <- ggplot()+
  geom_tile(data=filter(df7, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_7$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#B40F20", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#B40F20", show.legend = F)+
  coord_sf(xlim = c(167.905,168.297), ylim = c(-24.9,-24.618))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site7, file = "06_Figures/Rdata/site7.rdata")
ggsave(site7, filename="06_Figures/Rdata/site7.png")





# map site 8 = Eponge

acoustic_8 <- acoustic %>%
  filter(Site=="Eponge")

extent <- extent(168.217,168.52,-24.99,-24.778)
bathy8 <- crop(bathy100, extent)
spdf8 <- as(bathy8, "SpatialPixelsDataFrame")
df8 <- as.data.frame(spdf8)
colnames(df8) <- c("value", "x", "y")


site8 <- ggplot()+
  geom_tile(data=filter(df8, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_8$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#440154FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#440154FF", show.legend = F)+
  coord_sf(xlim = c(168.217,168.52), ylim = c(-24.99,-24.778))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site8, file = "06_Figures/Rdata/site8.rdata")
ggsave(site8, filename="06_Figures/Rdata/site8.png")


# map site 9 = Crypthelia

acoustic_9 <- acoustic %>%
  filter(Site=="Crypthelia")

extent <- extent(168.137,168.355,-23.353,-23.227)
bathy9 <- crop(bathy100, extent)
spdf9 <- as(bathy9, "SpatialPixelsDataFrame")
df9 <- as.data.frame(spdf9)
colnames(df9) <- c("value", "x", "y")


site9 <- ggplot()+
  geom_tile(data=filter(df9, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_9$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#B40F20", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#B40F20", show.legend = F)+
  coord_sf(xlim = c(168.137,168.355), ylim = c(-23.353,-23.227))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site9, file = "06_Figures/Rdata/site9.rdata")
ggsave(site9, filename="06_Figures/Rdata/site9.png")



# map site 10 = Torche

acoustic_10 <- acoustic %>%
  filter(Site=="Torche")

extent <- extent(167.536,167.813,-22.978,-22.765)
bathy10 <- crop(bathy100, extent)
spdf10 <- as(bathy10, "SpatialPixelsDataFrame")
df10 <- as.data.frame(spdf10)
colnames(df10) <- c("value", "x", "y")


site10 <- ggplot()+
  geom_tile(data=filter(df10, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_10$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#20A387FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#20A387FF", show.legend = F)+
  coord_sf(xlim = c(167.536,167.813), ylim = c(-22.978,-22.765))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site10, file = "06_Figures/Rdata/site10.rdata")
ggsave(site10, filename="06_Figures/Rdata/site10.png")


# map site 11 = Poum

acoustic_11 <- acoustic %>%
  filter(Site=="Poum")

extent <- extent(163.58,164.013,-20.31,-20.01)
bathy11 <- crop(bathy100, extent)
spdf11 <- as(bathy11, "SpatialPixelsDataFrame")
df11 <- as.data.frame(spdf11)
colnames(df11) <- c("value", "x", "y")


site11 <- ggplot()+
  geom_tile(data=filter(df11, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_11$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#FDE725FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#FDE725FF", show.legend = F)+
  coord_sf(xlim = c(163.58,164.013), ylim = c(-20.31,-20.01))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site11, file = "06_Figures/Rdata/site11.rdata")
ggsave(site11, filename="06_Figures/Rdata/site11.png")



# map site 12 = GrandLagonNord

acoustic_12 <- acoustic %>%
  filter(Site=="GrandLagonNord")

extent <- extent(163.078,163.382,-19.567,-19.356)
bathy12 <- crop(bathy100, extent)
spdf12 <- as(bathy12, "SpatialPixelsDataFrame")
df12 <- as.data.frame(spdf12)
colnames(df12) <- c("value", "x", "y")


site12 <- ggplot()+
  geom_tile(data=filter(df12, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_12$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#FDE725FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#FDE725FF", show.legend = F)+
  coord_sf(xlim = c(163.078,163.382), ylim = c(-19.567,-19.356))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site12, file = "06_Figures/Rdata/site12.rdata")
ggsave(site12, filename="06_Figures/Rdata/site12.png")



# map site 13 = Capel

acoustic_13 <- acoustic %>%
  filter(Site=="Capel") %>%
  filter(Longitude<159.7)

extent <- extent(158.928,160.364,-25.608,-24.626)
bathy13 <- crop(bathy100, extent)
spdf13 <- as(bathy13, "SpatialPixelsDataFrame")
df13 <- as.data.frame(spdf13)
colnames(df13) <- c("value", "x", "y")


site13 <- ggplot()+
  geom_tile(data=filter(df13, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_13$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#20A387FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#20A387FF", show.legend = F)+
  coord_sf(xlim = c(158.928,160.364), ylim = c(-25.608,-24.626))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site13, file = "06_Figures/Rdata/site13.rdata")
ggsave(site13, filename="06_Figures/Rdata/site13.png")




# map site 14 = Argo

acoustic_14 <- acoustic %>%
  filter(Site=="Argo")

extent <- extent(159.25,159.888,-23.443,-22.985)
bathy14 <- crop(bathy100, extent)
spdf14 <- as(bathy14, "SpatialPixelsDataFrame")
df14 <- as.data.frame(spdf14)
colnames(df14) <- c("value", "x", "y")


site14 <- ggplot()+
  geom_tile(data=filter(df14, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_14$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#B40F20", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#B40F20", show.legend = F)+
  coord_sf(xlim = c(159.25,159.888), ylim = c(-23.443,-22.985))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site14, file = "06_Figures/Rdata/site14.rdata")
ggsave(site14, filename="06_Figures/Rdata/site14.png")



# map site 15 = Fairway

acoustic_15 <- acoustic %>%
  filter(Site=="Fairway")

extent <- extent(162.017,162.559,-21.248,-20.874)
bathy15 <- crop(bathy100, extent)
spdf15 <- as(bathy15, "SpatialPixelsDataFrame")
df15 <- as.data.frame(spdf15)
colnames(df15) <- c("value", "x", "y")


site15 <- ggplot()+
  geom_tile(data=filter(df15, !is.na(value)), aes(x = x, y = y, fill = value), show.legend = F)+
  scale_fill_gradient(low = 'black', high = 'white',na.value = NA)+
  geom_sf(aes(), data = world, fill = "white", col="black") +
  geom_sf(size=2, data= acoustic_15$coords, shape=20, col="black", show.legend = F)+
  geom_sf(size=3.5, data= edna$coords, shape=21, col="black", fill="#20A387FF", show.legend = F)+
  geom_sf(size=3, data= bruvs$coords, shape=24, col="black", fill="#20A387FF", show.legend = F)+
  coord_sf(xlim = c(162.017,162.559), ylim = c(-21.248,-20.874))+
  labs(x="", y="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.key.height = unit(3, "mm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


save(site15, file = "06_Figures/Rdata/site15.rdata")
ggsave(site15, filename="06_Figures/Rdata/site15.png")


##################################################################
# Assemble figure 1

site1_6 <- ggarrange(site1, site6, ncol=2, labels = c("B", "C"))

fig1 <- ggarrange(map_global, site1_6, nrow=2, labels = c("A", ""), heights = c(1.2, 1), common.legend = T, legend = "bottom")

ggsave(fig1, filename = "06_Figures/Figure1_test.png", width = 10, height = 11)






##################################################################
# Encart Australie

ZEE <- st_read("c://Users/mathon/Desktop/PhD/Projets/Seamounts/SIG/World_EEZ/eez_boundaries_v11.shp")
ZEE_NC <- ZEE %>%
  filter(TERRITORY1=="New Caledonia") %>%
  filter(LINE_TYPE!="Straight Baseline")

ZEE_contour <- ZEE_NC[6]


ggplot()+
  geom_sf(aes(), data = world, fill = "grey80", col="black") +
  geom_sf(aes(), data=ZEE_contour, fill=NA, col="red", size=2)+
  coord_sf(xlim = c(113, 173), ylim = c(-40, -10))+
  guides(col=FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


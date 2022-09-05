library(ggplot2)
library(sf)
library(stars)
library(raster)
library(terra)
library(ggpubr)


# load bathy and transform to keep land
bathy <- raster("00_metadata/environmental/bathytopoMNT100m/MNT-nettoye_v3_FINAL.tif")
extent <- extent(158.1504, 171.7728,-25.632, -17.856)
bathy <- crop(bathy, extent)
sr <- "+proj=utm +zone=58 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
bathy <- projectRaster(bathy, crs = sr)
bathy[bathy>=0] <- 1
bathy[bathy < 0] <- NA

bathy_pol <- rasterToPolygons(bathy)
bathy_sf <- st_as_sf(bathy_pol)

save(bathy_sf, file="06_Figures/Rdata/bathy_sf.rdata")



######################################################################################
# load prioritization data

# blm=0.001
load("05_Marine_Spatial_Planning/03_Prioritization/Solution_blm_0.001/sol_2d_data.rdata")

data <- as.data.frame(sol_2d_data)
data$color <- NA
for (i in 1:nrow(data)) {
  if((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#FFFF19"
  }
  if ((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#8DA2C6"
  }
  if((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#FF7373"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#5A8539"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#F3670E"
  }
  if ((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#CB2C3C"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==1))
    data[i,"color"] <- "#8E400D"
}

count_0.001 <- as.data.frame(table(data$color))
count_0.001$zone <- c("depth1-2","depth2","all","depth3","depth1")

sol_2d_data$color <- data$color

plot_0.001 <- ggplot(sol_2d_data) +
  geom_sf(col=sol_2d_data$color)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("BLM = 0.001")+
  theme(plot.title = element_text(size=12, face="bold"),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))



# blm=0
load("05_Marine_Spatial_Planning/03_Prioritization/Solution_blm_0/sol_2d_data.rdata")

data <- as.data.frame(sol_2d_data)
data$color <- NA
for (i in 1:nrow(data)) {
  if((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#FFFF19"
  }
  if ((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#8DA2C6"
  }
  if((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#FF7373"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#5A8539"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#F3670E"
  }
  if ((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#CB2C3C"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==1))
    data[i,"color"] <- "#8E400D"
}

count_0 <- as.data.frame(table(data$color))
count_0$zone <- c("depth1-2","depth2","all","depth2-3","depth1-3","depth3","depth1")
for(i in 1:nrow(count_0)){
  count_0[i, "perc"] <- (count_0[i, "Freq"]/sum(count_0$Freq))*100
}

sol_2d_data$color <- data$color


plot_0 <- ggplot(sol_2d_data) +
  geom_sf(col=sol_2d_data$color)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("BLM = 0")+
  theme(plot.title = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))




# blm=1
load("05_Marine_Spatial_Planning/03_Prioritization/Solution_blm_1/sol_2d_data.rdata")

data <- as.data.frame(sol_2d_data)
data$color <- NA
for (i in 1:nrow(data)) {
  if((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#FFFF19"
  }
  if ((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#8DA2C6"
  }
  if((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#FF7373"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==0)){
    data[i,"color"] <- "#5A8539"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==0) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#F3670E"
  }
  if ((data[i, "solution_1_depth_1"]==0) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==1)){
    data[i,"color"] <- "#CB2C3C"
  }
  if ((data[i, "solution_1_depth_1"]==1) & (data[i,"solution_1_depth_2"]==1) & (data[i,"solution_1_depth_3"]==1))
    data[i,"color"] <- "#8E400D"
}

count_1 <- as.data.frame(table(data$color))
count_1$zone <- c("depth1-2","depth2","all","depth1")
for(i in 1:nrow(count_1)){
  count_1[i, "perc"] <- (count_1[i, "Freq"]/sum(count_1$Freq))*100
}

sol_2d_data$color <- data$color


plot_1 <- ggplot(sol_2d_data) +
  geom_sf(col=sol_2d_data$color)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("BLM = 1")+
  theme(plot.title = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


#############################################################
# assemble plot

plot_total <- ggarrange(plot_0, plot_1, nrow = 2, ncol = 1, labels=c("A", "B"))

ggsave(plot_total, filename = "06_Figures/plot_planification_map.png", width = 8, height = 10)
ggsave(plot_total, filename = "06_Figures/plot_planification_map.pdf", width = 8, height = 10)








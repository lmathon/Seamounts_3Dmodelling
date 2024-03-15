library(ggplot2)
library(sf)
library(stars)
library(raster)
library(terra)
library(ggpubr)
library(tidyverse)


# load land data
load("06_Figures/Rdata/bathy_sf.rdata")

# load MPA data and merge
MPA1 <- read_sf("05_Marine_Spatial_Planning/04_Comparison_MPA/MPA_data/Aires_protégées_du_gouvernement_de_la_Nouvelle-Calédonie.shp")
MPA2 <- read_sf("05_Marine_Spatial_Planning/04_Comparison_MPA/MPA_data/NC_AMP_NordSud.shp")

MPA2 <- st_transform(MPA2, crs = st_crs(MPA1))

MPA1 <- MPA1[,c(2,10)]
MPA2 <- MPA2[,c(2,16)]
names(MPA2) <- names(MPA1)

MPA <- rbind(MPA1,MPA2)


# define x and y limits of the map
xlabs1 = seq(158, 172,2)
ylabs1 = seq(-26,-16,2)

# Plot MPA
plot_MPA <- ggplot(MPA) +
  geom_sf(col="orange", fill="orange", alpha=0.5)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks = xlabs1, labels = paste0(xlabs1,'°E')) +
  scale_y_continuous(breaks = ylabs1, labels = paste0(ylabs1,'°S')) +
  ggtitle("Current MPAs in the Marine Park of the Coral Sea")+
  theme(plot.title = element_text(size=11, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# load BLM10 data
load("05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Solution30_blm_10/sol30_2d_data.rdata")

# remove rows with no prioritized cells
sol30_blm10 <- sol_2d_data %>%
  filter(solution_1_depth_1>0 | solution_1_depth_2>0 | solution_1_depth_3>0)

# Change crs to match MPA
sol30_blm10 <- st_transform(sol30_blm10, crs = st_crs(MPA))

# define x and y limits of the map
xlabs2 = seq(158, 172,2)
ylabs2 = seq(-25,-18,1)

plot_sol30 <- ggplot(sol30_blm10) +
  geom_sf(col="blue", fill="blue", alpha=0.5)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks = xlabs2, labels = paste0(xlabs2,'°E')) +
  scale_y_continuous(breaks = ylabs2, labels = paste0(ylabs2,'°S')) +
  ggtitle("Prioritized areas with target 30% and BLM=10")+
  theme(plot.title = element_text(size=11, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))



# compute common areas between MPA and solution
intersect <- st_intersection(sol30_blm10, MPA)
perc <- nrow(intersect)/nrow(sol30_blm10)*100

xlabs3 = seq(158, 172,2)
ylabs3 = seq(-25,-18,1)


# Plot common areas
plot_intersect <- ggplot(intersect) +
  geom_sf(col="darkgreen", fill="darkgreen", alpha=0.5)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks = xlabs3, labels = paste0(xlabs3,'°E')) +
  scale_y_continuous(breaks = ylabs3, labels = paste0(ylabs3,'°S')) +
  ggtitle("Areas from BLM10 solution already included in MPAs")+
  theme(plot.title = element_text(size=11, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# compute difference between MPA and solution
diff <- st_difference(sol30_blm10, st_union(st_geometry(intersect)))


# Plot unprotected areas
plot_diff <- ggplot(diff) +
  geom_sf(col="red", fill="red", alpha=0.5)+
  geom_sf(data = bathy_sf, fill = "grey50", col="grey50")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks = xlabs3, labels = paste0(xlabs3,'°E')) +
  scale_y_continuous(breaks = ylabs3, labels = paste0(ylabs3,'°S')) +
  ggtitle("Areas from BLM10 solution not included in MPAs")+
  theme(plot.title = element_text(size=11, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# plot all together

ggarrange(plot_MPA, plot_sol30, plot_intersect, plot_diff, nrow=2, ncol=2)

ggsave(file="05_Marine_Spatial_Planning/04_Comparison_MPA/Sup_Comparison_MPA_BLM10.jpg", width=13, height=10)

library(tidyverse)
library(fda)

load("02_formating_data/02_pelagic/Rdata/edna_richness_pelagic.rdata")
load("00_metadata/edna_explanatory_variables_pelagic.rdata")

edna_richness <- left_join(edna_richness_pelagic, edna_var[,c("Station", "Site", "Sampling_depth")])

facet_site <- ggplot(edna_richness)+
  geom_line(aes(x=Sampling_depth, y=richness_tot), size=1)+
  coord_flip()+
  scale_x_reverse()+
  ylab("richness")+
  xlab("depth")+
  theme_bw()+
  facet_wrap(~Site)


ggsave(facet_site, file="03_preliminary_analyses/02_pelagic/plot_richness_site.png")


edna_richness$Station <- sub("_.*", "", edna_richness$Station)
edna_richness$Station <- sub("-.*", "", edna_richness$Station)
edna_richness <- edna_richness[,-c(3,4)]
spread <- spread(edna_richness, Station, richness_tot)


depth <- as.vector(spread$Sampling_depth)
profils <- spread[,-1]
depth <-    -(depth)

# profil
matplot(profils, depth, type = 'l', las = 1, xlab = "MOTU Richness", ylab = "Depth")


mean_richness <- rowMeans(profils, na.rm = T)
points(mean_richness, depth, type = 'b', col = "black", pch=16)

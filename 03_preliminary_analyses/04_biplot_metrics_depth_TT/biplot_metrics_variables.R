library(tidyverse)
library(ggplot2)
library(ggpubr)

########################################################################################################
# BRUVS richness

load("02_formating_data/01_Benthic/Rdata/bruvs_richness_all.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_richness_all$Station <- rownames(bruvs_richness_all)
richness_bruvs <- left_join(bruvs_var, bruvs_richness_all)


bruvs_rich_depth <- ggplot(richness_bruvs, aes(x=BottomDepth, y=richness_tot))+
  geom_point()+
  ylab("Species richness BRUVS")+
  xlab("Bottom Depth")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


bruvs_rich_TT <- ggplot(richness_bruvs, aes(x=TravelTime, y=richness_tot))+
  geom_point()+
  ylab("Species richness BRUVS")+
  xlab("Travel Time")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


########################################################################################################
# BRUVS abundance

load("02_formating_data/01_Benthic/Rdata/bruvs_abundance_all.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")


abundance_bruvs <- left_join(bruvs_var, bruvs_abundance_all)


bruvs_abund_depth <- ggplot(abundance_bruvs, aes(x=BottomDepth, y=abundance_tot))+
  geom_point()+
  ylab("Species abundance BRUVS")+
  xlab("Bottom Depth")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


bruvs_abund_TT <- ggplot(abundance_bruvs, aes(x=TravelTime, y=abundance_tot))+
  geom_point()+
  ylab("Species abundance BRUVS")+
  xlab("Travel Time")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


########################################################################################################
# BRUVS biomass

load("02_formating_data/01_Benthic/Rdata/bruvs_biomass_all.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_biomass_all$Station <- rownames(bruvs_biomass_all)
biomass_bruvs <- left_join(bruvs_var, bruvs_biomass_all)


bruvs_biomass_depth <- ggplot(biomass_bruvs, aes(x=BottomDepth, y=biomass_tot))+
  geom_point()+
  ylab("Species biomass BRUVS")+
  xlab("Bottom Depth")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


bruvs_biomass_TT <- ggplot(biomass_bruvs, aes(x=TravelTime, y=biomass_tot))+
  geom_point()+
  ylab("Species biomass BRUVS")+
  xlab("Travel Time")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


########################################################################################################
# eDNA richness

load("02_formating_data/01_Benthic/Rdata/edna_richness_benthic.rdata")
load("00_metadata/edna_explanatory_variables_benthic.rdata")

richness_edna <- left_join(edna_var, edna_richness_benthic)
richness_edna$BottomDepth <- as.numeric(richness_edna$BottomDepth)


edna_rich_depth <- ggplot(richness_edna, aes(x=BottomDepth, y=richness_tot))+
  geom_point()+
  ylab("MOTU richness eDNA")+
  xlab("Bottom Depth")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


edna_rich_TT <- ggplot(richness_edna, aes(x=TravelTime, y=richness_tot))+
  geom_point()+
  ylab("MOTU richness eDNA")+
  xlab("Travel Time")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


#########################################################################################################
# Acoustic


load("02_formating_data/01_Benthic/Rdata/acoustic_benthic.rdata")
load("00_metadata/acoustic_explanatory_variables_benthic.rdata")

acoustic <- cbind(acoustic_fond, acoustic_var[,-c(1,2)])


acoustic_depth <- ggplot(acoustic, aes(x=BottomDepth, y=AcousticFond))+
  geom_point()+
  xlim(0,800)+
  ylab("Acoustic biomass")+
  xlab("Bottom Depth")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


acoustic_TT <- ggplot(acoustic, aes(x=TravelTime, y=AcousticFond))+
  geom_point()+
  ylab("Acoustic biomass")+
  xlab("Travel Time")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


###############################################################################################################
# plot metrics ~ Depth

depth <- ggarrange(bruvs_rich_depth, bruvs_abund_depth, bruvs_biomass_depth, edna_rich_depth, acoustic_depth, nrow = 3, ncol=2, labels = c("A","B","C","D","E"))
ggsave("03_preliminary_analyses/04_biplot_metrics_depth_TT/metrics_depth.png", width=8.5, height = 9)


###############################################################################################################
# plot metrics ~ Travel Time

TT <- ggarrange(bruvs_rich_TT, bruvs_abund_TT, bruvs_biomass_TT, edna_rich_TT, acoustic_TT, nrow = 3, ncol=2, labels = c("A","B","C","D","E"))
ggsave("03_preliminary_analyses/04_biplot_metrics_depth_TT/metrics_TravelTime.png", width=8.5, height = 9)

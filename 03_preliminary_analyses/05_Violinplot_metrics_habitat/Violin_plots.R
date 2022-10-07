library(tidyverse)
library(ggplot2)
library(ggpubr)

########################################################################################################
# BRUVS richness

load("02_formating_data/01_Benthic/Rdata/bruvs_richness_all.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_richness_all$Station <- rownames(bruvs_richness_all)
richness_bruvs <- left_join(bruvs_var, bruvs_richness_all)

richness_bruvs$Habitat2 <- gsub("DeepSlope", "DeepSlope150", richness_bruvs$Habitat2)
richness_bruvs$Habitat3 <- factor(richness_bruvs$Habitat2, levels = c("Summit50", "DeepSlope150", "Summit250", "Summit500"))


BRUVS_rich <- ggplot(richness_bruvs)+
  geom_violin(aes(richness_tot, Habitat3, fill=Habitat3), draw_quantiles = 0.5, show.legend = F)+
  #geom_boxplot(aes(richness_tot, Habitat3), width=0.1)+
  labs(y="Habitat", x="Species richness BRUVS")+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))



########################################################################################################
# BRUVS abundance

load("02_formating_data/01_Benthic/Rdata/bruvs_abundance_all.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")


abundance_bruvs <- left_join(bruvs_var, bruvs_abundance_all)

abundance_bruvs$Habitat2 <- gsub("DeepSlope", "DeepSlope150", abundance_bruvs$Habitat2)
abundance_bruvs$Habitat3 <- factor(abundance_bruvs$Habitat2, levels = c("Summit50", "DeepSlope150", "Summit250", "Summit500"))


BRUVS_abund <- ggplot(abundance_bruvs)+
  geom_violin(aes(abundance_tot, Habitat3, fill=Habitat3), draw_quantiles = 0.5, show.legend = F)+
  labs(y="Habitat", x="Species abundance BRUVS")+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))



########################################################################################################
# BRUVS biomass

load("02_formating_data/01_Benthic/Rdata/bruvs_biomass_all.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_biomass_all$Station <- rownames(bruvs_biomass_all)
biomass_bruvs <- left_join(bruvs_var, bruvs_biomass_all)

biomass_bruvs$Habitat2 <- gsub("DeepSlope", "DeepSlope150", biomass_bruvs$Habitat2)
biomass_bruvs$Habitat3 <- factor(biomass_bruvs$Habitat2, levels = c("Summit50", "DeepSlope150", "Summit250", "Summit500"))


BRUVS_biomass <- ggplot(biomass_bruvs)+
  geom_violin(aes(biomass_tot, Habitat3, fill=Habitat3), draw_quantiles = 0.5, show.legend = F)+
  labs(y="Habitat", x="Species biomass BRUVS")+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))




########################################################################################################
# eDNA richness

load("02_formating_data/01_Benthic/Rdata/edna_richness_benthic.rdata")
load("00_metadata/edna_explanatory_variables_benthic.rdata")

richness_edna <- left_join(edna_var, edna_richness_benthic)
richness_edna$BottomDepth <- as.numeric(richness_edna$BottomDepth)

richness_edna$Habitat2 <- gsub("DeepSlope", "DeepSlope150", richness_edna$Habitat2)
richness_edna$Habitat3 <- factor(richness_edna$Habitat2, levels = c("Summit50", "DeepSlope150", "Summit250", "Summit500"))


edna_richness <- ggplot(richness_edna)+
  geom_violin(aes(richness_tot, Habitat3, fill=Habitat3), draw_quantiles = 0.5, show.legend = F)+
  labs(y="Habitat", x="MOTU richness eDNA")+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


########################################################################################################
# Acoustic


load("02_formating_data/01_Benthic/Rdata/acoustic_benthic.rdata")
load("00_metadata/acoustic_explanatory_variables_benthic.rdata")

acoustic <- cbind(acoustic_fond, acoustic_var[,-c(1,2)])

acoustic$Habitat2 <- gsub("DeepSlope", "DeepSlope150", acoustic$Habitat2)
acoustic$Habitat3 <- factor(acoustic$Habitat2, levels = c("Summit50", "DeepSlope150", "Summit250", "Summit500"))


acoustic_biomass <- ggplot(acoustic)+
  geom_violin(aes(AcousticFond, Habitat3, fill=Habitat3), draw_quantiles = 0.5, show.legend = F)+
  labs(y="Habitat", x="Acoustic biomass")+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


###############################################################################################################
# plot all

all <- ggarrange(BRUVS_rich, BRUVS_abund, BRUVS_biomass, edna_richness, acoustic_biomass, nrow = 3, ncol=2, labels = c("A","B","C","D","E"))
ggsave("03_preliminary_analyses/05_Violinplot_metrics_habitat/All_metrics.png", width=8.5, height = 9)


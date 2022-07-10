library(tidyverse)


load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/benthic_acoustic_predict.rdata")
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/bruvs_richness_predict.rdata")
load("04_Modelling/01_benthic/01_BRUVs/03_BRT_biomass_BRUVS/BRT_Outputs/bruvs_biomass_predict.rdata")
load("04_Modelling/01_benthic/01_BRUVs/02_BRT_abundance_BRUVS/BRT_Outputs/bruvs_abundance_predict.rdata")
load("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/benthic_motu_predict.rdata")

cor.test(bruvs_abundance_predict$bruvs_abundance, bruvs_biomass_predict$bruvs_biomass, method="spearman")

cor.test(bruvs_richness_predict$bruvs_richness, bruvs_biomass_predict$bruvs_biomass, method="spearman")

cor.test(bruvs_richness_predict$bruvs_richness, bruvs_abundance_predict$bruvs_abundance, method="spearman")


cor.test(benthic_motu_predict$benthic_motus, bruvs_richness_predict$bruvs_richness, method="spearman")

cor.test(benthic_motu_predict$benthic_motus, bruvs_biomass_predict$bruvs_biomass, method="spearman")

cor.test(benthic_motu_predict$benthic_motus, benthic_acoustic_predict$acoustic_predict, method="spearman")

cor.test(bruvs_biomass_predict$bruvs_biomass, benthic_acoustic_predict$acoustic_predict, method="spearman")

cor.test(bruvs_abundance_predict$bruvs_abundance, benthic_acoustic_predict$acoustic_predict, method="spearman")

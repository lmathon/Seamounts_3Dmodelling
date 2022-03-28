library(tidyverse)
library(rfishbase)

bruvs <- read.csv("01_Raw_data/BruvsDataForLaetitia.csv", sep=";")

# --------------------------------------------------------------------------------------
#### Species biomass matrix ####
# --------------------------------------------------------------------------------------

bruvs_species <- bruvs[,c("Station", "Species", "Biomassg")]
bruvs_species$Biomassg <- as.numeric(bruvs_species$Biomassg)

# One column per species
bruvs_species <- pivot_wider(bruvs_species, names_from="Species", values_from="Biomassg", values_fill=NA)
bruvs_species <- as.data.frame(bruvs_species)
bruvs_species <- bruvs_species %>% replace(.=="NULL", 0)

bruvs_species[,c(2:191)] <- as.numeric(unlist(bruvs_species[,c(2:191)]))
rownames(bruvs_species) <- bruvs_species$Station
bruvs_species <- bruvs_species[,-1]

# --------------------------------------------------------------------------------------
#### Biomass all species ####
# --------------------------------------------------------------------------------------


# Sum of total abundance per station
bruvs_biomass_all <- as.data.frame(rowSums(bruvs_species))
colnames(bruvs_biomass_all) <- "biomass_tot"

# transform abundance data in log(x+1)
bruvs_biomass_all$Logbiomass_tot=log(bruvs_biomass_all$biomass_tot + 1)


save(bruvs_biomass_all, file="02_formating_data/01_Benthic/Rdata/bruvs_biomass_all.rdata")

library(tidyverse)
library(rfishbase)

bruvs <- read.csv("01_Raw_data/BruvsDataForLaetitia.csv", sep=";")

# ----------------------------------------------------------------------------------------
# explanatory variables
# ----------------------------------------------------------------------------------------

bruvs_var <- read.csv("01_Raw_data/Bruvs_Environmental_Variables_SEAMOUNTS_November2021.csv", sep=",")

#  transform travel time in hours
bruvs_var$TravelTime.Hour=bruvs_var$TravelTime / 3600

# transform distances in km
bruvs_var$ReefMinDist=bruvs_var$ReefMinDist.m/1000
bruvs_var$LandMinDist=bruvs_var$LandMinDist.m/1000

bruvs_var <- bruvs_var[,-c(16:18,24:38)]   
names(bruvs_var)[21] <- "BottomDepth"
names(bruvs_var)[19] <- "SummitAreaKm2"
names(bruvs_var)[22] <- "TravelTime"

# change summit area and summit rugosity for deepslope
for (i in 1:nrow(bruvs_var)) {
  if (bruvs_var[i,"Habitat"]=="DeepSlope"){
    bruvs_var[i,"ValleyDepth"] <- 3383
    bruvs_var[i,"SummitDepth"] <- 0
    bruvs_var[i,"Height"] <- 3383
    bruvs_var[i,"SummitAreaKm2"] <- 19268.7292212099
    bruvs_var[i,"SummitRugosity"] <- 16.680511910144
  }
}

bruvs_var$Habitat <- gsub("Summit500", "Seamount", bruvs_var$Habitat)
bruvs_var$Habitat <- gsub("Summit250", "Seamount", bruvs_var$Habitat)
bruvs_var$Habitat <- gsub("Summit50", "Seamount", bruvs_var$Habitat)

save(bruvs_var, file="00_metadata/bruvs_explanatory_variables.rdata")


# --------------------------------------------------------------------------------------
#### Species abundance matrix ####
# --------------------------------------------------------------------------------------

bruvs_species <- bruvs[,c("Station", "Species", "MaxN")]
bruvs_species$MaxN <- as.numeric(bruvs_species$MaxN)

# One column per species
bruvs_species <- pivot_wider(bruvs_species, names_from="Species", values_from="MaxN", values_fill=NA)
bruvs_species <- as.data.frame(bruvs_species)
bruvs_species <- bruvs_species %>% replace(.=="NULL", 0)

bruvs_species[,c(2:191)] <- as.numeric(unlist(bruvs_species[,c(2:191)]))
rownames(bruvs_species) <- bruvs_species$Station
bruvs_species <- bruvs_species[,-1]

save(bruvs_species, file="02_formating_data/01_Benthic/Rdata/bruvs_species_matrix.rdata")

# --------------------------------------------------------------------------------------
#### Abundance all species ####
# --------------------------------------------------------------------------------------

# Sum of total abundance per station
bruvs_species$abundance_tot <- rowSums(bruvs_species)

# transform abundance data in log(x+1)
bruvs_species$Logabundance_tot=log(bruvs_species$abundance_tot + 1)


# select only columns abundance and log(abundance)
bruvs_abundance_all <- bruvs_species[,c("abundance_tot", "Logabundance_tot")]

save(bruvs_abundance_all, file="02_formating_data/01_Bottom/Rdata/bruvs_abundance_all.rdata")


# ------------------------------------------------------------------------------------
####  Abundance Sharks ####
# ------------------------------------------------------------------------------------
# get species taxo and length from fishbase
x <- load_taxa()
species <- collect(x)

species$Species <- gsub(" ", "_", species$Species)

#join class with bruvs dataset
bruvs <- left_join(bruvs, species[,c("Species", "Class")])


bruvs <- bruvs %>% filter(Class=="Elasmobranchii")

bruvs_chondri <- bruvs[,c("Station", "Species", "MaxN")]
bruvs_chondri$MaxN <- as.numeric(bruvs_chondri$MaxN)

# One column per species
bruvs_chondri <- pivot_wider(bruvs_chondri, names_from="Species", values_from="MaxN", values_fill=NA)
bruvs_chondri <- as.data.frame(bruvs_chondri)
bruvs_chondri[is.na(bruvs_chondri)] <- 0

# Sum of total abundance per station
bruvs_chondri[,c(2:22)] <- as.numeric(unlist(bruvs_chondri[,c(2:22)]))
bruvs_chondri$abundance_tot <- rowSums(bruvs_chondri[,c(2:22)])

# transform abundance data in log(x+1)
bruvs_chondri$Logabundance_tot=log(bruvs_chondri$abundance_tot + 1)


# select only columns abundance and log(abundance)
bruvs_abundance_chondri <- bruvs_chondri[,c("Station", "abundance_tot", "Logabundance_tot")]

save(bruvs_abundance_chondri, file="02_formating_data/01_Benthic/Rdata/bruvs_abundance_chondri.rdata")

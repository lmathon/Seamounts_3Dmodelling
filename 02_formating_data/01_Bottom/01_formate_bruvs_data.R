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
bruvs_var$ReefMinDist.km=bruvs_var$ReefMinDist.m/1000
bruvs_var$LandMinDist.km=bruvs_var$LandMinDist.m/1000

save(bruvs_var, file="00_metadata/bruvs_explanatory_variables.rdata")

# ----------------------------------------------------------------------------------------
# get species taxo and length from fishbase
x <- load_taxa()
species <- collect(x)

species$Species <- gsub(" ", "_", species$Species)
#join class with bruvs dataset
bruvs <- left_join(bruvs, species[,c("Species", "Class")])

# --------------------------------------------------------------------------------------
#### All species ####
# --------------------------------------------------------------------------------------

bruvs_species <- bruvs[,c("Station", "Species", "MaxN")]
bruvs_species$MaxN <- as.numeric(bruvs_species$MaxN)

# One column per species
bruvs_species <- pivot_wider(bruvs_species, names_from="Species", values_from="MaxN", values_fill=NA)
bruvs_species <- as.data.frame(bruvs_species)
bruvs_species <- bruvs_species %>% replace(.=="NULL", 0)

bruvs_species[,c(2:191)] <- as.numeric(unlist(bruvs_species[,c(2:191)]))
save(bruvs_species, file="02_formating_data/01_Bottom/Rdata/bruvs_species_matrix.rdata")


# Sum of total abundance per station
bruvs_species$abundance_tot <- rowSums(bruvs_species[,c(2:191)])

# transform abundance data in log(x+1)
bruvs_species$Logabundance_tot=log(bruvs_species$abundance_tot + 1)


# select only columns abundance and log(abundance)
bruvs_abundance_all <- bruvs_species[,c("Station", "abundance_tot", "Logabundance_tot")]

save(bruvs_abundance_all, file="02_formating_data/01_Bottom/Rdata/bruvs_abundance_all.rdata")

# ------------------------------------------------------------------------------------
####  Frequent species ####
# ------------------------------------------------------------------------------------
col <- names(bruvs_species_variable[,c(2:191)])
count_occ <- data.frame()
for (i in 1:length(col)) {
  count_occ[i, "Species"] <- col[i]
  count_occ[i, "Occ"] <- sum(bruvs_species_variable[,col[i]]!=0)
}

count_20 <- count_occ %>% filter(Occ > 20)

bruvs_frequent_species <- cbind(bruvs_species_variable[,1], bruvs_species_variable[names(bruvs_species_variable)%in%count_20$Species], bruvs_species_variable[,c(194:234)])
names(bruvs_frequent_species)[names(bruvs_frequent_species)=="bruvs_species_variable[, 1]"] <- "Station"  

save(bruvs_frequent_species, file="02_formating_data/01_Bottom/Rdata/bruvs_frequent_species.rdata")

# ------------------------------------------------------------------------------------
####  Abundance Sharks ####
# ------------------------------------------------------------------------------------
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

save(bruvs_abundance_chondri, file="02_formating_data/01_Bottom/Rdata/bruvs_abundance_chondri.rdata")

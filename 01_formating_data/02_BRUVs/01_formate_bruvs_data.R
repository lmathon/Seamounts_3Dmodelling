library(tidyverse)

bruvs <- read.csv("01_formating_data/02_BRUVs/BruvsDataForLaetitia.csv", sep=";")

bruvs_species <- bruvs[,c("Station", "Species", "MaxN")]
bruvs_species$MaxN <- as.numeric(bruvs_species$MaxN)

# One column per species
bruvs_species <- pivot_wider(bruvs_species, names_from="Species", values_from="MaxN", values_fill=NA)
bruvs_species <- as.data.frame(bruvs_species)
bruvs_species <- bruvs_species %>% replace(.=="NULL", 0)

# Sum of total abundance per station
bruvs_species[,c(2:191)] <- as.numeric(unlist(bruvs_species[,c(2:191)]))
bruvs_species$abundance_tot <- rowSums(bruvs_species[,c(2:191)])

# transform abundance data in log(x+1)
bruvs_species$Logabundance_tot=log(bruvs_species$abundance_tot + 1)

# Join with explanatory variables
var <- read.csv("01_formating_data/02_BRUVs/Bruvs_Environmental_Variables_SEAMOUNTS_November2021.csv", sep=",")

bruvs_species_variable <- left_join(bruvs_species, var, by="Station")

#  transform travel time in hours
bruvs_species_variable$TravelTime.Hour=bruvs_species_variable$TravelTime / 3600

# transform distances in km
bruvs_species_variable$ReefMinDist.km=bruvs_species_variable$ReefMinDist.m/1000
bruvs_species_variable$LandMinDist.km=bruvs_species_variable$LandMinDist.m/1000

write.csv(bruvs_species_variable, "01_formating_data/02_BRUVs/Bruvs_species_variables.csv", row.names = F)
save(bruvs_species_variable, file="01_formating_data/02_BRUVs/Rdata/bruvs_species_variables.rdata")


#### 
# subset dataset with frequent species
library(tidyverse)

## Update variables in acoustic metadata
load("00_metadata/acoustic_explanatory_variables.rdata")

acoustic_var$Site <- gsub("Nepoui", "PoyaNepoui", acoustic_var$Site)
acoustic_var$Site <- gsub("JumeauOuest", "JumeauWest", acoustic_var$Site)

acoustic_var <- acoustic_var %>%
  mutate(Habitat = case_when(
    Site %in% c("Noumea", "PoyaNepoui", "Poum", "GLN") ~ "DeepSlope",
    Site %in% c("Antigonia", "Torche", "Capel", "Fairway") ~ "Summit50",
    Site %in% c("JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova") ~ "Summit250",
    Site %in% c("Stylaster", "IleDesPins", "Eponge") ~ "Summit500"
  ))



colnames(acoustic_var) <- c("Longitude","Latitude","Site","Day","Habitat","SummitDepth","ValleyDepth","Height",                      
                            "SummitAreaKm2","SummitRugosity","BottomDepth","SSTmean","SSTmin","SSTmax","SSTsd","Chla",
                            "EastwardVelocity","NorthwardVelocity","Salinity","seafloorTemp",
                            "SuspendedParticulateMatter","ReefMinDist.m","LandMinDist.m","TravelTime")


# change summit area and summit rugosity for deepslope
for (i in 1:nrow(acoustic_var)) {
  if (acoustic_var[i,"Habitat"]=="DeepSlope"){
    acoustic_var[i,"ValleyDepth"] <- 3383
    acoustic_var[i,"SummitDepth"] <- 0
    acoustic_var[i,"Height"] <- 3383
    acoustic_var[i,"SummitAreaKm2"] <- 19268.7292212099
    acoustic_var[i,"SummitRugosity"] <- 16.680511910144
  }
}

acoustic_var <- acoustic_var[-25]
save(acoustic_var, file="00_metadata/acoustic_explanatory_variables.rdata")

#################################################################################################
## update variables in bruvs metadata
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var <- bruvs_var[,-c(24:38)]   
names(bruvs_var)[24] <- "BottomDepth"
names(bruvs_var)[22] <- "SummitAreaKm2"

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

save(bruvs_var, file="00_metadata/bruvs_explanatory_variables.rdata")


#################################################################################################
## update variables in eDNA metadata fond
load("00_metadata/edna_explanatory_variables_fond.rdata")

colnames(edna_var)[24] <- "BottomDepth"
colnames(edna_var)[22] <- "SummitAreaKm2"

# change summit area and summit rugosity for deepslope
for (i in 1:nrow(edna_var)) {
  if (edna_var[i,"Habitat"]=="DeepSlope"){
    edna_var[i,"ValleyDepth"] <- 3383
    edna_var[i,"SummitDepth"] <- 0
    edna_var[i,"Height"] <- 3383
    edna_var[i,"SummitAreaKm2"] <- 19268.7292212099
    edna_var[i,"SummitRugosity"] <- 16.680511910144
  }
}

edna_var <- edna_var[-25]

save(edna_var, file="00_metadata/edna_explanatory_variables_fond.rdata")


#################################################################################################
## update variables in eDNA metadata
load("00_metadata/edna_explanatory_variables_pelagic.rdata")

colnames(edna_var)[24] <- "BottomDepth"
colnames(edna_var)[22] <- "SummitAreaKm2"


# change summit area and summit rugosity for deepslope
for (i in 1:nrow(edna_var)) {
  if (edna_var[i,"Habitat"]=="DeepSlope"){
    edna_var[i,"ValleyDepth"] <- 3383
    edna_var[i,"SummitDepth"] <- 0
    edna_var[i,"Height"] <- 3383
    edna_var[i,"SummitAreaKm2"] <- 19268.7292212099
    edna_var[i,"SummitRugosity"] <- 16.680511910144
  }
}

edna_var$BottomDepth <- gsub(">", "", edna_var$BottomDepth)
edna_var$BottomDepth <- gsub("sondeur éteint", NA, edna_var$BottomDepth)

save(edna_var, file="00_metadata/edna_explanatory_variables_pelagic.rdata")

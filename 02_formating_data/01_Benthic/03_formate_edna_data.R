library(tidyverse)

load("01_Raw_data/Clean_eDNA/Rdata/02-clean-data.Rdata")
var <- read.csv("01_Raw_data/eDNA_Environmental_Variables_SEAMOUNTS_November2021.csv", sep=",")

var <- var %>%
  filter(Habitat != "Pelagic")

edna <- df_all_filters %>%
  filter(habitat_type %in% c("Sommet"))

edna_motus <- edna[,c("code_explo", "definition", "count_reads")]

# Pull PCR replicate
edna_motus <- edna_motus %>%
  group_by(.dots = c("code_explo", "definition")) %>% 
  summarise(n_reads = sum(count_reads)) %>% 
  ungroup()

# One column per MOTU
edna_motus <- pivot_wider(edna_motus, names_from="definition", values_from="n_reads", values_fill=NA)
edna_motus[is.na(edna_motus)] <- 0
names(edna_motus)[names(edna_motus)=="code_explo"] <- "Station"

# complete with stations with 0 MOTUs
edna_motus <- full_join(edna_motus, var[,c("Station", "Site")])
edna_motus <- edna_motus[, -ncol(edna_motus)]
edna_motus[is.na(edna_motus)] <- 0

rownames(edna_motus) <- edna_motus$Station
edna_motus <- edna_motus[,-1]

save(edna_motus, file="02_formating_data/01_Benthic/Rdata/edna_motus_matrix.rdata")

# Sum of total richness per station
edna_motus$richness_tot <- rowSums(edna_motus)
edna_motus$logrichness_tot <- log(edna_motus$richness_tot+1)

# keep only richness and log
edna_richness_fond <- edna_motus[,c("richness_tot", "logrichness_tot")]

# save rdata
save(edna_richness_fond, file = "02_formating_data/01_Benthic/Rdata/edna_richness_fond.rdata")


#--------------------------------------------------------------------------------------------------------------------------------------
## Explanatory variables
#--------------------------------------------------------------------------------------------------------------------------------------

edna_var <- read.csv("01_Raw_data/EdnaDataForLaetitia.csv", sep=";")
edna_var <- edna_var[,c("Station", "Sampling_depth")]
edna_var <- edna_var %>% distinct(Station, .keep_all=T)

edna_var <- left_join(var, edna_var)

# change summit area and summit rugosity for deepslope
for (i in 1:nrow(edna_var)) {
  if (edna_var[i,"Habitat"]=="DeepSlope"){
    edna_var[i,"ValleyDepth"] <- 3383
    edna_var[i,"SummitDepth"] <- 0
    edna_var[i,"Height"] <- 3383
    edna_var[i,"SummitAreasKm2"] <- 19268.7292212099
    edna_var[i,"SummitRugosity"] <- 16.680511910144
  }
}


#  transform travel time in hours
edna_var$TravelTime=edna_var$TravelTime / 3600

# transform distances in km
edna_var$ReefMinDist=edna_var$ReefMinDist.m/1000
edna_var$LandMinDist=edna_var$LandMinDist.m/1000

# change Habitat variable
edna_var$Habitat <- gsub("Summit500", "Seamount", edna_var$Habitat)
edna_var$Habitat <- gsub("Summit250", "Seamount", edna_var$Habitat)
edna_var$Habitat <- gsub("Summit50", "Seamount", edna_var$Habitat)

edna_var <- edna_var[-c(17,18,25)]

colnames(edna_var) <- c("Station","Site","Longitude","Latitude","Habitat","EastwardVelocity","NorthwardVelocity","Salinity",
                        "SuspendedParticulateMatter","SSTmax","SSTmean","SSTmin","SSTsd","seafloorTemp","Chla","TravelTime",
                        "SummitDepth","ValleyDepth","Height", "SummitAreaKm2","SummitRugosity","BottomDepth","ReefMinDist","LandMinDist")


save(edna_var, file="00_metadata/edna_explanatory_variables_fond.rdata")


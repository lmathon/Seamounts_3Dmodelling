library(tidyverse)

load("01_Raw_data/Clean_eDNA/Rdata/02-clean-data.Rdata")
var <- read.csv("01_Raw_data/eDNA_Environmental_Variables_SEAMOUNTS_November2021.csv", sep=",")

var <- var %>%
  filter(Habitat == "Pelagic")

edna <- df_all_filters %>%
  filter(habitat_type %in% c("Pelagique"))

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

edna_motus$Station <- gsub("-", "_", edna_motus$Station)


edna_richness_pelagic <- edna_motus[,c("Station")]
edna_richness_pelagic$Sampling_Depth <- sub(".*_", "", edna_richness_pelagic$Station)

edna_motus <- edna_motus[,-1]
rownames(edna_motus) <- edna_richness_pelagic$Station

# Sum of total richness per station
edna_pa <- edna_motus
edna_pa[edna_pa>1] <- 1

edna_richness_pelagic$richness_tot <- rowSums(edna_pa)




# save rdata
save(edna_motus, file="02_formating_data/02_pelagic/Rdata/edna_motu_matrix_pelagic.rdata")
save(edna_richness_pelagic, file = "02_formating_data/02_pelagic/Rdata/edna_richness_pelagic.rdata")

# save explanatory variables
edna_var <- var %>%
  mutate(Habitat = case_when(
    Site %in% c("Noumea", "PoyaNepoui", "Poum", "GrandLagonNord") ~ "DeepSlope",
    Site %in% c("Antigonia", "Torche", "Capel", "Fairway","JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova","Stylaster", "IleDesPins", "Eponge") ~ "Seamount"
  ))

edna_var$Station <- gsub("-", "_", edna_var$Station)
#  transform travel time in hours
edna_var$TravelTime=edna_var$TravelTime / 3600

# transform distances in km
edna_var$ReefMinDist=edna_var$ReefMinDist.m/1000
edna_var$LandMinDist=edna_var$LandMinDist.m/1000
edna_var <- edna_var[-c(17,18)]

edna_var$Sampling_Depth <- sub(".*_", "", edna_var$Station)

colnames(edna_var) <- c("Station","Site","Latitude","Longitude","Habitat","EastwardVelocity","NorthwardVelocity","Salinity",
                        "SuspendedParticulateMatter","SSTmax","SSTmean","SSTmin","SSTsd","seafloorTemp","Chla","TravelTime",
                        "SummitDepth","ValleyDepth","Height", "SummitAreaKm2","SummitRugosity","BottomDepth","ReefMinDist",
                        "LandMinDist", "Sampling_Depth")


save(edna_var, file="00_metadata/edna_explanatory_variables_pelagic.rdata")


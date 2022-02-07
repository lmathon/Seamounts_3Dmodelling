library(tidyverse)

load("01_formating_data/01_eDNA/Rdata/02-clean-data.Rdata")
var <- read.csv("01_formating_data/01_eDNA/eDNA_Environmental_Variables_SEAMOUNTS_November2021.csv", sep=",")

edna <- df_all_filters %>%
  filter(habitat_type %in% c("Sommet", "Pelagique"))

edna_motus <- edna[,c("code_explo", "definition", "count_reads")]

# Pull PCR replicate
edna_motus <- edna_motus %>%
  group_by(.dots = c("code_explo", "definition")) %>% 
  summarise(n_reads = sum(count_reads)) %>% 
  ungroup()

# One column per MOTU
edna_motus <- pivot_wider(edna_motus, names_from="definition", values_from="n_reads", values_fill=NA)
edna_motus[is.na(edna_motus)] <- 0
edna_motus[,-1][edna_motus[,-1] >0] <- 1

# Sum of total richness per station
edna_motus$richness_tot <- rowSums(edna_motus[,c(2:442)])
edna_motus$logrichness_tot <- log(edna_motus$richness_tot+1)
names(edna_motus)[names(edna_motus)=="code_explo"] <- "Station"

# join with metadata and variables
meta <- read.csv("01_formating_data/01_eDNA/EdnaDataForLaetitia.csv", sep=";")
meta <- meta[,c("Station", "Date", "Time", "Sampling_depth")]
meta <- meta %>% distinct(Station, .keep_all=T)

edna_motu_variables <- full_join(edna_motus, meta)
edna_motu_variables <- left_join(edna_motu_variables, var)
edna_motu_variables[is.na(edna_motu_variables)] <- 0

# keep only richness and log
edna_richness_tot <- edna_motu_variables[,-c(2:442)]

# save csv and rdata
write.csv(edna_richness_tot, "01_formating_data/01_eDNA/edna_richness_tot.csv", row.names = F)
save(edna_motu_variables, file = "01_formating_data/01_eDNA/Rdata/edna_motus_variables.rdata")
save(edna_richness_tot, file = "01_formating_data/01_eDNA/Rdata/edna_richness_tot.rdata")

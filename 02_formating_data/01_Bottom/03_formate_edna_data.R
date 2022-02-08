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
edna_motus[,-1][edna_motus[,-1] >0] <- 1
names(edna_motus)[names(edna_motus)=="code_explo"] <- "Station"

# complete with stations with 0 MOTUs
edna_motus <- full_join(edna_motus, var[,c("Station", "Site")])
edna_motus <- edna_motus[, -403]
edna_motus[is.na(edna_motus)] <- 0

save(edna_motus, file="02_formating_data/01_Bottom/Rdata/edna_motus_matrix.rdata")

# Sum of total richness per station
edna_motus$richness_tot <- rowSums(edna_motus[,c(2:402)])
edna_motus$logrichness_tot <- log(edna_motus$richness_tot+1)

# keep only richness and log
edna_richness_fond <- edna_motus[,c("Station", "richness_tot", "logrichness_tot")]

# save rdata
save(edna_richness_fond, file = "02_formating_data/01_Bottom/Rdata/edna_richness_fond.rdata")

# save explanatory variables
edna_var <- read.csv("01_Raw_data/EdnaDataForLaetitia.csv", sep=";")
edna_var <- edna_var[,c("Station", "Sampling_depth")]
edna_var <- edna_var %>% distinct(Station, .keep_all=T)

edna_var <- left_join(var, edna_var)

save(edna_var, file="00_metadata/edna_explanatory_variables_fond.rdata")


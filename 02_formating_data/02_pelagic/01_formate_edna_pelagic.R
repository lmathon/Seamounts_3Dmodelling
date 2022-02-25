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
edna_motus[,-1][edna_motus[,-1] >0] <- 1
names(edna_motus)[names(edna_motus)=="code_explo"] <- "Station"

# complete with stations with 0 MOTUs
edna_motus <- full_join(edna_motus, var[,c("Station", "Site")])
edna_motus <- edna_motus[, -ncol(edna_motus)]
edna_motus[is.na(edna_motus)] <- 0


# Sum of total richness per station
edna_motus$richness_tot <- rowSums(edna_motus[,c(2:ncol(edna_motus))])


# keep only richness and log
edna_richness_pelagic <- edna_motus[,c("Station", "richness_tot")]

edna_richness_pelagic$Station <- gsub("-", "_", edna_richness_pelagic$Station)
edna_richness_pelagic$Depth <- sub(".*_", "", edna_richness_pelagic$Station)
edna_richness_pelagic$Station <- sub("_.*", "", edna_richness_pelagic$Station)

edna_richness_pelagic <- spread(edna_richness_pelagic, Depth, richness_tot)


# save rdata
save(edna_richness_pelagic, file = "02_formating_data/02_pelagic/Rdata/edna_richness_pelagic.rdata")

# save explanatory variables
var <- var %>%
  mutate(Habitat = case_when(
    Site %in% c("Noumea", "PoyaNepoui", "Poum", "GrandLagonNord") ~ "DeepSlope",
    Site %in% c("Antigonia", "Torche", "Capel", "Fairway") ~ "Summit50",
    Site %in% c("JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova") ~ "Summit250",
    Site %in% c("Stylaster", "IleDesPins", "Eponge") ~ "Summit500"
  ))
var$Station <- sub("_.*", "", var$Station)
var$Station <- sub("-.*", "", var$Station)
edna_var <- var %>% distinct(Station, .keep_all=T)

save(edna_var, file="00_metadata/edna_explanatory_variables_pelagic.rdata")


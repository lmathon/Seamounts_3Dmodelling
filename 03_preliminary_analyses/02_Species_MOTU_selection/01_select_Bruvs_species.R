library(gjam)
library(tidyverse)

load("02_formating_data/01_Benthic/Rdata/bruvs_species_matrix.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")



DP <- bruvs_var %>%
  filter(Site %in% c("Noumea", "PoyaNepoui", "Poum", "GrandLagonNord")) %>%
  distinct(Station)

SM50 <- bruvs_var %>%
  filter(Site %in% c("Antigonia", "Torche", "Capel", "Fairway")) %>%
  distinct(Station)

SM250 <- bruvs_var %>%
  filter(Site %in% c("JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova")) %>%
  distinct(Station)

SM500 <- bruvs_var %>%
  filter(Site %in% c("Stylaster", "IleDesPins", "Eponge")) %>%
  distinct(Station)


bruvs_DP <- bruvs_species[rownames(bruvs_species) %in% DP$Station,]
bruvs_SM50 <- bruvs_species[rownames(bruvs_species) %in% SM50$Station,]
bruvs_SM250 <- bruvs_species[rownames(bruvs_species) %in% SM250$Station,]
bruvs_SM500 <- bruvs_species[rownames(bruvs_species) %in% SM500$Station,]


Percenttrim=30
bruvs_SM50=gjamTrimY(bruvs_SM50, minObs =
                         round(dim(bruvs_SM50)[1]*Percenttrim/100),OTHER = FALSE)
bruvs_DP=gjamTrimY(bruvs_DP, minObs =
                               round(dim(bruvs_DP)[1]*Percenttrim/100),OTHER = FALSE)
bruvs_SM250=gjamTrimY(bruvs_SM250, minObs =
                               round(dim(bruvs_SM250)[1]*Percenttrim/100),OTHER = FALSE)
bruvs_SM500=gjamTrimY(bruvs_SM500, minObs =
                               round(dim(bruvs_SM500)[1]*Percenttrim/100),OTHER = FALSE)



SelectSp=c(row.names(as.data.frame(bruvs_SM50$nobs)),row.names(as.data.frame(bruvs_DP$nobs)),row.names(as.data.frame(bruvs_SM250$nobs)),row.names(as.data.frame(bruvs_SM500$nobs)))
length(unique(SelectSp))
sort(unique(SelectSp))


bruvs_species <- bruvs_species[,colnames(bruvs_species)%in%SelectSp]

save(bruvs_species, file="03_preliminary_analyses/02_Species_MOTU_selection/bruvs_species_selected.rdata")


df_bruvs_species <- data.frame() 
names <- colnames(bruvs_species)

for (i in 1:ncol(bruvs_species)) {
  df_bruvs_species[i,1] <- names[i]
  df_bruvs_species[i,2] <- min(bruvs_species[,i])
  df_bruvs_species[i,3] <- max(bruvs_species[,i])
  df_bruvs_species[i,4] <- mean(bruvs_species[,i])
  
}
colnames(df_bruvs_species) <- c("Species", "Min abundance", "Max abundance", "Mean abundance")

write.csv(df_bruvs_species, file = "03_preliminary_analyses/02_Species_MOTU_selection/table_bruvs_species.csv", row.names = F)

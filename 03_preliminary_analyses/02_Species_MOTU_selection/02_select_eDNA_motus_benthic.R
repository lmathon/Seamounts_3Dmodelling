library(gjam)
library(tidyverse)

load("02_formating_data/01_Benthic/Rdata/edna_motus_matrix_benthic.rdata")
load("00_metadata/edna_explanatory_variables_benthic.rdata")



DP <- edna_var %>%
  filter(Site %in% c("Noumea", "PoyaNepoui", "Poum", "GrandLagonNord")) %>%
  distinct(Station)

SM50 <- edna_var %>%
  filter(Site %in% c("Antigonia", "Torche", "Capel", "Fairway")) %>%
  distinct(Station)

SM250 <- edna_var %>%
  filter(Site %in% c("JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova")) %>%
  distinct(Station)

SM500 <- edna_var %>%
  filter(Site %in% c("Stylaster", "IleDesPins", "Eponge")) %>%
  distinct(Station)


edna_DP <- edna_motus[rownames(edna_motus) %in% DP$Station,]
edna_SM50 <- edna_motus[rownames(edna_motus) %in% SM50$Station,]
edna_SM250 <- edna_motus[rownames(edna_motus) %in% SM250$Station,]
edna_SM500 <- edna_motus[rownames(edna_motus) %in% SM500$Station,]


Percenttrim=30
edna_SM50=gjamTrimY(edna_SM50, minObs =
                       round(dim(edna_SM50)[1]*Percenttrim/100),OTHER = FALSE)
edna_DP=gjamTrimY(edna_DP, minObs =
                     round(dim(edna_DP)[1]*Percenttrim/100),OTHER = FALSE)
edna_SM250=gjamTrimY(edna_SM250, minObs =
                        round(dim(edna_SM250)[1]*Percenttrim/100),OTHER = FALSE)
edna_SM500=gjamTrimY(edna_SM500, minObs =
                        round(dim(edna_SM500)[1]*Percenttrim/100),OTHER = FALSE)



SelectSp=c(row.names(as.data.frame(edna_SM50$nobs)),row.names(as.data.frame(edna_DP$nobs)),row.names(as.data.frame(edna_SM250$nobs)),row.names(as.data.frame(edna_SM500$nobs)))
length(unique(SelectSp))
sort(unique(SelectSp))


edna_motus <- edna_motus[,colnames(edna_motus)%in%SelectSp]

save(edna_motus, file="03_preliminary_analyses/02_Species_MOTU_selection/edna_motus_selected.rdata")


df_MOTU_benthic <- data.frame() 
names <- colnames(edna_motus)

load("01_Raw_data/Clean_eDNA/Rdata/02-clean-data.Rdata")
id_taxa <- df_all_filters %>%
  distinct(definition, scientific_name_ncbi_corrected) %>%
  filter(definition %in% names)

for (i in 1:ncol(edna_motus)) {
  df_MOTU_benthic[i,1] <- names[i]
  df_MOTU_benthic[i,2] <- id_taxa %>% filter(definition==names[i]) %>% select(scientific_name_ncbi_corrected)
  df_MOTU_benthic[i,3] <- min(edna_motus[,i])
  df_MOTU_benthic[i,4] <- max(edna_motus[,i])
  df_MOTU_benthic[i,5] <- colMeans(edna_motus[,i])
}
colnames(df_MOTU_benthic) <- c("MOTU", "Assignment","Min read number", "Max read number", "Mean read number")

write.csv(df_MOTU_benthic, file = "03_preliminary_analyses/02_Species_MOTU_selection/table_MOTU_benthic.csv", row.names = F)

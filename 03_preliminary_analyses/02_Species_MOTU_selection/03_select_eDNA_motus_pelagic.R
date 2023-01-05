library(gjam)
library(tidyverse)

load("02_formating_data/02_pelagic/Rdata/edna_motu_matrix_pelagic.rdata")
load("00_metadata/edna_explanatory_variables_pelagic.rdata")



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

save(edna_motus, file="03_preliminary_analyses/02_Species_MOTU_selection/edna_motus_selected_pelagic.rdata")


df_MOTU_pelagic <- data.frame() 
names <- colnames(edna_motus)

load("01_Raw_data/Clean_eDNA/Rdata/02-clean-data.Rdata")
id_taxa <- df_all_filters %>%
  distinct(definition, scientific_name_ncbi_corrected) %>%
  filter(definition %in% names)


for (i in 1:ncol(edna_motus)) {
  df_MOTU_pelagic[i,1] <- names[i]
  df_MOTU_pelagic[i,2] <- id_taxa %>% filter(definition==names[i]) %>% select(scientific_name_ncbi_corrected)
  df_MOTU_pelagic[i,3] <- min(edna_motus[,i])
  df_MOTU_pelagic[i,4] <- max(edna_motus[,i])
  df_MOTU_pelagic[i,5] <- colMeans(edna_motus[,i])
}
colnames(df_MOTU_pelagic) <- c("MOTU", "Assignment","Min read number", "Max read number", "Mean read number")

df_MOTU_pelagic <- df_MOTU_pelagic %>%
  left_join(., df_all_filters[,c("sequence", "definition")], by=c("MOTU"="definition")) %>%
  distinct(MOTU, .keep_all=T)

write.csv(df_MOTU_pelagic, file = "03_preliminary_analyses/02_Species_MOTU_selection/table_MOTU_pelagic.csv", row.names = F)

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


Percenttrim=20
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

save(edna_motus, file="03_preliminary_analyses/edna_motus_selected.rdata")

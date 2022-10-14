library(tidyverse)
library(ggvenn)
library(nVennR)

# load bruvs species
load("02_formating_data/01_Benthic/Rdata/edna_motus_matrix_benthic.rdata")
load("00_metadata/edna_explanatory_variables_benthic.rdata")

# extract station - habitat
SM50 <- edna_var %>%
  filter(Habitat2=="Summit50")%>%
  distinct(Station)
SM50 <- as.vector(SM50$Station)

SM250 <- edna_var %>%
  filter(Habitat2=="Summit250")%>%
  distinct(Station)
SM250 <- as.vector(SM250$Station)

SM500 <- edna_var %>%
  filter(Habitat2=="Summit500")%>%
  distinct(Station)
SM500 <- as.vector(SM500$Station)

DS <- edna_var %>%
  filter(Habitat2=="DeepSlope")%>%
  distinct(Station)
DS <- as.vector(DS$Station)


# extract species list per habitat
edna_motus$Station <- rownames(edna_motus)

sp_SM50 <- edna_motus %>%
  filter(Station %in% SM50)
sp_SM50 <- sp_SM50[,-559]
sp_SM50 <- sp_SM50[, colSums(sp_SM50)>0]
sp_SM50 <- colnames(sp_SM50)


sp_SM250 <- edna_motus %>%
  filter(Station %in% SM250)
sp_SM250 <- sp_SM250[,-559]
sp_SM250 <- sp_SM250[, colSums(sp_SM250)>0]
sp_SM250 <- colnames(sp_SM250)


sp_SM500 <- edna_motus %>%
  filter(Station %in% SM500)
sp_SM500 <- sp_SM500[,-559]
sp_SM500 <- sp_SM500[, colSums(sp_SM500)>0]
sp_SM500 <- colnames(sp_SM500)


sp_DS <- edna_motus %>%
  filter(Station %in% DS)
sp_DS <- sp_DS[,-559]
sp_DS <- sp_DS[, colSums(sp_DS)>0]
sp_DS <- colnames(sp_DS)



# construct simple venn diagram
edna_venn <- list(
  Summit50 = sp_SM50,
  Summit250 = sp_SM250,
  Summit500 = sp_SM500,
  DeepSlope = sp_DS
)

vennplot_edna <- ggvenn(edna_venn, 
                         fill_color = c("#EE5E81", "#77CB81", "#FFEA5E", "#4DA7D9"),
                         stroke_size = 0.5, set_name_size = 4, show_percentage = TRUE, show_elements = FALSE)

ggsave(vennplot_edna, file="03_preliminary_analyses/06_Venn_diagram/eDNA_venn1.png")


edna_venn2 <- createVennObj(nSets = 4, sNames = c('SM50', 'SM250', 'SM500', 'DS'))
edna_venn2 <- setVennRegion(edna_venn2, c("SM50"), 95)
edna_venn2 <- setVennRegion(edna_venn2, c("SM250"), 48)
edna_venn2 <- setVennRegion(edna_venn2, c("SM500"), 21)
edna_venn2 <- setVennRegion(edna_venn2, c("DS"), 208)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "DS"), 59)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "SM250"), 16)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "SM500"), 7)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "SM500", "DS"), 7)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "SM500", "SM250"), 11)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "SM250", "DS"), 8)
edna_venn2 <- setVennRegion(edna_venn2, c("SM50", "SM500", "DS", "SM250"), 19)
edna_venn2 <- setVennRegion(edna_venn2, c("SM250", "SM500"), 21)
edna_venn2 <- setVennRegion(edna_venn2, c("SM250", "DS"), 17)
edna_venn2 <- setVennRegion(edna_venn2, c("SM250", "SM500", "DS"), 12)
edna_venn2 <- setVennRegion(edna_venn2, c("SM500", "DS"), 9)



vennplot_edna2 <- plotVenn(nVennObj = edna_venn2, systemShow=T)

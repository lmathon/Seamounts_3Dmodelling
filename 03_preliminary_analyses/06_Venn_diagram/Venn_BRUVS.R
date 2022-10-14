library(tidyverse)
library(ggvenn)
library(nVennR)

# load bruvs species
load("02_formating_data/01_Benthic/Rdata/bruvs_species_matrix.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")

# extract station - habitat
SM50 <- bruvs_var %>%
  filter(Habitat2=="Summit50")%>%
  distinct(Station)
SM50 <- as.vector(SM50$Station)

SM250 <- bruvs_var %>%
  filter(Habitat2=="Summit250")%>%
  distinct(Station)
SM250 <- as.vector(SM250$Station)

SM500 <- bruvs_var %>%
  filter(Habitat2=="Summit500")%>%
  distinct(Station)
SM500 <- as.vector(SM500$Station)

DS <- bruvs_var %>%
  filter(Habitat2=="DeepSlope")%>%
  distinct(Station)
DS <- as.vector(DS$Station)


# extract species list per habitat
bruvs_species$Station <- rownames(bruvs_species)

sp_SM50 <- bruvs_species %>%
  filter(Station %in% SM50)
sp_SM50 <- sp_SM50[, colSums(sp_SM50[,-191])>0]
sp_SM50 <- colnames(sp_SM50)


sp_SM250 <- bruvs_species %>%
  filter(Station %in% SM250)
sp_SM250 <- sp_SM250[, colSums(sp_SM250[,-191])>0]
sp_SM250 <- colnames(sp_SM250)


sp_SM500 <- bruvs_species %>%
  filter(Station %in% SM500)
sp_SM500 <- sp_SM500[, colSums(sp_SM500[,-191])>0]
sp_SM500 <- colnames(sp_SM500)


sp_DS <- bruvs_species %>%
  filter(Station %in% DS)
sp_DS <- sp_DS[, colSums(sp_DS[,-191])>0]
sp_DS <- colnames(sp_DS)



# construct simple venn diagram
bruvs_venn <- list(
  Summit50 = sp_SM50,
  Summit250 = sp_SM250,
  Summit500 = sp_SM500,
  DeepSlope = sp_DS
)

vennplot_bruvs <- ggvenn(bruvs_venn, 
                             fill_color = c("#EE5E81", "#77CB81", "#FFEA5E", "#4DA7D9"),
                             stroke_size = 0.5, set_name_size = 4, show_percentage = TRUE, show_elements = FALSE)

ggsave(vennplot_bruvs, file="03_preliminary_analyses/06_Venn_diagram/BRUVS_venn1.png")


bruvs_venn2 <- createVennObj(nSets = 4, sNames = c('SM50', 'SM250', 'SM500', 'DS'))
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50"), 53)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM250"), 5)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM500"), 15)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("DS"), 45)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "DS"), 39)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "SM250"), 3)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "SM500"), 4)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "SM500", "DS"), 3)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "SM500", "SM250"), 0)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "SM250", "DS"), 9)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM50", "SM500", "DS", "SM250"), 2)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM250", "SM500"), 7)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM250", "DS"), 4)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM250", "SM500", "DS"), 2)
bruvs_venn2 <- setVennRegion(bruvs_venn2, c("SM500", "DS"), 0)



vennplot_bruvs2 <- plotVenn(nVennObj = bruvs_venn2, systemShow=T)

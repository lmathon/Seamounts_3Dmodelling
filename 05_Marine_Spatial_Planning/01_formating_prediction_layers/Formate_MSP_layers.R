library(tidyverse)
library(raster)


##################################################################################################################################################
## BRUVS abundances species

load("04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/predictions.rdata")

# 0-200
abund_0_200 <- predictions %>%
  filter(BottomDepth <= 200)
abund_0_200$layer <- "0-200"

species <- colnames(abund_0_200[,1:15])

for (i in 1:length(species)) {
  df <- abund_0_200 %>% select(x, y, layer, species[[i]])
  assign(paste(species[i], "0_200", sep='_'),df)
}

save(Aphareusrutilans_0_200,Aprionvirescens_0_200,Carcharhinusalbimarginatus_0_200,Carcharhinusplumbeus_0_200,Epinepheluschlorostigma_0_200,
     Gymnocraniuseuanus_0_200, Lethrinusminiatus_0_200,Polymixiajaponica_0_200,Pristipomoidesargyrogrammicus_0_200,Pristipomoidesfilamentosus_0_200,
     Pristipomoidesflavipinnis_0_200, Pseudocaranxdentex_0_200, Seriolarivoliana_0_200, Squalusmegalops_0_200, Wattsiamossambica_0_200,
     file="05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/0_200_abund_species.rdata")

# 200-400
abund_200_400 <- predictions %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)
abund_200_400$layer <- "200-400"

species <- colnames(abund_200_400[,1:15])

for (i in 1:length(species)) {
  df <- abund_200_400 %>% select(x, y, layer, species[[i]])
  assign(paste(species[i], "200_400", sep='_'),df)
}

save(Aphareusrutilans_200_400,Aprionvirescens_200_400,Carcharhinusalbimarginatus_200_400,Carcharhinusplumbeus_200_400,Epinepheluschlorostigma_200_400,
     Gymnocraniuseuanus_200_400, Lethrinusminiatus_200_400,Polymixiajaponica_200_400,Pristipomoidesargyrogrammicus_200_400,Pristipomoidesfilamentosus_200_400,
     Pristipomoidesflavipinnis_200_400, Pseudocaranxdentex_200_400, Seriolarivoliana_200_400, Squalusmegalops_200_400, Wattsiamossambica_200_400,
     file="05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/200_400_abund_species.rdata")

# 400-600
abund_400_600 <- predictions %>%
  filter(BottomDepth > 400)
abund_400_600$layer <- "400-600"

species <- colnames(abund_400_600[,1:15])

for (i in 1:length(species)) {
  df <- abund_400_600 %>% select(x, y, layer, species[[i]])
  assign(paste(species[i], "400_600", sep='_'),df)
}

save(Aphareusrutilans_400_600,Aprionvirescens_400_600,Carcharhinusalbimarginatus_400_600,Carcharhinusplumbeus_400_600,Epinepheluschlorostigma_400_600,
     Gymnocraniuseuanus_400_600, Lethrinusminiatus_400_600,Polymixiajaponica_400_600,Pristipomoidesargyrogrammicus_400_600,Pristipomoidesfilamentosus_400_600,
     Pristipomoidesflavipinnis_400_600, Pseudocaranxdentex_400_600, Seriolarivoliana_400_600, Squalusmegalops_400_600, Wattsiamossambica_400_600,
     file="05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/400_600_abund_species.rdata")


##################################################################################################################################################
## eDNA reads MOTUs

load("04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/predictions.rdata")

# 0-200
reads_0_200 <- predictions %>%
  filter(BottomDepth <= 200)
reads_0_200$layer <- "0-200"

motus <- colnames(reads_0_200[,1:14])

for (i in 1:length(motus)) {
  df <- reads_0_200 %>% select(x, y, layer, motus[[i]])
  assign(paste("motu",i, "_0_200", sep=''),df)
}

save(motu1_0_200,motu2_0_200,motu3_0_200,motu4_0_200,motu5_0_200,motu6_0_200,motu7_0_200,
     motu8_0_200,motu9_0_200,motu10_0_200,motu11_0_200,motu12_0_200,motu13_0_200,motu14_0_200,
     file="05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/0_200_reads_motus.rdata")

# 200-400
reads_200_400 <- predictions %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)
reads_200_400$layer <- "200-400"

motus <- colnames(reads_200_400[,1:14])

for (i in 1:length(motus)) {
  df <- reads_200_400 %>% select(x, y, layer, motus[[i]])
  assign(paste("motu",i, "_200_400", sep=''),df)
  save
}

save(motu1_200_400,motu2_200_400,motu3_200_400,motu4_200_400,motu5_200_400,motu6_200_400,motu7_200_400,
     motu8_200_400,motu9_200_400,motu10_200_400,motu11_200_400,motu12_200_400,motu13_200_400,motu14_200_400,
     file="05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/200_400_reads_motus.rdata")

# 400-600
reads_400_600 <- predictions %>%
  filter(BottomDepth > 400)
reads_400_600$layer <- "400-600"

motus <- colnames(reads_400_600[,1:14])

for (i in 1:length(motus)) {
  df <- reads_400_600 %>% select(x, y, layer, motus[[i]])
  assign(paste("motu",i, "_400_600", sep=''),df)
  save
}

save(motu1_400_600,motu2_400_600,motu3_400_600,motu4_400_600,motu5_400_600,motu6_400_600,motu7_400_600,
     motu8_400_600,motu9_400_600,motu10_400_600,motu11_400_600,motu12_400_600,motu13_400_600,motu14_400_600,
     file="05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/400_600_reads_motus.rdata")


##################################################################################################################################################
## acoustic benthic

load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/benthic_acoustic_predict.rdata")

# 0-200
benthic_acoustic_0_200 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 200)
benthic_acoustic_0_200$layer <- "0-200"

benthic_acoustic_0_200 <- benthic_acoustic_0_200 %>% select(x,y,acoustic_predict, layer)

save(benthic_acoustic_0_200, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/benthic_acoustic_0_200.rdata")


# 200-400
benthic_acoustic_200_400 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)
benthic_acoustic_200_400$layer <- "200-400"
     
benthic_acoustic_200_400 <- benthic_acoustic_200_400 %>% select(x,y,acoustic_predict, layer)
     
save(benthic_acoustic_200_400, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/benthic_acoustic_200_400.rdata")


# 400-600
benthic_acoustic_400_600 <- benthic_acoustic_predict %>%
  filter(BottomDepth > 400)
benthic_acoustic_400_600$layer <- "400-600"

benthic_acoustic_400_600 <- benthic_acoustic_400_600 %>% select(x,y,acoustic_predict, layer)

save(benthic_acoustic_400_600, file="05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/benthic_acoustic_400_600.rdata")

################################################################################################################################################
## Raster 0-200

df_0_200 <- cbind(abund_0_200[,1:17], reads_0_200[,1:14], benthic_acoustic_0_200[,3:4])

df <- df_0_200
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_0_200 <- stack(df)

writeRaster(raster_0_200, filename = "05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/raster_0_200.tif")
save(df_0_200, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/df_0_200.rdata")


################################################################################################################################################
## Raster 200-400

df_200_400 <- cbind(abund_200_400[,1:17], reads_200_400[,1:14], benthic_acoustic_200_400[,3:4])

df <- df_200_400
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_200_400 <- stack(df)

writeRaster(raster_200_400, filename = "05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/raster_200_400.tif")
save(df_200_400, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/df_200_400.rdata")


################################################################################################################################################
## Raster 400-600

df_400_600 <- cbind(abund_400_600[,1:17], reads_400_600[,1:14], benthic_acoustic_400_600[,3:4])

df <- df_400_600
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_400_600 <- stack(df)

writeRaster(raster_400_600, filename = "05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/raster_400_600.tif")
save(df_400_600, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/df_400_600.rdata")

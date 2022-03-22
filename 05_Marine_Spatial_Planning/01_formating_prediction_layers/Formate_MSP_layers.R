library(tidyverse)
library(raster)


##################################################################################################################################################
## BRUVS abundances species

load("04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/predictions.rdata")

# 0-200
abund_0_200 <- predictions %>%
  filter(BottomDepth <= 200)

abund_0_200 <- abund_0_200 %>% dplyr::select(x,y,1:15)

# 200-400
abund_200_400 <- predictions %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

abund_200_400 <- abund_200_400 %>% dplyr::select(x,y,1:15)

# 400-600
abund_400_600 <- predictions %>%
  filter(BottomDepth > 400)

abund_400_600 <- abund_400_600 %>% dplyr::select(x,y,1:15)


##################################################################################################################################################
## eDNA reads MOTUs

load("04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/predictions.rdata")

# 0-200
reads_0_200 <- predictions %>%
  filter(BottomDepth <= 200)

reads_0_200 <- reads_0_200 %>% dplyr::select(x,y,1:14)


# 200-400
reads_200_400 <- predictions %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

reads_200_400 <- reads_200_400 %>% dplyr::select(x,y,1:14)


# 400-600
reads_400_600 <- predictions %>%
  filter(BottomDepth > 400)

reads_400_600 <- reads_400_600 %>% dplyr::select(x,y,1:14)


##################################################################################################################################################
## acoustic benthic

load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/benthic_acoustic_predict.rdata")

# 0-200
benthic_acoustic_0_200 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 200)

benthic_acoustic_0_200 <- benthic_acoustic_0_200 %>% dplyr::select(x,y,acoustic_predict)


# 200-400
benthic_acoustic_200_400 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

benthic_acoustic_200_400 <- benthic_acoustic_200_400 %>% dplyr::select(x,y,acoustic_predict)

# 400-600
benthic_acoustic_400_600 <- benthic_acoustic_predict %>%
  filter(BottomDepth > 400)

benthic_acoustic_400_600 <- benthic_acoustic_400_600 %>% dplyr::select(x,y,acoustic_predict)


##################################################################################################################################################
## BRUVs species richness

##################################################################################################################################################
## BRUVs biomass

##################################################################################################################################################
## eDNA MOTUs richness benthic

##################################################################################################################################################
## eDNA MOTUs richness pelagic

##################################################################################################################################################
## acoustic pelagic




################################################################################################################################################
## Raster 0-200

df_0_200 <- cbind(abund_0_200[,1:17], reads_0_200[,3:16], benthic_acoustic_0_200[,3])

df <- df_0_200
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_0_200 <- stack(df)

writeRaster(raster_0_200, filename = "05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/raster_0_200.tif", overwrite=TRUE)
save(df_0_200, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/df_0_200.rdata")


################################################################################################################################################
## Raster 200-400

df_200_400 <- cbind(abund_200_400[,1:17], reads_200_400[,3:16], benthic_acoustic_200_400[,3])

df <- df_200_400
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_200_400 <- stack(df)

writeRaster(raster_200_400, filename = "05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/raster_200_400.tif", overwrite=TRUE)
save(df_200_400, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/df_200_400.rdata")


################################################################################################################################################
## Raster 400-600

df_400_600 <- cbind(abund_400_600[,1:17], reads_400_600[,3:16], benthic_acoustic_400_600[,3])

df <- df_400_600
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_400_600 <- stack(df)

writeRaster(raster_400_600, filename = "05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/raster_400_600.tif", overwrite=TRUE)
save(df_400_600, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/df_400_600.rdata")

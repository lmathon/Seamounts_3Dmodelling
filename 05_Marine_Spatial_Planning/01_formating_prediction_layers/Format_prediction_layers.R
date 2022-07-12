library(tidyverse)
library(raster)
library(scales)




##################################################################################################################################################
## acoustic benthic

load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/benthic_acoustic_predict.rdata")

benthic_acoustic_predict$acoustic_predict[benthic_acoustic_predict$acoustic_predict<0] <- 0

# 0-200
benthic_acoustic_0_200 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 200)

benthic_acoustic_0_200 <- benthic_acoustic_0_200 %>% dplyr::select(x,y,acoustic_predict)
benthic_acoustic_0_200$x <- as.factor(benthic_acoustic_0_200$x)
benthic_acoustic_0_200$y <- as.factor(benthic_acoustic_0_200$y)
names(benthic_acoustic_0_200) <- c("x","y", "benthic_acoustic")

# 200-400
benthic_acoustic_200_400 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

benthic_acoustic_200_400 <- benthic_acoustic_200_400 %>% dplyr::select(x,y,acoustic_predict)
benthic_acoustic_200_400$x <- as.factor(benthic_acoustic_200_400$x)
benthic_acoustic_200_400$y <- as.factor(benthic_acoustic_200_400$y)
names(benthic_acoustic_200_400) <- c("x","y", "benthic_acoustic")

# 400-600
benthic_acoustic_400_600 <- benthic_acoustic_predict %>%
  filter(BottomDepth > 400)

benthic_acoustic_400_600 <- benthic_acoustic_400_600 %>% dplyr::select(x,y,acoustic_predict)
benthic_acoustic_400_600$x <- as.factor(benthic_acoustic_400_600$x)
benthic_acoustic_400_600$y <- as.factor(benthic_acoustic_400_600$y)
names(benthic_acoustic_400_600) <- c("x","y", "benthic_acoustic")

##################################################################################################################################################
## BRUVs species richness

load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/bruvs_richness_predict.rdata")

# 0-200
bruvs_richness_0_200 <- bruvs_richness_predict %>%
  filter(BottomDepth <= 200)

bruvs_richness_0_200 <- bruvs_richness_0_200 %>% dplyr::select(x,y,bruvs_richness)
bruvs_richness_0_200$x <- as.factor(bruvs_richness_0_200$x)
bruvs_richness_0_200$y <- as.factor(bruvs_richness_0_200$y)

# 200-400
bruvs_richness_200_400 <- bruvs_richness_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

bruvs_richness_200_400 <- bruvs_richness_200_400 %>% dplyr::select(x,y,bruvs_richness)
bruvs_richness_200_400$x <- as.factor(bruvs_richness_200_400$x)
bruvs_richness_200_400$y <- as.factor(bruvs_richness_200_400$y)

# 400-600
bruvs_richness_400_600 <- bruvs_richness_predict %>%
  filter(BottomDepth > 400)

bruvs_richness_400_600 <- bruvs_richness_400_600 %>% dplyr::select(x,y,bruvs_richness)
bruvs_richness_400_600$x <- as.factor(bruvs_richness_400_600$x)
bruvs_richness_400_600$y <- as.factor(bruvs_richness_400_600$y)

##################################################################################################################################################
## BRUVs biomass

load("04_Modelling/01_benthic/01_BRUVs/03_BRT_biomass_BRUVS/BRT_Outputs/bruvs_biomass_predict.rdata")

# 0-200
bruvs_biomass_0_200 <- bruvs_biomass_predict %>%
  filter(BottomDepth <= 200)

bruvs_biomass_0_200 <- bruvs_biomass_0_200 %>% dplyr::select(x,y,bruvs_biomass)
bruvs_biomass_0_200$x <- as.factor(bruvs_biomass_0_200$x)
bruvs_biomass_0_200$y <- as.factor(bruvs_biomass_0_200$y)

# 200-400
bruvs_biomass_200_400 <- bruvs_biomass_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

bruvs_biomass_200_400 <- bruvs_biomass_200_400 %>% dplyr::select(x,y,bruvs_biomass)
bruvs_biomass_200_400$x <- as.factor(bruvs_biomass_200_400$x)
bruvs_biomass_200_400$y <- as.factor(bruvs_biomass_200_400$y)

# 400-600
bruvs_biomass_400_600 <- bruvs_biomass_predict %>%
  filter(BottomDepth > 400)

bruvs_biomass_400_600 <- bruvs_biomass_400_600 %>% dplyr::select(x,y,bruvs_biomass)
bruvs_biomass_400_600$x <- as.factor(bruvs_biomass_400_600$x)
bruvs_biomass_400_600$y <- as.factor(bruvs_biomass_400_600$y)


##################################################################################################################################################
## BRUVs abundance

load("04_Modelling/01_benthic/01_BRUVs/02_BRT_abundance_BRUVS/BRT_Outputs/bruvs_abundance_predict.rdata")

# 0-200
bruvs_abundance_0_200 <- bruvs_abundance_predict %>%
  filter(BottomDepth <= 200)

bruvs_abundance_0_200 <- bruvs_abundance_0_200 %>% dplyr::select(x,y,bruvs_abundance)
bruvs_abundance_0_200$x <- as.factor(bruvs_abundance_0_200$x)
bruvs_abundance_0_200$y <- as.factor(bruvs_abundance_0_200$y)

# 200-400
bruvs_abundance_200_400 <- bruvs_abundance_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

bruvs_abundance_200_400 <- bruvs_abundance_200_400 %>% dplyr::select(x,y,bruvs_abundance)
bruvs_abundance_200_400$x <- as.factor(bruvs_abundance_200_400$x)
bruvs_abundance_200_400$y <- as.factor(bruvs_abundance_200_400$y)

# 400-600
bruvs_abundance_400_600 <- bruvs_abundance_predict %>%
  filter(BottomDepth > 400)

bruvs_abundance_400_600 <- bruvs_abundance_400_600 %>% dplyr::select(x,y,bruvs_abundance)
bruvs_abundance_400_600$x <- as.factor(bruvs_abundance_400_600$x)
bruvs_abundance_400_600$y <- as.factor(bruvs_abundance_400_600$y)

##################################################################################################################################################
## eDNA MOTUs richness benthic

load("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/benthic_motu_predict.rdata")

# 0-200
benthic_motu_0_200 <- benthic_motu_predict %>%
  filter(BottomDepth <= 200)

benthic_motu_0_200 <- benthic_motu_0_200 %>% dplyr::select(x,y,benthic_motus)
benthic_motu_0_200$x <- as.factor(benthic_motu_0_200$x)
benthic_motu_0_200$y <- as.factor(benthic_motu_0_200$y)

# 200-400
benthic_motu_200_400 <- benthic_motu_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

benthic_motu_200_400 <- benthic_motu_200_400 %>% dplyr::select(x,y,benthic_motus)
benthic_motu_200_400$x <- as.factor(benthic_motu_200_400$x)
benthic_motu_200_400$y <- as.factor(benthic_motu_200_400$y)

# 400-600
benthic_motu_400_600 <- benthic_motu_predict %>%
  filter(BottomDepth > 400)

benthic_motu_400_600 <- benthic_motu_400_600 %>% dplyr::select(x,y,benthic_motus)
benthic_motu_400_600$x <- as.factor(benthic_motu_400_600$x)
benthic_motu_400_600$y <- as.factor(benthic_motu_400_600$y)

##################################################################################################################################################
## eDNA MOTUs richness pelagic

load("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna/pelagic_motu_predict.rdata")

# 0_200
pelagic_motu_0_200 <- pelagic_motu_predict[,c(1,2,4:13)]
pelagic_motu_0_200$pelagic_motus <- rowMeans(pelagic_motu_0_200[,3:12], na.rm=TRUE)

pelagic_motu_0_200 <- pelagic_motu_0_200 %>% dplyr::select(x,y,pelagic_motus)
pelagic_motu_0_200$x <- as.character(pelagic_motu_0_200$x)
pelagic_motu_0_200$x <- as.numeric(pelagic_motu_0_200$x)
pelagic_motu_0_200$y <- as.character(pelagic_motu_0_200$y)
pelagic_motu_0_200$y <- as.numeric(pelagic_motu_0_200$y)

df <- pelagic_motu_0_200
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pred1 <- raster(df)
raster::plot(pred1,axes=FALSE, main="0 - 200m")

pelagic_motu_0_200$x <- as.factor(pelagic_motu_0_200$x)
pelagic_motu_0_200$y <- as.factor(pelagic_motu_0_200$y)

# 200_400
pelagic_motu_200_400 <- pelagic_motu_predict[,c(1,2,14:23)]
pelagic_motu_200_400$pelagic_motus <- rowMeans(pelagic_motu_200_400[,3:12], na.rm=TRUE)

pelagic_motu_200_400 <- pelagic_motu_200_400 %>% dplyr::select(x,y,pelagic_motus)
pelagic_motu_200_400$x <- as.character(pelagic_motu_200_400$x)
pelagic_motu_200_400$x <- as.numeric(pelagic_motu_200_400$x)
pelagic_motu_200_400$y <- as.character(pelagic_motu_200_400$y)
pelagic_motu_200_400$y <- as.numeric(pelagic_motu_200_400$y)

df <- pelagic_motu_200_400
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pred2 <- raster(df)
raster::plot(pred2,axes=FALSE, main="200 - 400m")

pelagic_motu_200_400$x <- as.factor(pelagic_motu_200_400$x)
pelagic_motu_200_400$y <- as.factor(pelagic_motu_200_400$y)

# 400_600
pelagic_motu_400_600 <- pelagic_motu_predict[,c(1,2,24:33)]
pelagic_motu_400_600$pelagic_motus <- rowMeans(pelagic_motu_400_600[,3:12], na.rm=TRUE)

pelagic_motu_400_600 <- pelagic_motu_400_600 %>% dplyr::select(x,y,pelagic_motus)
pelagic_motu_400_600$x <- as.character(pelagic_motu_400_600$x)
pelagic_motu_400_600$x <- as.numeric(pelagic_motu_400_600$x)
pelagic_motu_400_600$y <- as.character(pelagic_motu_400_600$y)
pelagic_motu_400_600$y <- as.numeric(pelagic_motu_400_600$y)

df <- pelagic_motu_400_600
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pred3 <- raster(df)
raster::plot(pred3,axes=FALSE, main="400 - 600m")

pelagic_motu_400_600$x <- as.factor(pelagic_motu_400_600$x)
pelagic_motu_400_600$y <- as.factor(pelagic_motu_400_600$y)

##################################################################################################################################################
## acoustic pelagic

load("04_Modelling/02_pelagic/01_acoustic/BRT_Output_acoustic/pelagic_acoustic_predict.rdata")

# 0_200
pelagic_acoustic_0_200 <- pelagic_acoustic_predict[,c(1,2,4:13)]
pelagic_acoustic_0_200$pelagic_acoustic <- rowSums(pelagic_acoustic_0_200[,3:12], na.rm=TRUE)

pelagic_acoustic_0_200 <- pelagic_acoustic_0_200 %>% dplyr::select(x,y,pelagic_acoustic)
pelagic_acoustic_0_200$x <- as.character(pelagic_acoustic_0_200$x)
pelagic_acoustic_0_200$x <- as.numeric(pelagic_acoustic_0_200$x)
pelagic_acoustic_0_200$y <- as.character(pelagic_acoustic_0_200$y)
pelagic_acoustic_0_200$y <- as.numeric(pelagic_acoustic_0_200$y)

df <- pelagic_acoustic_0_200
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pred1 <- raster(df)
raster::plot(pred1,axes=FALSE, main="0 - 200m")

pelagic_acoustic_0_200$x <- as.factor(pelagic_acoustic_0_200$x)
pelagic_acoustic_0_200$y <- as.factor(pelagic_acoustic_0_200$y)

# 200_400
pelagic_acoustic_200_400 <- pelagic_acoustic_predict[,c(1,2,14:23)]
pelagic_acoustic_200_400$pelagic_acoustic <- rowSums(pelagic_acoustic_200_400[,3:12], na.rm=TRUE)

pelagic_acoustic_200_400 <- pelagic_acoustic_200_400 %>% dplyr::select(x,y,pelagic_acoustic)
pelagic_acoustic_200_400$x <- as.character(pelagic_acoustic_200_400$x)
pelagic_acoustic_200_400$x <- as.numeric(pelagic_acoustic_200_400$x)
pelagic_acoustic_200_400$y <- as.character(pelagic_acoustic_200_400$y)
pelagic_acoustic_200_400$y <- as.numeric(pelagic_acoustic_200_400$y)

df <- pelagic_acoustic_200_400
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pred2 <- raster(df)
raster::plot(pred2,axes=FALSE, main="200 - 400m")

pelagic_acoustic_200_400$x <- as.factor(pelagic_acoustic_200_400$x)
pelagic_acoustic_200_400$y <- as.factor(pelagic_acoustic_200_400$y)

# 400_600
pelagic_acoustic_400_600 <- pelagic_acoustic_predict[,c(1,2,24:33)]
pelagic_acoustic_400_600$pelagic_acoustic <- rowSums(pelagic_acoustic_400_600[,3:12], na.rm=TRUE)

pelagic_acoustic_400_600 <- pelagic_acoustic_400_600 %>% dplyr::select(x,y,pelagic_acoustic)
pelagic_acoustic_400_600$x <- as.character(pelagic_acoustic_400_600$x)
pelagic_acoustic_400_600$x <- as.numeric(pelagic_acoustic_400_600$x)
pelagic_acoustic_400_600$y <- as.character(pelagic_acoustic_400_600$y)
pelagic_acoustic_400_600$y <- as.numeric(pelagic_acoustic_400_600$y)

df <- pelagic_acoustic_400_600
coordinates(df) <- ~x+y
gridded(df) <- TRUE
pred3 <- raster(df)
raster::plot(pred3,axes=FALSE, main="400 - 600m")

pelagic_acoustic_400_600$x <- as.factor(pelagic_acoustic_400_600$x)
pelagic_acoustic_400_600$y <- as.factor(pelagic_acoustic_400_600$y)




################################################################################################################################################
## df 0-200

df_0_200 <- left_join(pelagic_motu_0_200, bruvs_abundance_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, benthic_acoustic_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, bruvs_richness_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, bruvs_biomass_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, benthic_motu_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, pelagic_acoustic_0_200, by=c("x","y"))



df_0_200[is.na(df_0_200)] <- 0
df_0_200$layer <- 200



################################################################################################################################################
## df 200-400

df_200_400 <- left_join(pelagic_motu_200_400, bruvs_abundance_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, benthic_acoustic_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, bruvs_richness_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, bruvs_biomass_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, benthic_motu_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, pelagic_acoustic_200_400, by=c("x","y"))

df_200_400[is.na(df_200_400)] <- 0
df_200_400$layer <- 400


################################################################################################################################################
## df 400-600

df_400_600 <- left_join(pelagic_motu_400_600, bruvs_abundance_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, benthic_acoustic_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, bruvs_richness_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, bruvs_biomass_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, benthic_motu_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, pelagic_acoustic_400_600, by=c("x","y"))

df_400_600[is.na(df_400_600)] <- 0
df_400_600$layer <- 600


### Bind all to rescale

df_all <- rbind(df_0_200, df_200_400, df_400_600)

for (i in 3:(ncol(df_all)-1)) {
  df_all[,i] <- rescale(df_all[,i])

}

df_0_200 <- df_all %>%
  filter(layer==200)
df_200_400 <- df_all %>%
  filter(layer==400)
df_400_600 <- df_all %>%
  filter(layer==600)

df_0_200 <- df_0_200[,-10]
df_200_400 <- df_200_400[,-10]
df_400_600 <- df_400_600[,-10]

save(df_0_200, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_0_200.rdata")
save(df_200_400, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_200_400.rdata")
save(df_400_600, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_400_600.rdata")

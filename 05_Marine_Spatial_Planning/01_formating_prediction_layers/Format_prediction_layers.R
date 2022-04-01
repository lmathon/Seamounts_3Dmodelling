library(tidyverse)
library(raster)
library(scales)


##################################################################################################################################################
## BRUVS abundances species

load("04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/predictions.rdata")

for (i in 1:15) {
  predictions[,i] <- rescale(predictions[,i])
  
}

# 0-200
abund_0_200 <- predictions %>%
  filter(BottomDepth <= 200)

abund_0_200 <- abund_0_200 %>% dplyr::select(x,y,1:15)
abund_0_200$x <- as.factor(abund_0_200$x)
abund_0_200$y <- as.factor(abund_0_200$y)

# 200-400
abund_200_400 <- predictions %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

abund_200_400 <- abund_200_400 %>% dplyr::select(x,y,1:15)
abund_200_400$x <- as.factor(abund_200_400$x)
abund_200_400$y <- as.factor(abund_200_400$y)

# 400-600
abund_400_600 <- predictions %>%
  filter(BottomDepth > 400)

abund_400_600 <- abund_400_600 %>% dplyr::select(x,y,1:15)
abund_400_600$x <- as.factor(abund_400_600$x)
abund_400_600$y <- as.factor(abund_400_600$y)


##################################################################################################################################################
## eDNA reads MOTUs

load("04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/predictions.rdata")

for (i in 1:14) {
  predictions[,i] <- rescale(predictions[,i])
  
}

# 0-200
reads_0_200 <- predictions %>%
  filter(BottomDepth <= 200)

reads_0_200 <- reads_0_200 %>% dplyr::select(x,y,1:14)
reads_0_200$x <- as.factor(reads_0_200$x)
reads_0_200$y <- as.factor(reads_0_200$y)


# 200-400
reads_200_400 <- predictions %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

reads_200_400 <- reads_200_400 %>% dplyr::select(x,y,1:14)
reads_200_400$x <- as.factor(reads_200_400$x)
reads_200_400$y <- as.factor(reads_200_400$y)


# 400-600
reads_400_600 <- predictions %>%
  filter(BottomDepth > 400)

reads_400_600 <- reads_400_600 %>% dplyr::select(x,y,1:14)
reads_400_600$x <- as.factor(reads_400_600$x)
reads_400_600$y <- as.factor(reads_400_600$y)


##################################################################################################################################################
## acoustic benthic

load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/benthic_acoustic_predict.rdata")
benthic_acoustic_predict$acoustic_predict <- rescale(benthic_acoustic_predict$acoustic_predict)

# 0-200
benthic_acoustic_0_200 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 200)

benthic_acoustic_0_200 <- benthic_acoustic_0_200 %>% dplyr::select(x,y,acoustic_predict)
benthic_acoustic_0_200$x <- as.factor(benthic_acoustic_0_200$x)
benthic_acoustic_0_200$y <- as.factor(benthic_acoustic_0_200$y)
names(benthic_acoustic_0_200) <- c("x","y", "pelagic_acoustic")

# 200-400
benthic_acoustic_200_400 <- benthic_acoustic_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

benthic_acoustic_200_400 <- benthic_acoustic_200_400 %>% dplyr::select(x,y,acoustic_predict)
benthic_acoustic_200_400$x <- as.factor(benthic_acoustic_200_400$x)
benthic_acoustic_200_400$y <- as.factor(benthic_acoustic_200_400$y)
names(benthic_acoustic_200_400) <- c("x","y", "pelagic_acoustic")

# 400-600
benthic_acoustic_400_600 <- benthic_acoustic_predict %>%
  filter(BottomDepth > 400)

benthic_acoustic_400_600 <- benthic_acoustic_400_600 %>% dplyr::select(x,y,acoustic_predict)
benthic_acoustic_400_600$x <- as.factor(benthic_acoustic_400_600$x)
benthic_acoustic_400_600$y <- as.factor(benthic_acoustic_400_600$y)
names(benthic_acoustic_400_600) <- c("x","y", "pelagic_acoustic")

##################################################################################################################################################
## BRUVs species richness

load("04_Modelling/01_benthic/01_BRUVs/BRT_Output_bruvs/bruvs_richness_predict.rdata")
bruvs_richness_predict$bruvs_richness <- rescale(bruvs_richness_predict$bruvs_richness)

# 0-200
bruvs_richness_0_200 <- bruvs_richness_predict %>%
  filter(BottomDepth <= 200)

bruvs_richness_0_200 <- bruvs_richness_0_200 %>% dplyr::select(x,y,bruvs_richness)
bruvs_richness_0_200$x <- as.factor(bruvs_richness_0_200$x)
bruvs_richness_0_200$y <- as.factor(bruvs_richness_0_200$y)
names(bruvs_richness_0_200) <- c("x","y", "bruvs_richness")

# 200-400
bruvs_richness_200_400 <- bruvs_richness_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

bruvs_richness_200_400 <- bruvs_richness_200_400 %>% dplyr::select(x,y,bruvs_richness)
bruvs_richness_200_400$x <- as.factor(bruvs_richness_200_400$x)
bruvs_richness_200_400$y <- as.factor(bruvs_richness_200_400$y)
names(bruvs_richness_200_400) <- c("x","y", "bruvs_richness")

# 400-600
bruvs_richness_400_600 <- bruvs_richness_predict %>%
  filter(BottomDepth > 400)

bruvs_richness_400_600 <- bruvs_richness_400_600 %>% dplyr::select(x,y,bruvs_richness)
bruvs_richness_400_600$x <- as.factor(bruvs_richness_400_600$x)
bruvs_richness_400_600$y <- as.factor(bruvs_richness_400_600$y)
names(bruvs_richness_400_600) <- c("x","y", "bruvs_richness")

##################################################################################################################################################
## BRUVs biomass

load("04_Modelling/01_benthic/01_BRUVs/BRT_Output_bruvs/bruvs_biomass_predict.rdata")
bruvs_biomass_predict$bruvs_biomass <- rescale(bruvs_biomass_predict$bruvs_biomass)

# 0-200
bruvs_biomass_0_200 <- bruvs_biomass_predict %>%
  filter(BottomDepth <= 200)

bruvs_biomass_0_200 <- bruvs_biomass_0_200 %>% dplyr::select(x,y,bruvs_biomass)
bruvs_biomass_0_200$x <- as.factor(bruvs_biomass_0_200$x)
bruvs_biomass_0_200$y <- as.factor(bruvs_biomass_0_200$y)
names(bruvs_biomass_0_200) <- c("x","y", "bruvs_biomass")

# 200-400
bruvs_biomass_200_400 <- bruvs_biomass_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

bruvs_biomass_200_400 <- bruvs_biomass_200_400 %>% dplyr::select(x,y,bruvs_biomass)
bruvs_biomass_200_400$x <- as.factor(bruvs_biomass_200_400$x)
bruvs_biomass_200_400$y <- as.factor(bruvs_biomass_200_400$y)
names(bruvs_biomass_200_400) <- c("x","y", "bruvs_biomass")

# 400-600
bruvs_biomass_400_600 <- bruvs_biomass_predict %>%
  filter(BottomDepth > 400)

bruvs_biomass_400_600 <- bruvs_biomass_400_600 %>% dplyr::select(x,y,bruvs_biomass)
bruvs_biomass_400_600$x <- as.factor(bruvs_biomass_400_600$x)
bruvs_biomass_400_600$y <- as.factor(bruvs_biomass_400_600$y)
names(bruvs_biomass_400_600) <- c("x","y", "bruvs_biomass")

##################################################################################################################################################
## eDNA MOTUs richness benthic

load("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/benthic_motu_predict.rdata")
benthic_motu_predict$benthic_motus <- rescale(benthic_motu_predict$benthic_motus)

# 0-200
benthic_motu_0_200 <- benthic_motu_predict %>%
  filter(BottomDepth <= 200)

benthic_motu_0_200 <- benthic_motu_0_200 %>% dplyr::select(x,y,benthic_motus)
benthic_motu_0_200$x <- as.factor(benthic_motu_0_200$x)
benthic_motu_0_200$y <- as.factor(benthic_motu_0_200$y)
names(benthic_motu_0_200) <- c("x","y", "benthic_motus")

# 200-400
benthic_motu_200_400 <- benthic_motu_predict %>%
  filter(BottomDepth <= 400) %>%
  filter(BottomDepth > 200)

benthic_motu_200_400 <- benthic_motu_200_400 %>% dplyr::select(x,y,benthic_motus)
benthic_motu_200_400$x <- as.factor(benthic_motu_200_400$x)
benthic_motu_200_400$y <- as.factor(benthic_motu_200_400$y)
names(benthic_motu_200_400) <- c("x","y", "benthic_motus")

# 400-600
benthic_motu_400_600 <- benthic_motu_predict %>%
  filter(BottomDepth > 400)

benthic_motu_400_600 <- benthic_motu_400_600 %>% dplyr::select(x,y,benthic_motus)
benthic_motu_400_600$x <- as.factor(benthic_motu_400_600$x)
benthic_motu_400_600$y <- as.factor(benthic_motu_400_600$y)
names(benthic_motu_400_600) <- c("x","y", "benthic_motus")

##################################################################################################################################################
## eDNA MOTUs richness pelagic

load("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna/pelagic_motu_predict.rdata")

# 0_200
pelagic_motu_0_200 <- pelagic_motu_predict[,c(1,2,4:13)]
pelagic_motu_0_200$pelagic_motus <- rowMeans(pelagic_motu_0_200[,3:12], na.rm=TRUE)

pelagic_motu_0_200 <- pelagic_motu_0_200 %>% dplyr::select(x,y,pelagic_motus)
pelagic_motu_0_200$pelagic_motus <- rescale(pelagic_motu_0_200$pelagic_motus)
pelagic_motu_0_200$x <- as.factor(pelagic_motu_0_200$x)
pelagic_motu_0_200$y <- as.factor(pelagic_motu_0_200$y)

# 200_400
pelagic_motu_200_400 <- pelagic_motu_predict[,c(1,2,14:23)]
pelagic_motu_200_400$pelagic_motus <- rowMeans(pelagic_motu_200_400[,3:12], na.rm=TRUE)

pelagic_motu_200_400 <- pelagic_motu_200_400 %>% dplyr::select(x,y,pelagic_motus)
pelagic_motu_200_400$pelagic_motus <- rescale(pelagic_motu_200_400$pelagic_motus)
pelagic_motu_200_400$x <- as.factor(pelagic_motu_200_400$x)
pelagic_motu_200_400$y <- as.factor(pelagic_motu_200_400$y)

# 400_600
pelagic_motu_400_600 <- pelagic_motu_predict[,c(1,2,24:33)]
pelagic_motu_400_600$pelagic_motus <- rowMeans(pelagic_motu_400_600[,3:12], na.rm=TRUE)

pelagic_motu_400_600 <- pelagic_motu_400_600 %>% dplyr::select(x,y,pelagic_motus)
pelagic_motu_400_600$pelagic_motus <- rescale(pelagic_motu_400_600$pelagic_motus)
pelagic_motu_400_600$x <- as.factor(pelagic_motu_400_600$x)
pelagic_motu_400_600$y <- as.factor(pelagic_motu_400_600$y)

##################################################################################################################################################
## acoustic pelagic




################################################################################################################################################
## df 0-200

df_0_200 <- left_join(pelagic_motu_0_200, abund_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, reads_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, benthic_acoustic_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, bruvs_richness_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, bruvs_biomass_0_200, by=c("x","y"))
df_0_200 <- left_join(df_0_200, benthic_motu_0_200, by=c("x","y"))

df_0_200[is.na(df_0_200)] <- 0

save(df_0_200, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_0_200.rdata")


################################################################################################################################################
## df 200-400

df_200_400 <- left_join(pelagic_motu_200_400, abund_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, reads_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, benthic_acoustic_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, bruvs_richness_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, bruvs_biomass_200_400, by=c("x","y"))
df_200_400 <- left_join(df_200_400, benthic_motu_200_400, by=c("x","y"))

df_200_400[is.na(df_200_400)] <- 0

save(df_200_400, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_200_400.rdata")

################################################################################################################################################
## df 400-600

df_400_600 <- left_join(pelagic_motu_400_600, abund_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, reads_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, benthic_acoustic_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, bruvs_richness_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, bruvs_biomass_400_600, by=c("x","y"))
df_400_600 <- left_join(df_400_600, benthic_motu_400_600, by=c("x","y"))

df_400_600[is.na(df_400_600)] <- 0

save(df_400_600, file = "05_Marine_Spatial_Planning/01_formating_prediction_layers/Rdata/df_400_600.rdata")

library(tidyverse)
library(raster)
library(terra)

load("02_formating_data/00_Prediction_raster/Rdata/df_grandeterre2.rdata")
load("02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")

# select same variable order

df_GT <- df_grandeterre2 %>%
  dplyr::select(x,y,BottomDepth,Habitat,ValleyDepth,SummitDepth,Height,SummitAreaKm2,SummitRugosity,           
                SSTmean,SSTmin,SSTmax,SSTsd,Chla,EastwardVelocity,NorthwardVelocity,Salinity,
                seafloorTemp,SuspendedParticulateMatter,ReefMinDist,LandMinDist)
df_GT$id <- c(1:nrow(df_GT))

df_SM <- df_seamount_islands %>%
  dplyr::select(x,y,BottomDepth,Habitat,ValleyDepth,SummitDepth,Height,SummitAreaKm2,SummitRugosity,           
                SSTmean,SSTmin,SSTmax,SSTsd,Chla,EastwardVelocity,NorthwardVelocity,Salinity,
                seafloorTemp,SuspendedParticulateMatter,ReefMinDist,LandMinDist)
df_SM$id <- c((nrow(df_GT)+1):(nrow(df_GT)+nrow(df_SM)))

df_ALL <- rbind(df_GT, df_SM)

# transform into raster
df <- df_GT
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_GT <- stack(df)

df <- df_SM
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_SM <- stack(df)

# assemble together
origin(raster_GT) <- c(0,0)
origin(raster_SM) <- c(0,0)

raster_all <- merge(raster_GT, raster_SM)
names(raster_all) <- names(df_GT[,-c(1,2)])

df_all <- as.data.frame(raster_all, xy=TRUE)

df_all <- df_all %>%
  filter(!is.na(BottomDepth))

df_all <- df_all[,-4] # remove empty habitat variable
df_all <- left_join(df_all, df_ALL[,c("Habitat", "id")])

df_all$BottomDepth <- gsub("-", "", df_all$BottomDepth)
df_all$BottomDepth <- as.numeric(df_all$BottomDepth)


## df benthic : cut at 600m

df_benthic <- df_all %>%
  filter(BottomDepth<600)

df_benthic <- df_benthic[,-21]

df <- df_benthic
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_benthic <- stack(df)

plot(raster_benthic)

raster_benthic <- rast(raster_benthic)

writeRaster(raster_benthic, filename = "02_formating_data/00_Prediction_raster/Raster_df_predictions/raster_benthic.tif", overwrite=T)

save(df_benthic, file="02_formating_data/00_Prediction_raster/Raster_df_predictions/df_benthic.rdata")


## df_pelagic : cut at 2175m (deepest we sampled) and duplicate by sampling depth

df_pelagic <- df_all %>%
  filter(BottomDepth<2175)

df_pelagic <- df_pelagic[,-21]

sampling_depth <- seq(20, 800, by=20)
list_pelagic <- vector("list", 40)

for (i in 1:length(sampling_depth)) {
  list_pelagic[[i]] <- df_pelagic
  list_pelagic[[i]]$Sampling_Depth <- sampling_depth[[i]]
  
}

df_pelagic <- bind_rows(list_pelagic)

save(df_pelagic, file="02_formating_data/00_Prediction_raster/Raster_df_predictions/df_pelagic.rdata")

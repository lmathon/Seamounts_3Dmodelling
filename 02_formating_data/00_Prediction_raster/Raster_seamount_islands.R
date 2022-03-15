library(raster)
library(rgdal)
library(sp)
library(ncdf4)
library(rasterVis)
library(stringr)
library(dplyr)
library(rgeos)
library(geosphere)
library(terra)

### load rasters

raster_seamounts <- brick("02_formating_data/00_Prediction_raster/raster_seamounts.tif")
raster_islands <- brick("02_formating_data/00_Prediction_raster/raster_islands.tif")

# bind all
raster_list <- list(raster_islands, raster_seamounts)

raster_seamount_islands <- do.call(merge, raster_list)

raster_seamount_islands <- rast(raster_seamount_islands)
names(raster_seamount_islands) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")

df_seamount_islands <- as.data.frame(raster_seamount_islands, xy=TRUE)


####################################################################################################################################
### load ENVIRONMENT
SSTmean1k=raster("00_metadata/environmental/GRHSST/SSTmean1k.tif")
SSTmin1k=raster("00_metadata/environmental/GRHSST/SSTmin1k.tif")
SSTmax1k=raster("00_metadata/environmental/GRHSST/SSTmax1k.tif")
SSTsd1k=raster("00_metadata/environmental/GRHSST/SSTsd1k.tif")
ChlorA=raster("00_metadata/environmental/ChlorA/ChlorAmean4k.tif")
EastwardVelocity=raster("00_metadata/environmental/EastwardVelocity/EastwardVelocity_Surface_mean_8k.tif")
NorthwardVelocity=raster("00_metadata/environmental/NorthwardVelocity/NorthwardVelocity_Surface_mean_8k.tif")
Salinity=raster("00_metadata/environmental/Salinity/Salinity_Surface_mean_8k.tif")
SeaFloorPotentialTemperature=raster("00_metadata/environmental/SeaFloorPotentialTemperature/SeafloorTemp_Surface_mean_8k.tif")
SuspendedParticulateMatter=raster("00_metadata/environmental/SuspendedParticulateMatter/SuspMattermean4k.tif")



# assemble df and variables

df_seamount_islands$SSTmean=extract(SSTmean1k,df_seamount_islands[c(1,2)])
df_seamount_islands$SSTmin=extract(SSTmin1k,df_seamount_islands[c(1,2)])
df_seamount_islands$SSTmax=extract(SSTmax1k,df_seamount_islands[c(1,2)])
df_seamount_islands$SSTsd=extract(SSTsd1k,df_seamount_islands[c(1,2)])
df_seamount_islands$Chla=extract(ChlorA,df_seamount_islands[c(1,2)])
df_seamount_islands$EastwardVelocity=extract(EastwardVelocity,df_seamount_islands[c(1,2)])
df_seamount_islands$NorthwardVelocity=extract(NorthwardVelocity,df_seamount_islands[c(1,2)])
df_seamount_islands$Salinity=extract(Salinity,df_seamount_islands[c(1,2)])
df_seamount_islands$seafloorTemp=extract(SeaFloorPotentialTemperature,df_seamount_islands[c(1,2)])
df_seamount_islands$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,df_seamount_islands[c(1,2)])

############################################################################################################
## load travel time
TravelTime <- read.csv("02_formating_data/00_Prediction_raster/TravelTime_df_seamount_islands_aggregated.csv")
df_seamount_islands <- left_join(df_seamount_islands, TravelTime[,c(1,4)], by=c("ValleyDepth"="id"))


df_seamount_islands$BottomDepth <- df_seamount_islands$BottomDepth*(-1)
df_seamount_islands$SummitDepth <- df_seamount_islands$SummitDepth*(-1)
df_seamount_islands$ValleyDepth <- df_seamount_islands$ValleyDepth*(-1)
df_seamount_islands <- df_seamount_islands %>% filter(!is.na(Chla))
df_seamount_islands <- df_seamount_islands %>% filter(!is.na(EastwardVelocity))
df_seamount_islands$TravelTime=df_seamount_islands$TravelTime / 3600

save(df_seamount_islands, file="02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")


############################################################################################################
#Compute distance to land and reef
Geomorphology=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/NewCaledonia_v8.shp")

Reef <- Geomorphology[Geomorphology@data$REEF == 1, ]

Land <- Geomorphology[Geomorphology@data$L1_CODE == 2, ]

df_pnts=df_seamount_islands[,c(1,2)]
ReefMinDist.m = geosphere::dist2Line(df_pnts, Reef)
LandMinDist.m = geosphere::dist2Line(df_pnts, Land)
df_seamount_islands$ReefMinDist.m=ReefMinDist.m[,1]
df_seamount_islands$LandMinDist.m=LandMinDist.m[,1]


# transform distances in km
df_seamount_islands$ReefMinDist=df_seamount_islands$ReefMinDist.m/1000
df_seamount_islands$LandMinDist=df_seamount_islands$LandMinDist.m/1000
df_seamount_islands <- df_seamount_islands[,-c(21,22)]

# Transform Habitat into character in the df
df_seamount_islands <- df_seamount_islands %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Seamount",
    Habitat == 2 ~ "Seamount",
    Habitat == 3 ~ "Seamount"
  ))


# remove values > 60m for DeepSlope
df_seamount <- df_seamount_islands %>%
  filter(Habitat=="Seamount")
df_slope <- df_seamount_islands %>%
  filter(Habitat=="DeepSlope"& BottomDepth > 60)

df_seamount_islands <- rbind(df_seamount, df_slope)

# transform df back to raster multi-layers

df <- df_seamount_islands
coordinates(df) <- ~x+y
gridded(df) <- TRUE

raster_seamount_islands <- stack(df)

plot(raster_seamount_islands)

raster_seamount_islands <- rast(raster_seamount_islands)

writeRaster(raster_seamount_islands, filename = "02_formating_data/00_Prediction_raster/rasters/raster_seamount_islands.tif", overwrite=T)


save(df_seamount_islands, file="02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")

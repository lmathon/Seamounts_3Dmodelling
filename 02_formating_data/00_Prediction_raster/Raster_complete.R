if (!require("raster")) install.packages("raster")
if (!require("rgdal")) install.packages("rgdal")
if (!require("sp")) install.packages("sp")
if (!require("sf")) install.packages("sf")
if (!require("ncdf4")) install.packages("ncdf4")
if (!require("rasterVis")) install.packages("rasterVis")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("rgeos")) install.packages("rgeos")
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("geosphere")) install.packages("geosphere")


### load df seamounts et coast

load("02_formating_data/00_Prediction_raster/Rdata/df_seamounts.rdata")
load("02_formating_data/00_Prediction_raster/Rdata/df_islands.rdata")
load("02_formating_data/00_Prediction_raster/Rdata/df_grandeterre.rdata")

### load rasters

raster_seamounts <- brick("02_formating_data/00_Prediction_raster/raster_seamounts.tif")
raster_islands <- brick("02_formating_data/00_Prediction_raster/raster_islands.tif")
raster_grandeterre <- brick("02_formating_data/00_Prediction_raster/raster_grandeterre.tif")

# bind all
raster_list <- list(raster_grandeterre, raster_islands, raster_seamounts)

raster_complete <- do.call(merge, raster_list)

raster_complete <- rast(raster_complete)
names(raster_complete) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")

writeRaster(raster_complete, filename = "02_formating_data/00_Prediction_raster/raster_complete.tif", overwrite=T)

df_complete <- as.data.frame(raster_complete, xy=TRUE)

save(df_complete, file="02_formating_data/00_Prediction_raster/Rdata/df_complete.rdata")


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

df_complete$SSTmean=extract(SSTmean1k,df_complete[c(1,2)])
df_complete$SSTmin=extract(SSTmin1k,df_complete[c(1,2)])
df_complete$SSTmax=extract(SSTmax1k,df_complete[c(1,2)])
df_complete$SSTsd=extract(SSTsd1k,df_complete[c(1,2)])
df_complete$Chla=extract(ChlorA,df_complete[c(1,2)])
df_complete$EastwardVelocity=extract(EastwardVelocity,df_complete[c(1,2)])
df_complete$NorthwardVelocity=extract(NorthwardVelocity,df_complete[c(1,2)])
df_complete$Salinity=extract(Salinity,df_complete[c(1,2)])
df_complete$seafloorTemp=extract(SeaFloorPotentialTemperature,df_complete[c(1,2)])
df_complete$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,df_complete[c(1,2)])


# transform df back to raster multi-layers

df <- df_complete
coordinates(df) <- ~x+y
gridded(df) <- TRUE

raster_complete <- stack(df)

plot(raster_complete)

raster_complete <- rast(raster_complete)

writeRaster(raster_complete, filename = "02_formating_data/00_Prediction_raster/raster_complete.tif", overwrite=T)


# Transform Habitat into character in the df

df_complete <- df_complete %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Summit50",
    Habitat == 2 ~ "Summit250",
    Habitat == 3 ~ "Summit500"
  ))


save(df_complete, file="02_formating_data/00_Prediction_raster/Rdata/df_complete.rdata")
write.csv(df_complete, file="02_formating_data/00_Prediction_raster/df_prediction.csv", row.names = F)

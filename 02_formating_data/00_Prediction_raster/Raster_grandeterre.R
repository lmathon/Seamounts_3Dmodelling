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

raster_grandeterre <- brick("02_formating_data/00_Prediction_raster/raster_grandeterre.tif")

df_grandeterre2 <- as.data.frame(raster_grandeterre, xy=TRUE)

df_grandeterre2 <- df_grandeterre2 %>%
  filter(!is.na(Habitat))

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

df_grandeterre2$SSTmean=extract(SSTmean1k,df_grandeterre2[c(1,2)])
df_grandeterre2$SSTmin=extract(SSTmin1k,df_grandeterre2[c(1,2)])
df_grandeterre2$SSTmax=extract(SSTmax1k,df_grandeterre2[c(1,2)])
df_grandeterre2$SSTsd=extract(SSTsd1k,df_grandeterre2[c(1,2)])
df_grandeterre2$Chla=extract(ChlorA,df_grandeterre2[c(1,2)])
df_grandeterre2$EastwardVelocity=extract(EastwardVelocity,df_grandeterre2[c(1,2)])
df_grandeterre2$NorthwardVelocity=extract(NorthwardVelocity,df_grandeterre2[c(1,2)])
df_grandeterre2$Salinity=extract(Salinity,df_grandeterre2[c(1,2)])
df_grandeterre2$seafloorTemp=extract(SeaFloorPotentialTemperature,df_grandeterre2[c(1,2)])
df_grandeterre2$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,df_grandeterre2[c(1,2)])

############################################################################################################
#Compute distance to land and reef
Geomorphology=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/NewCaledonia_v8.shp")

Reef <- Geomorphology[Geomorphology@data$REEF == 1, ]

Land <- Geomorphology[Geomorphology@data$L1_CODE == 2, ]

df_pnts=df_grandeterre2[,c(1,2)]
ReefMinDist.m = geosphere::dist2Line(df_pnts, Reef)
LandMinDist.m = geosphere::dist2Line(df_pnts, Land)
df_grandeterre2$ReefMinDist.m=ReefMinDist.m[,1]
df_grandeterre2$LandMinDist.m=LandMinDist.m[,1]

# transform df back to raster multi-layers

df <- df_grandeterre2
coordinates(df) <- ~x+y
gridded(df) <- TRUE

raster_grandeterre2 <- stack(df)

plot(raster_grandeterre2)

raster_grandeterre2 <- rast(raster_grandeterre2)

writeRaster(raster_grandeterre2, filename = "02_formating_data/00_Prediction_raster/raster_grandeterre2.tif", overwrite=T)


# Transform Habitat into character in the df

df_grandeterre2 <- df_grandeterre2 %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Summit50",
    Habitat == 2 ~ "Summit250",
    Habitat == 3 ~ "Summit500"
  ))


save(df_grandeterre2, file="02_formating_data/00_Prediction_raster/Rdata/df_grandeterre2.rdata")

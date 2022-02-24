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

load("02_formating_data/00_Prediction_raster/df_seamounts.rdata")

# bind rows


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

#Geomorphology (land and reef)
Geomorphology=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/NewCaledonia_v8.shp")

head(Geomorphology@data)
unique(Geomorphology@data$REEF)
unique(Geomorphology@data$L1_CODE)
unique(Geomorphology@data$L1_ATTRIB)

Reef <- Geomorphology[Geomorphology@data$REEF == 1, ]
head(Reef@data)
unique(Reef@data$REEF)
plot(Reef)

Land <- Geomorphology[Geomorphology@data$L1_CODE == 2, ]
head(Land@data)
unique(Land@data$L1_CODE)
unique(Land@data$L1_ATTRIB)


# assemble df and variables

df$SSTmean1k=extract(SSTmean1k,df[c(1,2)])
df$SSTmin1k=extract(SSTmin1k,df[c(1,2)])
df$SSTmax1k=extract(SSTmax1k,df[c(1,2)])
df$SSTsd1k=extract(SSTsd1k,df[c(1,2)])
df$ChlorA=extract(ChlorA,df[c(1,2)])
df$EastwardVelocity=extract(EastwardVelocity,df[c(1,2)])
df$NorthwardVelocity=extract(NorthwardVelocity,df[c(1,2)])
df$Salinity=extract(Salinity,df[c(1,2)])
df$SeaFloorPotentialTemperature=extract(SeaFloorPotentialTemperature,df[c(1,2)])
df$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,df[c(1,2)])

df_pnts=df[,c(1,2)]
dist.Reef = geosphere::dist2Line(df_pnts, Reef)
dist.Land = geosphere::dist2Line(df_pnts, Land)
df$ShortestDistanceReef=dist.Reef[,1]
df$ShortestDistanceLand=dist.Land[,1]

# Calculate TravelTime


# transform df back to raster multi-layers

coordinates(df) <- ~x+y
gridded(df) <- TRUE

raster_complete <- stack(df)
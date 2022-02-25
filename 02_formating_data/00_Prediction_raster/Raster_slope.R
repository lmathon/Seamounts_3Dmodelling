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


# NC Bathymetry at 100 m resolution (2020 - given by Jean Roger)
Bathy_100=raster("00_metadata/environmental/bathytopoMNT100m/MNT-nettoye_v3_FINAL.tif")
Bathy_100

##################################################################################################################################
chesterfield=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_chesterfield.shp")

buffer_chesterfield <- buffer(chesterfield, width=0.1)
plot(buffer_chesterfield)

raster_chesterfield <- mask(Bathy_100, buffer_chesterfield)
plot(raster_chesterfield)

raster_chesterfield <- trim(raster_chesterfield, values=NA)
raster_chesterfield[raster_chesterfield > 0] <- NA

chesterfield_SummitDepth=round(max(values(raster_chesterfield),na.rm=TRUE))
chesterfield_ValleyDepth=round(min(values(raster_chesterfield),na.rm=TRUE))
chesterfield_Height=chesterfield_SummitDepth-chesterfield_ValleyDepth

lagoon <- shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_chesterfield_lagoon.shp")
lagoon <- aggregate(lagoon, dissolve=T)
buffer_lagoon <- buffer(lagoon, width=-0.01)


chesterfield_Summit_Poly=raster_chesterfield
values(chesterfield_Summit_Poly)[values(chesterfield_Summit_Poly) < -60] = NA

chesterfield_Summit_Poly2=mask(Bathy_100, buffer_lagoon)
chesterfield_Summit_Poly2 <- trim(chesterfield_Summit_Poly2, values=NA)
plot(chesterfield_Summit_Poly)
plot(chesterfield_Summit_Poly2, add=T)
chesterfield_Summit_Poly <- merge(chesterfield_Summit_Poly, chesterfield_Summit_Poly2)
chesterfield_SummitRugosity=sd(values(chesterfield_Summit_Poly),na.rm=TRUE)
chesterfield_SummitRugosity
values(chesterfield_Summit_Poly)[!is.na(values(chesterfield_Summit_Poly))] <- 1
plot(chesterfield_Summit_Poly)
chesterfield_Summit_Poly=rasterToPolygons(chesterfield_Summit_Poly, dissolve=T)
chesterfield_SummitArea=area(chesterfield_Summit_Poly)*1e-6


raster_chesterfield <- mask(raster_chesterfield, chesterfield_Summit_Poly, inverse=T)
plot(raster_chesterfield)

names(raster_chesterfield) <- "BottomDepth"

df_chesterfield <- as.data.frame(raster_chesterfield, xy=T)

for (i in 1:nrow(df_chesterfield)) {
  if (!is.na(df_chesterfield[i,"BottomDepth"])){
    df_chesterfield[i,"Habitat"] <- 4
    df_chesterfield[i,"ValleyDepth"] <- chesterfield_ValleyDepth
    df_chesterfield[i,"SummitDepth"] <- chesterfield_SummitDepth
    df_chesterfield[i,"Height"] <- chesterfield_Height
    df_chesterfield[i,"SummitAreaKm2"] <- chesterfield_SummitArea
    df_chesterfield[i,"SummitRugosity"] <- chesterfield_SummitRugosity
  }
}

df <- df_chesterfield
coordinates(df) <- ~x+y
gridded(df) <- TRUE

raster_chesterfield <- stack(df)

save(df_chesterfield, file="02_formating_data/00_Prediction_raster/Rdata/df_chesterfield.rdata")


##################################################################################################################################
chesterfield2=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_chesterfield2.shp")

buffer_chesterfield2 <- buffer(chesterfield2, width=0.08)
plot(buffer_chesterfield2)

raster_chesterfield2 <- mask(Bathy_100, buffer_chesterfield2)
raster_chesterfield2 <- trim(raster_chesterfield2, values=NA)
raster_chesterfield2[raster_chesterfield2 > 0] <- NA
plot(raster_chesterfield2)

chesterfield2_SummitDepth=round(max(values(raster_chesterfield2),na.rm=TRUE))
chesterfield2_ValleyDepth=round(min(values(raster_chesterfield2),na.rm=TRUE))
chesterfield2_Height=chesterfield2_SummitDepth-chesterfield2_ValleyDepth

chesterfield2_Summit_Poly=raster_chesterfield2
values(chesterfield2_Summit_Poly)[values(chesterfield2_Summit_Poly) < -60] = NA
plot(chesterfield2_Summit_Poly)
chesterfield2_SummitRugosity=sd(values(chesterfield2_Summit_Poly),na.rm=TRUE)
chesterfield2_SummitRugosity
values(chesterfield2_Summit_Poly)[!is.na(values(chesterfield2_Summit_Poly))] <- 1
plot(chesterfield2_Summit_Poly)
chesterfield2_Summit_Poly=rasterToPolygons(chesterfield2_Summit_Poly, dissolve=T)
chesterfield2_SummitArea=area(chesterfield2_Summit_Poly)*1e-6


raster_chesterfield2 <- mask(raster_chesterfield2, chesterfield2_Summit_Poly, inverse=T)
plot(raster_chesterfield2)

names(raster_chesterfield2) <- "BottomDepth"

df_chesterfield2 <- as.data.frame(raster_chesterfield2, xy=T)

for (i in 1:nrow(df_chesterfield2)) {
  if (!is.na(df_chesterfield2[i,"BottomDepth"])){
    df_chesterfield2[i,"Habitat"] <- 4
    df_chesterfield2[i,"ValleyDepth"] <- chesterfield2_ValleyDepth
    df_chesterfield2[i,"SummitDepth"] <- chesterfield2_SummitDepth
    df_chesterfield2[i,"Height"] <- chesterfield2_Height
    df_chesterfield2[i,"SummitAreaKm2"] <- chesterfield2_SummitArea
    df_chesterfield2[i,"SummitRugosity"] <- chesterfield2_SummitRugosity
  }
}

df <- df_chesterfield2
coordinates(df) <- ~x+y
gridded(df) <- TRUE

raster_chesterfield2 <- stack(df)
plot(raster_chesterfield2)

save(df_chesterfield2, file="02_formating_data/00_Prediction_raster/Rdata/df_chesterfield2.rdata")

##################################################################################################################################
bellona=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_bellona.shp")

buffer_bellona <- buffer(bellona, width=0.1)
plot(buffer_bellona)

raster_bellona <- mask(Bathy_100, buffer_bellona)
raster_bellona <- trim(raster_bellona, values=NA)
raster_bellona[raster_bellona > 0] <- NA
plot(raster_bellona)

bellona_SummitDepth=round(max(values(raster_bellona),na.rm=TRUE))
bellona_ValleyDepth=round(min(values(raster_bellona),na.rm=TRUE))
bellona_Height=bellona_SummitDepth-bellona_ValleyDepth

lagoon <- shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_bellona_lagon.shp")
lagoon <- aggregate(lagoon, dissolve=T)
buffer_lagon <- buffer(lagoon, width=-0.05)

bellona_Summit_Poly=mask(Bathy_100, buffer_lagon)
bellona_Summit_Poly <- trim(bellona_Summit_Poly, values=NA)
plot(bellona_Summit_Poly)
bellona_SummitRugosity=sd(values(bellona_Summit_Poly),na.rm=TRUE)
bellona_SummitRugosity
values(bellona_Summit_Poly)[!is.na(values(bellona_Summit_Poly))] <- 1
plot(bellona_Summit_Poly)
bellona_Summit_Poly=rasterToPolygons(bellona_Summit_Poly, dissolve=T)
bellona_SummitArea=area(bellona_Summit_Poly)*1e-6


raster_bellona <- mask(raster_bellona, bellona_Summit_Poly, inverse=T)
plot(raster_bellona)

names(raster_bellona) <- "BottomDepth"

df_bellona <- as.data.frame(raster_bellona, xy=T)

for (i in 1:nrow(df_bellona)) {
  if (!is.na(df_bellona[i,"BottomDepth"])){
    df_bellona[i,"Habitat"] <- 4
    df_bellona[i,"ValleyDepth"] <- bellona_ValleyDepth
    df_bellona[i,"SummitDepth"] <- bellona_SummitDepth
    df_bellona[i,"Height"] <- bellona_Height
    df_bellona[i,"SummitAreaKm2"] <- bellona_SummitArea
    df_bellona[i,"SummitRugosity"] <- bellona_SummitRugosity
  }
}

df <- df_bellona
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_bellona <- stack(df)
plot(raster_bellona)


save(df_bellona, file="02_formating_data/00_Prediction_raster/Rdata/df_bellona.rdata")


##################################################################################################################################
entrecasteaux1=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_entrecasteaux1.shp")

buffer_entrecasteaux1 <- buffer(entrecasteaux1, width=0.07)
plot(buffer_entrecasteaux1)

raster_entrecasteaux1 <- mask(Bathy_100, buffer_entrecasteaux1)
raster_entrecasteaux1 <- trim(raster_entrecasteaux1, values=NA)
raster_entrecasteaux1[raster_entrecasteaux1 > 0] <- NA
plot(raster_entrecasteaux1)

new_extent <- extent(162.743, 162.97, -18.30329, -17.81529)
raster_entrecasteaux1 <- crop(raster_entrecasteaux1, new_extent)

entrecasteaux1_SummitDepth=round(max(values(raster_entrecasteaux1),na.rm=TRUE))
entrecasteaux1_ValleyDepth=round(min(values(raster_entrecasteaux1),na.rm=TRUE))
entrecasteaux1_Height=entrecasteaux1_SummitDepth-entrecasteaux1_ValleyDepth


entrecasteaux1_Summit_Poly=raster_entrecasteaux1
values(entrecasteaux1_Summit_Poly)[values(entrecasteaux1_Summit_Poly) < -60] = NA
plot(entrecasteaux1_Summit_Poly)
entrecasteaux1_SummitRugosity=sd(values(entrecasteaux1_Summit_Poly),na.rm=TRUE)
entrecasteaux1_SummitRugosity
values(entrecasteaux1_Summit_Poly)[!is.na(values(entrecasteaux1_Summit_Poly))] <- 1
plot(entrecasteaux1_Summit_Poly)
entrecasteaux1_Summit_Poly=rasterToPolygons(entrecasteaux1_Summit_Poly, dissolve=T)
entrecasteaux1_SummitArea=area(entrecasteaux1_Summit_Poly)*1e-6


raster_entrecasteaux1 <- mask(raster_entrecasteaux1, entrecasteaux1_Summit_Poly, inverse=T)
plot(raster_entrecasteaux1)

names(raster_entrecasteaux1) <- "BottomDepth"

df_entrecasteaux1 <- as.data.frame(raster_entrecasteaux1, xy=T)

for (i in 1:nrow(df_entrecasteaux1)) {
  if (!is.na(df_entrecasteaux1[i,"BottomDepth"])){
    df_entrecasteaux1[i,"Habitat"] <- 4
    df_entrecasteaux1[i,"ValleyDepth"] <- entrecasteaux1_ValleyDepth
    df_entrecasteaux1[i,"SummitDepth"] <- entrecasteaux1_SummitDepth
    df_entrecasteaux1[i,"Height"] <- entrecasteaux1_Height
    df_entrecasteaux1[i,"SummitAreaKm2"] <- entrecasteaux1_SummitArea
    df_entrecasteaux1[i,"SummitRugosity"] <- entrecasteaux1_SummitRugosity
  }
}


df <- df_entrecasteaux1
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_entrecasteaux1 <- stack(df)
plot(raster_entrecasteaux1)

save(df_entrecasteaux1, file="02_formating_data/00_Prediction_raster/Rdata/df_entrecasteaux1.rdata")

##################################################################################################################################
entrecasteaux2=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_entrecasteaux2.shp")

buffer_entrecasteaux2 <- buffer(entrecasteaux2, width=0.07)
plot(buffer_entrecasteaux2)

raster_entrecasteaux2 <- mask(Bathy_100, buffer_entrecasteaux2)
raster_entrecasteaux2 <- trim(raster_entrecasteaux2, values=NA)
raster_entrecasteaux2[raster_entrecasteaux2 > 0] <- NA
plot(raster_entrecasteaux2)

new_extent <- extent(162.767, 162.94, -18.59929, -18.36729)
raster_entrecasteaux2 <- crop(raster_entrecasteaux2, new_extent)


entrecasteaux2_SummitDepth=round(max(values(raster_entrecasteaux2),na.rm=TRUE))
entrecasteaux2_ValleyDepth=round(min(values(raster_entrecasteaux2),na.rm=TRUE))
entrecasteaux2_Height=entrecasteaux2_SummitDepth-entrecasteaux2_ValleyDepth


entrecasteaux2_Summit_Poly=raster_entrecasteaux2
values(entrecasteaux2_Summit_Poly)[values(entrecasteaux2_Summit_Poly) < -60] = NA
plot(entrecasteaux2_Summit_Poly)
entrecasteaux2_SummitRugosity=sd(values(entrecasteaux2_Summit_Poly),na.rm=TRUE)
entrecasteaux2_SummitRugosity
values(entrecasteaux2_Summit_Poly)[!is.na(values(entrecasteaux2_Summit_Poly))] <- 1
plot(entrecasteaux2_Summit_Poly)
entrecasteaux2_Summit_Poly=rasterToPolygons(entrecasteaux2_Summit_Poly, dissolve=T)
entrecasteaux2_SummitArea=area(entrecasteaux2_Summit_Poly)*1e-6


raster_entrecasteaux2 <- mask(raster_entrecasteaux2, entrecasteaux2_Summit_Poly, inverse=T)
plot(raster_entrecasteaux2)

names(raster_entrecasteaux2) <- "BottomDepth"

df_entrecasteaux2 <- as.data.frame(raster_entrecasteaux2, xy=T)

for (i in 1:nrow(df_entrecasteaux2)) {
  if (!is.na(df_entrecasteaux2[i,"BottomDepth"])){
    df_entrecasteaux2[i,"Habitat"] <- 4
    df_entrecasteaux2[i,"ValleyDepth"] <- entrecasteaux2_ValleyDepth
    df_entrecasteaux2[i,"SummitDepth"] <- entrecasteaux2_SummitDepth
    df_entrecasteaux2[i,"Height"] <- entrecasteaux2_Height
    df_entrecasteaux2[i,"SummitAreaKm2"] <- entrecasteaux2_SummitArea
    df_entrecasteaux2[i,"SummitRugosity"] <- entrecasteaux2_SummitRugosity
  }
}

df <- df_entrecasteaux2
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_entrecasteaux2 <- stack(df)
plot(raster_entrecasteaux2)

save(df_entrecasteaux2, file="02_formating_data/00_Prediction_raster/Rdata/df_entrecasteaux2.rdata")

##################################################################################################################################
entrecasteaux3=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_entrecasteaux3.shp")

buffer_entrecasteaux3 <- buffer(entrecasteaux3, width=0.07)
plot(buffer_entrecasteaux3)

raster_entrecasteaux3 <- mask(Bathy_100, buffer_entrecasteaux3)
raster_entrecasteaux3 <- trim(raster_entrecasteaux3, values=NA)
raster_entrecasteaux3[raster_entrecasteaux3 > 0] <- NA
plot(raster_entrecasteaux3)

new_extent <- extent(162.94, 163.3126, -18.525, -18.25)
raster_entrecasteaux3 <- crop(raster_entrecasteaux3, new_extent)

entrecasteaux3_SummitDepth=round(max(values(raster_entrecasteaux3),na.rm=TRUE))
entrecasteaux3_ValleyDepth=round(min(values(raster_entrecasteaux3),na.rm=TRUE))
entrecasteaux3_Height=entrecasteaux3_SummitDepth-entrecasteaux3_ValleyDepth


entrecasteaux3_Summit_Poly=raster_entrecasteaux3
values(entrecasteaux3_Summit_Poly)[values(entrecasteaux3_Summit_Poly) < -60] = NA
plot(entrecasteaux3_Summit_Poly)
entrecasteaux3_SummitRugosity=sd(values(entrecasteaux3_Summit_Poly),na.rm=TRUE)
entrecasteaux3_SummitRugosity
values(entrecasteaux3_Summit_Poly)[!is.na(values(entrecasteaux3_Summit_Poly))] <- 1
plot(entrecasteaux3_Summit_Poly)
entrecasteaux3_Summit_Poly=rasterToPolygons(entrecasteaux3_Summit_Poly, dissolve=T)
entrecasteaux3_SummitArea=area(entrecasteaux3_Summit_Poly)*1e-6


raster_entrecasteaux3 <- mask(raster_entrecasteaux3, entrecasteaux3_Summit_Poly, inverse=T)
plot(raster_entrecasteaux3)

names(raster_entrecasteaux3) <- "BottomDepth"

df_entrecasteaux3 <- as.data.frame(raster_entrecasteaux3, xy=T)

for (i in 1:nrow(df_entrecasteaux3)) {
  if (!is.na(df_entrecasteaux3[i,"BottomDepth"])){
    df_entrecasteaux3[i,"Habitat"] <- 4
    df_entrecasteaux3[i,"ValleyDepth"] <- entrecasteaux3_ValleyDepth
    df_entrecasteaux3[i,"SummitDepth"] <- entrecasteaux3_SummitDepth
    df_entrecasteaux3[i,"Height"] <- entrecasteaux3_Height
    df_entrecasteaux3[i,"SummitAreaKm2"] <- entrecasteaux3_SummitArea
    df_entrecasteaux3[i,"SummitRugosity"] <- entrecasteaux3_SummitRugosity
  }
}

df <- df_entrecasteaux3
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_entrecasteaux3 <- stack(df)
plot(raster_entrecasteaux3$BottomDepth)

save(df_entrecasteaux3, file="02_formating_data/00_Prediction_raster/Rdata/df_entrecasteaux3.rdata")

##################################################################################################################################
entrecasteaux4=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_entrecasteaux4.shp")

buffer_entrecasteaux4 <- buffer(entrecasteaux4, width=0.08)
plot(buffer_entrecasteaux4)

raster_entrecasteaux4 <- mask(Bathy_100, buffer_entrecasteaux4)
raster_entrecasteaux4 <- trim(raster_entrecasteaux4, values=NA)
raster_entrecasteaux4[raster_entrecasteaux4 > 0] <- NA
plot(raster_entrecasteaux4)

new_extent <- extent(163.1158, 163.3286, -18.68249, -18.525)
raster_entrecasteaux4 <- crop(raster_entrecasteaux4, new_extent)

entrecasteaux4_SummitDepth=round(max(values(raster_entrecasteaux4),na.rm=TRUE))
entrecasteaux4_ValleyDepth=round(min(values(raster_entrecasteaux4),na.rm=TRUE))
entrecasteaux4_Height=entrecasteaux4_SummitDepth-entrecasteaux4_ValleyDepth


entrecasteaux4_Summit_Poly=raster_entrecasteaux4
values(entrecasteaux4_Summit_Poly)[values(entrecasteaux4_Summit_Poly) < -60] = NA
plot(entrecasteaux4_Summit_Poly)
entrecasteaux4_SummitRugosity=sd(values(entrecasteaux4_Summit_Poly),na.rm=TRUE)
entrecasteaux4_SummitRugosity
values(entrecasteaux4_Summit_Poly)[!is.na(values(entrecasteaux4_Summit_Poly))] <- 1
plot(entrecasteaux4_Summit_Poly)
entrecasteaux4_Summit_Poly=rasterToPolygons(entrecasteaux4_Summit_Poly, dissolve=T)
entrecasteaux4_SummitArea=area(entrecasteaux4_Summit_Poly)*1e-6


raster_entrecasteaux4 <- mask(raster_entrecasteaux4, entrecasteaux4_Summit_Poly, inverse=T)
plot(raster_entrecasteaux4)

names(raster_entrecasteaux4) <- "BottomDepth"

df_entrecasteaux4 <- as.data.frame(raster_entrecasteaux4, xy=T)

for (i in 1:nrow(df_entrecasteaux4)) {
  if (!is.na(df_entrecasteaux4[i,"BottomDepth"])){
    df_entrecasteaux4[i,"Habitat"] <- 4
    df_entrecasteaux4[i,"ValleyDepth"] <- entrecasteaux4_ValleyDepth
    df_entrecasteaux4[i,"SummitDepth"] <- entrecasteaux4_SummitDepth
    df_entrecasteaux4[i,"Height"] <- entrecasteaux4_Height
    df_entrecasteaux4[i,"SummitAreaKm2"] <- entrecasteaux4_SummitArea
    df_entrecasteaux4[i,"SummitRugosity"] <- entrecasteaux4_SummitRugosity
  }
}

df <- df_entrecasteaux4
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_entrecasteaux4 <- stack(df)
plot(raster_entrecasteaux4)

save(df_entrecasteaux4, file="02_formating_data/00_Prediction_raster/Rdata/df_entrecasteaux4.rdata")

##################################################################################################################################
entrecasteaux5=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_entrecasteaux5.shp")

buffer_entrecasteaux5 <- buffer(entrecasteaux5, width=0.1)
plot(buffer_entrecasteaux5)

raster_entrecasteaux5 <- mask(Bathy_100, buffer_entrecasteaux5)
raster_entrecasteaux5 <- trim(raster_entrecasteaux5, values=NA)
raster_entrecasteaux5[raster_entrecasteaux5 > 0] <- NA
plot(raster_entrecasteaux5)

new_extent <- extent(162.9702, 163.1286, -18.25049, -18.08409)
raster_entrecasteaux5 <- crop(raster_entrecasteaux5, new_extent)

entrecasteaux5_SummitDepth=round(max(values(raster_entrecasteaux5),na.rm=TRUE))
entrecasteaux5_ValleyDepth=round(min(values(raster_entrecasteaux5),na.rm=TRUE))
entrecasteaux5_Height=entrecasteaux5_SummitDepth-entrecasteaux5_ValleyDepth


entrecasteaux5_Summit_Poly=raster_entrecasteaux5
values(entrecasteaux5_Summit_Poly)[values(entrecasteaux5_Summit_Poly) < -60] = NA
plot(entrecasteaux5_Summit_Poly)
entrecasteaux5_SummitRugosity=sd(values(entrecasteaux5_Summit_Poly),na.rm=TRUE)
entrecasteaux5_SummitRugosity
values(entrecasteaux5_Summit_Poly)[!is.na(values(entrecasteaux5_Summit_Poly))] <- 1
plot(entrecasteaux5_Summit_Poly)
entrecasteaux5_Summit_Poly=rasterToPolygons(entrecasteaux5_Summit_Poly, dissolve=T)
entrecasteaux5_SummitArea=area(entrecasteaux5_Summit_Poly)*1e-6


raster_entrecasteaux5 <- mask(raster_entrecasteaux5, entrecasteaux5_Summit_Poly, inverse=T)
plot(raster_entrecasteaux5)

names(raster_entrecasteaux5) <- "BottomDepth"

df_entrecasteaux5 <- as.data.frame(raster_entrecasteaux5, xy=T)

for (i in 1:nrow(df_entrecasteaux5)) {
  if (!is.na(df_entrecasteaux5[i,"BottomDepth"])){
    df_entrecasteaux5[i,"Habitat"] <- 4
    df_entrecasteaux5[i,"ValleyDepth"] <- entrecasteaux5_ValleyDepth
    df_entrecasteaux5[i,"SummitDepth"] <- entrecasteaux5_SummitDepth
    df_entrecasteaux5[i,"Height"] <- entrecasteaux5_Height
    df_entrecasteaux5[i,"SummitAreaKm2"] <- entrecasteaux5_SummitArea
    df_entrecasteaux5[i,"SummitRugosity"] <- entrecasteaux5_SummitRugosity
  }
}

df <- df_entrecasteaux5
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_entrecasteaux5 <- stack(df)
plot(raster_entrecasteaux5)

save(df_entrecasteaux5, file="02_formating_data/00_Prediction_raster/Rdata/df_entrecasteaux5.rdata")

##################################################################################################################################
entrecasteaux6=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_entrecasteaux6.shp")

buffer_entrecasteaux6 <- buffer(entrecasteaux6, width=0.1)
plot(buffer_entrecasteaux6)

raster_entrecasteaux6 <- mask(Bathy_100, buffer_entrecasteaux6)
raster_entrecasteaux6 <- trim(raster_entrecasteaux6, values=NA)
raster_entrecasteaux6[raster_entrecasteaux6 > 0] <- NA
plot(raster_entrecasteaux6)

new_extent <- extent(162.9702, 163.2342, -18.08409, -17.89369)
raster_entrecasteaux6 <- crop(raster_entrecasteaux6, new_extent)

entrecasteaux6_SummitDepth=round(max(values(raster_entrecasteaux6),na.rm=TRUE))
entrecasteaux6_ValleyDepth=round(min(values(raster_entrecasteaux6),na.rm=TRUE))
entrecasteaux6_Height=entrecasteaux6_SummitDepth-entrecasteaux6_ValleyDepth


entrecasteaux6_Summit_Poly=raster_entrecasteaux6
values(entrecasteaux6_Summit_Poly)[values(entrecasteaux6_Summit_Poly) < -60] = NA
plot(entrecasteaux6_Summit_Poly)
entrecasteaux6_SummitRugosity=sd(values(entrecasteaux6_Summit_Poly),na.rm=TRUE)
entrecasteaux6_SummitRugosity
values(entrecasteaux6_Summit_Poly)[!is.na(values(entrecasteaux6_Summit_Poly))] <- 1
plot(entrecasteaux6_Summit_Poly)
entrecasteaux6_Summit_Poly=rasterToPolygons(entrecasteaux6_Summit_Poly, dissolve=T)
entrecasteaux6_SummitArea=area(entrecasteaux6_Summit_Poly)*1e-6


raster_entrecasteaux6 <- mask(raster_entrecasteaux6, entrecasteaux6_Summit_Poly, inverse=T)
plot(raster_entrecasteaux6)

names(raster_entrecasteaux6) <- "BottomDepth"

df_entrecasteaux6 <- as.data.frame(raster_entrecasteaux6, xy=T)

for (i in 1:nrow(df_entrecasteaux6)) {
  if (!is.na(df_entrecasteaux6[i,"BottomDepth"])){
    df_entrecasteaux6[i,"Habitat"] <- 4
    df_entrecasteaux6[i,"ValleyDepth"] <- entrecasteaux6_ValleyDepth
    df_entrecasteaux6[i,"SummitDepth"] <- entrecasteaux6_SummitDepth
    df_entrecasteaux6[i,"Height"] <- entrecasteaux6_Height
    df_entrecasteaux6[i,"SummitAreaKm2"] <- entrecasteaux6_SummitArea
    df_entrecasteaux6[i,"SummitRugosity"] <- entrecasteaux6_SummitRugosity
  }
}

df <- df_entrecasteaux6
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_entrecasteaux6 <- stack(df)
plot(raster_entrecasteaux6)

save(df_entrecasteaux6, file="02_formating_data/00_Prediction_raster/Rdata/df_entrecasteaux6.rdata")

##################################################################################################################################
iledespins=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_iledespins.shp")

buffer_iledespins <- buffer(iledespins, width=0.1)
plot(buffer_iledespins)

raster_iledespins <- mask(Bathy_100, buffer_iledespins)
raster_iledespins <- trim(raster_iledespins, values=NA)
raster_iledespins[raster_iledespins > 0] <- NA
plot(raster_iledespins)

iledespins_SummitDepth=round(max(values(raster_iledespins),na.rm=TRUE))
iledespins_ValleyDepth=round(min(values(raster_iledespins),na.rm=TRUE))
iledespins_Height=iledespins_SummitDepth-iledespins_ValleyDepth


iledespins_Summit_Poly=raster_iledespins
values(iledespins_Summit_Poly)[values(iledespins_Summit_Poly) < -60] = NA
plot(iledespins_Summit_Poly)
iledespins_SummitRugosity=sd(values(iledespins_Summit_Poly),na.rm=TRUE)
iledespins_SummitRugosity
values(iledespins_Summit_Poly)[!is.na(values(iledespins_Summit_Poly))] <- 1
plot(iledespins_Summit_Poly)
iledespins_Summit_Poly=rasterToPolygons(iledespins_Summit_Poly, dissolve=T)
iledespins_SummitArea=area(iledespins_Summit_Poly)*1e-6


raster_iledespins <- mask(raster_iledespins, iledespins_Summit_Poly, inverse=T)
plot(raster_iledespins)

names(raster_iledespins) <- "BottomDepth"

df_iledespins <- as.data.frame(raster_iledespins, xy=T)

for (i in 1:nrow(df_iledespins)) {
  if (!is.na(df_iledespins[i,"BottomDepth"])){
    df_iledespins[i,"Habitat"] <- 4
    df_iledespins[i,"ValleyDepth"] <- iledespins_ValleyDepth
    df_iledespins[i,"SummitDepth"] <- iledespins_SummitDepth
    df_iledespins[i,"Height"] <- iledespins_Height
    df_iledespins[i,"SummitAreaKm2"] <- iledespins_SummitArea
    df_iledespins[i,"SummitRugosity"] <- iledespins_SummitRugosity
  }
}

df <- df_iledespins
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_iledespins <- stack(df)
plot(raster_iledespins)

save(df_iledespins, file="02_formating_data/00_Prediction_raster/Rdata/df_iledespins.rdata")

##################################################################################################################################
lifou=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_lifou.shp")

buffer_lifou <- buffer(lifou, width=0.1)
plot(buffer_lifou)

raster_lifou <- mask(Bathy_100, buffer_lifou)
raster_lifou <- trim(raster_lifou, values=NA)
raster_lifou[raster_lifou > 0] <- NA
plot(raster_lifou)

lifou_SummitDepth=round(max(values(raster_lifou),na.rm=TRUE))
lifou_ValleyDepth=round(min(values(raster_lifou),na.rm=TRUE))
lifou_Height=lifou_SummitDepth-lifou_ValleyDepth


lifou_Summit_Poly=raster_lifou
values(lifou_Summit_Poly)[values(lifou_Summit_Poly) < -60] = NA
plot(lifou_Summit_Poly)
lifou_SummitRugosity=sd(values(lifou_Summit_Poly),na.rm=TRUE)
lifou_SummitRugosity
values(lifou_Summit_Poly)[!is.na(values(lifou_Summit_Poly))] <- 1
plot(lifou_Summit_Poly)
lifou_Summit_Poly=rasterToPolygons(lifou_Summit_Poly, dissolve=T)
lifou_SummitArea=area(lifou_Summit_Poly)*1e-6


raster_lifou <- mask(raster_lifou, lifou_Summit_Poly, inverse=T)
plot(raster_lifou)

names(raster_lifou) <- "BottomDepth"

df_lifou <- as.data.frame(raster_lifou, xy=T)

for (i in 1:nrow(df_lifou)) {
  if (!is.na(df_lifou[i,"BottomDepth"])){
    df_lifou[i,"Habitat"] <- 4
    df_lifou[i,"ValleyDepth"] <- lifou_ValleyDepth
    df_lifou[i,"SummitDepth"] <- lifou_SummitDepth
    df_lifou[i,"Height"] <- lifou_Height
    df_lifou[i,"SummitAreaKm2"] <- lifou_SummitArea
    df_lifou[i,"SummitRugosity"] <- lifou_SummitRugosity
  }
}

df <- df_lifou
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_lifou <- stack(df)
plot(raster_lifou)

save(df_lifou, file="02_formating_data/00_Prediction_raster/Rdata/df_lifou.rdata")

##################################################################################################################################
mare=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_mare.shp")

buffer_mare <- buffer(mare, width=0.1)
plot(buffer_mare)

raster_mare <- mask(Bathy_100, buffer_mare)
raster_mare <- trim(raster_mare, values=NA)
raster_mare[raster_mare > 0] <- NA
plot(raster_mare)

mare_SummitDepth=round(max(values(raster_mare),na.rm=TRUE))
mare_ValleyDepth=round(min(values(raster_mare),na.rm=TRUE))
mare_Height=mare_SummitDepth-mare_ValleyDepth


mare_Summit_Poly=raster_mare
values(mare_Summit_Poly)[values(mare_Summit_Poly) < -60] = NA
plot(mare_Summit_Poly)
mare_SummitRugosity=sd(values(mare_Summit_Poly),na.rm=TRUE)
mare_SummitRugosity
values(mare_Summit_Poly)[!is.na(values(mare_Summit_Poly))] <- 1
plot(mare_Summit_Poly)
mare_Summit_Poly=rasterToPolygons(mare_Summit_Poly, dissolve=T)
mare_SummitArea=area(mare_Summit_Poly)*1e-6


raster_mare <- mask(raster_mare, mare_Summit_Poly, inverse=T)
plot(raster_mare)

names(raster_mare) <- "BottomDepth"

df_mare <- as.data.frame(raster_mare, xy=T)

for (i in 1:nrow(df_mare)) {
  if (!is.na(df_mare[i,"BottomDepth"])){
    df_mare[i,"Habitat"] <- 4
    df_mare[i,"ValleyDepth"] <- mare_ValleyDepth
    df_mare[i,"SummitDepth"] <- mare_SummitDepth
    df_mare[i,"Height"] <- mare_Height
    df_mare[i,"SummitAreaKm2"] <- mare_SummitArea
    df_mare[i,"SummitRugosity"] <- mare_SummitRugosity
  }
}

df <- df_mare
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_mare <- stack(df)
plot(raster_mare)

save(df_mare, file="02_formating_data/00_Prediction_raster/Rdata/df_mare.rdata")

##################################################################################################################################
ouvea=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_ouvea.shp")

buffer_ouvea <- buffer(ouvea, width=0.1)
plot(buffer_ouvea)

raster_ouvea <- mask(Bathy_100, buffer_ouvea)
raster_ouvea <- trim(raster_ouvea, values=NA)
raster_ouvea[raster_ouvea > 0] <- NA
plot(raster_ouvea)

ouvea_SummitDepth=round(max(values(raster_ouvea),na.rm=TRUE))
ouvea_ValleyDepth=round(min(values(raster_ouvea),na.rm=TRUE))
ouvea_Height=ouvea_SummitDepth-ouvea_ValleyDepth


ouvea_Summit_Poly=raster_ouvea
values(ouvea_Summit_Poly)[values(ouvea_Summit_Poly) < -60] = NA
plot(ouvea_Summit_Poly)
ouvea_SummitRugosity=sd(values(ouvea_Summit_Poly),na.rm=TRUE)
ouvea_SummitRugosity
values(ouvea_Summit_Poly)[!is.na(values(ouvea_Summit_Poly))] <- 1
plot(ouvea_Summit_Poly)
ouvea_Summit_Poly=rasterToPolygons(ouvea_Summit_Poly, dissolve=T)
ouvea_SummitArea=area(ouvea_Summit_Poly)*1e-6


raster_ouvea <- mask(raster_ouvea, ouvea_Summit_Poly, inverse=T)
plot(raster_ouvea)

names(raster_ouvea) <- "BottomDepth"

df_ouvea <- as.data.frame(raster_ouvea, xy=T)

for (i in 1:nrow(df_ouvea)) {
  if (!is.na(df_ouvea[i,"BottomDepth"])){
    df_ouvea[i,"Habitat"] <- 4
    df_ouvea[i,"ValleyDepth"] <- ouvea_ValleyDepth
    df_ouvea[i,"SummitDepth"] <- ouvea_SummitDepth
    df_ouvea[i,"Height"] <- ouvea_Height
    df_ouvea[i,"SummitAreaKm2"] <- ouvea_SummitArea
    df_ouvea[i,"SummitRugosity"] <- ouvea_SummitRugosity
  }
}

df <- df_ouvea
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_ouvea <- stack(df)
plot(raster_ouvea)

save(df_ouvea, file="02_formating_data/00_Prediction_raster/Rdata/df_ouvea.rdata")


##################################################################################################################################
tiga=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_tiga.shp")

buffer_tiga <- buffer(tiga, width=0.1)
plot(buffer_tiga)

raster_tiga <- mask(Bathy_100, buffer_tiga)
raster_tiga <- trim(raster_tiga, values=NA)
raster_tiga[raster_tiga > 0] <- NA
plot(raster_tiga)

tiga_SummitDepth=round(max(values(raster_tiga),na.rm=TRUE))
tiga_ValleyDepth=round(min(values(raster_tiga),na.rm=TRUE))
tiga_Height=tiga_SummitDepth-tiga_ValleyDepth


tiga_Summit_Poly=raster_tiga
values(tiga_Summit_Poly)[values(tiga_Summit_Poly) < -60] = NA
plot(tiga_Summit_Poly)
tiga_SummitRugosity=sd(values(tiga_Summit_Poly),na.rm=TRUE)
tiga_SummitRugosity
values(tiga_Summit_Poly)[!is.na(values(tiga_Summit_Poly))] <- 1
plot(tiga_Summit_Poly)
tiga_Summit_Poly=rasterToPolygons(tiga_Summit_Poly, dissolve=T)
tiga_SummitArea=area(tiga_Summit_Poly)*1e-6


raster_tiga <- mask(raster_tiga, tiga_Summit_Poly, inverse=T)
plot(raster_tiga)

names(raster_tiga) <- "BottomDepth"

df_tiga <- as.data.frame(raster_tiga, xy=T)

for (i in 1:nrow(df_tiga)) {
  if (!is.na(df_tiga[i,"BottomDepth"])){
    df_tiga[i,"Habitat"] <- 4
    df_tiga[i,"ValleyDepth"] <- tiga_ValleyDepth
    df_tiga[i,"SummitDepth"] <- tiga_SummitDepth
    df_tiga[i,"Height"] <- tiga_Height
    df_tiga[i,"SummitAreaKm2"] <- tiga_SummitArea
    df_tiga[i,"SummitRugosity"] <- tiga_SummitRugosity
  }
}

df <- df_tiga
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_tiga <- stack(df)
plot(raster_tiga$BottomDepth)

save(df_tiga, file="02_formating_data/00_Prediction_raster/Rdata/df_tiga.rdata")

##################################################################################################################################
atoll1=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_atoll1.shp")

buffer_atoll1 <- buffer(atoll1, width=0.1)
plot(buffer_atoll1)

raster_atoll1 <- mask(Bathy_100, buffer_atoll1)
raster_atoll1 <- trim(raster_atoll1, values=NA)
raster_atoll1[raster_atoll1 > 0] <- NA
plot(raster_atoll1)

atoll1_SummitDepth=round(max(values(raster_atoll1),na.rm=TRUE))
atoll1_ValleyDepth=round(min(values(raster_atoll1),na.rm=TRUE))
atoll1_Height=atoll1_SummitDepth-atoll1_ValleyDepth


atoll1_Summit_Poly=raster_atoll1
values(atoll1_Summit_Poly)[values(atoll1_Summit_Poly) < -60] = NA
plot(atoll1_Summit_Poly)
atoll1_SummitRugosity=sd(values(atoll1_Summit_Poly),na.rm=TRUE)
atoll1_SummitRugosity
values(atoll1_Summit_Poly)[!is.na(values(atoll1_Summit_Poly))] <- 1
plot(atoll1_Summit_Poly)
atoll1_Summit_Poly=rasterToPolygons(atoll1_Summit_Poly, dissolve=T)
atoll1_SummitArea=area(atoll1_Summit_Poly)*1e-6


raster_atoll1 <- mask(raster_atoll1, atoll1_Summit_Poly, inverse=T)
plot(raster_atoll1)

names(raster_atoll1) <- "BottomDepth"

df_atoll1 <- as.data.frame(raster_atoll1, xy=T)

for (i in 1:nrow(df_atoll1)) {
  if (!is.na(df_atoll1[i,"BottomDepth"])){
    df_atoll1[i,"Habitat"] <- 4
    df_atoll1[i,"ValleyDepth"] <- atoll1_ValleyDepth
    df_atoll1[i,"SummitDepth"] <- atoll1_SummitDepth
    df_atoll1[i,"Height"] <- atoll1_Height
    df_atoll1[i,"SummitAreaKm2"] <- atoll1_SummitArea
    df_atoll1[i,"SummitRugosity"] <- atoll1_SummitRugosity
  }
}

df <- df_atoll1
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_atoll1 <- stack(df)
plot(raster_atoll1)

save(df_atoll1, file="02_formating_data/00_Prediction_raster/Rdata/df_atoll1.rdata")

##################################################################################################################################
atoll2=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_atoll2.shp")

buffer_atoll2 <- buffer(atoll2, width=0.07)
plot(buffer_atoll2)

raster_atoll2 <- mask(Bathy_100, buffer_atoll2)
raster_atoll2 <- trim(raster_atoll2, values=NA)
raster_atoll2[raster_atoll2 > 0] <- NA
plot(raster_atoll2)

atoll2_SummitDepth=round(max(values(raster_atoll2),na.rm=TRUE))
atoll2_ValleyDepth=round(min(values(raster_atoll2),na.rm=TRUE))
atoll2_Height=atoll2_SummitDepth-atoll2_ValleyDepth


atoll2_Summit_Poly=raster_atoll2
values(atoll2_Summit_Poly)[values(atoll2_Summit_Poly) < -60] = NA
plot(atoll2_Summit_Poly)
atoll2_SummitRugosity=sd(values(atoll2_Summit_Poly),na.rm=TRUE)
atoll2_SummitRugosity
values(atoll2_Summit_Poly)[!is.na(values(atoll2_Summit_Poly))] <- 1
plot(atoll2_Summit_Poly)
atoll2_Summit_Poly=rasterToPolygons(atoll2_Summit_Poly, dissolve=T)
atoll2_SummitArea=area(atoll2_Summit_Poly)*1e-6


raster_atoll2 <- mask(raster_atoll2, atoll2_Summit_Poly, inverse=T)
plot(raster_atoll2)

names(raster_atoll2) <- "BottomDepth"

df_atoll2 <- as.data.frame(raster_atoll2, xy=T)

for (i in 1:nrow(df_atoll2)) {
  if (!is.na(df_atoll2[i,"BottomDepth"])){
    df_atoll2[i,"Habitat"] <- 4
    df_atoll2[i,"ValleyDepth"] <- atoll2_ValleyDepth
    df_atoll2[i,"SummitDepth"] <- atoll2_SummitDepth
    df_atoll2[i,"Height"] <- atoll2_Height
    df_atoll2[i,"SummitAreaKm2"] <- atoll2_SummitArea
    df_atoll2[i,"SummitRugosity"] <- atoll2_SummitRugosity
  }
}

df <- df_atoll2
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_atoll2 <- stack(df)
plot(raster_atoll2)

save(df_atoll2, file="02_formating_data/00_Prediction_raster/Rdata/df_atoll2.rdata")

##################################################################################################################################
atoll5=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_atoll5.shp")

buffer_atoll5 <- buffer(atoll5, width=0.1)
plot(buffer_atoll5)

raster_atoll5 <- mask(Bathy_100, buffer_atoll5)
raster_atoll5 <- trim(raster_atoll5, values=NA)
raster_atoll5[raster_atoll5 > 0] <- NA
plot(raster_atoll5)

atoll5_SummitDepth=round(max(values(raster_atoll5),na.rm=TRUE))
atoll5_ValleyDepth=round(min(values(raster_atoll5),na.rm=TRUE))
atoll5_Height=atoll5_SummitDepth-atoll5_ValleyDepth


atoll5_Summit_Poly=raster_atoll5
values(atoll5_Summit_Poly)[values(atoll5_Summit_Poly) < -60] = NA
plot(atoll5_Summit_Poly)
atoll5_SummitRugosity=sd(values(atoll5_Summit_Poly),na.rm=TRUE)
atoll5_SummitRugosity
values(atoll5_Summit_Poly)[!is.na(values(atoll5_Summit_Poly))] <- 1
plot(atoll5_Summit_Poly)
atoll5_Summit_Poly=rasterToPolygons(atoll5_Summit_Poly, dissolve=T)
atoll5_SummitArea=area(atoll5_Summit_Poly)*1e-6


raster_atoll5 <- mask(raster_atoll5, atoll5_Summit_Poly, inverse=T)
plot(raster_atoll5)

names(raster_atoll5) <- "BottomDepth"

df_atoll5 <- as.data.frame(raster_atoll5, xy=T)

for (i in 1:nrow(df_atoll5)) {
  if (!is.na(df_atoll5[i,"BottomDepth"])){
    df_atoll5[i,"Habitat"] <- 4
    df_atoll5[i,"ValleyDepth"] <- atoll5_ValleyDepth
    df_atoll5[i,"SummitDepth"] <- atoll5_SummitDepth
    df_atoll5[i,"Height"] <- atoll5_Height
    df_atoll5[i,"SummitAreaKm2"] <- atoll5_SummitArea
    df_atoll5[i,"SummitRugosity"] <- atoll5_SummitRugosity
  }
}

df <- df_atoll5
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_atoll5 <- stack(df)
plot(raster_atoll5)

save(df_atoll5, file="02_formating_data/00_Prediction_raster/Rdata/df_atoll5.rdata")



##################################################################################################################################
## merge all raster

raster_list <- list(raster_ouvea, raster_atoll1, raster_atoll2, raster_atoll5, raster_bellona, raster_chesterfield,
                    raster_chesterfield2, raster_entrecasteaux1, raster_entrecasteaux2, raster_entrecasteaux3,
                    raster_entrecasteaux4, raster_entrecasteaux5, raster_entrecasteaux6, raster_iledespins,
                    raster_lifou, raster_mare, raster_tiga)

raster_islands <- do.call(merge, raster_list)


raster_islands <- rast(raster_islands)
names(raster_islands) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")

writeRaster(raster_islands, filename = "02_formating_data/00_Prediction_raster/raster_islands.tif", overwrite=T)

df_islands <- as.data.frame(raster_islands, xy=TRUE)

save(df_islands, file="02_formating_data/00_Prediction_raster/Rdata/df_islands.rdata")


######################################################################################################################################
grandeterre=shapefile("00_metadata/environmental/NewCaledoniaMilleniumGeomorphology/Shapefiles_subparts/NC_grande_terre.shp")

buffer_grandeterre <- buffer(grandeterre, width=0.1)
plot(buffer_grandeterre)

raster_grandeterre <- mask(Bathy_100, buffer_grandeterre)
raster_grandeterre <- trim(raster_grandeterre, values=NA)
raster_grandeterre[raster_grandeterre > 0] <- NA
plot(raster_grandeterre)

grandeterre_SummitDepth=round(max(values(raster_grandeterre),na.rm=TRUE))
grandeterre_ValleyDepth=round(min(values(raster_grandeterre),na.rm=TRUE))
grandeterre_Height=grandeterre_SummitDepth-grandeterre_ValleyDepth


grandeterre_Summit_Poly=raster_grandeterre
values(grandeterre_Summit_Poly)[values(grandeterre_Summit_Poly) < -60] = NA
plot(grandeterre_Summit_Poly)
grandeterre_SummitRugosity=sd(values(grandeterre_Summit_Poly),na.rm=TRUE)
grandeterre_SummitRugosity
values(grandeterre_Summit_Poly)[!is.na(values(grandeterre_Summit_Poly))] <- 1
plot(grandeterre_Summit_Poly)
grandeterre_Summit_Poly=rasterToPolygons(grandeterre_Summit_Poly, dissolve=T)
grandeterre_SummitArea=area(grandeterre_Summit_Poly)*1e-6


raster_grandeterre <- mask(raster_grandeterre, grandeterre_Summit_Poly, inverse=T)
plot(raster_grandeterre)

names(raster_grandeterre) <- "BottomDepth"

df_grandeterre <- as.data.frame(raster_grandeterre, xy=T)

for (i in 1:nrow(df_grandeterre)) {
  if (!is.na(df_grandeterre[i,"BottomDepth"])){
    df_grandeterre[i,"Habitat"] <- 4
    df_grandeterre[i,"ValleyDepth"] <- grandeterre_ValleyDepth
    df_grandeterre[i,"SummitDepth"] <- grandeterre_SummitDepth
    df_grandeterre[i,"Height"] <- grandeterre_Height
    df_grandeterre[i,"SummitAreaKm2"] <- grandeterre_SummitArea
    df_grandeterre[i,"SummitRugosity"] <- grandeterre_SummitRugosity
  }
}

df <- df_grandeterre
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_grandeterre <- stack(df)
plot(raster_grandeterre)

raster_grandeterre <- rast(raster_grandeterre)
names(raster_grandeterre) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")

writeRaster(raster_grandeterre, filename = "02_formating_data/00_Prediction_raster/raster_grandeterre.tif")

df_grandeterre <- as.data.frame(raster_grandeterre, xy=TRUE)

save(df_grandeterre, file="02_formating_data/00_Prediction_raster/Rdata/df_grandeterre.rdata")


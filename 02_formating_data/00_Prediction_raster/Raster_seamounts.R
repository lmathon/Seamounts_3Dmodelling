# INIT : source du 00_Initialisation ----
rm(list=ls(all=TRUE))


#load libraries
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



##read bathymetrie
# NC Bathymetry at 100 m resolution (2020 - given by Jean Roger)
Bathy_100=raster("00_metadata/environmental/bathytopoMNT100m/MNT-nettoye_v3_FINAL.tif")
Bathy_100

#####################################################################################################################################
# Antigonia
Antigonia_Extent=extent(167.92,168.2,-23.54,-23.15)
Antigonia_Depth=crop(Bathy_100,Antigonia_Extent)

plot(Antigonia_Depth)
contour(Antigonia_Depth,add=TRUE)

Antigonia_SummitDepth=round(max(values(Antigonia_Depth),na.rm=TRUE))
Antigonia_ValleyDepth=round(min(values(Antigonia_Depth),na.rm=TRUE))
Antigonia_Height=Antigonia_SummitDepth-Antigonia_ValleyDepth
Antigonia_SummitDepth
Antigonia_ValleyDepth
Antigonia_Height   ###### la prof du sommet est incorrecte, la prof la plus faible mesuree pour les cameras etant 54m

values(Antigonia_Depth)[values(Antigonia_Depth) > -54] = round(runif(1000,54,67),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 54m

Antigonia_SummitDepth=round(max(values(Antigonia_Depth),na.rm=TRUE))
Antigonia_ValleyDepth=round(min(values(Antigonia_Depth),na.rm=TRUE))
Antigonia_Height=Antigonia_SummitDepth-Antigonia_ValleyDepth
Antigonia_SummitDepth
Antigonia_ValleyDepth
Antigonia_Height

plot(Antigonia_Depth)
contour(Antigonia_Depth,add=TRUE)

####SUMMIT
Antigonia_Summit_Poly=Antigonia_Depth
values(Antigonia_Summit_Poly)[values(Antigonia_Summit_Poly) < Antigonia_SummitDepth+30/100*Antigonia_SummitDepth] = NA
plot(Antigonia_Summit_Poly)
Antigonia_SummitRugosity=sd(values(Antigonia_Summit_Poly),na.rm=TRUE)
Antigonia_SummitRugosity
values(Antigonia_Summit_Poly)[!is.na(values(Antigonia_Summit_Poly))] <- 1
plot(Antigonia_Summit_Poly)
Antigonia_Summit_Poly=rasterToPolygons(Antigonia_Summit_Poly, dissolve=TRUE)
Antigonia_SummitAreaKm2=area(Antigonia_Summit_Poly)*1e-6
Antigonia_SummitAreaKm2
plot(Antigonia_Depth)
contour(Antigonia_Depth,add=TRUE)
plot(Antigonia_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Antigonia_Slope_Poly=Antigonia_Depth
values(Antigonia_Slope_Poly)[values(Antigonia_Slope_Poly) > Antigonia_SummitDepth+30/100*Antigonia_SummitDepth | values(Antigonia_Slope_Poly) < Antigonia_ValleyDepth-30/100*Antigonia_ValleyDepth] = NA
plot(Antigonia_Slope_Poly)
values(Antigonia_Slope_Poly)[!is.na(values(Antigonia_Slope_Poly))] <- 1
plot(Antigonia_Slope_Poly)
Antigonia_Slope_Poly=rasterToPolygons(Antigonia_Slope_Poly, dissolve=TRUE)
plot(Antigonia_Depth)
contour(Antigonia_Depth,add=TRUE)
plot(Antigonia_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Antigonia_Summit_Poly,border="magenta",lwd=3,add=TRUE)


buffer_antigonia <- buffer(Antigonia_Slope_Poly, width=0.05)
plot(buffer_antigonia, add=T)

antigonia_extent <- mask(Bathy_100,buffer_antigonia)
raster_antigonia <- trim(antigonia_extent, values=NA)
plot(raster_antigonia)
names(raster_antigonia) <- c("BottomDepth")
df_antigonia <- as.data.frame(raster_antigonia, xy=TRUE)

for (i in 1:nrow(df_antigonia)) {
  if (!is.na(df_antigonia[i,"BottomDepth"])){
    df_antigonia[i,"Habitat"] <- 1
    df_antigonia[i,"ValleyDepth"] <- Antigonia_ValleyDepth
    df_antigonia[i,"SummitDepth"] <- Antigonia_SummitDepth
    df_antigonia[i,"Height"] <- Antigonia_Height
    df_antigonia[i,"SummitAreaKm2"] <- Antigonia_SummitAreaKm2
    df_antigonia[i,"SummitRugosity"] <- Antigonia_SummitRugosity
  }
}

df <- df_antigonia
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_antigonia <- stack(df)
plot(raster_antigonia)


#####################################################################################################################################
## Argo

Argo_Extent=extent(159.2,160.0,-23.55,-22.96)
Argo_Depth=crop(Bathy_100,Argo_Extent)

plot(Argo_Depth)
contour(Argo_Depth,add=TRUE)

Argo_SummitDepth=round(max(values(Argo_Depth),na.rm=TRUE))
Argo_ValleyDepth=round(min(values(Argo_Depth),na.rm=TRUE))
Argo_Height=Argo_SummitDepth-Argo_ValleyDepth
Argo_SummitDepth
Argo_ValleyDepth
Argo_Height   ###### la prof du sommet est incorrecte, la campagne de terrain indiquant un mont au sommet plat a environ 310 m de fond. Nos 10 mesure edna range entre 299 et 313 m

values(Argo_Depth)[values(Argo_Depth) > -300] = round(runif(1000,299,313),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 300m

Argo_SummitDepth=round(max(values(Argo_Depth),na.rm=TRUE))
Argo_ValleyDepth=round(min(values(Argo_Depth),na.rm=TRUE))
Argo_Height=Argo_SummitDepth-Argo_ValleyDepth
Argo_SummitDepth
Argo_ValleyDepth
Argo_Height

plot(Argo_Depth)
contour(Argo_Depth,add=TRUE)

####SUMMIT
Argo_Summit_Poly=Argo_Depth
values(Argo_Summit_Poly)[values(Argo_Summit_Poly) < Argo_SummitDepth+30/100*Argo_SummitDepth] = NA
plot(Argo_Summit_Poly)
Argo_SummitRugosity=sd(values(Argo_Summit_Poly),na.rm=TRUE)
Argo_SummitRugosity
values(Argo_Summit_Poly)[!is.na(values(Argo_Summit_Poly))] <- 1
plot(Argo_Summit_Poly)
Argo_Summit_Poly=rasterToPolygons(Argo_Summit_Poly, dissolve=TRUE)
Argo_SummitAreaKm2=area(Argo_Summit_Poly)*1e-6
Argo_SummitAreaKm2
plot(Argo_Depth)
contour(Argo_Depth,add=TRUE)
plot(Argo_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Argo_Slope_Poly=Argo_Depth
values(Argo_Slope_Poly)[values(Argo_Slope_Poly) > Argo_SummitDepth+30/100*Argo_SummitDepth | values(Argo_Slope_Poly) < Argo_ValleyDepth-30/100*Argo_ValleyDepth] = NA
plot(Argo_Slope_Poly)
values(Argo_Slope_Poly)[!is.na(values(Argo_Slope_Poly))] <- 1
plot(Argo_Slope_Poly)
Argo_Slope_Poly=rasterToPolygons(Argo_Slope_Poly, dissolve=TRUE)
plot(Argo_Depth)
contour(Argo_Depth,add=TRUE)
plot(Argo_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Argo_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Argo_poly <- Argo_Slope_Poly+Argo_Summit_Poly

buffer_argo <- buffer(Argo_poly, width=0.05)
plot(buffer_argo, add=T)

argo_extent <- mask(Bathy_100, buffer_argo)
raster_argo <- trim(argo_extent, values=NA)
plot(raster_argo)
names(raster_argo) <- c("BottomDepth")
df_argo <- as.data.frame(raster_argo, xy=TRUE)

for (i in 1:nrow(df_argo)) {
  if (!is.na(df_argo[i,"BottomDepth"])){
    df_argo[i,"Habitat"] <- 2
    df_argo[i,"ValleyDepth"] <- Argo_ValleyDepth
    df_argo[i,"SummitDepth"] <- Argo_SummitDepth
    df_argo[i,"Height"] <- Argo_Height
    df_argo[i,"SummitAreaKm2"] <- Argo_SummitAreaKm2
    df_argo[i,"SummitRugosity"] <- Argo_SummitRugosity
  }
}

df <- df_argo
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_argo <- stack(df)
plot(raster_argo)


#####################################################################################################################################
## Capel

Capel_Extent=extent(159.2,160.3,-25.8,-24.5)
Capel_Depth=crop(Bathy_100,Capel_Extent)

plot(Capel_Depth)
contour(Capel_Depth,add=TRUE)

Capel_SummitDepth=round(max(values(Capel_Depth),na.rm=TRUE))
Capel_ValleyDepth=round(min(values(Capel_Depth),na.rm=TRUE))
Capel_Height=Capel_SummitDepth-Capel_ValleyDepth
Capel_SummitDepth
Capel_ValleyDepth
Capel_Height   ###### la prof du sommet est incorrecte, la campagne de terrain indiquant un mont au sommet plat a environ 60 m de fond. Nos 10 mesures edna range entre 60 et 70 m

values(Capel_Depth)[values(Capel_Depth) >= -500] = round(runif(10000,60,70),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 500m

Capel_SummitDepth=round(max(values(Capel_Depth),na.rm=TRUE))
Capel_ValleyDepth=round(min(values(Capel_Depth),na.rm=TRUE))
Capel_Height=Capel_SummitDepth-Capel_ValleyDepth
Capel_SummitDepth
Capel_ValleyDepth
Capel_Height

plot(Capel_Depth)
contour(Capel_Depth,add=TRUE)

####SUMMIT
Capel_Summit_Poly=Capel_Depth
values(Capel_Summit_Poly)[values(Capel_Summit_Poly) < Capel_SummitDepth+30/100*Capel_SummitDepth] = NA
plot(Capel_Summit_Poly)
Capel_SummitRugosity=sd(values(Capel_Summit_Poly),na.rm=TRUE)
Capel_SummitRugosity
values(Capel_Summit_Poly)[!is.na(values(Capel_Summit_Poly))] <- 1
plot(Capel_Summit_Poly)
Capel_Summit_Poly=rasterToPolygons(Capel_Summit_Poly, dissolve=TRUE)
Capel_SummitAreaKm2=area(Capel_Summit_Poly)*1e-6
Capel_SummitAreaKm2
plot(Capel_Depth)
contour(Capel_Depth,add=TRUE)
plot(Capel_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Capel_Slope_Poly=Capel_Depth
values(Capel_Slope_Poly)[values(Capel_Slope_Poly) > Capel_SummitDepth+30/100*Capel_SummitDepth | values(Capel_Slope_Poly) < Capel_ValleyDepth-30/100*Capel_ValleyDepth] = NA
plot(Capel_Slope_Poly)
values(Capel_Slope_Poly)[!is.na(values(Capel_Slope_Poly))] <- 1
plot(Capel_Slope_Poly)
Capel_Slope_Poly=rasterToPolygons(Capel_Slope_Poly, dissolve=TRUE)
plot(Capel_Depth)
contour(Capel_Depth,add=TRUE)
plot(Capel_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Capel_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Capel_poly <- Capel_Slope_Poly+Capel_Summit_Poly

buffer_capel <- buffer(Capel_poly, width=0.02)
plot(buffer_capel, add=T)

capel_extent <- mask(Bathy_100, buffer_capel)
raster_capel <- trim(capel_extent, values=NA)
plot(raster_capel)

new_extent <- extent(159.1958,160.25,-25.81849,-24.55)
raster_capel <- crop(raster_capel, new_extent)

names(raster_capel) <- c("BottomDepth")
df_capel <- as.data.frame(raster_capel, xy=TRUE)

for (i in 1:nrow(df_capel)) {
  if (!is.na(df_capel[i,"BottomDepth"])){
    df_capel[i,"Habitat"] <- 1
    df_capel[i,"ValleyDepth"] <- Capel_ValleyDepth
    df_capel[i,"SummitDepth"] <- Capel_SummitDepth
    df_capel[i,"Height"] <- Capel_Height
    df_capel[i,"SummitAreaKm2"] <- Capel_SummitAreaKm2
    df_capel[i,"SummitRugosity"] <- Capel_SummitRugosity
  }
}

df <- df_capel
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_capel <- stack(df)
plot(raster_capel)


#####################################################################################################################################
## Crythelia

Crypthelia_Extent=extent(168.2,168.31,-23.39,-23.19)
Crypthelia_Depth=crop(Bathy_100,Crypthelia_Extent)

plot(Crypthelia_Depth)
contour(Crypthelia_Depth,add=TRUE)

Crypthelia_SummitDepth=round(max(values(Crypthelia_Depth),na.rm=TRUE))
Crypthelia_ValleyDepth=round(min(values(Crypthelia_Depth),na.rm=TRUE))
Crypthelia_Height=Crypthelia_SummitDepth-Crypthelia_ValleyDepth
Crypthelia_SummitDepth
Crypthelia_ValleyDepth
Crypthelia_Height   

plot(Crypthelia_Depth)
contour(Crypthelia_Depth,add=TRUE)

####SUMMIT
Crypthelia_Summit_Poly=Crypthelia_Depth
values(Crypthelia_Summit_Poly)[values(Crypthelia_Summit_Poly) < Crypthelia_SummitDepth+30/100*Crypthelia_SummitDepth] = NA
plot(Crypthelia_Summit_Poly)
Crypthelia_SummitRugosity=sd(values(Crypthelia_Summit_Poly),na.rm=TRUE)
Crypthelia_SummitRugosity
values(Crypthelia_Summit_Poly)[!is.na(values(Crypthelia_Summit_Poly))] <- 1
plot(Crypthelia_Summit_Poly)
Crypthelia_Summit_Poly=rasterToPolygons(Crypthelia_Summit_Poly, dissolve=TRUE)
Crypthelia_SummitAreaKm2=area(Crypthelia_Summit_Poly)*1e-6
Crypthelia_SummitAreaKm2
plot(Crypthelia_Depth)
contour(Crypthelia_Depth,add=TRUE)
plot(Crypthelia_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Crypthelia_Slope_Poly=Crypthelia_Depth
values(Crypthelia_Slope_Poly)[values(Crypthelia_Slope_Poly) > Crypthelia_SummitDepth+30/100*Crypthelia_SummitDepth | values(Crypthelia_Slope_Poly) < Crypthelia_ValleyDepth-30/100*Crypthelia_ValleyDepth] = NA
plot(Crypthelia_Slope_Poly)
values(Crypthelia_Slope_Poly)[!is.na(values(Crypthelia_Slope_Poly))] <- 1
plot(Crypthelia_Slope_Poly)
Crypthelia_Slope_Poly=rasterToPolygons(Crypthelia_Slope_Poly, dissolve=TRUE)
plot(Crypthelia_Depth)
contour(Crypthelia_Depth,add=TRUE)
plot(Crypthelia_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Crypthelia_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Crypthelia_poly <- Crypthelia_Slope_Poly+Crypthelia_Summit_Poly

buffer_crypthelia <- buffer(Crypthelia_poly, width=0.04)
plot(buffer_crypthelia, add=T)

crypthelia_extent <- mask(Bathy_100, buffer_crypthelia)
raster_crypthelia <- trim(crypthelia_extent, values=NA)
plot(raster_crypthelia)
names(raster_crypthelia) <- c("BottomDepth")
df_crypthelia <- as.data.frame(raster_crypthelia, xy=TRUE)

for (i in 1:nrow(df_crypthelia)) {
  if (!is.na(df_crypthelia[i,"BottomDepth"])){
    df_crypthelia[i,"Habitat"] <- 2
    df_crypthelia[i,"ValleyDepth"] <- Crypthelia_ValleyDepth
    df_crypthelia[i,"SummitDepth"] <- Crypthelia_SummitDepth
    df_crypthelia[i,"Height"] <- Crypthelia_Height
    df_crypthelia[i,"SummitAreaKm2"] <- Crypthelia_SummitAreaKm2
    df_crypthelia[i,"SummitRugosity"] <- Crypthelia_SummitRugosity
  }
}

df <- df_crypthelia
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_crypthelia <- stack(df)
plot(raster_crypthelia)



#####################################################################################################################################
## Eponge

Eponge_Extent=extent(168.3,168.45,-25.02,-24.83)
Eponge_Depth=crop(Bathy_100,Eponge_Extent)

plot(Eponge_Depth)
contour(Eponge_Depth,add=TRUE)

Eponge_SummitDepth=round(max(values(Eponge_Depth),na.rm=TRUE))
Eponge_ValleyDepth=round(min(values(Eponge_Depth),na.rm=TRUE))
Eponge_Height=Eponge_SummitDepth-Eponge_ValleyDepth
Eponge_SummitDepth
Eponge_ValleyDepth
Eponge_Height   

plot(Eponge_Depth)
contour(Eponge_Depth,add=TRUE)

####SUMMIT
Eponge_Summit_Poly=Eponge_Depth
values(Eponge_Summit_Poly)[values(Eponge_Summit_Poly) < Eponge_SummitDepth+30/100*Eponge_SummitDepth] = NA
plot(Eponge_Summit_Poly)
Eponge_SummitRugosity=sd(values(Eponge_Summit_Poly),na.rm=TRUE)
Eponge_SummitRugosity
values(Eponge_Summit_Poly)[!is.na(values(Eponge_Summit_Poly))] <- 1
plot(Eponge_Summit_Poly)
Eponge_Summit_Poly=rasterToPolygons(Eponge_Summit_Poly, dissolve=TRUE)
Eponge_SummitAreaKm2=area(Eponge_Summit_Poly)*1e-6
Eponge_SummitAreaKm2
plot(Eponge_Depth)
contour(Eponge_Depth,add=TRUE)
plot(Eponge_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Eponge_Slope_Poly=Eponge_Depth
values(Eponge_Slope_Poly)[values(Eponge_Slope_Poly) > Eponge_SummitDepth+30/100*Eponge_SummitDepth | values(Eponge_Slope_Poly) < Eponge_ValleyDepth-30/100*Eponge_ValleyDepth] = NA
plot(Eponge_Slope_Poly)
values(Eponge_Slope_Poly)[!is.na(values(Eponge_Slope_Poly))] <- 1
plot(Eponge_Slope_Poly)
Eponge_Slope_Poly=rasterToPolygons(Eponge_Slope_Poly, dissolve=TRUE)
plot(Eponge_Depth)
contour(Eponge_Depth,add=TRUE)
plot(Eponge_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Eponge_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Eponge_poly <- Eponge_Slope_Poly+Eponge_Summit_Poly


buffer_eponge <- buffer(Eponge_poly, width=0.05)
plot(buffer_eponge, add=T)

eponge_extent <- mask(Bathy_100, buffer_eponge)
raster_eponge <- trim(eponge_extent, values=NA)
plot(raster_eponge)
names(raster_eponge) <- c("BottomDepth")
df_eponge <- as.data.frame(raster_eponge, xy=TRUE)

for (i in 1:nrow(df_eponge)) {
  if (!is.na(df_eponge[i,"BottomDepth"])){
    df_eponge[i,"Habitat"] <- 3
    df_eponge[i,"ValleyDepth"] <- Eponge_ValleyDepth
    df_eponge[i,"SummitDepth"] <- Eponge_SummitDepth
    df_eponge[i,"Height"] <- Eponge_Height
    df_eponge[i,"SummitAreaKm2"] <- Eponge_SummitAreaKm2
    df_eponge[i,"SummitRugosity"] <- Eponge_SummitRugosity
  }
}

df <- df_eponge
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_eponge <- stack(df)
plot(raster_eponge)

#####################################################################################################################################
## Fairway

Fairway_Extent=extent(162.1,162.5,-21.15,-20.9)

Fairway_Depth=crop(Bathy_100,Fairway_Extent)

plot(Fairway_Depth)
contour(Fairway_Depth,add=TRUE)

Fairway_SummitDepth=round(max(values(Fairway_Depth),na.rm=TRUE))
Fairway_ValleyDepth=round(min(values(Fairway_Depth),na.rm=TRUE))
Fairway_Height=Fairway_SummitDepth-Fairway_ValleyDepth
Fairway_SummitDepth
Fairway_ValleyDepth
Fairway_Height   ###### la prof du sommet est incorrecte, la campagne de terrain indiquant un mont au sommet plat a environ 60 m de fond. Nos 10 mesure edna range entre 62 et 67 m


values(Fairway_Depth)[values(Fairway_Depth) > -260] = round(runif(10000,62,67),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 260m pour avoir une structure sommet homogene

Fairway_SummitDepth=round(max(values(Fairway_Depth),na.rm=TRUE))
Fairway_ValleyDepth=round(min(values(Fairway_Depth),na.rm=TRUE))
Fairway_Height=Fairway_SummitDepth-Fairway_ValleyDepth
Fairway_SummitDepth
Fairway_ValleyDepth
Fairway_Height

plot(Fairway_Depth)
contour(Fairway_Depth,add=TRUE)

####SUMMIT
Fairway_Summit_Poly=Fairway_Depth
values(Fairway_Summit_Poly)[values(Fairway_Summit_Poly) < Fairway_SummitDepth+30/100*Fairway_SummitDepth] = NA
plot(Fairway_Summit_Poly)
Fairway_SummitRugosity=sd(values(Fairway_Summit_Poly),na.rm=TRUE)
Fairway_SummitRugosity
values(Fairway_Summit_Poly)[!is.na(values(Fairway_Summit_Poly))] <- 1
plot(Fairway_Summit_Poly)
Fairway_Summit_Poly=rasterToPolygons(Fairway_Summit_Poly, dissolve=TRUE)
Fairway_SummitAreaKm2=area(Fairway_Summit_Poly)*1e-6
Fairway_SummitAreaKm2
plot(Fairway_Depth)
contour(Fairway_Depth,add=TRUE)
plot(Fairway_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Fairway_Slope_Poly=Fairway_Depth
values(Fairway_Slope_Poly)[values(Fairway_Slope_Poly) > Fairway_SummitDepth+30/100*Fairway_SummitDepth | values(Fairway_Slope_Poly) < Fairway_ValleyDepth-30/100*Fairway_ValleyDepth] = NA
plot(Fairway_Slope_Poly)
values(Fairway_Slope_Poly)[!is.na(values(Fairway_Slope_Poly))] <- 1
plot(Fairway_Slope_Poly)
Fairway_Slope_Poly=rasterToPolygons(Fairway_Slope_Poly, dissolve=TRUE)
plot(Fairway_Depth)
contour(Fairway_Depth,add=TRUE)
plot(Fairway_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Fairway_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Fairway_poly <- Fairway_Slope_Poly+Fairway_Summit_Poly

buffer_fairway <- buffer(Fairway_poly, width=0.02)
plot(buffer_fairway, add=T)

fairway_extent <- mask(Bathy_100, buffer_fairway)
raster_fairway <- trim(fairway_extent, values=NA)
plot(raster_fairway)
names(raster_fairway) <- c("BottomDepth")
df_fairway <- as.data.frame(raster_fairway, xy=TRUE)

for (i in 1:nrow(df_fairway)) {
  if (!is.na(df_fairway[i,"BottomDepth"])){
    df_fairway[i,"Habitat"] <- 1
    df_fairway[i,"ValleyDepth"] <- Fairway_ValleyDepth
    df_fairway[i,"SummitDepth"] <- Fairway_SummitDepth
    df_fairway[i,"Height"] <- Fairway_Height
    df_fairway[i,"SummitAreaKm2"] <- Fairway_SummitAreaKm2
    df_fairway[i,"SummitRugosity"] <- Fairway_SummitRugosity
  }
}

df <- df_fairway
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_fairway <- stack(df)
plot(raster_fairway)

#####################################################################################################################################
## Ile des Pins

IleDesPins_Extent=extent(167.37,167.45,-22.41,-22.35)
IleDesPins_Depth=crop(Bathy_100,IleDesPins_Extent)

plot(IleDesPins_Depth)
contour(IleDesPins_Depth,add=TRUE)

IleDesPins_SummitDepth=round(max(values(IleDesPins_Depth),na.rm=TRUE))
IleDesPins_ValleyDepth=round(min(values(IleDesPins_Depth),na.rm=TRUE))
IleDesPins_Height=IleDesPins_SummitDepth-IleDesPins_ValleyDepth
IleDesPins_SummitDepth
IleDesPins_ValleyDepth
IleDesPins_Height   

plot(IleDesPins_Depth)
contour(IleDesPins_Depth,add=TRUE)

####SUMMIT
IleDesPins_Summit_Poly=IleDesPins_Depth
values(IleDesPins_Summit_Poly)[values(IleDesPins_Summit_Poly) < IleDesPins_SummitDepth+30/100*IleDesPins_SummitDepth] = NA
plot(IleDesPins_Summit_Poly)
IleDesPins_SummitRugosity=sd(values(IleDesPins_Summit_Poly),na.rm=TRUE)
IleDesPins_SummitRugosity
values(IleDesPins_Summit_Poly)[!is.na(values(IleDesPins_Summit_Poly))] <- 1
plot(IleDesPins_Summit_Poly)
IleDesPins_Summit_Poly=rasterToPolygons(IleDesPins_Summit_Poly, dissolve=TRUE)
IleDesPins_SummitAreaKm2=area(IleDesPins_Summit_Poly)*1e-6
IleDesPins_SummitAreaKm2
plot(IleDesPins_Depth)
contour(IleDesPins_Depth,add=TRUE)
plot(IleDesPins_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
IleDesPins_Slope_Poly=IleDesPins_Depth
values(IleDesPins_Slope_Poly)[values(IleDesPins_Slope_Poly) > IleDesPins_SummitDepth+30/100*IleDesPins_SummitDepth | values(IleDesPins_Slope_Poly) < IleDesPins_ValleyDepth-30/100*IleDesPins_ValleyDepth] = NA
plot(IleDesPins_Slope_Poly)
values(IleDesPins_Slope_Poly)[!is.na(values(IleDesPins_Slope_Poly))] <- 1
plot(IleDesPins_Slope_Poly)
IleDesPins_Slope_Poly=rasterToPolygons(IleDesPins_Slope_Poly, dissolve=TRUE)
plot(IleDesPins_Depth)
contour(IleDesPins_Depth,add=TRUE)
plot(IleDesPins_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(IleDesPins_Summit_Poly,border="magenta",lwd=3,add=TRUE)
IleDesPins_poly <- IleDesPins_Slope_Poly+IleDesPins_Summit_Poly

buffer_iledespins <- buffer(IleDesPins_poly, width=0.02)
plot(buffer_iledespins, add=T)

iledespins_extent <- mask(Bathy_100, buffer_iledespins)
raster_iledespins <- trim(iledespins_extent, values=NA)
plot(raster_iledespins)
names(raster_iledespins) <- c("BottomDepth")
df_iledespins <- as.data.frame(raster_iledespins, xy=TRUE)

for (i in 1:nrow(df_iledespins)) {
  if (!is.na(df_iledespins[i,"BottomDepth"])){
    df_iledespins[i,"Habitat"] <- 3
    df_iledespins[i,"ValleyDepth"] <- IleDesPins_ValleyDepth
    df_iledespins[i,"SummitDepth"] <- IleDesPins_SummitDepth
    df_iledespins[i,"Height"] <- IleDesPins_Height
    df_iledespins[i,"SummitAreaKm2"] <- IleDesPins_SummitAreaKm2
    df_iledespins[i,"SummitRugosity"] <- IleDesPins_SummitRugosity
  }
}

df <- df_iledespins
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_iledespins <- stack(df)
plot(raster_iledespins)

#####################################################################################################################################
## Jumeau Ouest

JumeauOuest_Extent=extent(167.94,168.08,-23.77,-23.61)
JumeauOuest_Depth=crop(Bathy_100,JumeauOuest_Extent)

plot(JumeauOuest_Depth)
contour(JumeauOuest_Depth,add=TRUE)

JumeauOuest_SummitDepth=round(max(values(JumeauOuest_Depth),na.rm=TRUE))
JumeauOuest_ValleyDepth=round(min(values(JumeauOuest_Depth),na.rm=TRUE))
JumeauOuest_Height=JumeauOuest_SummitDepth-JumeauOuest_ValleyDepth
JumeauOuest_SummitDepth
JumeauOuest_ValleyDepth
JumeauOuest_Height   

plot(JumeauOuest_Depth)
contour(JumeauOuest_Depth,add=TRUE)

####SUMMIT
JumeauOuest_Summit_Poly=JumeauOuest_Depth
values(JumeauOuest_Summit_Poly)[values(JumeauOuest_Summit_Poly) < JumeauOuest_SummitDepth+30/100*JumeauOuest_SummitDepth] = NA
plot(JumeauOuest_Summit_Poly)
JumeauOuest_SummitRugosity=sd(values(JumeauOuest_Summit_Poly),na.rm=TRUE)
JumeauOuest_SummitRugosity
values(JumeauOuest_Summit_Poly)[!is.na(values(JumeauOuest_Summit_Poly))] <- 1
plot(JumeauOuest_Summit_Poly)
JumeauOuest_Summit_Poly=rasterToPolygons(JumeauOuest_Summit_Poly, dissolve=TRUE)
JumeauOuest_SummitAreaKm2=area(JumeauOuest_Summit_Poly)*1e-6
JumeauOuest_SummitAreaKm2
plot(JumeauOuest_Depth)
contour(JumeauOuest_Depth,add=TRUE)
plot(JumeauOuest_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
JumeauOuest_Slope_Poly=JumeauOuest_Depth
values(JumeauOuest_Slope_Poly)[values(JumeauOuest_Slope_Poly) > JumeauOuest_SummitDepth+30/100*JumeauOuest_SummitDepth | values(JumeauOuest_Slope_Poly) < JumeauOuest_ValleyDepth-30/100*JumeauOuest_ValleyDepth] = NA
plot(JumeauOuest_Slope_Poly)
values(JumeauOuest_Slope_Poly)[!is.na(values(JumeauOuest_Slope_Poly))] <- 1
plot(JumeauOuest_Slope_Poly)
JumeauOuest_Slope_Poly=rasterToPolygons(JumeauOuest_Slope_Poly, dissolve=TRUE)
plot(JumeauOuest_Depth)
contour(JumeauOuest_Depth,add=TRUE)
plot(JumeauOuest_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(JumeauOuest_Summit_Poly,border="magenta",lwd=3,add=TRUE)
JumeauOuest_poly <- JumeauOuest_Summit_Poly+JumeauOuest_Slope_Poly

buffer_jumeauouest <- buffer(JumeauOuest_poly, width=0.05)
plot(buffer_jumeauouest, add=T)

jumeauouest_extent <- mask(Bathy_100, buffer_jumeauouest)
raster_jumeauouest <- trim(jumeauouest_extent, values=NA)
plot(raster_jumeauouest)
names(raster_jumeauouest) <- c("BottomDepth")
df_jumeauouest <- as.data.frame(raster_jumeauouest, xy=TRUE)

for (i in 1:nrow(df_jumeauouest)) {
  if (!is.na(df_jumeauouest[i,"BottomDepth"])){
    df_jumeauouest[i,"Habitat"] <- 2
    df_jumeauouest[i,"ValleyDepth"] <- JumeauOuest_ValleyDepth
    df_jumeauouest[i,"SummitDepth"] <- JumeauOuest_SummitDepth
    df_jumeauouest[i,"Height"] <- JumeauOuest_Height
    df_jumeauouest[i,"SummitAreaKm2"] <- JumeauOuest_SummitAreaKm2
    df_jumeauouest[i,"SummitRugosity"] <- JumeauOuest_SummitRugosity
  }
}

df <- df_jumeauouest
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_jumeauouest <- stack(df)
plot(raster_jumeauouest)


#####################################################################################################################################
## Kaimon Maru

KaimonMaru_Extent=extent(168.02,168.25,-24.88,-24.63)
KaimonMaru_Depth=crop(Bathy_100,KaimonMaru_Extent)

plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)

KaimonMaru_SummitDepth=round(max(values(KaimonMaru_Depth),na.rm=TRUE))
KaimonMaru_ValleyDepth=round(min(values(KaimonMaru_Depth),na.rm=TRUE))
KaimonMaru_Height=KaimonMaru_SummitDepth-KaimonMaru_ValleyDepth
KaimonMaru_SummitDepth
KaimonMaru_ValleyDepth
KaimonMaru_Height   

plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)

####SUMMIT
KaimonMaru_Summit_Poly=KaimonMaru_Depth
values(KaimonMaru_Summit_Poly)[values(KaimonMaru_Summit_Poly) < KaimonMaru_SummitDepth+30/100*KaimonMaru_SummitDepth] = NA
plot(KaimonMaru_Summit_Poly)
KaimonMaru_SummitRugosity=sd(values(KaimonMaru_Summit_Poly),na.rm=TRUE)
KaimonMaru_SummitRugosity
values(KaimonMaru_Summit_Poly)[!is.na(values(KaimonMaru_Summit_Poly))] <- 1
plot(KaimonMaru_Summit_Poly)
KaimonMaru_Summit_Poly=rasterToPolygons(KaimonMaru_Summit_Poly, dissolve=TRUE)
KaimonMaru_SummitAreaKm2=area(KaimonMaru_Summit_Poly)*1e-6
KaimonMaru_SummitAreaKm2
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
KaimonMaru_Slope_Poly=KaimonMaru_Depth
values(KaimonMaru_Slope_Poly)[values(KaimonMaru_Slope_Poly) > KaimonMaru_SummitDepth+30/100*KaimonMaru_SummitDepth | values(KaimonMaru_Slope_Poly) < KaimonMaru_ValleyDepth-30/100*KaimonMaru_ValleyDepth] = NA
plot(KaimonMaru_Slope_Poly)
values(KaimonMaru_Slope_Poly)[!is.na(values(KaimonMaru_Slope_Poly))] <- 1
plot(KaimonMaru_Slope_Poly)
KaimonMaru_Slope_Poly=rasterToPolygons(KaimonMaru_Slope_Poly, dissolve=TRUE)
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
KaimonMaru_poly <- KaimonMaru_Slope_Poly+KaimonMaru_Summit_Poly

buffer_kaimonmaru <- buffer(KaimonMaru_poly, width=0.05)
plot(buffer_kaimonmaru, add=T)

kaimonmaru_extent <- mask(Bathy_100, buffer_kaimonmaru)
raster_kaimonmaru <- trim(kaimonmaru_extent, values=NA)
plot(raster_kaimonmaru)
names(raster_kaimonmaru) <- c("BottomDepth")
df_kaimonmaru <- as.data.frame(raster_kaimonmaru, xy=TRUE)

for (i in 1:nrow(df_kaimonmaru)) {
  if (!is.na(df_kaimonmaru[i,"BottomDepth"])){
    df_kaimonmaru[i,"Habitat"] <- 2
    df_kaimonmaru[i,"ValleyDepth"] <- KaimonMaru_ValleyDepth
    df_kaimonmaru[i,"SummitDepth"] <- KaimonMaru_SummitDepth
    df_kaimonmaru[i,"Height"] <- KaimonMaru_Height
    df_kaimonmaru[i,"SummitAreaKm2"] <- KaimonMaru_SummitAreaKm2
    df_kaimonmaru[i,"SummitRugosity"] <- KaimonMaru_SummitRugosity
  }
}

df <- df_kaimonmaru
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_kaimonmaru <- stack(df)
plot(raster_kaimonmaru)

#####################################################################################################################################
## Nova

Nova_Extent=extent(158.9,159.62,-23.0,-22.05)
Nova_Depth=crop(Bathy_100,Nova_Extent)

plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)

Nova_SummitDepth=round(max(values(Nova_Depth),na.rm=TRUE))
Nova_ValleyDepth=round(min(values(Nova_Depth),na.rm=TRUE))
Nova_Height=Nova_SummitDepth-Nova_ValleyDepth
Nova_SummitDepth
Nova_ValleyDepth
Nova_Height   

plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)

####SUMMIT
Nova_Summit_Poly=Nova_Depth
values(Nova_Summit_Poly)[values(Nova_Summit_Poly) < Nova_SummitDepth+30/100*Nova_SummitDepth] = NA
plot(Nova_Summit_Poly)
Nova_SummitRugosity=sd(values(Nova_Summit_Poly),na.rm=TRUE)
Nova_SummitRugosity
values(Nova_Summit_Poly)[!is.na(values(Nova_Summit_Poly))] <- 1
plot(Nova_Summit_Poly)
Nova_Summit_Poly=rasterToPolygons(Nova_Summit_Poly, dissolve=TRUE)
Nova_SummitAreaKm2=area(Nova_Summit_Poly)*1e-6
Nova_SummitAreaKm2
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Nova_Slope_Poly=Nova_Depth
values(Nova_Slope_Poly)[values(Nova_Slope_Poly) > Nova_SummitDepth+30/100*Nova_SummitDepth | values(Nova_Slope_Poly) < Nova_ValleyDepth-30/100*Nova_ValleyDepth] = NA
plot(Nova_Slope_Poly)
values(Nova_Slope_Poly)[!is.na(values(Nova_Slope_Poly))] <- 1
plot(Nova_Slope_Poly)
Nova_Slope_Poly=rasterToPolygons(Nova_Slope_Poly, dissolve=TRUE)
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Nova_poly <- Nova_Slope_Poly+Nova_Summit_Poly

buffer_nova <- buffer(Nova_poly, width=0.05)
plot(buffer_nova, add=T)

nova_extent <- mask(Bathy_100, buffer_nova)
raster_nova <- trim(nova_extent, values=NA)
plot(raster_nova)
names(raster_nova) <- c("BottomDepth")
df_nova <- as.data.frame(raster_nova, xy=TRUE)

for (i in 1:nrow(df_nova)) {
  if (!is.na(df_nova[i,"BottomDepth"])){
    df_nova[i,"Habitat"] <- 2
    df_nova[i,"ValleyDepth"] <- Nova_ValleyDepth
    df_nova[i,"SummitDepth"] <- Nova_SummitDepth
    df_nova[i,"Height"] <- Nova_Height
    df_nova[i,"SummitAreaKm2"] <- Nova_SummitAreaKm2
    df_nova[i,"SummitRugosity"] <- Nova_SummitRugosity
  }
}

df <- df_nova
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_nova <- stack(df)
plot(raster_nova)


#####################################################################################################################################
## Stylaster

Stylaster_Extent=extent(167.55,167.84,-23.76,-23.54)
Stylaster_Depth=crop(Bathy_100,Stylaster_Extent)

plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)

Stylaster_SummitDepth=round(max(values(Stylaster_Depth),na.rm=TRUE))
Stylaster_ValleyDepth=round(min(values(Stylaster_Depth),na.rm=TRUE))
Stylaster_Height=Stylaster_SummitDepth-Stylaster_ValleyDepth
Stylaster_SummitDepth
Stylaster_ValleyDepth
Stylaster_Height   

plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)

####SUMMIT
Stylaster_Summit_Poly=Stylaster_Depth
values(Stylaster_Summit_Poly)[values(Stylaster_Summit_Poly) < Stylaster_SummitDepth+30/100*Stylaster_SummitDepth] = NA
plot(Stylaster_Summit_Poly)
Stylaster_SummitRugosity=sd(values(Stylaster_Summit_Poly),na.rm=TRUE)
Stylaster_SummitRugosity
values(Stylaster_Summit_Poly)[!is.na(values(Stylaster_Summit_Poly))] <- 1
plot(Stylaster_Summit_Poly)
Stylaster_Summit_Poly=rasterToPolygons(Stylaster_Summit_Poly, dissolve=TRUE)
Stylaster_SummitAreaKm2=area(Stylaster_Summit_Poly)*1e-6
Stylaster_SummitAreaKm2
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Stylaster_Slope_Poly=Stylaster_Depth
values(Stylaster_Slope_Poly)[values(Stylaster_Slope_Poly) > Stylaster_SummitDepth+30/100*Stylaster_SummitDepth | values(Stylaster_Slope_Poly) < Stylaster_ValleyDepth-30/100*Stylaster_ValleyDepth] = NA
plot(Stylaster_Slope_Poly)
values(Stylaster_Slope_Poly)[!is.na(values(Stylaster_Slope_Poly))] <- 1
plot(Stylaster_Slope_Poly)
Stylaster_Slope_Poly=rasterToPolygons(Stylaster_Slope_Poly, dissolve=TRUE)
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Stylaster_poly <- Stylaster_Slope_Poly+Stylaster_Summit_Poly

buffer_stylaster <- buffer(Stylaster_poly, width=0.05)
plot(buffer_stylaster, add=T)

stylaster_extent <- mask(Bathy_100, buffer_stylaster)
raster_stylaster <- trim(stylaster_extent, values=NA)
plot(raster_stylaster)
names(raster_stylaster) <- c("BottomDepth")
df_stylaster <- as.data.frame(raster_stylaster, xy=TRUE)

for (i in 1:nrow(df_stylaster)) {
  if (!is.na(df_stylaster[i,"BottomDepth"])){
    df_stylaster[i,"Habitat"] <- 3
    df_stylaster[i,"ValleyDepth"] <- Stylaster_ValleyDepth
    df_stylaster[i,"SummitDepth"] <- Stylaster_SummitDepth
    df_stylaster[i,"Height"] <- Stylaster_Height
    df_stylaster[i,"SummitAreaKm2"] <- Stylaster_SummitAreaKm2
    df_stylaster[i,"SummitRugosity"] <- Stylaster_SummitRugosity
  }
}

df <- df_stylaster
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_stylaster <- stack(df)
plot(raster_stylaster)

#####################################################################################################################################
## Torche

Torche_Extent=extent(167.57,167.78,-22.94,-22.81)
Torche_Depth=crop(Bathy_100,Torche_Extent)

plot(Torche_Depth)
contour(Torche_Depth,add=TRUE)

Torche_SummitDepth=round(max(values(Torche_Depth),na.rm=TRUE))
Torche_ValleyDepth=round(min(values(Torche_Depth),na.rm=TRUE))
Torche_Height=Torche_SummitDepth-Torche_ValleyDepth
Torche_SummitDepth
Torche_ValleyDepth
Torche_Height   ###### la prof du sommet est incorrecte, la campagne de terrain indiquant un mont au sommet plat a environ 45 m de fond. Nos 10 mesure edna range entre 45 et 58 m


values(Torche_Depth)[values(Torche_Depth) > -45] = round(runif(1000,45,58),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 300m

Torche_SummitDepth=round(max(values(Torche_Depth),na.rm=TRUE))
Torche_ValleyDepth=round(min(values(Torche_Depth),na.rm=TRUE))
Torche_Height=Torche_SummitDepth-Torche_ValleyDepth
Torche_SummitDepth
Torche_ValleyDepth
Torche_Height

plot(Torche_Depth)
contour(Torche_Depth,add=TRUE)

####SUMMIT
Torche_Summit_Poly=Torche_Depth
values(Torche_Summit_Poly)[values(Torche_Summit_Poly) < Torche_SummitDepth+30/100*Torche_SummitDepth] = NA
plot(Torche_Summit_Poly)
Torche_SummitRugosity=sd(values(Torche_Summit_Poly),na.rm=TRUE)
Torche_SummitRugosity
values(Torche_Summit_Poly)[!is.na(values(Torche_Summit_Poly))] <- 1
plot(Torche_Summit_Poly)
Torche_Summit_Poly=rasterToPolygons(Torche_Summit_Poly, dissolve=TRUE)
Torche_SummitAreaKm2=area(Torche_Summit_Poly)*1e-6
Torche_SummitAreaKm2
plot(Torche_Depth)
contour(Torche_Depth,add=TRUE)
plot(Torche_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Torche_Slope_Poly=Torche_Depth
values(Torche_Slope_Poly)[values(Torche_Slope_Poly) > Torche_SummitDepth+30/100*Torche_SummitDepth | values(Torche_Slope_Poly) < Torche_ValleyDepth-30/100*Torche_ValleyDepth] = NA
plot(Torche_Slope_Poly)
values(Torche_Slope_Poly)[!is.na(values(Torche_Slope_Poly))] <- 1
plot(Torche_Slope_Poly)
Torche_Slope_Poly=rasterToPolygons(Torche_Slope_Poly, dissolve=TRUE)
plot(Torche_Depth)
contour(Torche_Depth,add=TRUE)
plot(Torche_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Torche_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Torche_poly <- Torche_Slope_Poly+Torche_Summit_Poly

buffer_torche <- buffer(Torche_poly, width=0.05)
plot(buffer_torche, add=T)

torche_extent <- mask(Bathy_100, buffer_torche)
raster_torche <- trim(torche_extent, values=NA)
plot(raster_torche)
names(raster_torche) <- c("BottomDepth")
df_torche <- as.data.frame(raster_torche, xy=TRUE)

for (i in 1:nrow(df_torche)) {
  if (!is.na(df_torche[i,"BottomDepth"])){
    df_torche[i,"Habitat"] <- 1
    df_torche[i,"ValleyDepth"] <- Torche_ValleyDepth
    df_torche[i,"SummitDepth"] <- Torche_SummitDepth
    df_torche[i,"Height"] <- Torche_Height
    df_torche[i,"SummitAreaKm2"] <- Torche_SummitAreaKm2
    df_torche[i,"SummitRugosity"] <- Torche_SummitRugosity
  }
}

df <- df_torche
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_torche <- stack(df)
plot(raster_torche)

#####################################################################################################################################
## Chesterfield

Chesterfield_Extent=extent(158.7,159.1,-20.15,-19.76)
Chesterfield_Depth=crop(Bathy_100,Chesterfield_Extent)

plot(Chesterfield_Depth)
contour(Chesterfield_Depth,add=TRUE)

Chesterfield_SummitDepth=round(max(values(Chesterfield_Depth),na.rm=TRUE))
Chesterfield_ValleyDepth=round(min(values(Chesterfield_Depth),na.rm=TRUE))
Chesterfield_Height=Chesterfield_SummitDepth-Chesterfield_ValleyDepth
Chesterfield_SummitDepth
Chesterfield_ValleyDepth
Chesterfield_Height   ###### la prof du sommet est incorrecte,

values(Chesterfield_Depth)[values(Chesterfield_Depth) > -40] = round(runif(1000,40,50),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 40m

Chesterfield_SummitDepth=round(max(values(Chesterfield_Depth),na.rm=TRUE))
Chesterfield_ValleyDepth=round(min(values(Chesterfield_Depth),na.rm=TRUE))
Chesterfield_Height=Chesterfield_SummitDepth-Chesterfield_ValleyDepth
Chesterfield_SummitDepth
Chesterfield_ValleyDepth
Chesterfield_Height

plot(Chesterfield_Depth)
contour(Chesterfield_Depth,add=TRUE)

####SUMMIT
Chesterfield_Summit_Poly=Chesterfield_Depth
values(Chesterfield_Summit_Poly)[values(Chesterfield_Summit_Poly) < Chesterfield_SummitDepth+30/100*Chesterfield_SummitDepth] = NA
plot(Chesterfield_Summit_Poly)
Chesterfield_SummitRugosity=sd(values(Chesterfield_Summit_Poly),na.rm=TRUE)
Chesterfield_SummitRugosity
values(Chesterfield_Summit_Poly)[!is.na(values(Chesterfield_Summit_Poly))] <- 1
plot(Chesterfield_Summit_Poly)
Chesterfield_Summit_Poly=rasterToPolygons(Chesterfield_Summit_Poly, dissolve=TRUE)
Chesterfield_SummitAreaKm2=area(Chesterfield_Summit_Poly)*1e-6
Chesterfield_SummitAreaKm2
plot(Chesterfield_Depth)
contour(Chesterfield_Depth,add=TRUE)
plot(Chesterfield_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Chesterfield_Slope_Poly=Chesterfield_Depth
values(Chesterfield_Slope_Poly)[values(Chesterfield_Slope_Poly) > Chesterfield_SummitDepth+30/100*Chesterfield_SummitDepth | values(Chesterfield_Slope_Poly) < Chesterfield_ValleyDepth-30/100*Chesterfield_ValleyDepth] = NA
plot(Chesterfield_Slope_Poly)
values(Chesterfield_Slope_Poly)[!is.na(values(Chesterfield_Slope_Poly))] <- 1
plot(Chesterfield_Slope_Poly)
Chesterfield_Slope_Poly=rasterToPolygons(Chesterfield_Slope_Poly, dissolve=TRUE)
plot(Chesterfield_Depth)
contour(Chesterfield_Depth,add=TRUE)
plot(Chesterfield_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Chesterfield_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Chesterfield_poly <- Chesterfield_Slope_Poly+Chesterfield_Summit_Poly

buffer_Chesterfield <- buffer(Chesterfield_poly, width=0.05)
plot(buffer_Chesterfield, add=T)

Chesterfield_extent <- mask(Bathy_100, buffer_Chesterfield)
raster_Chesterfield <- trim(Chesterfield_extent, values=NA)
plot(raster_Chesterfield)
names(raster_Chesterfield) <- c("BottomDepth")
df_chesterfield <- as.data.frame(raster_Chesterfield, xy=TRUE)

for (i in 1:nrow(df_chesterfield)) {
  if (!is.na(df_chesterfield[i,"BottomDepth"])){
    df_chesterfield[i,"Habitat"] <- 1
    df_chesterfield[i,"ValleyDepth"] <- Chesterfield_ValleyDepth
    df_chesterfield[i,"SummitDepth"] <- Chesterfield_SummitDepth
    df_chesterfield[i,"Height"] <- Chesterfield_Height
    df_chesterfield[i,"SummitAreaKm2"] <- Chesterfield_SummitAreaKm2
    df_chesterfield[i,"SummitRugosity"] <- Chesterfield_SummitRugosity
  }
}

df <- df_chesterfield
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_Chesterfield <- stack(df)
plot(raster_Chesterfield)

#####################################################################################################################################
## Jumeau Est

JumeauEst_Extent=extent(168.21,168.33,-23.86,-23.65)
JumeauEst_Depth=crop(Bathy_100,JumeauEst_Extent)

plot(JumeauEst_Depth)
contour(JumeauEst_Depth,add=TRUE)

JumeauEst_SummitDepth=round(max(values(JumeauEst_Depth),na.rm=TRUE))
JumeauEst_ValleyDepth=round(min(values(JumeauEst_Depth),na.rm=TRUE))
JumeauEst_Height=JumeauEst_SummitDepth-JumeauEst_ValleyDepth
JumeauEst_SummitDepth
JumeauEst_ValleyDepth
JumeauEst_Height   

plot(JumeauEst_Depth)
contour(JumeauEst_Depth,add=TRUE)

####SUMMIT
JumeauEst_Summit_Poly=JumeauEst_Depth
values(JumeauEst_Summit_Poly)[values(JumeauEst_Summit_Poly) < JumeauEst_SummitDepth+30/100*JumeauEst_SummitDepth] = NA
plot(JumeauEst_Summit_Poly)
JumeauEst_SummitRugosity=sd(values(JumeauEst_Summit_Poly),na.rm=TRUE)
JumeauEst_SummitRugosity
values(JumeauEst_Summit_Poly)[!is.na(values(JumeauEst_Summit_Poly))] <- 1
plot(JumeauEst_Summit_Poly)
JumeauEst_Summit_Poly=rasterToPolygons(JumeauEst_Summit_Poly, dissolve=TRUE)
JumeauEst_SummitAreaKm2=area(JumeauEst_Summit_Poly)*1e-6
JumeauEst_SummitAreaKm2
plot(JumeauEst_Depth)
contour(JumeauEst_Depth,add=TRUE)
plot(JumeauEst_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
JumeauEst_Slope_Poly=JumeauEst_Depth
values(JumeauEst_Slope_Poly)[values(JumeauEst_Slope_Poly) > JumeauEst_SummitDepth+30/100*JumeauEst_SummitDepth | values(JumeauEst_Slope_Poly) < JumeauEst_ValleyDepth-30/100*JumeauEst_ValleyDepth] = NA
plot(JumeauEst_Slope_Poly)
values(JumeauEst_Slope_Poly)[!is.na(values(JumeauEst_Slope_Poly))] <- 1
plot(JumeauEst_Slope_Poly)
JumeauEst_Slope_Poly=rasterToPolygons(JumeauEst_Slope_Poly, dissolve=TRUE)
plot(JumeauEst_Depth)
contour(JumeauEst_Depth,add=TRUE)
plot(JumeauEst_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(JumeauEst_Summit_Poly,border="magenta",lwd=3,add=TRUE)
JumeauEst_poly <- JumeauEst_Slope_Poly+JumeauEst_Summit_Poly


buffer_JumeauEst <- buffer(JumeauEst_poly, width=0.05)
plot(buffer_JumeauEst, add=T)

JumeauEst_extent <- mask(Bathy_100, buffer_JumeauEst)
raster_JumeauEst <- trim(JumeauEst_extent, values=NA)
plot(raster_JumeauEst)
names(raster_JumeauEst) <- c("BottomDepth")
df_jumeauest <- as.data.frame(raster_JumeauEst, xy=TRUE)

for (i in 1:nrow(df_jumeauest)) {
  if (!is.na(df_jumeauest[i,"BottomDepth"])){
    df_jumeauest[i,"Habitat"] <- 3
    df_jumeauest[i,"ValleyDepth"] <- JumeauEst_ValleyDepth
    df_jumeauest[i,"SummitDepth"] <- JumeauEst_SummitDepth
    df_jumeauest[i,"Height"] <- JumeauEst_Height
    df_jumeauest[i,"SummitAreaKm2"] <- JumeauEst_SummitAreaKm2
    df_jumeauest[i,"SummitRugosity"] <- JumeauEst_SummitRugosity
  }
}

df <- df_jumeauest
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_JumeauEst <- stack(df)
plot(raster_JumeauEst)

#####################################################################################################################################
## seamount01

Seamount01_Extent=extent(168.5,168.8,-22.15,-21.85)
Seamount01_Depth=crop(Bathy_100,Seamount01_Extent)

plot(Seamount01_Depth)
contour(Seamount01_Depth,add=TRUE)

Seamount01_SummitDepth=round(max(values(Seamount01_Depth),na.rm=TRUE))
Seamount01_ValleyDepth=round(min(values(Seamount01_Depth),na.rm=TRUE))
Seamount01_Height=Seamount01_SummitDepth-Seamount01_ValleyDepth
Seamount01_SummitDepth
Seamount01_ValleyDepth
Seamount01_Height   ###### la prof du sommet est incorrecte,

values(Seamount01_Depth)[values(Seamount01_Depth) > -19] = round(runif(1000,19,23),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 40m

Seamount01_SummitDepth=round(max(values(Seamount01_Depth),na.rm=TRUE))
Seamount01_ValleyDepth=round(min(values(Seamount01_Depth),na.rm=TRUE))
Seamount01_Height=Seamount01_SummitDepth-Seamount01_ValleyDepth
Seamount01_SummitDepth
Seamount01_ValleyDepth
Seamount01_Height

plot(Seamount01_Depth)
contour(Seamount01_Depth,add=TRUE)

####SUMMIT
Seamount01_Summit_Poly=Seamount01_Depth
values(Seamount01_Summit_Poly)[values(Seamount01_Summit_Poly) < Seamount01_SummitDepth+30/100*Seamount01_SummitDepth] = NA
plot(Seamount01_Summit_Poly)
Seamount01_SummitRugosity=sd(values(Seamount01_Summit_Poly),na.rm=TRUE)
Seamount01_SummitRugosity
values(Seamount01_Summit_Poly)[!is.na(values(Seamount01_Summit_Poly))] <- 1
plot(Seamount01_Summit_Poly)
Seamount01_Summit_Poly=rasterToPolygons(Seamount01_Summit_Poly, dissolve=TRUE)
Seamount01_SummitAreaKm2=area(Seamount01_Summit_Poly)*1e-6
Seamount01_SummitAreaKm2
plot(Seamount01_Depth)
contour(Seamount01_Depth,add=TRUE)
plot(Seamount01_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Seamount01_Slope_Poly=Seamount01_Depth
values(Seamount01_Slope_Poly)[values(Seamount01_Slope_Poly) > Seamount01_SummitDepth+30/100*Seamount01_SummitDepth | values(Seamount01_Slope_Poly) < Seamount01_ValleyDepth-30/100*Seamount01_ValleyDepth] = NA
plot(Seamount01_Slope_Poly)
values(Seamount01_Slope_Poly)[!is.na(values(Seamount01_Slope_Poly))] <- 1
plot(Seamount01_Slope_Poly)
Seamount01_Slope_Poly=rasterToPolygons(Seamount01_Slope_Poly, dissolve=TRUE)
plot(Seamount01_Depth)
contour(Seamount01_Depth,add=TRUE)
plot(Seamount01_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Seamount01_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Seamount01_poly <- Seamount01_Slope_Poly+Seamount01_Summit_Poly


buffer_Seamount01 <- buffer(Seamount01_poly, width=0.02)
plot(buffer_Seamount01, add=T)

Seamount01_extent <- mask(Bathy_100, buffer_Seamount01)
raster_Seamount01 <- trim(Seamount01_extent, values=NA)
plot(raster_Seamount01)
names(raster_Seamount01) <- c("BottomDepth")
df_seamount01 <- as.data.frame(raster_Seamount01, xy=TRUE)

for (i in 1:nrow(df_seamount01)) {
  if (!is.na(df_seamount01[i,"BottomDepth"])){
    df_seamount01[i,"Habitat"] <- 1
    df_seamount01[i,"ValleyDepth"] <- Seamount01_ValleyDepth
    df_seamount01[i,"SummitDepth"] <- Seamount01_SummitDepth
    df_seamount01[i,"Height"] <- Seamount01_Height
    df_seamount01[i,"SummitAreaKm2"] <- Seamount01_SummitAreaKm2
    df_seamount01[i,"SummitRugosity"] <- Seamount01_SummitRugosity
  }
}

df <- df_seamount01
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_Seamount01 <- stack(df)
plot(raster_Seamount01)


#####################################################################################################################################
## Seamount02

Seamount02_Extent=extent(168.53,168.9,-22.5,-22.15)
Seamount02_Depth=crop(Bathy_100,Seamount02_Extent)

plot(Seamount02_Depth)
contour(Seamount02_Depth,add=TRUE)

Seamount02_SummitDepth=round(max(values(Seamount02_Depth),na.rm=TRUE))
Seamount02_ValleyDepth=round(min(values(Seamount02_Depth),na.rm=TRUE))
Seamount02_Height=Seamount02_SummitDepth-Seamount02_ValleyDepth
Seamount02_SummitDepth
Seamount02_ValleyDepth
Seamount02_Height   

plot(Seamount02_Depth)
contour(Seamount02_Depth,add=TRUE)

####SUMMIT
Seamount02_Summit_Poly=Seamount02_Depth
values(Seamount02_Summit_Poly)[values(Seamount02_Summit_Poly) < Seamount02_SummitDepth+30/100*Seamount02_SummitDepth] = NA
plot(Seamount02_Summit_Poly)
Seamount02_SummitRugosity=sd(values(Seamount02_Summit_Poly),na.rm=TRUE)
Seamount02_SummitRugosity
values(Seamount02_Summit_Poly)[!is.na(values(Seamount02_Summit_Poly))] <- 1
plot(Seamount02_Summit_Poly)
Seamount02_Summit_Poly=rasterToPolygons(Seamount02_Summit_Poly, dissolve=TRUE)
Seamount02_SummitAreaKm2=area(Seamount02_Summit_Poly)*1e-6
Seamount02_SummitAreaKm2
plot(Seamount02_Depth)
contour(Seamount02_Depth,add=TRUE)
plot(Seamount02_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Seamount02_Slope_Poly=Seamount02_Depth
values(Seamount02_Slope_Poly)[values(Seamount02_Slope_Poly) > Seamount02_SummitDepth+30/100*Seamount02_SummitDepth | values(Seamount02_Slope_Poly) < Seamount02_ValleyDepth-30/100*Seamount02_ValleyDepth] = NA
plot(Seamount02_Slope_Poly)
values(Seamount02_Slope_Poly)[!is.na(values(Seamount02_Slope_Poly))] <- 1
plot(Seamount02_Slope_Poly)
Seamount02_Slope_Poly=rasterToPolygons(Seamount02_Slope_Poly, dissolve=TRUE)
plot(Seamount02_Depth)
contour(Seamount02_Depth,add=TRUE)
plot(Seamount02_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Seamount02_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Seamount02_poly <- Seamount02_Slope_Poly+Seamount02_Summit_Poly


buffer_Seamount02 <- buffer(Seamount02_poly, width=0.03)
plot(buffer_Seamount02, add=T)

Seamount02_extent <- mask(Bathy_100, buffer_Seamount02)
raster_Seamount02 <- trim(Seamount02_extent, values=NA)
plot(raster_Seamount02)
names(raster_Seamount02) <- c("BottomDepth")
df_seamount02 <- as.data.frame(raster_Seamount02, xy=TRUE)

for (i in 1:nrow(df_seamount02)) {
  if (!is.na(df_seamount02[i,"BottomDepth"])){
    df_seamount02[i,"Habitat"] <- 2
    df_seamount02[i,"ValleyDepth"] <- Seamount02_ValleyDepth
    df_seamount02[i,"SummitDepth"] <- Seamount02_SummitDepth
    df_seamount02[i,"Height"] <- Seamount02_Height
    df_seamount02[i,"SummitAreaKm2"] <- Seamount02_SummitAreaKm2
    df_seamount02[i,"SummitRugosity"] <- Seamount02_SummitRugosity
  }
}

df <- df_seamount02
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_Seamount02 <- stack(df)
plot(raster_Seamount02)


#####################################################################################################################################
## Seamount03

Seamount03_Extent=extent(168.89,169.17,-22.48,-22.23)
Seamount03_Depth=crop(Bathy_100,Seamount03_Extent)

plot(Seamount03_Depth)
contour(Seamount03_Depth,add=TRUE)

Seamount03_SummitDepth=round(max(values(Seamount03_Depth),na.rm=TRUE))
Seamount03_ValleyDepth=round(min(values(Seamount03_Depth),na.rm=TRUE))
Seamount03_Height=Seamount03_SummitDepth-Seamount03_ValleyDepth
Seamount03_SummitDepth
Seamount03_ValleyDepth
Seamount03_Height   

####SUMMIT
Seamount03_Summit_Poly=Seamount03_Depth
values(Seamount03_Summit_Poly)[values(Seamount03_Summit_Poly) < Seamount03_SummitDepth+30/100*Seamount03_SummitDepth] = NA
plot(Seamount03_Summit_Poly)
Seamount03_SummitRugosity=sd(values(Seamount03_Summit_Poly),na.rm=TRUE)
Seamount03_SummitRugosity
values(Seamount03_Summit_Poly)[!is.na(values(Seamount03_Summit_Poly))] <- 1
plot(Seamount03_Summit_Poly)
Seamount03_Summit_Poly=rasterToPolygons(Seamount03_Summit_Poly, dissolve=TRUE)
Seamount03_SummitAreaKm2=area(Seamount03_Summit_Poly)*1e-6
Seamount03_SummitAreaKm2
plot(Seamount03_Depth)
contour(Seamount03_Depth,add=TRUE)
plot(Seamount03_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Seamount03_Slope_Poly=Seamount03_Depth
values(Seamount03_Slope_Poly)[values(Seamount03_Slope_Poly) > Seamount03_SummitDepth+30/100*Seamount03_SummitDepth | values(Seamount03_Slope_Poly) < Seamount03_ValleyDepth-30/100*Seamount03_ValleyDepth] = NA
plot(Seamount03_Slope_Poly)
values(Seamount03_Slope_Poly)[!is.na(values(Seamount03_Slope_Poly))] <- 1
plot(Seamount03_Slope_Poly)
Seamount03_Slope_Poly=rasterToPolygons(Seamount03_Slope_Poly, dissolve=TRUE)
plot(Seamount03_Depth)
contour(Seamount03_Depth,add=TRUE)
plot(Seamount03_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Seamount03_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Seamount03_poly <- Seamount03_Slope_Poly+Seamount03_Summit_Poly


buffer_Seamount03 <- buffer(Seamount03_poly, width=0.05)
plot(buffer_Seamount03, add=T)

Seamount03_extent <- mask(Bathy_100, buffer_Seamount03)
raster_Seamount03 <- trim(Seamount03_extent, values=NA)
plot(raster_Seamount03)
names(raster_Seamount03) <- c("BottomDepth")
df_seamount03 <- as.data.frame(raster_Seamount03, xy=TRUE)

for (i in 1:nrow(df_seamount03)) {
  if (!is.na(df_seamount03[i,"BottomDepth"])){
    df_seamount03[i,"Habitat"] <- 1
    df_seamount03[i,"ValleyDepth"] <- Seamount03_ValleyDepth
    df_seamount03[i,"SummitDepth"] <- Seamount03_SummitDepth
    df_seamount03[i,"Height"] <- Seamount03_Height
    df_seamount03[i,"SummitAreaKm2"] <- Seamount03_SummitAreaKm2
    df_seamount03[i,"SummitRugosity"] <- Seamount03_SummitRugosity
  }
}

df <- df_seamount03
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_Seamount03 <- stack(df)
plot(raster_Seamount03)

#####################################################################################################################################
## Seamount04

Seamount04_Extent=extent(171.5,172,-22.6,-22.23)
Seamount04_Depth=crop(Bathy_100,Seamount04_Extent)

plot(Seamount04_Depth)
contour(Seamount04_Depth,add=TRUE)

Seamount04_SummitDepth=round(max(values(Seamount04_Depth),na.rm=TRUE))
Seamount04_ValleyDepth=round(min(values(Seamount04_Depth),na.rm=TRUE))
Seamount04_Height=Seamount04_SummitDepth-Seamount04_ValleyDepth
Seamount04_SummitDepth
Seamount04_ValleyDepth
Seamount04_Height   

####SUMMIT
Seamount04_Summit_Poly=Seamount04_Depth
values(Seamount04_Summit_Poly)[values(Seamount04_Summit_Poly) < Seamount04_SummitDepth+30/100*Seamount04_SummitDepth] = NA
plot(Seamount04_Summit_Poly)
Seamount04_SummitRugosity=sd(values(Seamount04_Summit_Poly),na.rm=TRUE)
Seamount04_SummitRugosity
values(Seamount04_Summit_Poly)[!is.na(values(Seamount04_Summit_Poly))] <- 1
plot(Seamount04_Summit_Poly)
Seamount04_Summit_Poly=rasterToPolygons(Seamount04_Summit_Poly, dissolve=TRUE)
Seamount04_SummitAreaKm2=area(Seamount04_Summit_Poly)*1e-6
Seamount04_SummitAreaKm2
plot(Seamount04_Depth)
contour(Seamount04_Depth,add=TRUE)
plot(Seamount04_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Seamount04_Slope_Poly=Seamount04_Depth
values(Seamount04_Slope_Poly)[values(Seamount04_Slope_Poly) > Seamount04_SummitDepth+30/100*Seamount04_SummitDepth | values(Seamount04_Slope_Poly) < Seamount04_ValleyDepth-30/100*Seamount04_ValleyDepth] = NA
plot(Seamount04_Slope_Poly)
values(Seamount04_Slope_Poly)[!is.na(values(Seamount04_Slope_Poly))] <- 1
plot(Seamount04_Slope_Poly)
Seamount04_Slope_Poly=rasterToPolygons(Seamount04_Slope_Poly, dissolve=TRUE)
plot(Seamount04_Depth)
contour(Seamount04_Depth,add=TRUE)
plot(Seamount04_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Seamount04_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Seamount04_poly <- Seamount04_Slope_Poly+Seamount04_Summit_Poly


buffer_Seamount04 <- buffer(Seamount04_poly, width=0.05)
plot(buffer_Seamount04, add=T)

Seamount04_extent <- mask(Bathy_100, buffer_Seamount04)
raster_Seamount04 <- trim(Seamount04_extent, values=NA)
plot(raster_Seamount04)
names(raster_Seamount04) <- c("BottomDepth")
df_seamount04 <- as.data.frame(raster_Seamount04, xy=TRUE)

for (i in 1:nrow(df_seamount04)) {
  if (!is.na(df_seamount04[i,"BottomDepth"])){
    df_seamount04[i,"Habitat"] <- 2
    df_seamount04[i,"ValleyDepth"] <- Seamount04_ValleyDepth
    df_seamount04[i,"SummitDepth"] <- Seamount04_SummitDepth
    df_seamount04[i,"Height"] <- Seamount04_Height
    df_seamount04[i,"SummitAreaKm2"] <- Seamount04_SummitAreaKm2
    df_seamount04[i,"SummitRugosity"] <- Seamount04_SummitRugosity
  }
}

df <- df_seamount04
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_Seamount04 <- stack(df)
plot(raster_Seamount04)


#####################################################################################################################################
## Introuvable

Introuvable_Extent=extent(168.55,168.75,-24.77,-24.55)
Introuvable_Depth=crop(Bathy_100,Introuvable_Extent)

plot(Introuvable_Depth)
contour(Introuvable_Depth,add=TRUE)

Introuvable_SummitDepth=round(max(values(Introuvable_Depth),na.rm=TRUE))
Introuvable_ValleyDepth=round(min(values(Introuvable_Depth),na.rm=TRUE))
Introuvable_Height=Introuvable_SummitDepth-Introuvable_ValleyDepth
Introuvable_SummitDepth
Introuvable_ValleyDepth
Introuvable_Height   

####SUMMIT
Introuvable_Summit_Poly=Introuvable_Depth
values(Introuvable_Summit_Poly)[values(Introuvable_Summit_Poly) < Introuvable_SummitDepth+30/100*Introuvable_SummitDepth] = NA
plot(Introuvable_Summit_Poly)
Introuvable_SummitRugosity=sd(values(Introuvable_Summit_Poly),na.rm=TRUE)
Introuvable_SummitRugosity
values(Introuvable_Summit_Poly)[!is.na(values(Introuvable_Summit_Poly))] <- 1
plot(Introuvable_Summit_Poly)
Introuvable_Summit_Poly=rasterToPolygons(Introuvable_Summit_Poly, dissolve=TRUE)
Introuvable_SummitAreaKm2=area(Introuvable_Summit_Poly)*1e-6
Introuvable_SummitAreaKm2
plot(Introuvable_Depth)
contour(Introuvable_Depth,add=TRUE)
plot(Introuvable_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Introuvable_Slope_Poly=Introuvable_Depth
values(Introuvable_Slope_Poly)[values(Introuvable_Slope_Poly) > Introuvable_SummitDepth+30/100*Introuvable_SummitDepth | values(Introuvable_Slope_Poly) < Introuvable_ValleyDepth-30/100*Introuvable_ValleyDepth] = NA
plot(Introuvable_Slope_Poly)
values(Introuvable_Slope_Poly)[!is.na(values(Introuvable_Slope_Poly))] <- 1
plot(Introuvable_Slope_Poly)
Introuvable_Slope_Poly=rasterToPolygons(Introuvable_Slope_Poly, dissolve=TRUE)
plot(Introuvable_Depth)
contour(Introuvable_Depth,add=TRUE)
plot(Introuvable_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Introuvable_Summit_Poly,border="magenta",lwd=3,add=TRUE)
Introuvable_poly <- Introuvable_Slope_Poly+Introuvable_Summit_Poly


buffer_Introuvable <- buffer(Introuvable_poly, width=0.05)
plot(buffer_Introuvable, add=T)

Introuvable_extent <- mask(Bathy_100, buffer_Introuvable)
raster_Introuvable <- trim(Introuvable_extent, values=NA)
plot(raster_Introuvable)
names(raster_Introuvable) <- c("BottomDepth")
df_Introuvable <- as.data.frame(raster_Introuvable, xy=TRUE)

for (i in 1:nrow(df_Introuvable)) {
  if (!is.na(df_Introuvable[i,"BottomDepth"])){
    df_Introuvable[i,"Habitat"] <- 3
    df_Introuvable[i,"ValleyDepth"] <- Introuvable_ValleyDepth
    df_Introuvable[i,"SummitDepth"] <- Introuvable_SummitDepth
    df_Introuvable[i,"Height"] <- Introuvable_Height
    df_Introuvable[i,"SummitAreaKm2"] <- Introuvable_SummitAreaKm2
    df_Introuvable[i,"SummitRugosity"] <- Introuvable_SummitRugosity
  }
}

df <- df_Introuvable
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_Introuvable <- stack(df)
plot(raster_Introuvable)

#####################################################################################################################################
## Seamount05

seamount05_Extent=extent(169.613,169.955,-24.135,-23.771)
seamount05_Depth=crop(Bathy_100,seamount05_Extent)

plot(seamount05_Depth)
contour(seamount05_Depth,add=TRUE)

seamount05_SummitDepth=round(max(values(seamount05_Depth),na.rm=TRUE))
seamount05_ValleyDepth=round(min(values(seamount05_Depth),na.rm=TRUE))
seamount05_Height=seamount05_SummitDepth-seamount05_ValleyDepth

####SUMMIT
seamount05_Summit_Poly=seamount05_Depth
values(seamount05_Summit_Poly)[values(seamount05_Summit_Poly) < seamount05_SummitDepth+30/100*seamount05_SummitDepth] = NA
plot(seamount05_Summit_Poly)
seamount05_SummitRugosity=sd(values(seamount05_Summit_Poly),na.rm=TRUE)
seamount05_SummitRugosity
values(seamount05_Summit_Poly)[!is.na(values(seamount05_Summit_Poly))] <- 1
plot(seamount05_Summit_Poly)
seamount05_Summit_Poly=rasterToPolygons(seamount05_Summit_Poly, dissolve=TRUE)
seamount05_SummitAreaKm2=area(seamount05_Summit_Poly)*1e-6
seamount05_SummitAreaKm2
plot(seamount05_Depth)
contour(seamount05_Depth,add=TRUE)
plot(seamount05_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount05_Slope_Poly=seamount05_Depth
values(seamount05_Slope_Poly)[values(seamount05_Slope_Poly) > seamount05_SummitDepth+30/100*seamount05_SummitDepth | values(seamount05_Slope_Poly) < seamount05_ValleyDepth-30/100*seamount05_ValleyDepth] = NA
plot(seamount05_Slope_Poly)
values(seamount05_Slope_Poly)[!is.na(values(seamount05_Slope_Poly))] <- 1
plot(seamount05_Slope_Poly)
seamount05_Slope_Poly=rasterToPolygons(seamount05_Slope_Poly, dissolve=TRUE)
plot(seamount05_Depth)
contour(seamount05_Depth,add=TRUE)
plot(seamount05_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount05_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount05_poly <- seamount05_Slope_Poly+seamount05_Summit_Poly

buffer_seamount05 <- buffer(seamount05_poly, width=0.03)
plot(buffer_seamount05, add=T)

seamount05_extent <- mask(Bathy_100, buffer_seamount05)
raster_seamount05 <- trim(seamount05_extent, values=NA)
plot(raster_seamount05)
names(raster_seamount05) <- c("BottomDepth")
df_seamount05 <- as.data.frame(raster_seamount05, xy=TRUE)

for (i in 1:nrow(df_seamount05)) {
  if (!is.na(df_seamount05[i,"BottomDepth"])){
    df_seamount05[i,"Habitat"] <- 3
    df_seamount05[i,"ValleyDepth"] <- seamount05_ValleyDepth
    df_seamount05[i,"SummitDepth"] <- seamount05_SummitDepth
    df_seamount05[i,"Height"] <- seamount05_Height
    df_seamount05[i,"SummitAreaKm2"] <- seamount05_SummitAreaKm2
    df_seamount05[i,"SummitRugosity"] <- seamount05_SummitRugosity
  }
}

df <- df_seamount05
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount05 <- stack(df)
plot(raster_seamount05)


#####################################################################################################################################
## seamount06

seamount06_Extent=extent(168.178,168.448,-23.101,-22.924)
seamount06_Depth=crop(Bathy_100,seamount06_Extent)

plot(seamount06_Depth)
contour(seamount06_Depth,add=TRUE)

seamount06_SummitDepth=round(max(values(seamount06_Depth),na.rm=TRUE))
seamount06_ValleyDepth=round(min(values(seamount06_Depth),na.rm=TRUE))
seamount06_Height=seamount06_SummitDepth-seamount06_ValleyDepth

####SUMMIT
seamount06_Summit_Poly=seamount06_Depth
values(seamount06_Summit_Poly)[values(seamount06_Summit_Poly) < seamount06_SummitDepth+30/100*seamount06_SummitDepth] = NA
plot(seamount06_Summit_Poly)
seamount06_SummitRugosity=sd(values(seamount06_Summit_Poly),na.rm=TRUE)
seamount06_SummitRugosity
values(seamount06_Summit_Poly)[!is.na(values(seamount06_Summit_Poly))] <- 1
plot(seamount06_Summit_Poly)
seamount06_Summit_Poly=rasterToPolygons(seamount06_Summit_Poly, dissolve=TRUE)
seamount06_SummitAreaKm2=area(seamount06_Summit_Poly)*1e-6
seamount06_SummitAreaKm2
plot(seamount06_Depth)
contour(seamount06_Depth,add=TRUE)
plot(seamount06_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount06_Slope_Poly=seamount06_Depth
values(seamount06_Slope_Poly)[values(seamount06_Slope_Poly) > seamount06_SummitDepth+30/100*seamount06_SummitDepth | values(seamount06_Slope_Poly) < seamount06_ValleyDepth-30/100*seamount06_ValleyDepth] = NA
plot(seamount06_Slope_Poly)
values(seamount06_Slope_Poly)[!is.na(values(seamount06_Slope_Poly))] <- 1
plot(seamount06_Slope_Poly)
seamount06_Slope_Poly=rasterToPolygons(seamount06_Slope_Poly, dissolve=TRUE)
plot(seamount06_Depth)
contour(seamount06_Depth,add=TRUE)
plot(seamount06_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount06_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount06_poly <- seamount06_Slope_Poly+seamount06_Summit_Poly

buffer_seamount06 <- buffer(seamount06_poly, width=0.02)
plot(buffer_seamount06, add=T)

seamount06_extent <- mask(Bathy_100, buffer_seamount06)
raster_seamount06 <- trim(seamount06_extent, values=NA)
plot(raster_seamount06)
names(raster_seamount06) <- c("BottomDepth")
df_seamount06 <- as.data.frame(raster_seamount06, xy=TRUE)

for (i in 1:nrow(df_seamount06)) {
  if (!is.na(df_seamount06[i,"BottomDepth"])){
    df_seamount06[i,"Habitat"] <- 2
    df_seamount06[i,"ValleyDepth"] <- seamount06_ValleyDepth
    df_seamount06[i,"SummitDepth"] <- seamount06_SummitDepth
    df_seamount06[i,"Height"] <- seamount06_Height
    df_seamount06[i,"SummitAreaKm2"] <- seamount06_SummitAreaKm2
    df_seamount06[i,"SummitRugosity"] <- seamount06_SummitRugosity
  }
}

df <- df_seamount06
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount06 <- stack(df)
plot(raster_seamount06)

#####################################################################################################################################
## seamount07

seamount07_Extent=extent(168.881,169.074,-22.736,-22.512)
seamount07_Depth=crop(Bathy_100,seamount07_Extent)

plot(seamount07_Depth)
contour(seamount07_Depth,add=TRUE)

seamount07_SummitDepth=round(max(values(seamount07_Depth),na.rm=TRUE))
seamount07_ValleyDepth=round(min(values(seamount07_Depth),na.rm=TRUE))
seamount07_Height=seamount07_SummitDepth-seamount07_ValleyDepth

####SUMMIT
seamount07_Summit_Poly=seamount07_Depth
values(seamount07_Summit_Poly)[values(seamount07_Summit_Poly) < seamount07_SummitDepth+30/100*seamount07_SummitDepth] = NA
plot(seamount07_Summit_Poly)
seamount07_SummitRugosity=sd(values(seamount07_Summit_Poly),na.rm=TRUE)
seamount07_SummitRugosity
values(seamount07_Summit_Poly)[!is.na(values(seamount07_Summit_Poly))] <- 1
plot(seamount07_Summit_Poly)
seamount07_Summit_Poly=rasterToPolygons(seamount07_Summit_Poly, dissolve=TRUE)
seamount07_SummitAreaKm2=area(seamount07_Summit_Poly)*1e-6
seamount07_SummitAreaKm2
plot(seamount07_Depth)
contour(seamount07_Depth,add=TRUE)
plot(seamount07_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount07_Slope_Poly=seamount07_Depth
values(seamount07_Slope_Poly)[values(seamount07_Slope_Poly) > seamount07_SummitDepth+30/100*seamount07_SummitDepth | values(seamount07_Slope_Poly) < seamount07_ValleyDepth-30/100*seamount07_ValleyDepth] = NA
plot(seamount07_Slope_Poly)
values(seamount07_Slope_Poly)[!is.na(values(seamount07_Slope_Poly))] <- 1
plot(seamount07_Slope_Poly)
seamount07_Slope_Poly=rasterToPolygons(seamount07_Slope_Poly, dissolve=TRUE)
plot(seamount07_Depth)
contour(seamount07_Depth,add=TRUE)
plot(seamount07_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount07_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount07_poly <- seamount07_Slope_Poly+seamount07_Summit_Poly

buffer_seamount07 <- buffer(seamount07_poly, width=0.02)
plot(buffer_seamount07, add=T)

seamount07_extent <- mask(Bathy_100, buffer_seamount07)
raster_seamount07 <- trim(seamount07_extent, values=NA)
plot(raster_seamount07)
names(raster_seamount07) <- c("BottomDepth")
df_seamount07 <- as.data.frame(raster_seamount07, xy=TRUE)

for (i in 1:nrow(df_seamount07)) {
  if (!is.na(df_seamount07[i,"BottomDepth"])){
    df_seamount07[i,"Habitat"] <- 1
    df_seamount07[i,"ValleyDepth"] <- seamount07_ValleyDepth
    df_seamount07[i,"SummitDepth"] <- seamount07_SummitDepth
    df_seamount07[i,"Height"] <- seamount07_Height
    df_seamount07[i,"SummitAreaKm2"] <- seamount07_SummitAreaKm2
    df_seamount07[i,"SummitRugosity"] <- seamount07_SummitRugosity
  }
}

df <- df_seamount07
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount07 <- stack(df)
plot(raster_seamount07)



#####################################################################################################################################
## seamount08

seamount08_Extent=extent(156.212,156.486,-21.663,-21.417)
seamount08_Depth=crop(Bathy_100,seamount08_Extent)

plot(seamount08_Depth)
contour(seamount08_Depth,add=TRUE)

seamount08_SummitDepth=round(max(values(seamount08_Depth),na.rm=TRUE))
seamount08_ValleyDepth=round(min(values(seamount08_Depth),na.rm=TRUE))
seamount08_Height=seamount08_SummitDepth-seamount08_ValleyDepth

####SUMMIT
seamount08_Summit_Poly=seamount08_Depth
values(seamount08_Summit_Poly)[values(seamount08_Summit_Poly) < seamount08_SummitDepth+30/100*seamount08_SummitDepth] = NA
plot(seamount08_Summit_Poly)
seamount08_SummitRugosity=sd(values(seamount08_Summit_Poly),na.rm=TRUE)
seamount08_SummitRugosity
values(seamount08_Summit_Poly)[!is.na(values(seamount08_Summit_Poly))] <- 1
plot(seamount08_Summit_Poly)
seamount08_Summit_Poly=rasterToPolygons(seamount08_Summit_Poly, dissolve=TRUE)
seamount08_SummitAreaKm2=area(seamount08_Summit_Poly)*1e-6
seamount08_SummitAreaKm2
plot(seamount08_Depth)
contour(seamount08_Depth,add=TRUE)
plot(seamount08_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount08_Slope_Poly=seamount08_Depth
values(seamount08_Slope_Poly)[values(seamount08_Slope_Poly) > seamount08_SummitDepth+30/100*seamount08_SummitDepth | values(seamount08_Slope_Poly) < seamount08_ValleyDepth-30/100*seamount08_ValleyDepth] = NA
plot(seamount08_Slope_Poly)
values(seamount08_Slope_Poly)[!is.na(values(seamount08_Slope_Poly))] <- 1
plot(seamount08_Slope_Poly)
seamount08_Slope_Poly=rasterToPolygons(seamount08_Slope_Poly, dissolve=TRUE)
plot(seamount08_Depth)
contour(seamount08_Depth,add=TRUE)
plot(seamount08_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount08_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount08_poly <- seamount08_Slope_Poly+seamount08_Summit_Poly

buffer_seamount08 <- buffer(seamount08_poly, width=0.02)
plot(buffer_seamount08, add=T)

seamount08_extent <- mask(Bathy_100, buffer_seamount08)
raster_seamount08 <- trim(seamount08_extent, values=NA)
plot(raster_seamount08)
names(raster_seamount08) <- c("BottomDepth")
df_seamount08 <- as.data.frame(raster_seamount08, xy=TRUE)

for (i in 1:nrow(df_seamount08)) {
  if (!is.na(df_seamount08[i,"BottomDepth"])){
    df_seamount08[i,"Habitat"] <- 2
    df_seamount08[i,"ValleyDepth"] <- seamount08_ValleyDepth
    df_seamount08[i,"SummitDepth"] <- seamount08_SummitDepth
    df_seamount08[i,"Height"] <- seamount08_Height
    df_seamount08[i,"SummitAreaKm2"] <- seamount08_SummitAreaKm2
    df_seamount08[i,"SummitRugosity"] <- seamount08_SummitRugosity
  }
}

df <- df_seamount08
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount08 <- stack(df)
plot(raster_seamount08)



#####################################################################################################################################
## seamount09

seamount09_Extent=extent(157.012,157.169,-21,-20.849)
seamount09_Depth=crop(Bathy_100,seamount09_Extent)

plot(seamount09_Depth)
contour(seamount09_Depth,add=TRUE)

seamount09_SummitDepth=round(max(values(seamount09_Depth),na.rm=TRUE))
seamount09_ValleyDepth=round(min(values(seamount09_Depth),na.rm=TRUE))
seamount09_Height=seamount09_SummitDepth-seamount09_ValleyDepth

####SUMMIT
seamount09_Summit_Poly=seamount09_Depth
values(seamount09_Summit_Poly)[values(seamount09_Summit_Poly) < seamount09_SummitDepth+30/100*seamount09_SummitDepth] = NA
plot(seamount09_Summit_Poly)
seamount09_SummitRugosity=sd(values(seamount09_Summit_Poly),na.rm=TRUE)
seamount09_SummitRugosity
values(seamount09_Summit_Poly)[!is.na(values(seamount09_Summit_Poly))] <- 1
plot(seamount09_Summit_Poly)
seamount09_Summit_Poly=rasterToPolygons(seamount09_Summit_Poly, dissolve=TRUE)
seamount09_SummitAreaKm2=area(seamount09_Summit_Poly)*1e-6
seamount09_SummitAreaKm2
plot(seamount09_Depth)
contour(seamount09_Depth,add=TRUE)
plot(seamount09_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount09_Slope_Poly=seamount09_Depth
values(seamount09_Slope_Poly)[values(seamount09_Slope_Poly) > seamount09_SummitDepth+30/100*seamount09_SummitDepth | values(seamount09_Slope_Poly) < seamount09_ValleyDepth-30/100*seamount09_ValleyDepth] = NA
plot(seamount09_Slope_Poly)
values(seamount09_Slope_Poly)[!is.na(values(seamount09_Slope_Poly))] <- 1
plot(seamount09_Slope_Poly)
seamount09_Slope_Poly=rasterToPolygons(seamount09_Slope_Poly, dissolve=TRUE)
plot(seamount09_Depth)
contour(seamount09_Depth,add=TRUE)
plot(seamount09_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount09_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount09_poly <- seamount09_Slope_Poly+seamount09_Summit_Poly

buffer_seamount09 <- buffer(seamount09_poly, width=0.03)
plot(buffer_seamount09, add=T)

seamount09_extent <- mask(Bathy_100, buffer_seamount09)
raster_seamount09 <- trim(seamount09_extent, values=NA)
plot(raster_seamount09)
names(raster_seamount09) <- c("BottomDepth")
df_seamount09 <- as.data.frame(raster_seamount09, xy=TRUE)

for (i in 1:nrow(df_seamount09)) {
  if (!is.na(df_seamount09[i,"BottomDepth"])){
    df_seamount09[i,"Habitat"] <- 2
    df_seamount09[i,"ValleyDepth"] <- seamount09_ValleyDepth
    df_seamount09[i,"SummitDepth"] <- seamount09_SummitDepth
    df_seamount09[i,"Height"] <- seamount09_Height
    df_seamount09[i,"SummitAreaKm2"] <- seamount09_SummitAreaKm2
    df_seamount09[i,"SummitRugosity"] <- seamount09_SummitRugosity
  }
}

df <- df_seamount09
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount09 <- stack(df)
plot(raster_seamount09)



#####################################################################################################################################
## seamount10

seamount10_Extent=extent(156.602,156.780,-22.128,-21.92)
seamount10_Depth=crop(Bathy_100,seamount10_Extent)

plot(seamount10_Depth)
contour(seamount10_Depth,add=TRUE)

seamount10_SummitDepth=round(max(values(seamount10_Depth),na.rm=TRUE))
seamount10_ValleyDepth=round(min(values(seamount10_Depth),na.rm=TRUE))
seamount10_Height=seamount10_SummitDepth-seamount10_ValleyDepth

####SUMMIT
seamount10_Summit_Poly=seamount10_Depth
values(seamount10_Summit_Poly)[values(seamount10_Summit_Poly) < seamount10_SummitDepth+30/100*seamount10_SummitDepth] = NA
plot(seamount10_Summit_Poly)
seamount10_SummitRugosity=sd(values(seamount10_Summit_Poly),na.rm=TRUE)
seamount10_SummitRugosity
values(seamount10_Summit_Poly)[!is.na(values(seamount10_Summit_Poly))] <- 1
plot(seamount10_Summit_Poly)
seamount10_Summit_Poly=rasterToPolygons(seamount10_Summit_Poly, dissolve=TRUE)
seamount10_SummitAreaKm2=area(seamount10_Summit_Poly)*1e-6
seamount10_SummitAreaKm2
plot(seamount10_Depth)
contour(seamount10_Depth,add=TRUE)
plot(seamount10_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount10_Slope_Poly=seamount10_Depth
values(seamount10_Slope_Poly)[values(seamount10_Slope_Poly) > seamount10_SummitDepth+30/100*seamount10_SummitDepth | values(seamount10_Slope_Poly) < seamount10_ValleyDepth-30/100*seamount10_ValleyDepth] = NA
plot(seamount10_Slope_Poly)
values(seamount10_Slope_Poly)[!is.na(values(seamount10_Slope_Poly))] <- 1
plot(seamount10_Slope_Poly)
seamount10_Slope_Poly=rasterToPolygons(seamount10_Slope_Poly, dissolve=TRUE)
plot(seamount10_Depth)
contour(seamount10_Depth,add=TRUE)
plot(seamount10_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount10_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount10_poly <- seamount10_Slope_Poly+seamount10_Summit_Poly

buffer_seamount10 <- buffer(seamount10_poly, width=0.03)
plot(buffer_seamount10, add=T)

seamount10_extent <- mask(Bathy_100, buffer_seamount10)
raster_seamount10 <- trim(seamount10_extent, values=NA)
plot(raster_seamount10)
names(raster_seamount10) <- c("BottomDepth")
df_seamount10 <- as.data.frame(raster_seamount10, xy=TRUE)

for (i in 1:nrow(df_seamount10)) {
  if (!is.na(df_seamount10[i,"BottomDepth"])){
    df_seamount10[i,"Habitat"] <- 2
    df_seamount10[i,"ValleyDepth"] <- seamount10_ValleyDepth
    df_seamount10[i,"SummitDepth"] <- seamount10_SummitDepth
    df_seamount10[i,"Height"] <- seamount10_Height
    df_seamount10[i,"SummitAreaKm2"] <- seamount10_SummitAreaKm2
    df_seamount10[i,"SummitRugosity"] <- seamount10_SummitRugosity
  }
}

df <- df_seamount10
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount10 <- stack(df)
plot(raster_seamount10)



#####################################################################################################################################
## seamount11

seamount11_Extent=extent(160.091,161.4,-20.942,-19.811)
seamount11_Depth=crop(Bathy_100,seamount11_Extent)

plot(seamount11_Depth)
contour(seamount11_Depth,add=TRUE)

seamount11_SummitDepth=round(max(values(seamount11_Depth),na.rm=TRUE))
seamount11_ValleyDepth=round(min(values(seamount11_Depth),na.rm=TRUE))
seamount11_Height=seamount11_SummitDepth-seamount11_ValleyDepth

####SUMMIT
seamount11_Summit_Poly=seamount11_Depth
values(seamount11_Summit_Poly)[values(seamount11_Summit_Poly) < -60] = NA
plot(seamount11_Summit_Poly)
seamount11_SummitRugosity=sd(values(seamount11_Summit_Poly),na.rm=TRUE)
seamount11_SummitRugosity
values(seamount11_Summit_Poly)[!is.na(values(seamount11_Summit_Poly))] <- 1
plot(seamount11_Summit_Poly)
seamount11_Summit_Poly=rasterToPolygons(seamount11_Summit_Poly, dissolve=TRUE)
seamount11_SummitAreaKm2=area(seamount11_Summit_Poly)*1e-6
seamount11_SummitAreaKm2
plot(seamount11_Depth)
contour(seamount11_Depth,add=TRUE)
plot(seamount11_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount11_Slope_Poly=seamount11_Depth
values(seamount11_Slope_Poly)[values(seamount11_Slope_Poly) > seamount11_SummitDepth+30/100*seamount11_SummitDepth | values(seamount11_Slope_Poly) < seamount11_ValleyDepth-30/100*seamount11_ValleyDepth] = NA
plot(seamount11_Slope_Poly)
values(seamount11_Slope_Poly)[!is.na(values(seamount11_Slope_Poly))] <- 1
plot(seamount11_Slope_Poly)
seamount11_Slope_Poly=rasterToPolygons(seamount11_Slope_Poly, dissolve=TRUE)
plot(seamount11_Depth)
contour(seamount11_Depth,add=TRUE)
plot(seamount11_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount11_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount11_poly <- seamount11_Slope_Poly+seamount11_Summit_Poly

buffer_seamount11 <- buffer(seamount11_poly, width=0.03)
plot(buffer_seamount11, add=T)

seamount11_extent <- mask(Bathy_100, buffer_seamount11)
raster_seamount11 <- trim(seamount11_extent, values=NA)
plot(raster_seamount11)
names(raster_seamount11) <- c("BottomDepth")
df_seamount11 <- as.data.frame(raster_seamount11, xy=TRUE)

for (i in 1:nrow(df_seamount11)) {
  if (!is.na(df_seamount11[i,"BottomDepth"])){
    df_seamount11[i,"Habitat"] <- 1
    df_seamount11[i,"ValleyDepth"] <- seamount11_ValleyDepth
    df_seamount11[i,"SummitDepth"] <- seamount11_SummitDepth
    df_seamount11[i,"Height"] <- seamount11_Height
    df_seamount11[i,"SummitAreaKm2"] <- seamount11_SummitAreaKm2
    df_seamount11[i,"SummitRugosity"] <- seamount11_SummitRugosity
  }
}

df <- df_seamount11
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount11 <- stack(df)
plot(raster_seamount11)



#####################################################################################################################################
## seamount12

seamount12_Extent=extent(161.446,161.873,-21.12,-20.633)
seamount12_Depth=crop(Bathy_100,seamount12_Extent)

plot(seamount12_Depth)
contour(seamount12_Depth,add=TRUE)

seamount12_SummitDepth=round(max(values(seamount12_Depth),na.rm=TRUE))
seamount12_ValleyDepth=round(min(values(seamount12_Depth),na.rm=TRUE))
seamount12_Height=seamount12_SummitDepth-seamount12_ValleyDepth

####SUMMIT
seamount12_Summit_Poly=seamount12_Depth
values(seamount12_Summit_Poly)[values(seamount12_Summit_Poly) < -60] = NA
plot(seamount12_Summit_Poly)
seamount12_SummitRugosity=sd(values(seamount12_Summit_Poly),na.rm=TRUE)
seamount12_SummitRugosity
values(seamount12_Summit_Poly)[!is.na(values(seamount12_Summit_Poly))] <- 1
plot(seamount12_Summit_Poly)
seamount12_Summit_Poly=rasterToPolygons(seamount12_Summit_Poly, dissolve=TRUE)
seamount12_SummitAreaKm2=area(seamount12_Summit_Poly)*1e-6
seamount12_SummitAreaKm2
plot(seamount12_Depth)
contour(seamount12_Depth,add=TRUE)
plot(seamount12_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
seamount12_Slope_Poly=seamount12_Depth
values(seamount12_Slope_Poly)[values(seamount12_Slope_Poly) > seamount12_SummitDepth+30/100*seamount12_SummitDepth | values(seamount12_Slope_Poly) < seamount12_ValleyDepth-30/100*seamount12_ValleyDepth] = NA
plot(seamount12_Slope_Poly)
values(seamount12_Slope_Poly)[!is.na(values(seamount12_Slope_Poly))] <- 1
plot(seamount12_Slope_Poly)
seamount12_Slope_Poly=rasterToPolygons(seamount12_Slope_Poly, dissolve=TRUE)
plot(seamount12_Depth)
contour(seamount12_Depth,add=TRUE)
plot(seamount12_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(seamount12_Summit_Poly,border="magenta",lwd=3,add=TRUE)
seamount12_poly <- seamount12_Slope_Poly+seamount12_Summit_Poly

buffer_seamount12 <- buffer(seamount12_poly, width=0.03)
plot(buffer_seamount12, add=T)

seamount12_extent <- mask(Bathy_100, buffer_seamount12)
raster_seamount12 <- trim(seamount12_extent, values=NA)
plot(raster_seamount12)
names(raster_seamount12) <- c("BottomDepth")
df_seamount12 <- as.data.frame(raster_seamount12, xy=TRUE)

for (i in 1:nrow(df_seamount12)) {
  if (!is.na(df_seamount12[i,"BottomDepth"])){
    df_seamount12[i,"Habitat"] <- 1
    df_seamount12[i,"ValleyDepth"] <- seamount12_ValleyDepth
    df_seamount12[i,"SummitDepth"] <- seamount12_SummitDepth
    df_seamount12[i,"Height"] <- seamount12_Height
    df_seamount12[i,"SummitAreaKm2"] <- seamount12_SummitAreaKm2
    df_seamount12[i,"SummitRugosity"] <- seamount12_SummitRugosity
  }
}

df <- df_seamount12
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_seamount12 <- stack(df)
plot(raster_seamount12)



#####################################################################################################################################
## Merge all df into one

raster_list <- list(raster_antigonia, raster_argo, raster_capel, raster_Chesterfield, raster_crypthelia, raster_eponge, raster_fairway,
                    raster_iledespins, raster_Introuvable, raster_JumeauEst, raster_jumeauouest, raster_kaimonmaru, raster_nova,
                    raster_Seamount01, raster_Seamount02, raster_Seamount03, raster_Seamount04, raster_stylaster, raster_torche,
                    raster_seamount05, raster_seamount06, raster_seamount07, raster_seamount08, raster_seamount09,raster_seamount10,
                    raster_seamount11, raster_seamount12)

raster_seamounts <- do.call(merge, raster_list)
names(raster_seamounts) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")
plot(raster_seamounts)

raster_seamounts <- rast(raster_seamounts)
names(raster_seamounts) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")

writeRaster(raster_seamounts, filename = "02_formating_data/00_Prediction_raster/raster_seamounts.tif", overwrite=T)

df_seamounts <- as.data.frame(raster_seamounts, xy=TRUE)

save(df_seamounts, file="02_formating_data/00_Prediction_raster/Rdata/df_seamounts.rdata")


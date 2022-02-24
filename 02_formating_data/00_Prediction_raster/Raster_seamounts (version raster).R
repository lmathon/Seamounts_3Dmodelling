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

## add all the predictor variables to the raster
Habitat <- raster_antigonia
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_antigonia)

ValleyDepth <- raster_antigonia
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Antigonia_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_antigonia)

SummitDepth <- raster_antigonia
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Antigonia_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_antigonia)

Height <- raster_antigonia
names(Height) <- c("Height")
Height <- setValues(Height$Height, Antigonia_Height)
Height <- mask(Height, buffer_antigonia)

SummitAreaKm2 <- raster_antigonia
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Antigonia_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_antigonia)

SummitRugosity <- raster_antigonia
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Antigonia_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_antigonia)

raster_antigonia <- stack(c(raster_antigonia, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))

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

## add all the predictor variables to the raster
Habitat <- raster_argo
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_argo)

ValleyDepth <- raster_argo
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Argo_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_argo)

SummitDepth <- raster_argo
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Argo_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_argo)

Height <- raster_argo
names(Height) <- c("Height")
Height <- setValues(Height$Height, Argo_Height)
Height <- mask(Height, buffer_argo)

SummitAreaKm2 <- raster_argo
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Argo_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_argo)

SummitRugosity <- raster_argo
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Argo_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_argo)

raster_argo <- stack(c(raster_argo, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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
names(raster_capel) <- c("BottomDepth")

## add all the predictor variables to the raster
Habitat <- raster_capel
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_capel)

ValleyDepth <- raster_capel
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Capel_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_capel)

SummitDepth <- raster_capel
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Capel_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_capel)

Height <- raster_capel
names(Height) <- c("Height")
Height <- setValues(Height$Height, Capel_Height)
Height <- mask(Height, buffer_capel)

SummitAreaKm2 <- raster_capel
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Capel_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_capel)

SummitRugosity <- raster_capel
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Capel_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_capel)

raster_capel <- stack(c(raster_capel, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_crypthelia
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_crypthelia)

ValleyDepth <- raster_crypthelia
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Crypthelia_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_crypthelia)

SummitDepth <- raster_crypthelia
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Crypthelia_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_crypthelia)

Height <- raster_crypthelia
names(Height) <- c("Height")
Height <- setValues(Height$Height, Crypthelia_Height)
Height <- mask(Height, buffer_crypthelia)

SummitAreaKm2 <- raster_crypthelia
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Crypthelia_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_crypthelia)

SummitRugosity <- raster_crypthelia
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Crypthelia_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_crypthelia)

raster_crypthelia <- stack(c(raster_crypthelia, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))



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

## add all the predictor variables to the raster
Habitat <- raster_eponge
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 3)
Habitat <- mask(Habitat, buffer_eponge)

ValleyDepth <- raster_eponge
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Eponge_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_eponge)

SummitDepth <- raster_eponge
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Eponge_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_eponge)

Height <- raster_eponge
names(Height) <- c("Height")
Height <- setValues(Height$Height, Eponge_Height)
Height <- mask(Height, buffer_eponge)

SummitAreaKm2 <- raster_eponge
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Eponge_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_eponge)

SummitRugosity <- raster_eponge
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Eponge_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_eponge)

raster_eponge <- stack(c(raster_eponge, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))

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

buffer_fairway <- buffer(Fairway_poly, width=0.05)
plot(buffer_fairway, add=T)

fairway_extent <- mask(Bathy_100, buffer_fairway)
raster_fairway <- trim(fairway_extent, values=NA)
plot(raster_fairway)
names(raster_fairway) <- c("BottomDepth")

## add all the predictor variables to the raster
Habitat <- raster_fairway
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_fairway)

ValleyDepth <- raster_fairway
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Fairway_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_fairway)

SummitDepth <- raster_fairway
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Fairway_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_fairway)

Height <- raster_fairway
names(Height) <- c("Height")
Height <- setValues(Height$Height, Fairway_Height)
Height <- mask(Height, buffer_fairway)

SummitAreaKm2 <- raster_fairway
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Fairway_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_fairway)

SummitRugosity <- raster_fairway
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Fairway_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_fairway)

raster_fairway <- stack(c(raster_fairway, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))

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

## add all the predictor variables to the raster
Habitat <- raster_iledespins
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 3)
Habitat <- mask(Habitat, buffer_iledespins)

ValleyDepth <- raster_iledespins
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, IleDesPins_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_iledespins)

SummitDepth <- raster_iledespins
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, IleDesPins_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_iledespins)

Height <- raster_iledespins
names(Height) <- c("Height")
Height <- setValues(Height$Height, IleDesPins_Height)
Height <- mask(Height, buffer_iledespins)

SummitAreaKm2 <- raster_iledespins
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, IleDesPins_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_iledespins)

SummitRugosity <- raster_iledespins
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, IleDesPins_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_iledespins)

raster_iledespins <- stack(c(raster_iledespins, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_jumeauouest
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_jumeauouest)

ValleyDepth <- raster_jumeauouest
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, JumeauOuest_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_jumeauouest)

SummitDepth <- raster_jumeauouest
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, JumeauOuest_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_jumeauouest)

Height <- raster_jumeauouest
names(Height) <- c("Height")
Height <- setValues(Height$Height, JumeauOuest_Height)
Height <- mask(Height, buffer_jumeauouest)

SummitAreaKm2 <- raster_jumeauouest
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, JumeauOuest_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_jumeauouest)

SummitRugosity <- raster_jumeauouest
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, JumeauOuest_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_jumeauouest)

raster_jumeauouest <- stack(c(raster_jumeauouest, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))

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

## add all the predictor variables to the raster
Habitat <- raster_kaimonmaru
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_kaimonmaru)

ValleyDepth <- raster_kaimonmaru
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, KaimonMaru_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_kaimonmaru)

SummitDepth <- raster_kaimonmaru
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, KaimonMaru_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_kaimonmaru)

Height <- raster_kaimonmaru
names(Height) <- c("Height")
Height <- setValues(Height$Height, KaimonMaru_Height)
Height <- mask(Height, buffer_kaimonmaru)

SummitAreaKm2 <- raster_kaimonmaru
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, KaimonMaru_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_kaimonmaru)

SummitRugosity <- raster_kaimonmaru
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, KaimonMaru_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_kaimonmaru)

raster_kaimonmaru <- stack(c(raster_kaimonmaru, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_nova
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_nova)

ValleyDepth <- raster_nova
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Nova_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_nova)

SummitDepth <- raster_nova
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Nova_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_nova)

Height <- raster_nova
names(Height) <- c("Height")
Height <- setValues(Height$Height, Nova_Height)
Height <- mask(Height, buffer_nova)

SummitAreaKm2 <- raster_nova
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Nova_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_nova)

SummitRugosity <- raster_nova
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Nova_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_nova)

raster_nova <- stack(c(raster_nova, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))



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

## add all the predictor variables to the raster
Habitat <- raster_stylaster
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 3)
Habitat <- mask(Habitat, buffer_stylaster)

ValleyDepth <- raster_stylaster
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Stylaster_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_stylaster)

SummitDepth <- raster_stylaster
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Stylaster_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_stylaster)

Height <- raster_stylaster
names(Height) <- c("Height")
Height <- setValues(Height$Height, Stylaster_Height)
Height <- mask(Height, buffer_stylaster)

SummitAreaKm2 <- raster_stylaster
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Stylaster_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_stylaster)

SummitRugosity <- raster_stylaster
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Stylaster_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_stylaster)

raster_stylaster <- stack(c(raster_stylaster, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))



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

## add all the predictor variables to the raster
Habitat <- raster_torche
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_torche)

ValleyDepth <- raster_torche
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Torche_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_torche)

SummitDepth <- raster_torche
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Torche_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_torche)

Height <- raster_torche
names(Height) <- c("Height")
Height <- setValues(Height$Height, Torche_Height)
Height <- mask(Height, buffer_torche)

SummitAreaKm2 <- raster_torche
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Torche_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_torche)

SummitRugosity <- raster_torche
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Torche_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_torche)

raster_torche <- stack(c(raster_torche, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_Chesterfield
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_Chesterfield)

ValleyDepth <- raster_Chesterfield
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Chesterfield_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_Chesterfield)

SummitDepth <- raster_Chesterfield
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Chesterfield_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_Chesterfield)

Height <- raster_Chesterfield
names(Height) <- c("Height")
Height <- setValues(Height$Height, Chesterfield_Height)
Height <- mask(Height, buffer_Chesterfield)

SummitAreaKm2 <- raster_Chesterfield
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Chesterfield_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_Chesterfield)

SummitRugosity <- raster_Chesterfield
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Chesterfield_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_Chesterfield)

raster_Chesterfield <- stack(c(raster_Chesterfield, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_JumeauEst
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 3)
Habitat <- mask(Habitat, buffer_JumeauEst)

ValleyDepth <- raster_JumeauEst
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, JumeauEst_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_JumeauEst)

SummitDepth <- raster_JumeauEst
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, JumeauEst_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_JumeauEst)

Height <- raster_JumeauEst
names(Height) <- c("Height")
Height <- setValues(Height$Height, JumeauEst_Height)
Height <- mask(Height, buffer_JumeauEst)

SummitAreaKm2 <- raster_JumeauEst
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, JumeauEst_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_JumeauEst)

SummitRugosity <- raster_JumeauEst
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, JumeauEst_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_JumeauEst)

raster_JumeauEst <- stack(c(raster_JumeauEst, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_Seamount01
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_Seamount01)

ValleyDepth <- raster_Seamount01
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Seamount01_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_Seamount01)

SummitDepth <- raster_Seamount01
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Seamount01_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_Seamount01)

Height <- raster_Seamount01
names(Height) <- c("Height")
Height <- setValues(Height$Height, Seamount01_Height)
Height <- mask(Height, buffer_Seamount01)

SummitAreaKm2 <- raster_Seamount01
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Seamount01_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_Seamount01)

SummitRugosity <- raster_Seamount01
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Seamount01_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_Seamount01)

raster_Seamount01 <- stack(c(raster_Seamount01, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))

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

## add all the predictor variables to the raster
Habitat <- raster_Seamount02
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_Seamount02)

ValleyDepth <- raster_Seamount02
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Seamount02_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_Seamount02)

SummitDepth <- raster_Seamount02
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Seamount02_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_Seamount02)

Height <- raster_Seamount02
names(Height) <- c("Height")
Height <- setValues(Height$Height, Seamount02_Height)
Height <- mask(Height, buffer_Seamount02)

SummitAreaKm2 <- raster_Seamount02
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Seamount02_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_Seamount02)

SummitRugosity <- raster_Seamount02
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Seamount02_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_Seamount02)

raster_Seamount02 <- stack(c(raster_Seamount02, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))

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

## add all the predictor variables to the raster
Habitat <- raster_Seamount03
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 1)
Habitat <- mask(Habitat, buffer_Seamount03)

ValleyDepth <- raster_Seamount03
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Seamount03_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_Seamount03)

SummitDepth <- raster_Seamount03
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Seamount03_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_Seamount03)

Height <- raster_Seamount03
names(Height) <- c("Height")
Height <- setValues(Height$Height, Seamount03_Height)
Height <- mask(Height, buffer_Seamount03)

SummitAreaKm2 <- raster_Seamount03
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Seamount03_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_Seamount03)

SummitRugosity <- raster_Seamount03
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Seamount03_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_Seamount03)

raster_Seamount03 <- stack(c(raster_Seamount03, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


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

## add all the predictor variables to the raster
Habitat <- raster_Seamount04
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 2)
Habitat <- mask(Habitat, buffer_Seamount04)

ValleyDepth <- raster_Seamount04
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Seamount04_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_Seamount04)

SummitDepth <- raster_Seamount04
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Seamount04_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_Seamount04)

Height <- raster_Seamount04
names(Height) <- c("Height")
Height <- setValues(Height$Height, Seamount04_Height)
Height <- mask(Height, buffer_Seamount04)

SummitAreaKm2 <- raster_Seamount04
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Seamount04_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_Seamount04)

SummitRugosity <- raster_Seamount04
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Seamount04_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_Seamount04)

raster_Seamount04 <- stack(c(raster_Seamount04, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))



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

## add all the predictor variables to the raster
Habitat <- raster_Introuvable
names(Habitat) <- c("Habitat")
Habitat <- setValues(Habitat$Habitat, 3)
Habitat <- mask(Habitat, buffer_Introuvable)

ValleyDepth <- raster_Introuvable
names(ValleyDepth) <- c("ValleyDepth")
ValleyDepth <- setValues(ValleyDepth$ValleyDepth, Introuvable_ValleyDepth)
ValleyDepth <- mask(ValleyDepth, buffer_Introuvable)

SummitDepth <- raster_Introuvable
names(SummitDepth) <- c("SummitDepth")
SummitDepth <- setValues(SummitDepth$SummitDepth, Introuvable_SummitDepth)
SummitDepth <- mask(SummitDepth, buffer_Introuvable)

Height <- raster_Introuvable
names(Height) <- c("Height")
Height <- setValues(Height$Height, Introuvable_Height)
Height <- mask(Height, buffer_Introuvable)

SummitAreaKm2 <- raster_Introuvable
names(SummitAreaKm2) <- c("SummitAreaKm2")
SummitAreaKm2 <- setValues(SummitAreaKm2$SummitAreaKm2, Introuvable_SummitAreaKm2)
SummitAreaKm2 <- mask(SummitAreaKm2, buffer_Introuvable)

SummitRugosity <- raster_Introuvable
names(SummitRugosity) <- c("SummitRugosity")
SummitRugosity <- setValues(SummitRugosity$SummitRugosity, Introuvable_SummitRugosity)
SummitRugosity <- mask(SummitRugosity, buffer_Introuvable)

raster_Introuvable <- stack(c(raster_Introuvable, Habitat, ValleyDepth, SummitDepth, Height, SummitAreaKm2, SummitRugosity))


#####################################################################################################################################
## Merge all rasters into one

raster_list <- list(raster_antigonia, raster_argo, raster_capel, raster_Chesterfield, raster_crypthelia, raster_eponge, raster_fairway,
                    raster_iledespins, raster_Introuvable, raster_JumeauEst, raster_jumeauouest, raster_kaimonmaru, raster_nova,
                    raster_Seamount01, raster_Seamount02, raster_Seamount03, raster_Seamount04, raster_stylaster, raster_torche)

raster_seamounts <- do.call(merge, raster_list)
names(raster_seamounts) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")
plot(raster_seamounts$Depth)

raster_seamounts <- rast(raster_seamounts)
names(raster_seamounts) <- c("BottomDepth","Habitat","ValleyDepth","SummitDepth","Height","SummitAreaKm2","SummitRugosity")

writeRaster(raster_seamounts, filename = "02_formating_data/00_Prediction_raster/raster_seamounts.tif")


buffer_seamounts <- (buffer_antigonia+buffer_argo+buffer_capel+buffer_Chesterfield+buffer_crypthelia+buffer_eponge+buffer_fairway+
                       buffer_iledespins+buffer_Introuvable+buffer_JumeauEst+buffer_jumeauouest+buffer_kaimonmaru+buffer_nova+
                       buffer_Seamount01+buffer_Seamount02+buffer_Seamount03+buffer_Seamount04+buffer_stylaster+buffer_torche)
save(buffer_seamounts, file = "02_formating_data/00_Prediction_raster/buffer_seamounts.rdata")

df_seamounts <- as.data.frame(raster_seamounts, xy=TRUE)

save(df_seamounts, file="02_formating_data/00_Prediction_raster/Rdata/df_seamounts.rdata")


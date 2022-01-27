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

# World GEBCO Bathymetry at 500 m resolution (download 2021)
Bathy_GEBCO=raster("data/environmental/Bathymetrie_GEBCO_2020_19_Jul_2021_56faf792d27a/gebco_2020_n-14.0_s-27.0_w155.0_e175.0.tif")
Bathy_GEBCO

# NC BATHYMETRY MNT at 500 m resolution (2009)
Bathy_500_LatLong=raster("data/environmental/bathytopoMNT500m/Bathy_500_LatLong.tif")
Bathy_500_LatLong

# NC Bathymetry at 100 m resolution (2020 - given by Jean Roger)
Bathy_100=raster("data/environmental/bathytopoMNT100m/MNT-nettoye_v3_FINAL.tif")
Bathy_100

plot(Bathy_500_LatLong)
plot(Bathy_GEBCO)
plot(Bathy_100)

## read eDNA stations
metaDna <- read_excel("data/Samples/SPYGEN_eDNA_Field_work_To_return_to_SPYGEN _All_Samples.xlsx", sheet = "NoBlanc")
metaDna=as.data.frame(metaDna)
metaDna=metaDna[,c(1,2,3,5,14,13,21,22,24,7)]
metaDna=metaDna[metaDna$Habitat=="Sommet" | metaDna$Habitat=="Pelagique",]
xy <- metaDna[,c(5,6)]
xy$Longitude_start=as.numeric(xy$Longitude_start)
xy$Latitude_start=as.numeric(xy$Latitude_start)
DNApoint <- SpatialPointsDataFrame(coords = xy, data = metaDna,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

########################################
############ Nova 
# read extent
Nova_Extent=extent(158.9,159.62,-23.0,-22.05)
Nova_Depth=crop(Bathy_100,Nova_Extent)

plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(DNApoint,add=TRUE)

metaDna$DepthMNT=round(extract(Nova_Depth,xy))*(-1)
metaDna[metaDna$Site_name=="Nova",] #### les prof sommets sont ok, et les profs pelagique aussi

Nova_SummitDepth=round(max(values(Nova_Depth),na.rm=TRUE))
Nova_BottomDepth=round(min(values(Nova_Depth),na.rm=TRUE))
Nova_Height=Nova_SummitDepth-Nova_BottomDepth
Nova_SummitDepth
Nova_BottomDepth
Nova_Height   ###### Aucune station adne disponible

# aucune correction de profondeur
#values(Nova_Depth)[values(Nova_Depth) > -300] = round(runif(1000,299,313),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 300m

Nova_SummitDepth=round(max(values(Nova_Depth),na.rm=TRUE))
Nova_BottomDepth=round(min(values(Nova_Depth),na.rm=TRUE))
Nova_Height=Nova_SummitDepth-Nova_BottomDepth
Nova_SummitDepth
Nova_BottomDepth
Nova_Height

plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(DNApoint,add=TRUE)

metaDna$DepthMNT=round(extract(Nova_Depth,xy))*(-1)
metaDna[metaDna$Site_name=="Nova",] #### les prof sommets sont ok, et les profs pelagique aussi

####SUMMIT
Nova_Summit_Poly=Nova_Depth
values(Nova_Summit_Poly)[values(Nova_Summit_Poly) < Nova_SummitDepth+30/100*Nova_SummitDepth] = NA
plot(Nova_Summit_Poly)
Nova_SummitRugosity=sd(values(Nova_Summit_Poly),na.rm=TRUE)
Nova_SummitRugosity
values(Nova_Summit_Poly)[!is.na(values(Nova_Summit_Poly))] <- 1
plot(Nova_Summit_Poly)
Nova_Summit_Poly=rasterToPolygons(Nova_Summit_Poly, dissolve=TRUE)
Nova_SummitArea=area(Nova_Summit_Poly)*1e-6
Nova_SummitArea
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Nova_Slope_Poly=Nova_Depth
values(Nova_Slope_Poly)[values(Nova_Slope_Poly) > Nova_SummitDepth+30/100*Nova_SummitDepth | values(Nova_Slope_Poly) < Nova_BottomDepth-30/100*Nova_BottomDepth] = NA
plot(Nova_Slope_Poly)
values(Nova_Slope_Poly)[!is.na(values(Nova_Slope_Poly))] <- 1
plot(Nova_Slope_Poly)
Nova_Slope_Poly=rasterToPolygons(Nova_Slope_Poly, dissolve=TRUE)
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(DNApoint,add=TRUE)

#####################################

### ENVIRONMENT
#SSTmean
SSTmean1k=raster("data/environmental/GRHSST/SSTmean1k.tif")

#SSTmin
SSTmin1k=raster("data/environmental/GRHSST/SSTmin1k.tif")

#SSTmax
SSTmax1k=raster("data/environmental/GRHSST/SSTmax1k.tif")

#SSTsd
SSTsd1k=raster("data/environmental/GRHSST/SSTsd1k.tif")

#ChlorA
ChlorA=raster("data/environmental/ChlorA/ChlorAmean4k.tif")

#EastwardVelocity
EastwardVelocity=raster("data/environmental/EastwardVelocity/EastwardVelocity_Surface_mean_8k.tif")

#NorthwardVelocity
NorthwardVelocity=raster("data/environmental/NorthwardVelocity/NorthwardVelocity_Surface_mean_8k.tif")

#Salinity
Salinity=raster("data/environmental/Salinity/Salinity_Surface_mean_8k.tif")


#SeaFloorPotentialTemperature
SeaFloorPotentialTemperature=raster("data/environmental/SeaFloorPotentialTemperature/SeafloorTemp_Surface_mean_8k.tif")


#SuspendedParticulateMatter
SuspendedParticulateMatter=raster("data/environmental/SuspendedParticulateMatter/SuspMattermean4k.tif")


#Geomorphology (land and reef)
Geomorphology=shapefile("data/environmental/NewCaledoniaMilleniumGeomorphology/NewCaledonia_v8.shp")

head(Geomorphology@data)
unique(Geomorphology@data$REEF)
unique(Geomorphology@data$L1_CODE)
unique(Geomorphology@data$L1_ATTRIB)

Reef <- Geomorphology[Geomorphology@data$REEF == 1, ]
head(Reef@data)
unique(Reef@data$REEF)
#plot(Reef)

Land <- Geomorphology[Geomorphology@data$L1_CODE == 2, ]
head(Land@data)
unique(Land@data$L1_CODE)
unique(Land@data$L1_ATTRIB)
#plot(Land)


############################################
##ACOUSTIC DATA

var_ncdf1 <- nc_open("data/acoustique/NetCDF_Seamount/Seamount01/IRD_SOOP-BA_A_20190403T234037Z_ALIS_FV02_Seamounts-38-70-120-200_END-20190418T201633Z_C-20210720T111138Z.nc")
var_ncdf2 <- nc_open("data/acoustique/NetCDF_Seamount/Seamount02/IRD_SOOP-BA_A_20190604T035253Z_ALIS_FV02_Seamounts-38-70-120-200_END-20190620T200951Z_C-20210720T111556Z.nc")
var_ncdf3 <- nc_open("data/acoustique/NetCDF_Seamount/Seamount03/IRD_SOOP-BA_A_20200804T095959Z_ALIS_FV02_Seamounts-38-70-120-200_END-20200824T052724Z_C-20210720T111913Z.nc")
var_ncdf4 <- nc_open("data/acoustique/NetCDF_Seamount/Seamount04/IRD_SOOP-BA_A_20200905T191244Z_ALIS_FV02_Seamounts-38-70-120-200_END-20200925T091621Z_C-20210720T112230Z.nc")


#names(var_ncdf$var)
#var_ncdf

# get variables of interest
ncvar1 = ncvar_get(var_ncdf1, 'sA') ###nautical_area_scattering_coefficient, units: m2 nmi-2
ncvar2 = ncvar_get(var_ncdf2, 'sA') ###nautical_area_scattering_coefficient, units: m2 nmi-2
ncvar3 = ncvar_get(var_ncdf3, 'sA') ###nautical_area_scattering_coefficient, units: m2 nmi-2
ncvar4 = ncvar_get(var_ncdf4, 'sA') ###nautical_area_scattering_coefficient, units: m2 nmi-2

#ncvar38Khz=ncvar[,,1]#### channel 1: 38Khz, 2: 70 Khz, 3: 120 Khz, 4: 200 Khz
nclat1 <- ncvar_get(var_ncdf1, 'latitude')
nclon1 <- ncvar_get(var_ncdf1, 'longitude')
ncday1 <- ncvar_get(var_ncdf1, 'day')     #  1 - Night, 2 - Sunrise, 3 - Day, 4 - Sunset (UTC)
nctime1 = ncvar_get(var_ncdf1, 'time')    #days since 1950-01-01 00:00:00 UTC
ncdepth1=ncvar_get(var_ncdf1, 'depth')

nclat2 <- ncvar_get(var_ncdf2, 'latitude')
nclon2 <- ncvar_get(var_ncdf2, 'longitude')
ncday2 <- ncvar_get(var_ncdf2, 'day')     #  1 - Night, 2 - Sunrise, 3 - Day, 4 - Sunset (UTC)
nctime2 = ncvar_get(var_ncdf2, 'time')    #days since 1950-01-01 00:00:00 UTC
ncdepth2=ncvar_get(var_ncdf2, 'depth')

nclat3 <- ncvar_get(var_ncdf3, 'latitude')
nclon3 <- ncvar_get(var_ncdf3, 'longitude')
ncday3 <- ncvar_get(var_ncdf3, 'day')     #  1 - Night, 2 - Sunrise, 3 - Day, 4 - Sunset (UTC)
nctime3 = ncvar_get(var_ncdf3, 'time')    #days since 1950-01-01 00:00:00 UTC
ncdepth3=ncvar_get(var_ncdf3, 'depth')

nclat4 <- ncvar_get(var_ncdf4, 'latitude')
nclon4 <- ncvar_get(var_ncdf4, 'longitude')
ncday4 <- ncvar_get(var_ncdf4, 'day')     #  1 - Night, 2 - Sunrise, 3 - Day, 4 - Sunset (UTC)
nctime4 = ncvar_get(var_ncdf4, 'time')    #days since 1950-01-01 00:00:00 UTC
ncdepth4=ncvar_get(var_ncdf4, 'depth')


# combine data of the 4 seamounts campaigns - and select only channel 1, that is 38kHz; on s'arrete à 795 m de prof, car le nb de couches est different entre les 4 fichiers campagne
dim(ncvar1[,,1])
dim(ncvar2[,,1])
dim(ncvar3[,,1])
dim(ncvar4[,,1])

ncvar=cbind(ncvar1[1:795,,1],ncvar2[1:795,,1],ncvar3[1:795,,1],ncvar4[1:795,,1])
nclat=c(as.vector(nclat1),as.vector(nclat2),as.vector(nclat3),as.vector(nclat4))
nclon=c(as.vector(nclon1),as.vector(nclon2),as.vector(nclon3),as.vector(nclon4))
ncday=c(as.vector(ncday1),as.vector(ncday2),as.vector(ncday3),as.vector(ncday4))
nctime=c(as.vector(nctime1),as.vector(nctime2),as.vector(nctime3),as.vector(nctime4))
ncdepth=c(as.vector(ncdepth1),as.vector(ncdepth2),as.vector(ncdepth3),as.vector(ncdepth4))
ncdepth=array(c(as.vector(ncdepth1[1:795]),as.vector(ncdepth2[1:795]),as.vector(ncdepth3[1:795]),as.vector(ncdepth4[1:795])),dim=c(795,4))

dim(ncvar)
length(nclat)
length(nclon)
length(ncday)
length(nctime)
dim(ncdepth)


###### TRANSFORM TIME INTO LOCAL TIME
#time is days since 1950-01-01 00:00:00 UTC
Sys.setenv(TZ='Pacific/Guadalcanal')
Sys.timezone()
UTCtime = as.POSIXct(nctime*24*60*60, origin="1950-01-01", tz="UTC")
Localtime=UTCtime
attributes(Localtime)$tzone <- "Pacific/Guadalcanal"
LocalHour=hour(Localtime)
LocalDay=ncday
LocalDay[LocalHour > 7 & LocalHour < 17]=3
LocalDay[LocalHour >= 17 & LocalHour < 20]=4
LocalDay[LocalHour >= 20 | LocalHour < 5]=1
LocalDay[LocalHour >= 5 & LocalHour <= 7]=2

#####LOOP BUILDING ACOUSTIC DATA

Site=Nova_Depth
Site
CoarseSite=aggregate(Site,fact=3,fun=mean)####on divise par 3 la resolution pour avoir 500 m resolution
CoarseSite
Site=CoarseSite


#DAY
for (p in 1:79) {
  k=p*10
  l=k-9
  if (p == 1){
    Acoustic_couche1=data.frame(x=nclon,y=nclat,sa=colMeans(ncvar[l:k,],na.rm = TRUE),time=nctime,day=ncday,Localtime=Localtime,LocalHour=LocalHour,LocalDay=LocalDay)
    Acoustic_couche1_Day=Acoustic_couche1[Acoustic_couche1$LocalDay==3,]
    rcouche1_Day = rasterize(Acoustic_couche1_Day[,c(1,2)],Site, Acoustic_couche1_Day[,c(3)], fun=mean, na.rm=TRUE)
    next
  }
  if (p == 2){
    Acoustic_couche2=data.frame(x=nclon,y=nclat,sa=colMeans(ncvar[l:k,],na.rm = TRUE),time=nctime,day=ncday,Localtime=Localtime,LocalHour=LocalHour,LocalDay=LocalDay)
    Acoustic_couche2_Day=Acoustic_couche2[Acoustic_couche2$LocalDay==3,]
    rcouche2_Day = rasterize(Acoustic_couche2_Day[,c(1,2)],Site, Acoustic_couche2_Day[,c(3)], fun=mean, na.rm=TRUE)
    Acoustic_stack_day=stack(rcouche1_Day,rcouche2_Day)
    next
  }
  Acoustic_couche=data.frame(x=nclon,y=nclat,sa=colMeans(ncvar[l:k,],na.rm = TRUE),time=nctime,day=ncday,Localtime=Localtime,LocalHour=LocalHour,LocalDay=LocalDay)
  Acoustic_couche_Day=Acoustic_couche[Acoustic_couche$LocalDay==3,]
  rcouche_Day = rasterize(Acoustic_couche_Day[,c(1,2)],Site, Acoustic_couche_Day[,c(3)], fun=mean, na.rm=TRUE)
  Acoustic_stack_day=stack(Acoustic_stack_day,rcouche_Day)
  
}
names(Acoustic_stack_day)=paste("Depth",seq(from=10,to=790,by=10),sep="_")
Acoustic_stack_day
nlayers(Acoustic_stack_day)


#NIGHT
for (p in 1:79) {
  k=p*10
  l=k-9
  if (p == 1){
    Acoustic_couche1=data.frame(x=nclon,y=nclat,sa=colMeans(ncvar[l:k,],na.rm = TRUE),time=nctime,day=ncday,Localtime=Localtime,LocalHour=LocalHour,LocalDay=LocalDay)
    Acoustic_couche1_Night=Acoustic_couche1[Acoustic_couche1$LocalDay==1,]
    rcouche1_Night = rasterize(Acoustic_couche1_Night[,c(1,2)],Site, Acoustic_couche1_Night[,c(3)], fun=mean, na.rm=TRUE)
    next
  }
  if (p == 2){
    Acoustic_couche2=data.frame(x=nclon,y=nclat,sa=colMeans(ncvar[l:k,],na.rm = TRUE),time=nctime,day=ncday,Localtime=Localtime,LocalHour=LocalHour,LocalDay=LocalDay)
    Acoustic_couche2_Night=Acoustic_couche2[Acoustic_couche2$LocalDay==1,]
    rcouche2_Night = rasterize(Acoustic_couche2_Night[,c(1,2)],Site, Acoustic_couche2_Night[,c(3)], fun=mean, na.rm=TRUE)
    Acoustic_stack_Night=stack(rcouche1_Night,rcouche2_Night)
    next
  }
  Acoustic_couche=data.frame(x=nclon,y=nclat,sa=colMeans(ncvar[l:k,],na.rm = TRUE),time=nctime,day=ncday,Localtime=Localtime,LocalHour=LocalHour,LocalDay=LocalDay)
  Acoustic_couche_Night=Acoustic_couche[Acoustic_couche$LocalDay==1,]
  rcouche_Night = rasterize(Acoustic_couche_Night[,c(1,2)],Site, Acoustic_couche_Night[,c(3)], fun=mean, na.rm=TRUE)
  Acoustic_stack_Night=stack(Acoustic_stack_Night,rcouche_Night)
  
}
names(Acoustic_stack_Night)=paste("Depth",seq(from=10,to=790,by=10),sep="_")
Acoustic_stack_Night
nlayers(Acoustic_stack_Night)

########

plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
#plot(DNApoint,add=TRUE)
plot(Acoustic_stack_Night[[2]], add=TRUE)
plot(Acoustic_stack_day[[2]], add=TRUE)


########     Nova NIGHT SLOPE ACOUSTIC
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Nova_Slope_Poly),add=TRUE)

Acoustic_Night_Nova_Slope=as.data.frame(mask(Acoustic_stack_Night,Nova_Slope_Poly),xy=TRUE)
Acoustic_Night_Nova_Slope=Acoustic_Night_Nova_Slope[rowSums(is.na(Acoustic_Night_Nova_Slope)) != ncol(Acoustic_Night_Nova_Slope)-2,]
Acoustic_Night_Nova_Slope$Site="Nova"
Acoustic_Night_Nova_Slope$Day="Night"
Acoustic_Night_Nova_Slope$Habitat="Slope"

#View(Acoustic_Night_Nova_Slope)
head(Acoustic_Night_Nova_Slope)
#############


########     Nova NIGHT SUMMIT ACOUSTIC
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Nova_Summit_Poly),add=TRUE)

Acoustic_Night_Nova_Summit=as.data.frame(mask(Acoustic_stack_Night,Nova_Summit_Poly),xy=TRUE)
Acoustic_Night_Nova_Summit=Acoustic_Night_Nova_Summit[rowSums(is.na(Acoustic_Night_Nova_Summit)) != ncol(Acoustic_Night_Nova_Summit)-2,]
Acoustic_Night_Nova_Summit$Site="Nova"
Acoustic_Night_Nova_Summit$Day="Night"
Acoustic_Night_Nova_Summit$Habitat="Summit"

#View(Acoustic_Night_Nova_Summit)
head(Acoustic_Night_Nova_Summit)
#############

########     Nova DAY SLOPE ACOUSTIC
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Nova_Slope_Poly),add=TRUE)

Acoustic_Day_Nova_Slope=as.data.frame(mask(Acoustic_stack_day,Nova_Slope_Poly),xy=TRUE)
Acoustic_Day_Nova_Slope=Acoustic_Day_Nova_Slope[rowSums(is.na(Acoustic_Day_Nova_Slope)) != ncol(Acoustic_Day_Nova_Slope)-2,]
Acoustic_Day_Nova_Slope$Site="Nova"
Acoustic_Day_Nova_Slope$Day="Day"
Acoustic_Day_Nova_Slope$Habitat="Slope"

#View(Acoustic_Day_Nova_Slope)
head(Acoustic_Day_Nova_Slope)
#############

########     Nova DAY SUMMIT ACOUSTIC
plot(Nova_Depth)
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Nova_Summit_Poly),add=TRUE)

Acoustic_Day_Nova_Summit=as.data.frame(mask(Acoustic_stack_day,Nova_Summit_Poly),xy=TRUE)
Acoustic_Day_Nova_Summit=Acoustic_Day_Nova_Summit[rowSums(is.na(Acoustic_Day_Nova_Summit)) != ncol(Acoustic_Day_Nova_Summit)-2,]
Acoustic_Day_Nova_Summit$Site="Nova"
Acoustic_Day_Nova_Summit$Day="Day"
Acoustic_Day_Nova_Summit$Habitat="Summit"

#View(Acoustic_Day_Nova_Summit)
head(Acoustic_Day_Nova_Summit)



###########################
#####   PUT TOGETHER ACOUSTIC DATA FOR SITE
###########################

##put together acoustic data for the site
Nova_Acoustic=rbind(Acoustic_Night_Nova_Slope,Acoustic_Night_Nova_Summit,Acoustic_Day_Nova_Slope,Acoustic_Day_Nova_Summit)

#add bathymetric data
Nova_Acoustic$SummitDepth=Nova_SummitDepth*(-1)
Nova_Acoustic$ValleyDepth=Nova_BottomDepth*(-1)
Nova_Acoustic$Height=Nova_Height
Nova_Acoustic$SummitAreasKm2=Nova_SummitArea
Nova_Acoustic$SummitRugosity=Nova_SummitRugosity
Nova_Acoustic$BottomDepth=extract(Nova_Depth,Nova_Acoustic[c(1,2)])*(-1)
View(Nova_Acoustic)
head(Nova_Acoustic)

# correct bad bottom depth for depth smaller than 800 m (maximum measured by EK60)
#head(Nova_Acoustic)
#cbind(names(Nova_Acoustic))

Nova_Acoustic2=Nova_Acoustic[,c(3:81)]
#cbind(names(Nova_Acoustic2))
head(Nova_Acoustic2)

# affecte une profondeur lorsque mesurable par EK60
for (i in 1:nrow(Nova_Acoustic2)) {
  for (p in 1:ncol(Nova_Acoustic2)) {
    if (sum(Nova_Acoustic2[i,c(p:79)],na.rm = TRUE)==0) {
      Nova_Acoustic$BottomDepth[i]=(p-1)*10
      break
    } 
  }
}


# affecte une profondeur lorsque non-mesurable par EK60
for (i in 1:nrow(Nova_Acoustic2)) {
    if (Nova_Acoustic2[i,79]!="NaN" & Nova_Acoustic$BottomDepth[i]<790) {
      Nova_Acoustic$BottomDepth[i]=mean(Nova_Acoustic$BottomDepth[Nova_Acoustic$BottomDepth > 790])
      next
  }
}


# reaffecte les habitats sommets et pente en fonction des profondeurs corrigees

Nova_Acoustic$Habitat[Nova_Acoustic$BottomDepth<=(Nova_SummitDepth+30/100*Nova_SummitDepth)*(-1)]="Summit"
Nova_Acoustic$Habitat[Nova_Acoustic$BottomDepth>(Nova_SummitDepth+30/100*Nova_SummitDepth)*(-1) &  
                      Nova_Acoustic$BottomDepth<=(Nova_BottomDepth-30/100*Nova_BottomDepth)*(-1)]="Slope"

# verifie qu'il n'y a aucune profondeur superieure a la limite infeieure de la pente
Nova_Acoustic[Nova_Acoustic$BottomDepth>(Nova_BottomDepth-30/100*Nova_BottomDepth)*(-1),]

#sample size for analysis at site
table(Nova_Acoustic$Habitat,Nova_Acoustic$Day)

# raster layer of surface acoustic data at site
Nova_Acoustic_Surface_raster=rasterFromXYZ(cbind(Nova_Acoustic$x,Nova_Acoustic$y,Nova_Acoustic$Depth_10),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Nova_Acoustic_RealSummit_Surface_raster=rasterFromXYZ(cbind(Nova_Acoustic$x[Nova_Acoustic$Habitat=="Summit"],Nova_Acoustic$y[Nova_Acoustic$Habitat=="Summit"],Nova_Acoustic$Depth_10[Nova_Acoustic$Habitat=="Summit"]),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Nova_Acoustic_RealSlope_Surface_raster=rasterFromXYZ(cbind(Nova_Acoustic$x[Nova_Acoustic$Habitat=="Slope"],Nova_Acoustic$y[Nova_Acoustic$Habitat=="Slope"],Nova_Acoustic$Depth_10[Nova_Acoustic$Habitat=="Slope"]),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



# add environmental data
head(Nova_Acoustic)
names(Nova_Acoustic)
Nova_Acoustic$SSTmean1k=extract(SSTmean1k,Nova_Acoustic[c(1,2)])
Nova_Acoustic$SSTmin1k=extract(SSTmin1k,Nova_Acoustic[c(1,2)])
Nova_Acoustic$SSTmax1k=extract(SSTmax1k,Nova_Acoustic[c(1,2)])
Nova_Acoustic$SSTsd1k=extract(SSTsd1k,Nova_Acoustic[c(1,2)])
Nova_Acoustic$ChlorA=extract(ChlorA,Nova_Acoustic[c(1,2)])
Nova_Acoustic$EastwardVelocity=extract(EastwardVelocity,Nova_Acoustic[c(1,2)])
Nova_Acoustic$NorthwardVelocity=extract(NorthwardVelocity,Nova_Acoustic[c(1,2)])
Nova_Acoustic$Salinity=extract(Salinity,Nova_Acoustic[c(1,2)])
Nova_Acoustic$SeaFloorPotentialTemperature=extract(SeaFloorPotentialTemperature,Nova_Acoustic[c(1,2)])
Nova_Acoustic$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,Nova_Acoustic[c(1,2)])

Nova_pnts=Nova_Acoustic[,c(1,2)]
dist.Reef = geosphere::dist2Line(Nova_pnts, Reef)
dist.Land = geosphere::dist2Line(Nova_pnts, Land)
Nova_Acoustic$ShortestDistanceReef=dist.Reef[,1]
Nova_Acoustic$ShortestDistanceLand=dist.Land[,1]



############# ajoute une couche acoustique surface (0-20 m) et fond (20 m au dessus du fond)

#surface 0-20m
Nova_Acoustic$AcousticSurface=rowMeans(Nova_Acoustic[,c(3:4)],na.rm = TRUE)


# fond: calcule la moyenne du signal acoustique pour les 2 dernieres couches au dessus du fond (20m) lorsque fond mesurable par EK60
Nova_Acoustic2=Nova_Acoustic[,c(3:81)]
Nova_Acoustic$AcousticFond="NaN"


for (i in 1:nrow(Nova_Acoustic2)) {
  for (p in 1:ncol(Nova_Acoustic2)) {
    k=p-2+2
    l=p-1+2
    if (sum(Nova_Acoustic2[i,c(p:79)],na.rm = TRUE)==0 & k==2) {
      Nova_Acoustic$AcousticFond[i]=Nova_Acoustic[i,l]
    }else if (sum(Nova_Acoustic2[i,c(p:79)],na.rm = TRUE)==0 & k!=2) {
      Nova_Acoustic$AcousticFond[i]=rowMeans(Nova_Acoustic[i,c(k:l)],na.rm = TRUE)
      break
    }
  }
}




#check final dataset
names(Nova_Acoustic)
head(Nova_Acoustic)
unique(Nova_Acoustic$Habitat)
unique(Nova_Acoustic$Day)
range(as.numeric(Nova_Acoustic$AcousticFond),na.rm=TRUE)
range(as.numeric(Nova_Acoustic$AcousticSurface),na.rm=TRUE)
table(Nova_Acoustic$Day,Nova_Acoustic$Habitat)
View(Nova_Acoustic)


# EXPORT DATA & plots
getwd()

#export directory
dir.exists("Nova_Output")
dir.create("Nova_Output")

write_xlsx(list(Nova = Nova_Acoustic),path="Nova_Output/Nova_Acoustic_Data.xlsx")
writeRaster(Nova_Depth, filename="Nova_Output/Nova_Depth.tif", format="GTiff", overwrite=TRUE)
writeRaster(Nova_Acoustic_Surface_raster, filename="Nova_Output/Nova_Acoustic_Surface_raster.tif", format="GTiff", overwrite=TRUE)

path=getwd()
setwd(paste(path,"/Nova_Output",sep=""))
getwd()
writeOGR(Nova_Slope_Poly, dsn = '.', layer = 'Nova_Slope_Poly', driver = "ESRI Shapefile")
writeOGR(Nova_Summit_Poly, dsn = '.', layer = 'Nova_Summit_Poly', driver = "ESRI Shapefile")

setwd(path)
getwd()

###### Plot data without showing error in depth summit/slope
plot(Nova_Depth,main="Nova")
contour(Nova_Depth,add=TRUE)
plot(Nova_Acoustic_Surface_raster,col="lavender",add=TRUE,legend=FALSE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)

dev.print(device = png, file = "Nova_Output/Nova_Map_Acoustic.png", width = 600)



###Plot real slope and summit

plot(Nova_Depth,main="Nova")
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(Nova_Acoustic_RealSummit_Surface_raster,col="magenta",add=TRUE,legend=FALSE)
plot(Nova_Acoustic_RealSlope_Surface_raster,col="blue",add=TRUE,legend=FALSE)

dev.print(device = png, file = "Nova_Output/Nova_Map_Acoustic_RealSlopeSummit.png", width = 600)


#################
##################
#################









##########MULTIPLOT
par(mfrow=c(2,2))
plot(Nova_Depth,main="Night SLope")
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Nova_Slope_Poly),add=TRUE)

plot(Nova_Depth,main="Night Summit")
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Nova_Summit_Poly),add=TRUE)


plot(Nova_Depth, main= "Day Slope")
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Nova_Slope_Poly),add=TRUE)

plot(Nova_Depth, main = "Day Summit")
contour(Nova_Depth,add=TRUE)
plot(Nova_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Nova_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Nova_Summit_Poly),add=TRUE)







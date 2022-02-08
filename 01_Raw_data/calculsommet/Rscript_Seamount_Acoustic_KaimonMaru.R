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
############ KaimonMaru 
# read extent
KaimonMaru_Extent=extent(168.02,168.25,-24.88,-24.63)
KaimonMaru_Depth=crop(Bathy_100,KaimonMaru_Extent)

plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(DNApoint,add=TRUE)

metaDna$DepthMNT=round(extract(KaimonMaru_Depth,xy))*(-1)
metaDna[metaDna$Site_name=="KaimonMaru",] #### les prof sommets sont ok, et les profs pelagique aussi

KaimonMaru_SummitDepth=round(max(values(KaimonMaru_Depth),na.rm=TRUE))
KaimonMaru_BottomDepth=round(min(values(KaimonMaru_Depth),na.rm=TRUE))
KaimonMaru_Height=KaimonMaru_SummitDepth-KaimonMaru_BottomDepth
KaimonMaru_SummitDepth
KaimonMaru_BottomDepth
KaimonMaru_Height   ###### la prof du sommet est correcte, la campagne de terrain indiquant un mont au sommet plat a environ 240 m de fond. Nos 10 mesure edna range entre 238 et 325 m

#aucune correction de profondeur
#values(KaimonMaru_Depth)[values(KaimonMaru_Depth) > -300] = round(runif(1000,299,313),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 300m

KaimonMaru_SummitDepth=round(max(values(KaimonMaru_Depth),na.rm=TRUE))
KaimonMaru_BottomDepth=round(min(values(KaimonMaru_Depth),na.rm=TRUE))
KaimonMaru_Height=KaimonMaru_SummitDepth-KaimonMaru_BottomDepth
KaimonMaru_SummitDepth
KaimonMaru_BottomDepth
KaimonMaru_Height

plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(DNApoint,add=TRUE)

metaDna$DepthMNT=round(extract(KaimonMaru_Depth,xy))*(-1)
metaDna[metaDna$Site_name=="KaimonMaru",] #### les prof sommets sont ok, et les profs pelagique aussi

####SUMMIT
KaimonMaru_Summit_Poly=KaimonMaru_Depth
values(KaimonMaru_Summit_Poly)[values(KaimonMaru_Summit_Poly) < KaimonMaru_SummitDepth+30/100*KaimonMaru_SummitDepth] = NA
plot(KaimonMaru_Summit_Poly)
KaimonMaru_SummitRugosity=sd(values(KaimonMaru_Summit_Poly),na.rm=TRUE)
KaimonMaru_SummitRugosity
values(KaimonMaru_Summit_Poly)[!is.na(values(KaimonMaru_Summit_Poly))] <- 1
plot(KaimonMaru_Summit_Poly)
KaimonMaru_Summit_Poly=rasterToPolygons(KaimonMaru_Summit_Poly, dissolve=TRUE)
KaimonMaru_SummitArea=area(KaimonMaru_Summit_Poly)*1e-6
KaimonMaru_SummitArea
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
KaimonMaru_Slope_Poly=KaimonMaru_Depth
values(KaimonMaru_Slope_Poly)[values(KaimonMaru_Slope_Poly) > KaimonMaru_SummitDepth+30/100*KaimonMaru_SummitDepth | values(KaimonMaru_Slope_Poly) < KaimonMaru_BottomDepth-30/100*KaimonMaru_BottomDepth] = NA
plot(KaimonMaru_Slope_Poly)
values(KaimonMaru_Slope_Poly)[!is.na(values(KaimonMaru_Slope_Poly))] <- 1
plot(KaimonMaru_Slope_Poly)
KaimonMaru_Slope_Poly=rasterToPolygons(KaimonMaru_Slope_Poly, dissolve=TRUE)
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
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

Site=KaimonMaru_Depth
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

plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
#plot(DNApoint,add=TRUE)
plot(Acoustic_stack_Night[[2]], add=TRUE)
plot(Acoustic_stack_day[[2]], add=TRUE)


########     KaimonMaru NIGHT SLOPE ACOUSTIC
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],KaimonMaru_Slope_Poly),add=TRUE)

Acoustic_Night_KaimonMaru_Slope=as.data.frame(mask(Acoustic_stack_Night,KaimonMaru_Slope_Poly),xy=TRUE)
Acoustic_Night_KaimonMaru_Slope=Acoustic_Night_KaimonMaru_Slope[rowSums(is.na(Acoustic_Night_KaimonMaru_Slope)) != ncol(Acoustic_Night_KaimonMaru_Slope)-2,]
Acoustic_Night_KaimonMaru_Slope$Site="KaimonMaru"
Acoustic_Night_KaimonMaru_Slope$Day="Night"
Acoustic_Night_KaimonMaru_Slope$Habitat="Slope"

#View(Acoustic_Night_KaimonMaru_Slope)
head(Acoustic_Night_KaimonMaru_Slope)
#############


########     KaimonMaru NIGHT SUMMIT ACOUSTIC
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],KaimonMaru_Summit_Poly),add=TRUE)

Acoustic_Night_KaimonMaru_Summit=as.data.frame(mask(Acoustic_stack_Night,KaimonMaru_Summit_Poly),xy=TRUE)
Acoustic_Night_KaimonMaru_Summit=Acoustic_Night_KaimonMaru_Summit[rowSums(is.na(Acoustic_Night_KaimonMaru_Summit)) != ncol(Acoustic_Night_KaimonMaru_Summit)-2,]
Acoustic_Night_KaimonMaru_Summit$Site="KaimonMaru"
Acoustic_Night_KaimonMaru_Summit$Day="Night"
Acoustic_Night_KaimonMaru_Summit$Habitat="Summit"

#View(Acoustic_Night_KaimonMaru_Summit)
head(Acoustic_Night_KaimonMaru_Summit)
#############

########     KaimonMaru DAY SLOPE ACOUSTIC
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],KaimonMaru_Slope_Poly),add=TRUE)

Acoustic_Day_KaimonMaru_Slope=as.data.frame(mask(Acoustic_stack_day,KaimonMaru_Slope_Poly),xy=TRUE)
Acoustic_Day_KaimonMaru_Slope=Acoustic_Day_KaimonMaru_Slope[rowSums(is.na(Acoustic_Day_KaimonMaru_Slope)) != ncol(Acoustic_Day_KaimonMaru_Slope)-2,]
Acoustic_Day_KaimonMaru_Slope$Site="KaimonMaru"
Acoustic_Day_KaimonMaru_Slope$Day="Day"
Acoustic_Day_KaimonMaru_Slope$Habitat="Slope"

#View(Acoustic_Day_KaimonMaru_Slope)
head(Acoustic_Day_KaimonMaru_Slope)
#############

########     KaimonMaru DAY SUMMIT ACOUSTIC
plot(KaimonMaru_Depth)
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],KaimonMaru_Summit_Poly),add=TRUE)

Acoustic_Day_KaimonMaru_Summit=as.data.frame(mask(Acoustic_stack_day,KaimonMaru_Summit_Poly),xy=TRUE)
Acoustic_Day_KaimonMaru_Summit=Acoustic_Day_KaimonMaru_Summit[rowSums(is.na(Acoustic_Day_KaimonMaru_Summit)) != ncol(Acoustic_Day_KaimonMaru_Summit)-2,]
Acoustic_Day_KaimonMaru_Summit$Site="KaimonMaru"
Acoustic_Day_KaimonMaru_Summit$Day="Day"
Acoustic_Day_KaimonMaru_Summit$Habitat="Summit"

#View(Acoustic_Day_KaimonMaru_Summit)
head(Acoustic_Day_KaimonMaru_Summit)



###########################
#####   PUT TOGETHER ACOUSTIC DATA FOR SITE
###########################

##put together acoustic data for the site
KaimonMaru_Acoustic=rbind(Acoustic_Night_KaimonMaru_Slope,Acoustic_Night_KaimonMaru_Summit,Acoustic_Day_KaimonMaru_Slope,Acoustic_Day_KaimonMaru_Summit)

#add bathymetric data
KaimonMaru_Acoustic$SummitDepth=KaimonMaru_SummitDepth*(-1)
KaimonMaru_Acoustic$ValleyDepth=KaimonMaru_BottomDepth*(-1)
KaimonMaru_Acoustic$Height=KaimonMaru_Height
KaimonMaru_Acoustic$SummitAreasKm2=KaimonMaru_SummitArea
KaimonMaru_Acoustic$SummitRugosity=KaimonMaru_SummitRugosity
KaimonMaru_Acoustic$BottomDepth=extract(KaimonMaru_Depth,KaimonMaru_Acoustic[c(1,2)])*(-1)
View(KaimonMaru_Acoustic)
head(KaimonMaru_Acoustic)

# correct bad bottom depth for depth smaller than 800 m (maximum measured by EK60)
#head(KaimonMaru_Acoustic)
#cbind(names(KaimonMaru_Acoustic))

KaimonMaru_Acoustic2=KaimonMaru_Acoustic[,c(3:81)]
#cbind(names(KaimonMaru_Acoustic2))
head(KaimonMaru_Acoustic2)

# affecte une profondeur lorsque mesurable par EK60
for (i in 1:nrow(KaimonMaru_Acoustic2)) {
  for (p in 1:ncol(KaimonMaru_Acoustic2)) {
    if (sum(KaimonMaru_Acoustic2[i,c(p:79)],na.rm = TRUE)==0) {
      KaimonMaru_Acoustic$BottomDepth[i]=(p-1)*10
      break
    } 
  }
}


# affecte une profondeur lorsque non-mesurable par EK60
for (i in 1:nrow(KaimonMaru_Acoustic2)) {
    if (KaimonMaru_Acoustic2[i,79]!="NaN" & KaimonMaru_Acoustic$BottomDepth[i]<790) {
      KaimonMaru_Acoustic$BottomDepth[i]=mean(KaimonMaru_Acoustic$BottomDepth[KaimonMaru_Acoustic$BottomDepth > 790])
      next
  }
}


# reaffecte les habitats sommets et pente en fonction des profondeurs corrigees

KaimonMaru_Acoustic$Habitat[KaimonMaru_Acoustic$BottomDepth<=(KaimonMaru_SummitDepth+30/100*KaimonMaru_SummitDepth)*(-1)]="Summit"
KaimonMaru_Acoustic$Habitat[KaimonMaru_Acoustic$BottomDepth>(KaimonMaru_SummitDepth+30/100*KaimonMaru_SummitDepth)*(-1) &  
                      KaimonMaru_Acoustic$BottomDepth<=(KaimonMaru_BottomDepth-30/100*KaimonMaru_BottomDepth)*(-1)]="Slope"

# verifie qu'il n'y a aucune profondeur superieure a la limite infeieure de la pente
KaimonMaru_Acoustic[KaimonMaru_Acoustic$BottomDepth>(KaimonMaru_BottomDepth-30/100*KaimonMaru_BottomDepth)*(-1),]

#sample size for analysis at site
table(KaimonMaru_Acoustic$Habitat,KaimonMaru_Acoustic$Day)

# raster layer of surface acoustic data at site
KaimonMaru_Acoustic_Surface_raster=rasterFromXYZ(cbind(KaimonMaru_Acoustic$x,KaimonMaru_Acoustic$y,KaimonMaru_Acoustic$Depth_10),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
KaimonMaru_Acoustic_RealSummit_Surface_raster=rasterFromXYZ(cbind(KaimonMaru_Acoustic$x[KaimonMaru_Acoustic$Habitat=="Summit"],KaimonMaru_Acoustic$y[KaimonMaru_Acoustic$Habitat=="Summit"],KaimonMaru_Acoustic$Depth_10[KaimonMaru_Acoustic$Habitat=="Summit"]),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
KaimonMaru_Acoustic_RealSlope_Surface_raster=rasterFromXYZ(cbind(KaimonMaru_Acoustic$x[KaimonMaru_Acoustic$Habitat=="Slope"],KaimonMaru_Acoustic$y[KaimonMaru_Acoustic$Habitat=="Slope"],KaimonMaru_Acoustic$Depth_10[KaimonMaru_Acoustic$Habitat=="Slope"]),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



# add environmental data
head(KaimonMaru_Acoustic)
names(KaimonMaru_Acoustic)
KaimonMaru_Acoustic$SSTmean1k=extract(SSTmean1k,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$SSTmin1k=extract(SSTmin1k,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$SSTmax1k=extract(SSTmax1k,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$SSTsd1k=extract(SSTsd1k,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$ChlorA=extract(ChlorA,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$EastwardVelocity=extract(EastwardVelocity,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$NorthwardVelocity=extract(NorthwardVelocity,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$Salinity=extract(Salinity,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$SeaFloorPotentialTemperature=extract(SeaFloorPotentialTemperature,KaimonMaru_Acoustic[c(1,2)])
KaimonMaru_Acoustic$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,KaimonMaru_Acoustic[c(1,2)])

KaimonMaru_pnts=KaimonMaru_Acoustic[,c(1,2)]
dist.Reef = geosphere::dist2Line(KaimonMaru_pnts, Reef)
dist.Land = geosphere::dist2Line(KaimonMaru_pnts, Land)
KaimonMaru_Acoustic$ShortestDistanceReef=dist.Reef[,1]
KaimonMaru_Acoustic$ShortestDistanceLand=dist.Land[,1]



############# ajoute une couche acoustique surface (0-20 m) et fond (20 m au dessus du fond)

#surface 0-20m
KaimonMaru_Acoustic$AcousticSurface=rowMeans(KaimonMaru_Acoustic[,c(3:4)],na.rm = TRUE)


# fond: calcule la moyenne du signal acoustique pour les 2 dernieres couches au dessus du fond (20m) lorsque fond mesurable par EK60
KaimonMaru_Acoustic2=KaimonMaru_Acoustic[,c(3:81)]
KaimonMaru_Acoustic$AcousticFond="NaN"


for (i in 1:nrow(KaimonMaru_Acoustic2)) {
  for (p in 1:ncol(KaimonMaru_Acoustic2)) {
    k=p-2+2
    l=p-1+2
    if (sum(KaimonMaru_Acoustic2[i,c(p:79)],na.rm = TRUE)==0 & k==2) {
      KaimonMaru_Acoustic$AcousticFond[i]=KaimonMaru_Acoustic[i,l]
    }else if (sum(KaimonMaru_Acoustic2[i,c(p:79)],na.rm = TRUE)==0 & k!=2) {
      KaimonMaru_Acoustic$AcousticFond[i]=rowMeans(KaimonMaru_Acoustic[i,c(k:l)],na.rm = TRUE)
      break
    }
  }
}




#check final dataset
names(KaimonMaru_Acoustic)
head(KaimonMaru_Acoustic)
unique(KaimonMaru_Acoustic$Habitat)
unique(KaimonMaru_Acoustic$Day)
range(as.numeric(KaimonMaru_Acoustic$AcousticFond),na.rm=TRUE)
range(as.numeric(KaimonMaru_Acoustic$AcousticSurface),na.rm=TRUE)
table(KaimonMaru_Acoustic$Day,KaimonMaru_Acoustic$Habitat)
View(KaimonMaru_Acoustic)


# EXPORT DATA & plots
getwd()

#export directory
dir.exists("KaimonMaru_Output")
dir.create("KaimonMaru_Output")

write_xlsx(list(KaimonMaru = KaimonMaru_Acoustic),path="KaimonMaru_Output/KaimonMaru_Acoustic_Data.xlsx")
writeRaster(KaimonMaru_Depth, filename="KaimonMaru_Output/KaimonMaru_Depth.tif", format="GTiff", overwrite=TRUE)
writeRaster(KaimonMaru_Acoustic_Surface_raster, filename="KaimonMaru_Output/KaimonMaru_Acoustic_Surface_raster.tif", format="GTiff", overwrite=TRUE)

path=getwd()
setwd(paste(path,"/KaimonMaru_Output",sep=""))
getwd()
writeOGR(KaimonMaru_Slope_Poly, dsn = '.', layer = 'KaimonMaru_Slope_Poly', driver = "ESRI Shapefile")
writeOGR(KaimonMaru_Summit_Poly, dsn = '.', layer = 'KaimonMaru_Summit_Poly', driver = "ESRI Shapefile")

setwd(path)
getwd()

###### Plot data without showing error in depth summit/slope
plot(KaimonMaru_Depth,main="KaimonMaru")
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Acoustic_Surface_raster,col="lavender",add=TRUE,legend=FALSE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)

dev.print(device = png, file = "KaimonMaru_Output/KaimonMaru_Map_Acoustic.png", width = 600)



###Plot real slope and summit

plot(KaimonMaru_Depth,main="KaimonMaru")
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(KaimonMaru_Acoustic_RealSummit_Surface_raster,col="magenta",add=TRUE,legend=FALSE)
plot(KaimonMaru_Acoustic_RealSlope_Surface_raster,col="blue",add=TRUE,legend=FALSE)

dev.print(device = png, file = "KaimonMaru_Output/KaimonMaru_Map_Acoustic_RealSlopeSummit.png", width = 600)


#################
##################
#################









##########MULTIPLOT
par(mfrow=c(2,2))
plot(KaimonMaru_Depth,main="Night SLope")
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],KaimonMaru_Slope_Poly),add=TRUE)

plot(KaimonMaru_Depth,main="Night Summit")
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],KaimonMaru_Summit_Poly),add=TRUE)


plot(KaimonMaru_Depth, main= "Day Slope")
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],KaimonMaru_Slope_Poly),add=TRUE)

plot(KaimonMaru_Depth, main = "Day Summit")
contour(KaimonMaru_Depth,add=TRUE)
plot(KaimonMaru_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(KaimonMaru_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],KaimonMaru_Summit_Poly),add=TRUE)







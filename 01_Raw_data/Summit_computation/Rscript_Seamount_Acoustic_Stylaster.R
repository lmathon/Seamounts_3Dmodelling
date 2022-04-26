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
############ Stylaster 
# read extent
Stylaster_Extent=extent(167.55,167.84,-23.76,-23.54)
Stylaster_Depth=crop(Bathy_100,Stylaster_Extent)

plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(DNApoint,add=TRUE)

metaDna$DepthMNT=round(extract(Stylaster_Depth,xy))*(-1)
metaDna[metaDna$Site_name=="Stylaster",] #### les prof sommets sont ok, et les profs pelagique aussi

Stylaster_SummitDepth=round(max(values(Stylaster_Depth),na.rm=TRUE))
Stylaster_BottomDepth=round(min(values(Stylaster_Depth),na.rm=TRUE))
Stylaster_Height=Stylaster_SummitDepth-Stylaster_BottomDepth
Stylaster_SummitDepth
Stylaster_BottomDepth
Stylaster_Height   ###### la prof du sommet est correcte, la campagne de terrain indiquant un mont au sommet plat a environ 440 m de fond. Nos 10 mesure edna range entre 439 et 488 m

# aucune correction de profondeur
#values(Stylaster_Depth)[values(Stylaster_Depth) > -300] = round(runif(1000,299,313),digit=4)*(-1)   ### correction des valeurs de profondeur inferieures à 300m

Stylaster_SummitDepth=round(max(values(Stylaster_Depth),na.rm=TRUE))
Stylaster_BottomDepth=round(min(values(Stylaster_Depth),na.rm=TRUE))
Stylaster_Height=Stylaster_SummitDepth-Stylaster_BottomDepth
Stylaster_SummitDepth
Stylaster_BottomDepth
Stylaster_Height

plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(DNApoint,add=TRUE)

metaDna$DepthMNT=round(extract(Stylaster_Depth,xy))*(-1)
metaDna[metaDna$Site_name=="Stylaster",] #### les prof sommets sont ok, et les profs pelagique aussi

####SUMMIT
Stylaster_Summit_Poly=Stylaster_Depth
values(Stylaster_Summit_Poly)[values(Stylaster_Summit_Poly) < Stylaster_SummitDepth+30/100*Stylaster_SummitDepth] = NA
plot(Stylaster_Summit_Poly)
Stylaster_SummitRugosity=sd(values(Stylaster_Summit_Poly),na.rm=TRUE)
Stylaster_SummitRugosity
values(Stylaster_Summit_Poly)[!is.na(values(Stylaster_Summit_Poly))] <- 1
plot(Stylaster_Summit_Poly)
Stylaster_Summit_Poly=rasterToPolygons(Stylaster_Summit_Poly, dissolve=TRUE)
Stylaster_SummitArea=area(Stylaster_Summit_Poly)*1e-6
Stylaster_SummitArea
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)

###SLOPE
Stylaster_Slope_Poly=Stylaster_Depth
values(Stylaster_Slope_Poly)[values(Stylaster_Slope_Poly) > Stylaster_SummitDepth+30/100*Stylaster_SummitDepth | values(Stylaster_Slope_Poly) < Stylaster_BottomDepth-30/100*Stylaster_BottomDepth] = NA
plot(Stylaster_Slope_Poly)
values(Stylaster_Slope_Poly)[!is.na(values(Stylaster_Slope_Poly))] <- 1
plot(Stylaster_Slope_Poly)
Stylaster_Slope_Poly=rasterToPolygons(Stylaster_Slope_Poly, dissolve=TRUE)
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
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

Site=Stylaster_Depth
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

plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
#plot(DNApoint,add=TRUE)
plot(Acoustic_stack_Night[[2]], add=TRUE)
plot(Acoustic_stack_day[[2]], add=TRUE)


########     Stylaster NIGHT SLOPE ACOUSTIC
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Stylaster_Slope_Poly),add=TRUE)

Acoustic_Night_Stylaster_Slope=as.data.frame(mask(Acoustic_stack_Night,Stylaster_Slope_Poly),xy=TRUE)
Acoustic_Night_Stylaster_Slope=Acoustic_Night_Stylaster_Slope[rowSums(is.na(Acoustic_Night_Stylaster_Slope)) != ncol(Acoustic_Night_Stylaster_Slope)-2,]
Acoustic_Night_Stylaster_Slope$Site="Stylaster"
Acoustic_Night_Stylaster_Slope$Day="Night"
Acoustic_Night_Stylaster_Slope$Habitat="Slope"

#View(Acoustic_Night_Stylaster_Slope)
head(Acoustic_Night_Stylaster_Slope)
#############


########     Stylaster NIGHT SUMMIT ACOUSTIC
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Stylaster_Summit_Poly),add=TRUE)

Acoustic_Night_Stylaster_Summit=as.data.frame(mask(Acoustic_stack_Night,Stylaster_Summit_Poly),xy=TRUE)
Acoustic_Night_Stylaster_Summit=Acoustic_Night_Stylaster_Summit[rowSums(is.na(Acoustic_Night_Stylaster_Summit)) != ncol(Acoustic_Night_Stylaster_Summit)-2,]
Acoustic_Night_Stylaster_Summit$Site="Stylaster"
Acoustic_Night_Stylaster_Summit$Day="Night"
Acoustic_Night_Stylaster_Summit$Habitat="Summit"

#View(Acoustic_Night_Stylaster_Summit)
head(Acoustic_Night_Stylaster_Summit)
#############

########     Stylaster DAY SLOPE ACOUSTIC
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Stylaster_Slope_Poly),add=TRUE)

Acoustic_Day_Stylaster_Slope=as.data.frame(mask(Acoustic_stack_day,Stylaster_Slope_Poly),xy=TRUE)
Acoustic_Day_Stylaster_Slope=Acoustic_Day_Stylaster_Slope[rowSums(is.na(Acoustic_Day_Stylaster_Slope)) != ncol(Acoustic_Day_Stylaster_Slope)-2,]
Acoustic_Day_Stylaster_Slope$Site="Stylaster"
Acoustic_Day_Stylaster_Slope$Day="Day"
Acoustic_Day_Stylaster_Slope$Habitat="Slope"

#View(Acoustic_Day_Stylaster_Slope)
head(Acoustic_Day_Stylaster_Slope)
#############

########     Stylaster DAY SUMMIT ACOUSTIC
plot(Stylaster_Depth)
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Stylaster_Summit_Poly),add=TRUE)

Acoustic_Day_Stylaster_Summit=as.data.frame(mask(Acoustic_stack_day,Stylaster_Summit_Poly),xy=TRUE)
Acoustic_Day_Stylaster_Summit=Acoustic_Day_Stylaster_Summit[rowSums(is.na(Acoustic_Day_Stylaster_Summit)) != ncol(Acoustic_Day_Stylaster_Summit)-2,]
Acoustic_Day_Stylaster_Summit$Site="Stylaster"
Acoustic_Day_Stylaster_Summit$Day="Day"
Acoustic_Day_Stylaster_Summit$Habitat="Summit"

#View(Acoustic_Day_Stylaster_Summit)
head(Acoustic_Day_Stylaster_Summit)



###########################
#####   PUT TOGETHER ACOUSTIC DATA FOR SITE
###########################

##put together acoustic data for the site
Stylaster_Acoustic=rbind(Acoustic_Night_Stylaster_Slope,Acoustic_Night_Stylaster_Summit,Acoustic_Day_Stylaster_Slope,Acoustic_Day_Stylaster_Summit)

#add bathymetric data
Stylaster_Acoustic$SummitDepth=Stylaster_SummitDepth*(-1)
Stylaster_Acoustic$ValleyDepth=Stylaster_BottomDepth*(-1)
Stylaster_Acoustic$Height=Stylaster_Height
Stylaster_Acoustic$SummitAreasKm2=Stylaster_SummitArea
Stylaster_Acoustic$SummitRugosity=Stylaster_SummitRugosity
Stylaster_Acoustic$BottomDepth=extract(Stylaster_Depth,Stylaster_Acoustic[c(1,2)])*(-1)
View(Stylaster_Acoustic)
head(Stylaster_Acoustic)

# correct bad bottom depth for depth smaller than 800 m (maximum measured by EK60)
#head(Stylaster_Acoustic)
#cbind(names(Stylaster_Acoustic))

Stylaster_Acoustic2=Stylaster_Acoustic[,c(3:81)]
#cbind(names(Stylaster_Acoustic2))
head(Stylaster_Acoustic2)

# affecte une profondeur lorsque mesurable par EK60
for (i in 1:nrow(Stylaster_Acoustic2)) {
  for (p in 1:ncol(Stylaster_Acoustic2)) {
    if (sum(Stylaster_Acoustic2[i,c(p:79)],na.rm = TRUE)==0) {
      Stylaster_Acoustic$BottomDepth[i]=(p-1)*10
      break
    } 
  }
}


# affecte une profondeur lorsque non-mesurable par EK60
for (i in 1:nrow(Stylaster_Acoustic2)) {
    if (Stylaster_Acoustic2[i,79]!="NaN" & Stylaster_Acoustic$BottomDepth[i]<790) {
      Stylaster_Acoustic$BottomDepth[i]=mean(Stylaster_Acoustic$BottomDepth[Stylaster_Acoustic$BottomDepth > 790])
      next
  }
}


# reaffecte les habitats sommets et pente en fonction des profondeurs corrigees

Stylaster_Acoustic$Habitat[Stylaster_Acoustic$BottomDepth<=(Stylaster_SummitDepth+30/100*Stylaster_SummitDepth)*(-1)]="Summit"
Stylaster_Acoustic$Habitat[Stylaster_Acoustic$BottomDepth>(Stylaster_SummitDepth+30/100*Stylaster_SummitDepth)*(-1) &  
                      Stylaster_Acoustic$BottomDepth<=(Stylaster_BottomDepth-30/100*Stylaster_BottomDepth)*(-1)]="Slope"

# verifie qu'il n'y a aucune profondeur superieure a la limite infeieure de la pente
Stylaster_Acoustic[Stylaster_Acoustic$BottomDepth>(Stylaster_BottomDepth-30/100*Stylaster_BottomDepth)*(-1),]

#sample size for analysis at site
table(Stylaster_Acoustic$Habitat,Stylaster_Acoustic$Day)

# raster layer of surface acoustic data at site
Stylaster_Acoustic_Surface_raster=rasterFromXYZ(cbind(Stylaster_Acoustic$x,Stylaster_Acoustic$y,Stylaster_Acoustic$Depth_10),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Stylaster_Acoustic_RealSummit_Surface_raster=rasterFromXYZ(cbind(Stylaster_Acoustic$x[Stylaster_Acoustic$Habitat=="Summit"],Stylaster_Acoustic$y[Stylaster_Acoustic$Habitat=="Summit"],Stylaster_Acoustic$Depth_10[Stylaster_Acoustic$Habitat=="Summit"]),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Stylaster_Acoustic_RealSlope_Surface_raster=rasterFromXYZ(cbind(Stylaster_Acoustic$x[Stylaster_Acoustic$Habitat=="Slope"],Stylaster_Acoustic$y[Stylaster_Acoustic$Habitat=="Slope"],Stylaster_Acoustic$Depth_10[Stylaster_Acoustic$Habitat=="Slope"]),crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



# add environmental data
head(Stylaster_Acoustic)
names(Stylaster_Acoustic)
Stylaster_Acoustic$SSTmean1k=extract(SSTmean1k,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$SSTmin1k=extract(SSTmin1k,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$SSTmax1k=extract(SSTmax1k,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$SSTsd1k=extract(SSTsd1k,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$ChlorA=extract(ChlorA,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$EastwardVelocity=extract(EastwardVelocity,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$NorthwardVelocity=extract(NorthwardVelocity,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$Salinity=extract(Salinity,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$SeaFloorPotentialTemperature=extract(SeaFloorPotentialTemperature,Stylaster_Acoustic[c(1,2)])
Stylaster_Acoustic$SuspendedParticulateMatter=extract(SuspendedParticulateMatter,Stylaster_Acoustic[c(1,2)])

Stylaster_pnts=Stylaster_Acoustic[,c(1,2)]
dist.Reef = geosphere::dist2Line(Stylaster_pnts, Reef)
dist.Land = geosphere::dist2Line(Stylaster_pnts, Land)
Stylaster_Acoustic$ShortestDistanceReef=dist.Reef[,1]
Stylaster_Acoustic$ShortestDistanceLand=dist.Land[,1]



############# ajoute une couche acoustique surface (0-20 m) et fond (20 m au dessus du fond)

#surface 0-20m
Stylaster_Acoustic$AcousticSurface=rowMeans(Stylaster_Acoustic[,c(3:4)],na.rm = TRUE)


# fond: calcule la moyenne du signal acoustique pour les 2 dernieres couches au dessus du fond (20m) lorsque fond mesurable par EK60
Stylaster_Acoustic2=Stylaster_Acoustic[,c(3:81)]
Stylaster_Acoustic$AcousticFond="NaN"


for (i in 1:nrow(Stylaster_Acoustic2)) {
  for (p in 1:ncol(Stylaster_Acoustic2)) {
    k=p-2+2
    l=p-1+2
    if (sum(Stylaster_Acoustic2[i,c(p:79)],na.rm = TRUE)==0 & k==2) {
      Stylaster_Acoustic$AcousticFond[i]=Stylaster_Acoustic[i,l]
    }else if (sum(Stylaster_Acoustic2[i,c(p:79)],na.rm = TRUE)==0 & k!=2) {
      Stylaster_Acoustic$AcousticFond[i]=rowMeans(Stylaster_Acoustic[i,c(k:l)],na.rm = TRUE)
      break
    }
  }
}




#check final dataset
names(Stylaster_Acoustic)
head(Stylaster_Acoustic)
unique(Stylaster_Acoustic$Habitat)
unique(Stylaster_Acoustic$Day)
range(as.numeric(Stylaster_Acoustic$AcousticFond),na.rm=TRUE)
range(as.numeric(Stylaster_Acoustic$AcousticSurface),na.rm=TRUE)
table(Stylaster_Acoustic$Day,Stylaster_Acoustic$Habitat)
View(Stylaster_Acoustic)


# EXPORT DATA & plots
getwd()

#export directory
dir.exists("Stylaster_Output")
dir.create("Stylaster_Output")

write_xlsx(list(Stylaster = Stylaster_Acoustic),path="Stylaster_Output/Stylaster_Acoustic_Data.xlsx")
writeRaster(Stylaster_Depth, filename="Stylaster_Output/Stylaster_Depth.tif", format="GTiff", overwrite=TRUE)
writeRaster(Stylaster_Acoustic_Surface_raster, filename="Stylaster_Output/Stylaster_Acoustic_Surface_raster.tif", format="GTiff", overwrite=TRUE)

path=getwd()
setwd(paste(path,"/Stylaster_Output",sep=""))
getwd()
writeOGR(Stylaster_Slope_Poly, dsn = '.', layer = 'Stylaster_Slope_Poly', driver = "ESRI Shapefile")
writeOGR(Stylaster_Summit_Poly, dsn = '.', layer = 'Stylaster_Summit_Poly', driver = "ESRI Shapefile")

setwd(path)
getwd()

###### Plot data without showing error in depth summit/slope
plot(Stylaster_Depth,main="Stylaster")
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Acoustic_Surface_raster,col="lavender",add=TRUE,legend=FALSE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)

dev.print(device = png, file = "Stylaster_Output/Stylaster_Map_Acoustic.png", width = 600)



###Plot real slope and summit

plot(Stylaster_Depth,main="Stylaster")
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(Stylaster_Acoustic_RealSummit_Surface_raster,col="magenta",add=TRUE,legend=FALSE)
plot(Stylaster_Acoustic_RealSlope_Surface_raster,col="blue",add=TRUE,legend=FALSE)

dev.print(device = png, file = "Stylaster_Output/Stylaster_Map_Acoustic_RealSlopeSummit.png", width = 600)


#################
##################
#################









##########MULTIPLOT
par(mfrow=c(2,2))
plot(Stylaster_Depth,main="Night SLope")
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Stylaster_Slope_Poly),add=TRUE)

plot(Stylaster_Depth,main="Night Summit")
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_Night[[2]],Stylaster_Summit_Poly),add=TRUE)


plot(Stylaster_Depth, main= "Day Slope")
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Stylaster_Slope_Poly),add=TRUE)

plot(Stylaster_Depth, main = "Day Summit")
contour(Stylaster_Depth,add=TRUE)
plot(Stylaster_Slope_Poly,border="blue",lwd=3,add=TRUE)
plot(Stylaster_Summit_Poly,border="magenta",lwd=3,add=TRUE)
plot(mask(Acoustic_stack_day[[2]],Stylaster_Summit_Poly),add=TRUE)







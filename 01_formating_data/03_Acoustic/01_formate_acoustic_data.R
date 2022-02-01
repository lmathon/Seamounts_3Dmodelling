## read acoustic file
Acoustic= read.csv("01_formating_data/03_Acoustic/Acoustic_Data_All_Site_Cut_10m.csv", sep=";")


names(Acoustic)
head(Acoustic)
summary(Acoustic) 

Acoustic$Site=as.factor(Acoustic$Site) 
Acoustic$Day=as.factor(Acoustic$Day) 
Acoustic$Habitat=as.factor(Acoustic$Habitat) 

#  transform travel time in hours
Acoustic$TravelTimeHour=Acoustic$TravelTime.seconds / 3600
Acoustic=Acoustic[-c(105)] # remove variable 105 (travel time in second)

# transform distances in km
Acoustic$ShortestDistanceLand=Acoustic$ShortestDistanceLand/1000
Acoustic$ShortestDistanceReef=Acoustic$ShortestDistanceReef/1000



# replace values > 100 with NA in AcousticFond and AcousticSurface
hist(Acoustic$AcousticFond) 
summary(Acoustic$AcousticFond)
Acoustic$AcousticFond[Acoustic$AcousticFond > 100] <- NA
hist(Acoustic$AcousticFond) 

hist(Acoustic$AcousticSurface) 
summary(Acoustic$AcousticSurface)
Acoustic$AcousticSurface[Acoustic$AcousticSurface > 100] <- NA
hist(Acoustic$AcousticSurface)

# transform acoustic data in log(x+1)
Acoustic$LogAcousticFond=log(Acoustic$AcousticFond + 1)
Acoustic$LogAcousticSurface=log(Acoustic$AcousticSurface + 1)


#rename lat long
colnames(Acoustic)[colnames(Acoustic) == "x"] <- "Longitude"
colnames(Acoustic)[colnames(Acoustic) == "y"] <- "Latitude"

# save rdata
save(Acoustic, file="01_formating_data/03_Acoustic/Rdata/acoustic.rdata")

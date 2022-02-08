## read acoustic file
Acoustic= read.csv("01_Raw_data/Acoustic_Data_All_Site_Cut_10m.csv", sep=";")

Acoustic <- Acoustic %>%
  filter(Habitat != "TransitBelow2500m")

names(Acoustic)
head(Acoustic)
summary(Acoustic) 

# extract and formate explanatory variables
acoustic_var <- Acoustic[,c(1,2,82:100,103:105)]

acoustic_var$Site=as.factor(acoustic_var$Site) 
acoustic_var$Day=as.factor(acoustic_var$Day) 
acoustic_var$Habitat=as.factor(acoustic_var$Habitat) 

#  transform travel time in hours
acoustic_var$TravelTimeHour=acoustic_var$TravelTime.seconds / 3600
acoustic_var=acoustic_var[-c(24)] # remove variable travel time in second

# transform distances in km
acoustic_var$ShortestDistanceLand=acoustic_var$ShortestDistanceLand/1000
acoustic_var$ShortestDistanceReef=acoustic_var$ShortestDistanceReef/1000

colnames(acoustic_var)[colnames(acoustic_var) == "x"] <- "Longitude"
colnames(acoustic_var)[colnames(acoustic_var) == "y"] <- "Latitude"

save(acoustic_var, file="00_metadata/acoustic_explanatory_variables.rdata")


# replace values > 100 with NA in AcousticFond and AcousticSurface
hist(Acoustic$AcousticFond) 
summary(Acoustic$AcousticFond)
Acoustic$AcousticFond[Acoustic$AcousticFond > 100] <- NA
hist(Acoustic$AcousticFond) 


# transform acoustic data in log(x+1)
Acoustic$LogAcousticFond=log(Acoustic$AcousticFond + 1)

colnames(Acoustic)[colnames(Acoustic) == "x"] <- "Longitude"
colnames(Acoustic)[colnames(Acoustic) == "y"] <- "Latitude"

acoustic_fond <- Acoustic[,c("Longitude", "Latitude", "AcousticFond", "LogAcousticFond")]

# save rdata
save(acoustic_fond, file="02_formating_data/01_Bottom/Rdata/acoustic_fond.rdata")

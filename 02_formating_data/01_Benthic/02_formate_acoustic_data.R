## read acoustic file
Acoustic= read.csv("01_Raw_data/Acoustic_Data_All_Site_Cut_10m.csv", sep=";")

Acoustic <- Acoustic %>%
  filter(Habitat != "TransitBelow2500m")

names(Acoustic)
head(Acoustic)
summary(Acoustic) 

#------------------------------------------------------------------------------------------------------------------------------------
### Explanatory variables
#------------------------------------------------------------------------------------------------------------------------------------

acoustic_var <- Acoustic[,c(1,2,82:100,103:105)]

acoustic_var$Site=as.factor(acoustic_var$Site) 
acoustic_var$Day=as.factor(acoustic_var$Day) 
acoustic_var$Habitat=as.factor(acoustic_var$Habitat) 

acoustic_var$Site <- gsub("Nepoui", "PoyaNepoui", acoustic_var$Site)
acoustic_var$Site <- gsub("JumeauOuest", "JumeauWest", acoustic_var$Site)

acoustic_var <- acoustic_var %>%
  mutate(Habitat = case_when(
    Site %in% c("Noumea", "PoyaNepoui", "Poum", "GLN") ~ "DeepSlope",
    Site %in% c("Antigonia", "Torche", "Capel", "Fairway","JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova","Stylaster", "IleDesPins", "Eponge") ~ "Seamount"
  ))



#  transform travel time in hours
acoustic_var$TravelTime=acoustic_var$TravelTime.seconds / 3600
acoustic_var=acoustic_var[-c(24)] # remove variable travel time in second

# transform distances in km
acoustic_var$ShortestDistanceLand=acoustic_var$ShortestDistanceLand/1000
acoustic_var$ShortestDistanceReef=acoustic_var$ShortestDistanceReef/1000

acoustic_var <- acoustic_var[,-4]

colnames(acoustic_var) <- c("Longitude","Latitude","Site","Habitat","SummitDepth","ValleyDepth","Height",                      
                            "SummitAreaKm2","SummitRugosity","BottomDepth","SSTmean","SSTmin","SSTmax","SSTsd","Chla",
                            "EastwardVelocity","NorthwardVelocity","Salinity","seafloorTemp",
                            "SuspendedParticulateMatter","ReefMinDist","LandMinDist","TravelTime")



save(acoustic_var, file="00_metadata/acoustic_explanatory_variables.rdata")

#------------------------------------------------------------------------------------------------------------------------------------
### Benthic acoustic
#------------------------------------------------------------------------------------------------------------------------------------


# replace values > 100 with NA in AcousticFond
hist(Acoustic$AcousticFond) 
summary(Acoustic$AcousticFond)
Acoustic$AcousticFond[Acoustic$AcousticFond > 100] <- NA
hist(Acoustic$AcousticFond) 


# transform acoustic data in log(x+1)
Acoustic$LogAcousticFond=log(Acoustic$AcousticFond + 1)

colnames(Acoustic)[colnames(Acoustic) == "x"] <- "Longitude"
colnames(Acoustic)[colnames(Acoustic) == "y"] <- "Latitude"

acoustic_fond <- Acoustic[,c("Longitude", "Latitude", "AcousticFond")]

# save rdata
save(acoustic_fond, file="02_formating_data/01_Benthic/Rdata/acoustic_fond.rdata")

library(tidyverse)
library(taxonomizr)
library(naniar)

## read acoustic file
Acoustic= read.csv("01_Raw_data/Acoustic_Data_All_Site_Cut_10m.csv", sep=";")

Acoustic <- Acoustic %>%
  filter(Habitat != "TransitBelow2500m")


#####################################################################################################################################
## All data

acoustic_pelagic <- Acoustic[,c(3:81)]

# replace values > 100 with 0 
acoustic_pelagic[acoustic_pelagic > 100] <- 0

# remove acoustic values in the last 20m before bottom

last_depth <- data.frame(row=rownames(acoustic_pelagic),
                         Depth=names(acoustic_pelagic)[-1][max.col(!is.na(acoustic_pelagic[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  acoustic_pelagic[i, last_depth[i, "Depth"]] <- NA
  
}

last_depth <- data.frame(row=rownames(acoustic_pelagic),
                         Depth=names(acoustic_pelagic)[-1][max.col(!is.na(acoustic_pelagic[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  acoustic_pelagic[i, last_depth[i, "Depth"]] <- NA
  
}

acoustic_pelagic <- cbind(acoustic_pelagic, Acoustic[,c(1:2, 82:105)])

# correct site names
acoustic_pelagic$Site <- gsub("Nepoui", "PoyaNepoui", acoustic_pelagic$Site)
acoustic_pelagic$Site <- gsub("JumeauOuest", "JumeauWest", acoustic_pelagic$Site)

# correct habitat variable
acoustic_pelagic <- acoustic_pelagic %>%
  mutate(Habitat = case_when(
    Site %in% c("Noumea", "PoyaNepoui", "Poum", "GLN") ~ "DeepSlope",
    Site %in% c("Antigonia", "Torche", "Capel", "Fairway","JumeauWest", "Crypthelia", "KaimonMaru", "Argo", "Nova","Stylaster", "IleDesPins", "Eponge") ~ "Seamount"
  ))

# transform distances in km
acoustic_pelagic$ShortestDistanceLand=acoustic_pelagic$ShortestDistanceLand/1000
acoustic_pelagic$ShortestDistanceReef=acoustic_pelagic$ShortestDistanceReef/1000

#  transform travel time in hours
acoustic_pelagic$TravelTime=acoustic_pelagic$TravelTime.seconds / 3600

# remove unwanted variables
acoustic_pelagic=acoustic_pelagic[,-c(83,101,102,105)] 

# melt acoustic data in one column + column sampling depth
acoustic_pelagic <- melt(acoustic_pelagic, id=c(80:102), variable.name = "Sampling_Depth", value.name = "sA")

colnames(acoustic_pelagic) <- c("Longitude","Latitude","Site","Habitat","SummitDepth","ValleyDepth","Height",                      
                            "SummitAreaKm2","SummitRugosity","BottomDepth","SSTmean","SSTmin","SSTmax","SSTsd","Chla",
                            "EastwardVelocity","NorthwardVelocity","Salinity","seafloorTemp",
                            "SuspendedParticulateMatter","ReefMinDist","LandMinDist","TravelTime", "Sampling_Depth", "sA")

# save rdata
save(acoustic_pelagic, file="02_formating_data/02_pelagic/Rdata/acoustic_pelagic.rdata")

acoustic_var <- acoustic_pelagic[,-ncol(acoustic_pelagic)]

save(acoustic_var, file="00_metadata/acoustic_explanatory_variables_pelagic.rdata")

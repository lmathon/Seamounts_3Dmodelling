library(tidyverse)
library(taxonomizr)
library(naniar)

## read acoustic file
Acoustic= read.csv("01_Raw_data/Acoustic_Data_All_Site_Cut_10m.csv", sep=";")

Acoustic <- Acoustic %>%
  filter(Habitat != "TransitBelow2500m")

Acoustic <- Acoustic[,c(1:81)]

# replace values > 100 with NA 
Acoustic[,c(3:81)][Acoustic[,c(3:81)] > 100] <- NA


colnames(Acoustic)[colnames(Acoustic) == "x"] <- "Longitude"
colnames(Acoustic)[colnames(Acoustic) == "y"] <- "Latitude"

# remove acoustic values in the last 20m before bottom

last_depth <- data.frame(row=rownames(Acoustic),
                         Depth=names(Acoustic)[-1][max.col(!is.na(Acoustic[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  Acoustic[i, last_depth[i, "Depth"]] <- NA
  
}

last_depth <- data.frame(row=rownames(Acoustic),
                         Depth=names(Acoustic)[-1][max.col(!is.na(Acoustic[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  Acoustic[i, last_depth[i, "Depth"]] <- NA
  
}

acoustic_pelagic <- Acoustic
# save rdata
save(acoustic_pelagic, file="02_formating_data/02_pelagic/Rdata/acoustic_pelagic.rdata")




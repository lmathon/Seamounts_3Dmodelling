library(tidyverse)
library(taxonomizr)
library(naniar)

## read acoustic file
Acoustic= read.csv("01_Raw_data/Acoustic_Data_All_Site_Cut_10m.csv", sep=";")

Acoustic <- Acoustic %>%
  filter(Habitat != "TransitBelow2500m")

#####################################################################################################################################
## All data

acoustic_pelagic <- acoustic_pelagic[,c(1:81)]
summary(acoustic_pelagic)

# replace values > 100 with 0 
acoustic_pelagic[,c(3:81)][acoustic_pelagic[,c(3:81)] > 100] <- 0


colnames(acoustic_pelagic)[colnames(acoustic_pelagic) == "x"] <- "Longitude"
colnames(acoustic_pelagic)[colnames(acoustic_pelagic) == "y"] <- "Latitude"

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


# save rdata
save(acoustic_pelagic, file="02_formating_data/02_pelagic/Rdata/acoustic_pelagic.rdata")


################################################################################################################################
## Acoustic day


acoustic_day <- Acoustic %>%
  filter(Day=="Day")

acoustic_day <- acoustic_day[,c(1:81)]
summary(acoustic_day)

# replace values > 100 with 0 
acoustic_day[,c(3:81)][acoustic_day[,c(3:81)] > 100] <- 0


colnames(acoustic_day)[colnames(acoustic_day) == "x"] <- "Longitude"
colnames(acoustic_day)[colnames(acoustic_day) == "y"] <- "Latitude"

# remove acoustic_day values in the last 20m before bottom

last_depth <- data.frame(row=rownames(acoustic_day),
                         Depth=names(acoustic_day)[-1][max.col(!is.na(acoustic_day[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  acoustic_day[i, last_depth[i, "Depth"]] <- NA
  
}

last_depth <- data.frame(row=rownames(acoustic_day),
                         Depth=names(acoustic_day)[-1][max.col(!is.na(acoustic_day[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  acoustic_day[i, last_depth[i, "Depth"]] <- NA
  
}


# save rdata
save(acoustic_day, file="02_formating_data/02_pelagic/Rdata/acoustic_day.rdata")



################################################################################################################################
## Acoustic night


acoustic_night <- Acoustic %>%
  filter(Day=="Night")

acoustic_night <- acoustic_night[,c(1:81)]
summary(acoustic_night)

# replace values > 100 with 0 
acoustic_night[,c(3:81)][acoustic_night[,c(3:81)] > 100] <- 0


colnames(acoustic_night)[colnames(acoustic_night) == "x"] <- "Longitude"
colnames(acoustic_night)[colnames(acoustic_night) == "y"] <- "Latitude"

# remove acoustic_night values in the last 20m before bottom

last_depth <- data.frame(row=rownames(acoustic_night),
                         Depth=names(acoustic_night)[-1][max.col(!is.na(acoustic_night[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  acoustic_night[i, last_depth[i, "Depth"]] <- NA
  
}

last_depth <- data.frame(row=rownames(acoustic_night),
                         Depth=names(acoustic_night)[-1][max.col(!is.na(acoustic_night[-1]), ties.method="last")])

for (i in 1:nrow(last_depth)) {
  acoustic_night[i, last_depth[i, "Depth"]] <- NA
  
}


# save rdata
save(acoustic_night, file="02_formating_data/02_pelagic/Rdata/acoustic_night.rdata")

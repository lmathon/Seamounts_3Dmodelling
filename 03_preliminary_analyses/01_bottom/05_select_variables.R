library(gjam)
library(tidyverse)
library(car)

source("03_preliminary_analyses/01_bottom/00_functions.R")


load("00_metadata/bruvs_explanatory_variables.rdata")
rownames(bruvs_var) <- bruvs_var$Station
bruvs_var <- bruvs_var[,-c(1:4,24:38)]

cor_var <- mixed_assoc(bruvs_var)

ggplot(cor_var, aes(x,y,fill=assoc))+
  geom_tile()+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(face="plain", size=10, angle=90, vjust = 0, hjust = 1))+
  scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint=0)


bruvs_var <- bruvs_var %>% select(-c("ValleyDepth", "SSTmin", "SSTmax", "LandMinDist.m", "SummitDepth"))

cor_var <- mixed_assoc(bruvs_var)

ggplot(cor_var, aes(x,y,fill=assoc))+
  geom_tile()+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(face="plain", size=10, angle=90, vjust = 0, hjust = 1))+
  scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint=0)



load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_all.rdata")
load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_chondri.rdata")
load("02_formating_data/01_Bottom/Rdata/bruvs_species_matrix.rdata")

colnames(bruvs_abundance_chondri) <- c("Station", "abundance_chondri", "Logabundance_chondri")

bruvs_data <- left_join(bruvs_abundance_all, bruvs_abundance_chondri)
bruvs_data[is.na(bruvs_data)] <- 0


mod <- lm(bruvs_data$abundance_tot ~ ., data=bruvs_var)
summary(mod)
car::vif(mod)

mod <- lm(bruvs_data$Logabundance_tot ~ ., data=bruvs_var)
summary(mod)
car::vif(mod)

mod <- lm(bruvs_data$abundance_chondri ~ ., data=bruvs_var)
summary(mod)
car::vif(mod)

mod <- lm(bruvs_data$Logabundance_chondri ~ ., data=bruvs_var)
summary(mod)
car::vif(mod)


save(bruvs_var, file="00_metadata/bruvs_selected_variables.rdata")

load("00_metadata/acoustic_explanatory_variables.rdata")
colnames(acoustic_var) <- c("Longitude","Latitude","Site","Day","Habitat","SummitDepth","ValleyDepth","Height",                      
                            "SummitAreasKm2","SummitRugosity","BottomDepth","SSTmean","SSTmin","SSTmax","SSTsd","Chla",
                            "EastwardVelocity","NorthwardVelocity","Salinity","seafloorTemp",
                            "SuspendedParticulateMatter","ReefMinDist.m","LandMinDist.m","TravelTime")
acoustic_var <- acoustic_var %>% select(c("Longitude", "Latitude", "Site", "Day"), names(bruvs_var[,-15]))
save(acoustic_var, file="00_metadata/acoustic_selected_variables.rdata")

load("00_metadata/edna_explanatory_variables_fond.rdata")
rownames(edna_var) <- edna_var$Station
edna_var <- edna_var %>% select(names(bruvs_var))
save(edna_var, file="00_metadata/edna_selected_variables_fond.rdata")

load("00_metadata/edna_explanatory_variables_pelagic.rdata")
rownames(edna_var) <- edna_var$Station
edna_var <- edna_var %>% select(names(bruvs_var))
save(edna_var, file="00_metadata/edna_selected_variables_pelagic.rdata")

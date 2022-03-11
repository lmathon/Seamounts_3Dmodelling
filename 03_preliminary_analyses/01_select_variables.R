library(vegan)
library(tidyverse)
library(car)
library(modEvA)
library(MuMIn)

source("03_preliminary_analyses/00_functions.R")


load("00_metadata/bruvs_explanatory_variables.rdata")

cor_var <- mixed_assoc(bruvs_var[,-c(1:4)])

ggplot(cor_var, aes(x,y,fill=assoc))+
  geom_tile()+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(face="plain", size=10, angle=90, vjust = 0, hjust = 1))+
  scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint=0)

# Salinity negatively correlated with  SSTmax, SSTmean, SSTmin
# ReefMinDist negatively correlated with  SSTmax, SSTmean, SSTmin
# Height negatively correlated with SummitDepth
# Habitat & SummitArea highly correlated
# Height correlated with Habitat, ValleyDepth and SummitAreas
# LandMinDist correlated with TravelTime

# try correlations when removing Salinity, Height, LandMinDist, SSTmax, SSTmin 

cor_var <- mixed_assoc(bruvs_var[,-c(1:4,8, 10, 12, 18, 24)])

ggplot(cor_var, aes(x,y,fill=assoc))+
  geom_tile()+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(face="plain", size=10, angle=90, vjust = 0, hjust = 1))+
  scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint=0)


# Try removing also suspendedParticulateMatter, SummitRugosity
cor_var <- mixed_assoc(bruvs_var[,-c(1:4,8,9,10,12,18,20,24)])

ggplot(cor_var, aes(x,y,fill=assoc))+
  geom_tile()+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(face="plain", size=10, angle=90, vjust = 0, hjust = 1))+
  scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint=0)

# SummitDepth still correlated with BottomDepth
# SummitAreaKm2 still correlated with Habitat


# Non-correlated variables are : Habitat, EastwardVelocity, NorthwardVelocity, SSTmean, SSTsd, seafloorTemp, Chla, SummitDepth, ValleyDepth, SummitAreaKm2, BottomDepth, TravelTime, ReefMinDist



############################################################################################################################################
## Test on BRUVS abundance tot

load("02_formating_data/01_Benthic/Rdata/bruvs_abundance_all.rdata")

bruvs <- left_join(bruvs_abundance_all[,c("abundance_tot","Station")], bruvs_var[,-c(2:5,18,24)])
bruvs <- bruvs[,-2]

bruvs2 <- (bruvs[,-1])^2
colnames(bruvs2) <- c("EastwardVelocity2","NorthwardVelocity2","Salinity2","SuspendedParticulateMatter2",
                      "SSTmax2","SSTmean2","SSTmin2","SSTsd2","seafloorTemp2","Chla2","SummitDepth2","ValleyDepth2",               
                      "SummitAreaKm22","SummitRugosity2","BottomDepth2","TravelTime2","ReefMinDist2")

bruvs <- cbind(bruvs, bruvs2)

bruvs3 <- (bruvs[,c(2:18)])^3
colnames(bruvs3) <- c("EastwardVelocity3","NorthwardVelocity3","Salinity3","SuspendedParticulateMatter3",
                      "SSTmax3","SSTmean3","SSTmin3","SSTsd3","seafloorTemp3","Chla3","SummitDepth3","ValleyDepth3",               
                      "SummitAreaKm23","SummitRugosity3","BottomDepth3","TravelTime3","ReefMinDist3")
bruvs <- cbind(bruvs, bruvs3)

cor_full <- cor(bruvs)



library(tidyverse)
library(car)

source("03_preliminary_analyses/01_bottom/00_functions.R")


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


# Selected variables are : Habitat, EastwardVelocity, NorthwardVelocity, SSTmean, SSTsd, seafloorTemp, Chla, SummitDepth, ValleyDepth, SummitAreaKm2, BottomDepth, TravelTime, ReefMinDist


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

lm_null <- glm(abundance_tot ~ 1, bruvs_abundance_all, family = "poisson")
glm_full <- glm(abundance_tot ~ ., bruvs, family = "poisson", na.action = na.fail)
lm_full <- glm(abundance_tot ~ Salinity +
                SuspendedParticulateMatter + EastwardVelocity + NorthwardVelocity + SSTmax + SSTmean + SSTmin + 
                SSTsd + seafloorTemp + Chla + SummitDepth + ValleyDepth + 
                SummitAreaKm2 + SummitRugosity + BottomDepth + TravelTime + 
                ReefMinDist + I(EastwardVelocity^2) + I(NorthwardVelocity^2) + I(Salinity^2) + 
                I(SuspendedParticulateMatter^2) + I(SSTmax^2) + I(SSTmean^2) + I(SSTmin^2) + 
                I(SSTsd^2) + I(seafloorTemp^2) + I(Chla^2) + I(SummitDepth^2) + I(ValleyDepth^2) + 
                I(SummitAreaKm2^2) + I(SummitRugosity^2) + I(BottomDepth^2) + I(TravelTime^2) + 
                I(ReefMinDist^2) + I(EastwardVelocity^3) + I(NorthwardVelocity^3) + I(Salinity^3) + 
                I(SuspendedParticulateMatter^3) + I(SSTmax^3) + I(SSTmean^3) + I(SSTmin^3) + 
                I(SSTsd^3) + I(seafloorTemp^3) + I(Chla^3) + I(SummitDepth^3) + I(ValleyDepth^3) + 
                I(SummitAreaKm2^3) + I(SummitRugosity^3) + I(BottomDepth^3) + I(TravelTime^3) + 
                I(ReefMinDist^3), bruvs, family = "poisson")

anova(lm_null, lm_full)
summary(lm_full)
AIC(lm_full)
RsqGLM(lm_full)

imp <- dredge(glm_full)

lm1 <- glm(abundance_tot ~ EastwardVelocity, bruvs, family = "poisson")
AIC(lm1)
RsqGLM(lm1)
anova(lm_null, lm1)

lm2 <- glm(abundance_tot ~ EastwardVelocity+NorthwardVelocity+BottomDepth+Chla+I(SSTmean^2)+
            ReefMinDist+I(TravelTime^2), bruvs, family = "poisson")

AIC(lm2)
RsqGLM(lm2)
anova(lm_null, lm2)


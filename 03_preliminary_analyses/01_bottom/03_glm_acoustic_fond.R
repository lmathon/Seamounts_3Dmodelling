library(vegan)
library(performance)
library(ggplot2)
library(jtools)

load("02_formating_data/01_Bottom/Rdata/acoustic_fond.rdata")
load("00_metadata/acoustic_explanatory_variables.rdata")

Acoustic <- cbind(acoustic_fond, acoustic_var)
Acoustic <- Acoustic[,-c(5,6)]

###
names(Acoustic)
glm.variables= glm(AcousticFond ~ Day+Habitat+SummitDepth+ValleyDepth+
                     Height+SummitAreasKm2+SummitRugosity+BottomDepth+
                     SSTmean1k + SSTmin1k+SSTmax1k+ SSTsd1k+ChlorA+
                     EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                     SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand+TravelTimeHour, 
                   family=Gamma, data=Acoustic)
anova(glm.variables)
summary(glm.variables) 
AIC(glm.variables)

# remove non significant variables
glm.variables= glm(AcousticFond ~ Day+Habitat+SummitDepth+ValleyDepth+
                           SummitRugosity+BottomDepth+
                           SSTmean1k + SSTmin1k+SSTmax1k+ SSTsd1k+
                           EastwardVelocity+Salinity+
                           SuspendedParticulateMatter+ShortestDistanceLand, 
                         family=Gamma, data=Acoustic)
anova(glm.variables)
summary(glm.variables)
AIC(glm.variables)
r2(glm.variables)


summ(glm.variables, confint = TRUE)

effect_plot(glm.variables, pred = Day, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Habitat, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = EastwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = NorthwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Salinity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SuspendedParticulateMatter, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTmax, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTmean, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTmin, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTsd, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = seafloorTemp, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Chla, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = TravelTimeHour, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = ShortestDistanceReef, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = ShortestDistanceLand, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Height, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitAreasKm2, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitRugosity, interval = TRUE, plot.points = TRUE)


ggplot(Acoustic, aes(Site, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(Acoustic, aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(Acoustic, aes(Day, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Period") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))+
  facet_wrap(~as.factor(Day), nrow=2)



## anova a 1 facteur

anova.habitat= aov(AcousticFond ~ Habitat, data=Acoustic) 
summary(anova.habitat) 


## anova a 2 facteurs

anova.Site.habitat= aov(AcousticFond ~ Site*Habitat, data=Acoustic) 
summary(anova.Site.habitat) 
TukeyHSD(anova.Site.habitat, "Site")





####### GLM


glm.Site.habitat= glm(AcousticFond ~ Site*Habitat, family=gaussian, data=Acoustic)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat= glm(AcousticFond ~ Site+Habitat, family=gaussian, data=Acoustic)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(AcousticFond ~ Site+Habitat+BottomDepth, family=gaussian, data=Acoustic)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)




# ------------------------------------------------------------------------------------
# On log(acousticFond)
# ---------------------------------------------------------------------------------------
###

glm.variables= glm(LogAcousticFond ~ Day+Habitat+SummitDepth+ValleyDepth+
                     Height+SummitAreasKm2+SummitRugosity+BottomDepth+
                     SSTmean1k + SSTmin1k+SSTmax1k+ SSTsd1k+ChlorA+
                     EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                     SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand+TravelTimeHour, 
                   family=Gamma, data=Acoustic)
anova(glm.variables)
summary(glm.variables) 
AIC(glm.variables)

# remove non significant variables
glm.variables= glm(LogAcousticFond ~ Day+Habitat+SummitDepth+ValleyDepth+
                     SummitRugosity+BottomDepth+
                     SSTmean1k + SSTmin1k+SSTmax1k+ SSTsd1k+
                     EastwardVelocity+Salinity+
                     SuspendedParticulateMatter+ShortestDistanceLand, 
                   family=Gamma, data=Acoustic)
anova(glm.variables)
summary(glm.variables)
AIC(glm.variables)
r2(glm.variables)


summ(glm.variables, confint = TRUE)

effect_plot(glm.variables, pred = Day, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Habitat, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = EastwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = NorthwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Salinity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SuspendedParticulateMatter, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTmax, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTmean, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTmin, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SSTsd, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = seafloorTemp, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Chla, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = TravelTimeHour, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = ShortestDistanceReef, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = ShortestDistanceLand, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Height, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitAreasKm2, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitRugosity, interval = TRUE, plot.points = TRUE)


ggplot(Acoustic, aes(Site, LogAcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(Acoustic, aes(Habitat, LogAcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(Acoustic, aes(Day, LogAcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Period") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(aes(y = LogAcousticFond, x = Site, fill = Habitat), data = Acoustic) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))+
  facet_wrap(~as.factor(Day), nrow=2)



## anova a 1 facteur

anova.habitat= aov(LogAcousticFond ~ Habitat, data=Acoustic) 
summary(anova.habitat) 


## anova a 2 facteurs

anova.Site.habitat= aov(LogAcousticFond ~ Site*Habitat, data=Acoustic) 
summary(anova.Site.habitat) 
TukeyHSD(anova.Site.habitat, "Site")



####### GLM


glm.Site.habitat= glm(LogAcousticFond ~ Site*Habitat, family=gaussian, data=Acoustic)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat= glm(LogAcousticFond ~ Site+Habitat, family=gaussian, data=Acoustic)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(LogAcousticFond ~ Site+Habitat+BottomDepth, family=gaussian, data=Acoustic)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)


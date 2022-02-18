library(vegan)
library(performance)
library(ggplot2)
library(jtools)

load("02_formating_data/01_Bottom/Rdata/edna_richness_fond.rdata")
load("00_metadata/edna_explanatory_variables_fond.rdata")

edna_richness_tot <- left_join(edna_richness_fond, edna_var)

#------------------------------------------------------------------------------------------
# On all species abundance
#------------------------------------------------------------------------------------------

names(edna_richness_tot)

summary(edna_richness_tot)
edna_richness_tot$Habitat <- as.factor(edna_richness_tot$Habitat)
edna_richness_tot$Site <- as.factor(edna_richness_tot$Site)
edna_richness_tot$Depth <- gsub("sondeur éteint", NA, edna_richness_tot$Depth)
edna_richness_tot$Depth <- as.numeric(edna_richness_tot$Depth)


###

glm.variables= glm(richness_tot ~ Sampling_depth+Habitat+Site+EastwardVelocity+NorthwardVelocity+Salinity+
                    SuspendedParticulateMatter+SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+ValleyDepth+
                    TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+Height+SummitAreasKm2+SummitRugosity+
                    Depth, family=poisson, data=edna_richness_tot)

anova(glm.variables)
summary(glm.variables) 
AIC(glm.variables)

# remove non significant variables
glm.variables= glm(richness_tot ~ Sampling_depth+Habitat+EastwardVelocity+NorthwardVelocity+Salinity+
                     SuspendedParticulateMatter+SSTmax+seafloorTemp+Chla+ValleyDepth+
                     TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+SummitRugosity+
                     Depth, family=poisson, data=edna_richness_tot)
anova(glm.variables)
summary(glm.variables) 

r2(glm.variables)
AIC(glm.variables)


summ(glm.variables, confint = TRUE)

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
effect_plot(glm.variables, pred = TravelTime, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = ReefMinDist.m, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = LandMinDist.m, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Height, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitAreasKm2, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitRugosity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Complex, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Reef.structure, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Rocks, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Rubbles, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Gravel, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Sand, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Mud, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Indeterm, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Coral, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Filter_feeders, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Vegetation, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Bare, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = HabitatDiversity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Depth, interval = TRUE, plot.points = TRUE)



ggplot(edna_richness_tot, aes(Site, richness_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("richness")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(edna_richness_tot, aes(Habitat, richness_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("richness")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = richness_tot, x = Site, fill = Habitat), data = edna_richness_tot) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("richness") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

## anova a 1 facteur

anova.habitat= aov(richness_tot ~ Habitat, data=edna_richness_tot) 
summary(anova.habitat) 

## anova a 2 facteurs

anova.Site.habitat= aov(richness_tot ~ Site*Habitat, data=edna_richness_tot) 
summary(anova.Site.habitat) 

TukeyHSD(anova.Site.habitat, "Site")


####### GLM

##
glm.Site.habitat= glm(richness_tot ~ Site+Habitat, family=gaussian, data=edna_richness_tot)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(richness_tot ~ Site+Habitat+Depth, family=gaussian, data=edna_richness_tot)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)



#------------------------------------------------------------------------------------------
# On log (all species abundance)
#------------------------------------------------------------------------------------------

glm.variables= glm(logrichness_tot ~ Sampling_depth+Habitat+EastwardVelocity+NorthwardVelocity+Salinity+
                     SuspendedParticulateMatter+SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+ValleyDepth+
                     TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+Height+SummitAreasKm2+
                     Depth, family=gaussian, data=edna_richness_tot)

anova(glm.variables)
summary(glm.variables) 
AIC(glm.variables)


summ(glm.variables, confint = TRUE)

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
effect_plot(glm.variables, pred = TravelTime, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = ReefMinDist.m, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = LandMinDist.m, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Height, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitAreasKm2, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = SummitRugosity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Complex, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Reef.structure, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Rocks, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Rubbles, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Gravel, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Sand, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Mud, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Indeterm, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Coral, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Filter_feeders, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Vegetation, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Bare, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = HabitatDiversity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables, pred = Depth, interval = TRUE, plot.points = TRUE)



ggplot(edna_richness_tot, aes(Site, logrichness_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(edna_richness_tot, aes(Habitat, logrichness_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = logrichness_tot, x = Site, fill = Habitat), data = edna_richness_tot) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

## anova a 1 facteur

anova.habitat= aov(logrichness_tot ~ Habitat, data=edna_richness_tot) 
summary(anova.habitat) 

## anova a 2 facteurs

anova.Site.habitat= aov(logrichness_tot ~ Site*Habitat, data=edna_richness_tot) 
summary(anova.Site.habitat) 

TukeyHSD(anova.Site.habitat, "Site")


####### GLM

##
glm.Site.habitat= glm(logrichness_tot ~ Site+Habitat, family=gaussian, data=edna_richness_tot)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(logrichness_tot ~ Site+Habitat+Depth, family=gaussian, data=edna_richness_tot)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)



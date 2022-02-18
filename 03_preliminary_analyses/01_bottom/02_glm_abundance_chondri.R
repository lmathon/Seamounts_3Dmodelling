library(vegan)
library(performance)
library(ggplot2)
library(jtools)

load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_chondri.rdata")
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_abundance_chondri <- left_join(bruvs_abundance_chondri, bruvs_var)


#------------------------------------------------------------------------------------------
# On all species abundance
#------------------------------------------------------------------------------------------

summary(bruvs_abundance_chondri)
bruvs_abundance_chondri$Habitat <- as.factor(bruvs_abundance_chondri$Habitat)
bruvs_abundance_chondri$Site <- as.factor(bruvs_abundance_chondri$Site)


###
names(bruvs_abundance_chondri)
glm.variables= glm(abundance_tot ~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+
                     SuspendedParticulateMatter+SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+ValleyDepth+
                     TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+Height+SummitAreasKm2+SummitRugosity+
                     Topo+Complex+Reef.structure+Rocks+Rubbles+Gravel+Sand+Mud+Indeterm+Coral+Filter_feeders+
                     Vegetation+Bare+HabitatDiversity+Depth, family=poisson, data=bruvs_abundance_chondri)

anova(glm.variables)
summary(glm.variables) 
AIC(glm.variables)

# remove non significant variables
glm.variables= glm(abundance_tot ~ SSTsd+TravelTime+Reef.structure+Rocks+Rubbles+Gravel+Sand+Indeterm, family=poisson, data=bruvs_abundance_chondri)
anova(glm.variables)
summary(glm.variables) 

r2(glm.variables)
AIC(glm.variables)

#poisson

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



ggplot(bruvs_abundance_chondri, aes(Site, abundance_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(bruvs_abundance_chondri, aes(Habitat, abundance_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = abundance_tot, x = Site, fill = Habitat), data = bruvs_abundance_chondri) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

## anova a 1 facteur

anova.habitat= aov(abundance_tot ~ Habitat, data=bruvs_abundance_chondri) 
summary(anova.habitat) 

## anova a 2 facteurs

anova.Site.habitat= aov(abundance_tot ~ Site*Habitat, data=bruvs_abundance_chondri) 
summary(anova.Site.habitat) 

TukeyHSD(anova.Site.habitat, "Site")


####### GLM

##
glm.Site.habitat= glm(abundance_tot ~ Site+Habitat, family=gaussian, data=bruvs_abundance_chondri)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(abundance_tot ~ Site+Habitat+Depth, family=gaussian, data=bruvs_abundance_chondri)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)



#------------------------------------------------------------------------------------------
# On log (all species abundance)
#------------------------------------------------------------------------------------------

glm.variables= glm(Logabundance_tot ~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+
                     SuspendedParticulateMatter+SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+ValleyDepth+
                     TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+Height+SummitAreasKm2+SummitRugosity+
                     Topo+Complex+Reef.structure+Rocks+Rubbles+Gravel+Sand+Mud+Indeterm+Coral+Filter_feeders+
                     Vegetation+Bare+HabitatDiversity+Depth, family=gaussian, data=bruvs_abundance_chondri)

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



ggplot(bruvs_abundance_chondri, aes(Site, Logabundance_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(bruvs_abundance_chondri, aes(Habitat, Logabundance_tot)) + 
  geom_jitter(colour="lightgrey") + 
  geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = Logabundance_tot, x = Site, fill = Habitat), data = bruvs_abundance_chondri) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

## anova a 1 facteur

anova.habitat= aov(Logabundance_tot ~ Habitat, data=bruvs_abundance_chondri) 
summary(anova.habitat) 

## anova a 2 facteurs

anova.Site.habitat= aov(Logabundance_tot ~ Site*Habitat, data=bruvs_abundance_chondri) 
summary(anova.Site.habitat) 

TukeyHSD(anova.Site.habitat, "Site")


####### GLM

##
glm.Site.habitat= glm(Logabundance_tot ~ Site+Habitat, family=gaussian, data=bruvs_abundance_chondri)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(Logabundance_tot ~ Site+Habitat+Depth, family=gaussian, data=bruvs_abundance_chondri)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)



library(vegan)
library(performance)
library(ggplot2)

load("01_formating_data/02_BRUVs/Rdata/bruvs_species_variables.rdata")

summary(bruvs_species_variable)
bruvs_species_variable$Habitat <- as.factor(bruvs_species_variable$Habitat)
bruvs_species_variable$Site <- as.factor(bruvs_species_variable$Site)


###
names(bruvs_species_variable)
glm.variables= glm(abundance_tot ~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+
                     SuspendedParticulateMatter+SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+ValleyDepth+
                     TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+Height+SummitAreasKm2+SummitRugosity+
                     Topo+Complex+Reef.structure+Rocks+Rubbles+Gravel+Sand+Mud+Indeterm+Coral+Filter_feeders+
                     Vegetation+Bare+HabitatDiversity+Depth, family=poisson, data=bruvs_species_variable)
anova(glm.variables)
summary(glm.variables) 

# on enleve valley depth car la hauteur c'est la difference entre sommet et vallee
glm.variables= glm(abundance_tot ~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+
                     SuspendedParticulateMatter+SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+
                     TravelTime+ReefMinDist.m+LandMinDist.m+SummitDepth+Height+SummitAreasKm2+SummitRugosity+
                     Topo+Complex+Reef.structure+Rocks+Rubbles+Gravel+Sand+Mud+Indeterm+Coral+Filter_feeders+
                     Vegetation+Bare+HabitatDiversity+Depth, family=poisson, data=bruvs_species_variable)
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
effect_plot(glm.variables, pred = Topo, interval = TRUE, plot.points = TRUE)
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



###################
#################
###############


boxplot(abundance_tot ~ Habitat, data = bruvs_species_variable, col = "lightgray",na.rm=TRUE)
boxplot(abundance_tot ~ Site, data = bruvs_species_variable, col = "lightgray",na.rm=TRUE,ylim=c(0,100))## eponge semble tres variable par rapport aux autres sites


boxplot(abundance_tot ~ Site:Habitat, data = bruvs_species_variable, col = "lightgray",na.rm=TRUE)



ggplot(bruvs_species_variable, aes(Site, abundance_tot)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#un peu plus joli
ggplot(bruvs_species_variable, aes(Site, abundance_tot)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(bruvs_species_variable, aes(Habitat, abundance_tot)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Habitat") + ylab("abundance")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = abundance_tot, x = Site, fill = Habitat), data = bruvs_species_variable) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

## anova a 1 facteur

anova.habitat= aov(abundance_tot ~ Habitat, data=bruvs_species_variable) 
summary(anova.habitat) 

## anova a 2 facteurs

anova.Site.habitat= aov(abundance_tot ~ Site*Habitat, data=bruvs_species_variable) 
summary(anova.Site.habitat) 


# post-hoc pairwise comparison de Tukey pour determiner quel site est significativement different de quel autre.
# on voit par exemple que Crypthelia et Antigonia sont significativement differents (probabibilite ajustee de 0.007)
TukeyHSD(anova.Site.habitat, "Site")



# la meme analyse avec une transformation log. 
anova.Site.habitat= aov(log(abundance_tot+1) ~ Site*Habitat, data=bruvs_species_variable) 
summary(anova.Site.habitat) 


ggplot(aes(y = log(abundance_tot+1), x = Site, fill=Habitat), data = bruvs_species_variable) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("abundance") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

TukeyHSD(anova.Site.habitat, "Site:Habitat")


####### GLM


glm.Site.habitat= glm(abundance_tot ~ Site*Habitat, family=gaussian, data=bruvs_species_variable)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat= glm(abundance_tot ~ Site+Habitat, family=gaussian, data=bruvs_species_variable)

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(abundance_tot ~ Site+Habitat+Depth, family=gaussian, data=bruvs_species_variable)

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)



lm.variables.log= lm(log(abundance_tot+1) ~ Habitat+SummitDepth+ValleyDepth+Depth+SummitAreasKm2+SSTmean, data=bruvs_species_variable)
anova(lm.variables.log)
summary(lm.variables.log)  
summ(lm.variables.log)
summ(lm.variables.log, confint = TRUE, digits = 3)
effect_plot(lm.variables.log, pred = BottomDepth, interval = TRUE, plot.points = TRUE)


summ(lm.variables.log, confint = TRUE, digits = 3)

effect_plot(lm.variables.log, pred = HabitatSummit, interval = TRUE, plot.points = TRUE)





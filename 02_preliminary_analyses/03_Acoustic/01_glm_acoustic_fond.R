load("01_formating_data/03_Acoustic/Rdata/acoustic.rdata")


###
names(Acoustic)
glm.variables= glm(AcousticFond ~ Day+Habitat+SummitDepth+ValleyDepth+
                     Height+SummitAreasKm2+SummitRugosity+
                     +BottomDepth+SSTmean1k + SSTmin1k+SSTmax1k+
                     SSTsd1k+ChlorA+EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                     SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand, family=Gamma, data=Acoustic)
anova(glm.variables)
summary(glm.variables) 

# on enleve valley depth car la hauteur c'est la difference entre sommet et vallee
glm.variables.gamma= glm(AcousticFond ~ Day+Habitat+SummitDepth+
                           Height+SummitAreasKm2+SummitRugosity+
                           +BottomDepth+SSTmean1k + SSTmin1k+SSTmax1k+
                           SSTsd1k+ChlorA+EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                           SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand, family=Gamma, data=Acoustic_fond)
anova(glm.variables.gamma)
summary(glm.variables.gamma) 

glm.variables.lnor= glm(AcousticFond ~ Day+Habitat+SummitDepth+
                          Height+SummitAreasKm2+SummitRugosity+
                          +BottomDepth+SSTmean1k + SSTmin1k+SSTmax1k+
                          SSTsd1k+ChlorA+EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                          SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand, family=gaussian(link="log"), data=Acoustic_fond)
anova(glm.variables.lnor)
summary(glm.variables.lnor) 


# le AIC du gamma est plus petit que le AIC du lognormal, a priori le gamma est mieux
AIC(glm.variables.lnor) 
AIC(glm.variables.gamma) 

# aide a interpretation
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

#gamma
summary(glm.variables.gamma) 
summ(glm.variables.gamma)
summ(glm.variables.gamma, confint = TRUE, digits = 3)
summ(glm.variables.gamma, confint = TRUE, ci.width = .5)
summ(glm.variables.gamma, confint = TRUE, pvals = FALSE)

effect_plot(glm.variables.gamma, pred = SummitDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = Height, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SummitRugosity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = BottomDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SSTmin1k, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SSTsd1k, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = EastwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = NorthwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = Salinity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SuspendedParticulateMatter, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = ShortestDistanceLand, interval = TRUE, plot.points = TRUE)
plot_summs(glm.variables.gamma, scale = TRUE, inner_ci_level = .9) # ne marche pas


###lognormal
summary(glm.variables.lnor) 
summ(glm.variables.lnor)
summ(glm.variables.lnor, confint = TRUE, digits = 3)
summ(glm.variables.lnor, confint = TRUE, ci.width = .5)
summ(glm.variables.lnor, confint = TRUE, pvals = FALSE)

plot_summs(glm.variables.lnor, scale = TRUE, inner_ci_level = .9)

effect_plot(glm.variables.lnor, pred = Day, interval = TRUE, plot.points = TRUE) ## effet positif
effect_plot(glm.variables.lnor, pred = SummitDepth, interval = TRUE, plot.points = TRUE) ## effet positif
effect_plot(glm.variables.lnor, pred = Height, interval = TRUE, plot.points = TRUE) ##positif
effect_plot(glm.variables.lnor, pred = SummitAreasKm2, interval = TRUE, plot.points = TRUE) ## effet positif
effect_plot(glm.variables.lnor, pred = SummitRugosity , interval = TRUE, plot.points = TRUE) ## effet negatif
effect_plot(glm.variables.lnor, pred = BottomDepth, interval = TRUE, plot.points = TRUE) ##negatif
effect_plot(glm.variables.lnor, pred = SSTmin1k, interval = TRUE, plot.points = TRUE) ##negatif
effect_plot(glm.variables.lnor, pred = SSTmax1k, interval = TRUE, plot.points = TRUE) ## negatif
effect_plot(glm.variables.lnor, pred = SSTsd1k, interval = TRUE, plot.points = TRUE) ## positif
effect_plot(glm.variables.lnor, pred = EastwardVelocity, interval = TRUE, plot.points = TRUE) ##positif
effect_plot(glm.variables.lnor, pred = NorthwardVelocity, interval = TRUE, plot.points = TRUE) ##positif
effect_plot(glm.variables.lnor, pred = Salinity, interval = TRUE, plot.points = TRUE) ## negatif
effect_plot(glm.variables.lnor, pred = SeaFloorPotentialTemperature, interval = TRUE, plot.points = TRUE) ## positif
effect_plot(glm.variables.lnor, pred = SuspendedParticulateMatter, interval = TRUE, plot.points = TRUE) ## positif
effect_plot(glm.variables.lnor, pred = ShortestDistanceLand, interval = TRUE, plot.points = TRUE) ## negatif



export_summs(glm.variables.lnor, scale = TRUE)






###################
#################
###############




ess= glm(AcousticFond ~ Day*Habitat*SummitDepth+
           Height*SummitAreasKm2*SummitRugosity*
           BottomDepth*SSTmean1k * SSTmin1k+SSTmax1k*
           SSTsd1k*ChlorA*EastwardVelocity*NorthwardVelocity*Salinity*SeaFloorPotentialTemperature*
           SuspendedParticulateMatter*ShortestDistanceReef*ShortestDistanceLand, family=gaussian(link="log"), data=Acoustic_fond)
anova(ess)
summary(ess) 










?boxplot

## boxplot on a formula:
#boxplot(count ~ spray, data = InsectSprays, col = "lightgray")

boxplot(AcousticSurface ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE)
boxplot(AcousticSurface ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,20))


boxplot(AcousticFond ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE)
boxplot(AcousticFond ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,35))## il y a pas mal de tres grosses valeurs. peut etre faire le menage?
boxplot(AcousticFond ~ Site, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,100))## eponge semble tres variable par rapport aux autres sites
boxplot(AcousticFond ~ Day, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,35))


## With less effort (slightly different) using factor *interaction*:
#boxplot(len ~ dose:supp, data = ToothGrowth,
#        boxwex = 0.5, col = c("orange", "yellow"),
#       main = "Guinea Pigs' Tooth Growth",
#        xlab = "Vitamin C dose mg", ylab = "tooth length",
#       sep = ":", lex.order = TRUE, ylim = c(0, 35), yaxs = "i")


boxplot(AcousticFond ~ Site:Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,220))## dur a voir les labels, rotation avec les graphes de la biblio ggplot



ggplot(Acoustic, aes(Site, AcousticFond)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#un peu plus joli
ggplot(Acoustic, aes(Site, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

# tu peux stocker dans un objet pour faire du multiplot plus tard
mongraphe1=ggplot(Acoustic, aes(Site, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

mongraphe1


ggplot(Acoustic, aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


# ici on enleve les transitsbelow2500m 
ggplot(Acoustic[Acoustic$Habitat!="TransitBelow2500m",], aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


## un graphe a trois facteurs, site x habitat x moment_journee
ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))+
  facet_wrap(~as.factor(Day), nrow=2)



## anova a 1 facteur

anova.habitat= aov(AcousticFond ~ Habitat, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) 
summary(anova.habitat) # ce resultat dit qu'il n'y a pas de differences statistiquement significative entre pente et sommet (sans tenir compte des autres facteurs)

ggplot(Acoustic[Acoustic$Habitat!="TransitBelow2500m",], aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


## anova a 2 facteurs

anova.Site.habitat= aov(AcousticFond ~ Site*Habitat, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) 
summary(anova.Site.habitat) # cette analyse de variance a 2 facteurs dit que l'interaction site x habitat n'est pas significative, que le signal n'est pas significativement different entre pente/sommet, mais qu'il y a des differences significatives entre sites

ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = AcousticFond, x = Site), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

# post-hoc pairwise comparison de Tukey pour determiner quel site est significativement different de quel autre.
# on voit par exemple que Crypthelia et Antigonia sont significativement differents (probabibilite ajustee de 0.007)
TukeyHSD(anova.Site.habitat, "Site")



# la meme analyse avec une transformation log. Cette fois tout est significatif: il faut regarder l'interaction
anova.Site.habitat= aov(log(AcousticFond+1) ~ Site*Habitat, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) 
summary(anova.Site.habitat) # cette anlyse de variance a 2 facteurs dit que l'interaction site x habitat n'est pas significative, que le signal n'est pas significativement different entre pente/sommet, mais qu'il y a des differences significatives entre sites


ggplot(aes(y = log(AcousticFond+1), x = Site, fill=Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,7.5)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

TukeyHSD(anova.Site.habitat, "Site:Habitat")


####### GLM


glm.Site.habitat= glm(AcousticFond ~ Site*Habitat, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat= glm(AcousticFond ~ Site+Habitat, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(AcousticFond ~ Site+Habitat+BottomDepth, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)


###
names(Acoustic)
glm.variables= glm(AcousticFond ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
anova(glm.variables)
summary(glm.variables)  ### je te laisse ajouter toutes les variables et analyser celles qui sont significatives, et faire les graphes qui vont bien

# un glm avec une famille gaussienne = pareil qu'un lm
lm.variables= lm(AcousticFond ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
anova(lm.variables)
summary(lm.variables)  ### je te laisse ajouter toutes les variables et analyser celles qui sont significatives, et faire les graphes qui vont bien


# aide a interpretation
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

summ(lm.variables)
summ(glm.variables)

summ(lm.variables, confint = TRUE, digits = 3)
summ(lm.variables, confint = TRUE, ci.width = .5)
summ(lm.variables, confint = TRUE, pvals = FALSE)

effect_plot(lm.variables, pred = BottomDepth, interval = TRUE, plot.points = TRUE)

?effect_plot


lm.variables.log= lm(log(AcousticFond+1) ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
anova(lm.variables.log)
summary(lm.variables.log)  ### je te laisse ajouter toutes les variables et analyser celles qui sont significatives, et faire les graphes qui vont bien
summ(lm.variables.log)
summ(lm.variables.log, confint = TRUE, digits = 3)
effect_plot(lm.variables.log, pred = BottomDepth, interval = TRUE, plot.points = TRUE)

summ(lm.variables)
plot_summs(lm.variables)
plot_summs(lm.variables, scale = TRUE)
plot_summs(lm.variables, scale = TRUE, inner_ci_level = .9)

plot_summs(lm.variables.log, scale = TRUE, inner_ci_level = .9)


Acoustic$AcousticFondLog=log(Acoustic$AcousticFond+1)

lm.variables.log= lm(AcousticFondLog ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
plot_summs(lm.variables.log, scale = TRUE, inner_ci_level = .9)
plot_summs(lm.variables, scale = TRUE, inner_ci_level = .9)
plot_summs(lm.variables,lm.variables.log, scale = TRUE, inner_ci_level = .9)


plot_summs(lm.variables, scale = TRUE, inner_ci_level = .9)
plot_summs(lm.variables.log, scale = TRUE, inner_ci_level = .9)

summ(lm.variables, confint = TRUE, digits = 3)
summ(lm.variables.log, confint = TRUE, digits = 3)

effect_plot(lm.variables.log, pred = HabitatSummit, interval = TRUE, plot.points = TRUE)

?effect_plot
#transformation variable
#https://rcompanion.org/handbook/I_12.html


summ(lm.variables.log, confint = TRUE, digits = 3)


###https://www.r-bloggers.com/2016/07/introduction-to-the-rms-package/
##https://rdrr.io/cran/rms/man/plot.Predict.html

ols.variables.log= ols(AcousticFondLog ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",], x=TRUE, y=TRUE)
ols.variables.log
summary(ols.variables.log)
anova(ols.variables.log)
plot(anova(ols.variables.log), what='proportion chisq')
plot(Predict(ols.variables.log))
vif(ols.variables.log) # test for multicolinearity
Predict(ols.variables.log)
an <- anova(ols.variables.log)
plot(Predict(ols.variables.log), anova=an, pval=TRUE)

ddist <- datadist(Acoustic$Habitat,Day,SummitDepth,ValleyDepth,BottomDepth,SummitAreasKm2+SSTmean1k)
options(datadist='ddist')

datadist(Acoustic$Habitat)


#######https://rdrr.io/cran/rms/man/plot.Predict.html
# a creuser car exemple interessant mais jarrive pas a le faire marcher
Acoustic2=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]
Acoustic2$AcousticFondLog=log(Acoustic2$AcousticFond+1)
Acoustic2$Habitat=as.factor(Acoustic2$Habitat)
Acoustic2$Day=as.factor(Acoustic2$Day)

ddist <- datadist(Acoustic$Habitat,Acoustic$Day,Acoustic$SummitDepth,Acoustic$ValleyDepth,Acoustic$BottomDepth,Acoustic$SummitAreasKm2+Acoustic$SSTmean1k)
options(datadist='ddist')

ols.variables.log= ols(AcousticFondLog ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic2, x=TRUE, y=TRUE)
summary(ols.variables.log)

anova(ols.variables.log)
plot(anova(ols.variables.log), what='aic')
plot(anova(ols.variables.log), what='')
plot(anova(ols.variables.log), what='partial R2')
plot(anova(ols.variables.log), what='proportion R2')
plot(anova(ols.variables.log), what='proportion chisq')
plot(Predict(ols.variables.log))




